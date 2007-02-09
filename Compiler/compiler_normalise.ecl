% BEGIN LICENSE BLOCK
% Version: CMPL 1.1
%
% The contents of this file are subject to the Cisco-style Mozilla Public
% License Version 1.1 (the "License"); you may not use this file except
% in compliance with the License.  You may obtain a copy of the License
% at www.eclipse-clp.org/license.
% 
% Software distributed under the License is distributed on an "AS IS"
% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.  See
% the License for the specific language governing rights and limitations
% under the License. 
% 
% The Original Code is  The ECLiPSe Constraint Logic Programming System. 
% The Initial Developer of the Original Code is  Cisco Systems, Inc. 
% Portions created by the Initial Developer are
% Copyright (C) 2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): Joachim Schimpf.
% 
% END LICENSE BLOCK
% ----------------------------------------------------------------------
% System:	ECLiPSe Constraint Logic Programming System
% Component:	ECLiPSe III compiler
% Version:	$Id: compiler_normalise.ecl,v 1.2 2007/02/09 02:54:48 kish_shen Exp $
% ----------------------------------------------------------------------

:- module(compiler_normalise).

:- comment(summary, "ECLiPSe III compiler - source code normaliser").
:- comment(copyright, "Cisco Technology Inc").
:- comment(author, "Joachim Schimpf").
:- comment(date, "$Date: 2007/02/09 02:54:48 $").

:- comment(desc, html("
	This module creates the normalised form of the source predicate on
	which the subsequent compiler passes do their work.
	<P>
	The clause structure is destroyed and replaced by inline disjunction.
	The idea being that the compiler will treat disjunctions such that
	we don't lose anything by doing so.
	<P>
	The decision to treat disjunctions directly implies that the bodies
	have forks and joins of control flow, which makes everything much
	more complicated compared to a conjunction-only clause compiler.
	The upside is that there is more scope for optimization, e.g.
	more sharing of the same environment, less data movement.
	<P>
	In the normalised form, the code is generally put in to a form that
	is more convenient for later processing:
	<UL>
	<LI>wrappers identify type of item
	<LI>lists of goals rather than comma/semicolon structures
	</UL>
	The constituents of a normalised source are:
	<PRE>
	Pred ::=	Conjunction

	Conjunction ::=	[Goal|...]

	Goal ::=	SimpleGoal
		|	Disjunction
		|	Indexpoint

	SimpleGoal ::=	goal{Term, ...}		% also used for head

	Disjunction ::=	disjunction{Branches, ...}

	Indexpoint ::=	indexpoint{...}

	Branches ::=	[Conjunction|...]

	Term ::=	variable{...}
		|	structure{...}
		|	ground_structure{...}	% not yet done
		|	[Term|...]		% list
		|	AtomicTerm		% atomic terms literally
	</PRE>

")).

:- lib(hash).

:- use_module(compiler_common).

:- ensure_loaded(compiler_analysis).
:- import print_goal_state/3 from compiler_analysis.


%----------------------------------------------------------------------
:- export
	normalize_clauses/5,
        normalize_clauses_annotated/7.
%----------------------------------------------------------------------

:- comment(normalize_clauses_annotated/7, hidden).

:- comment(normalize_clauses/5, [
    summary:"Transforms a list of clauses into the normalised predicate representation",
    args:[
	"Clauses":"A list of clauses for a single predicate",
	"VarNames":"A list of [Name|Var] pairs (like in readvar/3)",
	"Normalised":"The normalised, ground form of the predicate",
	"VarCount":"Number of distinct variables in the normalised code",
	"Module":"Context module"
    ],
    amode:normalize_clauses(+,+,-,-,+),
    desc:ascii("
	Build the ground representation of a single predicate (clause list).

	This deals with:
	    -	replacing variables with descriptors (struct variable())
		There is a fresh descriptor for every occurrence!
	    -	wrapping structures into descriptors (struct structure())
	    -	flattening conjunctions and disjunctions
	    -	modules (lookup and context), including tool->tool_body
	    -	variable goals -> metacall
	    -	classifying goals as simple or regular

	A normalised body or any other conjunction is a list of subgoals.
	A normalised disjunction is a struct disjunction()
	A normalised other subgoal is a struct goal()
    ")
]).

normalize_clauses(Clauses, VarNames, NormClauses, VarCount, Module) :-
        normalize_clauses_annotated(Clauses, _Ann, VarNames, _Files, NormClauses, VarCount, Module).

normalize_clauses_annotated(Clauses, AnnClauses, VarNames, Files, NormClauses, VarCount, Module) :-
	% We need to rename the (immediate) head variables apart,
	% because the normalisation will unify the head vars of the
	% different clauses (necessary for indexing analysis).
	% Fortunately, copy_term_vars preserves our variable names...
	( foreach(OrigClause,Clauses), foreach(NewClause,NewClauses) do
	    head_arg_vars(OrigClause, HeadArgVars),
	    copy_term_vars(HeadArgVars, OrigClause, NewClause)
	),
	normalize_clauses_noshare(NewClauses, AnnClauses, VarNames, Files, NormClauses, VarCount, Module).

    head_arg_vars(Clause, HeadArgVars) :-
	clause_head_body(Clause, _, Head, _, _, _, _),
	( foreacharg(Arg,Head), fromto(HeadArgVars,Vars1,Vars0,[]) do
	    ( var(Arg) -> Vars1=[Arg|Vars0] ; Vars1=Vars0 )
	).


% This can be used directly if it is known that the clauses
% don't have shared variables, e.g. when they come from the parser.
normalize_clauses_noshare(Clauses, AnnClauses, VarNames, Files, NormClauses, VarCount, Module) :-
	normalize_clause_list(Clauses, AnnClauses, Files, NormClauses, Module, Vs, []),
	assign_varids(Vs, VarNames, VarCount).
%	reorder_goals(NormClauses0, NormClauses).


%----------------------------------------------------------------------
% Method for dealing with variables:
% In the normalised representation, variables are replaced by
% variable{} descriptors, initially with uninstantiated varid-fields.
% A list of these descriptors is collected, and used at the end by
% assign_varids/3 to fill in the varid-fields.
%
% Handling of cuts in control constructs:
%	call(!)		local effect
%	once(!)		local effect
%	not(!)		local effect
%	(! -> ...)	local effect (was error in ECLiPSe I)
%	(... -> !)	global effect
%	(! ; ...)	global effect
%	(... ; !)	global effect
%
% Handling of 'true':
%	true/0 should really be removed completely in this step. However,
%	historically true/0 has been a regular goal in ECLiPSe, and
%	is used to force waking befores cuts, etc.  So removing it completely
%	would break old code in subtle ways. We should introduce a simple
%	version of it that is guaranteed to be eliminated completely, e.g.
%	for use in macro expansions.  Possible name ''/0.
%
% Disjunctions:
%	Disjunctions are flattened as much as possible.
%----------------------------------------------------------------------

:- mode normalize_body(?,?,?,+,+,-,+,+,-,-,+,+,+).
normalize_body(Var, AnnVar, File, Branch, CallNr0, CallNr, Cut, Vs0, Vs, Goals, Goals0, LM, CM) :-
	var(Var), !,
        AnnVar = annotated_term{from:From},
	normalize_goal(call(Var), From, File, Branch, CallNr0, CallNr, Cut, Vs0, Vs, Goals, Goals0, LM, CM).

normalize_body('', _Ann, _File, _Branch, CallNr, CallNr, _Cut, Vs, Vs, Goals, Goals, _LM, _CM) :- !.
normalize_body(true, _Ann, _File, _Branch, CallNr, CallNr, _Cut, Vs, Vs, Goals, Goals, _LM, _CM) :- !.

normalize_body(call(G), Ann, File, Branch, CallNr0, CallNr, _Cut, Vs0, Vs, Goals0, Goals, _LM, CM) :-
	nonvar(G), !,
	Goals0 = [SavecutGoal|Goals1],
	same_call_pos(Branch, CallNr0, CallNr1, CallPos0),
	savecut_goal(CallPos0, Vs0, Vs1, LocalCut, SavecutGoal),
	% cuts have local effect!
        Ann = annotated_term{term:call(AnnG)},
        normalize_body(G, AnnG, File, Branch, CallNr1, CallNr, LocalCut, Vs1, Vs, Goals1, Goals, CM, CM).

normalize_body(once(G), Ann, File, Branch, CallNr0, CallNr, _Cut, Vs0, Vs, Goals0, Goals, _LM, CM) :- !,
	Goals0 = [SavecutGoal|Goals1],
	same_call_pos(Branch, CallNr0, CallNr1, CallPos0),
	savecut_goal(CallPos0, Vs0, Vs1, LocalCut, SavecutGoal),
	% cuts have local effect!
        Ann = annotated_term{term:once(AG)},
        normalize_body(G, AG, File, Branch, CallNr1, CallNr2, LocalCut, Vs1, Vs2, Goals1, Goals2, CM, CM),
	Goals2 = [CuttoGoal|Goals],
	same_call_pos(Branch, CallNr2, CallNr, CallPos1),
	cutto_goal(CallPos1, Vs2, Vs, LocalCut, CuttoGoal).

normalize_body(not(G), Ann, File, Branch, CallNr0, CallNr, Cut, Vs0, Vs, Goals0, Goals, LM, CM) :- !,
        Ann = annotated_term{term:not(AG)},
        update_struct(annotated_term,[term:(\+AG)],Ann,Ann0),
        normalize_body(\+G, Ann0, File, Branch, CallNr0, CallNr, Cut, Vs0, Vs, Goals0, Goals, LM, CM).

normalize_body(\+G, Ann, File, Branch, CallNr0, CallNr, Cut, Vs0, Vs, Goals0, Goals, LM, CM) :- !,
        Ann = annotated_term{term:(\+AG),from:Fr,to:To},
        Ann1 = annotated_term{term:(AnnIf;AnnNone),from:Fr,to:To},
        AnnIf = annotated_term{term:(AG->annotated_term{term:fail,from:Fr,to:To})},
        AnnNone = annotated_term{term:'',from:Fr,to:To},
        normalize_body((G->fail;''), Ann1, File, Branch, CallNr0, CallNr, Cut, Vs0, Vs, Goals0, Goals, LM, CM).

normalize_body((G1->G2), Ann, File, Branch, CallNr0, CallNr, Cut, Vs0, Vs, Goals0, Goals, _LM, CM) :- !,
	% this is a ->/2 that's _not_ inside a ;/2
	Goals0 = [SavecutGoal|Goals1],
	same_call_pos(Branch, CallNr0, CallNr1, CallPos0),
	savecut_goal(CallPos0, Vs0, Vs1, LocalCut, SavecutGoal),
	% cuts have local effect!
        Ann = annotated_term{term:(AG1->AG2)},
        normalize_body(G1, AG1, File, Branch, CallNr1, CallNr2, LocalCut, Vs1, Vs2, Goals1, Goals2, CM, CM),
	Goals2 = [CuttoGoal|Goals3],
	same_call_pos(Branch, CallNr2, CallNr3, CallPos1),
	cutto_goal(CallPos1, Vs2, Vs3, LocalCut, CuttoGoal),
	normalize_body(G2, AG2, File, Branch, CallNr3, CallNr, Cut, Vs3, Vs, Goals3, Goals, CM, CM).

normalize_body((G1;G2), Ann, File, Branch, CallNr0, CallNr, Cut, Vs0, Vs, Goals, Goals0, _LM, CM) :- !,
	Goals = [SavecutGoal,disjunction{arity:0,callpos:CallPos,branches:Branches}|Goals0],
	same_call_pos(Branch, CallNr0, CallNr1, CallPos0),
	savecut_goal(CallPos0, Vs0, Vs1, DisjCut, SavecutGoal),
	new_call_pos(Branch, CallNr1, CallNr2, _CallPos),
	new_call_pos(Branch, CallNr2, CallNr, CallPos),
        Ann = annotated_term{term:(AG1;AG2)},
	normalize_left_branch(G1, AG1, File, CallPos, 1, BranchNr1, Cut, DisjCut, Vs1, Vs2, Branches, Branches1, CM, CM),
	normalize_right_branch(G2, AG2, File, CallPos, BranchNr1, _NBranches, Cut, DisjCut, Vs2, Vs, Branches1, [], CM, CM).

normalize_body((G1,G2), Ann, File, Branch, CallNr0, CallNr, Cut, Vs0, Vs, Goals, Goals0, _LM, CM) :- !,
	% this could be changed such that the lookup module propagates
	% through the comma (would be incompatible with Eclipse =< 5)
        Ann = annotated_term{term:(AG1,AG2)},
	normalize_body(G1, AG1, File, Branch, CallNr0, CallNr1, Cut, Vs0, Vs1, Goals, Goals1, CM, CM),
	normalize_body(G2, AG2, File, Branch, CallNr1, CallNr, Cut, Vs1, Vs, Goals1, Goals0, CM, CM).

normalize_body(G@M, Ann, File, Branch, CallNr0, CallNr, Cut, Vs0, Vs, Goals, Goals0, LM, _CM) :-
	% get_flag((@)/2, definition_module, sepia_kernel)@LM,
        !,
	% this could be changed such that the lookup module propagates
	% through the @ (would be incompatible with Eclipse =< 5)
        Ann = annotated_term{term:(AG@_AM),from:F},
        ( atom(M) ->
            normalize_body(G, AG, File, Branch, CallNr0, CallNr, Cut, Vs0, Vs, Goals, Goals0, LM, M)
	; var(G) ->
            
	    normalize_goal(call(G), F, File, Branch, CallNr0, CallNr, Cut, Vs0, Vs, Goals, Goals0, LM, M)
	;
	    normalize_goal(G, F, File, Branch, CallNr0, CallNr, Cut, Vs0, Vs, Goals, Goals0, LM, M)
	).

normalize_body(LM:G, Ann, File, Branch, CallNr0, CallNr, Cut, Vs0, Vs, Goals, Goals0, _LM0, CM) :-
	% get_flag((:)/2, definition_module, sepia_kernel)@LM0,
	atom(LM), nonvar(G),
	!,
        Ann = annotated_term{term:_ALM:AG},
        normalize_body(G, AG, File, Branch, CallNr0, CallNr, Cut, Vs0, Vs, Goals, Goals0, LM, CM).

normalize_body(!, _Ann, _File, Branch, CallNr0, CallNr, Cut, Vs0, Vs, Goals, Goals0, _LM, _CM) :- !,
	Goals = [CuttoGoal|Goals0],
	same_call_pos(Branch, CallNr0, CallNr, CallPos),
	cutto_goal(CallPos, Vs0, Vs, Cut, CuttoGoal).

normalize_body(X=Y, Ann, File, Branch, CallNr0, CallNr, Cut, Vs0, Vs, Goals, Goals0, LM, CM) :- !,
        Ann = annotated_term{from:From},
        % for now, we do not try to break the annotated goal down to each
        % simple unification
        simplify_unification(X, Y, UnifGoals, []),
	(
	    foreach(UnifGoal, UnifGoals),
	    fromto(CallNr0,CallNr1,CallNr2,CallNr),
	    fromto(Vs0,Vs1,Vs2,Vs),
	    fromto(Goals,Goals1,Goals2,Goals0),
	    param(Branch,Cut,LM,CM,From,File)
	do
	    normalize_goal(UnifGoal, From, File, Branch, CallNr1, CallNr2, Cut, Vs1, Vs2, Goals1, Goals2, LM, CM)
	).

normalize_body(G, Ann, File, Branch, CallNr0, CallNr, Cut, Vs0, Vs, Goals, Goals0, LM, CM) :-
        Ann = annotated_term{from:From},
        normalize_goal(G, From, File, Branch, CallNr0, CallNr, Cut, Vs0, Vs, Goals, Goals0, LM, CM).


normalize_goal(G, From, File, Branch, CallNr0, CallNr, _Cut, Vs0, Vs, [Goal|Goals], Goals, LM, CM) :-
	Goal = goal{
	    kind:Kind,
	    callpos:CallPos,
	    definition_module:DM,
	    lookup_module:LM1,
	    functor:N1/A1,
	    args:NormArgs,
            pos:From,
            path:File
	},
	functor(G, N, A),
	( get_flag(N/A, tool, on)@LM ->		% replace tool with tool body
	    tool_body(N/A, N1/A1, DM)@LM,
	    LM1=DM,
	    normalize_term(CM, NormCM, Vs0, Vs1),	% CM may be a variable
	    ModuleArg = [NormCM]
	; get_flag(N/A, definition_module, DM)@LM ->
	    N1=N, A1=A, LM1=LM,
	    Vs1=Vs0, ModuleArg = []
	;
	    DM = [],				% definition module unknown
	    N1=N, A1=A, LM1=LM,
	    Vs1=Vs0, ModuleArg = []
	),
	( get_flag(N/A, call_type, external)@LM ->	% classify goal
	    same_call_pos(Branch, CallNr0, CallNr, CallPos),
	    Kind = simple
	;
	    new_call_pos(Branch, CallNr0, CallNr, CallPos),
	    Kind = regular
	),
	(					% normalize arguments
	    for(I,1,A),
	    fromto(NormArgs,[NormArg|MoreArgs],MoreArgs,ModuleArg),
	    fromto(Vs1,Vs2,Vs3,Vs),
	    param(G)
	do
	    arg(I, G, Arg),
	    normalize_term(Arg, NormArg, Vs2, Vs3)
	).


normalize_left_branch((G1->G2), Ann, File, DisjCallPos, BranchNr0, BranchNr, Cut, DisjCut, Vs0, Vs, Branches, Branches0, _LM, CM) ?- !,
	% we have an if-then-else (a branch of the disjunction that gets cut)
	Branches = [[SavecutGoal|Goals]|Branches0],
	new_branch(DisjCallPos, BranchNr0, BranchNr, BranchPos),
	same_call_pos(BranchPos, 1, CallNr1, CallPos1),
	savecut_goal(CallPos1, Vs0, Vs1, LocalCut, SavecutGoal),
        Ann = annotated_term{term:(AG1->AG2)},
	normalize_body(G1, AG1, File, BranchPos, CallNr1, CallNr2, LocalCut, Vs1, Vs2, Goals, Goals1, CM, CM),
	Goals1 = [CuttoGoal|Goals2],
	same_call_pos(BranchPos, CallNr2, CallNr3, CallPos2),
	cutto_goal(CallPos2, Vs2, Vs3, DisjCut, CuttoGoal),
	normalize_body(G2, AG2, File, BranchPos, CallNr3, _CallNr, Cut, Vs3, Vs, Goals2, [], CM, CM).
normalize_left_branch((G1;G2), Ann, File, DisjCallPos, BranchNr0, BranchNr, Cut, DisjCut, Vs0, Vs, Branches, Branches0, _LM, CM) ?-
	% An if-then-else in the left alternative needs its own choicepoint!
	nonvar(G1), G1 \= (_ -> _),
	!,
	% This disjunction can be merged with the parent one
        Ann = annotated_term{term:(AG1;AG2)},
        normalize_left_branch(G1, AG1, File, DisjCallPos, BranchNr0, BranchNr1, Cut, DisjCut, Vs0, Vs1, Branches, Branches1, CM, CM),
	normalize_right_branch(G2, AG2, File, DisjCallPos, BranchNr1, BranchNr, Cut, DisjCut, Vs1, Vs, Branches1, Branches0, CM, CM).
normalize_left_branch(G1, Ann, File, DisjCallPos, BranchNr0, BranchNr, Cut, _DisjCut, Vs0, Vs, [Goals|Branches], Branches, LM, CM) :-
	% A normal (uncut) branch of the disjunction
	new_branch(DisjCallPos, BranchNr0, BranchNr, BranchPos),
	normalize_body(G1, Ann, File, BranchPos, 1, _CallNr, Cut, Vs0, Vs, Goals, [], LM, CM).


normalize_right_branch((G1;G2), Ann, File, DisjCallPos, BranchNr0, BranchNr, Cut, LocalCut, Vs0, Vs, Branches, Branches0, _LM, CM) ?- !,
	% This disjunction can be merged with the parent one
        Ann = annotated_term{term:(AG1;AG2)},
        normalize_left_branch(G1, AG1, File, DisjCallPos, BranchNr0, BranchNr1, Cut, LocalCut, Vs0, Vs1, Branches, Branches1, CM, CM),
	normalize_right_branch(G2, AG2, File, DisjCallPos, BranchNr1, BranchNr, Cut, LocalCut, Vs1, Vs, Branches1, Branches0, CM, CM).
normalize_right_branch(G1, Ann, File, DisjCallPos, BranchNr0, BranchNr, Cut, _LocalCut, Vs0, Vs, [Goals|Goals0], Goals0, LM, CM) :-
	new_branch(DisjCallPos, BranchNr0, BranchNr, Branch),
	normalize_body(G1, Ann, File, Branch, 1, _CallNr, Cut, Vs0, Vs, Goals, [], LM, CM).


% Normalise a list of standard clauses, facts, etc

% normalize_clause_list(+Clauses, ?AnnClauses, ?Files, -NormClause, +CM, +Vs0, -Vs)
normalize_clause_list([Clause], [AnnClause], [File], NormClause, CM, Vs0, Vs) :- !,
	NormClause = [SavecutGoal|NormClause1],
	same_call_pos([], 1, _CallNr, CallPos),
	savecut_goal(CallPos, Vs0, Vs1, Cut, SavecutGoal),
	normalize_clause(Clause, AnnClause, File, [], NormClause1, CM, Cut, Vs1, Vs, _).
normalize_clause_list(Clauses, AnnClauses, Files, NormClauses, CM, Vs0, Vs) :-
	NormClauses = [
	    SavecutGoal,
	    disjunction{arity:Arity,callpos:CallPos,branches:NormBranches}
	],
	clauses_arity(Clauses, Arity),
	same_call_pos([], 1, CallNr1, CallPos0),
	savecut_goal(CallPos0, Vs0, Vs1, Cut, SavecutGoal),
	new_call_pos([], CallNr1, CallNr2, _CallPos),
	new_call_pos([], CallNr2, _CallNr, CallPos),
%	new_call_pos([], CallNr1, _CallNr, CallPos),
	(
	    foreach(Clause,Clauses),
            foreach(AnnClause,AnnClauses),
            foreach(File,Files),
	    foreach([NormHead|Goals],NormBranches),
	    fromto(1,BranchNr1,BranchNr2,_BranchNr),
	    fromto(Vs1,Vs2,Vs3,Vs),
	    param(CallPos,CM,Cut,_HeadVars)
	do
	    new_branch(CallPos, BranchNr1, BranchNr2, ClauseBranch),
	    normalize_clause(Clause, AnnClause, File, ClauseBranch, [NormHead|Goals], CM, Cut, Vs2, Vs3, _HeadVars)
	).

    normalize_clause(Clause, AnnClause, File, Branch, Goals, CM, Cut, Vs0, Vs, HeadVars) :-
	clause_head_body(Clause, AnnClause, Head, Body, AnnHead, AnnBody, _HeadType),
	same_call_pos(Branch, 1, CallNr, CallPos),
	normalize_head(Head, AnnHead, File, CallPos, Goals, Goals1, CM, Vs0, Vs1, HeadVars),
	normalize_body(Body, AnnBody, File, Branch, CallNr, _CallNr, Cut, Vs1, Vs, Goals1, [], CM, CM).

    :- mode clause_head_body(+,?,-,-,-,-,-).
    clause_head_body(H:-B, Ann, H, B, AH, AB, unify) :- !,
        Ann = annotated_term{term:(AH:-AB)}.
    clause_head_body(H?-B, Ann, H, B, AH, AB, match) :- !,
        Ann = annotated_term{term:(AH?-AB)}.
    clause_head_body(H, AH, H, '', AH, '', unify).

    clauses_arity([Clause|_], A) ?-
    	clause_head_body(Clause, _, H, _, _, _, _),
	functor(H, _, A).


% Create a goal to save the cut position in a new variable
% This will be optimized away later if the variable remains unused

savecut_goal(CallPos, Vs0, Vs1, CutVar, Goal) :-
	Goal = goal{
	    kind:simple,
	    callpos:CallPos,
	    definition_module:sepia_kernel,
	    lookup_module:sepia_kernel,
	    functor:get_cut/1,
	    args:[NormCutVar]
	},
	new_aux_variable(CutVar, NormCutVar, Vs0, Vs1).


cutto_goal(CallPos, Vs0, Vs1, CutVar, Goal) :-
	Goal = goal{
	    kind:simple,
	    callpos:CallPos,
	    definition_module:sepia_kernel,
	    lookup_module:sepia_kernel,
	    functor:cut_to/1,
	    args:[NormCutVar]
	},
	normalize_term(CutVar, NormCutVar, Vs0, Vs1).



% Normalised term representation:
%
%	variables	struct(variable)
%	atomic		as such
%	lists		as such
%	structs		structure(structure)
%	ground struct	ground_structure(ground)   TODO

:- mode normalize_term(?,-,-,+).
normalize_term(X, VarDesc, [VarDesc|Vs], Vs) :-
	var(X), !,
	VarDesc = variable{source_var:X,source_name:Name},
	( get_var_info(X, name, Name) -> true ; true ).
normalize_term(X, X, Vs, Vs) :-
	atomic(X).
normalize_term([X|Xs], [Y|Ys], Vs, Vs0) :- !,
	normalize_term(X, Y, Vs, Vs1),
	normalize_term(Xs, Ys, Vs1, Vs0).
normalize_term(X, structure{name:N,arity:A,args:Args}, Vs, Vs0) :-
	compound(X),
	functor(X, N, A),
	(
	    for(I,1,A),
	    foreach(NormArg,Args),
	    fromto(Vs,Vs2,Vs1,Vs0),
	    param(X)
	do
	    arg(I, X, Arg),
	    normalize_term(Arg, NormArg, Vs2, Vs1)
	).



% Introduce a new, auxiliary source variable that was not in the
% original source. Treat occurrences like normal source variables.
% The name '' suppresses singleton warnings later
new_aux_variable(X, VarDesc, [VarDesc|Vs], Vs) :-
	VarDesc = variable{source_name:'',source_var:X}.



% Fill in the variable{varid:} fields with a unique integer >= 1
% for every distinct variable.  Note that we have not yet separated
% identical variables that occur in parallel branches of a disjunction,
% that is taken care of in the variable classification pass.

assign_varids(Vs, _VarNames, N) :-
	sort(source_var of variable, =<, Vs, SortedVs),
	(
	    foreach(variable{source_var:X,varid:I},SortedVs),
	    fromto(_OtherVar,X0,X,_),
	    fromto(0,I0,I,N)
	do
	    ( X == X0 -> I = I0 ; I is I0+1 )
	).

/*
% Fill in the variable{varid:} fields with
% a unique integer >= 1 for every distinct variable
% Also fill in source_name fields according to VarNames list.
% If name information is missing, set source_name to the source variable.

assign_varids(Vs, VarNames, N) :-
	sort(source_var of variable, =<, Vs, SortedVs),
	sort(2, <, VarNames, SortedNames),
	assign_varids_names(SortedVs, _OtherVar, SortedNames, 0, N).

    assign_varids_names([], _X, _Names, I, I).
    assign_varids_names([variable{source_var:X,varid:I,source_name:Name}|List], X0, Names0, I0, IN) :-
    	( X == X0 -> I = I0 ; I is I0+1 ),
	lookup_name(X, Names0, Names, Name),
	assign_varids_names(List, X, Names, I, IN).

    % If we don't have a name, use X itself
    lookup_name(X, [], [], X).
    lookup_name(X, Names, Names2, Name) :-
    	Names = [[VName|V]|Names1],
    	( X == V ->
	    Name = VName, Names2 = Names
	; X @> V ->
	    lookup_name(X, Names1, Names2, Name)
	;
	    Name = X, Names2 = Names
	).
*/
    	
    	

% From a head, construct a normalised head, which is a pseudo
% goal of kind:head. The normalised head contains distinct variables.
% Head unifications get flattened into a sequence of =/2 goals.
% NOTE: the HeadVars list is shared between all heads and causes the
% head variables of all clauses to be unified (for indexing analysis)!

normalize_head(Head, AnnHead, File, CallPos, Goals, Goals0, Module, Vs0, Vs, HeadVars) :-
        AnnHead = annotated_term{term:HeadAnn,from:From},
        Goals = [goal{
		    kind:head, callpos:CallPos, lookup_module:Module,
		    definition_module:Module, pos:From, path:File,
		    functor:N/A, args:HeadArgs}
		|Goals1],
	    functor(Head, N, A),
            functor(HeadAnn, N, A),
	    (
		for(I,1,A),
		foreach(HeadArg,HeadArgs),
		fromto(Vs0,Vs1,Vs3,Vs),
		fromto([],Seen1,Seen2,_),
		fromto(Goals1,Goals2,Goals3,Goals0),
		foreach(HeadVar,HeadVars),
		param(Head,HeadAnn,CallPos,File)
	    do
		Goals2 = [goal{
			kind:simple,
                        pos:From,
                        path:File,
			callpos:CallPos,
			definition_module:sepia_kernel,
			lookup_module:sepia_kernel,
			functor:(=)/2,
			args:[HeadArg,NormArg]
		    }|Goals3],
                arg(I, HeadAnn, annotated_term{from:From}),
		arg(I, Head, Arg),
		normalize_term(Arg, NormArg, Vs1,Vs2),
		( var(Arg), varnonmember(Arg,Seen1) ->
		    % Don't create a new variable for the first occurrence.
		    % But create a dummy-goal X=X in order not to lose
		    % the variable occurrence altogether. This is necessary
		    % if the variable turns out to be permanent and has only
		    % this head occurrence in the first chunk. The X=X goal
		    % will then trigger initialisation of the environment slot.
		    Seen2 = [Arg|Seen1],
		    HeadArg = NormArg, Vs3 = Vs2, HeadVar = Arg
		;
		    % head argument is nonvariable or repeated var occurrence
		    Seen2 = Seen1,
		    new_aux_variable(HeadVar, HeadArg, Vs2, Vs3)
		)
	    ).

varnonmember(_X, []).
varnonmember(X, [Y|Ys]) :-
	X \== Y,
	varnonmember(X, Ys).

simplify_unification(X, Y, [X=Y|T], T) :- var(X), !.
simplify_unification(X, Y, [Y=X|T], T) :- var(Y), !.
simplify_unification(X, Y, L, L) :- X == Y, !.
simplify_unification(X, Y, L, L0) :- functor(X, F, N), functor(Y, F, N), !,
	(
	    for(I,1,N),
	    fromto(L,L1,L2,L0),
	    param(X,Y)
	do
	    arg(I, X, AX),
	    arg(I, Y, AY),
	    simplify_unification(AX, AY, L1, L2)
	).
simplify_unification(_X, _Y, [fail|T], T).


%----------------------------------------------------------------------
% Reorder a simple basic block prefix
% - bring indexable tests to the front
% - bring Var=Var unifications together
%----------------------------------------------------------------------

reorder_goals([], []).
reorder_goals(Goals, RGoals) :- Goals = [_|_],
	reorder_prefix(Goals, RGoals).
reorder_goals(disjunction{branches:Branches}, disjunction{branches:RBranches}) :-
	(
	    foreach(Branch,Branches),
	    foreach(RBranch,RBranches)
	do
	    reorder_goals(Branch, RBranch)
		
	).


reorder_prefix(Goals, ReorderedGoals) :-
	extract_and_prioritize_simple_prefix(Goals, KeyPrefix, Rest),
	keysort(KeyPrefix, SortedKeyPrefix),
	strip_key(SortedKeyPrefix, SortedPrefix),
	append(SortedPrefix, Rest, ReorderedGoals).

    strip_key([], []).
    strip_key([_K-X|KXs], [X|Xs]) :-
	strip_key(KXs, Xs).
	
extract_and_prioritize_simple_prefix([], [], []).
extract_and_prioritize_simple_prefix([Goal|Goals], Prefix, Rest) :-
	normalize_unif(Goal, Goal1),
	( prefix_goal(Goal1, Prio) ->
	    Prefix = [Prio-Goal1|Prefix0],
	    extract_and_prioritize_simple_prefix(Goals, Prefix0, Rest)
	;
	    Prefix = [],
	    Rest = [Goal1|Goals]
	).

:- mode prefix_goal(+,-).
%prefix_goal(goal{functor:atom/1}, 1) :- !.
%prefix_goal(goal{functor:atomic/1}, 1) :- !.
%prefix_goal(goal{functor:number/1}, 1) :- !.
%prefix_goal(goal{functor:var/1}, 1) :- !.
%prefix_goal(goal{functor:nonvar/1}, 1) :- !.
%prefix_goal(goal{functor:integer/1}, 1) :- !.
%prefix_goal(goal{functor:real/1}, 1) :- !.
%prefix_goal(goal{functor:rational/1}, 1) :- !.
%prefix_goal(goal{functor:breal/1}, 1) :- !.
%prefix_goal(goal{functor:free/1}, 1) :- !.
%prefix_goal(goal{functor:string/1}, 1) :- !.
%prefix_goal(goal{functor:meta/1}, 1) :- !.
%prefix_goal(goal{functor:is_handle/1}, 1) :- !.
%prefix_goal(goal{functor:is_suspension/1}, 1) :- !.
prefix_goal(goal{functor:(=)/2,args:[_,Y]}, 1) :- atomic(Y), !.
prefix_goal(goal{functor:(=)/2,args:[_,[_|_]]}, 2) :- !.
prefix_goal(goal{functor:(=)/2,args:[_,structure{}]}, 2) :- !.
prefix_goal(goal{functor:(=)/2,args:[variable{},variable{}]}, 3) :- !.
prefix_goal(goal{functor:(=)/2,args:[X,Y]}, 4) :- !,
	printf(warning_output, "Unclassified unification %w%n", [X=Y]).


normalize_unif(Goal, NormUnif) :-
	Goal = goal{functor:(=)/2,args:[X,Y]},
	Y = variable{},
	X \= variable{},
	!,
	update_struct(goal, args:[Y,X], Goal, NormUnif).
normalize_unif(Goal, Goal).


%----------------------------------------------------------------------
% 
%----------------------------------------------------------------------

:- export print_normalized_clause/2.

print_normalized_clause(Stream, Clause) :-
	writeln(Stream, "------ Normalized Source ------"),
	print_normalized_goal(Stream, 0, Clause).


print_normalized_goal(_Stream, _Indent, []).
print_normalized_goal(Stream, Indent, [Goal|Goals]) :-
	print_normalized_goal(Stream, Indent, Goal),
	print_normalized_goal(Stream, Indent, Goals).
print_normalized_goal(Stream, Indent, disjunction{arity:A,callpos:P,branches:Bs}) :-
	( foreach(Branch,Bs), fromto("TRY",T,"RETRY",_), param(Stream,Indent,P,A) do
	    Indent1 is Indent+1,
	    indent(Stream, Indent),
	    printf(Stream, "%w/%d  (callpos:", [T,A]),
	    print_call_pos(Stream, P),
	    writeln(Stream, ")"),
	    print_normalized_goal(Stream, Indent1, Branch)
	),
	indent(Stream, Indent),
	write(Stream, "JOIN  (callpos:"),
	print_call_pos(Stream, P),
	writeln(Stream, ")").
print_normalized_goal(Stream, Indent, goal{kind:K,callpos:P,state:State,
		lookup_module:LM, functor:F,args:Args,envmap:EAM,
                pos:Pos,path:Path}) :-
	indent(Stream, Indent),
	( K == head -> write(Stream, "HEAD") ; write(Stream, "GOAL") ),
	printf(Stream, "  %w  (lm:%w, kind:%w, path:%w, pos:%w callpos:", [F,LM,K,Path,Pos]),
	print_call_pos(Stream, P),
	decode_activity_map(EAM,Env),
	printf(Stream, ", env:%w)%n", [Env]),
	ArgIndent is Indent+1,
	( foreach(A,Args), param(Stream,ArgIndent) do
	    indent(Stream, ArgIndent), writeln(Stream, A)
	),
	print_goal_state(Stream, Indent, State).
print_normalized_goal(Stream, Indent, indexpoint{callpos:P,args:Args}) :-
	indent(Stream, Indent),
	write(Stream, "INDEX  (callpos:"),
	print_call_pos(Stream, P),
	writeln(Stream, ")"),
	ArgIndent is Indent+1,
	( foreach(A,Args), param(Stream,ArgIndent) do
	    indent(Stream, ArgIndent), writeln(Stream, A)
	).


/*
print_normalized_head(Stream, Head) :-
	functor(Head, F, A),
	printf(Stream, "HEAD  %w%n", [F/A]),
	(
	    foreacharg(Arg,Head),
	    param(Stream)
	do
	    indent(Stream, 1), writeln(Stream, Arg)
	).
*/


