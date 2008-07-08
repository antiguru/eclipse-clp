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
% Version:	$Id: compiler_normalise.ecl,v 1.12 2008/07/08 22:33:20 jschimpf Exp $
% ----------------------------------------------------------------------

:- module(compiler_normalise).

:- comment(summary, "ECLiPSe III compiler - source code normaliser").
:- comment(copyright, "Cisco Technology Inc").
:- comment(author, "Joachim Schimpf, Kish Shen").
:- comment(date, "$Date: 2008/07/08 22:33:20 $").

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

	SimpleGoal ::=	goal{Term, ...}		% also used for head

	Disjunction ::=	disjunction{Branches, ...}

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

:- import print_goal_state/3 from compiler_analysis.

:- import
	meta_index/2,
	get_attribute/3
   from sepia_kernel.


% Utilities to deal with (optionally uninstantiated) annotated terms
% TODO: inline these
:- local op(700, xfx, =:).
Var =: _Template :- var(Var), !.
%Term =: Term.
Term =: Term2 :- verify instance(Term, Term2), Term = Term2.

varg(I, T, A) :- ( var(T) -> true ; arg(I, T, A) ).

ann_update_term(_NewTermAnn, Ann, _NewAnn) :- var(Ann), !.
ann_update_term(NewTermAnn, Ann, NewAnn) :-
	type_of(NewTermAnn, Type),
	update_struct(annotated_term, [term:NewTermAnn,type:Type], Ann, NewAnn).

ann_location(Ann, '', 0, 0, 0) :- var(Ann), !.
ann_location(annotated_term{file:File,line:Line,from:From,to:To}, File, Line, From, To).
		

%----------------------------------------------------------------------
:- export
	normalize_clauses_noshare/5,
        normalize_clauses_noshare_annotated/6,
	normalize_clauses/5,
        normalize_clauses_annotated/6.
%----------------------------------------------------------------------

:- comment(normalize_clauses_annotated/6, hidden).

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
        normalize_clauses_annotated(Clauses, _Ann, VarNames, NormClauses, VarCount, Module).

normalize_clauses_annotated(Clauses, AnnClauses, VarNames, NormClauses, VarCount, Module) :-
	% We need to rename the (immediate) head variables apart,
	% because the normalisation will unify the head vars of the
	% different clauses (necessary for indexing analysis).
	% Fortunately, copy_term_vars preserves our variable names...
	(
	    foreach(OrigClause,Clauses),
	    foreach(NewClause,NewClauses),
	    foreach(OrigAnnClause,AnnClauses),
	    foreach(NewAnnClause,NewAnnClauses)
	do
	    head_arg_vars(OrigClause, HeadArgVars),
	    copy_term_vars(HeadArgVars, OrigClause-OrigAnnClause, NewClause-NewAnnClause)
	),
	normalize_clauses_noshare_annotated(NewClauses, NewAnnClauses, VarNames, NormClauses, VarCount, Module).

    head_arg_vars(Clause, HeadArgVars) :-
	clause_head_body(Clause, _, Head, _, _, _, _),
	( foreacharg(Arg,Head), fromto(HeadArgVars,Vars1,Vars0,[]) do
	    % BUG: we cannot copy metas because we'd lose the attributes!
%	    ( var(Arg) -> Vars1=[Arg|Vars0] ; Vars1=Vars0 )
	    ( free(Arg) -> Vars1=[Arg|Vars0] ; Vars1=Vars0 )
	).


% This can be used directly if it is known that the clauses
% don't have shared variables, e.g. when they come from the parser.
normalize_clauses_noshare(Clauses, VarNames, NormClauses, VarCount, Module) :-
	normalize_clauses_noshare_annotated(Clauses, _Ann, VarNames, NormClauses, VarCount, Module).

normalize_clauses_noshare_annotated(Clauses, AnnClauses, VarNames, NormClauses, VarCount, Module) :-
	normalize_clause_list(Clauses, AnnClauses, NormClauses, Module, Vs, []),
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
%	is used to force waking before cuts, etc.  So removing it completely
%	would break old code in subtle ways. We therefore introduce a simple
%	version of it, called ''/0, that is guaranteed to be eliminated
%	completely, e.g. for use in macro expansions.
%
% Disjunctions:
%	Disjunctions are flattened as much as possible.
%----------------------------------------------------------------------

:- mode normalize_body(?,?,+,+,-,+,+,-,-,+,+,+,+).
normalize_body(Var, AnnVar, Branch, CallNr0, CallNr, Cut, Vs0, Vs, Goals, Goals0, LM, CM, _PM) :-
	var(Var), !,
	ann_update_term(call(AnnVar), AnnVar, AnnCall),
	normalize_goal(call(Var), AnnCall, Branch, CallNr0, CallNr, Cut, Vs0, Vs, Goals, Goals0, LM, CM).

normalize_body('', _Ann, _Branch, CallNr, CallNr, _Cut, Vs, Vs, Goals, Goals, _LM, _CM, _PM) :- !.
%normalize_body(true, _Ann, _Branch, CallNr, CallNr, _Cut, Vs, Vs, Goals, Goals, _LM, _CM, _PM) :- !.

%normalize_body(call(G), Ann, Branch, CallNr0, CallNr, _Cut, Vs0, Vs, Goals0, Goals, _LM, CM, PM) :-
%	nonvar(G), !,
%	Goals0 = [SavecutGoal|Goals1],
%	same_call_pos(Branch, CallNr0, CallNr1, CallPos0),
%	savecut_goal(CallPos0, Vs0, Vs1, LocalCut, SavecutGoal),
%	% cuts have local effect!
%        Ann =: annotated_term{term:call(AnnG)},
%        normalize_body(G, AnnG, Branch, CallNr1, CallNr, LocalCut, Vs1, Vs, Goals1, Goals, CM-any, CM, PM).

normalize_body(once(G), Ann, Branch, CallNr0, CallNr, _Cut, Vs0, Vs, Goals0, Goals, _LM, CM, PM) :- !,
	Goals0 = [SavecutGoal|Goals1],
	same_call_pos(Branch, CallNr0, CallNr1, CallPos0),
	savecut_goal(CallPos0, Vs0, Vs1, LocalCut, SavecutGoal),
	% cuts have local effect!
        Ann =: annotated_term{term:once(AG)},
        normalize_body(G, AG, Branch, CallNr1, CallNr2, LocalCut, Vs1, Vs2, Goals1, Goals2, CM-any, CM, PM),
	Goals2 = [CuttoGoal|Goals],
	same_call_pos(Branch, CallNr2, CallNr, CallPos1),
	cutto_goal(CallPos1, Vs2, Vs, LocalCut, CuttoGoal).

normalize_body(not(G), Ann, Branch, CallNr0, CallNr, Cut, Vs0, Vs, Goals0, Goals, LM, CM, PM) :- !,
        Ann =: annotated_term{term:not(AG)},
        ann_update_term(\+AG, Ann, Ann0),
        normalize_body(\+G, Ann0, Branch, CallNr0, CallNr, Cut, Vs0, Vs, Goals0, Goals, LM, CM, PM).

normalize_body(\+G, Ann, Branch, CallNr0, CallNr, Cut, Vs0, Vs, Goals0, Goals, LM, CM, PM) :- !,
        Ann =: annotated_term{term:(\+AG)},
        ann_update_term(fail, Ann, AnnFail),
        ann_update_term('', Ann, AnnTrue),
        ann_update_term((AG->AnnFail), Ann, AnnCond),
        ann_update_term((AnnCond;AnnTrue), Ann, AnnITE),
        normalize_body((G->fail;''), AnnITE, Branch, CallNr0, CallNr, Cut, Vs0, Vs, Goals0, Goals, LM, CM, PM).

normalize_body((G1->G2), Ann, Branch, CallNr0, CallNr, Cut, Vs0, Vs, Goals0, Goals, _LM, CM, PM) :- !,
	% this is a ->/2 that's _not_ inside a ;/2
	Goals0 = [SavecutGoal|Goals1],
	same_call_pos(Branch, CallNr0, CallNr1, CallPos0),
	savecut_goal(CallPos0, Vs0, Vs1, LocalCut, SavecutGoal),
	% cuts have local effect!
        Ann =: annotated_term{term:(AG1->AG2)},
        normalize_body(G1, AG1, Branch, CallNr1, CallNr2, LocalCut, Vs1, Vs2, Goals1, Goals2, CM-any, CM, PM),
	Goals2 = [CuttoGoal|Goals3],
	same_call_pos(Branch, CallNr2, CallNr3, CallPos1),
	cutto_goal(CallPos1, Vs2, Vs3, LocalCut, CuttoGoal),
	normalize_body(G2, AG2, Branch, CallNr3, CallNr, Cut, Vs3, Vs, Goals3, Goals, CM-any, CM, PM).

	% TODO: compile softcut!  Preliminary: metacall it.
normalize_body((G1*->G2;G3), Ann, Branch, CallNr0, CallNr, Cut, Vs0, Vs, Goals, Goals0, LM, CM, PM) ?- !,
	ann_update_term(call(Ann), Ann, AnnCall),
	normalize_body(call(G1*->G2;G3), AnnCall, Branch, CallNr0, CallNr, Cut, Vs0, Vs, Goals, Goals0, LM, CM, PM).

normalize_body((G1;G2), Ann, Branch, CallNr0, CallNr, Cut, Vs0, Vs, Goals, Goals0, _LM, CM, PM) :- !,
	Goals = [SavecutGoal,disjunction{callpos:CallPos,branches:Branches}|Goals0],
	same_call_pos(Branch, CallNr0, CallNr1, CallPos0),
	savecut_goal(CallPos0, Vs0, Vs1, DisjCut, SavecutGoal),
	new_call_pos(Branch, CallNr1, CallNr2, _CallPos),
	new_call_pos(Branch, CallNr2, CallNr, CallPos),
        Ann =: annotated_term{term:(AG1;AG2)},
	normalize_left_branch(G1, AG1, CallPos, 1, BranchNr1, Cut, DisjCut, Vs1, Vs2, Branches, Branches1, CM-any, CM, PM),
	normalize_right_branch(G2, AG2, CallPos, BranchNr1, _NBranches, Cut, DisjCut, Vs2, Vs, Branches1, [], CM-any, CM, PM).

normalize_body((G1,G2), Ann, Branch, CallNr0, CallNr, Cut, Vs0, Vs, Goals, Goals0, _LM, CM, PM) :- !,
	% this could be changed such that the lookup module propagates
	% through the comma (would be incompatible with Eclipse =< 5)
        Ann =: annotated_term{term:(AG1,AG2)},
	normalize_body(G1, AG1, Branch, CallNr0, CallNr1, Cut, Vs0, Vs1, Goals, Goals1, CM-any, CM, PM),
	normalize_body(G2, AG2, Branch, CallNr1, CallNr, Cut, Vs1, Vs, Goals1, Goals0, CM-any, CM, PM).

normalize_body(G@M, Ann, Branch, CallNr0, CallNr, Cut, Vs0, Vs, Goals, Goals0, LM, _CM, PM) :-
	% get_flag((@)/2, definition_module, sepia_kernel)@LM,
        !,
	% this could be changed such that the lookup module propagates
	% through the @ (would be incompatible with Eclipse =< 5)
	Ann =: annotated_term{term:(AG@_AM)},
        ( atom(M) ->
            normalize_body(G, AG, Branch, CallNr0, CallNr, Cut, Vs0, Vs, Goals, Goals0, LM, M, PM)
	; var(G) ->
	    ann_update_term(call(AG), Ann, AnnCall),
	    normalize_goal(call(G), AnnCall, Branch, CallNr0, CallNr, Cut, Vs0, Vs, Goals, Goals0, LM, M)
	;
	    normalize_goal(G, AG, Branch, CallNr0, CallNr, Cut, Vs0, Vs, Goals, Goals0, LM, M)
	).

normalize_body(LM:G, Ann, Branch, CallNr0, CallNr, Cut, Vs0, Vs, Goals, Goals0, _LM0, CM, PM) :-
	% get_flag((:)/2, definition_module, sepia_kernel)@LM0,
	atom(LM), nonvar(G),
	!,
        Ann =: annotated_term{term:(_ALM:AG)},
        normalize_body(G, AG, Branch, CallNr0, CallNr, Cut, Vs0, Vs, Goals, Goals0, LM-exported, CM, PM).

normalize_body(!, _Ann, Branch, CallNr0, CallNr, Cut, Vs0, Vs, Goals, Goals0, _LM, _CM, _PM) :- !,
	Goals = [CuttoGoal|Goals0],
	same_call_pos(Branch, CallNr0, CallNr, CallPos),
	cutto_goal(CallPos, Vs0, Vs, Cut, CuttoGoal).

normalize_body(X=Y, Ann, Branch, CallNr0, CallNr, Cut, Vs0, Vs, Goals, Goals0, LM, CM, _PM) :- !,
        simplify_unification(X=Y, Ann, UnifGoals, AnnUnifGoals),
	(
	    foreach(UnifGoal, UnifGoals),
	    foreach(AnnUnifGoal, AnnUnifGoals),
	    fromto(CallNr0,CallNr1,CallNr2,CallNr),
	    fromto(Vs0,Vs1,Vs2,Vs),
	    fromto(Goals,Goals1,Goals2,Goals0),
	    param(Branch,Cut,LM,CM)
	do
	    normalize_goal(UnifGoal, AnnUnifGoal, Branch, CallNr1, CallNr2, Cut, Vs1, Vs2, Goals1, Goals2, LM, CM)
	).

normalize_body(G, Ann, Branch, CallNr0, CallNr, Cut, Vs0, Vs, Goals, Goals0, LM, CM, _PM) :-
        normalize_goal(G, Ann, Branch, CallNr0, CallNr, Cut, Vs0, Vs, Goals, Goals0, LM, CM).


normalize_goal(G, AnnG, Branch, CallNr0, CallNr, _Cut, Vs0, Vs, [Goal|Goals], Goals, LM-Vis, CM) :-
	callable(G),
	!,
	AnnG =: annotated_term{term:GAnn},
	ann_location(AnnG, File, Line, From, To),
	Goal = goal{
	    kind:Kind,
	    callpos:CallPos,
	    definition_module:DM,
	    lookup_module:LM1,
	    functor:N1/A1,
	    args:NormArgs,
            path:File,
            line:Line,
            from:From,
            to:To
	},
	functor(G, N, A),
	get_pred_info(Vis, LM, N/A, DM, ToolBody, CallType),
	( ToolBody = N1/A1 -> 			% replace tool with tool body
	    LM1=DM,
	    ann_update_term(CM, AnnG, AnnCM),
	    normalize_term(CM, AnnCM, NormCM, Vs0, Vs1, =),	% CM may be a variable
	    ModuleArg = [NormCM]
	;
	    N1=N, A1=A, LM1=LM,
	    Vs1=Vs0, ModuleArg = []
	),
	( CallType = external ->
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
	    param(G,GAnn)
	do
	    arg(I, G, Arg),
	    varg(I, GAnn, AnnArg),
	    normalize_term(Arg, AnnArg, NormArg, Vs2, Vs3, =)
	    % ( Vs2==Vs3 -> term is ground ; true )
	).
normalize_goal(G, AnnG, _, _, _, _, _, _, _, _, LM, _) :-
	compiler_event(#illegal_goal, term, AnnG, G, LM).


    % Look up relevant properties of the called predicate.
    % If it is not known yet, assume defaults (regular, non-tool, []-module).
    get_pred_info(ReqVis, LM, Pred, DM, ToolBody, CallType) :-
	(
	    current_module(LM),
	    get_flag(Pred, visibility, Vis)@LM,
	    required_visibility(ReqVis, Vis)
	->

	    ( get_flag(Pred, tool, on)@LM ->
		tool_body(Pred, ToolBody, DM)@LM
	    ;
		ToolBody = none,
		get_flag(Pred, definition_module, DM)@LM
	    ),
	    get_flag(Pred, call_type, CallType)@LM
	;
	    % Nothing known about Pred, assume defaults
	    DM = [], ToolBody = none, CallType = prolog
	).

    required_visibility(any, _) :- !.
    required_visibility(exported, exported) :- !.
    required_visibility(exported, reexported).


normalize_left_branch((G1->G2), Ann, DisjCallPos, BranchNr0, BranchNr, Cut, DisjCut, Vs0, Vs, Branches, Branches0, _LM, CM, PM) ?- !,
	% we have an if-then-else (a branch of the disjunction that gets cut)
	Branches = [[SavecutGoal|Goals]|Branches0],
	new_branch(DisjCallPos, BranchNr0, BranchNr, BranchPos),
	same_call_pos(BranchPos, 1, CallNr1, CallPos1),
	savecut_goal(CallPos1, Vs0, Vs1, LocalCut, SavecutGoal),
        Ann =: annotated_term{term:(AG1->AG2)},
	normalize_body(G1, AG1, BranchPos, CallNr1, CallNr2, LocalCut, Vs1, Vs2, Goals, Goals1, CM-any, CM, PM),
	Goals1 = [CuttoGoal|Goals2],
	same_call_pos(BranchPos, CallNr2, CallNr3, CallPos2),
	cutto_goal(CallPos2, Vs2, Vs3, DisjCut, CuttoGoal),
	normalize_body(G2, AG2, BranchPos, CallNr3, _CallNr, Cut, Vs3, Vs, Goals2, [], CM-any, CM, PM).
normalize_left_branch((G1;G2), Ann, DisjCallPos, BranchNr0, BranchNr, Cut, DisjCut, Vs0, Vs, Branches, Branches0, _LM, CM, PM) ?-
	% An if-then-else in the left alternative needs its own choicepoint!
	nonvar(G1), G1 \= (_ -> _),
	!,
	% This disjunction can be merged with the parent one
        Ann =: annotated_term{term:(AG1;AG2)},
        normalize_left_branch(G1, AG1, DisjCallPos, BranchNr0, BranchNr1, Cut, DisjCut, Vs0, Vs1, Branches, Branches1, CM-any, CM, PM),
	normalize_right_branch(G2, AG2, DisjCallPos, BranchNr1, BranchNr, Cut, DisjCut, Vs1, Vs, Branches1, Branches0, CM-any, CM, PM).
normalize_left_branch(G1, Ann, DisjCallPos, BranchNr0, BranchNr, Cut, _DisjCut, Vs0, Vs, [Goals|Branches], Branches, LM, CM, PM) :-
	% A normal (uncut) branch of the disjunction
	new_branch(DisjCallPos, BranchNr0, BranchNr, BranchPos),
	normalize_body(G1, Ann, BranchPos, 1, _CallNr, Cut, Vs0, Vs, Goals, [], LM, CM, PM).


normalize_right_branch((G1;G2), Ann, DisjCallPos, BranchNr0, BranchNr, Cut, LocalCut, Vs0, Vs, Branches, Branches0, _LM, CM, PM) ?- !,
	% This disjunction can be merged with the parent one
        Ann =: annotated_term{term:(AG1;AG2)},
        normalize_left_branch(G1, AG1, DisjCallPos, BranchNr0, BranchNr1, Cut, LocalCut, Vs0, Vs1, Branches, Branches1, CM-any, CM, PM),
	normalize_right_branch(G2, AG2, DisjCallPos, BranchNr1, BranchNr, Cut, LocalCut, Vs1, Vs, Branches1, Branches0, CM-any, CM, PM).
normalize_right_branch(G1, Ann, DisjCallPos, BranchNr0, BranchNr, Cut, _LocalCut, Vs0, Vs, [Goals|Goals0], Goals0, LM, CM, PM) :-
	new_branch(DisjCallPos, BranchNr0, BranchNr, Branch),
	normalize_body(G1, Ann, Branch, 1, _CallNr, Cut, Vs0, Vs, Goals, [], LM, CM, PM).


% Normalise a list of standard clauses, facts, etc

% normalize_clause_list(+Clauses, ?AnnClauses, -NormClause, +CM, +Vs0, -Vs)
normalize_clause_list([Clause], [AnnClause], NormClause, CM, Vs0, Vs) :- !,
	NormClause = [HeadMarker, SavecutGoal |NormClause1],
	same_call_pos([], 1, _CallNr, CallPos),
	savecut_goal(CallPos, Vs0, Vs1, Cut, SavecutGoal),
	normalize_clause(Clause, AnnClause, [], NormClause1, CM, Cut, Vs1, Vs2, HeadVars),
	head_marker(Clause, CM, CallPos, HeadVars, Vs2, Vs, HeadMarker).
normalize_clause_list(Clauses, AnnClauses, NormClauses, CM, Vs0, Vs) :-
	NormClauses = [
	    HeadMarker,
	    SavecutGoal, 
	    disjunction{callpos:CallPos,branches:NormBranches}
	],
	same_call_pos([], 1, CallNr1, CallPos0),
	savecut_goal(CallPos0, Vs0, Vs1, Cut, SavecutGoal),
	new_call_pos([], CallNr1, CallNr2, _CallPos),
	new_call_pos([], CallNr2, _CallNr, CallPos),
%	new_call_pos([], CallNr1, _CallNr, CallPos),
	(
	    foreach(Clause,Clauses),
            foreach(AnnClause,AnnClauses),
	    foreach(Goals,NormBranches),
	    fromto(1,BranchNr1,BranchNr2,_BranchNr),
	    fromto(Vs1,Vs2,Vs3,Vs4),
	    param(CallPos,CM,Cut,HeadVars)
	do
	    new_branch(CallPos, BranchNr1, BranchNr2, ClauseBranch),
	    normalize_clause(Clause, AnnClause, ClauseBranch, Goals, CM, Cut, Vs2, Vs3, HeadVars)
	),
	Clauses = [SomeClause|_],
	head_marker(SomeClause, CM, CallPos0, HeadVars, Vs4, Vs, HeadMarker).

    normalize_clause(Clause, AnnClause, Branch, Goals, CM, Cut, Vs0, Vs, HeadVars) :-
	clause_head_body(Clause, AnnClause, Head, Body, AnnHead, AnnBody, HeadType),
	same_call_pos(Branch, 1, CallNr, CallPos),
	normalize_head(HeadType, Head, AnnHead, CallPos, Goals, Goals1, Vs0, Vs1, HeadVars),
	normalize_body(Body, AnnBody, Branch, CallNr, _CallNr, Cut, Vs1, Vs, Goals1, [], CM-any, CM, CM).

    :- mode clause_head_body(+,?,-,-,-,-,-).
    clause_head_body((H0:- -?->B0), Ann, H, B, AH, AB, HeadType) ?- !,
        Ann =: annotated_term{term:(AH:-AnnMatch)},
        AnnMatch =: annotated_term{term:(-?->AB)},
	H=H0, B=B0, HeadType = (?=).
    clause_head_body((H0:-B0), Ann, H, B, AH, AB, HeadType) ?- !,
        Ann =: annotated_term{term:(AH:-AB)},
	H=H0, B=B0, HeadType = (=).
    clause_head_body((H0?-B0), Ann, H, B, AH, AB, HeadType) ?- !,
        Ann =: annotated_term{term:(AH?-AB)},
	H=H0, B=B0, HeadType = (?=).
    clause_head_body(H, AH, H, '', AH, '', =).


head_marker(Clause, CM, CallPos, HeadVars, Vs0, Vs, Goal) :-
	(
	    foreach(HeadVar,HeadVars),
	    foreach(HeadArg,HeadArgs),
	    fromto(Vs0,Vs1,Vs2,Vs)
	do
	    new_aux_variable(HeadVar, HeadArg, Vs1, Vs2)
	),
	Goal = goal{
	    kind:head, callpos:CallPos,
	    lookup_module:CM, definition_module:CM,
	    functor:N/A, args:HeadArgs},
    	clause_head_body(Clause, _, H, _, _, _, _),
	functor(H, N, A).


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
	normalize_term(CutVar, _, NormCutVar, Vs0, Vs1, =).



% Normalised term representation:
%
%	variables	struct(variable)
%	attr.variables	struct(attrvar) - only if AttrFlag == (?=) !
%	atomic		as such
%	lists		as such
%	structs		structure(structure)
%	ground struct	ground_structure(ground)   TODO

:- mode normalize_term(?,?,-,-,+,+).
normalize_term(X, AnnX, Desc, [X-VarDesc|Vs1], Vs0, AttrFlag) :-
	var(X), !,
	( nonvar(AnnX) ->
	    VarDesc = variable{source_info:AnnX}
	; get_var_info(X, name, Name) ->
	    VarDesc = variable{source_info:name(Name)}
	;
	    VarDesc = variable{source_info:none}
	),
	( meta(X), AttrFlag = (?=) ->
	    Desc = attrvar{variable:VarDesc,meta:NormMeta},
	    meta_attr_struct(X, Meta),
	    normalize_term(Meta, _Ann, NormMeta, Vs1, Vs0, AttrFlag)
	;
	    % treat as plain variable when not in matching-clause head
	    Desc = VarDesc, Vs1 = Vs0
	).
normalize_term([X|Xs], Ann, [Y|Ys], Vs, Vs0, AttrFlag) :- !,
	Ann =: annotated_term{term:[AnnX|AnnXs]},
	normalize_term(X, AnnX, Y, Vs, Vs1, AttrFlag),
	normalize_term(Xs, AnnXs, Ys, Vs1, Vs0, AttrFlag).
normalize_term(X, Ann, structure{name:N,arity:A,args:Args}, Vs, Vs0, AttrFlag) :-
	compound(X), !,
	Ann =: annotated_term{term:AnnX},
	functor(X, N, A),
	(
	    for(I,1,A),
	    foreach(NormArg,Args),
	    fromto(Vs,Vs2,Vs1,Vs0),
	    param(X,AnnX,AttrFlag)
	do
	    arg(I, X, Arg),
	    varg(I, AnnX, AnnArg),
	    normalize_term(Arg, AnnArg, NormArg, Vs2, Vs1, AttrFlag)
	).
normalize_term(X, _Ann, X, Vs, Vs, _AttrFlag) :- atom(X), !.
normalize_term(X, _Ann, X, Vs, Vs, _AttrFlag) :- number(X), !.
normalize_term(X, _Ann, X, Vs, Vs, _AttrFlag) :- string(X), !.
normalize_term(X, Ann, _, _, _, _) :-
	( nonvar(Ann) ->
	    Ann =: annotated_term{file:File,line:Line},
	    printf(error, "File %w, line %d: ", [File,Line])
	;
	    true
	),
	type_of(X, Type),
	printf(error, "Cannot compile term of type %w: %w%n", [Type,X]),
	exit_block(abort_compile_predicate).


% Build a structure meta/N with all attributes of X
meta_attr_struct(X, Meta) :-
	meta_attributes(X, 1, Attrs),
	Meta =.. [meta|Attrs].

    meta_attributes(X, I, Attrs) :-
	( meta_index(_Name,I) ->
	    get_attribute(X, Attr, I),
	    Attrs = [Attr|Attrs1],
	    I1 is I+1,
	    meta_attributes(X, I1, Attrs1)
	;
	    Attrs = []
	).


% Introduce a new, auxiliary source variable that was not in the
% original source, or an additional occurrence of a source variable.
% Treat occurrences like normal source variables.
new_aux_variable(X, VarDesc, [X-VarDesc|Vs], Vs) :-
	VarDesc = variable{source_info:none}.



% Fill in the variable{varid:} fields with a unique integer >= 1
% for every distinct variable.  Note that we have not yet separated
% identical variables that occur in parallel branches of a disjunction,
% that is taken care of in the variable classification pass.

assign_varids(Vs, _VarNames, N) :-
	keysort(Vs, SortedVs),
	(
	    foreach(X-variable{varid:I},SortedVs),
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
	keysort(Vs, SortedVs),
	sort(2, <, VarNames, SortedNames),
	assign_varids_names(SortedVs, _OtherVar, SortedNames, 0, N).

    assign_varids_names([], _X, _Names, I, I).
    assign_varids_names([X-variable{varid:I,source_name:Name}|List], X0, Names0, I0, IN) :-
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
%
% p(a) :- ...	is normalised into p(T) :- T?=a, ...
% p(X,X) :- ...	is normalised into p(X,T) :- X=T, ...
% p(X,X) ?- ...	is normalised into p(X,T) :- X==T, ...
% p(X{A}) ?- ... is normalised into into p(X) :- X?=X{A}, ...
% p(X{A},X{A}) ?- ... is normalised into p(X,T) :- X?=X{A}, T==X, ...
%
% attrvar{} descriptors are only created on the rhs of ?=/2, in all other
% locations, we use simple variable{} descriptors for attributed variables.

normalize_head(HeadType, Head, AnnHead, CallPos, Goals1, Goals, Vs0, Vs, HeadVars) :-
	AnnHead =: annotated_term{term:HeadAnn},
	(
	    foreacharg(Arg,Head,I),
	    fromto(Vs0,Vs1,Vs3,Vs),
	    fromto([],Seen1,Seen2,_),
	    fromto(Goals1,Goals2,Goals3,Goals),
	    foreach(HeadVar,HeadVars),
	    param(HeadType,HeadAnn,CallPos)
	do
	    Goal = goal{
		    kind:simple,
		    path:File,
		    line:Line,
		    from:From,
		    to:To,
		    callpos:CallPos,
		    definition_module:sepia_kernel,
		    lookup_module:sepia_kernel,
		    functor:Op/2,
		    args:[HeadArg,NormArg]
		},
	    varg(I, HeadAnn, AnnArg),
	    ann_location(AnnArg, File, Line, From, To),
	    ( nonvar(Arg) ->
		% p(nonvar) :-  becomes  p(T) :- T=nonvar
		% p(nonvar) ?-  becomes  p(T) :- T?=nonvar
		Goals2 = [Goal|Goals3],
		normalize_term(Arg, AnnArg, NormArg, Vs1,Vs2, HeadType),
		Seen2 = Seen1,
		Op = HeadType,
		new_aux_variable(HeadVar, HeadArg, Vs2, Vs3)
	    ; varnonmember(Arg,Seen1) ->
		Seen2 = [Arg|Seen1],
		( meta(Arg), HeadType = (?=) ->
		    % p(X{A}) ?-  becomes  p(X) :- X?=X{A}
		    Goals2 = [Goal|Goals3],
		    Op = HeadType,
		    normalize_term(Arg, AnnArg, NormArg, Vs1, Vs3, ?=),
		    NormArg = attrvar{variable:HeadArg},
		    HeadVar = Arg
		;
		    % Don't create a new variable for the first occurrence.
		    HeadVar = Arg,
		    normalize_term(Arg, AnnArg, HeadArg, Vs1, Vs3, =),
		    Goals2 = Goals3
		)
	    ;
		% repeat occurence: T=X (or T==X for matching)
		Goals2 = [Goal|Goals3],
		normalize_term(Arg, AnnArg, NormArg, Vs1,Vs2, =),
		Seen2 = Seen1,
		headtype_varop(HeadType, Op),
		new_aux_variable(HeadVar, HeadArg, Vs2, Vs3)
	    )
	).

headtype_varop(=, =).
headtype_varop(?=, ==).

varnonmember(_X, []).
varnonmember(X, [Y|Ys]) :-
	X \== Y,
	varnonmember(X, Ys).


% Flatten unifications and normalise them such that there is always
% a variable on the left hand side. The sub-unifications inherit the
% source annotation from the original unification goal.
simplify_unification(X=Y, Ann, UnifGoals, AnnUnifGoals) ?-
	Ann =: annotated_term{term:(AnnX=AnnY)},
	simplify_unification(X, Y, AnnX, AnnY, Ann, UnifGoals, [], AnnUnifGoals, []).


%:- mode simplify_unification(?,?,?,?,?,-,?,-,?).
simplify_unification(X, Y, AnnX, AnnY, Ann, [X=Y|T], T, [AnnEq|AT], AT) :-
	var(X), !,
	ann_update_term(AnnX=AnnY, Ann, AnnEq).
simplify_unification(X, Y, AnnX, AnnY, Ann, Us, Us0, AnnUs, AnnUs0) :-
	var(Y), !,
	simplify_unification(Y, X, AnnY, AnnX, Ann, Us, Us0, AnnUs, AnnUs0).
simplify_unification(X, Y, _AnnX, _AnnY, _Ann, Us, Us, AnnUs, AnnUs) :-
	X == Y, !.
simplify_unification(X, Y, AnnX, AnnY, Ann, L, L0, AnnL, AnnL0) :-
	functor(X, F, N), functor(Y, F, N), !,
	AnnX =: annotated_term{term:XAnn},
	AnnY =: annotated_term{term:YAnn},
	(
	    for(I,1,N),
	    fromto(L,L1,L2,L0),
	    fromto(AnnL,AnnL1,AnnL2,AnnL0),
	    param(X,Y,XAnn,YAnn,Ann)
	do
	    arg(I, X, AX),
	    arg(I, Y, AY),
	    varg(I, XAnn, AnnAX),
	    varg(I, YAnn, AnnAY),
	    simplify_unification(AX, AY, AnnAX, AnnAY, Ann, L1, L2, AnnL1, AnnL2)
	).
simplify_unification(_X, _Y, _AnnX, _AnnY, Ann, [fail|T], T, [AnnF|AT], AT) :-
	ann_update_term(fail, Ann, AnnF).


/*
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
*/

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
print_normalized_goal(Stream, Indent, disjunction{determinism:Det,arity:A,
		args:Args,indexvars:IndexVars,callpos:P,branches:Bs,branchheadargs:BHA}) :-
	indent(Stream, Indent),
	printf(Stream, "DISJ/%w  (%w, callpos:", [A,Det]),
	print_call_pos(Stream, P),
	writeln(Stream, ")"),
	ArgIndent is Indent+1,
	( foreach(Arg,Args), param(Stream,ArgIndent) do
	    indent(Stream, ArgIndent), writeln(Stream, Arg)
	),
	indent(Stream, Indent),
	writeln(Stream, "INDEXES:"),
	( foreach(Arg,IndexVars), param(Stream,ArgIndent) do
	    indent(Stream, ArgIndent), writeln(Stream, Arg)
	),
	( foreach(Branch,Bs), fromto("TRY",T,"RETRY",_), count(BranchI,1,_), param(Stream,Indent,P,A,BHA) do
	    Indent1 is Indent+1,
	    indent(Stream, Indent),
	    printf(Stream, "%w/%w  (callpos:", [T,A]),
	    append(P, [BranchI], PB),
	    print_call_pos(Stream, PB),
	    writeln(Stream, ")"),
	    ArgIndent is Indent1+1,
	    ( integer(A), A>0 ->
		arg(BranchI, BHA, Args),
		( foreach(Arg,Args), param(Stream,ArgIndent) do
		    indent(Stream, ArgIndent), writeln(Stream, Arg)
		)
	    ;
		true
	    ),
	    print_normalized_goal(Stream, Indent1, Branch)
	),
	indent(Stream, Indent),
	write(Stream, "JOIN  (callpos:"),
	print_call_pos(Stream, P),
	writeln(Stream, ")").
print_normalized_goal(Stream, Indent, goal{kind:K,callpos:P,state:State,
		lookup_module:LM, functor:F,args:Args,envmap:EAM,envsize:ESize,
                path:Path,line:Line}) :-
	indent(Stream, Indent),
	( K == head -> write(Stream, "HEAD") ; write(Stream, "GOAL") ),
        printf(Stream, "  %w  (lm:%w, kind:%w, path:%w, line:%w callpos:", [F,LM,K,Path,Line]),
	print_call_pos(Stream, P),
	decode_activity_map(EAM,Env),
	printf(Stream, ", env:%w@%w)%n", [ESize,Env]),
	ArgIndent is Indent+1,
	( foreach(Arg,Args), param(Stream,ArgIndent) do
	    indent(Stream, ArgIndent), writeln(Stream, Arg)
	),
	print_goal_state(Stream, Indent, State).