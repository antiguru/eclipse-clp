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
% Version:	$Id: compiler_indexing.ecl,v 1.1 2006/09/23 01:45:09 snovello Exp $
%----------------------------------------------------------------------

:- module(compiler_indexing).

:- comment(summary, "ECLiPSe III compiler - indexing").
:- comment(copyright, "Cisco Technology Inc").
:- comment(author, "Joachim Schimpf").
:- comment(date, "$Date: 2006/09/23 01:45:09 $").

:- use_module(compiler_common).
:- use_module(compiler_analysis).

:- comment(desc, ascii("
   This pass finds information that can be exploited for indexing (i.e.
   filtering alternatives from disjunctions). The disjunctions are annotated
   with this information, and a pseudo-goal (struct indexpoint{}) is inserted
   into the normalised code, at the beginning of the first alternative of
   every indexable disjunction.

   The code generator uses this information to generate switch-instructions
   and try-sequences.
")).


% Structure describing a guard test:
% Specifies which values of a variable the guard will accept.
% Cut (!) is encoded as a pseudo-guard with varid:0,class:[]
:- local struct(guard(
    	branchnr,	% branch in which this guard occurs
	varid,		% variable that this guard tests (or 0)
	class		% list of: atomic Tag name, value(Val,Tag) or N/A
    )).


:- export indexing_transformation/2.

indexing_transformation([], []).
indexing_transformation([Goal|Goals], OutGoals0) :-
	( Goal = disjunction{branches:Branches} ->
	    update_struct(disjunction, [branches:OutBranches], Goal, OutGoal),
	    index_disjunction(Goal, IndexPoint),
	    OutGoals0 = [OutGoal|OutGoals4],
	    (
		foreach(Branch,Branches),
		foreach(OutBranch,OutBranches0)
	    do
		indexing_transformation(Branch, OutBranch)
	    ),
	    % we insert this marker into the goal sequence mainly for
	    % variable classification purposes
	    OutBranches0 = [OutBranch1|OutBranches2N],
	    insert_after_head(IndexPoint, OutBranch1, IndexedOutBranch1),
	    OutBranches = [IndexedOutBranch1|OutBranches2N]
	;
	    OutGoals0 = [Goal|OutGoals4]
	),
	indexing_transformation(Goals, OutGoals4).

    insert_after_head(IndexPoint, Branch, IndexedBranch) :-
	( Branch = [Head|RestOfBranch], Head = goal{kind:head} ->
	    IndexedBranch = [Head,IndexPoint|RestOfBranch]
	;
	    IndexedBranch = [IndexPoint|Branch]
	).
    	

index_disjunction(Disjunction, IndexPoint) :-
	Disjunction = disjunction{callpos:CallPos,branches:Branches,branchlabels:BranchLabelArray,state:StartState,index:IndexPoint},
	IndexPoint = indexpoint{callpos:IndexCallPos,args:Args,indexes:OrderedIndexes,disjunction:Disjunction},
	new_branch(CallPos, 1, _, FirstBranch),
	same_call_pos(FirstBranch, 1, _, IndexCallPos),
	(
	    count(I,1,NBranches),
	    foreach(Branch,Branches),
	    foreach(GuardInfo,GuardsByBranch),
	    param(StartState)
	do
	    extract_guards_from_prefix(Branch, I, StartState, GuardInfo, [])
	),
	dim(BranchLabelArray, [NBranches]),
	% GuardsByBranch list of (list of guards for each branch)
	get_index_classes(GuardsByBranch, ClassesByVarids),

	% compute an index for each interesting variable VarId
	% ClassesByVarids is a list of Varid-ValueClasses
	(
	    foreach(VarId-Classes,ClassesByVarids),
	    foreach(VarDesc,Args),
	    foreach(index{variable:VarDesc,partition:VaridClassBranches,other:VarIdDefaultBranches},Indexes),
	    param(GuardsByBranch)
	do
	    % create variable access descriptor (must be done before
	    % compute_lifetimes) for use in generate_code later
	    new_vardesc(VarId, VarDesc),
	    % compute possible branches for each value-class
	    (
		foreach(Class,Classes),
		foreach(Class-Branches,VaridClassBranches),
		param(VarId,GuardsByBranch)
	    do
		possible_branches(VarId, Class, GuardsByBranch, Branches, [], 1)
	    ),
	    % compute possible branches for values outside any value-class
	    possible_branches(VarId, other, GuardsByBranch, VarIdDefaultBranches, [], 1)
	),
	eval_index_quality(Indexes, NBranches),
	sort(quality of index, =<, Indexes, OrderedIndexes).


possible_branches(_VarId, _Class, [], Branches, Branches, _I).
possible_branches(VarId, Class, [BranchGuard|BranchGuards], Branches, Branches0, I) :-
	I1 is I+1,
	passes_guard(VarId, Class, BranchGuard, Result, Commit),
	( Result == false ->
	    % guard definitely fails
	    possible_branches(VarId, Class, BranchGuards, Branches, Branches0, I1)
	; Commit == true ->
	    % guard may succeed and is definitely cut
	    Branches = [I|Branches0]
	;
	    % guard may succeed and may or may not be cut
	    Branches = [I|Branches1],
	    possible_branches(VarId, Class, BranchGuards, Branches1, Branches0, I1)
	).



extract_guards_from_prefix([], _BranchNr, _StartState, Info, Info).
extract_guards_from_prefix([Goal|Goals], BranchNr, StartState, Info, Info0) :-
	(
	    % consider only builtin predicates
	    % caution: regular preds can wake (and fail!)
	    Goal = goal{kind:simple,definition_module:sepia_kernel},
	    extract_guards_from_goal(Goal, BranchNr, StartState, Info, Info1)
	->
	    extract_guards_from_prefix(Goals, BranchNr, StartState, Info1, Info0)

	; Goal = goal{kind:head,state:HeadState} ->
	    % Use the head's binding information instead of what was known
	    % prior to the disjunction
	    extract_guards_from_prefix(Goals, BranchNr, HeadState, Info, Info0)
%	    extract_guards_from_prefix(Goals, BranchNr, StartState, Info, Info0)
	;
	    % end of guard
	    Info = Info0
	).

	
% PRE: the goal is a builtin from sepia_kernel.
% Fail if encountering a goal that signals end-of-guard.
% Regular goal can cause waking (and therefore insert failures).
% StartState is the analysis state at the beginning of the disjunction.

extract_guards_from_goal(goal{functor:get_cut/1}, _BranchNr, _StartState, Info, Info) :- !.
extract_guards_from_goal(goal{functor:cut_to/1},
		BranchNr, _StartState, Info, Info0) :- !,
	% ugly: indicate commit by varid0 and class []
	Info = [guard{branchnr:BranchNr,varid:0,class:[]}|Info0].
extract_guards_from_goal(goal{functor:(=)/2, args:[Lhs,Rhs]},
		BranchNr, StartState, Info, Info0) :- !,
	% unifications should be normalised and always
	% have a variable on the left hand side
	certainly_once Lhs = variable{varid:VarId},
	% state_lookup_binding should succeed iff the variable was known
	% before the start of the disjunction
	( state_lookup_binding(StartState, VarId, _Binding) ->
	    ( atomic_tag(Rhs, Tag) ->
		( value_indexable(Tag) ->
		    Info = [guard{branchnr:BranchNr,varid:VarId,class:[var,value(Rhs,Tag)]}|Info0]
		;
		    Info = [guard{branchnr:BranchNr,varid:VarId,class:[var,Tag]}|Info0]
		)
	    ; Rhs = structure{name:F,arity:A} ->
		Info = [guard{branchnr:BranchNr,varid:VarId,class:[var,F/A]}|Info0]
	    ; Rhs = [_|_] ->
		Info = [guard{branchnr:BranchNr,varid:VarId,class:[var,list]}|Info0]
	    ; verify Rhs = variable{},
		Info = Info0
	    )
	;
	    % nothing known about the variable, ignore
	    Info = Info0
	).
extract_guards_from_goal(goal{functor:(==)/2, args:[Lhs,Rhs]},
		BranchNr, StartState, Info, Info0) :- !,
	( Lhs = variable{varid:VarId}, Rhs \= variable{} ->
	    extract_guards_from_identity(VarId, Rhs, BranchNr, StartState, Info, Info0)
	; Rhs = variable{varid:VarId}, Lhs \= variable{} ->
	    extract_guards_from_identity(VarId, Lhs, BranchNr, StartState, Info, Info0)
	;
	    Info = Info0
	).
extract_guards_from_goal(goal{
    		functor:Test/1, args:[variable{varid:VarId}] },
		BranchNr, StartState, Info, Info0) :-
	type_test(Test, Classes),
	!,
	( state_lookup_binding(StartState, VarId, _Binding) ->
	    Info = [guard{branchnr:BranchNr,varid:VarId,class:Classes}|Info0]
	;
	    % nothing known about the variable, ignore
	    Info = Info0
	).

    % For the ==/2 predicate, matching, etc
    extract_guards_from_identity(VarId, Rhs, BranchNr, StartState, Info, Info0) :-
	% state_lookup_binding should succeed iff the variable was known
	% before the start of the disjunction
	( state_lookup_binding(StartState, VarId, _Binding) ->
	    ( atomic_tag(Rhs, Tag) ->
		( value_indexable(Tag) ->
		    Info = [guard{branchnr:BranchNr,varid:VarId,class:[value(Rhs,Tag)]}|Info0]
		;
		    Info = [guard{branchnr:BranchNr,varid:VarId,class:[Tag]}|Info0]
		)
	    ; Rhs = structure{name:F,arity:A} ->
		Info = [guard{branchnr:BranchNr,varid:VarId,class:[F/A]}|Info0]
	    ; verify Rhs = [_|_],
		Info = [guard{branchnr:BranchNr,varid:VarId,class:[list]}|Info0]
	    )
	;
	    % nothing known about the variable, ignore
	    Info = Info0
	).


% The tags that have switch_on_value instructions
:- mode value_indexable(+).
value_indexable(integer).
value_indexable(atom).
value_indexable(structure).

% Compute the tag of a value
:- mode atomic_tag(+,-).
atomic_tag(X, Class) :- integer(X),
	( sepia_kernel:bignum(X) -> Class=bignum ; Class=integer).
atomic_tag([], '[]') :- !.
atomic_tag(X, atom) :- atom(X).
atomic_tag(X, breal) :- breal(X).
atomic_tag(X, double) :- float(X).
atomic_tag(X, rational) :- rational(X).
atomic_tag(X, handle) :- is_handle(X).	% can't occur in textual source
atomic_tag(X, string) :- string(X).


% Compute the tag sets resulting from various type tests
type_test(atom,		[atom,[]]).
type_test(atomic,	[[],atom,bignum,breal,double,handle,integer,rational,string]).
type_test(bignum,	[bignum]).
type_test(breal,	[breal]).
type_test(compound,	[list,structure]).
type_test(float,	[double]).
%type_test(free,		[free]).
type_test(ground,	[[],atom,bignum,breal,list,structure,double,handle,integer,rational,string]).	% not only tag test
type_test(integer,	[bignum,integer]).
type_test(is_handle,	[handle]).
type_test(is_list,	[[],list]).	% not only tag test!
type_test(is_suspension, [[],list]).	% not only tag test!
%type_test(meta,		[meta]).
type_test(nonvar,	[[],atom,bignum,breal,list,structure,double,handle,integer,rational,string]).
type_test(number,	[bignum,breal,double,integer,rational]).
type_test(rational,	[rational]).
type_test(real,		[breal,double]).
type_test(string,	[string]).
%type_test(var,		[free,meta]).
type_test(var,		[var]).


% compute a list Varid-ListOfValueClasses

get_index_classes(GuardsByBranch, ClassesByVarids) :-
	(
	    foreach(BranchGuards,GuardsByBranch)
		>> foreach(guard{varid:VarId,class:Classes},BranchGuards)
		    >> (foreach(Class,Classes), param(VarId)),
	    fromto(VaridClasses,VaridClasses1,VaridClasses0,[])
	do
	    ( VarId == 0 -> VaridClasses1=VaridClasses0
	    ; VaridClasses1 = [VarId-Class|VaridClasses0] )
	),
	sort(0, <, VaridClasses, SortedUniqVaridClasses),
	group_same_key_values(SortedUniqVaridClasses, ClassesByVarids).


% passes_guard(VarId, Class, GuardInfo, Result, Commit)
% Computes the outcome of the guard for the case that variable
% VarId has value Class. Result is true, false or maybe.
% Commit says whether a cut is reached (also true, false or maybe).

passes_guard(VarId, Class, GuardInfo, Result, Commit) :-
	(
	    foreach(guard{varid:GVarId,class:GClasses},GuardInfo),
	    fromto(true,Result0,Result1,Result),
	    fromto(false,Commit0,Commit1,Commit),
	    param(VarId,Class)
	do
	    ( VarId == GVarId ->
		( foreach(GClass,GClasses),fromto(false,R0,R1,R),param(Class) do
		    guard_test(Class, GClass, GR),
		    guard_or(R0, GR, R1)
		),
		Commit1=Commit0

	    ; GVarId == 0 ->	% a commit
		R = true,
		guard_or(Commit0, Result0, Commit1)

	    ;			% an unrelated guard
		R = maybe,
		Commit1=Commit0
	    ),
	    guard_and(Result0, R, Result1)
	).
	

% guard_test(+VariableHasClass, +GuardAllowsClass, -Result)
:- mode guard_test(+,+,-).
guard_test(other, _, false) :- !.
guard_test(F/N, F/N, maybe) :- !.
guard_test(list, list, maybe) :- !.
guard_test(Class, Class, true) :- !.
guard_test(value(_,Class), Class, true) :- atom(Class), !.
guard_test(Class, value(_,Class), maybe) :- atom(Class), !.
guard_test(_/_, structure, true) :- !.
guard_test(structure, _/_, maybe) :- !.
guard_test(_, _, false).

guard_and(true, true, true) :- !.
guard_and(false, _, false) :- !.
guard_and(_, false, false) :- !.
guard_and(_, _, maybe).

guard_or(false, false, false) :- !.
guard_or(true, _, true) :- !.
guard_or(_, true, true) :- !.
guard_or(_, _, maybe).



% Evaluate index quality: A positive float, the smaller the better.
% Roughly computes fan-out (number of alternatives jumped to)
% divided by fan-in (number of different argument values tested for)

eval_index_quality(Indexes, _NBranches) :-	
	(
	    foreach(index{quality:Q,partition:ClassesBranches,other:DefaultBranches},Indexes)
	do
	    % collect all occurring sets of alternatives
	    % TODO: remove all value entries for non-switchable types (float etc)
	    ( memberchk(var-_, ClassesBranches) ->
		strip_keys(ClassesBranches, Branches0),
		Branches = [DefaultBranches|Branches0]
	    ;
		strip_keys(ClassesBranches, Branches0),
		Branches = [DefaultBranches,[]|Branches0]

	    ),
	    % remove duplicate sets
	    sort(Branches, BranchesSets),
	    flatten(BranchesSets, AllTargetBranches),
	    % This is the quality measure
	    Q is length(AllTargetBranches)/length(BranchesSets)
	).



dump_indexes(_CallPos, []).
dump_indexes(CallPos, Indexes) :- Indexes = [_|_],
	printf("INDEXES for disjunction %w%n", [CallPos]),
	(
	    count(I,1,_),
	    foreach(index{quality:Q,variable:variable{varid:VarId},partition:Parts,other:Other},Indexes)
	do
	    printf("%d. Quality %.1f, variable %d%n", [I,Q,VarId]),
	    ( foreach(Part,Parts) do
		printf("    %w%n", [Part])
	    ),
	    printf("    %w%n", [other-Other])
	).

