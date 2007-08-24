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
% Version:	$Id: compiler_varclass.ecl,v 1.5 2007/08/24 21:50:51 jschimpf Exp $
%
% Related paper (although we haven't used any of their algorithms):
% H.Vandecasteele,B.Demoen,G.Janssens: Compiling Large Disjunctions
% KU Leuven 2001
% ----------------------------------------------------------------------


:- module(compiler_varclass).

:- comment(summary, "ECLiPSe III compiler - variable classification").
:- comment(copyright, "Cisco Technology Inc").
:- comment(author, "Joachim Schimpf").
:- comment(date, "$Date: 2007/08/24 21:50:51 $").

:- comment(desc, ascii("
    This pass (actually two passes: compute_lifetimes and assign_env_slots)
    takes as input a normalised predicate and finds out

    - how many distinct variables there actually are (variables with the same
      name occurring in parallel or-branches are considered distinct)

    - classifies variables into void, temp, or permanent

    - assigns environment slots to the permanent variables

    - compute the size of environment needed

    - computes 'environment activity map' for each call position

    - makes singleton warnings

    Note that, in this context, we talk about 'first' and 'last' occurrences
    only with a granularity of 'call positions', i.e. all occurrences of a
    variable in the first/last chunk it occurs in are considered (and marked
    as) 'first'/'last'.
    That way, later compiler stages are still free to reorder operations
    within each chunk without affecting this variable classification.
")).


:- lib(m_map).

:- use_module(compiler_common).


% struct(slot) describes one true, distinct variable. There may be more
% of those than there are variables in the source, because we classify
% variables in parallel disjunctive branches as being distinct.

:- local struct(slot(		% one for every truly distinct variable
	source_info,		% for error messages only
	firstpos,		% position of first occurrence
				% (must be first for sorting!)
	lastpos,		% position of last occurrence
	lastflag,		% shared isalast-flag of all last occurrences
	class			% shared with all occurrences (struct(variable))
    )).

:- comment(struct(slot), [
    summary:"Temporary data structure during computation of lifetimes",
    fields:[
	firstpos:"call position of first variable occurrence",
	lastpos:"call position of last variable occurrence",
	lastflag:"shared isalast-flag of all last variable occurrences",
	class:"shared class-field of all variable occurrences"
    ],
    see_also:[struct(variable)]
]).

% Maybe we could speed up processing by sharing the variable descriptors
% for each chunk, and keeping them separately. This would benefit the passes
% compute_lifetimes and assign_env_slots - they would not have to deal
% with multiple occurrences in the same chunk.


%----------------------------------------------------------------------
% Variable lifetimes and detection of false sharing
%
% We build a map that stores for each variable the first and
% last occurrences (in terms of call positions).
%
% Also, the individual occurrences are marked as first and/or last
% but only with a granularity of call positions (i.e. all occurrences
% in the chunk are marked). The exact first occurrence is determined
% later when generating code for the chunk. This has the advantage
% that everything within the chunk can still be reordered after this
% pass.
%
% Because of disjunctive branches, there can be more than one
% first and last occurrence of each variable. Moreover, variables
% with the same name in different branches are really different
% variables, so this pass finds out how many different variables
% there really are.
%
% The disjunctions are conceptually traversed in parallel,
% When joining up, we merge the branches's maps into one.
% 
% Data structures:
%    Variable occurrence:
%	variable(VarId, IsAFirst, IsALast, ClassAndPermLocation)
%    Maintain map of:
%	VarId - [slot(FirstPos,LastPos,LastFlag,Location), ...]
%		one slot for each truly distinct variable
% 
% The two interesting operations are
%
%	- registering a new occurrence of a variable
%	- merging the information when disjunctive branches join up
%
%
% TODO: could keep slot lists in reverse order wrt firstpos,
% then they could be merged more efficiently.
%----------------------------------------------------------------------

:- comment(compute_lifetimes/2, [
    summary:"Compute variable lifetimes and detection of false sharing",
    amode:compute_lifetimes(+,-),
    args:[
	"Body":"Normalised predicate",
	"Lifetimes":"A map varid->struct(slot)"
    ],
    see_also:[print_occurrences/1]
]).

:- export compute_lifetimes/2.

compute_lifetimes(Body, Lifetimes) :-
	m_map:init(Lifetimes0),
	compute_lifetimes(Body, Lifetimes0, Lifetimes),
	set_last_occurrence_flags(Lifetimes).
	
    set_last_occurrence_flags(Lifetimes) :-
    	m_map:values(Lifetimes, SlotsList),
	( foreach(Slots,SlotsList) do
	    ( foreach(Slot,Slots) do
	    	Slot = slot{lastflag:last}
	    )
	).

compute_lifetimes([], Map, Map).
compute_lifetimes([Goal|Goals], Map0, Map) :-
	compute_lifetimes(Goal, Map0, Map1),
	compute_lifetimes(Goals, Map1, Map).
compute_lifetimes(disjunction{branches:Branches,callpos:DisjPos}, Map0, Map) :-
	(
	    foreach(Branch,Branches),
	    foreach(BranchMap,BranchMaps),
	    param(Map0)
	do
	    compute_lifetimes(Branch, Map0, BranchMap)
	),
	merge_branches(DisjPos, BranchMaps, Map).
compute_lifetimes(goal{callpos:GoalPos,args:Args}, Map0, Map) :-
	compute_lifetimes_term(GoalPos, Args, Map0, Map).
compute_lifetimes(indexpoint{callpos:CallPos,args:Args}, Map0, Map) :-
	compute_lifetimes_term(CallPos, Args, Map0, Map).

    compute_lifetimes_term(CallPos, [X|Xs], Map0, Map) :-
	compute_lifetimes_term(CallPos, X, Map0, Map1),
	compute_lifetimes_term(CallPos, Xs, Map1, Map).
    compute_lifetimes_term(CallPos, Occurrence, Map0, Map) :-
	Occurrence = variable{},
	register_occurrence(CallPos, Occurrence, Map0, Map).
    compute_lifetimes_term(CallPos, attrvar{variable:Avar,meta:Meta}, Map0, Map) :-
	compute_lifetimes_term(CallPos, Avar, Map0, Map1),
	compute_lifetimes_term(CallPos, Meta, Map1, Map).
    compute_lifetimes_term(CallPos, structure{args:Args}, Map0, Map) :-
	compute_lifetimes_term(CallPos, Args, Map0, Map).
    compute_lifetimes_term(_CallPos, Term, Map, Map) :- atomic(Term).


% When encountering a variable VarId at CallPos:
% 
% VarId not seen at all so far:
%	- add new slot entry
%	- it's the first and last occurrence
%
% VarId has one slot:
%	- it's a new last ocurrence of that variable
%	- update the slot's last-information
%
% VarId has multiple slots:
%	- the new occurrence means the multiple slots must be merged
%	- the summary slot takes the common prefix of all first occurrences
%	- the current occurrence is the last
%	- the locations are all unified
 
register_occurrence(CallPos, Occurrence, Map0, Map) :-
	Occurrence = variable{source_info:Source,varid:VarId,isalast:LastFlag,class:Location},
	( m_map:search(Map0, VarId, OldEntry) ->
	    OldEntry = [OldSlot|Slots],
	    OldSlot = slot{firstpos:FP0,lastpos:LP0,lastflag:LF0,class:Location},
	    merge_slots(Slots, FP0, FP, Location),
	    update_struct(slot, [firstpos:FP,lastpos:CallPos,lastflag:LastFlag], OldSlot, NewSlot),
	    % check for multiple first occurrences
	    ( CallPos = FP -> Occurrence = variable{isafirst:first} ; true ),
	    % check for multiple last occurrences
	    ( CallPos = LP0 -> LastFlag = LF0 ; true ),
	    m_map:det_update(Map0, VarId, [NewSlot], Map)
	;
	    % first occurrence
	    m_map:det_insert(Map0, VarId, [slot{source_info:Source,firstpos:CallPos,
	    	lastpos:CallPos,lastflag:LastFlag,class:Location}], Map),
	    Occurrence = variable{isafirst:first}
	).

    % - unifies all the slot's class fields
    % - computes the common prefix for the first position
    merge_slots([], FP, FP, nonvoid(_)).
    merge_slots([slot{firstpos:ThisFP,class:Location}|Slots], FP0, FP, Location) :-
    	common_pos(ThisFP, FP0, FP1),
	merge_slots(Slots, FP1, FP, Location).


% Merge the slot information from the branches:
% 
% The maps from the different branches may contain (for a particular VarId):
%
% all first occurrence(s) in current disjunction:
%			---C1--C2--	C1-C2
%	---------------|				-> C1-C2,D1-D2
%			---D1--D2--	D1-D2
%	keep all (they are different)
%
% common, identical entries:
%			-----------	A1-A2
%	---A1--A2------|				-> A1-A2
%			-----------	A1-A2
%	first and last occurrence are older than the disjunction
%	we keep one of them (they are all the same).
% 
% multiple entries, last occurrences older than current disjunction:
%	---A1--A2--	-----------	A1-A2,B1-B2
%		   |---|				-> A1-A2,B1-B2
%	---B1--B2--	-----------	A1-A2,B1-B2
%	keep one of each (they are different)
% 
% first occurrence older, last in current disjunction:
% some branches will still have old-old entry
%			-----C-----	A1-C
%	---A1--A2------|				-> A1-CD
%			-----------	A1-A2
%	where CD is the end of disjunction's callpos (C<CD)
%
% first occurrence older, last in current disjunction:
% some branches may still have old-old entry
%			-----C-----	A1-C
%	---A1--A2------|				-> A1-CD
%			-----D-----	A1-D
%	where CD is the end of disjunction's callpos (C<CD,D<CD)
%
% first occurrences older, last in current disjunction:
% some branches will still have multiple old-old entries
%	---A1--A2--	-----C-----	AB-C
%		   |---|				-> AB-CD
%	---B1--B2--	-----------	A1-A2,B1-B2
%	where CD is the end of disjunction's callpos (C<CD)
%	and AB the common prefix of the first occurrences (AB<A1,AB<B1).
%
% first occurrence older, last _is_ current disjunction:
% some branches may still have multiple old-old entries
%	---A1--A2--	-----C-----	AB-C
%		   |---|				-> AB-CD
%	---B1--B2--	-----D-----	AB-D
%	where CD is the end of disjunction's callpos (C<CD,D<CD)
%	and AB the common prefix of the first occurrences (AB<A1,AB<B1).
%
% entries with common first and different last occurrences:
%	- first occurrence is older than the disjunction!
%	- summarise them into one entry (by taking the common prefix of the
%	last occurrences, unifying the class, and unifying the isalast
%	flags)
%
% entries whose first occurrence differs:
%	- the first occurrence may be in this or in an earlier disjunction!
%	- keep them both, they represent conceptually different variables.

merge_branches(DisjPos, BranchMaps, MergedMap) :-
	(
	    foreach(Map,BranchMaps),
	    fromto(Lists, [MapList|Lists1], Lists1, Tail)
	do
	    m_map:to_sorted_assoc_list(Map, MapList)
	),
	merge_sorted_lists(Lists, Tail, MergedList),
	concat_same_key_values_unstable(MergedList, GroupedList),
	(
	    foreach(VarId-Slots,GroupedList),
	    foreach(VarId-NewSlots,NewGroupedList),
	    param(DisjPos)
	do
	    % remove duplicates AND sort by ascending firstpos
	    sort(Slots, SortedNoDupSlots),
	    SortedNoDupSlots = [slot{firstpos:OldestFirst}|_],
	    (
		compare_pos(OldestFirst, DisjPos, Res),
		verify Res = (<),
	    	slots_ending_ge(DisjPos, SortedNoDupSlots, SlotsEnteringDisj),
		SlotsEnteringDisj = [Slot1|_]
	    ->
		% unify lastflags
		( foreach(slot{lastflag:LF},SlotsEnteringDisj), param(LF) do
		    true
		),
		% replace with a single summary slot
		append(DisjPos, [?,?], DisjEndPos),
		update_struct(slot, [firstpos:OldestFirst,
			lastpos:DisjEndPos,lastflag:LF], Slot1, NewSlot),
		NewSlots = [NewSlot]
	    ;
	    	% all occurrences in current disjunction
	    	% or all before current disjunction
		NewSlots = SortedNoDupSlots
	    )
	),
	m_map:from_sorted_assoc_list(NewGroupedList, MergedMap).


    slots_ending_ge(_Pos, [], []).
    slots_ending_ge(Pos, [Slot|Slots], SlotsGe) :-
	Slot = slot{lastpos:LP},
	( compare_pos(LP, Pos, Res) ->
	    verify Res = (<),
	    slots_ending_ge(Pos, Slots, SlotsGe)
	;
	    SlotsGe = [Slot|SlotsGe1],
	    slots_ending_ge(Pos, Slots, SlotsGe1)
	).


:- comment(print_occurrences/1, [
    summary:"Debugging: print result of variable lifetime analysis",
    amode:print_occurrences(+),
    args:[
	"Lifetimes":"A map varid->struct(slot)"
    ],
    see_also:[compute_lifetimes/2]
]).

:- export print_occurrences/1.

print_occurrences(Map) :-
	writeln("------ Variable Lifetimes ------"),
	m_map:count(Map, N),
	( for(VarId,1,N), param(Map) do
	    m_map:lookup(Map, VarId, Slots),
	    printf("Variable #%d:%n", [VarId]),
	    ( foreach(Slot,Slots) do printf("  %w%n", [Slot]) ),
	    nl
	).


%----------------------------------------------------------------------
% This pass does:
% - Variable classification (void, temp, perm)
% - Environment slot allocation
% - Environment size computation:
%	-1  no environment needed
%	 0  empty environment needed
%	>0  environment of given size needed
%
% Environment slots are allocated in a similar way as in the WAM or
% in ECLiPSe I, i.e. ordered according to their last occurrence. This
% means that the environment can shrink during clause execution (whether
% physically by trimming, or virtually - for gc only - by size tracking).
%
% If we have variables local to branches, they can use the same slot as
% other local variables in parallel branches.
% But we do NOT reuse slots for consecutive lifetimes, e.g.
%	p :- p(X), q(X), r(Y), s(Y).
% This could only be done when either determinism information is
% available, or an extra trail check/trailing is accepted:  If there
% were a choicepoint inside p/1 or q/1, reusing X's slot would require
% conditional (value-)trailing of the old slot value before it is reused
% for Y.
%
% A problem is posed by variables whose lifetime starts before a disjunction
% and extends into one or more disjunctive branches (without surviving the
% disjunction): it may not be possible to compute an optimal slot with
% minimal lifetime, because the relative order of the ends of lifetimes
% with other variables may be different in different branches.  We currently
% treat such variables as always surviving until the end of the disjunction
% (this may prevent garbage collection, but is in effect the same as in
% ECLiPSe I).
% 
% Because of the way the garbage collector marks environments, we can
% only shrink the active environment, not selectively mark slots as
% no longer active.
%----------------------------------------------------------------------


:- export assign_env_slots/4.

assign_env_slots(Body, Map, EnvSize, Options) :-
	m_map:to_assoc_list(Map, MapList),
	strip_keys(MapList, Slots),
	flatten(Slots, FlatSlots),
	classify_voids_and_temps(FlatSlots, PermSlots, Options),
	% The sorting here is a bit subtle: we rely on the callpos
	% partial order being compatible with the total term order.
	sort(firstpos of slot, >=, PermSlots, SlotsIncStart),
	sort(lastpos of slot, >=, SlotsIncStart, SlotsInc),
	init_branch(Branch),
%	assign_perm_slots(SlotsInc, Branch, 0, EnvSize0),
	foreachcallposinbranch(Branch, SlotsInc, SlotsRest, 0, EnvSize0),
	verify SlotsRest==[],

	mark_env_activity(Body, EnvSize0),

	( EnvSize0 == 0, only_tail_calls(Body) ->
	    EnvSize = -1
	;
	    EnvSize = EnvSize0
	).


% Deal with the void and temporary variables, and filter them out
classify_voids_and_temps(AllSlots, PermSlots, Options) :-
	(
	    foreach(Slot,AllSlots),
	    fromto(PermSlots,PermSlots2,PermSlots1,[]),
	    param(Options)
	do
	    Slot = slot{firstpos:First,lastpos:Last,class:Loc,source_info:Source},
	    ( var(Loc) ->			% void
		Loc = void,
		singleton_warning(Source, Options),
		PermSlots2=PermSlots1
	    ;
		Loc = nonvoid(Where),	% needs assignment
		verify var(Where),
		( First == Last ->
		    Where = temp,
		    PermSlots2=PermSlots1
		;
		    PermSlots2=[Slot|PermSlots1]
		)
	    )
	).


%singleton_warning(+VarSourceInfo, +Options).
singleton_warning(annotated_term{type:var(Name),file:Path,line:Line}, options{warnings:on}) ?- !,
	( atom_string(Name, NameS), substring(NameS,"_",1) ->
	    true
	;
	    pathname(Path, _, File),
	    printf(warning_output, "File %w, line %d: Singleton variable %w%n", [File,Line,Name])
	).
singleton_warning(_, _).


foreachcallposinbranch(_Branch, [], [], Y, Y).
foreachcallposinbranch(Branch, [Slot|Slots], RestSlots, Y0, Y) :-
	% Branch is list of even length, e.g. [], [7,2]
	% SlotPos is list of odd length, e.g. [7], [7,2,7] but not [7,?,?]
	Slot = slot{lastpos:SlotPos,class:nonvoid(Loc)},
	( append(Branch, RelPos, SlotPos) ->
	    RelPos = [PosInBranch|SubBranch],
	    verify PosInBranch \== ?,
	    ( (SubBranch = [] ; SubBranch = [?,?]) ->
		Y1 is Y0+1, Loc = y(Y1),	% assign env slot
		Slots1 = Slots
	    ;
		% SlotPos is deeper down, RelPos=[7,2,7], [7,2,7,?,?] or longer
		% process branches at callpos [7]
		append(Branch, [PosInBranch], Pos),	% nested disjunction
		foreachbranchatcallpos(Pos, [Slot|Slots], Slots1, Y0, Y0, Y1)
	    ),
	    foreachcallposinbranch(Branch, Slots1, RestSlots, Y1, Y)
	;
	    % the first slot does not end in this branch, return
	    RestSlots = [Slot|Slots],
	    Y = Y0
	).

% process all slots that start with Pos
foreachbranchatcallpos(_Pos, [], [], _Y0, Y, Y).
foreachbranchatcallpos(Pos, [Slot|Slots], RestSlots, Y0, Ymax0, Ymax) :-
	% Pos is list of odd length, e.g. [7], [7,2,7], but not [7,?,?]
	% SlotPos is list of odd length, e.g. [7], [7,2,7] but not [7,?,?]
	Slot = slot{lastpos:SlotPos},
	% is Slot in a branch below this callpos? Always true for initial invocation
	( append(Pos, RelPos, SlotPos) ->
	    RelPos = [RelBranch|SubPos],
	    verify RelBranch \== ?,
	    % RelPos is [2,7] or [2,7,?,?] or [2,7,2,7] or longer
	    % which means we are going into branch 2 at Pos
	    ( (SubPos = [_] ; SubPos = [_,?,?]) ->
		append(Pos, [RelBranch], Branch),
		foreachcallposinbranch(Branch, [Slot|Slots], Slots1, Y0, Y1),
		Ymax1 is max(Ymax0,Y1),
		foreachbranchatcallpos(Pos, Slots1, RestSlots, Y0, Ymax1, Ymax)
	    ;
		append(Pos, [_,_], Pos1),	% branch deeper down
		append(Pos1, _, SlotPos),
		foreachbranchatcallpos(Pos1, [Slot|Slots], RestSlots, Y0, Ymax0, Ymax)
	    )
	;
	    % Slot not at this callpos, return
	    RestSlots = [Slot|Slots],
	    Ymax = Ymax0
	).


% succeed if the clause has no disjunctions
% and at most one tail recursive regular call

only_tail_calls([]).
only_tail_calls([goal{kind:Kind}|Goals]) :-
	( Kind = regular ->
	    Goals = []
	;
	    only_tail_calls(Goals)
	).


%----------------------------------------------------------------------
% Computing environment activity maps
%
% We assume that environment slots are already allocated to permanent
% variables. The job of this phase is to compute environment slot activity
% maps for various points in the code, in particular call positions
% and entry and exit points of disjunctive branches. These maps are
% simple bitmaps, with bit i-1 (i>0) indicating that Yi is active.
%
% We make a forward and a backward pass through the directed acyclic
% graph formed by the normalised clause. During the forward pass, we
% annotate every goal with two sets:
%	- seen_before (the slots that occurred before this goal)
%	- seen_here (the slots that occur in this goal)
%
% Then we make a backward pass to discover the last occurrences and
% compute the actual environment activity maps. With the current strategy
% of globalising all environment variables, a slot's activity ends at
% the call that has its last occurrence(s).
%----------------------------------------------------------------------

% Auxiliary structures built during forward pass, and traversed backward
:- local struct(rev_goal(	% wrapper for goal{}
    	goal,		% the goal{} all this belongs to
	seen_before,	% bitmap of variables seen before this goal
	seen_here)	% bitmap of variables occurring in this goal
    ).

:- local struct(rev_disj(	% wrapper for disjunction{}
	disjunction,	% the disjunction{} all this belongs to
    	rev_branches,	% list of reversed branches, for backward traversal
	seen_before,	% bitmap of variables seen before this branch
	seen_at_end,	% bitmap of variables seen at end of each branch
	seen_at_ends)	% list of bitmaps of variables seen at end of each branch
    ).


mark_env_activity(Clause, FullEnvSize) :-
	mark_env_activity_fwd(Clause, 0, _Before, [], Reverse),
	mark_env_activity_bwd(Reverse, 0, _After, FullEnvSize).


mark_env_activity_fwd([], Seen, Seen, Reverse, Reverse).
mark_env_activity_fwd([Goal|Goals], Seen0, Seen, Reverse0, Reverse) :-
	mark_env_activity_fwd(Goal, Seen0, Seen1, Reverse0, Reverse1),
	mark_env_activity_fwd(Goals, Seen1, Seen, Reverse1, Reverse).
mark_env_activity_fwd(Disjunction, Seen0, Seen, Reverse, [RevDisj|Reverse]) :-
	Disjunction = disjunction{branches:Branches},
	RevDisj = rev_disj{rev_branches:RevBranches,disjunction:Disjunction,
		seen_before:Seen0, seen_at_end:Seen, seen_at_ends:SeenEnds},
	(
	    foreach(Branch,Branches),
	    foreach(RevBranch,RevBranches),
	    foreach(SeenEndBranch,SeenEnds),
	    fromto(Seen0,Seen1,Seen2,Seen),
	    param(Seen0)
	do
	    mark_env_activity_fwd(Branch, Seen0, SeenEndBranch, [], RevBranch),
	    Seen2 is Seen1 \/ SeenEndBranch
	).
mark_env_activity_fwd(Goal, Seen0, Seen, Reverse, [RevGoal|Reverse]) :-
	Goal = goal{args:Args},
	RevGoal = rev_goal{seen_here:UsedHere,seen_before:Seen0,goal:Goal},
	mark_env_activity_args(Args, 0, UsedHere),
	Seen is Seen0 \/ UsedHere.
mark_env_activity_fwd(Indexpoint, Seen0, Seen, Reverse, Reverse) :-
	Indexpoint = indexpoint{args:Args},
	mark_env_activity_args(Args, 0, UsedHere),
	Seen is Seen0 \/ UsedHere.


    :- mode mark_env_activity_args(+,+,-).
    mark_env_activity_args([], EAM, EAM).
    mark_env_activity_args([X|Xs], EAM0, EAM) :-
	mark_env_activity_term(X, EAM0, EAM1),
	mark_env_activity_args(Xs, EAM1, EAM).

    :- mode mark_env_activity_term(+,+,-).
    mark_env_activity_term(Var, EAM0, EAM) :-
	Var = variable{class:Loc},
	( Loc = nonvoid(y(Y)) ->
	    EAM is setbit(EAM0, Y-1)		% set the seen-flag
	;
	    EAM0=EAM
	).
    mark_env_activity_term(attrvar{variable:Avar,meta:Meta}, EAM0, EAM) :-
	mark_env_activity_term(Avar, EAM0, EAM1),
	mark_env_activity_term(Meta, EAM1, EAM).
    mark_env_activity_term([X|Xs], EAM0, EAM) :-
	mark_env_activity_term(X, EAM0, EAM1),
	mark_env_activity_term(Xs, EAM1, EAM).
    mark_env_activity_term(structure{args:Args}, EAM0, EAM) :-
	mark_env_activity_term(Args, EAM0, EAM).
    mark_env_activity_term(Term, EAM, EAM) :- atomic(Term).



% Backwards traversal of the clause DAG to discover last occurrences.
% Using the auxiliary data structure created during the forward pass,
% and the seen_before/seen_here-fields filled in during the forward pass.

mark_env_activity_bwd([], After, After, _ESize).
mark_env_activity_bwd([Goal|Goals], After0, After, ESize) :-
	mark_env_activity_bwd(Goal, After0, After1, ESize),
	mark_env_activity_bwd(Goals, After1, After, ESize).
mark_env_activity_bwd(rev_disj{rev_branches:Branches,
			seen_before:SeenBeforeDisj,
			seen_at_end:SeenEndDisj,
			seen_at_ends:SeenEnds,
			disjunction:disjunction{
			    entrymap:DisjEntryEAM,
			    exitmap:DisjExitEAM,
			    branchentrymaps:BranchEntryEamArray,
			    branchinitmaps:BranchExitInits,
			    index:indexpoint{envmap:DisjEntryEAM}}},
		After0, After, ESize) :-
	% EAM after exiting the disjunction
	DisjExitEAM is SeenEndDisj /\ After0,
	(
	    foreach(Branch,Branches),
	    foreach(SeenEnd,SeenEnds),
	    foreach(BranchEntryEAM,BranchEntryEAMs),
	    foreach(BranchExitInit,BranchExitInits),
	    fromto(After0,After1,After2,After),
	    param(SeenBeforeDisj,After0,ESize,DisjExitEAM)
	do
	    % slots that are active after the disjunction, but not
	    % at the end of the branch, must be initialised
	    % on branch exit!
	    BranchExitEAM is SeenEnd /\ After0,
	    BranchExitInit is DisjExitEAM /\ \BranchExitEAM,
	    mark_env_activity_bwd(Branch, After0, BranchAndAfter, ESize),
	    BranchEntryEAM is SeenBeforeDisj /\ BranchAndAfter,
	    After2 is After1 \/ BranchAndAfter
	),
	% EAM before entering the disjunction
	DisjEntryEAM is SeenBeforeDisj /\ After,
	BranchEntryEamArray =.. [[]|BranchEntryEAMs].
mark_env_activity_bwd(rev_goal{seen_before:Before,seen_here:UsedHere,goal:Goal}, After0, After, ESize) :-
	Goal = goal{envmap:EAM,envsize:ESize},
	% if variables were not globalised, slots would remain active during call:
%	EAM is UsedHere \/ (Before /\ After0),
	% when unsafe variables are globalised, slots are released on call:
	EAM is (UsedHere \/ Before) /\ After0,
	After is After0 \/ UsedHere.

