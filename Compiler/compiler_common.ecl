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
% Version:	$Id: compiler_common.ecl,v 1.2 2007/02/09 02:54:48 kish_shen Exp $
% ----------------------------------------------------------------------

:- module(compiler_common).

:- comment(summary, "ECLiPSe III compiler - common data structures and auxiliaries").
:- comment(copyright, "Cisco Technology Inc").
:- comment(author, "Joachim Schimpf").
:- comment(date, "$Date: 2007/02/09 02:54:48 $").


%----------------------------------------------------------------------
% Common options-structure
%----------------------------------------------------------------------

:- comment(struct(options), [
    summary:"Compiler Options",
    fields:[
	"output":"Either 'print' (print resulting WAM code to the output stream),
	    'print(Stream)' (print WAM code to Stream), 'listing(File)' (print WAM
	    code to File, 'listing' (print WAM code to input file with .lst suffix),
	    or 'store' in which case the WAM code is assembled and stored in memory
	    as the code for the predicate.  (default: store)",
	"print_normalised":"print result of the normalisation pass.",
	"print_lifetimes":"print result of the variable lifetime analysis.",
	"print_raw_code":" print annotated WAM code before register allocation.",
	"print_final_code":" print annotated WAM code after register allocation."
    ]
]).

:- export struct(options(
	output,
	print_normalised,
	print_lifetimes,
	print_raw_code,
	print_final_code
    )).


valid_option_field(print_normalised, print_normalised of options).
valid_option_field(print_lifetimes, print_lifetimes of options).
valid_option_field(print_raw_code, print_raw_code of options).
valid_option_field(print_final_code, print_final_code of options).
valid_option_field(output, output of options).

valid_option_value(print_normalised, Value) :- onoff(Value).
valid_option_value(print_lifetimes, Value) :- onoff(Value).
valid_option_value(print_raw_code, Value) :- onoff(Value).
valid_option_value(print_final_code, Value) :- onoff(Value).
valid_option_value(output, listing(_File)).
valid_option_value(output, listing).
valid_option_value(output, print(_Stream)).
valid_option_value(output, print).
valid_option_value(output, store).

onoff(off).
onoff(on).

default_options(options{
	print_normalised:off,
	print_lifetimes:off,
	print_raw_code:off,
	print_final_code:off,
	output:store
}).


%----------------------------------------------------------------------
% Normalised source data structures
%----------------------------------------------------------------------

% Descriptor for a subgoal other than a conjunction or disjunction
% Also used for the clause head

:- comment(struct(goal), [
    summary:"Descriptor for a subgoal other than a conjunction or disjunction",
    desc:ascii("
	Descriptor for a subgoal other than a conjunction or disjunction.
	These goals come in three flavours:

	    - simple (inline)
	    - regular
	    - head (treated as pseudo-goal)
    "),
    fields:[
	kind:		"Kind of goal (simple|regular|head)",
	callpos:	"identifier for the chunk it occurs in",
	functor:	"the goal's functor (Name/Arity)",
	args:		"list of normalised terms",
	envmap:		"environment activity bitmap (at call time)",
	envsize:	"environment size (at call time)",
	state:		"execution state at call time (struct(state)),"
			" the result of the analysis phase",
        pos:            "start position (from start of file) of goal",
        path:           "full file path to file in which goal occurs",
        lookup_module:	"module where to look up the predicate definition ([] if context module)",
	definition_module: "module where the predicate is defined ([] if unknown)"
    ]
]).

:- export struct(goal(
    	kind,
	callpos,
	functor,
	args,
	envmap,
	envsize,
	state,
        pos,
        path,
        lookup_module,
    	definition_module	% cached
    )).


:- comment(struct(disjunction), [
    summary:"Descriptor for a disjunction",
    desc:ascii("
	Descriptor for a disjunction, resulting either from a disjunction
	in the source (;/2) or from multiple clauses.
    "),
    fields:[
	callpos:	"identifier for the chunk it occurs in",
	entrymap:	"environment activity bitmap (at retry time)",
	entrysize:	"environment size (at retry time)",
	exitmap:	"environment activity bitmap (at branch end)",
	exitsize:	"environment size (at branch end)",
	arity:		"pseudo-arity (valid arguments at retry time)",
    	branches:	"list of normalised goals (at least 2 elements)",
	branchlabels:	"array[NBranches] of Labels (shared with corresponding indexpoint)",
	index:		"struct indexpoint",
	state:		"execution state on entry (struct(state)),"
			" the result of the analysis phase"
    ]
]).

:- export struct(disjunction(
	callpos,
	entrymap,
	entrysize,
	exitmap,
	exitsize,
	arity,		% arity for try instructions
    	branches,	% list of list of goals
	branchlabels,	% array[NBranches] of Labels, shared with indexpoint
	index,
	state
    )).


:- comment(struct(indexpoint), [
    summary:"Descriptor for a disjunction's indexes",
    desc:ascii("
	Descriptor for indexing. This is a pseudo goal that occurs
	at the beginning of the first alternative of a disjunction.
	It is treated similar to a simple goal.
    "),
    fields:[
	callpos:	"identifier for the chunk it occurs in",
	envmap:		"environment activity bitmap",
	args:		"list of variable{} which are being indexed upon",
	nextaltlabel:	"label of second alternative's retry/trust",
	indexes:	"list of index{} in order of decreasing quality",
	disjunction:	"disjunction{} that this index is connected to"
    ]
]).

:- export struct(indexpoint(
	callpos,
	envmap,		% environment activity bitmap
	args,		% list of variable{} that are switched on
			% (a cache for the ones in indexes below)
	nextaltlabel,	% label of first retry_me_else
	indexes,	% list of index{}
	disjunction	% disjunction{} that this index is connected to
    )).


:- comment(struct(index), [
    summary:"Descriptor for one index",
    desc:ascii("
	Descriptor for one index, i.e. a branching decision
	on a single variable.
    "),
    fields:[
	quality:	"positive float: index quality, the smaller the better",
	variable:	"variable{} that this index works on",
	partition:	"list of Class-Branches pairs, e.g. [var-[1,2], value(1,integer)-[1]]",
	other:		"list of default branches, when value doesn't fall in any class"
    ]
]).

:- export struct(index(
	quality,	% positive float, 0.0=best
	variable,	% variable{}
	partition,	% list of Class-Branches pairs
	other		% list of Branches
    )).



:- comment(struct(structure), [
    summary:"Descriptor for a compound term",
    desc:ascii("
	Descriptor for a compound term (other than a list) that occurred in
	the source code. Lists in the source code are simply represented as
	lists in the normalised code. Atomic constants in the source code
	are represented as themselves in the normalised form.
    "),
    fields:[
	name:		"functor name (atom)",
	arity:		"functor arity (integer)",
	args:		"list of arguments (normalised terms)"
    ]
]).

:- export struct(structure(	% maybe this should be simpler
	name,
	arity,
	args
    )).


% We have one struct(variable) for every variable occurrence.
%
% isfirst/islast are set for _all_ occurrences in the chunk where the
% variable occurs first/last in order to still allow for reordering later.
%
% The class field is shared between all occurrences which belong to the
% same instance of the variable. For temporary variables it is nonvoid(t(T)).
% For permanent variables it is nonvoid(y(I)) where I is filled in once known.
%
% All information apart from varid could in principle be held in separate
% tables, indexed on varid.

:- comment(struct(variable), [
    summary:"Descriptor for a variable occurrence in the normalised source",
    desc:ascii("
	We have one struct(variable) for every variable occurrence.
	isfirst/islast are set for _all_ occurrences in the chunk where
	the variable occurs first/last in order to still allow for later
	reordering.

	The class field is shared between all occurrences which belong to
	the same instance of the variable.  Its possible values are:
	
	- void           for void variables (only one occurrence)
	- nonvoid(temp)  for temporary variables (occurs only in one chunk)
	- nonvoid(y(Y))  for permanent variables (Y filled in once known)

	Possible alternative implementation: All information apart from varid
	could in principle be held in separate tables, indexed on varid.
    "),
    fields:[
	varid:		"unique source variable id (integer), created by normalize_clauses",
	source_var:	"source variable",
	source_name:	"variable name (atom) if any, else the source"
			" variable itself (for error messages)",
	isafirst:	"'first' if first occurrence, else uninst (filled in by compute_lifetimes)",
	isalast:	"'last' if last occurrence, else uninst (filled in by compute_lifetimes)",
	class:		"variable class and permanent location (filled in by assign_env_slots)"
    ]
]).

:- export struct(variable(
	varid,			% unique source variable id (integer)
	source_var,		% source variable (possibly a copy!)
	source_name,		% variable name (atom) if any,
				% else uninitialised (for error messages)
	isafirst,		% 'first' if first occurrence, else uninst
	isalast,		% 'last' if last occurrence, else uninst

	class			% variable class and preliminary location:
				%	void
				%	nonvoid(temp)
				%	nonvoid(y(I))
    )).


% Get a descriptor for an additional occurrence of existing variable
% This can only be used _before_ compute_lifetimes!
:- export new_vardesc/2.
new_vardesc(VarId, variable{varid:VarId}).

/*
% Introduce a new, auxiliary source variable into normalised code
:- export new_aux_variable_norm/3.
new_aux_variable_norm(VarDesc, VId0, VId) :-
	VarDesc = variable{varid:VId,source_name:''},
	VId is VId0+1.
*/

%----------------------------------------------------------------------
% Annotated WAM code
% This is generated by the code generation phase and understood
% by register allocation.
%----------------------------------------------------------------------

:- comment(struct(code), [
    summary:"Annotated abstract machine instruction",
    desc:ascii("
	This is the format that the code generator produces (list of
	struct(code)), and which is fed into the register allocator and
	the peephole optimizer.  Finally, the annotations are stripped
	and the instr-fields alone are use as input to the assembler.

	The comment field can be used for debugging, e.g. it can contain
	information about the purpose of an instruction, or about the
	source construct it relates to.
    "),
    see_also:[generate_code/5,print_annotated_code/1],
    fields:[
	instr:	"WAM instruction (assembler input format)",
	regs:	"Register usage descriptor",
	comment:"Arbitrary ground term"
    ]
]).

:- export struct(code(
	instr,			% AM instruction (assembler input format)
	regs,			% list of register descriptors
	comment			% string
    )).


%----------------------------------------------------------------------
% Analysis phase data structures
%----------------------------------------------------------------------

:- comment(struct(state), [
    summary:"Descriptor for dataflow analysis results",
    desc:ascii("
	This describes what is known about the state of computation at the
	end of a subgoal (or at predicate entry, for heads).
	This is where the results of the flow and binding analysis get
	collected. It contains determinism and binding information.
	The information is stored in struct(goal)

	Possible determinism values:

	    det
	    failure

	Possible binding values:

	    alias(VarId)
	    type(Type)
	    value(Constant)		if simple type (ground?)

	Possible types:

	    The ones returned by type_of/2 (except 'var')
	    F/Ar	for a particular functor
	    []		the empty type (no value possible)
	    'univ'	anything (the default)

    "),
    see_also:[binding_analysis/1,struct(goal)],
    fields:[
	determinism:	"an atom (det|failure)",
	bindings:	"a map varid->binding"
    ]
]).

:- export struct(state(
    	determinism,	% an atom (det|failure|...)
	bindings	% a map varid->binding
    )).



%----------------------------------------------------------------------
% Debugging
%----------------------------------------------------------------------

:- comment((verify)/1, [
    summary:"Debugging aid: verify a condition",
    args:["Goal":"A condition to check"],
    see_also:[(certainly_once)/1],
    desc:ascii("
	Checks at runtime whether a given condition is true, and prints
    	an error and aborts if false.
	Goal is not supposed to contribute to the semantics of the program,
	i.e. it should not bind anything or cause side effects.
	When checks are disabled, verify(Goal) is replaced by true.
    ")
]).

:- comment((certainly_once)/1, [
    summary:"Debugging aid: make sure a deterministic goal does not fail",
    args:["Goal":"A goal to execute"],
    see_also:[(verify)/1],
    desc:ascii("
	Executes once(Goal) and checks that it doesn't fail. If it fails,
	prints an error and aborts.
	Unlike verify/1, the goal Goal is part of the program semantics.
	When checks are disabled, certainly_once(Goal) is replaced by once(Goal).
    ")
]).

:- export (verify)/1.
:- export (certainly_once)/1.
:- export op(900, fy, verify).
:- export op(900, fy, certainly_once).
expand_check((verify Goal), (Goal->true;printf(error, "Check failed: %w%n", [Goal]),abort) ).
expand_check((certainly_once Goal), (Goal->true;printf(error, "Goal failed unexpectedly: %w%n", [Goal]),abort) ).
expand_nocheck(verify _, true).
expand_nocheck(certainly_once Goal, once Goal).

% Uncomment alternative lines to enable/disable checks
:- inline((verify)/1, expand_check/2).
%:- inline((verify)/1, expand_nocheck/2).
:- inline((certainly_once)/1, expand_check/2).
%:- inline((certainly_once)/1, expand_nocheck/2).

verify _Goal :-
	verify _Goal.

certainly_once _Goal :-
	certainly_once _Goal.

:- export indent/2.
indent(Stream, Indent) :-
	I is 4*Indent,
	printf(Stream, "%*c", [I,0' ]).


%----------------------------------------------------------------------
% Parameters
%----------------------------------------------------------------------

:- comment(wam_registers/1, [
    summary:"The number of argument registers of the abstract machine",
    amode:wam_registers(-),
    args:["N":"An integer"]
]).

:- export wam_registers/1.
wam_registers(255).


%----------------------------------------------------------------------
% General auxiliaries
%----------------------------------------------------------------------

:- export
	strip_keys/2,
	merge_sorted_lists/3,
	group_same_key_values/2,
	group_same_key_values/3,
	merge_same_key_values/2,
	concat_same_key_values_unstable/2,
	concat_same_key_values_stable/2.
	

% merge_sorted_lists(+Lists, ?ListsTail, -MergedList)
merge_sorted_lists([], [], []).
merge_sorted_lists([L], [], L) :- !.
merge_sorted_lists([L1,L2|Ls], [L12|Tail], L) :-
    	merge(0, =<, L1, L2, L12),
	merge_sorted_lists(Ls, Tail, L).


group_same_key_values([], []).
group_same_key_values([K-V|List], [K-[V|KVs]|GroupedList]) :-
        group_same_key_values1(List, K, KVs, GroupedList).

    group_same_key_values1([], _, [], []).
    group_same_key_values1([K-V|List], K, [V|KVs], GroupedList) :- !,
        group_same_key_values1(List, K, KVs, GroupedList).
    group_same_key_values1([K-V|List], _K, [], [K-[V|KVs]|GroupedList]) :-
        group_same_key_values1(List, K, KVs, GroupedList).


group_same_key_values(_Pos, [], []).
group_same_key_values(Pos, [KV|List], [[KV|KVs]|GroupedList]) :-
	arg(Pos, KV, K),
        group_same_key_values2(Pos, List, K, KVs, GroupedList).

    group_same_key_values2(_Pos, [], _, [], []).
    group_same_key_values2(Pos, [KV|List], K, [KV|KVs], GroupedList) :-
	arg(Pos, KV, K1), K == K1, !,
        group_same_key_values2(Pos, List, K, KVs, GroupedList).
    group_same_key_values2(Pos, [KV|List], _K, [], [[KV|KVs]|GroupedList]) :-
	arg(Pos, KV, K),
        group_same_key_values2(Pos, List, K, KVs, GroupedList).


% the values are assumed to be lists.
% unstable version: concats the lists in reverse order,
% but makes less copies (no copy for groups of size one)
concat_same_key_values_unstable([], []).
concat_same_key_values_unstable([K-V|List], [K-Vs|GroupedList]) :-
        concat_same_key_values_unstable(List, V, K, Vs, GroupedList).

    concat_same_key_values_unstable([], Vs, _K, Vs, []).
    concat_same_key_values_unstable([K-V|List], Vs, K, KVs, GroupedList) :- !,
	append(V, Vs, VVs),
        concat_same_key_values_unstable(List, VVs, K, KVs, GroupedList).
    concat_same_key_values_unstable([K-V|List], Vs0, _K, Vs0, [K-Vs|GroupedList]) :-
        concat_same_key_values_unstable(List, V, K, Vs, GroupedList).


% the values are assumed to be lists.
% stable version: concats the lists in their original order
concat_same_key_values_stable([], []).
concat_same_key_values_stable([K-V|List], [K-VKVs|GroupedList]) :-
	append(V, KVs, VKVs),
        concat_same_key_values_stable(List, K, KVs, GroupedList).

    concat_same_key_values_stable([], _, [], []).
    concat_same_key_values_stable([K-V|List], K, VKVs, GroupedList) :- !,
	append(V, KVs, VKVs),
        concat_same_key_values_stable(List, K, KVs, GroupedList).
    concat_same_key_values_stable([K-V|List], _K, [], [K-VKVs|GroupedList]) :-
	append(V, KVs, VKVs),
        concat_same_key_values_stable(List, K, KVs, GroupedList).


% the values are assumed to be sorted lists.
merge_same_key_values(MultiKeyValues, KeyMergedValues) :-
    	group_same_key_values(MultiKeyValues, KeyMultiValues),
	( foreach(K-MultiValues,KeyMultiValues),
	  foreach(K-MergedValues,KeyMergedValues)
	do
	    merge_sorted_lists(MultiValues, [], MergedValues)
	).


:- export strip_keys/2.
strip_keys([], []).
strip_keys([_-X|KXs], [X|Xs]) :-
	strip_keys(KXs, Xs).


%----------------------------------------------------------------------
% top_sort(+AdjArray, -Ordered, -UpEdges)
% AdjArray is array of lists of adjacent nodes.
% This returns a topological order in Ordered.
% If the graph was cyclic, the order is computed under the assumption
% that the nodes in UpEdges have been removed from the graph.
% Note that for unordered items, the algorithm will reverse the
% previously existing order!
%----------------------------------------------------------------------

:- export top_sort/4.
top_sort(Adj, PreOrdered, Ordered, UpEdges) :-
	functor(Adj, F, MaxNode),
	functor(Seen, F, MaxNode),
	(
%	    for(StartNode, 1, MaxNode),
	    foreach(StartNode, PreOrdered),
	    fromto([], Ordered0, Ordered1, Ordered),
	    fromto([], UpEdges0, UpEdges1, UpEdges),
	    param(Adj,Seen)
	do
	    arg(StartNode, Seen, NodeSeen),
	    ( var(NodeSeen) ->
		topsort_visit(_, Adj, Seen, [StartNode], [],
				Ordered0, Ordered1, UpEdges0, UpEdges1)
	    ;
	    	Ordered0 = Ordered1,
	    	UpEdges0 = UpEdges1
	    )
	).

    topsort_visit(_From, _Adj, _Seen, [], [], Ordered0, Ordered, UpEdges0, UpEdges) :- !,
    	Ordered = Ordered0,
    	UpEdges = UpEdges0.
    topsort_visit(From, Adj, Seen, [], [[Node|Edges]|Stack], Ordered0, Ordered, UpEdges0, UpEdges) :- !,
	arg(Node, Seen, seen(done)),	% mark done
	topsort_visit(From, Adj, Seen, Edges, Stack, [Node|Ordered0], Ordered, UpEdges0, UpEdges).
    topsort_visit(From, Adj, Seen, EdgeEdges, Stack, Ordered0, Ordered, UpEdges0, UpEdges) :-
	EdgeEdges = [Node|Edges],
	arg(Node, Seen, NodeSeen),
	( var(NodeSeen) ->
	    NodeSeen = seen(_),		% mark visited
	    arg(Node, Adj, Successors),
	    topsort_visit(Node, Adj, Seen, Successors, [EdgeEdges|Stack], Ordered0, Ordered, UpEdges0, UpEdges)
	;
	    NodeSeen = seen(Done),
	    ( nonvar(Done) ->
		topsort_visit(From, Adj, Seen, Edges, Stack, Ordered0, Ordered, UpEdges0, UpEdges)
	    ;
		% we have an upward edge, i.e. a cycle: record and ignore it
		topsort_visit(From, Adj, Seen, Edges, Stack, Ordered0, Ordered, [From->Node|UpEdges0], UpEdges)
	    )
	).


%----------------------------------------------------------------------
% Chunks and Call Positions
% A chunk is a sequences of simple goals that ends with a regular goal
% or the end of the clause. All goals in a chunk have the same call
% position.  A call position is a list of positive integers (odd length):
%	[Pos]
%	[Pos,Branch1,Pos1,...,BranchN,PosN]
% In a flat clause, the call positions are [1] .. [N].
% If there are disjunctions, the parallel branches are distinguished by
% a branch number appended to the disjunction's call position. The chunks
% inside the branch again have a chunk number appended to the branch id.
% So [5,2,3] is the 3rd chunk in the second alternative of the disjunction
% in toplevel chunk 5.
%----------------------------------------------------------------------

:- export
	compare_pos/3,
	common_pos/3,
	init_branch/1,
	parallel_branch/2,
	same_call_pos/4,
	new_call_pos/4,
	new_branch/4,
	subsumes_pos/2,
	pos_branch/2,
	prev_call_pos/2,
	print_call_pos/2.

init_branch([]).

new_branch(CallPos, BranchNr0, BranchNr, Branch) :-
	BranchNr is BranchNr0+1,
	append(CallPos, [BranchNr0], Branch).


new_call_pos(Branch, CallNr0, CallNr, CallPos) :-
	CallNr is CallNr0+1,
	append(Branch, [CallNr0], CallPos).


same_call_pos(Branch, CallNr, CallNr, CallPos) :-
	append(Branch, [CallNr], CallPos).

prev_call_pos(CallPos, PrevCallPos) :-
	once append(Branch, [Pos], CallPos),
	Pos1 is Pos-1,
	append(Branch, [Pos1], PrevCallPos).

compare_pos([L|Ls], [R|Rs], Result) :-	% fails if not ordered
	( L < R ->
	    Result = (<)
	; L > R ->
	    Result = (>)
	;
	    compare_branches(Ls, Rs, Result)
	).

    compare_branches([], [], =).
    compare_branches([Branch|Ls], [Branch|Rs], Res) :-
    	compare_pos(Ls, Rs, Res).


% The positions are definitely in parallel branches
% Fails if comparable (<,>,=) or insufficient information
parallel_branch([P,B1|Ls], [P,B2|Rs]) :-
	( B1 == B2 ->
	    parallel_branch(Ls, Rs)
	;
	    true
	).


common_pos([L|Ls], [R|Rs], CCs) :-	% error if comparable
	( L == R ->
	    CCs = [L|Cs],
	    common_branch(Ls, Rs, Cs)
	;
	    writeln(error,"Error: comparable positions in common_pos/3"),
	    abort
	).

    common_branch([], _, CCs) :- !, CCs=[].
    common_branch(_, [], CCs) :- !, CCs=[].
    common_branch([LBranch|Ls], [RBranch|Rs], CCs) :-
    	( LBranch == RBranch ->
	    CCs = [LBranch|Cs],
	    common_pos(Ls, Rs, Cs)
	;
	    CCs = []
	).


subsumes_pos(L, R) :-
	append(L, _, R).


pos_branch(P, B) :-
	append(B, [_], P), !.


print_call_pos(_Stream, []).
print_call_pos(Stream, [Pos|Branch]) :-
	write(Pos),
	( Branch = [B|More] ->
	    write([B]),
	    print_call_pos(Stream, More)
	;
	    true
	).


%----------------------------------------------------------------------
% Environment activity maps 
% These are bitmaps indicating which environment slots are uninitialised
% If slot y(Y) is uninitialised, bit 2^Y is set.
% Bit 0 is unused (will be used to indicate a pointer to a large bitmap).
%----------------------------------------------------------------------

:- export decode_activity_map/2.
decode_activity_map(Map, _Unknown) :-
	var(Map), !.
decode_activity_map(Map, List) :-
	(
	    fromto(Map, Map1, Map2, 0),
	    fromto(List, List1, List2, []),
	    count(I,1,_)
	do
	    Map2 is Map1 >> 1,
	    ( Map2 /\ 1 =:= 0 -> List1=List2 ; List1=[I|List2] )
	).

