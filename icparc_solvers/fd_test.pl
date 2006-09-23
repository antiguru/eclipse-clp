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
% Copyright (C) 2000 - 2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): Joachim Schimpf, IC-Parc
% 
% END LICENSE BLOCK
% ----------------------------------------------------------------------
% System:	ECLiPSe Constraint Logic Programming System
% Author:	Joachim Schimpf, IC-Parc, Imperial College, London
% Version:	$Id: fd_test.pl,v 1.1 2006/09/23 01:53:31 snovello Exp $
%
% IDENTIFICATION:	fd_test.pl
%
%	Support for testing global constraints with random patterns
%
% ----------------------------------------------------------------------

%----------------------------------------------------------------------
% Parameters:
%	variable range
%	number of variables
%	reductions: bounds, holes
%	max number of vars reduced in each step
%----------------------------------------------------------------------

:- module(fd_test).

:- export
	% making a random number of variables with random domains
	make_random_variables/1,
	make_n_random_variables/4,

	% applying random bound reductions to variables in a list
	random_reduce_until_ground/3.

:- lib(fd).

%----------------------------------------------------------------------

rtest :-
	rtest_stream(output).

rtest(N, File) :-
	seed(12345),
	open(File, write, Out),
	( count(I,1,N), param(Out) do
	    printf(Out, "----- %d -----%n", [I]),
	    rtest_stream(Out)
	),
	close(Out).

rtest(N) :-
	seed(12345),
	( count(I,1,N) do
	    rtest_stream(null)
	).

rtest_stream(Out) :-
	make_random_variables(Vars),
	printf(Out, "%mw%n", [Vars]),
	(
	    sum_ge_zero(Vars),
	    random_reduce_until_ground(Out, Vars, Vars)
	->
	    printf(Out, "%mw%nyes.%n", [Vars])
	;
	    printf(Out, "no.%n", [])
	),
	fail.		% fail to avoid garbage collection
rtest_stream(_).


benchmark(N) :-
	seed(12345),
	( count(I,1,N) do
	    benchmark_one
	).

benchmark_one(Seed) :-
	seed(Seed),
	make_n_random_variables(100, -10, 10, Vars),
	sum_ge_zero(Vars),
	labeling(Vars),
	!,
	fail.
benchmark_one(_).

benchmark_one :-
	make_n_random_variables(1000, -10, 10, Vars),
	sum_ge_zero(Vars),
%	sum(Vars) #>= 0,
	once labeling(Vars),
	fail.
benchmark_one.

%----------------------------------------------------------------------

random_reduce_until_ground(Out, OriginalVars, Remaining) :-
	term_variables(Remaining, Vars),
	( Vars = [] ->
	    true
	;
	    N is 1 + random mod 3,
	    pick_randomly(N, Vars, PickedVars),
	    call_priority((
		    random_reduce(PickedVars),
		    printf(Out, "L %mw%n", [OriginalVars])
	    ), 2),
	    % propagation happens here!
	    printf(Out, "P %mw%n", [OriginalVars]),
	    random_reduce_until_ground(Out, OriginalVars, Vars)
	).


make_random_variables(Vars) :-
	N is 1 + random mod 10,
	make_n_random_variables(N, -10, 10, Vars).

make_n_random_variables(0, _, _, []) :- !.
make_n_random_variables(N, Min, Max, [X|Xs]) :-
	make_random_variable(Min, Max, X),
	N1 is N-1,
	make_n_random_variables(N1, Min, Max, Xs).

make_random_variable(Min, Max, X) :-
	Min =< Max,
	DomSizeMax is Max - Min + 1,		% 1 ..
	DomSizeMinus1 is random mod DomSizeMax,	% 0 .. < DomSizeMax
	LeftOver is DomSizeMax-DomSizeMinus1,	% 1 .. =< DomSizeMax
	L is Min + (random mod LeftOver),	% Min .. Max
	H is L + DomSizeMinus1,
	X::L..H.

random_reduce_one(VarArr) :-
	functor(VarArr,_,N),
	I is (random mod N) + 1,
	arg(I, VarArr, X),
	( var(X) ->
	    random_reduce(X)
	; nonground(VarArr) ->
	    random_reduce_one(VarArr)
	).

random_reduce(X) :-		% random bound reduction on X
	var(X),
	dvar_range(X, Min, Max),
	DomSizeMinus1 is Max - Min,
	NewDomSizeMinus1 is random mod DomSizeMinus1,
	LeftOver is DomSizeMinus1+1-NewDomSizeMinus1,
	NewL is Min + (random mod LeftOver),
	NewH is NewL + NewDomSizeMinus1,
	X::NewL..NewH.
random_reduce([]).
random_reduce([X|Xs]) :-
	random_reduce(X),
	random_reduce(Xs).

random_sequence(X) :-
	var(X),
	random_reduce(X),
	printf("%mw%n",[X]),
	random_sequence(X).
random_sequence(X) :-
	nonvar(X).


shuffle(L, R) :-
        add_random_keys(L, KL),
        keysort(KL, KR),
        rm_keys(KR, R).

        % add random key to each list element
add_random_keys([], []).
add_random_keys([A|L], [K-A|KL]) :-
        random(K),
        add_random_keys(L, KL).

        % remove keys from association list
rm_keys([], []).
rm_keys([_K-A|KL], [A|L]) :-
        rm_keys(KL, L).


pick_randomly(N, List, PickedElems) :-
	shuffle(List, ShuffledList),
	length(PickedElems, N),
	once append(PickedElems, _, ShuffledList).

%----------------------------------------------------------------------

