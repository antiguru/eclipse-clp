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
% Contributor(s): Joachim Schimpf, Coninfer Ltd
% 
% END LICENSE BLOCK
% ----------------------------------------------------------------------
% System:	ECLiPSe Constraint Logic Programming System
% Author:	Joachim Schimpf, Coninfer Ltd
% Version:	$Id: generic_prop_test_util.ecl,v 1.1 2013/01/27 01:37:35 jschimpf Exp $
%
% IDENTIFICATION:	generic_prop_test_util.pl
%
%	Support for testing global constraints with random patterns
%	Based on older module fd_test.
%
% ----------------------------------------------------------------------

%----------------------------------------------------------------------
% TODO Parameters:
%	variable range
%	number of variables
%	reductions: bounds, holes
%	max number of vars reduced in each step
%----------------------------------------------------------------------

:- comment(categories, ["Development Tools"]).
:- comment(summary, "Tools for testing global constraints").
:- comment(author, "Joachim Schimpf, Coninfer Ltd").
:- comment(copyright, "Joachim Schimpf, Coninfer Ltd").
:- comment(date, "$Date: 2013/01/27 01:37:35 $").


%----------------------------------------------------------------------

:- export random_reduction_test/2.
:- tool(random_reduction_test/2,random_reduction_test_/3).

random_reduction_test_(Out, Goal, Module) :-
	term_variables(Goal, Vars),
	random_reduction_test_(Out, Goal, Vars, Module).


:- export random_reduction_test/3.
:- tool(random_reduction_test/3,random_reduction_test_/4).

random_reduction_test_(Out, Goal, Vars, Module) :-
	set_stream_property(Out, output_options, [variables(anonymous)]),
	printf(Out, "C %mW%n", [Goal]),
	(
	    call(Goal)@Module,
	    printf(Out, "P %mW%n", [Goal]),
	    random_reduce_until_ground(Out, Goal, Vars)
	->
	    printf(Out, "yes.%n", [])
	;
	    printf(Out, "no.%n", [])
	),
	fail.		% fail, to avoid garbage collection
random_reduction_test_(_, _, _, _).


%----------------------------------------------------------------------
% apply random bound reductions to variables in a list
%----------------------------------------------------------------------

:- export random_reduce_until_ground/3.

random_reduce_until_ground(Out, OriginalVars, Remaining) :-
	term_variables(Remaining, Vars),
	( Vars = [] ->
	    true
	;
	    N is 1 + random_mod(3),
	    pick_randomly(N, Vars, PickedVars),
	    call_priority((
		    random_reduce(PickedVars),
		    copy_term(OriginalVars, Before),
		    printf(Out, "L %mW%n", [OriginalVars])
	    ), 2),
	    % propagation happens here!
	    true,
	    ( variant(Before, OriginalVars) ->
	    	true
	    ; 
		printf(Out, "P %mW%n", [OriginalVars])
	    ),
	    random_reduce_until_ground(Out, OriginalVars, Vars)
	).


random_reduce_one(VarArr) :-
	functor(VarArr,_,N),
	I is random_mod(N) + 1,
	arg(I, VarArr, X),
	( var(X) ->
	    random_reduce(X)
	; nonground(VarArr) ->
	    random_reduce_one(VarArr)
	).


random_sequence(X) :-
	var(X),
	random_reduce(X),
	printf("%mw%n",[X]),
	random_sequence(X).
random_sequence(X) :-
	nonvar(X).


random_reduce(X) :-		% random bound reduction on X
	var(X),
	get_finite_bounds(X, Min, Max),
	DomSizeMinus1 is Max - Min,
	NewDomSizeMinus1 is random_mod(DomSizeMinus1),
	LeftOver is DomSizeMinus1+1-NewDomSizeMinus1,
	NewL is Min + random_mod(LeftOver),
	NewH is NewL + NewDomSizeMinus1,
	X::NewL..NewH.
random_reduce([]).
random_reduce([X|Xs]) :-
	random_reduce(X),
	random_reduce(Xs).


%----------------------------------------------------------------------
% Make random domain variables
%----------------------------------------------------------------------

:- export make_n_random_variables/4.

make_n_random_variables(0, _, _, []) :- !.
make_n_random_variables(N, Min, Max, [X|Xs]) :-
	make_random_variable(Min, Max, X),
	N1 is N-1,
	make_n_random_variables(N1, Min, Max, Xs).

make_random_variable(Min, Max, X) :-
	Min =< Max,
	DomSizeMax is Max - Min + 1,		% 1 ..
	DomSizeMinus1 is random_mod(DomSizeMax),	% 0 .. < DomSizeMax
	LeftOver is DomSizeMax-DomSizeMinus1,	% 1 .. =< DomSizeMax
	L is Min + random_mod(LeftOver),	% Min .. Max
	H is L + DomSizeMinus1,
	X::L..H.


%----------------------------------------------------------------------
% Other random utilities
%----------------------------------------------------------------------

:- export random_int_between/3.
random_int_between(Min, Max, X) :-
	Diff is Max-Min+1,
	X is Min + random_mod(Diff).


:- export make_n_random_seeds/2.
make_n_random_seeds(N, Seeds) :-
	( for(_,1,N), foreach(Seed,Seeds) do
	    Seed is 1 + random_mod(2147483647)
	).
	

shuffle(L, R) :-
        add_random_keys(L, KL),
        keysort(KL, KR),
        rm_keys(KR, R).

        % add random key to each list element
add_random_keys([], []).
add_random_keys([A|L], [K-A|KL]) :-
        %random(K),
        frandom(K),
        add_random_keys(L, KL).

        % remove keys from association list
rm_keys([], []).
rm_keys([_K-A|KL], [A|L]) :-
        rm_keys(KL, L).


pick_randomly(N, List, PickedElems) :-
	shuffle(List, ShuffledList),
	length(PickedElems, N),
	once append(PickedElems, _, ShuffledList).

random_mod(Sup, X) :-
%	X is random mod Sup.
	X is fix(frandom * Sup).

%----------------------------------------------------------------------

