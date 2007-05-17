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
% Component:	ECLiPSe III compiler tests
% Version:	$Id: compiler_test.ecl,v 1.3 2007/05/17 23:59:43 jschimpf Exp $
% ----------------------------------------------------------------------

:- use_module(compiler_top).
:- lib(numbervars).
:- lib(coverage).


% Compile everything in coverage mode

ccompile :-
	coverage:ccompile(compiler_common),
	coverage:ccompile(compiler_normalise),
	coverage:ccompile(compiler_analysis),
	coverage:ccompile(compiler_indexing),
	coverage:ccompile(compiler_peephole),
	coverage:ccompile(compiler_codegen),
	coverage:ccompile(compiler_regassign),
	coverage:ccompile(compiler_varclass).


% Compile everything normally

compile :-
	eclipse_language:compile(compiler_common),
	eclipse_language:compile(compiler_normalise),
	eclipse_language:compile(compiler_analysis),
	eclipse_language:compile(compiler_indexing),
	eclipse_language:compile(compiler_peephole),
	eclipse_language:compile(compiler_codegen),
	eclipse_language:compile(compiler_regassign),
	eclipse_language:compile(compiler_varclass).


% Run one test (or all matching tests)

:- export op(200, fx, (test)).
test(Name) :-
	test(Name, [load:none,output:print,debug:off,expand_goals:off,print_indexes:on]).

test(Name, Options) :-
	(
	    testclause(Name, Pred0),
	    ( Pred0 = [_|_] -> Pred = Pred0 ; Pred = [Pred0] ),
	    printf("%n------ Test %w -------%n", [Name]),
	    writeclauses(Pred),

	    block(
		    ( compile_term(Pred, Options) ->
			true
		    ;
			printf("%n------ Test %w failed -------%n", [Name])
		    ),
		    Tag,
		    printf("%n------ Test %w aborted (%w) -------%n", [Name,Tag])
		),
	    fail
	;
	    true
	).

    writeclauses(Clauses) :-
	( foreach(Clause,Clauses) do
	    \+ \+ (
		numbervars(Clause, 0, _),
		writeclause(Clause)
	    )
	),
	nl.



% Run all tests

test :-
	test(_).


% Run all tests with output to file 'test.res'

ftest :-
	open("test.res",write,output),
	set_stream(warning_output, output),
	test,
	close(warning_output),
	close(output).


% Run coverage test

ctest :-
	ccompile,
	set_stream(output,null),
	set_stream(warning_output, null),
	test,
	close(warning_output),
	close(output),
	result.


%----------------------------------------------------------------------
% Test clauses
%
% testclause(TestName, ClauseOrListOfClausesForSinglePredicate)
%----------------------------------------------------------------------

% tests for variable classification and environment slot allocation

testclause(1, (
	p :- p(_X)
    )).
testclause(2, (
	p :- p(X),q(X),r(X)
    )).
testclause(3, (
	p :- (q(X);r(X))
    )).
testclause(4, (
	p :- p(X),(q(X);r(X))
    )).
testclause(5, (
	p :- (q(X);r(X)),p(X)
    )).
testclause(6, (
	p :- (q(X),r(X);s(X),t(X))
    )).
testclause(7, (
	p :- f(X),(q(X),r(X);s(X),t(X))
    )).
testclause(8, (
	p :- (q(X),r(X);s(X),t(X)),f(X)
    )).
testclause(9, (
	p :- (a(X) ; (b(X);c(X)) , (d(X);e(X)))
    )).
testclause(10, [(
	p(a,X) :- q(X)
    ),(
	p(b,X) :- r(X)
    )]).
	
testclause(11, (
	p :- p(X,X)
    )).
testclause(12, (
	p :- p(X,X),q(X,X),r(X,X)
    )).
testclause(13, (
	p :- (q(X,X);r(X,X))
    )).
testclause(14, (
	p :- p(X,X),(q(X,X);r(X,X))
    )).
testclause(15, (
	p :- (q(X,X);r(X,X)),p(X,X)
    )).
testclause(16, (
	p :- (q(X,X),r(X,X);s(X,X),t(X,X))
    )).
testclause(17, (
	p :- f(X,X),(q(X,X),r(X,X);s(X,X),t(X,X))
    )).
testclause(18, (
	p :- (q(X,X),r(X,X);s(X,X),t(X,X)),f(X,X)
    )).
testclause(19, (
	p :- a(X), (a(X,Y) ; a(X,Y,Y))
    )).
testclause(30, (
	p :- p, +(1,2,3)
    )).
testclause(31, (
	p :- p(X), q, +(X,2,3)
    )).
testclause(32, (
	p(X,X,_Y) :- q(a,b,X)
    )).
testclause(33, (
	% cannot lose both a(1) and a(2) as location for X!
	p(X,X) :- +(_A,_B,1), q(a,b,X)
    )).
testclause(34, (
	p(X,Y,Z) :- +(_A,_B,_C), p(X,Y,Z)
    )).
testclause(35, (
	p(X) :- (q(X);r(X))
    )).
testclause(36, (
	p :- _=X, q, r(X)
    )).
testclause(37, (
	p :- A=X, q(A), r(X)
    )).
testclause(38, (
	p(X) :- q, r(X)
    )).
testclause(39, (
	p(X) :- A=X, q(A), r(X)
    )).
testclause(40, (
	p :- q(X), (r1(Z),r2(X,Z);s(X))
    )).
testclause(41, (
	p :- (!; p(A),p(A),!)
    )).
testclause(42, [
	(p(A,B) :- !),
	(p(A,B) :- p(A),p(B),!)
    ]).
testclause(43, (
	p :- s(C),(c(C); p(A),p(A),c(C))
    )).
testclause(44, (
	p :- s(C),(c(C); p(A),p(C),c(A))
    )).
testclause(45, (
	p :- s(C), (p(C),q,r ; p,q(C),r ; p,q,r(C))
    )).
testclause(46, (
	p :- s(C), (p,q,r(C) ; p,q(C),r ; p(C),q,r)
    )).
testclause(47, (
	p :- s(C), (p,q(C),r ; p,q,r(C) ; p,q,r(C))
    )).

testclause(50, (
	p :- s(A), (p(A),q(B),r(B) ; p(B),q(B,C),r(C))
    )).
testclause(51, (
	p :- s(A), (p(A),q(B),r(B) ; p1(A),q1(D),q1(D) ; p(B),q(B,C),r(C))
    )).
testclause(52, (
	p :- (p(A) ; q), (p(A) ; q)
    )).
testclause(53, (
	p :- (p(A) ; q(A)), (p(A) ; q)
    )).
testclause(54, (
	p(X) :- X=a, p, p(X)
    )).
testclause(55, (
	p(X) :- X=T, p(T), p(X)
    )).
testclause(56, (
	p(X,T) :- X=T, p(T), p(X)
    )).
testclause(57, (
	p(X,Y) :- X=Y, p, p(X,Y)
    )).
testclause(58, (
	p(X) :- X=Y, p, p(X,Y)
    )).
testclause(59, (
	p(X) :- X=_, p, p(X)
    )).
testclause(60, (
	p :- X=Y, p, p(X,Y)
    )).
	

testclause(101, (
    	p :- p(X),q(Y,X),r(Y),t(Y)
    )).
testclause(102, (
    	p :- p(X),q(X,Z,_T),r(Z,Y),t(Y)
    )).
testclause(103, (
	p :- ( f(Y),g(Y),a(X) ; b(Z),c(Z),d(X) ), e(X)
    )).
testclause(104, (
	p :- ( f(Y),g(Y),a(X) ; b(Z),c(Z) ), e(X)
    )).
testclause(105, (
	p :- ( f(Y),g(Y),a(X) ; b(Z),c(Z) ), e(X), f(X)
    )).
testclause(106, (
	p :- a(X), ( b(X),c(Y) ; d(X),e(Y) ), f(Y)
    )).
testclause(107, (
	p :- a(X), ( b(X),c(Z) ; d(X),e(Z) ), f(_Y)
    )).
testclause(108, (
	p :- a(X), ( b(X) ; c ), d
    )).
testclause(109, (
	p :- a, b(X), c(Y), d(X), e(Y), f
    )).
testclause(110, (
	p :- a(A), a(A,B,C), ( a ; b(B), c(B) ; d(D), e(D) ; f(E), g(E) ), f(D,C,F), g(F)
    )).

testclause(120, (
	(sentence(B,C,D,E,F) :- declarative(B,C,G,E,H),terminator(.,G,D,H,F))
    )).


% put sequences

testclause(200, (
	p :- a(X,X)
    )).
testclause(201, (
	p :- a(f(X),X)
    )).
testclause(202, (
	p :- a(f(X),X), b(f(X),X), c(X,f(X))
    )).
testclause(203, (
	p :- a(f(X),g(X)), b(f(g(X),X)), c(f(X,g(X)))
    )).
testclause(204, (
	p(X) :- a(X)
    )).
testclause(205, (
	p(X) :- a(a,X)
    )).
testclause(206, (
	p(a)
    )).
testclause(207, (
	p(X) :- a(X,X)
    )).
testclause(208, (
	p(X,Y) :- a(X,Y)
    )).
testclause(209, (
	p(X,Y) :- a(Y,X)
    )).
testclause(210, (
	p :- X=Y, a(Y,X)
    )).
testclause(211, (
	p :- a(X,f(X))
    )).
testclause(212, (
	p(X) :- a(f(X))
    )).
testclause(213, (
	p(X,Y,Z) :- a(Z,X,Y)
    )).
testclause(214, (
	p(X,Y,Z) :- a(f(Z),f(X),f(Y))	% subopt
    )).
testclause(215, (
	p(X,Y,Z,X) :- a(Z,X,Y)	% subopt
    )).
testclause(216, (
	p(X,Y,Z,X) :- a(f(Z),f(X),f(Y))	% subopt
    )).
testclause(217, (
	p(f(X)) :- q(X)
    )).
testclause(218, (
	p(X,Y,Z) :- a(Y,Z,X)
    )).
testclause(219, (
	p(X,Y,Z) :- A=X, a(Z,A,Y)
    )).
testclause(220, (
	p(X,Y,Z) :- A=X, a(Y,Z,A)
    )).
testclause(221, (
	p :- a(X,f(X))
    )).
testclause(222, (
	p :- q(a,X,_Y,X)
    )).
testclause(223, (
	p :- q(X,X), r(X)
    )).
testclause(224, (
	p :- q(f(X),X), r(X)
    )).
testclause(225, (
	p :- q([a]), r(_X)
    )).
testclause(226, (
	p :- q([a,b]), r(_X)
    )).
testclause(227, (
	p :- q([a,X,b]), r(X)
    )).
testclause(228, (
	p :- q([a,b|X]), r(X)
    )).
testclause(229, (
	p :- q([X],X), r(X)
    )).
testclause(230, (
	p :- q(X,[X]), r(X)
    )).
testclause(231, (
	p :- q([X],X)
    )).
testclause(232, (
	p :- q(X,[X])
    )).
testclause(233, (
	p(X) :- q([X],X)
    )).
testclause(234, (
	p(X) :- q(X,[X])
    )).
testclause(235, (
	p(_X) :- q("hello",3.4,5_2)
    )).
testclause(236, (
	p(_X) :- q(f("hello",3.4,5_2))
    )).

% test cases from Janssens,Demoen,Marien
testclause(jdm(1), (
	p(T,U,a):-q(T,b,f(U))
    )).
testclause(jdm(2), [
	del(t(nil,X,R),X,R),
	del(t(L,X,nil),X,L),
	(del(t(L,X,R),X,r(L,Y,R1)) :- delmin(R,Y,R1))
    ]).
testclause(jdm(3), [
    	dob(person(_,_,D,_),D)
    ]).
testclause(jdm(4), [
	( qsort([X|Tail],Sorted) :-
	    split(X,Tail,Small,Big),
	    qsort(Small,SortedSmall),
	    qsort(Big,SortedBig),
	    append(SortedSmall,SortedBig,Sorted)
	)
    ]).
testclause(jdm(r4), [
	( qsort(Sorted,[X|Tail]) :-
	    split(X,Tail,Small,Big),
	    qsort(Small,SortedSmall),
	    qsort(Big,SortedBig),
	    append(SortedSmall,SortedBig,Sorted)
	)
    ]).
testclause(jdm(5), [
	rev(L,R) :- revacc([],L,R)
    ]).
testclause(jdm(6), [
	p(T,U,V,W) :- q(a,T,U,V,W)
    ]).

% test cases from Matyska,Jergova,Toman
testclause(mjt(1), (
	p(f(T),g(T)) :- q(a,T)
    )).
testclause(mjt(2), (
	p(b(X,Z),Z,Y,a(X)) :- q(c,Y,X,Z)
    )).
testclause(mjt(3), (
	p(g(Y),s(X,h(Y)),f(X)) :- var(X), var(Y), q(a,Y,X)
    )).

% simple predicates
testclause(300, (
	p(X,Y) :- p(X), +(X,Y,Z), -(X,_A,Z), q(Z)
    )).
testclause(301, (
	p(X,Y) :- p(X), r(X,Y,Z), q(Z)
    )).
testclause(302, (
	p(X,Y) :- p(Y), r(X,Y,Z), q(Z)
    )).
testclause(303, (
	p :- q, X=1, Z=3, p(Y), r(X,Y,Z)
    )).
testclause(304, (
	p :- q, A1=f(X),a(A1,X)
    )).
testclause(305, (
	p(X,Y) :- Z=1, X=Y, q(Y,Z)
    )).
testclause(306, (p1(X) :- X=1)).
testclause(307, (p2(X) :- 1=X)).
testclause(308, (p3(X) :- X=1, p1(X), p1(X))).	% TODO: get_integer before move A Y
testclause(309, (p4(X) :- 1=X, p1(X), p1(X))).
testclause(310, (p5(f(X)) :- X=1, p1(X), p1(X))).
testclause(311, (p5a(f(X)) :- p1(X), X=1, p1(X))).
testclause(312, (p6(f(X)) :- 1=X, p1(X), p1(X))).
testclause(313, (p6a(f(X)) :- p1(X), 1=X, p1(X))).
testclause(314, (p7(f(X)) :- X=1, p1(X))).
testclause(315, (p8(f(X)) :- 1=X, p1(X))).
testclause(316, (p9(Y) :- X=1, p1(X), p1(X), X=Y)).
testclause(317, (p9a(Y) :- p1(_), X=1, p1(X), p1(X), X=Y)).
testclause(318, (p10(Y) :- 1=X, p1(X), p1(X), X=Y)).
testclause(319, (p10a(Y) :- p1(_), 1=X, p1(X), X=Y)).
testclause(320, (p11(Y) :- X=1, call(true), X=Y)).
testclause(321, (p12(A, B, C) :- X=1, Y=2, Z=3, call(true), A=X, B=Y, C=Z)).
testclause(322, (p13(A, B) :- X=1, Y=2, _=3, current_op(_, _, _), A=X, B=Y)).
testclause(323, (b1 :- D=s(X), eq(X,Y), D=s(1), Y = 1)).


testclause(u(1), (
	p(foo(_,X,Y)) :- q(X), r(Y)
    )).

testclause(ana(1), (
	p :- q, X=1, _Z=3, p(X)
    )).
testclause(ana(2), (
	p :- X=1, X=_Y
    )).
testclause(ana(3), (
	p :- _X=_Y
    )).
testclause(ana(4), (
	p :- Y=3, _X=Y
    )).
testclause(ana(5), (
	p :- Y=3, q, Y=4, r
    )).
testclause(ana(6), (
	p :- Y=3, q, X=Y, r, X=4
    )).
testclause(ana(7), (
	p :- Desc=f(_A,_), q, Desc=f(_,_B), r
    )).
testclause(ana(8), (
	p :- integer(X), ( X=1 ; X=2), q
    )).
testclause(ana(9), (
	p(X,Y) :- f(a,Y)=f(X,b), q
    )).


testclause(idx(1), (
	(p :- p(X), ( X=1, p1 ; X=2, p2))
    )).
testclause(idx(2), (
	(p :- p(X,Y), ( Y=a, X=1, p1 ; X=2, Y=b, p2))
    )).
testclause(idx(3), (
	(p(X,Y) :- ( Y=a, X=1, p1 ; X=2, Y=b, p2 ; Y=c, p3))
    )).
testclause(idx(4), (
	(p(X,Y) :- ( Y=a, X=1, p1 ; integer(X), Y=b, p2; number(X), p3))
    )).
testclause(idx(5), (
	(p(X) :- ( X=1, p1 ; X=2, p2 ; integer(X), p3; number(X), p4))
    )).
testclause(idx(6), (
	(p :- p(X,Y), ( Y==a, X=1, p1 ; X=2, Y==b, p2))
    )).
testclause(idx(7), (
	(p(X) :- ( X=[], p1 ; X=[_|_], p2))
    )).
testclause(idx(8), (
	(p(X) :- ( var(X), !, p ; q ))
    )).
testclause(idx(9), (
	(p(X) :- ( X=3.1, p ; X=9.9, q ))
    )).
testclause(idx(10), (
	(p(X) :- ( X="foo", p ; X="bar", q ; r))
    )).
testclause(idx(11), (
	(p(X) :- ( X=f(1), p ; X = g(1,2), q ; r ))
    )).
testclause(idx(12), (
	(p(X) :- ( X=[], p ; r ))
    )).
testclause(idx(13), (
	(p(X) :- ( X=[] -> p ; r ))
    )).
testclause(idx(14), (
	(p :- p(X,Y), ( a==Y, 1=X, p1 ; 2=X, b==Y, p2))
    )).
testclause(idx(15), (
	(p :- p(X,Y), ( a==Y, 1=X, p1 ; 2=X, b==Y, p2 ; X==Y, p3))
    )).
testclause(idx(16), (
	(p(_X) :- ( var(_Z), !, p ; q ))
    )).
testclause(idx(20), (
	(p(X) :- ( X==[], p ; X==[a], q ))
    )).
testclause(idx(21), (
	(p(X) :- ( X==f(a), r ; X=="hello", s))
    )).
testclause(idx(22), (
	(p(X) :- ( X==[], p ; X==[a], q ; X==f(a), r ))
    )).
testclause(idx(23), (
	(p :- ( _==[], p ; q))
    )).
testclause(idx(24), (
	(p(X) :- ( X=f(_), !, p1 ; X=[_|_], !, p2 ; compound(X), p2 ))
    )).
testclause(idx(25), (p(X) :-
	    ( X=1, p_integer
	    ; X=10000000000000000000, p_big
	    ; X=1.1, p_float
	    ; X=5_2, p_rational
	    ; X=0.9__1.1, p_breal
	    ; X="hello", p_string
	    ; X=hello, p_atom
	    ; X=[], p_nil
	    ; X=[_|_], p_list
	    ; X=foo(bar), p_struct
    ))).
testclause(idx(26), (p(X) :-
	    ( var(X), p_var
	    ; integer(X), p_integer
%	    ; sepia_kernel:bignum(X), p_big
	    ; float(X), p_float
	    ; rational(X), p_rational
	    ; breal(X), p_breal
	    ; string(X), p_string
	    ; nonvar(X), X=[], !, p_nil
	    ; atom(X), p_atom
	    ; nonvar(X), X=[_|_], !, p_list
	    ; compound(X), p_struct
	    ; is_handle(X), p_handle
    ))).
testclause(idx(27), (
	(p(X) :- ( atom(X), p_string ; X = hello, p_hello ))
    )).
testclause(idx(28), (
	(p(X) :- ( X="abc",p1 ; X="def",p2 ; X=ghi, p3))
    )).
testclause(idx(29), ([
	(p(X) :- X=1, p, q(X)),
	(p(X) :- X=2, q, r(X))
    ])).
testclause(idx(30), (
	(p(X) :- ( X=1, !, p1 ; X=2, !, p2 ; p4 ))
    )).
testclause(idx(31), (
	(p(X) :- ( X=1, !, p1 ; X=2, !, p2 ; integer(X), p4 ))
    )).
testclause(idx(32), (
	(p(X) :- ( X=f(_), !, p1 ; X=g(_), !, p2 ; X=[_|_], !, p3 ; compound(X), p4 ))
    )).
testclause(idx(33), (
	[(p(f(A,B,C)) :- !, p1(A,B,C)), (p([A|B]) :- !, p2(A,B)), (p(X) :- compound(X), p3)]
    )).
testclause(idx(34), (
	[(p(f(A,B,A),C) :- !, p1(A,B,C)), (p([A|B],B) :- !, p2(A,B)), (p(X,_) :- compound(X), p3)]
    )).
testclause(idx(35), (
	[(p(f(A,B,C)) :- -?-> !, p1(A,B,C)), (p([A|B]) :- -?-> !, p2(A,B)), (p(X) :- compound(X), p3)]
    )).
testclause(idx(36), (
	[(p(f(A,B,C),C) :- -?-> !, p1(A,B,C)), (p([A|B],B) :- -?-> !, p2(A,B)), (p(X,_) :- compound(X), p3)]
    )).
testclause(idx(40), (
	(p(X) :- ( free(X), p1 ; meta(X), p2 ; p3 ))
    )).
testclause(idx(41), (
	(p(X) :- ( free(X), !, p1 ; meta(X), !, p2 ; p3 ))
    )).
testclause(idx(42), (
	(p(X) :- ( free(X), p1 ; meta(X), p2 ; atom(X), p3 ))
    )).
testclause(idx(43), (
	(p(X) :- ( free(X), p1 ; p2 ))
    )).
testclause(idx(44), (
	(p(X) :- ( free(X), !, p1 ; p2 ))
    )).
testclause(idx(45), (
	(p(X) :- ( free(X), p1 ; p2 ))
    )).
testclause(idx(46), (
	(p(X) :- ( meta(X), p1 ; p2 ))
    )).
testclause(idx(47), (
	(p(X) :- ( meta(X), !, p1 ; p2 ))
    )).
testclause(idx(48), (
	(p(X) :- ( meta(X), p1 ; p2 ))
    )).
testclause(idx(49), (
	(p(X) :- ( free(X), p1 ; var(X), p2 ; p3 ))
    )).
testclause(idx(50), (
	(p(X) :- ( free(X), !, p1 ; var(X), !, p2 ; p3 ))
    )).
testclause(idx(51), (
	(p(X) :- ( free(X), p1 ; var(X), p2 ; atom(X), p3 ))
    )).
testclause(idx(52), (
	(p(X) :- ( var(X), !, p1 ; p2))
    )).
testclause(idx(53), (
	(p(X) :- ( var(X), !, p1 ; X=1, p2 ; X=2, p3))
    )).
testclause(idx(54), (
	(p(X) :- ( var(X), !, p1 ; integer(X), X=1, p2 ; integer(X), X=2, p3))
    )).
testclause(idx(55), (
	(p(X) :- ( var(X), !, p1 ; X=1, !, p2 ; integer(X), p3))
    )).
testclause(idx(56), (
	(p(X,Y) :- ( integer(X), Y=a, p1 ; integer(X), Y=b, p2 ; integer(X), Y=c, p3 ; atom(X), p4))
    )).
testclause(idx(100), ([
	(p(1) :- -?-> p_1),
	(p(2) :- -?-> p_2)
    ])).
testclause(idx(101), ([
	    ( p(1) :- -?-> p_integer),
	    ( p(10000000000000000000) :- -?-> p_big),
	    ( p(1.1) :- -?-> p_float),
	    ( p(5_2) :- -?-> p_rational),
	    ( p(0.9__1.1) :- -?-> p_breal),
	    ( p("hello") :- -?-> p_string),
	    ( p(hello) :- -?-> p_atom),
	    ( p([]) :- -?-> p_nil),
	    ( p([_|_]) :- -?-> p_list),
	    ( p(foo(bar)) :- -?-> p_struct)
    ])).
testclause(idx(200), ([
	( p(X,Y,Z) :- var(X), !, Y=Z),
	( p(a,Y,Z) :- !, a(Y,Z)),
	( p(X,Y,Z) :- q(X,Y,Z))
    ])).

testclause(head(100), ( p(a) :- q)).
testclause(head(101), ( p(a,_X,3) :- q)).
testclause(head(102), ( p(X,X) :- q)).
testclause(head(103), ( p(a,X,_Y,3.5,5_2,"hello",X) :- q)).

testclause(head(1), (p(f(1)) :- q)).
testclause(head(2), (p(f(1,2)) :- q)).
testclause(head(3), (p(f(g(1))) :- q)).
testclause(head(4), (p(f(g(1),h(2))) :- q)).
testclause(head(5), (p(f(g(1),h(2),i(3))) :- q)).
testclause(head(6), (p(f(4,g(1),h(2),i(3))) :- q)).
testclause(head(7), (p(f(4,g(1),5,h(2),i(3))) :- q)).
testclause(head(8), (p(f(4,g(1),5,h(2),6,i(3))) :- q)).
testclause(head(9), (p(f(4,g(1),5,h(2),6,i(3),7)) :- q)).
testclause(head(10), (p(f(4,g(1),5,h(2),6,i(3),7)) :- q)).
testclause(head(11), (p(f(4,g(1),5,h(2),6,i(3),7,8)) :- q)).
testclause(head(12), (p(f(g(1),5,h(2),6,i(3),7,8)) :- q)).
testclause(head(13), (p(f(g(1),h(2),6,i(3),7,8)) :- q)).
testclause(head(14), (p(f(g(1),h(2),i(3),7,8)) :- q)).
testclause(head(15), (p(f(g(1),h(2),i(3),8)) :- q)).
testclause(head(16), (p(X,Y) :- X = f(Y,Z), q(X,Z))).
testclause(head(17), (p(X,Y) :- X = f(Z,Y), q(X,Z))).
testclause(head(18), (p(X,Y) :- X = f(g(Y),Z), q(X,Z))).
testclause(head(19), (p(f(X)) :- q, r(X))).
testclause(head(20), (p(f(X,X)) :- q, r(X))).

testclause(head(20), (p([g(1),h(2)]) :- q)).
testclause(head(21), (p([[1,2],[3|T],[4,5],c]) :- q(T))).
testclause(head(22), (p([a,[1,2],[3,4],b,[5|T],c]) :- q(T))).
testclause(head(23), (p([a,[1,2],[3,4],b,[5|T]|c]) :- q(T))).
testclause(head(24), (p([a,[1,2],[3,4],b,[5|T]]) :- q(T))).

testclause(head(30), (p(f([1,2],[3|T],c)) :- q(T))).
testclause(head(31), (p(f(a,[1,2],b,[3|T],c)) :- q(T))).
testclause(head(32), (p(f(a,[1,2],[3|T],c)) :- q(T))).
testclause(head(33), (p(f(a,[1,2],b,[3|T])) :- q(T))).
testclause(head(34), (p(f("hello",3.5,5_3)) :- q)).

testclause(match(1), (p(1,2.3,3_4,a,"s",[]) ?- true)).
testclause(match(2), (p(X,Y,_,Z,Z) ?- q(X),r(Y))).
testclause(match(3), (p(X,X,Y,Y) ?- q(X),r(Y))).
testclause(match(4), (p(foo(bar(1),_,baz(2))) ?- true)).
testclause(match(5), (p([1,2,[3,4]]) ?- true)).
testclause(match(6), (p(foo(X,X)) ?- true)).
testclause(match(7), (p(foo(X,X,Y,Y)) ?- q(X),r(Y))).
testclause(match(8), (p(X,foo(X,Y),Y) ?- true)).
testclause(match(9), (p(X,foo(X,Y),Y) ?- p(X),r(Y))).

testclause(match(10), (p(X{suspend:S}) ?- p(X,S))).
testclause(match(11), (p(X{suspend:S}) ?- p(a,X,S))).
testclause(match(12), (p(X{suspend:S},X) ?- p(a,b,X,S))).
testclause(match(13), (p(X{suspend:S}) ?- q, p(X,S))).
testclause(match(14), (p(X{suspend:S},X) ?- q, p(X,S))).
testclause(match(20), (p(f(X{suspend:S})) ?- p(X,S))).
testclause(match(21), (p(f(a,X{suspend:S})) ?- p(X,S))).
testclause(match(22), (p(f(a,X{suspend:S},c)) ?- p(X,S))).
testclause(match(23), (p(f(bar(baz),X{suspend:S},c)) ?- p(X,S))).
testclause(match(24), (p(f(X{suspend:S},X)) ?- p(X,S))).	% suboptimal, matches attributes twice
testclause(match(25), (p(f(bar(baz),a,X{suspend:S},c)) ?- p(X,S))).
testclause(match(30), [(p(X{suspend:S}) ?- p(X,S)),
	(p(X) :- integer(X), q(X))]).
testclause(match(31), [(p(X{suspend:S}) ?- !, p(X,S)),
	(p(X) :- q(X))]).
testclause(match(32), [(p(X{suspend:S}) ?- p(X,S)),
	(p(X) :- free(X), q(X))]).
testclause(match(33), [(p(X{suspend:S}) ?- p(X,S)),
	(p(X) :- meta(X), q(X))]).
testclause(match(34), [(p(X) :- free(X), q(X)),
	(p(X{suspend:S}) ?- p(X,S))]).

testclause(unify(1), (p(X,Y,Z) :- q, X = f(1,g(Y,W),V,h(Z,Y)), r(Z,W),s(V))).

testclause(dis(1), (p :- (a;b;c;d))).
testclause(dis(2), (p :- (((a;b);c);d))).
testclause(dis(3), (p :- (a;(b;c);d))).
testclause(dis(4), (p :- (a->aa;b))).
testclause(dis(5), (p :- q,(a->aa;b))).
testclause(dis(6), (p :- (a->aa;b->bb;c->cc;d))).
testclause(dis(7), (p :- (a->aa;b->bb;c->cc;d->dd))).
testclause(dis(8), (p :- (a;b->bb;c->cc;d))).
testclause(dis(9), (p :- (a->aa;b;c->cc;d))).
testclause(dis(10),(p :- (a->aa;b->bb;c;d))).
testclause(dis(11),(p :- (a->aa;(b->bb;c);d))).

testclause(clause(1), [(p :- a),(p :- b)]).
testclause(clause(2), [(p :- a),(p :- b),(p:-c,d)]).
testclause(clause(3), [(p :- a->aa),(p :- b)]).

testclause(special(1), (p(G) :- G)).
testclause(special(2), (p(G) :- call(G))).
testclause(special(3), (p(G,M) :- G@M)).
testclause(special(4), (p(G) :- G@lists)).
testclause(special(5), (p(G,M) :- M:G)).
testclause(special(6), (p(G) :- lists:G)).
testclause(special(7), (p :- 3=3)).
testclause(special(8), (p :- 3=4)).
testclause(special(9), (p :- call(q))).
testclause(special(10), (p :- q, call((a,!,b)), r)).
testclause(special(11), (p(M) :- call(q)@M)).
testclause(special(12), (p :- call(q)@lists)).
testclause(special(13), (p :- call(writeln(hello))@lists)).
testclause(special(13), (p :- writeln(hello)@lists)).

testclause(tool(1), (p(DM) :- get_flag(p/0,definition_module,DM))).
testclause(tool(2), (p(DM) :- get_flag(p/0,definition_module,DM)@lists)).
testclause(tool(3), (p(M,DM) :- get_flag(p/0,definition_module,DM)@M)).
testclause(tool(4), (p :- q@lists)).
testclause(tool(5), (p(M) :- q@M)).

testclause(cut(1), (p :- !)).
testclause(cut(2), (p :- q, !)).
testclause(cut(3), (p :- q, !, r)).
testclause(cut(4), [(p :- q, !, r),(p :- s)]).
testclause(cut(5), [(p :- a),(p :- !,bb),(p:-c,d)]).
testclause(cut(6), [(p :- a),(p :- b,!,bb),(p:-c,d)]).

testclause(env(1), [
	(p :-
	    q(A,B,C,D,E,F,G),
	    r(H,I,J,K,L,M,N),
	    q(A,B,C,D,E,F,G),
	    r(H,I,J,K,L,M,N)
	)
]).
testclause(env(2), [
	(p :-
	    q(A0,B0,C0,D0,E0,F0,G0,H0,I0,J0,K0,L0,M0,N0,O0,P0,Q0,R0,S0,T0),
	    r(A1,B1,C1,D1,E1,F1,G1,H1,I1,J1,K1,L1,M1,N1,O1,P1,Q1,R1,S1,T1),
	    s(A0,B0,C0,D0,E0,F0,G0,H0,I0,J0,K0,L0,M0,N0,O0,P0,Q0,R0,S0,T0),
	    t(A1,B1,C1,D1,E1,F1,G1,H1,I1,J1,K1,L1,M1,N1,O1,P1,Q1,R1,S1,T1)
	)
]).

testclause(bench(1), [
    (conc([], Ys, Ys)),
    (conc([X|Xs], Ys, [X|XsYs]) :- conc(Xs, Ys, XsYs))
]).
testclause(bench(2), [
    (conc(A,B,C) :-
	    A=[], B=Ys, C=Ys
	;
	    A=[X|Xs], B=Ys, C=[X|XsYs], conc(Xs, Ys, XsYs)
    )
]).
testclause(bench(3), [
    (conc(A,B,C) :-
	    A=[X|Xs], B=Ys, C=[X|XsYs], conc(Xs, Ys, XsYs)
	;
	    A=[], B=Ys, C=Ys
    )
]).

testclause(bug(1), [
    simplify_code([], []),
    (simplify_code([code(Instr,_,_)|More], SimplifiedCode) :-
	    ( simplify(Instr, More, SimplifiedCode0) ->
		simplify_code(SimplifiedCode0, SimplifiedCode)
	    ;
		SimplifiedCode = [Instr|SimplifiedCode0],
		simplify_code(More, SimplifiedCode0)
	    ))
]).
testclause(bug(2), [
    (loop(Xs) :- ( foreach(X,Xs) do writeln(X) ))
]).
testclause(bug(3), [
    (p(A, B, C) :-
	( A = B -> C = foo ; else )
    )
]).
testclause(bug(4), [
    (p(First, Last, PermSlots2) :-
	( First == Last ->
	    _Where = temp,
	    PermSlots2=PermSlots1
	;
	    PermSlots2=[_Slot|PermSlots1]
	)
    )
]).
testclause(bug(5), [
    (indexing_transformation([A|_], C) :-
	(
	    A = disjunction(_)
	->
	    then
	;
	    C = [_|_]
	)
    )
]).
testclause(bug(6), [
    insert_after_head(IndexPoint, Branch, IndexedBranch) :-
	( Branch = [Head|RestOfBranch], Head = goal(head,_) ->
	    IndexedBranch = [Head,IndexPoint|RestOfBranch]
	;
	    IndexedBranch = [IndexPoint|Branch]
	)
]).
testclause(bug(7), [
    % bug was that integer index table wasn't sorted.
    % TODO: no A2 index, because occurrence of :/2 isn't considered inside
    % guard, because cut occurs only after regular goal (clause 4).
    (declaration_warning_handler(_N, _Pred, lists) :- !),
    (declaration_warning_handler(_N, _Pred, profile) :- !),
    (declaration_warning_handler(75, Pred, Module) :- !,
	get_flag_body(Pred, definition_module, DM, Module),
	get_deprecation_advice(Pred, DM, Advice),
	!,
	warning_handler(75, Pred, Module),
	printf(warning_output, " Advice: %w%n", [Advice])),
    (declaration_warning_handler(85, BadModule:_, _Module) :-
	known_library(BadModule),
	!),
    (declaration_warning_handler(N, Pred, Module) :-
	warning_handler(N, Pred, Module))
]).
testclause(bug(8), [
    (attach_suspensions(postponed, Susp) ?- !,
	postpone_suspensions(Susp)),
    (attach_suspensions(Trigger, Susp) :-
	atom(Trigger), !,
	attach_suspensions1(Trigger, Susp)),
    (attach_suspensions(Trigger, Susp) :-
	nonvar(Trigger), !, 
	error(5, attach_suspensions(Trigger, Susp))),
    (attach_suspensions(Trigger, Susp) :-
	error(4, attach_suspensions(Trigger, Susp)))
]).

%----------------------------------------------------------------------
% The following are tests from the old compiler test suite
%----------------------------------------------------------------------

% Allocated temporaries: temporary variables that have their first occurrence
%  in a compound argument of a simple goal and that occur in more than one goal.

testclause(at1, [(at1 :- var(f(X)), X == a, q(X, f(X)))]).
testclause(at2, [(at2 :- f(X) == f(Y), X == Y, q(X, Y))]).
testclause(at3, [(at3 :- f(X) == f(Y), X == Y, q(f(X), f(Y)))]).
testclause(at4, [(at4 :- q, f(X) == f(Y), X == Y, q(X, Y))]).
%testclause(at5, [(at5 :- var(X{a}), X == a, q(X, f(X)))]).
%testclause(at6, [(at6 :- X{a} == Y{b}, X == Y, q(X, Y))]).
%testclause(at7, [(at7 :- Y{a} == X{b}, X == Y, q(f(X), f(Y)))]).
%testclause(at8, [(at8 :- q, X{a} == Y{b}, X == Y, q(X, Y))]).
%testclause(at9, [(at9 :- var(f(_X)), var(Y{a}))]).

% **** optimization not done in new compiler ***
% Global temporaries are nonvoid temporary variables that have their first
%  occurrence in a compound argument of a goal and that occur only in this
%  goal. Since their position on the global stack is known, the first
%  occurrence is treated like a void and the subsequent ones use an S offset
%  Variable that occur in regular subgoal arguments are allocated
%  in that argument

testclause(ag1, [(ag1 :- *(f(X), f(X), f(X)))]).
testclause(ag2, [(ag2 :- *(X, X, f(X)))]).
testclause(ag3, [(ag3 :- X == f(X), q(X))]).
%testclause(ag4, [(ag4 :- *(X{a}, X, X), f(Y{a}) == f(Y), Z{a} == f(Z), f(U{a}) == U)]).
testclause(ag5, [(ag5 :- p(f(X), f(X), f(X)))]).
testclause(ag6, [(ag6 :- p(X, X, f(X)), p(f(Y), f(Y)), p(Z, f(Z)), p(f(U), U))]).
%testclause(ag7, [(ag7 :- p(X{a}, X), p(f(Y{a}), f(Y)), p(Z{a}, f(Z)), p(f(U{a}), U))]).
testclause(ag8, [(ag8 :- X =.. [f, Y, Y], q(X))]).

% Variables that occur first in an argument of a simple goal and then
%  in a compound argument of the same goal

testclause(am1, [(am1 :- f(X) == X)]).
testclause(am2, [(am2 :- f(X) == X, q(X))]).
testclause(am3, [(am3 :- f(X) == X, q(X), r)]).
testclause(am4, [(am4 :- *(f(X), X, f(X)))]).
testclause(am5, [(am5 :- p(X), *(f(X), X, f(X)), q(X))]).
testclause(am6, [(am6 :- var(X), *(X, f(X), X), q(X))]).
testclause(am7, [(am7 :- p(X), *(f(X), X, f(X)), q(X))]).
testclause(am8, [(am8 :- p(X), *(X, f(X), X), q(X))]).
testclause(am9, [(am9 :- var(X), f(X) == X, q(X))]).
testclause(am10, [(am10 :- p(X), var(X), f(X) == X, q(X))]).
testclause(am11, [(am11 :- var(X), f(X) == X)]).
testclause(am12, [(am12 :- var(X), X == f(X))]).
testclause(am13, [(am13 :- X == f(X))]).
testclause(am14, [(am14 :- eq(X, _Y), f(X) == X)]).


% Simple If-Then-Else
%  Last goal
testclause(disj1, [(p :- var(_X) -> b; c)]).
testclause(disj2, [(p :- var(X) -> b; var(X) -> c; d)]).
testclause(disj3, [(p :- var(_X) -> var(_Y); var(_Z))]).

% Not last goal
testclause(disj4, [(p :- (var(_X) -> b; c), e, f)]).
testclause(disj5, [(p :- (var(X) -> b; var(X) -> c; d), e, f)]).
testclause(disj6, [(p :- (var(_X) -> var(_Y); var(_Z)), e, f)]).

testclause(disj7, [(p :- (var(_A); var(_B)), c, d)]).

% Blocks and cuts

testclause(disj8, [(p :- once(true)->fail; true)]).

testclause(disj9, [(p :- q, once(var(_X)), p)]).

testclause(disj10, [(p :- q, once(var(_X)))]).

testclause(disj11, [(p :- (a, b -> c) -> q)]).

testclause(disj12, [(p :- q, var(_X)->true)]).

testclause(disj13, [(p :- q, var(X), !, p(X))]).

testclause(disj14, [(p :- (a; var(X) -> var(X)), b)]).

testclause(disj15, [(p :- var(X) -> var(X), !; (p, var(X), !; var(X), !))]).

testclause(disj16, [(p :- not((p, !, once((a, !, b)))))]).

% Various
testclause(disj17, [(p :- q(_); r(_))]).

testclause(disj18, [(p1(A, B, f(C)) :- A = B, A = C, a = b, q, argc(X), X = a)]).

testclause(disj19, [(t :- not(not((!, fail))))]).

testclause(disj20, [(t :- true->not(true->fail))]).

testclause(disj21, [(t :- q, var(A) -> p(A))]).

testclause(disj22, [(q(X) :- p(a) -> r(X))]).

testclause(disj23, [(g( A, Term, B ) :-
    copy_term( Term, d( A, B, AccessTerm ) ),
    once( AccessTerm ))]).

testclause(disj24, [(p2( [_H|T], M, S ) :-
    l( M, S, _A/_B ),
    q( T, M, S ))]).

testclause(disj25, [(p(X) :- ((X=1, !; X=2), fail); X = 4)]).

testclause(disj26, [(p(A, B) :-
	var(X),
	q(X) -> r(X); s(A, B))]).

testclause(disj27, [(p3(X, _Y, _Z) :-
     (X = '' ->
	 A = "", B = C
     ;
	 A = "a", B = [X, C]
     ))]).

testclause(disj28, [(s :-
    (Type==open ->
	true
    ;
    Type==def ->
	(transformed(_R) -> true ; true)
    ))]).

testclause(disj29, [(test(X):-
     !,
     writeln(clause1),
     (
	     X = 1
     ->
	     writeln(ok)
     ;
	     writeln(wrong)
     ))]).

testclause(disj30, [(test:-
     getcond(A, B),
     ( A -> ( B -> true)))]).

testclause(disj31, [(go(L) :-
	L = [_Aa, _Bb, _Cc],
	findall( X, member(X, L), L0),
	L0 = [1,2,3], nl, write('L = '), write(L))]).

testclause(disj32, [(append([X|L1], L2, [X|L3]) :- append(L1, L2, L3))]).


testclause(disj33, [(c(X, Y) :- (X=Y->a;b),c)]).

testclause(disj34, [(producer(S) :-
	random(N),
	(N > 0
	->      S = [N|S1]
	;       S = [zero|S1]),
	producer(S1))]).
