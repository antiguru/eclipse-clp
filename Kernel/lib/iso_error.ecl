% ----------------------------------------------------------------------
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
% The Original Code is  The iso_error compatibility library for ECLiPSe
% The Initial Developer of the Original Code is  Cisco Systems, Inc. 
% Portions created by the Initial Developer are
% Copyright (C) 2009 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): Joachim Schimpf, Coninfer Ltd
% 
% END LICENSE BLOCK
%
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: iso_error.ecl,v 1.1 2011/04/27 12:25:57 jschimpf Exp $
% ----------------------------------------------------------------------

%
% ISO errors
% 
% 
% instantiation_error
%     instantiation_error.
% 
% type_error(ValidType, Culprit)
%     type_error(list, [foo|bar]).
% 
% domain_error(ValidDomain, Culprit)
%     domain_error(non_empty_list, []).
% 
% existence_error(ObjectType, Culprit)
%     ObjectType in { procedure, source_sink, stream } 
%     existence_error(procedure, ex_nihilo/0).
% 
% permission_error(Operation, PermissionType, Culprit)
%     Operation in { access, create, input, modify, open, output, reposition },
%     PermissionType in { binary_stream, flag, operator, past_end_of_stream, private_procedure, static_procedure, source_sink, stream, text_stream } 
%     permission_error(open, source_sink, '/etc/shadow')
% 
% representation_error(Flag)
%     Flag in { character, character_code, in_character_code, max_arity, max_integer, min_integer } 
% 
% evaluation_error(Error)
%     Error in { float_overflow, int_overflow, undefined, underflow, zero_divisor } 
% 
% resource_error(Resource)
%     resource_error(stack).
% 
% syntax_error(Imp_dep_atom)
%     syntax_error(operator_expected)>
% 
% system_error 
% 
% nnn_error(Culprit)
% 
% 
% TODO:
%	- make the mapping from eclipse error to iso error class more flexible
%	- deal with complex types (such as predicate_indicatory) which
%	  are defined via component types (atom/integer)
%	- complete everything
%

:- module(iso_error, [], [iso]).

:- local op(650, xfx, @).
:- local op(1100, xfy, do).


:- set_event_handler(230, uncaught_handler/2).
uncaught_handler(_, exit_block(iso_ball_thrown)) :-
	getval(ball, Ball)@iso,
	setval(ball, _)@iso,
	write_term(error, Ball, [quoted(true)]),
	nl(error),
	abort.
uncaught_handler(N, Goal) :-
	error(default(N), Goal).


throw_error(Exception, Culprit) :-
	functor(Culprit, F, N),
	ImpDefTerm = F/N,
	throw(error(Exception, ImpDefTerm)).


%----------------------------------------------------------------------
% instantiation_error
%----------------------------------------------------------------------

:- set_event_handler(4, iso_instantiation_error_handler_4/2).
iso_instantiation_error_handler_4(4, Culprit_) :-
	real_culprit(Culprit_, Culprit),
	throw_error(instantiation_error, Culprit).


%----------------------------------------------------------------------
% type_error - fairly complete, except 'variable' type
% Predicate table see below
%----------------------------------------------------------------------

:- set_event_handler(5, iso_type_error_handler/2).
:- set_event_handler(24, iso_type_error_handler/2).
iso_type_error_handler(E, Culprit_) :-
	real_culprit(Culprit_, Culprit),
	( type_check(Culprit, Exception) ->
	    throw_error(Exception, Culprit)
	;
	    error(default(E), Culprit)
	).

type_check(Culprit, type_error(Expected, Actual)) :-
	Culprit = functor(_,_,C), integer(C), C>0, !,
	type_check_arg(1, 3, functor(nonvar,atom,integer), Culprit, Expected, Actual).
type_check(Culprit, Exception) :-
	Culprit = (_ =.. List), !,
	( List == [] -> Exception = domain_error(non_empty_list,List)
	; is_list(List) ->
	    ( List = [H] -> Exception = type_error(atomic,H)
	    ; List = [H|_] -> Exception = type_error(atom,H)
	    )
	; Exception = type_error(list,List)
	).
type_check(Culprit, type_error(Expected, Actual)) :-
	functor(Culprit, F, N),
	functor(Type, F, N),
	type(Type),	% fail if we have no type spec
	type_check_arg(1, N, Type, Culprit, Expected, Actual).

    type_check_arg(I, N, Type, Culprit, Expected, Actual) :-
    	I =< N,
	arg(I, Type, ArgType),
	arg(I, Culprit, ArgVal),
	( (var(ArgVal) ; is_of_type(ArgVal, ArgType)) ->
	    I1 is I+1,
	    type_check_arg(I1, N, Type, Culprit, Expected, Actual)
	;
	    Expected = ArgType, Actual=ArgVal
	).


%----------------------------------------------------------------------
% domain_error - fairly complete
% Predicate table see below
%----------------------------------------------------------------------

:- set_event_handler(6, iso_domain_error_handler/2).
iso_domain_error_handler(_, arg(0,_,_)) :- !, fail.
iso_domain_error_handler(E, Culprit) :-
	( domain_check(Culprit, Exception) ->
	    throw_error(Exception, Culprit)
	;
	    error(default(E), Culprit)
	).

:- mode domain_check(+,-).
domain_check(get_flag(F,_), domain_error(prolog_flag, F)) :- !.
domain_check(set_flag(F,V), domain_error(flag_value, F+V)) :- !. % approximate
domain_check(functor(_,_,C), representation_error(max_arity)) :-
	C >= 0, !.	% if error 6 is raised, must be max-arity
domain_check(Culprit, domain_error(Expected, Actual)) :-
	functor(Culprit, F, N),
	functor(Type, F, N),
	domain(Type),	% fail if we have no domain spec
	domain_check_arg(1, N, Type, Culprit, Expected, Actual).

    domain_check_arg(I, N, Type, Culprit, Expected, Actual) :-
    	I =< N,
	arg(I, Type, ArgType),
	arg(I, Culprit, ArgVal),
	( (var(ArgVal) ; is_of_domain(ArgVal, ArgType)) ->
	    I1 is I+1,
	    domain_check_arg(I1, N, Type, Culprit, Expected, Actual)
	;
	    Expected = ArgType, Actual=ArgVal
	).


%----------------------------------------------------------------------
% representation_error
%----------------------------------------------------------------------


%----------------------------------------------------------------------
% evaluation_error - only 'undefined' exists in ECLiPSe
%----------------------------------------------------------------------

:- set_event_handler(20, iso_evaluation_error_handler/2).
iso_evaluation_error_handler(_, Culprit) :-
	% pass the full Culprit here for more detail
	throw(error(evaluation_error(undefined), Culprit)).


%----------------------------------------------------------------------
% Type table
% Should have entries for everything where ECLiPSe raises 5 (type error)
%----------------------------------------------------------------------

% TODO: complete
type(functor(nonvar, atomic, integer)).
type(arg(integer, compound, term)).
type(nonvar =.. list).
type(copy_term(term,term)).
type(term is evaluable).
type(op(integer,atom,atom_or_atom_list)).
type(clause(callable,callable)).
type(current_predicate(predicate_indicator)).
type(asserta(callable)).
type(assertz(callable)).
type(retract(callable)).
type(abolish(predicate_indicator)).
type(findall(term,callable,list)).
type(bagof(term,callable,list)).
type(setof(term,callable,list)).
type(current_input(stream)).
type(current_output(stream)).
type(set_input(stream_or_alias)).
type(set_output(stream_or_alias)).
type(open(source_sink,io_mode,stream,stream_options)).
type(open(source_sink,io_mode,stream)).
type(close(stream_or_alias)).
type(close(stream_or_alias,close_options)).
type(flush_output(stream_or_alias)).
type(stream_property(stream_or_alias,stream_property)).
type(set_stream_position(stream_or_alias,stream_position)).
type(get_byte(in_byte)).
type(get_byte(stream_or_alias,in_byte)).
type(get_char(in_character)).
type(get_char(stream_or_alias,in_character)).
type(get_code(in_character_code)).
type(get_code(stream_or_alias,in_character_code)).
type(peek_byte(in_byte)).
type(peek_byte(stream_or_alias,in_byte)).
type(peek_char(in_character)).
type(peek_char(stream_or_alias,in_character)).
type(peek_code(in_character_code)).
type(peek_code(stream_or_alias,in_character_code)).
type(put_byte(byte)).
type(put_byte(stream_or_alias,byte)).
type(put_char(character)).
type(put_char(stream_or_alias,character)).
type(put_code(character_code)).
type(put_code(stream_or_alias,character_code)).
type(nl(stream_or_alias)).
type(read_term(term,read_options_list)).
type(read_term(stream_or_alias,term,read_options_list)).
type(read(term)).
type(read(stream_or_alias,term)).
type(write_term(term,write_options_list)).
type(write_term(stream_or_alias,term,write_options_list)).
type(write(term)).
type(write(stream_or_alias,term)).
type(writeq(term)).
type(writeq(stream_or_alias,term)).
type(write_canonical(stream_or_alias,term)).
type(write_canonical(term)).
type(op(operator_priority,operator_specifier,atom_or_atom_list)).
type(current_op(operator_priority,operator_specifier,atom)).
type(char_conversion(character,character)).
type(current_char_conversion(character,character)).
type(\+(callable)).
type(once(callable)).
type(atom_length(atom,integer)).
type(atom_concat(atom,atom,atom)).
type(sub_atom(atom,integer,integer,integer,atom)).
type(atom_chars(atom,character_code_list)).
type(atom_codes(atom,character_code_list)).
type(char_code(character,character_code)).
type(number_chars(number,character_list)).
type(number_codes(number,character_code_list)).
type(set_prolog_flag(atom,nonvar)).
type(current_prolog_flag(atom,term)).
type(halt(integer)).


:- mode is_of_type(+,++).

is_of_type(_X, term)	:- !.
is_of_type(X, nonvar) :- !, nonvar(X).
is_of_type(X, list)	:- !, is_list(X).
is_of_type(X, atomic)	:- !, atomic(X).
is_of_type(X, number)	:- !, number(X).
is_of_type(X, evaluable) :- !, once (number(X);callable(X)).
is_of_type(F/N, predicate_indicator) ?- !, atom(F), integer(N).
is_of_type(X, callable) :- !, callable(X).
is_of_type(X, byte) :- !, 0=<X, X=<255.
is_of_type(X, character) :- !, char(X).
is_of_type(X, character_code) :- !, char_code(X).
is_of_type(X, in_byte) :- !, -1=<X, X=<255.
is_of_type(X, in_character) :- !, once (char(X);X==end_of_file).
is_of_type(X, in_character_code) :- !, once (char_code(X); -1==X).
is_of_type(X, clause) :- !, callable(X).
is_of_type(X, close_options) :- !, close_options(X).
%is_of_type(_DUBIOUS, flag) :- !.
is_of_type(X, head) :- !, callable(X).
is_of_type(X, atom_or_atom_list) ?- (atom(X) ; X\==[], atom_list(X)), !.
is_of_type(X, Type)	:- type_of(X, Type).


%----------------------------------------------------------------------
% Domain table
% Should have entries for everything where ECLiPSe raises 6 (range error)
%----------------------------------------------------------------------

% TODO: complete
domain(functor(any,any,not_less_than_zero)).
domain(arg(not_less_than_zero,any,any)).
domain(op(operator_priority,operator_specifier,atom_or_atom_list)).
domain(current_prolog_flag(flag,any)).
domain(set_prolog_flag(flag,any)).



% domains -- can assume the type is checked already
:- mode is_of_type(+,++).

is_of_domain(X, character_code_list) ?- char_code_list(X), !.

is_of_domain(X, close_option) ?- close_option(X), !.

%is_of_domain(X, flag_value) ?- !. % done in domain_check/2

is_of_domain(read, io_mode) ?- !.
is_of_domain(write, io_mode) ?- !.
is_of_domain(append, io_mode) ?- !.

is_of_domain(X, non_empty_list) ?- !, X\==[].

is_of_domain(X, not_less_than_zero) ?- !, X >= 0.

is_of_domain(X, operator_priority) ?- !, 0 =< X, X =< 1200.

is_of_domain(fx, operator_specifier) ?- !.
is_of_domain(fy, operator_specifier) ?- !.
is_of_domain(xfx, operator_specifier) ?- !.
is_of_domain(xfy, operator_specifier) ?- !.
is_of_domain(yfx, operator_specifier) ?- !.
is_of_domain(xf, operator_specifier) ?- !.
is_of_domain(yf, operator_specifier) ?- !.

%is_of_domain(X, prolog_flag) ?- !. % done in domain_check/2

is_of_domain(variables(_), read_option) ?- !.
is_of_domain(variable_names(_), read_option) ?- !.
is_of_domain(singletons(_), read_option) ?- !.

is_of_domain(X, source_sink) ?- source_sink(X), !.

is_of_domain(X, stream) ?- atomic(X), !.

is_of_domain(type(text), stream_option) ?- !.
is_of_domain(type(binary), stream_option) ?- !.
is_of_domain(reposition(B), stream_option) ?- bool(B), !.
is_of_domain(alias(A), stream_option) ?- is_of_domain(A, stream_or_alias), !.
is_of_domain(eof_action(error), stream_option) ?- !.
is_of_domain(eof_action(eof_code), stream_option) ?- !.
is_of_domain(eof_action(reset), stream_option) ?- !.
is_of_domain(eof_action(reset), stream_option) ?- !.

is_of_domain(X, stream_or_alias) ?- atomic(X), !.
is_of_domain(X, stream_position) ?- integer(X), !.

is_of_domain(X, stream_property) ?- is_of_domain(X, stream_option), !.
is_of_domain(file_name(_), stream_property) ?- !.
is_of_domain(mode(M), stream_property) ?- is_of_domain(M, io_mode), !.
is_of_domain(input, stream_property) ?- !.
is_of_domain(output, stream_property) ?- !.
is_of_domain(position(P), stream_property) ?- is_of_domain(P, stream_position), !.

is_of_domain(quoted(B), write_option) ?- bool(B), !.
is_of_domain(ignore_ops(B), write_option) ?- bool(B), !.
is_of_domain(numbervars(B), write_option) ?- bool(B), !.

is_of_domain(_, any).


bool(true) ?- true.
bool(false) ?- true.

atom_list([]) ?- true.
atom_list([X|Xs]) ?- atom(X), atom_list(Xs).

char(X) :- atom(X), atom_length(X, 1).

char_list([]) ?- true.
char_list([X|Xs]) ?- char(X), char_list(Xs).

char_code(X) :- integer(X), 0=<X, X=<2147483647.

char_code_list([]) ?- true.
char_code_list([X|Xs]) ?- char_code(X), char_code_list(Xs).

close_option(force(B)) ?- bool(B).

close_options([]) ?- true.
close_options([X|Xs]) ?- close_option(X), close_options(Xs).

source_sink(X) ?- atom(X).
source_sink(X) ?- string(X).
source_sink(string(S)) ?- string(S).
source_sink(queue(S)) ?- string(S).
source_sink(fd(I)) ?- integer(I).


%----------------------------------------------------------------------
% Cases where the ECLiPSe culprit doesn't match the expected one
%----------------------------------------------------------------------

:- mode real_culprit(+,-).
real_culprit(In, Out) :-
	real_culprit1(In, Out), !.
real_culprit(In, In).

:- mode real_culprit1(+,-).
real_culprit1(>(A,B,_), >(A,B)).
real_culprit1(<(A,B,_), <(A,B)).
real_culprit1(>=(A,B,_), >=(A,B)).
real_culprit1(=<(A,B,_), =<(A,B)).
real_culprit1(=:=(A,B,_), =:=(A,B)).
real_culprit1(=\=(A,B,_), =\=(A,B)).
real_culprit1(get_stream(input,S), current_input(S)).
real_culprit1(get_stream(output,S), current_output(S)).
real_culprit1(set_stream(input,S), set_input(S)).
real_culprit1(set_stream(output,S), set_output(S)).
real_culprit1(get_flag(F,V), current_prolog_flag(F,V)).
real_culprit1(set_flag(F,V), set_prolog_flag(F,V)).
