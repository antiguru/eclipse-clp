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
% The Original Code is  The ECLiPSe Constraint Logic Programming System. 
% The Initial Developer of the Original Code is  Cisco Systems, Inc. 
% Portions created by the Initial Developer are
% Copyright (C) 1989-2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): ECRC GmbH
% 
% END LICENSE BLOCK
%
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: ifdef.pl,v 1.1 2006/09/23 01:55:20 snovello Exp $
% ----------------------------------------------------------------------

/*
 * SEPIA PROLOG SOURCE MODULE
 */

/*
 * IDENTIFICATION:	ifdef.pl
 *
 * DESCRIPTION: 	This is a package that allows conditional compilation.
 *			The syntax is very similar to C:
 *
 *			:- ifdef Goal.
 *			...
 *			:- elseif Goal.
 *			...
 *			:- else.	% or :- else(Goal).
 *			...
 *			:- endif.	% or :- endif(Goal).
 *
 *			The conditionals can be nested.
 *
 */

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(ifdef).

:- export (ifdef)/1.
:- export op(1100, fy, [ifdef, elseif]).

:- tool((ifdef)/1, ifdef_body/2).

ifdef_body(Cond, Module) :-
	compiled_stream(S),
	(call(Cond, Module) ->
		then_part(S, Module)
	;
		else_part(S, Module)
	).

then_part(S, Module) :-
	read_to_next(S, List, End, yes, Module),
	expand_clauses(List, Module, XList),
	compile_term(XList)@Module,
	skip_to_endif(S, End, Module).

else_part(S, Module) :-
	read_to_next(S, _, End, no, Module),
	(End = elseif(Cond) ->
	    (call(Cond, Module) ->
		then_part(S, Module)
	    ;
		else_part(S, Module)
	    )
	;
	End \== endif ->
	    read_to_next(S, List, _, yes, Module),
	    expand_clauses(List, Module, XList),
	    compile_term(XList)@Module
	;
	    true
	).

skip_to_endif(_, endif, _) :- !.
skip_to_endif(S, _, Module) :-
	read_to_next(S, _, End, no, Module),
	skip_to_endif(S, End, Module).

read_to_next(S, List, End, True, Module) :-
    read(S, Term)@Module,
    (Term = (:- Goal) ->
	next_goal(S, Goal, List, End, True, Module)
    ;
    Term == end_of_file ->
	ifdef_error(S)
    ;
	List = [Term|Next],
	read_to_next(S, Next, End, True, Module)
    ).

next_goal(_, elseif(Cond), [], elseif(Cond), _, _) :- !.
next_goal(_, else, [], else, _, _) :- !.
next_goal(_, else(_), [], else, _, _) :- !.
next_goal(_, endif, [], endif, _, _) :- !.
next_goal(_, endif(_), [], endif, _, _) :- !.
next_goal(S, Goal, List, End, True, Module) :-
    (True == yes -> call(Goal, Module); true),
    read_to_next(S, List, End, True, Module).

ifdef_error(Stream) :-
    (
	get_stream_info(Stream, name, File),
        get_stream_info(Stream, line, Line)
    ->
	true
    ;
	File = ??,
	Line = 0
    ),
    printf(error,
    	"%n*** ifdef without a matching endif before line %w in the file %s%n",
	[Line, File]),
    abort.


expand_clauses([], _Module, []).
expand_clauses([Clause|Clauses], Module, [XClause|XClauses]) :-
    expand_clause(Clause, Module, XClause),
    expand_clauses(Clauses, Module, XClauses).

expand_clause((H:-B), Module, Clause) ?- !,
	expand_goal(B, BX)@Module,
	Clause = (H:-BX).
expand_clause((H?-B), Module, Clause) ?- !,
	expand_goal(B, BX)@Module,
	Clause = (H?-BX).
expand_clause(Clause, _Module, Clause).
