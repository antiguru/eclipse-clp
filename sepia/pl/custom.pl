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
% Copyright (C) 1990-2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): ECRC GmbH
% Contributor(s): IC-Parc, Imperal College London
% 
% END LICENSE BLOCK
%
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: custom.pl,v 1.1 2006/09/23 01:55:10 snovello Exp $
% ----------------------------------------------------------------------

/*
 * SEPIA PROLOG SOURCE MODULE
 *
 * IDENTIFICATION:	custom.pl 
 *
 * DESCRIPTION: 	Various utility predicates, for user convenience.
 *
 * CONTENTS:
 *
 *	This file contains the source code for some examples from the paper
 *
 *	"A Guide to SEPIA Customisation and Advanced Programming"
 *	(ECRC TR-LP-50)
 *
 *	Other examples from this paper can be found in the library files
 *	undef.pl, make.pl, module_autoload.pl, answer.pl, quintus_util.pl
 *	non_consecutive.pl
 */


%
% Predicate for the '-e pred' command line option
%

start :-
    argc(N),
    find_argument(1, N, Arg),
    % next line is like open(Arg, string, S), read(S, Term), close(S)
    term_string(Term, Arg),
    Term.

% Find the -e argument and return what follows after the entry
% If not found, return true/0
find_argument(N, N, "true") :- !.
find_argument(I, N, Arg) :-
    argv(I, "-e"),
    I2 is I + 2,
    I2 < N,
    !,
    argv(I2, Arg).
find_argument(I, N, Arg) :-
    I1 is I + 1,
    find_argument(I1, N, Arg).


:- module_interface(conditional_spy_point).
:- export
	(if)/2,
	start_cond_spy/0,
	end_cond_spy/0.
:- begin_module(conditional_spy_point).
:- import spy_body/2 from sepia_kernel.
:- tool((if)/2, if_body/3).
:- dynamic cond_spy/1.

% Define the primitive to set a conditional spy point.
if_body(spy(Goal), Condition, Module) :-
    functor(Goal, F, A),
    spy_body(F/A, Module),
    debug_macro(0'L, "@call_explicit(start_cond_spy, conditional_spy_point). @call_explicit(end_cond_spy, conditional_spy_point)"),
    asserta((cond_spy(Goal) :- Condition)).

% Start the conditional spying. Set up a pipe, change the debugger prompt
% so that the term can be read in and change the io handler.
start_cond_spy :-
    set_stream(di, debug_input),
    set_stream(do, debug_output),
    open("cl", string, leap),
    set_stream(debug_input, leap),
    pipe(sigio(in), out),
    set_stream(debug_output, out),
    set_prompt(debug_input, " .\n", debug_output),
    set_interrupt_handler(io, read_debug_output/0),
    set_error_handler(154, end_cond_spy/0).

% At the end, close the pipe
end_cond_spy :-
    get_interrupt_handler(io, H, M),
    set_interrupt_handler(io, true/0),
    reset_error_handler(154),
    close(out),
    close(in),
    call(set_interrupt_handler(io, H), M),
    nl(debug_output).

% This procedure handles the signal io. If there is 'leap' in the pipe
% (the echo of the 'l' option), it reads a line and ignores it.
% Otherwise there is a trace line in the pipe, it is read and inspected.
read_debug_output :-
    get(in, Type),
    (Type == 0'l ->
        read_string(in, "\n", _, _)
    ;
        get(in, _Spy),
        read_trace_line
    ).

read_trace_line :-
    get(in, _),        % (
    read_token(in, _Invoc, _),
    get(in, _),        % )
    read_token(in, _Depth, _),
    get(in, _),             % ' '
    get(in, _),             % *
    read_string(in, " ", _, _Port),
    read(in, Goal),
    (cond_spy(Goal) ->
                % Reset the stream and handlers
        set_stream(debug_output, do),
        set_stream(debug_input, di)
    ;
        seek(debug_input, 1)
    ).


%
% To repeat the previous query if the user types '!!'
%

:- module_interface(query).
:- op(1, fy, !).			% to be able to type !!
:- begin_module(query).
:- make_local_array(last, prolog).
:- setval(last, goal(true, [])).      % initialise the last query

last_query(_, goal(!!, [], Goal, VL)) :-
    !,
    getval(last, goal(Goal, VL)).
last_query(_, goal(Goal, VL, _, _)) :-
    setval(last, goal(Goal, VL)).

:- set_error_handler(154, last_query/2).


%
% An import link is automatically abolished when a new visibility
% is declared
%

:- module_interface(handlers).
:- begin_module(handlers).

:- import
        abolish_body/2,
        import_body/2,
        local_body/2,
        global_body/2,
        export_body/2
from sepia_kernel.

import_handler(_, (import Pred from Module), Caller_mod) :-
        !,
        abolish_body(Pred, Caller_mod),
        import_body((Pred from Module), Caller_mod).
import_handler(_, local(Pred), Module) :-
        !,
        abolish_body(Pred, Module),
        local_body(Pred, Module).
import_handler(_, export(Pred), Module) :-
        !,
        abolish_body(Pred, Module),
        export_body(Pred, Module).
import_handler(_, global(Pred), Module) :-
        !,
        abolish_body(Pred, Module),
        global_body(Pred, Module).

import_handler(N , Culprit, Module) :-
        error(default(N), Culprit, Module).

:- set_error_handler(94, import_handler/3).
