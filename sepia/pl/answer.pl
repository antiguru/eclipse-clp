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
% Copyright (C) 1995-2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): ECRC GmbH
% 
% END LICENSE BLOCK
%
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: answer.pl,v 1.1 2006/09/23 01:55:06 snovello Exp $
% ----------------------------------------------------------------------

/*
 * SEPIA PROLOG SOURCE MODULE
 *
 * IDENTIFICATION:	answer.pl 
 *
 * DESCRIPTION: 	Utility predicates, for user convenience.
 *
 * CONTENTS:
 *
 */


:- module(answer).

:- export print_answer/3, answer/2.

% Print the answer binding and report delayed goals.
print_answer(_, List, M) :-
    bind_vars(List, 0, Length),
    print_values(List, Length),
    delayed_goals(DL),
    (DL == [] ->
        true
    ;
        error(273, DL, M)       % report the delayed goals
    ).

% Bind each free variable to a term containing its name
% and find out the length of the longest name of a bound variable.
bind_vars([], Length, Length).
bind_vars([[Name|Value]|Rest], SoFar, Max) :-
    (var(Value) ->
        Value = '$VAR'(Name),
        L = SoFar
    ;
        % note the use of atom_length/2 as arithmetic functor
        L is max(atom_length(Name), SoFar)
    ),
    bind_vars(Rest, L, Max).

% Print the answer binding using portray/2 in printf/3.
print_values([], _).
print_values([[Name|Value]|Rest], L) :-
    (Value == '$VAR'(Name) ->
        true                                  % don't report X = X
    ;
        printf(answer_output, "%-*s = %p\n", [L, Name, Value])
    ),
    print_values(Rest, L).

% Print an input variable as its name.
portray(S, '$VAR'(Name)) :-
    atom(Name),
    write(S, Name).



answer(_, yes) :-
    ask_more.
answer(_, last_yes) :-
    write(toplevel_output, '\nyes.\n').
answer(_, no) :-
    write(toplevel_output, '\nno.\n').
answer(_, more_answers) :-
    ask_more.
answer(_, last_answer) :-
    write(toplevel_output, '\nyes.\n').
answer(_, no_answer) :-
    write(toplevel_output, '\nno.\n').



ask_more :-
    write(toplevel_output, '\tMore? '),
    flush(toplevel_output),
    tyi(toplevel_input, C),
    (C == 0'; ->
        nl(toplevel_output),
        fail
    ;
        true
    ).
