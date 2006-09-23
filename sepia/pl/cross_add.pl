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
% Version:	$Id: cross_add.pl,v 1.1 2006/09/23 01:55:10 snovello Exp $
% ----------------------------------------------------------------------

% % SEPIA DEMO PROGRAM
%
% IDENTIFICATION:       cross_add.pl
%
% AUTHOR:               Joachim Schimpf
%
% CONTENTS:             cross_add/0
%			close_cross_add/0
%
% DESCRIPTION:
%
%	This program solves a crossword puzzle with given arithmetic
%	conditions. The grid has to be filled with the digits 1..9,
%	no digit must occur twice in a word, and the sum of all digits
%	in a word is specified.
%

:- module(cross_add).

:- export cross_add/0, close_cross_add/0.
:- export top/0, top/2.

:- use_module(library(fd)).


:- import get_flag_body/4 from sepia_kernel.
:-  make_local_array(flags),
    get_flag(debug_compile, DC),
    get_flag(variable_names, VN),
    setval(flags, flags(DC, VN)),
    nodbgcomp.

%-------------------------------
% crossword specification
%-------------------------------

x_max(12).		% 0 .. x_max	size of the grid
y_max(12).		% 0 .. y_max

grid_has_holes.

digit(9).
digit(8).
digit(7).
digit(6).
digit(5).
digit(4).
digit(3).
digit(2).
digit(1).

position_constraints(HWords, VWords, Show) :-

	HWords = [
	     9 - [A1, A2, A3],		% Sum - List of fields
	     3 - [A7, A8],
	    17 - [A10, A11],
	    15 - [B0, B1, B2, B3, B4],
	    13 - [B6, B7, B8],
	    24 - [B10, B11, B12],
	    15 - [C0, C1],
	    16 - [C3, C4],
	     5 - [C6, C7],
	    13 - [C11, C12],
	    17 - [D3, D4],
	    31 - [D6, D7, D8, D9, D10, D11, D12],
	    13 - [E0, E1],
	    45 - [E3, E4, E5, E6, E7, E8, E9, E10, E11],
	    31 - [F0, F1, F2, F3, F4, F5],
	    12 - [F7, F8],
	    30 - [G1, G2, G3, G4],
	    10 - [G8, G9, G10, G11],
	     6 - [H4, H5],
	    39 - [H7, H8, H9, H10, H11, H12],
	    45 - [I1, I2, I3, I4, I5, I6, I7, I8, I9],
	     9 - [I11, I12],
	    28 - [J0, J1, J2, J3, J4, J5, J6],
	     3 - [J8, J9],
	     9 - [K0, K1],
	    16 - [K5, K6],
	    16 - [K8, K9],
	     8 - [K11, K12],
	    21 - [L0, L1, L2],
	    12 - [L4, L5, L6],
	    15 - [L8, L9, L10, L11, L12],
	    14 - [M1, M2],
	    12 - [M4, M5],
	    24 - [M9, M10, M11]
	],

	VWords = [
	    10 - [B0, C0],
	     6 - [E0, F0],
	    12 - [J0, K0, L0],
	    12 - [A1, B1, C1],
	    24 - [E1, F1, G1],
	    28 - [I1, J1, K1, L1, M1],
	     3 - [A2, B2],
	    14 - [F2, G2],
	    13 - [I2, J2],
	     9 - [L2, M2],
	    41 - [A3, B3, C3, D3, E3, F3, G3],
	    16 - [I3, J3],
	    45 - [B4, C4, D4, E4, F4, G4, H4, I4, J4],
	    11 - [L4, M4],
	    17 - [E5, F5],
	    22 - [H5, I5, J5, K5, L5, M5],
	    10 - [B6, C6, D6, E6],
	    30 - [I6, J6, K6, L6],
	    21 - [A7, B7, C7, D7, E7, F7],
	    14 - [H7, I7],
	    11 - [A8, B8],
	     9 - [D9, E9],
	    45 - [D8, E8, F8, G8, H8, I8, J8, K8, L8],
	    30 - [G9, H9, I9, J9, K9, L9, M9],
	    16 - [A10, B10],
	    11 - [D10, E10],
	     8 - [G10, H10],
	    13 - [L10, M10],
	    35 - [A11, B11, C11, D11, E11],
	     7 - [G11, H11, I11],
	    10 - [K11, L11, M11],
	    20 - [B12, C12, D12],
	    14 - [H12, I12],
	     9 - [K12, L12]
	],

	( Show == show ->
	    init_window(HWords, VWords),
	    show_row(0,[1,2,3,7,8,10,11],[A1,A2,A3,A7,A8,A10,A11]),
	    show_row(1,[0,1,2,3,4,6,7,8,10,11,12],
		    [B0,B1,B2,B3,B4,B6,B7,B8,B10,B11,B12]),
	    show_row(2,[0,1,3,4,6,7,11,12],[C0,C1,C3,C4,C6,C7,C11,C12]),
	    show_row(3,[3,4,6,7,8,9,10,11,12],[D3,D4,D6,D7,D8,D9,D10,D11,D12]),
	    show_row(4,[0,1,3,4,5,6,7,8,9,10,11],
		    [E0,E1,E3,E4,E5,E6,E7,E8,E9,E10,E11]),
	    show_row(5,[0,1,2,3,4,5,7,8],[F0,F1,F2,F3,F4,F5,F7,F8]),
	    show_row(6,[1,2,3,4,8,9,10,11],[G1,G2,G3,G4,G8,G9,G10,G11]),
	    show_row(7,[4,5,7,8,9,10,11,12],[H4,H5,H7,H8,H9,H10,H11,H12]),
	    show_row(8,[1,2,3,4,5,6,7,8,9,11,12],
		    [I1,I2,I3,I4,I5,I6,I7,I8,I9,I11,I12]),
	    show_row(9,[0,1,2,3,4,5,6,8,9],[J0,J1,J2,J3,J4,J5,J6,J8,J9]),
	    show_row(10,[0,1,5,6,8,9,11,12],[K0,K1,K5,K6,K8,K9,K11,K12]),
	    show_row(11,[0,1,2,4,5,6,8,9,10,11,12],
		    [L0,L1,L2,L4,L5,L6,L8,L9,L10,L11,L12]),
	    show_row(12,[1,2,4,5,9,10,11],[M1,M2,M4,M5,M9,M10,M11])
	;
	    true
	).


%-------------------------------
% run the puzzle
%-------------------------------

top :-
	top(show, first).

top(Show, All) :-
	    cputime(T0),
	position_constraints(HWords, VWords, Show),
	    append(HWords, VWords, Words),
	needed_constraints(Words),
	    cputime(T1),
	(
	    generate(HWords, VWords),
	    Tsolve is cputime-T1,
	    writeln("Tsolve"=Tsolve),
	    (All == all -> fail ; !)
	;
	    Tproof is cputime-T1,
	    writeln("Tproof"=Tproof)
	).


%-------------------------------
% setup the arithmetic constraints
%-------------------------------

needed_constraints([]).
needed_constraints([Result-Word|Words]) :-
	Word :: 1..9,
	alldistinct(Word),
	list_sum(Word, Sum),
	Result #= Sum,
	needed_constraints(Words).

list_sum([], 0).
list_sum([X|Xs], S) :-
	list_sum(Xs, X, S).

list_sum([], X, X).
list_sum([X|Xs], S0, S) :-
	list_sum(Xs, S0+X, S).


%-------------------------------
% value generation
%-------------------------------

% interleaved generation of horizontal and vertical words

generate([], []) :- !.
generate([], [_-Word|Words]) :-
	!,
	generate_digits(Word),
	generate([], Words).
generate([_-Word|Words], []) :-
	!,
	generate_digits(Word),
	generate(Words, []).
generate([_-HWord|HWords], [_-VWord|VWords]) :-
	generate_digits(HWord),
	generate_digits(VWord),
	generate(HWords, VWords).

generate_digits([]).
generate_digits([H|T]) :-
%	indomain(H),
	digit(H),		% better than indomain/1
	generate_digits(T).

% generate_digits([]) :- !.
% generate_digits(List) :-
%	deleteff(Var, List, Rest),
% %	indomain(Var),
%	digit(Var),
%	generate_digits(Rest).

%-------------------------------
% graphic display
%-------------------------------

window_data(600,100,500,500).

map_coordinates(X, Y, Xd, Yd) :-
	Xd is 2*X + 1,
	Yd is 2*(y_max - Y) + 1.

show_row(Y,Xs,Vs) :-			% setup delayed variable printers
	show_row(Y,-1,Xs,Vs).

show_row(Y,X0,[],[]) :-
	!,
	X is x_max + 1,
	fill_holes(Y,X0,X).
show_row(Y,X0,[X|Xs],[V|Vs]):-
	fill_holes(Y,X0,X),
	map_coordinates(X, Y, Xd, Yd),
	show(Xd,Yd,V),
	show_row(Y,X,Xs,Vs).

fill_holes(Y,X0,X) :-			% make the holes black
	X1 is X0+1,
	( X1 =:= X ->
	    true
	;
	    Xd0 is 2*X1,
	    Xd1 is 2*X,
	    Yd0 is 2*(y_max - Y),
	    Yd1 is Yd0 + 2,
	    interior_style(1,0),
	    fill_color(1),
	    rectangle(Xd0, Yd0, Xd1, Yd1)
	).

delay show(_, _, V) if var(V).

show(X,Y,V):-
	text_color(1),
	text(X,Y,V).
show(X,Y,V):-
	text_color(0),
	text(X,Y,V),
	fail.


init_window(HWords, VWords) :-		% display the grid
	Xmax is 2 * x_max + 2,
	Ymax is 2 * y_max + 2,
	Xframe is Xmax + 1,
	Yframe is Ymax + 1,
	(open_2d(-1, -1, Xframe, Yframe) -> true ; clear_view_surface(0)),
        set_colors,
	line_width(1),
	grid_hor(0, Xmax, 0, Ymax, 2),
	grid_ver(0, Xmax, 0, Ymax, 2),
	line_width(3),
	line(0, 0, Xmax, 0),
	line(0, 0, 0, Ymax),
	( grid_has_holes ->
	    line(Xmax, 0, Xmax, Ymax),
	    line(0, Ymax, Xmax, Ymax)
	;
	    mark_hor(HWords, 0, 0),
	    mark_ver(VWords, 0, 0)
	).

grid_hor(X, Xmax, Y, Ymax, Inc) :-
	Y < Ymax ->
	    line(X, Y, Xmax, Y),
	    Y1 is Y + Inc,
	    grid_hor(X, Xmax, Y1, Ymax, Inc)
	;
	    true.

grid_ver(X, Xmax, Y, Ymax, Inc) :-
	X < Xmax ->
	    line(X, Y, X, Ymax),
	    X1 is X + Inc,
	    grid_ver(X1, Xmax, Y, Ymax, Inc)
	;
	    true.

mark_hor([], _, _).
mark_hor([_-Word|Words], Count0, Xoff) :-
	X0 is 2*(Count0 mod 7) + Xoff,
	Y0 is 2*(7 - Count0 // 7) - 1,
	Y1 is Y0 + 2,
	line(X0, Y0, X0, Y1),
	Count1 is Count0 + length(Word),
	mark_hor(Words, Count1, Xoff).

mark_ver([], _, _).
mark_ver([_-Word|Words], Count0, Xoff) :-
	Y0 is 2*(7 - Count0 mod 7) + 1,
	X0 is 2*(Count0 // 7) + Xoff,
	X1 is X0 + 2,
	line(X0, Y0, X1, Y0),
	Count1 is Count0 + length(Word),
	mark_ver(Words, Count1, Xoff).

display_time(X, Y, Time) :-
	open(_, string, S),
	printf(S, "%.2f s", Time),
	current_stream(Buf, _, S),
	text(X, Y, Buf).

color(Number, Name) :-
        call(colour(Name, R, G, B), kegi),
        color_rgb(Number, R, G, B).
 
set_colors :-
        (getenv("COLOUR", _) ->
            color(0, white),
            color(1, black),
            color(2, red),
            color(3, cyan),
            color(4, green)
        ;
            true
        ).

%-------------------------------
% demo goals
%-------------------------------

cross_add :-
        global(window_data/4),
        top.

close_cross_add :-
        (get_flag_body(window_data/4, definition_module, cross_add, kegi) ->
            local(window_data/4),
            close_2d
        ;
            true
        ).

:-  getval(flags, flags(DC, VN)),
    set_flag(debug_compile, DC),
    set_flag(variable_names, VN),
    erase_array(flags).

