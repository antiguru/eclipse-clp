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
% Version:	$Id: cross.pl,v 1.1 2006/09/23 01:55:10 snovello Exp $
% ----------------------------------------------------------------------

% % SEPIA DEMO PROGRAM
%
% IDENTIFICATION:       cross.pl
%
% AUTHORS:              Micha Meier, Thierry Le Provost, Joachim Schimpf
%			and Emmanuel van Rossum.
%			The representation of the crossword is due to Thierry
%			(when you see it, it is obvious, but it was not
%			obvious to find it), the  graphics comes from a program
%			by Joachim.
%
% CONTENTS:             set_up_crossword/1
%
%
% DESCRIPTION:
%
% Reads the crossword data from the specified file and returns the
% horizontal and vertical words, where a word is a term w(Var1, Var2, ...)
% with Var(i) corresponding to the crossword fields. The crossword is displayed
% and a suspended goal is set up for every white field on it, which
% will display it as soon as it obtains a value. The format of the words
% and their displaying can be changed by changing the macro for word/1
% and the predicate show_var/3.
%
% After the return, the following predicates are available:
%
%	grid_size(Float)	- Specifies how big is the displayed crossword
%				  (must be defined in File).
%	layout(Layout)		- Layout is the list of lists which represent
%				  the crossword, black fields are represented
%				  by 0 (must be defined in File).
%	w/2, w/3, ..., w/Max	- Predicates that contain the ASCII codes
%				  of the input words. File must contain
%				  the definition of a predicate word/1
%				  that lists all the words as atoms.
%	max_length(Max)		- Max is the length of the longest word
%	x_max(X)		- X is the number of columns
%	y_max(Y)		- Y is the number of rows
%

:- coroutine.

:- dynamic
	max_length/1,
	x_max/1,
	y_max/1,
	window_data/4.

:- global window_data/4.

set_up_crossword(File, Gr, HWords, VWords) :-
	retract_all(max_length(_)),
	retract_all(x_max(_)),
	retract_all(y_max(_)),
	asserta(max_length(0)),
	compile(File),
	layout(Layout),
	length(Layout, RowsNo),
	Layout = [Row1|_],
	length(Row1, ColumnsNo),
	asserta(x_max(ColumnsNo)),
	asserta(y_max(RowsNo)),
	(Gr == yes -> display_grid; true),
	horizontal_words(Layout, HWords, []),	% displays the crossword
	vertical_words(Layout, VWords, []),
	(Gr == yes ->
	    show_rows(1, Layout)
	;
	    true
	).

destroy_crossword :-
	abolish(max_length/1),
	abolish(x_max/1),
	abolish(y_max/1),
	abolish(window_data/4),
	close_2d.

horizontal_words([], L, L).
horizontal_words([Row|Rows], HWords, Cont) :-
	collect_words(Row, HWords, Cont1),
	horizontal_words(Rows, Cont1, Cont).

vertical_words([[]|_], L, L) :- !.
vertical_words(Layout, VWords, Cont) :-
	get_column(Layout, Column, Rest),
	collect_words(Column, VWords, Cont1),
	vertical_words(Rest, Cont1, Cont).

get_column([], [], []) :- !.
get_column([[H|Row]|Next], [H|Col], [Row|Rest]) :-
	get_column(Next, Col, Rest).

collect_words([], L, L).
collect_words([H|T], Words, Cont) :-
	collect_word([H|T], LWord, [], Length, 0, Rest),
	(Length > 1 ->
	    functor(Word, w, Length),
	    max_length(Max),
	    (Max < Length ->
		retract(max_length(_)),
		asserta(max_length(Length))
	    ;
		true
	    ),
	    Word =.. [w|LWord],
	    Words = [Word|Cont1]
	;
	    Cont1 = Words
	),
	collect_words(Rest, Cont1, Cont).

collect_word([], W, W, L, L, []).
collect_word([H|T], [H|Next], Cont, Length, L0, Rest) :-
	var(H),
	!,
	L1 is L0 + 1,
	collect_word(T, Next, Cont, Length, L1, Rest).
collect_word([0|Rest], Cont, Cont, Length, Length, Rest).

show_rows(_, []).
show_rows(I, [Row|Rows]) :-
	show_row(1, I, Row),
	I1 is I + 1,
	show_rows(I1, Rows).

show_row(_, _, []).
show_row(X, Y, [Field|Rest]) :-
	show_field(X, Y, Field),
	X1 is X + 1,
	show_row(X1, Y, Rest).

show_field(X, Y, Var) :-
	var(Var),
	!,
	map_coordinates(X, Y, Xd, Yd),
	show(Xd, Yd, Var).
show_field(X, Y, 0) :-
	fill_color(1),
	interior_style(1,0),
	Xd0 is 10*(X - 1),
	Xd1 is Xd0 + 10,
	Yd0 is 10*(y_max - Y),
	Yd1 is Yd0 + 10,
	rectangle(Xd0, Yd0, Xd1, Yd1).

map_coordinates(X, Y, Xd, Yd) :-
	Yd is 10*(y_max - Y) + 2,
	Xd is 10*X - 8.

delay show(_, _, V) if var(V).
show(X,Y,V):-
	text_color(40),
	show_var(X, Y, V).
show(X,Y,_):-
	fill_color(0),
	interior_style(1, 0),
	Xd0 is X - 1,
	Yd0 is Y,
	Xd1 is Xd0 + 8,
	Yd1 is Yd0 + 8,
	rectangle(Xd0, Yd0, Xd1, Yd1),
	fail.

display_grid :-		% display the grid
	Xmax is 10 * x_max,
	Ymax is 10 * y_max,
	Xframe is Xmax + 1,
	Yframe is Ymax + 1,
	WX is fix(Xmax * grid_size),
	WY is fix(Ymax * grid_size),
	(retract(window_data(A, B, _, _)) ->
	    Xs = A, Ys = B
	;
	    Xs = 0, Ys = 0
	),
	asserta(window_data(Xs, Ys, WX, WY)),
	(open_2d(-1, -1, Xframe, Yframe) -> true ; clear_view_surface(0)),
	(get_flag(loaded_library, k_x11) ->
	    call(window_fact(_, C, _, _, _, _, _, _, _, _, _, _, _, _, _, _, Gfont, _), kegi),
	    Size is WY * 9 // Yframe,
	    concat_atom(['-linotype-times-bold-r-normal--', Size, '-*'], FName),
	    call(xOpenFont(C, FName, Font), kegi),
	    call((
		xSetGC(C, Gfont, [xFont(Font)]),
		xFlush(C)), kegi)
	    
	;
	    text_precision(1),
	    character_height(7)
	),
	line_width(2),
	grid_hor(0, Xmax, 0, Ymax, 10),
	grid_ver(0, Xmax, 0, Ymax, 10),
	line_width(3),
	line(0, 0, Xmax, 0),
	line(0, 0, 0, Ymax),
	line(Xmax, 0, Xmax, Ymax),
	line(0, Ymax, Xmax, Ymax).

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

% Represent letters as upper ASCII
/*
trans_word(no_macro_expansion(word(Word)), CharTerm) :-
	name(Word, List),
	lower_to_upper(List, UpList),
	CharTerm =.. [w|UpList].

lower_to_upper([], []).
lower_to_upper([Low|RestL], [Up|RestU]) :-
	Up is Low /\ 16'df,
	lower_to_upper(RestL, RestU).

show_var(X, Y, Var) :-
	char_int(Char, Var),
	text(X, Y, Char).
*/

% Represent letters as upper case atoms
trans_word(no_macro_expansion(word(Word)), CharTerm) :-
	name(Word, List),
	lower_to_upper(List, UpList),
	CharTerm =.. [w|UpList].

lower_to_upper([], []).
lower_to_upper([Low|RestL], [Up|RestU]) :-
	UpI is Low /\ 16'df,
	char_int(UpS, UpI),
	atom_string(Up, UpS),
	lower_to_upper(RestL, RestU).

show_var(X, Y, Var) :-
	text(X, Y, Var).

:- define_local_macro(word/1, trans_word/2, []).

%:- skipped set_up_crossword/4.
