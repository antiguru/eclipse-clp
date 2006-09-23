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
% Contributor(s): IC-Parc, Imperal College London
% 
% END LICENSE BLOCK
%
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: corse.pl,v 1.1 2006/09/23 01:55:09 snovello Exp $
% ----------------------------------------------------------------------

%
% SEPIA DEMO PROGRAM
%
% IDENTIFICATION:	corse.pl
%
% AUTHOR:		Thomas Graf
%			Joachim Schimpf
%			Micha Meier
%
% CONTENTS:		go_corsica/0		runs the demo
%
% DESCRIPTION:
%
%	Draw a map of Corsica and place the city names in a
%	non-overlapping way. Several parameters may be
%	specified in the dialog window.
%

:- module(corse).

:- coroutine.
:- global go_corsica/0, close_corsica/0.

:- dynamic
	window_data/4.

:- make_local_array(name_height(6), integer),
   setval(name_height(5), 75),
   setval(name_height(4), 55),
   setval(name_height(3), 32),
   setval(name_height(2), 25),
   setval(name_height(1), 17),
   make_local_array(split_flag),
   make_local_array(mode),
   setval(split_flag, 'split'),
   setval(mode, simple),
   make_local_array(marker_scale).

:-      make_local_array(flags),
	get_flag(debug_compile, DC),
	get_flag(variable_names, VN),
	setval(flags, flags(DC, VN)),
	nodbgcomp,
	true.

% To avoid redefining color 0, which would cause funny effects on the screen,
% we use 100 in color mode, but we still have to use 0 in mono mode.
tr_white(_, White) :-
	(getenv("COLOUR", _) ->
		White = 100
	;
		White = 0
	).

tr_right(_, 2190).

:- define_macro(no_macro_expansion(white), tr_white/2, []).
:- define_macro(no_macro_expansion(right_border), tr_right/2, []).

marker_gap(8).
split_gap(10).
frame_gap(18).

:- dynamic
	loaded_font/2,
	stop/0.


go_corsica :-
	(open_pce -> true; true),
	my_open_window,
	draw_map,
	make_dialog.

%-------------------------------------------------------
% placement routine
%-------------------------------------------------------

place_names :-
	clear_screen,
	set_drawing_mode(replace),
	draw_map,
	message('Reading the city data'),
	read_cities(Cities),

	sort_cities(Cities, Sorted),
	getval(mode, Mode),
	(solve(Sorted, Mode) ->
	    set_drawing_mode(replace),
	    clear_screen,
	    draw_map,
	    message('Solution found'),
	    text_color(106),
	    draw_final_locations(Sorted)
	;
	    set_drawing_mode(replace),
	    message('No solution found')
	).

solve(Cities, constraints) :-
	message('Processing the constraints'),
	constrain_cities(Cities),
	message('Solving the constraints'),
	set_drawing_mode(xor),
	label(Cities).
solve(Cities, simple) :-
	message('Building connected groups'),
	connect_groups(Cities),
	label_groups(Cities, 1),
	sort(1, =<, Cities, Sorted),
	split_into_groups(Sorted, Grouped),
	message('Generating the values'),
	set_drawing_mode(xor),
	place_simple(Grouped, Rest),		% deterministic algorithm
	constrain(Rest),			% rest by coroutining
	place_try(Rest).			% ... with backtracking

message(Text) :-
	fill_color(white),
	interior_style(1, 0),
	rectangle(10, 10, 2550, 150),
	text_color(107),
	text_font_index(3),
	text(20, 50, Text).
	
sort_cities(Cities, Sorted) :-
	terms_to_keyed(Cities, Keyed),
	sort(1, >=, Keyed, KSorted),
	terms_to_keyed(Sorted, KSorted).

terms_to_keyed([], []).
terms_to_keyed([City|Cities], [Key-City|Rest]) :-
	arg(2, City, box(_, _, _, Key)),
	terms_to_keyed(Cities, Rest).

draw_final_locations([]).
draw_final_locations([City|Cities]) :-
	display_city(City),
	draw_final_locations(Cities).

% Generating the values using the first fail principle
label([]).
label(Cities) :-
	deletedd(Cities, city(_, Frame, Box, Name, _, X, Y, Vars), Rest),
	(find_free_position(Vars, Pos) ->
	    memberchk([Name|Pos], Vars)
	;
	    member([Name|Pos], Vars)
	),
	valid_box(Pos, X, Y, Frame, Box),
	draw_box(Box),
	clear_city(Vars),
	(stop -> exit_block(stopped) ; label(Rest)).

find_free_position([[Var|Pos]|_], Pos) :-
	Var \== 0,
	not_overlap(Var),
	!.
find_free_position([_|Vars], Pos) :-
	find_free_position(Vars, Pos).

not_overlap(_{suspend:MX}) ?-
	arg(1, MX, Susps-_),
	find_matching_goal(Susps, overlap(_, _)),
	!,
	fail.
not_overlap(_).

find_matching_goal(Susps, _) :-
    var(Susps), !,
    fail.
find_matching_goal([Susp|_], Goal) :-
    suspension_to_goal(Susp, Goal, _Module),
    !.
find_matching_goal([_|Susps], Goal) :-
    find_matching_goal(Susps, Goal).

deletedd([City|Cities], Selected, Rest) :-
	eval_constraints(City, Number),
	deletedd(Cities, Selected, City, Number, _Rest),
	delete(Selected, [City|Cities], Rest).

:- mode deletedd(+, +, -, +, -).
deletedd([], City, City, _, []) :- !.
deletedd([City|Cities], Selected, SoFar, Number, Rest) :-
	(Number =< 1500 ->
		Selected = SoFar,
		Rest = [City|Cities]
	;
		eval_constraints(City, NewNumber),
		(NewNumber < Number ->
			Rest = [SoFar|NewRest],
			deletedd(Cities, Selected, City, NewNumber, NewRest)
		;
			Rest = [City|NewRest],
			deletedd(Cities, Selected, SoFar, Number, NewRest)
		)
	).

eval_constraints(city(_, _, _, _, _, _, _, Vars), Number) :-
	eval_constraints(Vars, Number, 0).

eval_constraints([], N, N).
eval_constraints([[Var|_]|Vars], Number, A) :-
	delayed_goals_number(Var, N0),
	N = N0,
	(N == 0 ->
		    Number = 1000
	;
	var(Var) ->
		A1 is A - N + 1000,
		eval_constraints(Vars, Number, A1)
	;
		eval_constraints(Vars, Number, A)
	).

clear_city(Vars) :-
	clear_city(Vars, Free, Zeros),
	Free = Zeros.

clear_city([], [], []).
clear_city([[Var|_]|Vars], [Var|Free], [0|Zeros]) :-
	var(Var),
	!,
	clear_city(Vars, Free, Zeros).
clear_city([_|Vars], Free, Zeros) :-
	clear_city(Vars, Free, Zeros).

member(X,[X|_]).
member(X,[_|T]) :- member(X,T).

memberchk(X,[X|_]) :- !.
memberchk(X,[_|T]):- memberchk(X,T).

:- dynamic valid_box/5.

valid_box('right up', X0, Y0, box(_, _, X2, Y2), box(X0, Yup, X2, Y2)) :-
	Yup is Y0 + marker_gap,
	X2 =< right_border,
	true.
valid_box('right down', X0, Y0, box(_, Y1, X2, _), box(X0, Y1, X2, Y0)) :-
	X2 =< right_border,
	true.
valid_box('left up', X0, Y0, box(X1, _, _, Y2), box(X1, Yup, X0, Y2)) :-
	Yup is Y0 + marker_gap,
	X1 >= 0,
	true.
valid_box('left down', X0, Y0, box(X1, Y1, _, _), box(X1, Y1, X0, Y0)) :-
	X1 >= 0,
	true.
valid_box('middle up', X0, Y0, box(X1, _, X2, Y2), box(Xi, Yup, Xa, Y2)) :-
	Xi is (X1 + X0) // 2,
	Xa is (X0 + X2) // 2,
	Yup is Y0 + marker_gap.
valid_box('middle down', X0, Y0, box(X1, Y1, X2, _), box(Xi, Y1, Xa, Y0)) :-
	Xi is (X1 + X0) // 2,
	Xa is (X0 + X2) // 2.


% setup constraints in the form of delayed goals
% for nondeterministic placements

constrain_cities([]).
constrain_cities([City|Cities]) :-
	arg(8, City, Vars),
	constrain_pairs(Vars, City, Cities),
	constrain_cities(Cities).

%:- skipped constrain_cities/1.

% Check one city agains the remaining ones
constrain_pairs(_, _, []).
constrain_pairs(Vars, City1, [City2|Cities]) :-
	arg(2, City1, Frame1),
	arg(2, City2, Frame2),
	(allowed(Frame1, Frame2) ->
		check_free_positions(Vars, City1)
	;
		check_overlapped(Vars, City1, City2)
	),
	(arg(2, Frame1) =< arg(4, Frame2) + frame_gap ->
	    constrain_pairs(Vars, City1, Cities)
	;
	    true
	).

% Check one city against another one
check_overlapped([], _, _).
check_overlapped([[Var|Pos]|Vars], City1, City2) :-
	City1 = city(_, Frame, _, _, _, X, Y, _),
	(valid_box(Pos, X, Y, Frame, Box) ->
	    arg(8, City2, Vars2),
	    check_overlapped_pairs(Var, Box, Vars2, City2, 0, N),
	    (N == 6 ->
		    Var = 0
	    ;
		    true
	    )
	;
	    Var = 0
	),
	check_overlapped(Vars, City1, City2).

% Remove impossible positions
check_free_positions([], _).
check_free_positions([[Var|Pos]|Vars], City) :-
	City = city(_, Frame, _, _, _, X, Y, _),
	(valid_box(Pos, X, Y, Frame, _) ->
	    true
	;
	    Var = 0
	),
	check_free_positions(Vars, City).

% Check one box against another city
check_overlapped_pairs(_, _, [], _, N, N).
check_overlapped_pairs(Var1, Box1, [[Var2|Pos]|Vars], City, N0, N) :-
	City = city(_, Frame, _, _, _, X, Y, _),
	(valid_box(Pos, X, Y, Frame, Box2) ->
	    (allowed(Box1, Box2) ->
		    N0 = N1
	    ;
		    overlap(Var1, Var2),
		    N1 is N0 + 1
	    )
	;
	    N1 is N0 + 1
	),
	check_overlapped_pairs(Var1, Box1, Vars, City, N1, N).

% Constrain the shared variables only if something was really placed,
% otherwise the suspended goal disappears.
delay overlap(Var1, Var2) if var(Var1), var(Var2).
overlap(Var1, Var2) :-
	(atom(Var1) ->
		Var2 = 0
	;
	atom(Var2) ->
		Var1 = 0
	;
		true
	).

% allowed/2 succeeds iff the distance between two boxes is sufficient

delay allowed(_, Y) if nonground(Y).
delay allowed(X, _) if nonground(X).
allowed(box(X11, Y11, X21, Y21), box(X12, Y12, X22, Y22)) :-
	X11 - X22 >= frame_gap -> true ;
	X12 - X21 >= frame_gap -> true ;
	Y11 - Y22 >= frame_gap -> true ;
	Y12 - Y21 >= frame_gap -> true.

% The group algorithm
% deterministic placement algorithm for frames that
% have a completely unoverlapped sub-box

place_simple([], []).
place_simple([Group|Groups], [GroupRest|Rests]) :-
	iterate_place_simple(Group, GroupRest),
	place_simple(Groups, Rests).

iterate_place_simple(Group, GroupRest) :-
	place_simple(Group, [], GroupRest0, Flag),
	(Flag == binding_done ->
		iterate_place_simple(GroupRest0, GroupRest)
	;
		GroupRest = GroupRest0
	).

place_simple([], Tried, Tried, nothing_done).
place_simple([City|Cities], Tried0, Tried, binding_done) :-
	City = city(_, Frame, Box, _, _, X, Y),
	valid_box(_, X, Y, Frame, Box),
	box_unoverlapped_by_frames(Box, Cities),
	box_unoverlapped_by_frames(Box, Tried0),
	draw_box(Box),
	!,				% commit this choice for Box
	place_simple(Cities, Tried0, Tried, _).
place_simple([City|Cities], Tried0, Tried, Flag) :-
	% Box for City could not be placed
	place_simple(Cities, [City|Tried0], Tried, Flag).

box_unoverlapped_by_frames(Box, []).
box_unoverlapped_by_frames(Box, [City|Cities]) :-
	arg(2, City, Frame),
	allowed(Box, Frame),
	box_unoverlapped_by_frames(Box, Cities).


% nondeterministic generation of placements

place_try([]).
place_try([Group|Groups]) :-
	box_in_frame(Group),
	!,
	place_try(Groups).

box_in_frame([]).
box_in_frame([city(_, Frame, Box, _, _, X, Y, _)|Cities]) :-
	valid_box(_, X, Y, Frame, Box),
	draw_box(Box),
	(stop -> exit_block(stopped) ; box_in_frame(Cities)).


% setup constraints in the form of delayed goals
% for nondeterministic placements

constrain([]).
constrain([Group|Groups]) :-
	constrain_group(Group),
	constrain(Groups).

constrain_group([]).
constrain_group([City|Cities]) :-
	constrain_pairs(City, Cities),
	constrain_group(Cities).

constrain_pairs(_, []).
constrain_pairs(City1, [City2|Cities]) :-
	arg(3, City1, Box1),
	arg(3, City2, Box2),
	allowed(Box1, Box2),
	constrain_pairs(City1, Cities).


% grouping algorithm
% split the cities into independent groups

split_into_groups([], []).
split_into_groups([City1, City2|Cs], [[City1|FG]|Gs]) :-
	same_group(City1, City2),
	!,
	split_into_groups([City2|Cs], [FG|Gs]).
split_into_groups([City1|Cs], [[City1]|Gs]) :-
%	not same_group(City1, City2),
	split_into_groups(Cs, Gs).

same_group(City1, City2) :-
	arg(1, City1, Gr),
	arg(1, City2, Gr).


label_groups([], _).
label_groups([City|Cs], N) :-
	arg(1, City, Group),
	var(Group) ->
		Group = N,
		N1 is N+1,
		label_groups(Cs, N1)
	;
		label_groups(Cs, N).
		

connect_groups([]).
connect_groups([C|Cs]) :-
	connect_groups(C, Cs),
	connect_groups(Cs).

connect_groups(_, []).
connect_groups(City1, [city(Group2,Frame2,_,_,_,_,_,_)|Cs]) :-
	City1 = city(Group1,Frame1,_,_,_,_,_,_),
	(allowed(Frame1, Frame2) ->
		true
	;
		Group1 = Group2
	),
	connect_groups(City1, Cs).

%-------------------------------------------------------
% setup the data structures
%-------------------------------------------------------

read_cities(Cities) :-
	locations(List),
	findall([_|X], valid_box(X, _, _, _, _), Vars),
	convert(List, Cities, Vars).

convert([],[], _).
convert([[Name,_,Prio,X,Y]|TL], [City|TC], Vars) :-
	copy_term(Vars, NewVars),
	City = city(_Group, _Frame, _Box, Name, Prio, X, Y, NewVars),
	compute_frame(City),
	draw_frame(City),
	convert(TL, TC, Vars).


compute_frame(city(_, box(X1, Y1, X2, Y2), _, Name, Prio, X, Y, _)) :-
	name_length(Name, Lines, W),
	getval(name_height(Prio), Height),
	XM is fix(W*Height/30),
	( Lines == 1 ->
	    YM = Height
	;
	    YM is 2*Height + split_gap
	),
	X1 is X - XM,
	X2 is X + XM,
	Y1 is Y - YM - marker_gap,
	Y2 is Y + YM + marker_gap.


% compute the print width of a name (W) and the number of lines needed (H)

name_length(Name, H, W) :-
	atom_string(Name, Str),
	( split_name(Str, Str1, Str2) ->
		W0 is max(string_width(Str1), string_width(Str2)),
		H = 2
	;
		string_width(Str, W0),
		H = 1
	),
	W is W0 + 10.


split_name(Str, Str1, Str2) :-
	getval(split_flag, split),
	string_length(Str, Len),
	Len > 12,
	splittable_string(Str, Pos),
	substring(Str, 1, Pos, Str1),
	Len2 is string_length(Str) - Pos,
	Pos2 is Pos + 1,
	substring(Str, Pos2, Len2, Str2),
	!.

splittable_string(Str, Pos) :-
  (substring(Str, Pos, _, "-"); substring(Str, Pos, _, " ")),
  Pos > 4,
  !.


string_width(String, Width) :-
	string_length(String, Len),
	string_width(String, Len, 0, Width).

string_width(_, 0, L, L) :- !.
string_width(String, Pos, Len0, Len) :-
	substring(String, Pos, 1, Letter),
	char_int(Letter, Ascii),
	Len1 is Len0 + letter_width(Ascii),
	Pos1 is Pos - 1,
	string_width(String, Pos1, Len1, Len).

% width for height 30

:- mode letter_width(+,-).

letter_width(0'., 14).
letter_width(0'', 16).
letter_width(0'I, 22).
letter_width(0' , 24).
letter_width(0'J, 26).
letter_width(0'L, 32).
letter_width(0'T, 32).
letter_width(0'A, 33).
letter_width(0'F, 33).
letter_width(0'S, 33).
letter_width(0'Z, 33).
letter_width(0'C, 34).
letter_width(0'E, 34).
letter_width(0'B, 37).
letter_width(0'D, 37).
letter_width(0'G, 37).
letter_width(0'O, 37).
letter_width(0'P, 37).
letter_width(0'Q, 37).
letter_width(0'R, 37).
letter_width(0'V, 37).
letter_width(0'X, 37).
letter_width(0'-, 37).
letter_width(0'K, 38).
letter_width(0'N, 40).
letter_width(0'Y, 40).
letter_width(0'H, 40).
letter_width(0'U, 40).
letter_width(0'W, 40).
letter_width(0'M, 41).


%-------------------------------------------------------
% drawing
%-------------------------------------------------------

my_open_window :-
	global(window_data/4),
	asserta(window_data(0,0,500,870)),
	(open_2d(0,0,right_border,3900) -> true ; true ),
	setval(marker_scale, 10),
	call(sepia_dir(Dir), kegi),
	(getenv("COLOUR", _) ->
	    concat_atoms(Dir, '/demo/sepia/corse.xwd', Path)
	;
	    concat_atoms(Dir, '/demo/sepia/corse.xwd.mono', Path)
	),
	load_bitmap(Path),
	set_2d_colors,
	clear_screen,
	interior_style(0,1),
        perimeter_width(1),
        perimeter_type(0),
        perimeter_color(103).

my_close_window :-
	close_2d.

draw_map :-
	pixel_array(80, 4000).

draw_corsica([]).
draw_corsica([F|R]) :-
	polyline(F),
	draw_corsica(R).

draw_box(box(X1, Y1, X2, Y2)) :-
	fill_color(104),
	interior_style(1, 0),
	rectangle(X1, Y1, X2, Y2).
draw_box(box(X1, Y1, X2, Y2)) :-
	fill_color(104),
	interior_style(1, 0),
	rectangle(X1, Y1, X2, Y2),
	fail.


draw_frame(city(_, box(X1, Y1, X2, Y2), _, _, Prio, X0, Y0, _)) :-
        MS is getval(marker_scale) * Prio,
        marker_size(MS),
	marker(X0, Y0),
	fill_color(0),
	interior_style(1,1),
	rectangle(X1, Y1, X2, Y2),
	interior_style(0,1),
	rectangle(X1, Y1, X2, Y2).


% print a city marker and the name into the specified box

display_city(city(_, _, box(X1, Y1, X2, Y2), Name, Prio, X0, Y0, _)) :-
	getval(name_height(Prio), Height),
	fill_color(105),
	interior_style(1, 0),
	Mh is Height//3,
	circle(X0, Y0, Mh),
	call(window_fact(_, C, _, _, _, _, _, _, _, _, _, _, _, _, _, _, Gfont, _), kegi),
	(loaded_font(Height, Font) ->
	    true
	;
	    Size is 850 * Height // 2400,
	    concat_atom(['-linotype-times-bold-r-normal--', Size, '-*'], FName),
	    call(xOpenFont(C, FName, Font), kegi),
	    assert(loaded_font(Height, Font))
	),
	call((
	    xSetGC(C, Gfont, [xFont(Font)]),
	    xFlush(C)), kegi),
	atom_string(Name, Str),
	( split_name(Str, Str1, Str2) ->
		Ym is Y2 - Height,
		text(X1, Ym, Str1),
		text(X1, Y1, Str2)
	;
		text(X1, Y1, Str)
	).

color_2d(Number, Name) :-
	call(colour(Name, R, G, B), kegi),
	color_rgb(Number, R, G, B).

color_2d_xor(Number, Name, Xor) :-
	call(colour(Name, R, G, B), kegi),
	call(color_rgb_xor(Number, Xor, R, G, B), kegi).

set_2d_colors :-
	(getenv("COLOUR", _) ->
	    color_rgb(100, 150, 225, 255),	% sea
	    color_2d(101, lightgrey),		% iland
	    color_2d(102, yellow),		% beach
	    color_2d(103, red),			% possible city position box
	    color_2d_xor(104, deeppink, 0),	% current city position box
	    color_2d(105, yellow),		% city circle
	    color_2d(106, yellow),		% city name
	    color_2d(107, black) 		% message
	;
	    true
	).

clear_screen :-
	clear_view_surface(white).
%-------------------------------------------------------
% dialog window
%-------------------------------------------------------

:- make_callback(corse_dialog_pressed/2).
:- make_callback(split_selected/2).
:- make_callback(constraints_selected/2).
:- make_callback(prio_selected/2).
:- make_callback(slider1/2).
:- make_callback(slider2/2).
:- make_callback(slider3/2).
:- make_callback(slider4/2).
:- make_callback(slider5/2).

make_dialog :-
	new_dialog(@map_dialog, 'Map Layout Dialog', dialog_panel),
	new(@ prio_menu, popup('Placement Priority', cascade(0, prio_selected, 0))),
	send(@map_dialog, popup, @prio_menu),
	make_menu,
	send(@map_dialog, open, point(380, 575)).

make_menu :-
	findall(X, valid_box(X,_,_,_,_), Labels),
	send(@prio_menu, append, Labels).

new_browser(Browser, Label, Contents) :-
	new(Browser, browser(Label)),
	send(Browser, append, Contents).


dialog_panel(@corse_run, button('Run', corse_dialog_pressed), append, []).
dialog_panel(@corse_stop, button('Abort', corse_dialog_pressed), right, []).
dialog_panel(@corse_quit, button('Quit', corse_dialog_pressed), right, []).
dialog_panel(_, label(none, ''), below, []).
dialog_panel(_, menu('Long names are', cycle, cascade(0, split_selected, 0),
	['split', 'not split']), below, []).
dialog_panel(_, menu('Solving mode', cycle, cascade(0, constraints_selected, 0),
	[simple, constraints]), below, []).
dialog_panel(_, label(none, 'Specify text heights for city sizes:'), below, []).
dialog_panel(_, slider('city 5', 10, 80, 75, slider5), below, [width:100]).
dialog_panel(_, slider('city 4', 10, 80, 55, slider4), below, [width:100]).
dialog_panel(_, slider('city 3', 10, 80, 32, slider3), below, [width:100]).
dialog_panel(_, slider('city 2', 10, 80, 25, slider2), below, [width:100]).
dialog_panel(_, slider('city 1', 10, 80, 17, slider1), below, [width:100]).
dialog_panel(_, label(none, 'Position preferences on popup menu'), below, []).

split_selected(_, Flag) :-
	setval(split_flag, Flag).

constraints_selected(_, Flag) :-
	setval(mode, Flag).

prio_selected(_, Place) :-
	retract(valid_box(Place,A,B,C,D) :- E), !,
	asserta(valid_box(Place,A,B,C,D) :- E),
	refresh_prio_menu.

refresh_prio_menu :-
	send(@prio_menu, clear),
	make_menu.

slider1(_, Value) :-
	setval(name_height(1), Value).
slider2(_, Value) :-
	setval(name_height(2), Value).
slider3(_, Value) :-
	setval(name_height(3), Value).
slider4(_, Value) :-
	setval(name_height(4), Value).
slider5(_, Value) :-
	setval(name_height(5), Value).

corse_dialog_pressed(_, 'Run') :-
	retract_all(stop),
	send(@pce, async, 0),
	send(@corse_stop, [greyed:off, active:on]),
	send(@corse_run, [greyed:on, active:off]),
	send(@corse_quit, [greyed:on, active:off]),
	block(place_names, stopped, true),
	send(@corse_quit, [greyed:off, active:on]),
	send(@corse_run, [greyed:off, active:on]),
	send(@corse_stop, [greyed:on, active:off]).
corse_dialog_pressed(_, 'Abort') :-
	send(@corse_run, [greyed:off, active:on]),
	send(@corse_quit, [greyed:off, active:on]),
	send(@corse_stop, [greyed:on, active:off]),
	assert(stop).
corse_dialog_pressed(_, 'Quit') :-
	close_corsica,
	abort.

%-------------------------------------------------------
% cleanup
%-------------------------------------------------------

close_corsica :-
	object(@map_dialog),
	send(@map_dialog, destroy),
	call(get_flag(window_data/4, definition_module, corse), kegi),
	local(window_data/4),
	close_2d,
	!.
close_corsica.

%-------------------------------------------------------
% map data
%-------------------------------------------------------

:- mode corsica(-).

corsica([
[1588, 3793, 1580, 3795, 1568, 3795, 1561, 3800, 1549, 3800, 
1542, 3796, 1534, 3792, 1525, 3791, 1516, 3792, 1509, 3790, 
1484, 3770, 1482, 3760, 1486, 3750, 1486, 3742, 1485, 3729, 
1491, 3725, 1494, 3721, 1494, 3716, 1489, 3702, 1487, 3692, 
1488, 3685, 1492, 3676, 1502, 3666, 1509, 3652, 1511, 3649, 
1502, 3646, 1496, 3638, 1493, 3630, 1500, 3616, 1505, 3610, 
1508, 3605, 1499, 3604, 1495, 3599, 1492, 3592, 1481, 3588, 
1473, 3585, 1465, 3578, 1452, 3568, 1448, 3562, 1447, 3551, 
1452, 3539, 1452, 3526, 1456, 3503, 1467, 3494, 1475, 3489, 
1470, 3482, 1461, 3474, 1455, 3462, 1450, 3454, 1436, 3442, 
1431, 3438, 1427, 3432, 1427, 3424, 1425, 3418, 1427, 3407, 
1434, 3398, 1437, 3387, 1441, 3377, 1451, 3372, 1456, 3371, 
1458, 3367, 1455, 3363, 1459, 3352, 1460, 3350, 1464, 3345, 
1469, 3342, 1470, 3336, 1470, 3328, 1474, 3324, 1475, 3320, 
1472, 3314, 1468, 3306, 1469, 3294, 1470, 3282, 1470, 3272, 
1472, 3261, 1474, 3254, 1472, 3241, 1476, 3232, 1482, 3220, 
1481, 3211, 1485, 3204, 1481, 3198, 1475, 3192, 1468, 3185, 
1464, 3178, 1458, 3173, 1450, 3170, 1447, 3158, 1445, 3145, 
1445, 3130, 1447, 3124, 1441, 3117, 1420, 3095, 1415, 3095, 
1406, 3088, 1404, 3075, 1398, 3072, 1387, 3078, 1377, 3089, 
1365, 3115],
[1365, 3115, 1363, 3124, 1355, 3130, 1343, 3130, 1337, 3140, 
1335, 3156, 1334, 3168, 1319, 3173, 1310, 3171, 1297, 3184, 
1294, 3192, 1285, 3199, 1273, 3202, 1261, 3194, 1256, 3188, 
1251, 3183, 1243, 3184, 1230, 3187, 1208, 3199, 1203, 3201, 
1192, 3207, 1186, 3207, 1173, 3203, 1163, 3201, 1155, 3202, 
1151, 3202, 1149, 3196, 1146, 3192, 1138, 3197, 1130, 3197, 
1122, 3193, 1110, 3188, 1101, 3182, 1090, 3168, 1075, 3166, 
1070, 3167, 1059, 3158, 1060, 3141, 1052, 3132, 1045, 3121, 
1039, 3117, 1034, 3117, 1022, 3123, 1020, 3111, 1020, 3107, 
1010, 3096, 1015, 3085, 1016, 3070, 1021, 3061, 1021, 3055, 
1024, 3049, 1011, 3047, 1005, 3043, 994, 3037, 974, 3032, 
962, 3027, 959, 3019, 956, 3012, 946, 3005, 940, 3003, 
929, 3009, 914, 3004, 887, 3001, 875, 2998, 858, 2992, 
834, 2989, 825, 2993, 815, 3009, 807, 2998, 795, 2990, 
780, 2984, 759, 2980, 741, 2978, 722, 2962, 715, 2945, 
706, 2933, 688, 2937, 672, 2935, 660, 2925, 649, 2915, 
644, 2920, 639, 2927, 630, 2926, 612, 2921, 605, 2918, 
613, 2903, 614, 2895, 611, 2889, 599, 2882, 599, 2876, 
597, 2866, 600, 2860, 602, 2852, 596, 2846, 582, 2834, 
569, 2819, 540, 2820, 534, 2823, 523, 2846, 510, 2846, 
499, 2846],
[499, 2846, 489, 2841, 484, 2830, 479, 2830, 478, 2843, 
478, 2859, 472, 2876, 466, 2878, 459, 2870, 459, 2869, 
450, 2864, 447, 2855, 453, 2833, 463, 2818, 461, 2808, 
468, 2799, 469, 2794, 460, 2792, 456, 2790, 461, 2780, 
456, 2776, 453, 2770, 456, 2761, 461, 2756, 461, 2752, 
457, 2748, 438, 2750, 431, 2751, 423, 2745, 421, 2739, 
416, 2730, 410, 2727, 399, 2728, 391, 2730, 380, 2725, 
375, 2717, 376, 2703, 375, 2694, 375, 2688, 361, 2665, 
352, 2649, 354, 2644, 359, 2639, 370, 2641, 387, 2642, 
393, 2634, 394, 2623, 387, 2614, 377, 2604, 372, 2600, 
374, 2593, 370, 2587, 361, 2583, 355, 2574, 346, 2566, 
344, 2557, 344, 2551, 346, 2535, 340, 2526, 326, 2523, 
311, 2525, 300, 2523, 295, 2519, 281, 2508, 275, 2498, 
280, 2489, 272, 2480, 273, 2475, 276, 2466, 274, 2454, 
264, 2448, 253, 2443, 238, 2443, 223, 2441, 217, 2440, 
215, 2432, 211, 2423, 204, 2418, 194, 2420, 185, 2423, 
182, 2434, 177, 2447, 173, 2438, 171, 2422, 172, 2412, 
189, 2408, 190, 2400, 190, 2392, 187, 2382, 184, 2373, 
181, 2368, 188, 2361, 181, 2349, 180, 2337, 198, 2337, 
216, 2352],
[216, 2352, 235, 2370, 264, 2375, 272, 2368, 276, 2364, 
285, 2364, 292, 2355, 301, 2351, 300, 2326, 291, 2320, 
255, 2312, 253, 2301, 257, 2291, 258, 2278, 276, 2276, 
292, 2283, 299, 2286, 307, 2274, 313, 2265, 329, 2260, 
339, 2262, 355, 2262, 366, 2249, 375, 2249, 381, 2239, 
381, 2218, 398, 2213, 396, 2197, 408, 2189, 408, 2183, 
378, 2175, 323, 2165, 307, 2157, 300, 2159, 286, 2160, 
272, 2157, 265, 2151, 250, 2142, 239, 2140, 228, 2138, 
215, 2130, 205, 2124, 193, 2123, 179, 2125, 166, 2127, 
158, 2126, 157, 2121, 155, 2120, 159, 2112, 170, 2105, 
182, 2108, 198, 2108, 204, 2100, 205, 2095, 195, 2084, 
209, 2072, 194, 2055, 199, 2050, 213, 2056, 218, 2048, 
216, 2041, 212, 2026, 214, 2013, 222, 2002, 221, 1997, 
205, 1998, 192, 1991, 188, 1982, 195, 1975, 208, 1975, 
228, 1975, 233, 1970, 225, 1960, 216, 1956, 210, 1945, 
195, 1940, 179, 1932, 179, 1927, 183, 1925, 205, 1933, 
217, 1934, 225, 1933, 233, 1926, 234, 1918, 225, 1909, 
220, 1899, 214, 1890, 218, 1888, 229, 1887, 253, 1890, 
254, 1897, 260, 1894, 272, 1893, 280, 1872, 296, 1870, 
302, 1868],
[302, 1868, 312, 1861, 322, 1862, 340, 1836, 345, 1829, 
361, 1834, 377, 1830, 386, 1828, 393, 1837, 407, 1841, 
422, 1811, 420, 1791, 437, 1779, 444, 1764, 438, 1737, 
470, 1740, 481, 1727, 484, 1709, 482, 1696, 468, 1688, 
453, 1692, 450, 1679, 433, 1667, 404, 1658, 393, 1662, 
387, 1660, 381, 1650, 369, 1645, 347, 1631, 338, 1618, 
335, 1604, 338, 1594, 339, 1587, 350, 1586, 358, 1576, 
354, 1564, 341, 1547, 329, 1532, 325, 1529, 297, 1526, 
293, 1525, 285, 1530, 270, 1533, 261, 1533, 248, 1530, 
234, 1524, 232, 1521, 227, 1514, 231, 1509, 237, 1501, 
240, 1494, 249, 1483, 253, 1469, 261, 1466, 274, 1459, 
274, 1444, 271, 1427, 261, 1418, 257, 1408, 253, 1393, 
251, 1376, 251, 1367, 266, 1376, 302, 1384, 335, 1384, 
350, 1386, 378, 1383, 419, 1387, 431, 1391, 445, 1399, 
456, 1412, 470, 1430, 473, 1439, 488, 1444, 498, 1429, 
512, 1428, 531, 1427, 539, 1426, 554, 1429, 566, 1427, 
565, 1418, 563, 1416, 568, 1410, 566, 1405, 556, 1404, 
556, 1397, 567, 1389, 568, 1375, 569, 1367, 564, 1360, 
556, 1351, 548, 1344, 538, 1341, 534, 1330, 531, 1316, 
533, 1295],
[533, 1295, 538, 1274, 539, 1265, 534, 1263, 525, 1257, 
511, 1255, 497, 1257, 488, 1251, 489, 1246, 493, 1237, 
500, 1239, 510, 1242, 511, 1236, 524, 1228, 528, 1225, 
526, 1215, 519, 1199, 514, 1176, 509, 1175, 495, 1175, 
482, 1176, 475, 1170, 466, 1158, 462, 1152, 451, 1150, 
435, 1150, 426, 1159, 421, 1156, 414, 1148, 413, 1148, 
431, 1134, 442, 1126, 445, 1099, 444, 1096, 435, 1094, 
432, 1084, 426, 1071, 419, 1065, 405, 1059, 393, 1053, 
392, 1051, 380, 1044, 368, 1042, 353, 1045, 344, 1048, 
336, 1048, 331, 1040, 329, 1034, 328, 1027, 334, 1026, 
338, 1027, 346, 1024, 367, 1022, 390, 1023, 401, 1018, 
402, 1015, 404, 1007, 401, 998, 401, 990, 404, 982, 
415, 980, 425, 982, 437, 985, 451, 992, 471, 1001, 
486, 1009, 503, 1015, 515, 1017, 526, 1009, 530, 1001, 
527, 987, 519, 978, 513, 964, 514, 957, 520, 950, 
535, 945, 535, 938, 533, 934, 544, 930, 549, 938, 
558, 950, 569, 951, 587, 955, 599, 956, 605, 941, 
611, 930, 622, 924, 634, 920, 660, 918, 684, 914, 
697, 911, 701, 904, 717, 906, 733, 903, 740, 901, 
746, 892],
[746, 892, 738, 881, 728, 874, 710, 871, 701, 867, 
690, 863, 690, 852, 688, 832, 682, 823, 671, 811, 
661, 811, 647, 808, 632, 802, 620, 793, 606, 782, 
584, 767, 573, 775, 567, 787, 563, 792, 559, 783, 
558, 783, 555, 778, 548, 772, 543, 766, 543, 756, 
542, 749, 540, 746, 533, 742, 533, 738, 536, 727, 
538, 714, 536, 705, 531, 697, 524, 693, 512, 686, 
513, 687, 516, 681, 522, 674, 526, 670, 535, 668, 
537, 659, 544, 651, 553, 644, 551, 638, 544, 633, 
536, 627, 529, 621, 531, 612, 538, 603, 548, 603, 
563, 603, 574, 601, 577, 595, 577, 588, 583, 582, 
590, 579, 595, 579, 603, 583, 609, 584, 617, 582, 
626, 577, 628, 574, 634, 576, 639, 571, 643, 556, 
634, 551, 626, 542, 620, 533, 620, 523, 624, 519, 
636, 520, 645, 523, 657, 524, 667, 527, 674, 530, 
686, 530, 694, 520, 699, 514, 692, 504, 693, 496, 
700, 494, 719, 498, 731, 496, 748, 484, 753, 472, 
752, 462, 760, 460, 765, 468, 771, 470, 778, 463, 
782, 457, 789, 457, 796, 462, 810, 464, 824, 455, 
823, 445],
[823, 445, 837, 436, 846, 426, 852, 422, 857, 428, 
863, 439, 870, 440, 883, 432, 889, 426, 904, 424, 
911, 412, 920, 403, 925, 394, 929, 403, 934, 412, 
944, 413, 947, 404, 945, 389, 951, 384, 961, 387, 
972, 390, 975, 396, 982, 406, 991, 415, 1001, 422, 
1012, 426, 1009, 419, 1001, 411, 994, 395, 992, 379, 
998, 364, 1008, 355, 1018, 353, 1033, 360, 1042, 367, 
1049, 356, 1052, 352, 1062, 352, 1073, 346, 1078, 356, 
1090, 358, 1099, 358, 1092, 351, 1082, 339, 1068, 321, 
1059, 309, 1056, 301, 1044, 285, 1039, 272, 1037, 250, 
1042, 236, 1055, 237, 1072, 242, 1091, 249, 1094, 239, 
1111, 234, 1130, 222, 1150, 223, 1161, 214, 1174, 189, 
1175, 181, 1179, 172, 1193, 170, 1202, 168, 1211, 170, 
1212, 174, 1220, 173, 1233, 176, 1246, 179, 1251, 180, 
1254, 186, 1253, 193, 1255, 204, 1258, 208, 1264, 214, 
1269, 224, 1275, 233, 1286, 244, 1297, 261, 1304, 272, 
1313, 280, 1316, 292, 1319, 301, 1320, 305, 1317, 312, 
1310, 310, 1293, 300, 1272, 284, 1264, 278, 1263, 275, 
1263, 270, 1259, 263, 1254, 262, 1250, 267, 1246, 275, 
1244, 281],
[1244, 281, 1247, 284, 1254, 288, 1258, 298, 1260, 312, 
1258, 324, 1255, 337, 1262, 344, 1267, 345, 1279, 350, 
1293, 359, 1304, 366, 1320, 380, 1323, 390, 1328, 394, 
1337, 392, 1340, 396, 1337, 402, 1340, 406, 1350, 410, 
1354, 419, 1359, 425, 1363, 438, 1361, 448, 1360, 457, 
1360, 464, 1355, 466, 1350, 467, 1343, 475, 1348, 482, 
1355, 495, 1356, 506, 1352, 517, 1345, 524, 1346, 535, 
1349, 542, 1356, 539, 1366, 539, 1373, 542, 1379, 546, 
1385, 556, 1393, 568, 1403, 578, 1417, 590, 1429, 601, 
1443, 609, 1447, 609, 1450, 607, 1455, 602, 1458, 604, 
1468, 612, 1473, 618, 1475, 623, 1475, 631, 1473, 641, 
1478, 650, 1486, 662, 1491, 669, 1495, 675, 1501, 683, 
1500, 687, 1495, 690, 1486, 689, 1462, 685, 1455, 692, 
1451, 700, 1437, 707, 1422, 709, 1412, 704, 1404, 696, 
1401, 683, 1396, 678, 1385, 670, 1383, 661, 1378, 656, 
1377, 662, 1372, 669, 1362, 674, 1362, 688, 1366, 710, 
1374, 717, 1391, 725, 1398, 733, 1397, 744, 1403, 755, 
1411, 751, 1419, 745, 1417, 734, 1413, 728, 1416, 723, 
1423, 726, 1424, 727, 1437, 729, 1447, 730, 1459, 732, 
1472, 732],
[1472, 732, 1476, 743, 1468, 761, 1463, 770, 1466, 783, 
1475, 785, 1486, 785, 1493, 780, 1500, 776, 1499, 778, 
1506, 785, 1514, 796, 1524, 803, 1531, 814, 1534, 825, 
1530, 832, 1522, 837, 1530, 842, 1530, 851, 1520, 858, 
1517, 867, 1518, 876, 1527, 885, 1533, 895, 1544, 896, 
1551, 897, 1557, 900, 1558, 910, 1558, 914, 1563, 922, 
1569, 931, 1566, 944, 1564, 953, 1563, 968, 1566, 976, 
1567, 988, 1566, 1006, 1569, 1021, 1570, 1057, 1566, 1072, 
1556, 1089, 1562, 1111, 1559, 1125, 1557, 1139, 1561, 1158, 
1565, 1172, 1569, 1189, 1570, 1194, 1568, 1215, 1568, 1228, 
1561, 1265, 1559, 1296, 1556, 1304, 1558, 1309, 1564, 1328, 
1561, 1342, 1565, 1358, 1571, 1372, 1572, 1379, 1581, 1388, 
1582, 1401, 1580, 1407, 1585, 1446, 1580, 1450, 1572, 1451, 
1570, 1457, 1570, 1467, 1565, 1473, 1564, 1479, 1573, 1494, 
1578, 1481, 1582, 1474, 1583, 1463, 1585, 1477, 1589, 1495, 
1598, 1511, 1608, 1529, 1614, 1538, 1604, 1536, 1610, 1545, 
1625, 1555, 1639, 1568, 1659, 1593, 1670, 1610, 1691, 1634, 
1708, 1668, 1723, 1693, 1749, 1735, 1770, 1773, 1786, 1788, 
1804, 1824, 1810, 1839, 1815, 1882, 1812, 1908, 1817, 1954, 
1820, 1999],
[1820, 1999, 1816, 2038, 1818, 2053, 1814, 2064, 1810, 2079, 
1813, 2111, 1817, 2147, 1824, 2184, 1817, 2230, 1809, 2275, 
1803, 2290, 1799, 2301, 1792, 2316, 1792, 2343, 1787, 2363, 
1786, 2387, 1785, 2413, 1788, 2447, 1790, 2483, 1805, 2520, 
1801, 2531, 1800, 2552, 1800, 2569, 1795, 2592, 1787, 2620, 
1786, 2642, 1785, 2663, 1782, 2681, 1786, 2705, 1789, 2730, 
1791, 2749, 1784, 2798, 1769, 2842, 1757, 2864, 1745, 2884, 
1717, 2923, 1705, 2939, 1694, 2963, 1680, 2984, 1666, 3005, 
1658, 3021, 1655, 3022, 1654, 3015, 1660, 3001, 1667, 2989, 
1671, 2976, 1677, 2966, 1685, 2954, 1691, 2944, 1695, 2934, 
1692, 2919, 1691, 2905, 1697, 2892, 1704, 2887, 1707, 2892, 
1702, 2908, 1706, 2916, 1716, 2907, 1726, 2892, 1734, 2880, 
1739, 2870, 1744, 2858, 1747, 2850, 1752, 2841, 1754, 2826, 
1753, 2818, 1743, 2822, 1727, 2834, 1711, 2843, 1700, 2847, 
1694, 2858, 1689, 2873, 1681, 2898, 1675, 2915, 1677, 2930, 
1668, 2952, 1659, 2968, 1643, 2980, 1643, 2995, 1646, 3009, 
1649, 3012, 1648, 3031, 1648, 3046, 1649, 3063, 1650, 3079, 
1649, 3097, 1655, 3122, 1663, 3155, 1662, 3174, 1669, 3210, 
1677, 3246, 1682, 3272, 1694, 3292, 1704, 3312, 1711, 3330, 
1719, 3352],
[1719, 3352, 1713, 3378, 1706, 3399, 1700, 3414, 1700, 3425, 
1701, 3433, 1703, 3445, 1700, 3461, 1698, 3477, 1690, 3499, 
1687, 3523, 1687, 3533, 1685, 3566, 1683, 3596, 1677, 3626, 
1680, 3650, 1661, 3669, 1661, 3673, 1656, 3675, 1659, 3692, 
1658, 3709, 1661, 3721, 1668, 3734, 1673, 3743, 1676, 3750, 
1661, 3758, 1648, 3766, 1642, 3770, 1638, 3778, 1639, 3778, 
1631, 3786, 1626, 3785, 1617, 3788, 1616, 3797, 1611, 3803, 
1604, 3805, 1602, 3800, 1595, 3795, 1588, 3793]]).

%-------------------------------------------------------
% location data
%-------------------------------------------------------

:- mode locations(-).

locations([
['BASTIA', 'VILLES', 5, 1621, 3125],
['AJACCIO', 'VILLES', 5, 447, 1433],
['CALVI', 'VILLES', 4, 500, 2831],
['CORTE', 'VILLES', 4, 1144, 2268],
['SARTENE', 'VILLES', 4, 833, 758],
['BONIFACIO', 'VILLES', 4, 1128, 241],
['PORTO-VECCHIO', 'VILLES', 4, 1329, 669],
['ROGLIANO', 'VILLES', 3, 1583, 3682],
['BRANDO', 'VILLES', 3, 1644, 3300],
['SAN-MARTINO-DI-LOTA', 'VILLES', 3, 1622, 3198],
['OLETTA', 'VILLES', 3, 1489, 2995],
['L\'ILE ROUSSE', 'VILLES', 3, 807, 2977],
['MURATO', 'VILLES', 3, 1438, 2857],
['BELGODERE', 'VILLES', 3, 938, 2880],
['BORGO', 'VILLES', 3, 1598, 2800],
['CAMPITELLO', 'VILLES', 3, 1419, 2742],
['CALENZANA', 'VILLES', 3, 678, 2713],
['VESCOVATO', 'VILLES', 3, 1601, 2665],
['MOROSAGLIA', 'VILLES', 3, 1384, 2546],
['PORTA', 'VILLES', 3, 1477, 2519],
['PIEDICROCE', 'VILLES', 3, 1505, 2402],
['OMESSA', 'VILLES', 3, 1242, 2409],
['CALACUCCIA', 'VILLES', 3, 928, 2349],
['CERVIONE', 'VILLES', 3, 1693, 2320],
['PIETRA-DI-VERDE', 'VILLES', 3, 1631, 2247],
['SERMANO', 'VILLES', 3, 1334, 2269],
['PIANA', 'VILLES', 3, 297, 2138],
['VENACO', 'VILLES', 3, 1156, 2093],
['GHISONI', 'VILLES', 3, 1231, 1819],
['BOCOGNANO', 'VILLES', 3, 998, 1771],
['VICO', 'VILLES', 3, 576, 1961],
['SARI-D\'ORCINO', 'VILLES', 3, 594, 1729],
['BASTELICA', 'VILLES', 3, 963, 1606],
['PRUNELLI-DI-FIUMORBO', 'VILLES', 3, 1419, 1614],
['ZICAVO', 'VILLES', 3, 1089, 1389],
['STA-MARIA-SICCHE', 'VILLES', 3, 856, 1319],
['PETRETO BICCHISANO', 'VILLES', 3, 868, 1114],
['OLMETO', 'VILLES', 3, 741, 971],
['LEVIE', 'VILLES', 3, 1091, 938],
['C. DE BAVELLA', 'SYMB PONCTUELS', 3, 1249, 1118],
['C. DE VERGIO', 'SYMB PONCTUELS', 3, 695, 2213],
['C. DE VIZZAVONE', 'SYMB PONCTUELS', 3, 1092, 1847],
['C. DE VERDE', 'SYMB PONCTUELS', 3, 1213, 1627],
['LURI', 'VILLES', 2, 1573, 3545],
['NONZA', 'VILLES', 2, 1482, 3303],
['ST-FLORENT', 'VILLES', 2, 1416, 3079],
['S.-PIETRO-DI-TENDA', 'VILLES', 2, 1331, 2917],
['LAMA', 'VILLES', 2, 1198, 2859],
['MURO', 'VILLES', 2, 768, 2790],
['CASTIFAO', 'VILLES', 2, 1088, 2694],
['OLMI-CAPPELLA', 'VILLES', 2, 938, 2755],
['PONTE-LECCIA', 'VILLES', 2, 1229, 2599],
['HAUT-ASCO', 'VILLES', 2, 813, 2498],
['VALLE D\'ALESANI', 'VILLES', 2, 1573, 2309],
['EVISA', 'VILLES', 2, 574, 2154],
['SOCCIA', 'VILLES', 2, 748, 2013],
['PIEDICORTE-DI-GAGGIO', 'VILLES', 2, 1430, 2107],
['VEZZANI', 'VILLES', 2, 1292, 1968],
['ALERIA', 'VILLES', 2, 1704, 1833],
['SALICE', 'VILLES', 2, 730, 1859],
['CARGESE', 'VILLES', 2, 235, 1901],
['GHISONACCIA', 'VILLES', 2, 1559, 1632],
['AULLENE', 'VILLES', 2, 1014, 1091],
['STA-LUCIA-DI-TALLANO', 'VILLES', 2, 1003, 908],
['FIGARI', 'VILLES', 2, 1086, 457],
['VIZZAVONA', 'VILLES', 1, 1140, 1907],
['ZONZA', 'VILLES', 1, 730, 864],
['PROPRIANO', 'VILLES', 1, 1163, 1035]
]).

:-      getval(flags, flags(DC, VN)),
	set_flag(debug_compile, DC),
	set_flag(variable_names, VN),
	erase_array(flags).
