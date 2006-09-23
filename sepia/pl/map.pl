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
% Version:	$Id: map.pl,v 1.1 2006/09/23 01:55:28 snovello Exp $
% ----------------------------------------------------------------------

/*

  SEPIA DEMO PROGRAM
 
  IDENTIFICATION:	queens.pl
 
  AUTHOR:		CHIP team
			Micha Meier
 
  CONTENTS:		go_map/0		runs the demo
 
  DESCRIPTION:
 
 	This program shows the difference of various constraint
 	solving approaches.

		Map Colouring	110 Countries

		sample graph of Marting Gardner Scientific American 4/1975
		(from a CHIP demo)

		using plain backtracking, coroutining or domain variables
		each variable has a domain of values {1,2,3,4}
*/

:- module(map).
:- use_module(library(fd)).

:- global
	close_map/0,
	go_map/0.

:- dynamic
	stop/0.
:- 
	make_local_array(mode),
	setval(mode, plain).

:-      make_local_array(flags),
	get_flag(debug_compile, DC),
	get_flag(variable_names, VN),
	setval(flags, flags(DC, VN)),
	nodbgcomp,
	true.

:- op(500, xfy, diff).

window_data(0, 0, 800, 420).

go_map :-
	global(window_data/4),
	(open_2d(0,10,450,230) -> true ; true),
	(open_pce -> true; true),
	make_dialog,
	draw_map.

close_map :-
	(close_2d -> true; true),
	object(@map_dialog),
	send(@map_dialog, destroy),
	call(get_flag(window_data/4, definition_module, map), kegi),
	local(window_data/4),
	!.
close_map.

place_map :-
	draw_map,
	draw_colors(L),
	getval(mode, Mode),
	cputime(T0),
	solve(Mode, L),
	T is cputime - T0,
	write("Time = "),
	writeln(T).

map0(Method) :-
	cputime(T0),
	solve(Method, _),
	T is cputime - T0,
	printf("Time = %.2f\n", T).

solve(delay, L) :-
	compile_term([X diff Y :- X ~= Y]),
	init(L),
	color(L,[1,2,3,4]).
solve(domains, L) :-
	compile_term([X diff Y :- X ## Y]),
	L :: 1..4,
	init(L),
	color(L,[1,2,3,4]).
solve(plain, L) :-
	compile_term([X diff Y :- (integer(X), integer(Y)) -> X \== Y; true]),
	make_var_list(110, L),
	solve_plain(L, L, [1,2,3,4]).

solve_plain([], _, _).
solve_plain([H|T], L, Colors) :-
	member(H,Colors),
	rotate(Colors,Colors1),
	init(L),
	(stop -> exit_block(abort) ; solve_plain(T, L, Colors1)).
	
make_var_list(0, []).
make_var_list(N, [_|T]) :-
	N1 is N - 1,
	make_var_list(N1, T).

X diff Y :-
	##(X, Y).

:- is_predicate(compile_term/1) -> true; assert(compile_term(_)).

color([],_).
color([H|T],Colors):- 
	member(H,Colors),
	rotate(Colors,Colors1),
	(stop -> exit_block(abort) ; color(T,Colors1)).

rotate([A,B,C,D],[B,C,D,A]).

init([T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,
T19,T20,T21,T22,T23,T24,T25,T26,T27,T28,T29,
T30,T31,T32,T33,T34,T35,T36,T37,T38,T39,T40,T41,T42,T43,T44,T45,
T46,T47,T48,T49,T50,T51,T52,T53,T54,T55,T56,T57,T58,T59,T60,T61,
T62,T63,T64,T65,T66,T67,T68,T69,T70,T71,T72,T73,T74,T75,T76,T77,
T78,T79,T80,T81,T82,T83,T84,T85,T86,T87,T88,T89,T90,T91,T92,T93,
T94,T95,T96,T97,T98,T99,T100,T101,T102,T103,T104,T105,T106,T107,T108,
T109,T110]):-
T2 diff T1,T3 diff T1,T4 diff T1,T5 diff T1,T6 diff T1,T7 diff T1,T8 diff T1,T9 diff T1,T10 diff T1,
T11 diff T1,T20 diff T1,T73 diff T1,T109 diff T1,T110 diff T1,T3 diff T2,T12 diff T2,T57 diff T2,
T102 diff T2,T103 diff T2,T104 diff T2,T105 diff T2,T106 diff T2,T110 diff T2,T4 diff T3,
T12 diff T3,T13 diff T3,T5 diff T4,T13 diff T4,T14 diff T4,T6 diff T5,T14 diff T5,T15 diff T5,T7 diff T6,
T15 diff T6,T16 diff T6,T8 diff T7,T16 diff T7,T17 diff T7,T9 diff T8,T17 diff T8,
T18 diff T8,T10 diff T9,T18 diff T9,T19 diff T9,T11 diff T10,T19 diff T10,T20 diff T10,
T20 diff T11,T13 diff T12,T21 diff T12,T57 diff T12,T58 diff T12,T14 diff T13,T21 diff T13,
T22 diff T13,T15 diff T14,T22 diff T14,T23 diff T14,T16 diff T15,T23 diff T15,T24 diff T15,
T17 diff T16,T24 diff T16,T25 diff T16,T18 diff T17,T25 diff T17,T26 diff T17,T19 diff T18,
T26 diff T18,T27 diff T18,T20 diff T19,T27 diff T19,T28 diff T19,T28 diff T20,T73 diff T20,
T22 diff T21,T29 diff T21,T58 diff T21,T59 diff T21,T23 diff T22,T29 diff T22,T30 diff T22,
T24 diff T23,T30 diff T23,T31 diff T23,T25 diff T24,T31 diff T24,T32 diff T24,T26 diff T25,
T32 diff T25,T33 diff T25,T27 diff T26,T33 diff T26,T34 diff T26,T28 diff T27,T34 diff T27,
T35 diff T27,T35 diff T28,T72 diff T28,T73 diff T28,T30 diff T29,T36 diff T29,T59 diff T29,
T60 diff T29,T31 diff T30,T36 diff T30,T37 diff T30,T32 diff T31,T37 diff T31,T38 diff T31,
T33 diff T32,T38 diff T32,T39 diff T32,T34 diff T33,T39 diff T33,T40 diff T33,T35 diff T34,
T40 diff T34,T41 diff T34,T41 diff T35,T71 diff T35,T72 diff T35,T37 diff T36,T42 diff T36,
T60 diff T36,T61 diff T36,T38 diff T37,T42 diff T37,T43 diff T37,T39 diff T38,T43 diff T38,
T44 diff T38,T40 diff T39,T44 diff T39,T45 diff T39,T41 diff T40,T45 diff T40,T46 diff T40,
T46 diff T41,T70 diff T41,T71 diff T41,T43 diff T42,T47 diff T42,T61 diff T42,T62 diff T42,
T44 diff T43,T47 diff T43,T48 diff T43,T45 diff T44,T48 diff T44,T49 diff T44,T46 diff T45,
T49 diff T45,T50 diff T45,T50 diff T46,T69 diff T46,T70 diff T46,T48 diff T47,T51 diff T47,
T62 diff T47,T63 diff T47,T49 diff T48,T51 diff T48,T52 diff T48,T50 diff T49,T52 diff T49,
T53 diff T49,T53 diff T50,T68 diff T50,T69 diff T50,T52 diff T51,T54 diff T51,T63 diff T51,
T64 diff T51,T53 diff T52,T54 diff T52,T55 diff T52,T55 diff T53,T67 diff T53,T68 diff T53,
T55 diff T54,T56 diff T54,T64 diff T54,T65 diff T54,T56 diff T55,T66 diff T55,T67 diff T55,
T65 diff T56,T66 diff T56,T58 diff T57,T102 diff T57,T59 diff T58,T95 diff T58,T102 diff T58,
T60 diff T59,T89 diff T59,T95 diff T59,T61 diff T60,T84 diff T60,T89 diff T60,T62 diff T61,
T80 diff T61,T84 diff T61,T63 diff T62,T77 diff T62,T80 diff T62,T64 diff T63,T75 diff T63,
T77 diff T63,T65 diff T64,T74 diff T64,T75 diff T64,T66 diff T65,T74 diff T65,T67 diff T66,
T74 diff T66,T68 diff T67,T74 diff T67,T76 diff T67,T69 diff T68,T76 diff T68,T79 diff T68,
T70 diff T69,T79 diff T69,T83 diff T69,T71 diff T70,T83 diff T70,T88 diff T70,T72 diff T71,
T88 diff T71,T94 diff T71,T73 diff T72,T94 diff T72,T101 diff T72,T101 diff T73,T109 diff T73,
T75 diff T74,T76 diff T74,T76 diff T75,T77 diff T75,T78 diff T75,T78 diff T76,T79 diff T76,
T78 diff T77,T80 diff T77,T81 diff T77,T79 diff T78,T81 diff T78,T82 diff T78,T82 diff T79,
T83 diff T79,T81 diff T80,T84 diff T80,T85 diff T80,T82 diff T81,T85 diff T81,T86 diff T81,
T83 diff T82,T86 diff T82,T87 diff T82,T87 diff T83,T88 diff T83,T85 diff T84,T89 diff T84,
T90 diff T84,T86 diff T85,T90 diff T85,T91 diff T85,T87 diff T86,T91 diff T86,T92 diff T86,
T88 diff T87,T92 diff T87,T93 diff T87,T93 diff T88,T94 diff T88,T90 diff T89,T95 diff T89,
T96 diff T89,T91 diff T90,T96 diff T90,T97 diff T90,T92 diff T91,T97 diff T91,T98 diff T91,
T93 diff T92,T98 diff T92,T99 diff T92,T94 diff T93,T99 diff T93,T100 diff T93,T100 diff T94,
T101 diff T94,T96 diff T95,T102 diff T95,T103 diff T95,T97 diff T96,T103 diff T96,T104 diff T96,
T98 diff T97,T104 diff T97,T105 diff T97,T99 diff T98,T105 diff T98,T106 diff T98,T100 diff T99,
T106 diff T99,T107 diff T99,T101 diff T100,T107 diff T100,T108 diff T100,T108 diff T101,
T109 diff T101,T103 diff T102,T104 diff T103,T105 diff T104,T106 diff T105,T107 diff T106,
T110 diff T106,T108 diff T107,T110 diff T107,T109 diff T108,T110 diff T108,T110 diff T109.

draw_map :-
	clear_view_surface(1),
	text_font_index(0),
	text_color(1),
	interior_style(1,0),
	fill_color(0),
	draw_map0.

draw_colors(
[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,
T19,T20,T21,T22,T23,T24,T25,T26,T27,T28,T29,
T30,T31,T32,T33,T34,T35,T36,T37,T38,T39,T40,T41,T42,T43,T44,T45,
T46,T47,T48,T49,T50,T51,T52,T53,T54,T55,T56,T57,T58,T59,T60,T61,
T62,T63,T64,T65,T66,T67,T68,T69,T70,T71,T72,T73,T74,T75,T76,T77,
T78,T79,T80,T81,T82,T83,T84,T85,T86,T87,T88,T89,T90,T91,T92,T93,
T94,T95,T96,T97,T98,T99,T100,T101,T102,T103,T104,T105,T106,T107,T108,
T109,T110]) :-
	country(T1,1,1,43,2),
	country(T1,41,1,43,22),
	country(T1,38,12,43,22),
	country(T1,37,21,39,22),
	country(T2,1,2,5,3),
	country(T2,1,2,3,23),
	country(T2,1,22,23,23),
	country(T3,5,2,9,3),
	country(T4,9,2,13,3),
	country(T5,13,2,17,3),
	country(T6,17,2,21,3),
	country(T7,21,2,25,3),
	country(T8,25,2,29,3),
	country(T9,29,2,33,3),
	country(T10,33,2,37,3),
	country_only(T11,37,2,41,3),
	country(T11,39,2,41,12),
	country(T12,3,3,7,4),
	country(T12,3,3,5,12),
	country(T13,7,3,11,4),
	country(T14,11,3,15,4),
	country(T15,15,3,19,4),
	country(T16,19,3,23,4),
	country(T17,23,3,27,4),
	country(T18,27,3,31,4),
	country(T19,31,3,35,4),
	country_only(T20,35,3,39,4),
	country(T20,37,3,39,12),
	country(T21,5,4,9,5),
	country(T21,5,4,7,12),
	country(T22,9,4,13,5),
	country(T23,13,4,17,5),
	country(T24,17,4,21,5),
	country(T25,21,4,25,5),
	country(T26,25,4,29,5),
	country(T27,29,4,33,5),
	country_only(T28,33,4,37,5),
	country(T28,35,4,37,12),
	country(T29,7,5,11,6),
	country(T29,7,5,9,12),
	country(T30,11,5,15,6),
	country(T31,15,5,19,6),
	country(T32,19,5,23,6),
	country(T33,23,5,27,6),
	country(T34,27,5,31,6),
	country_only(T35,31,5,35,6),
	country(T35,33,5,35,12),
	country(T36,9,6,13,7),
	country(T36,9,6,11,12),
	country(T37,13,6,17,7),
	country(T38,17,6,21,7),
	country(T39,21,6,25,7),
	country(T40,25,6,29,7),
	country_only(T41,29,6,33,7),
	country(T41,31,6,33,12),
	country(T42,11,7,15,8),
	country(T42,11,7,13,12),
	country(T43,15,7,19,8),
	country(T44,19,7,23,8),
	country(T45,23,7,27,8),
	country_only(T46,27,7,31,8),
	country(T46,29,7,31,12),
	country(T47,13,8,17,9),
	country(T47,13,8,15,12),
	country(T48,17,8,21,9),
	country(T49,21,8,25,9),
	country_only(T50,25,8,29,9),
	country(T50,27,8,29,12),
	country(T51,15,9,19,10),
	country(T51,15,9,17,12),
	country(T52,19,9,23,10),
	country_only(T53,23,9,27,10),
	country(T53,25,9,27,12),
	country(T54,17,10,21,11),
	country(T54,17,10,19,12),
	country_only(T55,21,10,25,11),
	country(T55,23,10,25,12),
	country(T56,19,11,23,13),
	country_only(T57,3,12,4,22),
	country(T57,3,21,5,22),
	country_only(T58,4,12,6,21),
	country(T58,4,20,8,21),
	country_only(T59,6,12,8,20),
	country(T59,6,19,10,20),
	country_only(T60,8,12,10,19),
	country(T60,8,18,12,19),
	country_only(T61,10,12,12,18),
	country(T61,10,17,14,18),
	country_only(T62,12,12,14,17),
	country(T62,12,16,16,17),
	country_only(T63,14,12,16,16),
	country(T63,14,15,18,16),
	country_only(T64,16,12,18,15),
	country(T64,16,14,20,15),
	country_only(T65,18,12,19,14),
	country(T65,18,13,21,14),

	country_only(T66,23,12,24,14),
	country(T66,21,13,24,14),
	country_only(T67,24,12,26,15),
	country(T67,23,14,26,15),
	country_only(T68,26,12,28,16),
	country(T68,24,15,28,16),
	country_only(T69,28,12,30,17),
	country(T69,26,16,30,17),
	country_only(T70,30,12,32,18),
	country(T70,29,17,32,18),
	country_only(T71,32,12,34,19),
	country(T71,31,18,34,19),
	country_only(T72,34,12,36,20),
	country(T72,33,19,36,20),
	country_only(T73,36,12,38,21),
	country(T73,35,20,38,21),

	country(T74,20,14,23,15),
	country(T75,18,15,21,16),
	country(T76,21,15,24,16),
	country(T77,16,16,19,17),
	country(T78,19,16,23,17),
	country(T79,23,16,26,17),
	country(T80,14,17,17,18),
	country(T81,17,17,21,18),
	country(T82,21,17,25,18),
	country(T83,25,17,29,18),
	country(T84,12,18,15,19),
	country(T85,15,18,19,19),
	country(T86,19,18,23,19),
	country(T87,23,18,27,19),
	country(T88,27,18,31,19),
	country(T89,10,19,13,20),
	country(T90,13,19,17,20),
	country(T91,17,19,21,20),
	country(T92,21,19,25,20),
	country(T93,25,19,29,20),
	country(T94,29,19,33,20),
	country(T95,8,20,11,21),
	country(T96,11,20,15,21),
	country(T97,15,20,19,21),
	country(T98,19,20,23,21),
	country(T99,23,20,27,21),
	country(T100,27,20,31,21),
	country(T101,31,20,35,21),
	country(T102,5,21,9,22),
	country(T103,9,21,13,22),
	country(T104,13,21,17,22),
	country(T105,17,21,21,22),
	country(T106,21,21,25,22),
	country(T107,25,21,29,22),
	country(T108,29,21,33,22),
	country(T109,33,21,37,22),
	country(T110,23,22,43,23).

draw_map0 :-
	rect(1,1,43,2),
	rect(41,1,43,22),
	rect(38,12,43,22),
	rect(37,21,39,22),
	rect(1,2,5,3),
	rect(1,2,3,23),
	rect(1,22,23,23),
	rect(5,2,9,3),
	rect(9,2,13,3),
	rect(13,2,17,3),
	rect(17,2,21,3),
	rect(21,2,25,3),
	rect(25,2,29,3),
	rect(29,2,33,3),
	rect(33,2,37,3),
	rect(37,2,41,3),
	rect(39,2,41,12),
	rect(3,3,7,4),
	rect(3,3,5,12),
	rect(7,3,11,4),
	rect(11,3,15,4),
	rect(15,3,19,4),
	rect(19,3,23,4),
	rect(23,3,27,4),
	rect(27,3,31,4),
	rect(31,3,35,4),
	rect(35,3,39,4),
	rect(37,3,39,12),
	rect(5,4,9,5),
	rect(5,4,7,12),
	rect(9,4,13,5),
	rect(13,4,17,5),
	rect(17,4,21,5),
	rect(21,4,25,5),
	rect(25,4,29,5),
	rect(29,4,33,5),
	rect(33,4,37,5),
	rect(35,4,37,12),
	rect(7,5,11,6),
	rect(7,5,9,12),
	rect(11,5,15,6),
	rect(15,5,19,6),
	rect(19,5,23,6),
	rect(23,5,27,6),
	rect(27,5,31,6),
	rect(31,5,35,6),
	rect(33,5,35,12),
	rect(9,6,13,7),
	rect(9,6,11,12),
	rect(13,6,17,7),
	rect(17,6,21,7),
	rect(21,6,25,7),
	rect(25,6,29,7),
	rect(29,6,33,7),
	rect(31,6,33,12),
	rect(11,7,15,8),
	rect(11,7,13,12),
	rect(15,7,19,8),
	rect(19,7,23,8),
	rect(23,7,27,8),
	rect(27,7,31,8),
	rect(29,7,31,12),
	rect(13,8,17,9),
	rect(13,8,15,12),
	rect(17,8,21,9),
	rect(21,8,25,9),
	rect(25,8,29,9),
	rect(27,8,29,12),
	rect(15,9,19,10),
	rect(15,9,17,12),
	rect(19,9,23,10),
	rect(23,9,27,10),
	rect(25,9,27,12),
	rect(17,10,21,11),
	rect(17,10,19,12),
	rect(21,10,25,11),
	rect(23,10,25,12),
	rect(19,11,23,13),
	rect(3,12,4,22),
	rect(3,21,5,22),
	rect(4,12,6,21),
	rect(4,20,8,21),
	rect(6,12,8,20),
	rect(6,19,10,20),
	rect(8,12,10,19),
	rect(8,18,12,19),
	rect(10,12,12,18),
	rect(10,17,14,18),
	rect(12,12,14,17),
	rect(12,16,16,17),
	rect(14,12,16,16),
	rect(14,15,18,16),
	rect(16,12,18,15),
	rect(16,14,20,15),
	rect(18,12,19,14),
	rect(18,13,21,14),
	rect(23,12,24,14),
	rect(21,13,24,14),
	rect(24,12,26,15),
	rect(23,14,26,15),
	rect(26,12,28,16),
	rect(24,15,28,16),
	rect(28,12,30,17),
	rect(26,16,30,17),
	rect(30,12,32,18),
	rect(29,17,32,18),
	rect(32,12,34,19),
	rect(31,18,34,19),
	rect(34,12,36,20),
	rect(33,19,36,20),
	rect(36,12,38,21),
	rect(35,20,38,21),
	rect(20,14,23,15),
	rect(18,15,21,16),
	rect(21,15,24,16),
	rect(16,16,19,17),
	rect(19,16,23,17),
	rect(23,16,26,17),
	rect(14,17,17,18),
	rect(17,17,21,18),
	rect(21,17,25,18),
	rect(25,17,29,18),
	rect(12,18,15,19),
	rect(15,18,19,19),
	rect(19,18,23,19),
	rect(23,18,27,19),
	rect(27,18,31,19),
	rect(10,19,13,20),
	rect(13,19,17,20),
	rect(17,19,21,20),
	rect(21,19,25,20),
	rect(25,19,29,20),
	rect(29,19,33,20),
	rect(8,20,11,21),
	rect(11,20,15,21),
	rect(15,20,19,21),
	rect(19,20,23,21),
	rect(23,20,27,21),
	rect(27,20,31,21),
	rect(31,20,35,21),
	rect(5,21,9,22),
	rect(9,21,13,22),
	rect(13,21,17,22),
	rect(17,21,21,22),
	rect(21,21,25,22),
	rect(25,21,29,22),
	rect(29,21,33,22),
	rect(33,21,37,22),
	rect(23,22,43,23).

rect(X,Y,X1,Y1) :-
	Xd is 10*X+2,
	Yd is 10*Y+2,
	X1d is 10*X1-2,
	Y1d is 10*Y1-2,
	rectangle(Xd,Yd,X1d,Y1d).

country(X, A, B, C, D) :-
	(getenv("COLOUR", _) ->
		country_col(X, A, B, C, D)
	;
		country_mono(X, A, B, C, D)
	).

country_only(X, A, B, C, D) :-
	(getenv("COLOUR", _) ->
		country_col(X, A, B, C, D)
	;
		true
	).

delay country_mono(X,_,_,_,_) if var(X).
country_mono(X,A,B,C,D):- 
	text_color(1),
	Ad is 10*A + 4,
	Bd is 10*B + 3,
	text(Ad,Bd,X).
country_mono(X,A,B,C,D):- 
	text_color(0),
	Ad is 10*A + 4,
	Bd is 10*B + 3,
	text(Ad,Bd,X),
	fail.

delay country_col(X,_,_,_,_) if var(X).
country_col(X,A,B,C,D):- 
	mapping(X,Color),
	fill_color(Color),
	Ad is 10*A + 2,
	Bd is 10*B + 1,
	Cd is 10*C - 0,
	Dd is 10*D - 1,
	rectangle(Ad,Bd,Cd,Dd).
country_col(X,A,B,C,D):- 
	fill_color(0),
	Ad is 10*A + 2,
	Bd is 10*B + 1,
	Cd is 10*C - 0,
	Dd is 10*D - 1,
	rectangle(Ad,Bd,Cd,Dd),
	fail.

mapping(1,2).
mapping(2,21).
mapping(3,58).
mapping(4,80).


%-------------------------------------------------------
% dialog window
%-------------------------------------------------------

:- make_callback(map_dialog_pressed/2).
:- make_callback(map_constraints_selected/2).

make_dialog :-
	new_dialog(@map_dialog, 'Map Colouring Demo', dialog_panel),
	send(@map_dialog, size, size(200, 110)),
	send(@map_dialog, open, point(810, 0)).

dialog_panel(@map_run, button('Run', map_dialog_pressed), append, []).
dialog_panel(@map_stop, button('Abort', map_dialog_pressed), right, []).
dialog_panel(@map_quit, button('Quit', map_dialog_pressed), right, []).
dialog_panel(_, label(none, ''), below, []).
dialog_panel(_, menu('Solving mode', cycle, cascade(0, map_constraints_selected, 0),
	[plain, delay, domains]), below, []).

map_constraints_selected(_, Mode) :-
	setval(mode, Mode).

map_dialog_pressed(_, 'Run') :-
	retract_all(stop),
	send(@pce, async, 0),
	send(@map_run, [greyed:on, active:off]),
	send(@map_quit, [greyed:on, active:off]),
	send(@map_stop, [greyed:off, active:on]),
	block(place_map, stopped, true),
	send(@map_quit, [greyed:off, active:on]),
	send(@map_stop, [greyed:on, active:off]),
	send(@map_run, [greyed:off, active:on]).
map_dialog_pressed(_, 'Abort') :-
	send(@map_run, [greyed:off, active:on]),
	send(@map_quit, [greyed:off, active:on]),
	send(@map_stop, [greyed:on, active:off]),
	assert(stop).
map_dialog_pressed(_, 'Quit') :-
	close_map,
	abort.

:-      getval(flags, flags(DC, VN)),
	set_flag(debug_compile, DC),
	set_flag(variable_names, VN),
	erase_array(flags).
