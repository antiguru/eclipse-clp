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
% Version:	$Id: k_2d.pl,v 1.1 2006/09/23 01:55:26 snovello Exp $
% ----------------------------------------------------------------------

/*****************************************************************************
* IDENTIFICATION	k_2d.pl
* DESCRIPTION		prolog code for x11 xwip interface
* CONTENTS		this is only for x11 systems
* AUTHOR		Philip Kay ECRC GmbH
			Updated to be used as a library with generic XWIP
			(Micha)
*****************************************************************************/


/*****************************************************************************
* compiled declarations
*****************************************************************************/
:- begin_module(kegi).
:- system_debug.

:- dynamic
	window_fact/18,
	colour_fact/2.

:- export
	xwip_mode/0,
	destroy_window/0,
	destroy_window/1,
	current_window/1.

:- export
	open_2d/4, 
	close_2d/0, 
	circular_arc_center/7, 
	circular_arc_center_close/7, 
	circular_arc_3pt/6, 
	circular_arc_3pt_close/6, 
	cell_array/6, 
	pixel_array/2, 
	bitmap/3, 
	load_bitmap/1,
	line/4, 
	polyline/1, 
	disjoint_polyline/1, 
	polymarker/1, 
	polygon/1, 
	partial_polygon/2, 
	color_rgb/4, 
	rectangle/4, 
	ellipse/4, 
	vdc_extent/4, 
	circle/3, 
	text/3, 
	append_text/1, 
	marker/2, 
	interior_style/2, 
	clear_view_surface/1, 
	text_precision/1, 
	text_font_index/1, 
	character_height/1, 
	text_color/1, 
	marker_size/1, 
	marker_color/1, 
	fill_color/1, 
	perimeter_type/1, 
	perimeter_width/1, 
	perimeter_color/1, 
	line_type/1, 
	line_end_style/1, 
	line_width/1, 
	line_color/1, 
	marker_type/1, 
	reset_to_defaults/0, 
	mouse/3, 
	mouse1/6, 
	get_bitmap/2, 
	close_window/0, 
	open_window/0, 
	move_window/0, 
	move_window/2, 
	stretch_window/0, 
	stretch_window/2, 
	expose_window/0, 
	hide_window/0, 
	refresh_window/0, 
	stretch_char_window/2, 
	set_tool_header/1, 
	set_icon/1, 
	set_icon_label/1,
	set_drawing_mode/1.
:- export
	color_rgb_xor/5.

:- make_local_array(text).

:- import
	printf_/5,
	symbol_address/2,
	trimcore/0
    from sepia_kernel.

/*****************************************************************************
*
*  Loading XWIP
*
*****************************************************************************/

%
% ECLiPSe XWIP Package
%

% version
version(V) :-
	recordz(ext_version, V).

banner(_, SepiaMessage) :-
	(recorded(ext_version, V) ->
	    printf(toplevel_output, "%w%n%w%n%b", [SepiaMessage, V])
	;
	    printf(toplevel_output, "%w%b", [SepiaMessage])
	).

:- set_error_handler(164, banner/2).

:- tool(format/3, format_body/4).
format(F, A) :-
	format(output, F, A).

format_body(Stream, Format, ArgList, Module) :-
        printf_(Stream, Format, ArgList, Module, 0'~, ErrF, ErrL, Res),
	(Res = 0 ->
                true
        ;
                error(Res, format(Stream, ErrF, ErrL), Module)
	).

:- (symbol_address(p_pxOpenConnection, _) ->
	true
    ;
	lib(foreign),

	get_flag(installation_directory, Dir),
	compile_term([(
	    xwip_compile(File) :-
		concat_string([Dir, "/lib_graphic/xwip/", File], Path),
		compile(Path)
	    )]),

% load foreign functions
	xwip_compile(ff),

	get_flag(installation_directory, Dir),
	get_flag(hostarch, Arch),
	get_flag(object_suffix, O),
	(O = "o" ->
	    concat_string([Dir, "/lib_graphic/", Arch, "/xwip.o ",
		'-L/usr/local/lib/X11 -lX11'], OpLoad)
	;
	    concat_string([Dir, "/lib_graphic/", Arch, "/xwip.so"], OpLoad)
	),
	load(OpLoad),
	declare_externals(kegi),

	abolish((foreign_file/2,foreign/3)),

% load tables
	xwip_compile(table),
	xwip_compile(cfont),
	xwip_compile(xatom),

% load predicates
	xwip_compile(util),
	xwip_compile(pred),

% and add our changes
	xwip_compile(extras),

% cleanup
	garbage_collect,
	trimcore,

	set_flag(syntax_option, nl_in_quotes),
	xwip_compile(version),
	set_flag(syntax_option, not nl_in_quotes)
    ).

/*****************************************************************************
*  2d output start preds predicates for cgi and x11
*
*  open_2d(+X1,+Y1,+X2,+Y2)
*  open_2d1(+X1,+Y1,+X2,+Y2)
*    +X1 +Y1  view surface bottom left
*    +X2 +Y2  view surface relative coordinate top right
*
*****************************************************************************/
open_2d(X1, Y1, X2, Y2):-
	open_2d_x11(X1, Y1, X2, Y2).

open_2d1(X1, Y1, X2, Y2):-
	writeln("warning: open_2d1/4 is not supported with X11 interface"),
	open_2d_x11(X1, Y1, X2, Y2).

open_2d_x11(X1, Y1, X2, Y2):-
	open_x11(X1, Y1, X2, Y2).

get_tool_position(Toolposition) :-
	(is_predicate(window_data/4) ->
		window_data(X, Y, W, H)
	;
		X is 502, Y is 0,
		W is 650, H is 650
	),
	concat_atom([X, ", ", Y, ", ", W, ", ", H, ", 502, 0, 64, 64, 0"], Pos),
	atom_string(Pos, Toolposition).

/*****************************************************************************
* opening and closing operations
*****************************************************************************/
open_x11(Lx, Ly, Ux, Uy):-
	xConnections(L),
	open_connection(L, C),
	xSynchronize(C, xTrue),
	open_window(C, Lx, Ly, Ux, Uy, _Win),
	xSynchronize(C, xFalse), !.

/* The expose event may be sent differently depending on the Server/WM
   and the way the event mask is made:
   xCreate	xSet	Xnews+TWM Xnews+olwm	MITX+twm	MITX+olwm
   +		+	+		-	+		+
   +		-	-		-	+		+
   -		+	+		+	-		-
*/

open_window(C, Lx, Ly, Ux, Uy, Win):-
	getval(window, N),
	concat_atom(['window ', N], Name),
	incval(window),
	(is_predicate(window_data/4) ->
		window_data(X, Y, W, H)
	;
		X is 0, Y is 0,
		W is 500, H is 500
	),
	xCreateFontCursor(C, arrow, Cursor), 
	xQueryConnection(C, [xDefaultScreen(S)]),
	xQueryScreen(S, [xRootWindow(R), xDefaultColormap(CM)]),
	colour_fact(0, Back),
	colour_fact(1, Fore),
	Ops0 = [xBackingStore(xAlways), xBackPixel(Back),
		xBorderPixel(Fore), xColormap(CM),
		xBitGravity(xSouthWest),
		xCursor(Cursor)
	    ],
	(xQueryConnection(C,
	    [xServerVendor('X11/NeWS - Sun Microsystems Inc.')]) ->
		Ops = Ops0
	;
		Ops = [xEventMask([xExposure])|Ops0]
	),
	xCreateWindow(C, R, X, Y, W, H, 1, xCopyFromParent, xInputOutput,
	    xCopyFromParent, Ops, Win),
	xSetWindow(C, Win, [xEventMask([xExposure])]),
	xSetStandardProperties(C, Win, Name, Name, [], [], []),
	xMapWindow(C, Win),
	xFlush(C),
	open_gcs(C, Win, Back, Fore, Gline, Gsolid, Gfill, Gmarker, Gfont, Gbitmap),
	F is min((W / (Ux - Lx)), (H / (Uy - Ly))),
	assert(window_fact(N, C, S, CM, Win, W, H, Lx, Ly, Ux, Uy, F, Gline, Gsolid, Gfill, Gmarker, Gfont, Gbitmap)),
	setval(current, N),
	xNextEvent(C, xTrue, _),
	xSetWindow(C, Win, [xEventMask([])]).

open_connection([C|_], C):- !.
open_connection([], C):-
	getenv("DISPLAY", D),
	atom_string(Display, D),
	xOpenConnection(Display, C),
	retract_all(window_fact(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)),
	retract_all(colour_fact(_, _)),
	open_colours(C),
	open_fonts(C),
	setval(window, 1).

open_colours(C):-
	xQueryConnection(C, [xDefaultScreen(S)]),
	xQueryScreen(S, [xRootVisual(V), xDefaultColormap(CM)]),
	xQueryVisual(V, [xColormapEntries(E)]),
	colour_cgi(N, R, G, B),
	assert(colour_fact(N, 0)),
	open_colour(E, N, C, CM, R, G, B, Pixel),
	retract(colour_fact(N, 0)),
	assert(colour_fact(N, Pixel)),
	fail.
open_colours(_).

open_colour(2, 0, C, CM, _, _, _, Pixel):-
	!,
	xRequestColor(C, CM, xColor(65025, 65025, 65025), xCell(Pixel, xColor(_, _, _))).
open_colour(2, _, C, CM, _, _, _, Pixel):-
	!,
	xRequestColor(C, CM, xColor(0, 0, 0), xCell(Pixel, xColor(_, _, _))).
open_colour(_, _, C, CM, R, G, B, Pixel):-
	xRequestColor(C, CM, xColor(R, G, B), xCell(Pixel, xColor(_, _, _))).

open_fonts(C):-
	xOpenFont(C, 'fixed', F1),
	xOpenFont(C, '6x10', F2),
	xOpenFont(C, '8x13', F3),
	xOpenFont(C, '9x15', F4),
	xOpenFont(C, 'cursor', F5),
	record(fonts, [F1, F2, F3, F4, F5, F5]).

open_gcs(C, Win, Back, Fore, Gline, Gsolid, Gfill, Gmarker, Gfont, Gbitmap):-
	graphic_context(line, Back, Fore, L1),
	graphic_context(solid, Back, Fore, L2),
	graphic_context(fill, Back, Fore, L3),
	graphic_context(marker, Back, Fore, L4),
	graphic_context(font, Back, Fore, L5),
	graphic_context(bitmap, Back, Fore, L6),
	xCreateGC(C, Win, L1, Gline),
	xCreateGC(C, Win, L2, Gsolid),
	xCreateGC(C, Win, L3, Gfill),
	xCreateGC(C, Win, L4, Gmarker),
	xCreateGC(C, Win, L5, Gfont),
	xCreateGC(C, Win, L6, Gbitmap),
	erase_all(solid), record(solid, [0]),
	erase_all(marker), record(marker, [0, 10]).

graphic_context(line, Back, Fore, [xBackground(Back), xForeground(Fore)]).
graphic_context(solid, Back, Fore, [xFillRule(xTrue), xArcMode(xTrue), xBackground(Back), xForeground(Fore)]).
graphic_context(fill, Back, Fore, [xArcMode(xTrue), xFillRule(xTrue), xBackground(Back), xForeground(Fore)]).
graphic_context(marker, Back, Fore, [xCapStyle(xRound), xJoinStyle(xRound), xBackground(Back), xForeground(Fore)]).
graphic_context(bitmap, Back, Fore, [xBackground(Back), xForeground(Fore)]).
graphic_context(font, Back, Fore, [xFont(F), xBackground(Back), xForeground(Fore)]):-
	recorded(fonts, [F, _, _, _, _, _]), !.

close_2d:-
	xConnections(L),
	not(atom(L)),
	close_connections(L),
	erase_all(fonts),
	erase_all(window).

close_connections([]).
close_connections([C|T]):-
	xCloseConnection(C),
	close_connections(T).

reset_to_defaults:-
	getval(current, N),
	window_fact(N, C, _, _, _, _, _, _, _, _, _, _, Gline, Gsolid, Gfill, Gmarker, Gfont, Gbitmap), !,
	colour_fact(0, Back),
	colour_fact(1, Fore),
	graphic_context(line, Back, Fore, L1),
	graphic_context(solid, Back, Fore, L2),
	graphic_context(fill, Back, Fore, L3),
	graphic_context(marker, Back, Fore, L4),
	graphic_context(font, Back, Fore, L5),
	graphic_context(bitmap, Back, Fore, L6),
	xSetGC(C, Gline, L1),
	xSetGC(C, Gsolid, L2),
	xSetGC(C, Gfill, L3),
	xSetGC(C, Gmarker, L4),
	xSetGC(C, Gfont, L5),
	xSetGC(C, Gbitmap, L6),
	erase_all(solid),
	record(solid, [0]),
	erase_all(marker),
	record(marker, [0, 10]).

/*****************************************************************************
* colour operations
*****************************************************************************/
color_rgb(Num, R, G, B):-
	colour_fact(Num, Old),
	getval(current, N),
	window_fact(N, C, S, CM, _, _, _, _, _, _, _, _, _, _, _, _, _, _), !,
	xQueryScreen(S, [xRootVisual(V)]),
	xQueryVisual(V, [xColormapEntries(E)]),
	R1 is R * 255, G1 is G * 255, B1 is B * 255,
	xFreeColors(C, CM, [Old], 0),
	open_colour(E, Num, C, CM, R1, G1, B1, Pixel),
	retract(colour_fact(Num, _)),
	assert(colour_fact(Num, Pixel)), !.

color_rgb_xor(Num, Xor, R, G, B):-
	colour_fact(Num, Old),
	getval(current, N),
	window_fact(N, C, S, CM, _, _, _, _, _, _, _, _, _, _, _, _, _, _), !,
	xQueryScreen(S, [xRootVisual(V)]),
	xQueryVisual(V, [xColormapEntries(E)]),
	R1 is R * 255, G1 is G * 255, B1 is B * 255,
	xFreeColors(C, CM, [Old], 0),
	open_colour(E, Num, C, CM, R1, G1, B1, Pixel),
	colour_fact(Xor, XorC),
	NumC is xor(XorC, Pixel),
	retract(colour_fact(Num, _)),
	assert(colour_fact(Num, NumC)), !.



line_color(Num):-
	getval(current, N),
	window_fact(N, C, _, _, _, _, _, _, _, _, _, _, Gline, _, _, _, _, _), !,
	set_colour(Num, C, Gline).
	
marker_color(Num):-
	getval(current, N),
	window_fact(N, C, _, _, _, _, _, _, _, _, _, _, _, _, _, Gmarker, _, _), !,
	set_colour(Num, C, Gmarker).

perimeter_color(Num):-
	getval(current, N),
	window_fact(N, C, _, _, _, _, _, _, _, _, _, _, _, Gsolid, _, _, _, _), !,
	set_colour(Num, C, Gsolid).

text_color(Num):-
	getval(current, N),
	window_fact(N, C, _, _, _, _, _, _, _, _, _, _, _, _, _, _, Gfont, _), !,
	set_colour(Num, C, Gfont).

fill_color(Num):-
	getval(current, N),
	window_fact(N, C, _, _, _, _, _, _, _, _, _, _, _, _, Gfill, _, _, _), !,
	set_colour(Num, C, Gfill).

set_colour(Num, C, GC):-
	colour_fact(Num, P), !,
	xSetGC(C, GC, [xForeground(P)]).

clear_view_surface(Num):-
	colour_fact(Num, P),
	getval(current, N),
	window_fact(N, C, _, _, Win, _, _, _, _, _, _, _, _, _, _, _, _, _), !,
	xSetWindow(C, Win, [xBackPixel(P)]),
	xClearArea(C, Win, 0, 0, 0, 0, xFalse), !,
	xFlush(C).


/*****************************************************************************
* window operations
*****************************************************************************/
current_window(N):-
	var(N),
	getval(current, N).
current_window(N):-
	window_fact(N, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _), !,
	setval(current, N).
current_window(_):-
	writeln('error: please specify a legal window number'),
	fail.

destroy_window:-
	getval(current, N),
	destroy_window(N).

destroy_window(N):-
	window_fact(N, C, _, _, Win, _, _, _, _, _, _, _, _, _, _, _, _, _),
	destroy_window(N, C, Win).

destroy_window(N, C, Win):-
	xSynchronize(C, xTrue),
	xDestroyWindow(C, Win),
	xSynchronize(C, xFalse),
	retract(window_fact(N, C, _, _, Win, _, _, _, _, _, _, _, _, _, _, _, _, _)),
	window_fact(Any, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _),
	setval(current, Any).
destroy_window(_, _, _):-
	close_2d.

set_icon_label(Text):-
	getval(current, N),
	window_fact(N, C, _, _, Win, _, _, _, _, _, _, _, _, _, _, _, _, _), !,
	xSetStandardProperties(C, Win, [], Text, [], [], []), !,
	xFlush(C).

set_tool_header(Text):-
	getval(current, N),
	window_fact(N, C, _, _, Win, _, _, _, _, _, _, _, _, _, _, _, _, _), !,
	xSetStandardProperties(C, Win, Text, [], [], [], []), !,
	xFlush(C).

close_window:-
	getval(current, N),
	window_fact(N, C, _, _, Win, _, _, _, _, _, _, _, _, _, _, _, _, _), !,
	xUnmapWindow(C, Win), !,
	xFlush(C).
	
open_window:-
	getval(current, N),
	window_fact(N, C, _, _, Win, _, _, _, _, _, _, _, _, _, _, _, _, _), !,
	xMapWindow(C, Win), !,
	xFlush(C).

move_window:-
	move_window(0,0).

move_window(X, Y):-
	window_do([xX(X), xY(Y)]).

stretch_char_window(_, _):-
	writeln('warning: stretch_char_window/3 is not supported with X11 interface').

stretch_window:-
	stretch_window(500,500).

stretch_window(Width,Height):-
	getval(current, N),
	window_fact(N, C, S, CM, Win, W, H, Lx, Ly, Ux, Uy, F, Gline, Gsolid, Gfill, Gmarker, Gfont, Gbitmap), !,
	retract(window_fact(N, C, S, CM, Win, W, H, Lx, Ly, Ux, Uy, F, Gline, Gsolid, Gfill, Gmarker, Gfont, Gbitmap)),
	Factor is min((Width / (Ux - Lx)), (Height / (Uy - Ly))),
	assert(window_fact(N, C, S, CM, Win, Width, Height, Lx, Ly, Ux, Uy, Factor, Gline, Gsolid, Gfill, Gmarker, Gfont, Gbitmap)),
	window_do([xWidth(Width), xHeight(Height)]),
	refresh_window.

expose_window:-
	window_do([xStackMode(xAbove)]).
	
hide_window:-
	window_do([xStackMode(xBelow)]).
	
refresh_window:-
	open_window.

set_icon(_):-
	writeln('warning: set_icon/1 is not supported with X11 interface').

window_do(Action):-
	getval(current, N),
	window_fact(N, C, _, _, Win, _, _, _, _, _, _, _, _, _, _, _, _, _), !,
	xSetWindow(C, Win, Action), !,
	xFlush(C).


/*****************************************************************************
* mouse operations
*****************************************************************************/
mouse(T, X, Y):-
	getval(current, N),
	window_fact(N, C, _, _, Win, _, H, Lx, Ly, _, _, F, _, _, _, _, _, _), !,
	xCreateFontCursor(C, pencil, C1),
	xSetWindow(C, Win, [xEventMask([xButtonPress]), xCursor(C1)]),
	xFlush(C),
	xNextEvent(C, xTrue, xEvent(xButtonPress, _, _, _, _, _, _, _, WX, WY, _, _, _, T, _)),
	integer(WX),
	integer(WY),
	convert_mouse(F, Lx, Ly, H, WX, WY, X, Y),
	xCreateFontCursor(C, arrow, C2), 
	xSetWindow(C, Win, [xEventMask([]), xCursor(C2)]),
	xFlush(C), !.

mouse1(_, T, RX1, RY1, RX2, RY2):-
	getval(current, N),
	window_fact(N, C, _, _, Win, _, H, Lx, Ly, _, _, F, _, _, _, _, _, _), !,
	xCreateFontCursor(C, pencil, C1),
	xSetWindow(C, Win, [xEventMask([xButtonPress]), xCursor(C1)]),
	xFlush(C),
	xNextEvent(C, xTrue, xEvent(xButtonPress, _, _, _, _, _, _, _, X1, Y1, _, _, _, T, _)),
	!, integer(X1), integer(Y1),
	convert_mouse(F, Lx, Ly, H, X1, Y1, RX1, RY1),
	xNextEvent(C, xTrue, xEvent(xButtonPress, _, _, _, _, _, _, _, X2, Y2, _, _, _, T, _)),
	!, integer(X2), integer(Y2),
	convert_mouse(F, Lx, Ly, H, X2, Y2, RX2, RY2),
	xCreateFontCursor(C, arrow, C2), 
	xSetWindow(C, Win, [xEventMask([]), xCursor(C2)]),
	xFlush(C), !.


/*****************************************************************************
* bitmap type operations
*****************************************************************************/
bitmap(Width, Height, Values):-
	getval(current, N),
	window_fact(N, C, _, _, Win, _, _, _, _, _, _, _, _, _, _, _, _, _), !,
	xQueryWindow(C, Win, [xVisual(V), xDepth(D)]),
	xCreateImage(C, V, D, xXYPixmap, 0, Width, Height, xDefault, xDefault, I),
	RW is Width - 1,
	RH is Height - 1,
	bitmap_fill(0, 0, RW, RH, Values, I),
	erase_all(bitmap),
	record(bitmap, [I, Width, Height]).

bitmap_fill(_, _, _, _, [], _):- !.
bitmap_fill(W, H, W, H, [P], I):-
	!,
	xSetPixel(I, W, H, P).
bitmap_fill(W, C, W, H, [P|T], I):-
	!,
	xSetPixel(I, W, C, P),
	C1 is C + 1,
	bitmap_fill(0, C1, W, H, T, I).
bitmap_fill(R, C, W, H, [P|T], I):-
	!,
	xSetPixel(I, R, C, P),
	R1 is R + 1,
	bitmap_fill(R1, C, W, H, T, I).

pixel_array(X, Y):-
	getval(current, N),
	window_fact(N, C, _, _, Win, _, H, Lx, Ly, _, _, F, _, _, _, _, _, Gbitmap), !,
	recorded(bitmap, [I, Width, Height]), !,
	convert_coor(F, Lx, Ly, H, X, Y, RX, RY),
	xPutImage(C, Win, Gbitmap, I, 0, 0, RX, RY, Width, Height), !,
	xFlush(C).

cell_array(_, _, _, _, _, _):-
	writeln('warning: cell_array/6 is not supported with X11 interface').

load_bitmap(File) :-
	getval(current, N),
	window_fact(N, C, _, _, Win, _, _, _, _, _, _, _, _, _, _, _, _, GC), !,
	xQueryWindow(C, Win, [xVisual(V), xDepth(_)]),
	pxLoadImage(File, C, V, GC, I, 0),
	xQueryImage(I, [xWidth(Width), xHeight(Height)]),
	erase_all(bitmap),
	record(bitmap, [I, Width, Height]).


/*****************************************************************************
* line type operations
*****************************************************************************/
line(X1, Y1, X2, Y2):-
	getval(current, N),
	window_fact(N, C, _, _, Win, _, H, Lx, Ly, _, _, F, Gline, _, _, _, _, _), !,
	convert_coor(F, Lx, Ly, H, X1, Y1, A1, B1),
	convert_coor(F, Lx, Ly, H, X2, Y2, A2, B2),
	xDrawSegments(C, Win, Gline, [xSegment(A1, B1, A2, B2)]), !,
	xFlush(C).

circular_arc_3pt(X1, Y1, X2, Y2, X3, Y3):-
	circ_arc_3pt(X1, Y1, X2, Y2, X3, Y3, RX, RY, R, A1, A2),
	general_arc_line(RX, RY, R, A1, A2).

circular_arc_centre(X1, Y1, X2, Y2, X3, Y3, R):-
	M1 is X2 - X1,
	M2 is Y2 - Y1,
	arc1(M1, M2, A1),
	M3 is X3 - X1,
	M4 is Y3 - Y1,
	arc1(M3, M4, A2),
	general_arc_line(X1, Y1, R, A1, A2).

general_arc_line(X, Y, R, A1, A2):-
	getval(current, N),
	window_fact(N, C, _, _, Win, _, H, Lx, Ly, _, _, F, Gline, _, _, _, _, _), !,
	X1 is X - R,
	Y1 is Y + R,
	Len is R * 2,
	convert_coor(F, Lx, Ly, H, X1, Y1, RX, RY),
	convert_length(F, Len, RL),
	RA1 is fix(A1 * 64),
	RA2 is fix(A2 * 64),
	xDrawArcs(C, Win, Gline, [xArc(RX, RY, RL, RL, RA1, RA2)]), !,
	xFlush(C).

polyline(L):-
	getval(current, N),
	window_fact(N, C, _, _, Win, _, H, Lx, Ly, _, _, F, Gline, _, _, _, _, _), !,
	walk_polyline(F, Lx, Ly, H, L, PL),
	xDrawLines(C, Win, Gline, PL, xFalse), !,
	xFlush(C).

partial_polygon(L, 0):-
	!,
	polyline(L).
partial_polygon(L, 1):-
	polygon(L).
	
walk_polyline(_, _, _, _, [],[]):- !.
walk_polyline(F, Lx, Ly, H, [H1|[H2|T1]],[xPoint(A1,B2)|T2]):-
	convert_coor(F, Lx, Ly, H, H1,H2,A1,B2),
	walk_polyline(F, Lx, Ly, H, T1,T2).

disjoint_polyline([]):- !.
disjoint_polyline([X1|[Y1|[X2|[Y2|T]]]]):-
	line(X1, Y1, X2, Y2),
	disjoint_polyline(T).

line_width(Wid):-
	Wid >= 0, !,
	getval(current, N),
	window_fact(N, C, _, _, _, _, _, _, _, _, _,_, Gline, _, _, _, _, _), !,
	xSetGC(C, Gline, [xLineWidth(Wid)]).
line_width(Wid):-
	write('error: line width '), write(Wid), writeln(' is not supported'),
	fail.

line_type(T):-
	line_type_data(T, Type, Length), !,
	getval(current, N),
	window_fact(N, C, _, _, _, _, _, _, _, _, _, _, Gline, _, _, _, _, _), !,
	xSetGC(C, Gline, [xLineStyle(Type), xDashLength(Length)]).
line_type(T):-
	write('error: line type '), write(T), writeln(' is not supported'),
	fail.

line_type_data(0, xSolid, 4).
line_type_data(1, xOnOffDash, 4).
line_type_data(2, xDoubleDash, 4).
line_type_data(3, xDoubleDash, 8).
line_type_data(4, xDoubleDash, 12).
line_type_data(5, xDoubleDash, 16).

line_end_style(T):-
	line_endstyle_data(T, Name), !,
	getval(current, N),
	window_fact(N, C, _, _, _, _, _, _, _, _, _,_, Gline, _, _, _, _, _), !,
	xSetGC(C, Gline, [xCapStyle(Name)]).
line_end_style(T):-
	write('error: line end style '), write(T), writeln(' is not supported'),
	fail.

line_endstyle_data(0, xButt).
line_endstyle_data(1, xNotLast).
line_endstyle_data(2, xRound).
line_endstyle_data(3, xProjecting).

set_drawing_mode(Mode) :-
	getval(current, N),
	window_fact(N, C, _, _, _, _, _, _, _, _, _, _, Gline, Gsolid, Gfill, Gmarker, Gfont, Gbitmap), !,
	convert_mode(Mode, T),
	xSetMode(C, Gline, T),
	xSetMode(C, Gsolid, T),
	xSetMode(C, Gfill, T),
	xSetMode(C, Gmarker, T),
	xSetMode(C, Gfont, T),
	xSetMode(C, Gbitmap, T).

% This one is from X.h
convert_mode(and, 1).
convert_mode(replace, 3).
convert_mode(xor, 6).
convert_mode(or, 7).
convert_mode(not, 10).

/*****************************************************************************
* solid type operations
*****************************************************************************/
rectangle(X1, Y1, X2, Y2):-
	getval(current, N),
	window_fact(N, C, _, _, Win, _, H, Lx, Ly, _, _, F, _, Gsolid, Gfill, _, _, _), !,
	SX is min(X1, X2),
	SY is min(Y1, Y2),
	BX is max(X1, X2),
	BY is max(Y1, Y2),
	W1 is BX - SX,
	H1 is BY - SY,
	convert_coor(F, Lx, Ly, H, SX, BY, X, Y),
	convert_length(F, W1, Width),
	convert_length(F, H1, Height),
	recorded(solid, [Mode]), !,
	rectangle_draw(Mode, C, Win, Gsolid, Gfill, N, X, Y, Width, Height),
	xFlush(C).

rectangle_draw(0, C, Win, Gsolid, _, _, X, Y, Width, Height):-
	!,
	xDrawRectangles(C, Win, Gsolid, [xRectangle(X, Y, Width, Height)]), !.
rectangle_draw(1, C, Win, _, Gfill, _, X, Y, Width, Height):-
	xFillRectangles(C, Win, Gfill, [xRectangle(X, Y, Width, Height)]), !.
	
circle(X, Y, R):-
	recorded(solid, [Mode]), !,
	general_arc(Mode, X, Y, R, R, 0, 360).

ellipse(X, Y, R1, R2):-
	recorded(solid, [Mode]), !,
	general_arc(Mode, X, Y, R1, R2, 0, 360).

circular_arc_3pt_close(X1, Y1, X2, Y2, X3, Y3):-
	circ_arc_3pt(X1, Y1, X2, Y2, X3, Y3, RX, RY, R, A1, A2),
	recorded(solid, [Mode]), !,
	arc_draw_lines(Mode, RX, RY, X1, Y1, X3, Y3),
	general_arc(Mode, RX, RY, R, R, A1, A2).

circular_arc_centre_close(X1, Y1, X2, Y2, X3, Y3, R):-
	M1 is X2 - X1,
	M2 is Y2 - Y1,
	arc1(M1, M2, _),
	M3 is X3 - X1,
	M4 is Y3 - Y1,
	arc1(M3, M4, _),
	recorded(solid, [Mode]), !,
	arc_draw_lines(Mode, X1, Y1, X2, Y2, X3, Y3),
	circular_arc_centre(X1, Y1, X2, Y2, X3, Y3, R).

circ_arc_3pt(X1, Y1, X2, Y2, X3, Y3, RX, RY, R, A1, A2):-
	FX2 is X2 - X1,
	FY2 is Y2 - Y1,
	FX3 is X3 - X1,
	FY3 is Y3 - Y1,
	arc_3pt(FX2, FY2, FX3, FY3, CX, CY, R),
	TX1 is 0 - CX,
	TY1 is 0 - CY,
	TX3 is FX3 - CX,
	TY3 is FY3 - CY,
	arc1(TX1, TY1, A1),
	arc1(TX3, TY3, AT),
	A2 is (fix(((360 - A1) + AT)) mod 360) + (((360 - A1) + AT) - fix(((360 - A1) + AT))),
	RX is CX + X1,
	RY is CY + Y1.

arc_draw_lines(1, _, _, _, _, _, _).
arc_draw_lines(0, RX, RY, X1, Y1, X3, Y3):-
	getval(current, N),
	window_fact(N, C, _, _, Win, _, H, Lx, Ly, _, _, F, _, Gsolid, _, _, _, _), !,
	convert_coor(F, Lx, Ly, H, RX, RY, RCX, RCY),
	convert_coor(F, Lx, Ly, H, X1, Y1, RX1, RY1),
	convert_coor(F, Lx, Ly, H, X3, Y3, RX3, RY3),
	xDrawSegments(C, Win, Gsolid, [xSegment(RCX, RCY, RX1, RY1), xSegment(RCX, RCY, RX3, RY3)]).

general_arc(Mode, X, Y, R1, R2, A1, A2):-
	getval(current, N),
	window_fact(N, C, _, _, Win, _, H, Lx, Ly, _, _, F, _, Gsolid, Gfill, _, _, _), !,
	X1 is X - R1,
	Y1 is Y + R2,
	Width is R1 * 2,
	Height is R2 * 2,
	convert_coor(F, Lx, Ly, H, X1, Y1, RX, RY),
	convert_length(F, Width, RW),
	convert_length(F, Height, RH),
	RA1 is fix(A1 * 64),
	RA2 is fix(A2 * 64),
	general_arc_draw(Mode, C, Win, Gsolid, Gfill, N, RX, RY, RW, RH, RA1, RA2),
	xFlush(C).

general_arc_draw(0, C, Win, Gsolid, _, _, RX, RY, RW, RH, A1, A2):-
	!,
	xDrawArcs(C, Win, Gsolid, [xArc(RX, RY, RW, RH, A1, A2)]), !.
general_arc_draw(1, C, Win, _, Gfill, _, RX, RY, RW, RH, A1, A2):-
	xFillArcs(C, Win, Gfill, [xArc(RX, RY, RW, RH, A1, A2)]), !.

arc_3pt(X1, Y1, _, _, _, _, _):-
	X1 =:= 0,
	Y1 =:= 0, !, arc_error.
arc_3pt(X1, _, X2, _, _, _, _):-
	X1 =:= 0,
	X2 =:= 0, !, arc_error.
arc_3pt(_, Y1, _, Y2, _, _, _):-
	Y1 =:= 0,
	Y2 =:= 0, !,
	arc_error.
arc_3pt(_, _, X2, Y2, _, _, _):-
	X2 =:= 0,
	Y2 =:= 0, !, arc_error.
arc_3pt(X1, Y1, X2, Y2, X, Y, R):-
	X1 =:= 0, !,
	Y is (Y1 * Y1) / (2 * Y1),
	X is (((X2 * X2) + (Y2 * Y2)) - (2 * Y2 * Y)) / (2 * X2),
	R is sqrt((X * X) + (Y * Y)).
arc_3pt(X1, Y1, X2, Y2, X, Y, R):-
	Y1 =:= 0, !,
	X is (X1 * X1) / (2 * X1),
	Y is (((Y2 * Y2) + (X2 * X2)) - (2 * X2 * X)) / (2 * Y2),
	R is sqrt((X * X) + (Y * Y)).
arc_3pt(X1, Y1, X2, Y2, X, Y, R):-
	X2 =:= 0, !,
	Y is (Y2 * Y2) / (2 * Y2),
	X is (((X1 * X1) + (Y1 * Y1)) - (2 * Y1 * Y)) / (2 * X1),
	R is sqrt((X * X) + (Y * Y)).
arc_3pt(X1, Y1, X2, Y2, X, Y, R):-
	Y2 =:= 0, !,
	X is (X2 * X2) / (2 * X2),
	Y is (((Y1 * Y1) + (X1 * X1)) - (2 * X1 * X)) / (2 * Y1),
	R is sqrt((X * X) + (Y * Y)).
arc_3pt(X1, Y1, X2, Y2, X, Y, R):-
	trans(X1, Y1, H1, I1, R1),
	trans(X2, Y2, H2, I2, R2),
	mult(H2, H1, I1, R1, MH1, MI1, MR1),
	mult(H1, H2, I2, R2, MH2, MI2, MR2),
	arc2(MH1, MI1, MR1, MH2, MI2, MR2, X, Y, R).

arc2(MH1, MI1, MR1, MH2, MI2, MR2, X, Y, R):-
	MH1 < 0, MH2 < 0, !,
	Y is (MR1 - MR2) / (MI1 - MI2),
	X is (MR1 - (Y * MI1)) / MH1,
	R is sqrt((X * X) + (Y * Y)).
arc2(MH1, MI1, MR1, MH2, MI2, MR2, X, Y, R):-
	MH1 > 0, MH2 > 0, !,
	Y is (MR1 - MR2) / (MI1 - MI2),
	X is (MR1 - (Y * MI1)) / MH1,
	R is sqrt((X * X) + (Y * Y)).
arc2(MH1, MI1, MR1, _MH2, MI2, MR2, X, Y, R):-
	Y is (MR1 + MR2) / (MI1 + MI2),
	X is (MR1 - (Y * MI1)) / MH1,
	R is sqrt((X * X) + (Y * Y)).

trans(X, Y, H, I, R):-
	H is 2 * X,
	I is 2 * Y,
	R is (X * X) + (Y * Y).

mult(F, H, I, R, MH, MI, MR):-
	MH is F * H,
	MI is F * I,
	MR is F * R.

arc1(M1, M2, 0):-
	M1 =:= 0,
	M2 =:= 0, !.
arc1(M1, M2, 270):-
	M1 =:= 0,
	M2 < 0, !.
arc1(M1, _, 90):- 
	M1 =:= 0, !.
arc1(M1, M2, 180):-
	M2 =:= 0,
	M1 < 0, !.
arc1(_, M2, 90):-
	M2 =:= 0, !.
arc1(M1, M2, A):-
	M1 > 0,
	M2 > 0,
	A is abs(atan(M2 / M1)) * (180 / pi), !.
arc1(M1, M2, A):-
	M1 < 0,
	M2 > 0,
	A is abs(atan(M1 / M2)) * (180 / pi) + 90, !.
arc1(M1, M2, A):-
	M1 < 0,
	M2 < 0,
	A is abs(atan(M2 / M1)) * (180 / pi) + 180, !.
arc1(M1, M2, A):-
	M1 > 0,
	M2 < 0,
	A is abs(atan(M1 / M2)) * (180 / pi) + 270, !.

arc_error:-
	writeln('error: the 3 points specified do not lie on an arc'),
	fail.
	
polygon(L):-
	recorded(solid, [0]), !,
	polygon_line(L).
polygon(L):-
	getval(current, N),
	window_fact(N, C, _, _, Win, _, H, Lx, Ly, _, _, F, _, _, Gfill, _, _, _), !,
	walk_polyline(F, Lx, Ly, H, L, PL),
	xFillPolygon(C, Win, Gfill, PL, xComplex, xFalse), !,
	polygon_line(L),
	xFlush(C).

polygon_line([F1|[F2|T]]):-
	getval(current, N),
	window_fact(N, C, _, _, Win, _, H, Lx, Ly, _, _, F, _, Gsolid, _, _, _, _), !,
	walk_polygon(H, Lx, Ly, F, F1, F2, F1, F2, T, PL),
	xDrawSegments(C, Win, Gsolid, PL), !,
	xFlush(C).

walk_polygon(H, Lx, Ly, F, F1, F2, L1, L2, [], [xSegment(RL1, RL2, RF1, RF2)|[]]):-
	!,
	convert_coor(F, Lx, Ly, H, L1, L2, RL1, RL2),
	convert_coor(F, Lx, Ly, H, F1, F2, RF1, RF2).
walk_polygon(H, Lx, Ly, F, F1, F2, L1, L2, [H1|[H2|T1]], [xSegment(RL1, RL2, RH1, RH2)|T2]):-
	convert_coor(F, Lx, Ly, H, L1, L2, RL1, RL2),
	convert_coor(F, Lx, Ly, H, H1, H2, RH1, RH2),
	walk_polygon(H, Lx, Ly, F, F1, F2, H1, H2, T1, T2).

interior_style(0, Wid):-
	!,
	erase_all(solid),
	record(solid, [0]),
	getval(current, N),
	window_fact(N, C, _, _, _, _, _, _, _, _, _, _, _, Gsolid, Gfill, _, _, _), !,
	xSetGC(C, Gsolid, [xLineWidth(Wid)]),
	xSetGC(C, Gfill, [xLineWidth(Wid)]).
interior_style(S, Wid):-
	interior_style_data(S, Fill), !,
	erase_all(solid),
	record(solid, [1]),
	getval(current, N),
	window_fact(N, C, _, _, _, _, _, _, _, _, _, _, _, Gsolid, Gfill, _, _, _), !,
	xSetGC(C, Gsolid, [xLineWidth(Wid)]),
	xSetGC(C, Gfill, [xLineWidth(Wid), xFillStyle(Fill)]).
interior_style(F, _):-
	write('error: interior style '), write(F), writeln(' is not supported'),
	fail.

interior_style_data(1, xSolid).
interior_style_data(2, xTiled).
interior_style_data(3, xStippled).
interior_style_data(4, xOpaqueStippled).

perimeter_type(T):-
	line_type_data(T, Type, Length), !,
	getval(current, N),
	window_fact(N, C, _, _, _, _, _, _, _, _, _, _, _, Gsolid, Gfill, _, _, _), !,
	xSetGC(C, Gsolid, [xLineStyle(Type), xDashLength(Length)]),
	xSetGC(C, Gfill, [xLineStyle(Type), xDashLength(Length)]).
perimeter_type(T):-
	write('error: perimeter type '), write(T), writeln(' is not supported'),
	fail.

perimeter_width(Wid):-
	Wid >= 0, !,
	getval(current, N),
	window_fact(N, C, _, _, _, _, _, _, _, _, _, _, _, Gsolid, Gfill, _, _, _), !,
	xSetGC(C, Gsolid, [xLineWidth(Wid)]),
	xSetGC(C, Gfill, [xLineWidth(Wid)]).
perimeter_width(Wid):-
	write('error: perimeter width '), write(Wid), writeln(' is not supported'),
	fail.


/*****************************************************************************
* marker type operations
*****************************************************************************/
marker(X1, Y1):-
	getval(current, N),
	window_fact(N, C, _, _, Win, _, H, Lx, Ly, _, _, F, _, _, _, Gmarker, _, _), !,
	convert_coor(F, Lx, Ly, H, X1, Y1, X2, Y2),
	recorded(marker, [Type, S]), !,
	convert_length(F, S, Size),
	marker_draw(Type, C, Win, Gmarker, Size, X2, Y2),
	xFlush(C).

polymarker(L):-
	getval(current, N),
	window_fact(N, C, _, _, Win, _, H, Lx, Ly, _, _, F, _, _, _, Gmarker, _, _), !,
	recorded(marker, [Type, S]), !,
	convert_length(F, S, Size),
	walk_polymarker(L, Type, Size, C, Win, Gmarker, H, Lx, Ly, F), !,
	xFlush(C).

walk_polymarker([], _, _, _, _, _, _, _, _, _):- !.
walk_polymarker([X|[Y|T]], Type, Size, C, W, G, H, Lx, Ly, F):-
	convert_coor(F, Lx, Ly, H, X, Y, X1, Y1),
	marker_draw(Type, C, W, G, Size, X1, Y1),
	walk_polymarker(T, Type, Size, C, W, G, H, Lx, Ly, F).

marker_draw(0, C, W, G, S, X, Y):-
	!,
	X1 is X - (S//2),
	Y1 is Y - (S//2),
	xDrawRectangles(C, W, G, [xRectangle(X1, Y1, S, S)]), !.
marker_draw(1, C, W, G, S, X, Y):-
	!,
	X1 is X - (S // 2),
	Y1 is Y + (S // 2),
	X2 is X + (S // 2),
	Y2 is Y - (S // 2),
	xDrawSegments(C, W, G, [xSegment(X, Y1, X, Y2), xSegment(X1, Y, X2, Y)]), !.
marker_draw(2, C, W, G, S, X, Y):-
	!,
	marker_draw(1, C, W, G, S, X, Y),
	marker_draw(4, C, W, G, S, X, Y).
marker_draw(3, C, W, G, S, X, Y):-
	!,
	X1 is X - (S // 2),
	Y1 is Y - (S // 2),
	xDrawArcs(C, W, G, [xArc(X1, Y1, S, S, 0, 0)]), !.
marker_draw(4, C, W, G, S, X, Y):-
	X1 is X - (S // 2),
	Y1 is Y + (S // 2),
	X2 is X + (S // 2),
	Y2 is Y - (S // 2),
	xDrawSegments(C, W, G, [xSegment(X1, Y1, X2, Y2), xSegment(X1, Y2, X2, Y1)]), !.

marker_type(T):-
	T >= 0,
	T < 5, !,
	recorded(marker, [_, S]), !,
	erase_all(marker),
	record(marker,[T, S]).
marker_type(T):-
	write('error: marker type '), write(T), writeln(' is not supported'),
	fail.

marker_size(S):-
	S > 0, !,
	recorded(marker, [T, _]), !,
	erase_all(marker),
	record(marker,[T, S]).
marker_size(S):-
	write('error: marker size '), write(S), writeln(' is not supported'),
	fail.


/*****************************************************************************
* text type operations
*****************************************************************************/
text(X, Y, T):-
	convert_to_string(T, TS),
	getval(current, N),
	window_fact(N, C, _, _, Win, _, H, Lx, Ly, _, _, F, _, _, _, _, Gfont, _), !,
	convert_coor(F, Lx, Ly, H, X, Y, X1, Y1),
	xDrawText(C, Win, Gfont, X1, Y1, [TS]), !,
	xFlush(C),
	xQueryGC(Gfont, [xFont(Font)]),
	setval(text, [X1, Y1, T, Font]).

append_text(T):-
	convert_to_string(T, TS),
	getval(text, [X, Y, O, Font]),
	getval(current, N),
	window_fact(N, C, _, _, Win, _, _, _, _, _, _, _, _, _, _, _, Gfont, _), !,
	xLoadFont(C, Font, LF),
	xQueryFont(LF, [xMinWidth(FW)]),
	xUnloadFont(LF),
	name(O, TL),
	length(TL, L),
	X1 is (L * FW) + X,
	xDrawText(C, Win, Gfont, X1, Y, [TS]), !,
	xFlush(C),
	xQueryGC(Gfont, [xFont(NF)]),
	setval(text, [X1, Y, T, NF]).
append_text(_):-
	writeln('error: no previous text to append to'),
	fail.

convert_to_string(Text, String) :-
	(string(Text) ->
		Text = String
	;
	atom(Text) ->
		atom_string(Text, String)
	;
	number(Text) ->
		number_string(Text, String)
	;
	compound(Text) ->
		Text = String		% really!
	;
		term_string(Text, String)
	).

text_font_index(T):-
	T1 is T + 1,
	recorded(fonts, [F1, F2, F3, F4, F5, F6]), !,
	arg(T1, .(F1, F2, F3, F4, F5, F6), Font), !,
	getval(current, N),
	window_fact(N, C, _, _, _, _, _, _, _, _, _, _, _, _, _, _, Gfont, _), !,
	xSetGC(C, Gfont, [xFont(Font)]), !,
	xFlush(C).
text_font_index(T):-
	write('error: text font '), write(T), writeln(' is not supported'),
	fail.

text_precision(_):-
	writeln('warning: text_precision/1 is not supported with X11 interface').

character_height(_):-
	writeln('warning: character_height/1 is not supported with X11 interface').


/*****************************************************************************
* turn on xwip mode
*****************************************************************************/
xwip_mode:-
	global(qconn/0),
	global(qscreens/0),
	global(qdepths/0),
	global(qvisuals/0),
	global(qwindow/0),
	global(qpixmap/1),
	global(qgc/1),
	global(qimage/0),
	global(qcolors/0),
	global(qfont/0),
	global(stdmap/1),
	global(qpointer/0),
	global(keyboard/0),
	global(getevent/1),
	global(setmask/0),
	global(qwins/0),

	global(xOpenConnection/2),
	global(xCloseConnection/1),
	global(xConnections/1),
	global(xQueryConnection/2),
	global(xScreens/1),
	global(xQueryScreen/2),
	global(xDepths/1),
	global(xQueryDepth/2),
	global(xVisuals/1),
	global(xQueryVisual/2),

	global(xCreateWindow/12),
	global(xDestroyWindow/2),
	global(xDestroySubwindows/2),
	global(xQueryWindow/3),
	global(xSetWindow/3),
	global(xQueryTree/5),
	global(xTranslateCoordinates/8),
	global(xMapWindow/2),
	global(xMapSubwindows/2),
	global(xUnmapWindow/2),
	global(xUnmapSubwindows/2),
	global(xCirculateSubwindows/3),

	global(xAtomExists/3),
	global(xAtom/3),
	global(xWindowProperties/3),
	global(xGetProperty/11),
	global(xSetProperty/6),
	global(xDeleteProperty/3),
	global(xRotateProperties/4),
	global(xGetSelectionOwner/3),
	global(xSetSelectionOwner/4),
	global(xConvertSelection/6),

	global(xCreatePixmap/6),
	global(xDestroyPixmap/2),
	global(xQueryPixmap/3),

	global(xCreateColormap/5),
	global(xDestroyColormap/2),
	global(xMoveToNewColormap/3),
	global(xLoadedColormaps/3),
	global(xLoadColormap/2),
	global(xUnloadColormap/2),
	global(xParseColor/4),
	global(xQueryColor/5),
	global(xGetColors/3),
	global(xRequestColor/4),
	global(xRequestNamedColor/5),
	global(xAllocColorCells/7),
	global(xAllocColorPlanes/11),
	global(xFreeColors/4),
	global(xSetColors/3),
	global(xSetNamedColor/4),

	global(xGCs/1),
	global(xCreateGC/4),
	global(xDestroyGC/2),
	global(xCopyGC/4),
	global(xQueryGC/2),
	global(xQueryBestSize/7),
	global(xSetGC/3),
	global(xSetClips/6),
	global(xSetDashes/4),

	global(xClearArea/7),
	global(xCopyArea/10),
	global(xCopyPlane/11),
	global(xDrawPoints/5),
	global(xDrawLines/5),
	global(xDrawSegments/4),
	global(xDrawRectangles/4),
	global(xDrawArcs/4),
	global(xFillRectangles/4),
	global(xFillPolygon/6),
	global(xFillArcs/4),

	global(xImages/1),
	global(xCreateImage/10),
	global(xDestroyImage/1),
	global(xQueryImage/2),
	global(xGetImage/10),
	global(xPutImage/10),
	global(xSubImage/6),
	global(xAddPixel/2),
	global(xGetPixel/4),
	global(xSetPixel/4),

	global(xOpenFont/3),
	global(xCloseFont/2),
	global(xLoadedFonts/1),
	global(xLoadFont/3),
	global(xUnloadFont/1),
	global(xGetFontPath/2),
	global(xSetFontPath/2),
	global(xListFonts/4),
	global(xQueryFont/2),
	global(xGetFontProperty/3),
	global(xGetCharInfo/8),
	global(xTextEvents/7),
	global(xDrawText/6),
	global(xImageText/6),

	global(xCreateFontCursor/3),
	global(xCreateGlyphCursor/8),
	global(xCreatePixmapCursor/8),
	global(xDestroyCursor/2),
	global(xRecolorCursor/4),

	global(xPointerState/9),
	global(xWrapPointer/9),
	global(xQueryPointer/2),
	global(xSetPointer/2),
	global(xGetPointerMapping/3),
	global(xSetPointerMapping/2),

	global(xDownKeymap/2),
	global(xQueryKeyboard/2),
	global(xSetKeyboard/2),
	global(xGetKeyboardMapping/4),
	global(xSetKeyboardMapping/3),
	global(xGetModifierMapping/2),
	global(xSetModifierMapping/3),
	global(xBell/2),
	global(xKeycodeToKeysym/4),
	global(xKeysymToKeycode/3),
	global(xRefreshMapping/1),

	global(xSetSaveSet/3),
	global(xReparentWindow/5),
	global(xSetCloseDownMode/2),
	global(xKillClient/2),
	global(xGetInputFocus/3),
	global(xSetInputFocus/4),
	global(xGrabPointer/10),
	global(xUngrabPointer/2),
	global(xSetActivePointer/4),
	global(xGrabButton/10),
	global(xUngrabButton/4),
	global(xGrabKeyboard/7),
	global(xUngrabKeyboard/2),
	global(xGrabKey/7),
	global(xUngrabKey/4),
	global(xAllowEvents/3),
	global(xGrabServer/1),
	global(xUngrabServer/1),
	global(xGetScreenSaver/5),
	global(xSetScreenSaver/5),
	global(xScreenSaver/2),
	global(xQueryAccess/3),
	global(xSetAccess/2),
	global(xSetHostAccess/3),

	global(xEventsQueued/3),
	global(xNextEvent/3),
	global(xGetEvent/7),
	global(xSelectEvent/10),
	global(xPutBackEvent/2),
	global(xSendEvent/5),
	global(xFlush/1),
	global(xSync/2),
	global(xSynchronize/2),
	global(xGetMotionEvents/5),

	global(xListExtensions/2),
	global(xQueryExtension/5),
	global(xGetHostName/1),
	global(xNoOp/1),
	global(xGetDefault/4),
	global(xParseGeometry/7),
	global(xGeometry/11),
	global(xSetStandardProperties/7),
	global(xSetWMHints/3),
	global(xSetNormalHints/3),
	global(xOk/0),
	global(xPrecision/1).


/*****************************************************************************
* query section for X11
*****************************************************************************/
kformat(P, Q):-
	write(P),
	writeln(Q).

qconn:-
	getval(current, N),
	window_fact(N, C, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _), !,
	xQueryConnection(C, [xNetworkDescriptor(A00),
			     xProtocolVersion(A01),
			     xProtocolRevision(A02),
			     xServerVendor(A03),
			     xImageByteOrder(A04),
			     xImageUnit(A05),
			     xImagePad(A06),
			     xImageBitOrder(A07),
			     xVendorRelease(A08),
			     xQueueLength(A09),
			     xLastEvent(A10),
			     xLastRequest(A11),
			     xConnectionName(A12),
			     xMotionBuffer(A16),
			     xDefaultScreen(A13),
			     xScreens(A14)]),
	xConnections(A15), 
        kformat('connection: ', [C]),
	kformat('	net fd: 		', [A00]),
	kformat('	protocol version:	', [A01]),
	kformat('	protocol revision:	', [A02]),
	kformat('	server vendor:		', [A03]),
	kformat('	image byte order:	', [A04]),
	kformat('	image unit:		', [A05]),
	kformat('	image pad unit:		', [A06]),
	kformat('	image bit order:	', [A07]),
	kformat('	vendor release:		', [A08]),
	kformat('	queue length:		', [A09]),
	kformat('	last event:		', [A10]),
	kformat('	last request:		', [A11]),
	kformat('	connection name:	', [A12]),
	kformat('	motion buffer size:	', [A16]),
	kformat('	default screen:		', [A13]), 
	kformat('	connection screens:	', [A14]),
	kformat('connections: ', [A15]).

qscreens:-
	xScreens(S),
	qscreen(S).
	
qscreen([]).
qscreen([S|T]):-
	xQueryScreen(S, [xConnection(A00),
			 xRootWindow(A01),
			 xWidth(A02),
			 xHeight(A03),
			 xPhysicalWidth(A04),
			 xPhysicalHeight(A05),
			 xRootDepth(A06),
			 xRootVisual(A07),
			 xDefaultGC(A08),
			 xDefaultColormap(A09),
			 xWhitePixel(A10),
			 xBlackPixel(A11),
			 xMaxColormaps(A12),
			 xMinColormaps(A13),
			 xBackingStore(A14),
			 xSaveUnders(A15),
			 xRootEventMask(A16),
			 xDepths(A17)]),
	kformat('screen ', S),
	kformat('	connection: 		', [A00]),
	kformat('	root window:		', [A01]),
	kformat('	width:			', [A02]),
	kformat('	height:			', [A03]),
	kformat('	physical width:		', [A04]),
	kformat('	physical height:	', [A05]),
	kformat('	root depth:		', [A06]),
	kformat('	root visual:		', [A07]),
	kformat('	default GC:		', [A08]),
	kformat('	default colormap:	', [A09]),
	kformat('	white pixel:		', [A10]),
	kformat('	black pixel:		', [A11]),
	kformat('	max colormaps:		', [A12]),
	kformat('	min colormaps:		', [A13]),
	kformat('	backing store:		', [A14]),
	kformat('	save unders:		', [A15]),
	kformat('	root event mask:	', [A16]),
	kformat('	screen depths:		', [A17]),
	qscreen(T).

qdepths:-
	xDepths(D),
	qdepth(D).

qdepth([]).
qdepth([D | DL]):-
	xQueryDepth(D, [xDepth(DV), xVisuals(VL)]),
	write('depth '), write([D]),
	write(' value '), write([DV]),
	write(' visuals '), writeln(VL),
	qdepth(DL).

qvisuals:-
	xVisuals(V),
	qvisual(V).

qvisual([]).
qvisual([V | T]):-
	xQueryVisual(V, [xClass(A00),
			 xRedMask(A01),
			 xGreenMask(A02),
			 xBlueMask(A03),
			 xColormapBits(A04),
			 xColormapEntries(A05)]),
	kformat('visual ', V),
	kformat('	class:			', [A00]),
	kformat('	red mask:		', [A01]),
	kformat('	green mask:		', [A02]),
	kformat('	blue mask:		', [A03]),
	kformat('	colormap bits:		', [A04]),
	kformat('	colormap entries:	', [A05]),
	qvisual(T).

qwindow:-
	getval(current, N),
	window_fact(N, C, _, _, Win, _, _, _, _, _, _, _, _, _, _, _, _, _), !,
	xQueryWindow(C, Win, [xX(A00),
	                    xY(A01),
			    xWidth(A02),
			    xHeight(A03),
			    xBorderWidth(A04),
			    xDepth(A05),
			    xRootWindow(A06),
			    xScreen(A07),
			    xVisual(A08),
			    xClass(A09),
			    xUnionEventMask(A10),
			    xBitGravity(A11),
			    xWinGravity(A12),
			    xBackingStore(A13),
			    xBackingPlanes(A14),
			    xBackingPixel(A15),
			    xOverrideRedirect(A16),
			    xSaveUnder(A17),
			    xEventMask(A18),
			    xDontPropagate(A19),
			    xColormap(A20),
			    xColormapLoaded(A21),
			    xState(A22)]),
	kformat('window ', Win),
	kformat('	X			', [A00]),
	kformat('	Y			', [A01]),
	kformat('	width			', [A02]),
	kformat('	height			', [A03]),
	kformat('	border width		', [A04]),
	kformat('	depth			', [A05]),
	kformat('	root window		', [A06]),
	kformat('	screen			', [A07]),
	kformat('	visual			', [A08]),
	kformat('	class			', [A09]),
	kformat('	union event mask	', [A10]),
	kformat('	bit gravity		', [A11]),
	kformat('	window gravity		', [A12]),
	kformat('	backing store		', [A13]),
	kformat('	backing planes		', [A14]),
	kformat('	backing pixel		', [A15]),
	kformat('	override redirect	', [A16]),
	kformat('	save under		', [A17]),
	kformat('	event mask		', [A18]),
	kformat('	dont propagate		', [A19]),
	kformat('	colormap		', [A20]),
	kformat('	colormap loaded		', [A21]),
	kformat('	state			', [A22]).

qpixmap(P):-
	getval(current, N),
	window_fact(N, C, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _), !,
	xQueryPixmap(C, P, [xWidth(A00),
			    xHeight(A01),
			    xBorderWidth(A02),
			    xDepth(A03),
			    xScreen(A04)]),
	kformat('pixmap ', P),
	kformat('	width:		', [A00]),
	kformat('	height:		', [A01]),
	kformat('	border width:	', [A02]),
	kformat('	depth:		', [A03]),
	kformat('	screen:		', [A04]).

qgc(G):-
	xQueryGC(G, [xFunction(A00),
		     xPlaneMask(A01),
		     xForeground(A02),
		     xBackground(A03),
		     xLineWidth(A04),
		     xLineStyle(A05),
		     xCapStyle(A06),
		     xJoinStyle(A07),
		     xFillStyle(A08),
		     xFillRule(A09),
		     xTile(A10),
		     xStipple(A11),
		     xTileStipXOrigin(A12),
		     xTileStipYOrigin(A13),
		     xFont(A14),
		     xSubwindowMode(A15),
		     xGraphicsExposures(A16),
		     xClipXOrigin(A17),
		     xClipYOrigin(A18),
		     xClipMask(A19),
		     xDashOffset(A20),
		     xDashLength(A21),
		     xArcMode(A22),
		     xClipList(A23),
		     xDashList(A24)]),
	kformat('GC ', G),
	kformat('	function		', [A00]),
	kformat('	plane mask		', [A01]),
	kformat('	foreground		', [A02]),
	kformat('	background		', [A03]),
 	kformat('	line width		', [A04]),
	kformat('	line style		', [A05]),
	kformat('	cap style		', [A06]),
	kformat('	join style		', [A07]),
	kformat('	fill style		', [A08]),
	kformat('	fill rule		', [A09]),
	kformat('	tile			', [A10]),
	kformat('	stipple			', [A11]),
	kformat('	tile/stipple X origin	', [A12]),
	kformat('	tile/stipple Y origin	', [A13]),
	kformat('	font			', [A14]),
	kformat('	subwindow mode		', [A15]),
	kformat('	graphics exposures	', [A16]),
	kformat('	clip X origin		', [A17]),
	kformat('	clip Y origin		', [A18]),
	kformat('	clip mask		', [A19]),
	kformat('	dash offset		', [A20]),
	kformat('	dash length		', [A21]),
	kformat('	arc mode		', [A22]),
	kformat('	clip list		', [A23]),
	kformat('	dashlist		', [A24]).

qimage:-
	recorded(bitmap, [I, _, _]), !,
	xQueryImage(I, [xWidth(A00),
			xHeight(A01),
			xOffset(A02),
			xFormat(A03),
			xImageByteOrder(A04),
			xImageUnit(A05),
			xImagePad(A06),
			xImageBitOrder(A07),
			xDepth(A08),
			xLineBytes(A09),
			xPixelBits(A10),
			xRedMask(A11),
			xGreenMask(A12),
			xBlueMask(A13)]),
	kformat('image ', [I]),
	kformat('	width			', [A00]),
	kformat('	height			', [A01]),
	kformat('	offset			', [A02]),
	kformat('	format			', [A03]),
	kformat('	byte order		', [A04]),
	kformat('	image unit		', [A05]),
	kformat('	image pad		', [A06]),
	kformat('	image bit order		', [A07]),
	kformat('	depth			', [A08]),
	kformat('	line bytes		', [A09]),
	kformat('	pixel bits		', [A10]),
	kformat('	red mask		', [A11]),
	kformat('	green mask		', [A12]),
	kformat('	blue mask 		', [A13]).
	
qcolors:-
	getval(current, N),
	window_fact(N, C, _, CM, _, _, _, _, _, _, _, _, _, _, _, _, _, _), !,
	xGetColors(C, CM, [xCell(0,ZERO),
			   xCell(1,ONE),
			   xCell(2,TWO),
			   xCell(3,THREE),
			   xCell(4,FOUR),
			   xCell(5,FIVE),
			   xCell(6,SIX),
			   xCell(7,SEVEN),
			   xCell(8,EIGHT),
			   xCell(9,NINE),
			   xCell(10,TEN),
			   xCell(11,ELEVEN),
			   xCell(12,TWELVE),
			   xCell(13,THIRTEEN),
			   xCell(14,FOURTEEN),
			   xCell(15,FIFTEEN)]),
	kformat('connection', [C]),
	kformat('colormap  ', [CM]),
	kformat('	 0:	', [ZERO]),
	kformat('	 1:	', [ONE]),
	kformat('	 2:	', [TWO]),
	kformat('	 3:	', [THREE]),
	kformat('	 4:	', [FOUR]),
	kformat('	 5:	', [FIVE]),
	kformat('	 6:	', [SIX]),
	kformat('	 7:	', [SEVEN]),
	kformat('	 8:	', [EIGHT]),
	kformat('	 9:	', [NINE]),
	kformat('	10:	', [TEN]),
	kformat('	11:	', [ELEVEN]),
	kformat('	12:	', [TWELVE]),
	kformat('	13:	', [THIRTEEN]),
	kformat('	14:	', [FOURTEEN]),
	kformat('	15:	', [FIFTEEN]).

qfont:-
	getval(current, N),
	window_fact(N, C, _, _, _, _, _, _, _, _, _, _, _, _, _, _, Gfont, _), !,
	xQueryGC(Gfont, [xFont(F)]),
	xLoadFont(C, F, LF),
	xQueryFont(LF, [xConnection(Z00),
		       xFont(Z01),
                       xDirection(A00),
		       xMinColumn(A01),
		       xMaxColumn(A02),
		       xMinRow(A03),
		       xMaxRow(A04),
		       xAllExist(A05),
		       xDefaultChar(A06),
		       xMinLeft(A07),
		       xMinRight(A08),
		       xMinWidth(A09),
		       xMinAscent(A10),
		       xMinDescent(A11),
		       xMinAttribute(A12),
		       xMaxLeft(A13),
		       xMaxRight(A14),
		       xMaxWidth(A15),
		       xMaxAscent(A16),
		       xMaxDescent(A17),
		       xMaxAttribute(A18),
		       xAscent(A19),
		       xDescent(A20),
		       xProperties(PL)]),
	xUnloadFont(LF),
	kformat('font ', F),
	kformat('	connection	', [Z00]),
	kformat('	font XID	', [Z01]),
	kformat('	direction	', [A00]),
	kformat('	min column	', [A01]),
	kformat('	max column	', [A02]),
	kformat('	min row		', [A03]),
	kformat('	max row		', [A04]),
	kformat('	all exist	', [A05]),
	kformat('	default char	', [A06]),
	kformat('	min left	', [A07]),
	kformat('	min right	', [A08]),
	kformat('	min width	', [A09]),
	kformat('	min ascent	', [A10]),
	kformat('	min descent	', [A11]),
	kformat('	min attribute	', [A12]),
	kformat('	max left	', [A13]),
	kformat('	max right	', [A14]),
	kformat('	max width	', [A15]),
	kformat('	max ascent	', [A16]),
	kformat('	max descent	', [A17]),
	kformat('	max attribute	', [A18]),
	kformat('	font ascent	', [A19]),
	kformat('	font descent	', [A20]),
	kformat('	font properties	', [PL]).

stdmap(P):-
	getval(current, N),
	window_fact(N, C, S, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _), !,
	xQueryScreen(S, [xRootWindow(RW)]),
	xGetProperty(C, RW, P, 0, 8, 0, 24, 0, 24, 0, V),
	kformat('map   : ', [P]),
	kformat('values: ', [V]).

qpointer:-
	getval(current, N),
	window_fact(N, C, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _), !,
	xQueryPointer(C, [xNumerator(Num), xDenominator(Den), xThreshold(Thr)]),
	xGetPointerMapping(C, 3, Map),
	kformat('pointer with connection ', [C]),
	kformat('	numerator	', [Num]),
	kformat('	denominator	', [Den]),
	kformat('	threshold	', [Thr]),
	kformat('	mapping		', [Map]).

qkeyboard:-
	getval(current, N),
	window_fact(N, C, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _), !,
	xQueryKeyboard(C, [xKeyClickPercent(A00),
			   xBellPercent(A01),
			   xBellPitch(A02),
			   xBellDuration(A03),
			   xLed(A04),
			   xAutoRepeatMode(A05),
			   xRepeatKeymap(A07)]),
	xDownKeymap(C, A06),
	kformat('keyboard with connection ', [C]),
	kformat('	key click %	', [A00]),
    	kformat('	bell %		', [A01]),
    	kformat('	bell pitch	', [A02]),
	kformat('	bell duration	', [A03]),
	kformat('	LEDs		', [A04]),
	kformat('	auto repeat	', [A05]),
	kformat('	down keymap	', [A06]),
	kformat('	repeat keymap	', [A07]).

getevent(E):-
	getval(current, N),
	window_fact(N, C, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _), !,
	xGetEvent(C, xAny, [], xAny, xTrue, xFalse, E).

setmask:-
	getval(current, N),
	window_fact(N, C, _, _, Win, _, _, _, _, _, _, _, _, _, _, _, _, _), !,
	xSetWindow(C, Win, [xEventMask([xKeyPress,
					xKeyRelease,
					xButtonPress,
					xButtonRelease,
					xEnterWindow,
					xLeaveWindow,
					xPointerMotion,
					xPointerMotionHint,
					xButtonMotion,
					xButton1Motion,
					xButton2Motion,
					xButton3Motion,
					xButton4Motion,
					xButton5Motion,
					xKeymapState,
					xFocusChange,
					xOwnerGrabButton,
					xExposure,
					xVisibilityChange,
					xColormapChange,
					xPropertyChange,
					xStructureNotify,
					xSubstructureNotify,
					xSubstructureRedirect,
					xResizeRedirect])]).
qwins:-
	window_fact(N, C, S, CM, Win, W, H, Lx, Ly, Ux, Uy, F, Gline, Gsolid, Gfill, Gmarker, Gfont, Gbitmap),
	writeln([N, C, S, CM, Win, W, H, Lx, Ly, Ux, Uy, F, Gline, Gsolid, Gfill, Gmarker, Gfont, Gbitmap]),
	fail.
qwins.



/******************************************************************************
* CGI colour map:               	                                      *
******************************************************************************/
colour_cgi(0, 65025, 65025, 65025).
colour_cgi(1, 0, 0, 0).
colour_cgi(2, 65025, 6120, 0).
colour_cgi(3, 65025, 9180, 0).
colour_cgi(4, 65025, 12240, 0).
colour_cgi(5, 65025, 15300, 0).
colour_cgi(6, 65025, 18360, 0).
colour_cgi(7, 65025, 21675, 0).
colour_cgi(8, 65025, 24735, 0).
colour_cgi(9, 65025, 27795, 0).
colour_cgi(10, 65025, 30855, 0).
colour_cgi(11, 65025, 33915, 0).
colour_cgi(12, 65025, 36975, 0).
colour_cgi(13, 65025, 40035, 0).
colour_cgi(14, 65025, 43350, 0).
colour_cgi(15, 65025, 46410, 0).
colour_cgi(16, 65025, 49470, 0).
colour_cgi(17, 65025, 52530, 0).
colour_cgi(18, 65025, 55590, 0).
colour_cgi(19, 65025, 58650, 0).
colour_cgi(20, 65025, 61710, 0).
colour_cgi(21, 65025, 65025, 0).
colour_cgi(22, 61965, 65025, 0).
colour_cgi(23, 58905, 65025, 0).
colour_cgi(24, 55845, 65025, 0).
colour_cgi(25, 52785, 65025, 0).
colour_cgi(26, 49725, 65025, 0).
colour_cgi(27, 46665, 65025, 0).
colour_cgi(28, 43350, 65025, 0).
colour_cgi(29, 40290, 65025, 0).
colour_cgi(30, 37230, 65025, 0).
colour_cgi(31, 34170, 65025, 0).
colour_cgi(32, 31110, 65025, 0).
colour_cgi(33, 28050, 65025, 0).
colour_cgi(34, 24990, 65025, 0).
colour_cgi(35, 21675, 65025, 0).
colour_cgi(36, 18615, 65025, 0).
colour_cgi(37, 15555, 65025, 0).
colour_cgi(38, 12495, 65025, 0).
colour_cgi(39, 9435, 65025, 0).
colour_cgi(40, 6375, 65025, 0).
colour_cgi(41, 3315, 65025, 0).
colour_cgi(42, 0, 65025, 0).
colour_cgi(43, 0, 65025, 3060).
colour_cgi(44, 0, 65025, 6120).
colour_cgi(45, 0, 65025, 9180).
colour_cgi(46, 0, 65025, 12240).
colour_cgi(47, 0, 65025, 15300).
colour_cgi(48, 0, 65025, 18360).
colour_cgi(49, 0, 65025, 21675).
colour_cgi(50, 0, 65025, 24735).
colour_cgi(51, 0, 65025, 27795).
colour_cgi(52, 0, 65025, 30855).
colour_cgi(53, 0, 65025, 33915).
colour_cgi(54, 0, 65025, 36975).
colour_cgi(55, 0, 65025, 40035).
colour_cgi(56, 0, 65025, 43350).
colour_cgi(57, 0, 65025, 46410).
colour_cgi(58, 0, 65025, 49470).
colour_cgi(59, 0, 65025, 52530).
colour_cgi(60, 0, 65025, 55590).
colour_cgi(61, 0, 65025, 58650).
colour_cgi(62, 0, 65025, 61710).
colour_cgi(63, 0, 65025, 65025).
colour_cgi(64, 0, 61965, 65025).
colour_cgi(65, 0, 58905, 65025).
colour_cgi(66, 0, 55845, 65025).
colour_cgi(67, 0, 52785, 65025).
colour_cgi(68, 0, 49725, 65025).
colour_cgi(69, 0, 46665, 65025).
colour_cgi(70, 0, 43350, 65025).
colour_cgi(71, 0, 40290, 65025).
colour_cgi(72, 0, 37230, 65025).
colour_cgi(73, 0, 34170, 65025).
colour_cgi(74, 0, 31110, 65025).
colour_cgi(75, 0, 28050, 65025).
colour_cgi(76, 0, 24990, 65025).
colour_cgi(77, 0, 21675, 65025).
colour_cgi(78, 0, 18615, 65025).
colour_cgi(79, 0, 15555, 65025).
colour_cgi(80, 0, 12495, 65025).
colour_cgi(81, 0, 9435, 65025).
colour_cgi(82, 0, 6375, 65025).
colour_cgi(83, 0, 3315, 65025).
colour_cgi(84, 0, 0, 65025).
colour_cgi(85, 3060, 0, 65025).
colour_cgi(86, 6120, 0, 65025).
colour_cgi(87, 9180, 0, 65025).
colour_cgi(88, 12240, 0, 65025).
colour_cgi(89, 15300, 0, 65025).
colour_cgi(90, 18360, 0, 65025).
colour_cgi(91, 21675, 0, 65025).
colour_cgi(92, 24735, 0, 65025).
colour_cgi(93, 27795, 0, 65025).
colour_cgi(94, 30855, 0, 65025).
colour_cgi(95, 33915, 0, 65025).
colour_cgi(96, 36975, 0, 65025).
colour_cgi(97, 40035, 0, 65025).
colour_cgi(98, 43350, 0, 65025).
colour_cgi(99, 46410, 0, 65025).
colour_cgi(100, 49470, 0, 65025).
colour_cgi(101, 52530, 0, 65025).
colour_cgi(102, 55590, 0, 65025).
colour_cgi(103, 58650, 0, 65025).
colour_cgi(104, 61710, 0, 65025).
colour_cgi(105, 65025, 0, 65025).
colour_cgi(106, 65025, 0, 61965).
colour_cgi(107, 65025, 0, 58905).
colour_cgi(108, 65025, 0, 55845).
colour_cgi(109, 65025, 0, 52785).
colour_cgi(110, 65025, 0, 49725).
colour_cgi(111, 65025, 0, 46665).
colour_cgi(112, 65025, 0, 43350).
colour_cgi(113, 65025, 0, 40290).
colour_cgi(114, 65025, 0, 37230).
colour_cgi(115, 65025, 0, 34170).
colour_cgi(116, 65025, 0, 31110).
colour_cgi(117, 65025, 0, 28050).
colour_cgi(118, 65025, 0, 24990).
colour_cgi(119, 65025, 0, 21675).
colour_cgi(120, 65025, 0, 18615).
colour_cgi(121, 65025, 0, 15555).
colour_cgi(122, 65025, 0, 12495).
colour_cgi(123, 65025, 0, 9435).
colour_cgi(124, 65025, 0, 6375).
colour_cgi(125, 65025, 0, 3315).
colour_cgi(126, 0, 0, 0).
colour_cgi(127, 65025, 65025, 65025).
/*
colour_cgi(128, 65025, 63750, 63750).
colour_cgi(129, 63240, 63240, 65025).
colour_cgi(130, 62475, 62475, 62475).
colour_cgi(131, 56100, 56100, 56100).
colour_cgi(132, 65025, 63750, 61200).
colour_cgi(133, 64515, 62475, 58650).
colour_cgi(134, 63750, 61200, 58650).
colour_cgi(135, 63750, 59925, 54825).
colour_cgi(136, 65025, 60945, 54315).
colour_cgi(137, 65025, 59925, 52275).
colour_cgi(138, 65025, 58140, 49980).
colour_cgi(139, 65025, 55590, 47175).
colour_cgi(140, 65025, 56610, 44115).
colour_cgi(141, 65025, 58140, 46155).
colour_cgi(142, 65025, 63240, 56100).
colour_cgi(143, 65025, 65025, 61200).
colour_cgi(144, 65025, 63750, 52275).
colour_cgi(145, 65025, 62475, 60690).
colour_cgi(146, 61200, 65025, 61200).
colour_cgi(147, 62475, 65025, 63750).
colour_cgi(148, 61200, 65025, 65025).
colour_cgi(149, 61200, 63240, 65025).
colour_cgi(150, 58650, 58650, 63750).
colour_cgi(151, 65025, 61200, 62475).
colour_cgi(152, 65025, 58140, 57375).
colour_cgi(153, 11985, 20145, 20145).
colour_cgi(154, 26775, 26775, 26775).
colour_cgi(155, 28560, 32640, 36720).
colour_cgi(156, 30345, 34680, 39015).
colour_cgi(157, 48960, 48960, 48960).
colour_cgi(158, 53805, 53805, 53805).
colour_cgi(159, 6375, 6375, 28560).
colour_cgi(160, 0, 0, 32640).
colour_cgi(161, 25500, 37995, 60435).
colour_cgi(162, 18360, 15555, 35445).
colour_cgi(163, 27030, 22950, 52275).
colour_cgi(164, 31365, 26520, 60690).
colour_cgi(165, 33660, 28560, 65025).
colour_cgi(166, 0, 0, 52275).
colour_cgi(167, 16575, 26775, 57375).
colour_cgi(168, 0, 0, 65025).
colour_cgi(169, 7650, 36720, 65025).
colour_cgi(170, 0, 48705, 65025).
colour_cgi(171, 34425, 52530, 59925).
colour_cgi(172, 34425, 52530, 63750).
colour_cgi(173, 17850, 33150, 45900).
colour_cgi(174, 44880, 49980, 56610).
colour_cgi(175, 44115, 55080, 58650).
colour_cgi(176, 44880, 57120, 58650).
colour_cgi(177, 44625, 60690, 60690).
colour_cgi(178, 0, 52530, 53295).
colour_cgi(179, 18360, 53295, 52020).
colour_cgi(180, 16320, 57120, 53040).
colour_cgi(181, 0, 65025, 65025).
colour_cgi(182, 57120, 65025, 65025).
colour_cgi(183, 24225, 40290, 40800).
colour_cgi(184, 26010, 52275, 43350).
colour_cgi(185, 32385, 65025, 54060).
colour_cgi(186, 0, 25500, 0).
colour_cgi(187, 21675, 27285, 11985).
colour_cgi(188, 36465, 47940, 36465).
colour_cgi(189, 11730, 35445, 22185).
colour_cgi(190, 15300, 45645, 28815).
colour_cgi(191, 8160, 45390, 43350).
colour_cgi(192, 38760, 64005, 38760).
colour_cgi(193, 0, 65025, 32385).
colour_cgi(194, 31620, 64260, 0).
colour_cgi(195, 0, 65025, 0).
colour_cgi(196, 32385, 65025, 0).
colour_cgi(197, 0, 63750, 39270).
colour_cgi(198, 44115, 65025, 11985).
colour_cgi(199, 12750, 52275, 12750).
colour_cgi(200, 39270, 52275, 12750).
colour_cgi(201, 8670, 35445, 8670).
colour_cgi(202, 27285, 36210, 8925).
colour_cgi(203, 48195, 46665, 27285).
colour_cgi(204, 61200, 58650, 35700).
colour_cgi(205, 60690, 59160, 43350).
colour_cgi(206, 63750, 63750, 53550).
colour_cgi(207, 65025, 65025, 57120).
colour_cgi(208, 65025, 65025, 0).
colour_cgi(209, 65025, 54825, 0).
colour_cgi(210, 60690, 56355, 33150).
colour_cgi(211, 55590, 42075, 8160).
colour_cgi(212, 46920, 34170, 2805).
colour_cgi(213, 47940, 36465, 36465).
colour_cgi(214, 52275, 23460, 23460).
colour_cgi(215, 35445, 17595, 4845).
colour_cgi(216, 40800, 20910, 11475).
colour_cgi(217, 52275, 33915, 16065).
colour_cgi(218, 56610, 46920, 34425).
colour_cgi(219, 62475, 62475, 56100).
colour_cgi(220, 62475, 56610, 45645).
colour_cgi(221, 62220, 41820, 24480).
colour_cgi(222, 53550, 45900, 35700).
colour_cgi(223, 53550, 26775, 7650).
colour_cgi(224, 45390, 8670, 8670).
colour_cgi(225, 42075, 10710, 10710).
colour_cgi(226, 59415, 38250, 31110).
colour_cgi(227, 63750, 32640, 29070).
colour_cgi(228, 65025, 40800, 31110).
colour_cgi(229, 65025, 42075, 0).
colour_cgi(230, 65025, 35700, 0).
colour_cgi(231, 65025, 32385, 20400).
colour_cgi(232, 61200, 32640, 32640).
colour_cgi(233, 65025, 25245, 18105).
colour_cgi(234, 65025, 17595, 0).
colour_cgi(235, 65025, 0, 0).
colour_cgi(236, 65025, 26775, 45900).
colour_cgi(237, 65025, 5100, 37485).
colour_cgi(238, 65025, 48960, 51765).
colour_cgi(239, 65025, 46410, 49215).
colour_cgi(240, 55845, 28560, 37485).
colour_cgi(241, 44880, 12240, 24480).
colour_cgi(242, 50745, 5355, 33915).
colour_cgi(243, 53040, 8160, 36720).
colour_cgi(244, 65025, 0, 65025).
colour_cgi(245, 60690, 33150, 60690).
colour_cgi(246, 56355, 40800, 56355).
colour_cgi(247, 55590, 28560, 54570).
colour_cgi(248, 47430, 21675, 53805).
colour_cgi(249, 39015, 12750, 52020).
colour_cgi(250, 37740, 0, 53805).
colour_cgi(251, 35190, 10965, 57630).
colour_cgi(252, 55080, 48705, 55080).
colour_cgi(253, 65025, 65025, 65025).
colour_cgi(254, 0, 0, 0).
colour_cgi(255, 0, 0, 0).
*/


/*****************************************************************************
* compiled declarations:                                                     *
*****************************************************************************/
:- skipped
	xwip_mode/0,
	destroy_window/0,
	destroy_window/1,
	current_window/1.

:- skipped
	close_2d/0, 
	circular_arc_center/7, 
	circular_arc_center_close/7, 
	circular_arc_3pt/6, 
	circular_arc_3pt_close/6, 
	cell_array/6, 
	pixel_array/2, 
	bitmap/3, 
	load_bitmap/1,
	line/4, 
	polyline/1, 
	disjoint_polyline/1, 
	polymarker/1, 
	polygon/1, 
	partial_polygon/2, 
	color_rgb/4, 
	rectangle/4, 
	ellipse/4, 
	vdc_extent/4, 
	circle/3, 
	text/3, 
	append_text/1, 
	marker/2, 
	interior_style/2, 
	clear_view_surface/1, 
	text_precision/1, 
	text_font_index/1, 
	character_height/1, 
	text_color/1, 
	marker_size/1, 
	marker_color/1, 
	fill_color/1, 
	perimeter_type/1, 
	perimeter_width/1, 
	perimeter_color/1, 
	line_type/1, 
	line_end_style/1, 
	line_width/1, 
	line_color/1, 
	marker_type/1, 
	reset_to_defaults/0, 
	mouse/3, 
	mouse1/6, 
	get_bitmap/2, 
	close_window/0, 
	open_window/0, 
	move_window/0, 
	move_window/2, 
	stretch_window/0, 
	stretch_window/2, 
	expose_window/0, 
	hide_window/0, 
	refresh_window/0, 
	stretch_char_window/2, 
	set_tool_header/1, 
	set_icon/1, 
	set_icon_label/1,
	set_drawing_mode/1.
