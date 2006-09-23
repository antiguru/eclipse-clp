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
% Version:	$Id: perfmeter.pl,v 1.1 2006/09/23 01:55:32 snovello Exp $
% ----------------------------------------------------------------------

%
% SEPIA DEMO PROGRAM
%
% IDENTIFICATION:	perfmeter.pl
%
% AUTHOR:		Micha Meier
%
% CONTENTS:		perfmeter/0		runs the demo
%
% DESCRIPTION:
%
%		Makes a Sun-like perfmeter for Prolog memory areas using PCE.
%

:- module(perfmeter).

:- global
	perfmeter/0,
	perfmeter/1,
	close_perfmeter/0.

:- export
	start/1.

:- make_callback(perfmeter_menu/2).
:- make_callback(done_perfmeter/2).

:- make_local_array(gc),
   make_local_array(type),
   make_local_array(bitmap),
   make_local_array(last_overflow),
   make_local_array(xsize),
   make_local_array(ysize),
   make_local_array(y_max),
   make_local_array(last).

:-      make_local_array(flags),
	get_flag(debug_compile, DC),
	get_flag(variable_names, VN),
	setval(flags, flags(DC, VN)),
	nodbgcomp,
	true.

perfmeter :-
	perfmeter(1).

perfmeter(T) :-
	(open_pce->true;true),
	send(@pce, log, off),
	new(Popup, popup(popup, cascade(0, perfmeter_menu, 0))),
	send(Popup, append, [global, local trail, control, gc,
		'general heap', 'code heap']),
	send(Popup, pinned, on),
	XSize = 120,
	YSize = 120,
	new(@perfmeter_pic, picture(perfmeter, size(XSize, YSize))),
	send(@perfmeter_pic, vertical_scrollbar, off),
	send(@perfmeter_pic, horizontal_scrollbar, off),
	send(@perfmeter_pic, left_down, 0),
	send(@perfmeter_pic, left_up, 0),
	send(@perfmeter_pic, left_drag, 0),
	send(@perfmeter_pic, popup, Popup),
	Xf is XSize + 5,
	Yf is YSize + 5,
	send(@perfmeter_pic, size, size(Xf, Yf)),
	send(@perfmeter_pic, open),
	send(@perfmeter_pic, done_message, cascade(@perfmeter_pic, done_perfmeter, 0)),
	new(@perfmeter_text, text('', 0, 0, left)),
	new(@perfmeter_size, text('', 0, 0, left)),
	new(@perfmeter_k, text('', 0, 0, left)),
	set_interrupt_handler(alrm, update/0),
	setval(gc, 0),
	setval(type, 'Global stack'),
	resize(XSize, YSize, reset),
	sleep(1),		% because of X
	start(T).

close_perfmeter :-
	set_timer(real, 0),
	send(@perfmeter_pic, destroy),
	speed_pce(0).

done_perfmeter(_, _) :-
	close_perfmeter.

init(Type, Reset) :-
	get_flag(enable_interrupts, Flag),
	set_flag(enable_interrupts, off),
	block(init(Type, Reset, Flag), X, reset(X, Flag)).

init(Type, Reset, Flag) :-
	(Type == gc ->
	    get_value(Type, Value)	% to setval gc
	;
	    true
	),
	get_value(Type, Value),
	getval(bitmap, B),
	send(@B, clear),
	(Reset == reset ->
	    (Value > 0 ->
		YMax is 10^fix(ln(2*Value)/2.30259)
	    ;
		YMax = 1
	    )
	;
	    getval(y_max, YMax)
	),
	set_y_max(YMax),
	getval(ysize, Y),
	setval(type, Type),
	send(@perfmeter_text, string, Type),
	G is Y - Y * Value // YMax,
	setval(last, G),
	update,
	set_flag(enable_interrupts, Flag).

start(T) :-
	set_timer(real, 0),
	get(@perfmeter_pic, area, area(X1, Y1, X2, Y2)),
	Xs is X2 - X1,
	Ys is Y2 - Y1 + 21,
	(Xs =:= getval(xsize),
	Ys =:= getval(ysize) ->
		true
	;
		resize(Xs, Ys, cont)
	),
	speed_pce(1),
	set_timer(real, T).

stop :-
	set_timer(real, 0).

update :-
	get_flag(enable_interrupts, Flag),
	set_flag(enable_interrupts, off),
	block(update1(Flag), X, reset(X, Flag)).

update1(Flag) :-
	getval(y_max, YMax),
	getval(type, Type),
	get_value(Type, Value),
	getval(ysize, Y),
	getval(xsize, X),
	Yb is Y - 20,
	G is Yb * Value // YMax,	% value scaled to bitmap height
	getval(last, Last),
	getval(bitmap, Old),
	new(@B, bitmap(X, Yb)),
	send(@B, clear),
	New0 is Yb - G - 1,		% Y coord of the new point
	Ym is Yb // 2,			% Y coord of the middle
	Xoff is X - 1,
	(New0 < 0 ->
		expand(YMax, Ym, Yb, @B, Xoff, New0, Last, New),
		Line = false,
		setval(last_overflow, 0)
	;
	New0 > Ym ->			% below the middle point
		incval(last_overflow),
		(getval(last_overflow) > 3*X/4 ->
			contract(YMax, Yb, @B, Xoff, New0, Last, New),
			Line = false,
			setval(last_overflow, 0)
		;
			Line = true,
			New = New0
		)
	;
		New = New0,
		Line = true,
		setval(last_overflow, 0)
	),
	(send(@Old, clip_area, area(1, 0, Xoff, Yb)),
	send(@B, copy, @Old) ->
	    send(@B, set, point(Xoff, New))
	;
	    true
	),
	(Line == false ->
		true
	;
	    (Last - New > 1 ->
		    make_line(New, Last, @B, Xoff)
	    ;
	    New - Last > 1 ->
		    make_line(Last, New, @B, Xoff)
	    ;
		    true
	    )
	),
	setval(bitmap, B),
	send(@Old, free),
	send(@perfmeter_pic, display, @B),
	flush(pce_out),
	setval(last, New),
	set_flag(enable_interrupts, Flag).

resize(XSize, YSize, Reset) :-
	Yb is YSize - 20,
	new(@B, bitmap(XSize, Yb)),
	send(@B, clear),
	setval(bitmap, B),
	setval(last_overflow, 0),
	setval(xsize, XSize),
	setval(ysize, YSize),
	send(@perfmeter_size, free),
	send(@perfmeter_text, free),
	send(@perfmeter_k, free),
	Yt is Yb + 3,
	new(@perfmeter_text, text('', 5, Yt, left)),
	send(@perfmeter_text, font, font(courier, roman, 10)),
	Xt is XSize - 9,
	new(@perfmeter_size, text('', Xt, Yt, right)),
	send(@perfmeter_size, font, font(courier, roman, 10)),
	Xk is XSize - 2,
	new(@perfmeter_k, text('', Xk, Yt, right)),
	send(@perfmeter_k, font, font(courier, roman, 10)),
	send(@perfmeter_pic, display, @perfmeter_text),
	send(@perfmeter_pic, display, @perfmeter_size),
	send(@perfmeter_pic, display, @perfmeter_k),
	getval(type, Type),
	init(Type, Reset).

reset(Tag, Flag) :-
	set_flag(enable_interrupts, Flag),
	exit_block(Tag).

get_value('Global stack', Value) :-
	statistics(global_stack_used, Value).
get_value('Local stack', Value) :-
	statistics(local_stack_used, Value).
get_value('Control stack', Value) :-
	statistics(control_stack_used, Value).
get_value('Trail', Value) :-
	statistics(trail_stack_used, Value).
get_value('Code heap', Value) :-
	statistics(code_heap_used, Value).
get_value('General heap', Value) :-
	statistics(general_heap_used, Value).
get_value('Collections', Value) :-
	statistics(gc_number, GC),
	Value is GC - getval(gc),
	setval(gc, GC).

perfmeter_menu(_, global) :-
	init('Global stack', reset).
perfmeter_menu(_, local) :-
	init('Local stack', reset).
perfmeter_menu(_, control) :-
	init('Control stack', reset).
perfmeter_menu(_, trail) :-
	init('Trail', reset).
perfmeter_menu(_, gc) :-
	init('Collections', reset).
perfmeter_menu(_, 'general heap') :-
	init('General heap', reset).
perfmeter_menu(_, 'code heap') :-
	init('Code heap', reset).

set_y_max(Max) :-
	setval(y_max, Max),
	(Max >= 1000 ->
	    YMax is Max // 1000,
	    send(@perfmeter_k, string, k),
	    send(@perfmeter_size, string, YMax)
	;
	    send(@perfmeter_k, string, ''),
	    send(@perfmeter_size, string, Max)
	).

expand(YMax, Ym, Yb, B, X, New0, Last, New) :-
	NewMax is YMax * 2,
	NewLast is Ym + Last//2,
	New1 is Ym + New0//2,
	(New1 < 0 ->
		NewYm is Ym * 2,
		expand(NewMax, NewYm, Yb, B, X, New1, NewLast, New)
	;
		set_y_max(NewMax),
		setval(last, NewLast),
		make_dotted_line(New1, Last, B, X),
		New = New1
	).

contract(YMax, Yb, B, X, New0, Last, New) :-
	NewMax is YMax // 2,
	NewLast is 2 * Last - Yb,
	New is 2 * New0 - Yb,
	set_y_max(NewMax),
	setval(last, NewLast),
	make_dotted_line(Last, New, B, X).

make_line(From, To, B, X) :-
	S is From,
	E is To,
	make_line(S, E, 1, B, X).

make_dotted_line(From, To, B, X) :-
	(From < To ->
	    S is From - 8,
	    E is To + 8
	;
	    E is From + 8,
	    S is To - 8
	),
	make_line(S, E, 2, B, X).

make_line(Start, End, Incr, B, X) :-
	(Start < End ->
	    send(B, set, point(X, Start)),
	    S is Start + Incr,
	    make_line(S, End, Incr, B, X)
	;
	    true
	).

:-      getval(flags, flags(DC, VN)),
	set_flag(debug_compile, DC),
	set_flag(variable_names, VN),
	erase_array(flags).
