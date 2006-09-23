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
% Version:	$Id: k_pce.pl,v 1.1 2006/09/23 01:55:27 snovello Exp $
% ----------------------------------------------------------------------

/*****************************************************************************
* IDENTIFICATION	k_pce.pl
* DESCRIPTION		control predicates for pce interface
* CONTENTS		this is for all systems
* AUTHOR		Philip Kay ECRC GmbH
*****************************************************************************/


/*****************************************************************************
* compiled declarations
*****************************************************************************/
:- system.

:- begin_module(kegi).
:- import
	set_default_error_handler/2,
	error_handler/2,
	system_error_handler/4,
	export_body/2,
	it_handler/1,
	(block)/4
    from sepia_kernel.

:-  (getval(kegi_sepia, 3) -> 
	(import get_flag_body/4,
		untraced_call/2 from sepia_kernel),
	local_record(font),
	local_record(font_sizes),
	local_record(font_styles),
	local_record(font_families)
    ;
	(import get_flag_body/4 from db)
    ).

:- export
	reset_pce/0,
	close_pce/0,
	make_callback/1,
	new/2, 
	send/2,
	send/3, 
	fsend/2,
	fsend/3, 
	get/3, 
	object/1, 
	object/2.


/*****************************************************************************
* enable user callbacks to be visible only in kegi module
*****************************************************************************/
:- tool(make_callback/1, make_callback/2).

% make_callback cannot handle name clashes because PCE knows nothing
% about modules, otherwise we could just export the callback and
% use call_explicit/2 to call it.
make_callback(_, kegi) :- !.
make_callback(Pred, Module):-
	getval(kegi_sepia, 3),
	!,
	% Check if we already see this procedure from that module
	(get_flag(Pred, definition_module, Module),
	 get_flag(Pred, defined, on) ->
	    true
	;
	% Check if we see it from another module
	get_flag(Pred, definition_module, Module1),
	Module \== Module1,
	(get_flag(Pred, defined, on) ->
	    error(330, "Name clash in callback procedures")
	;
	    abolish(Pred),
	    fail
	) ->
	    fail
	;
	    (get_flag_body(Pred, visibility, exported, Module) ->
		true
	    ;
		export_body(Pred, Module)
	    ),
	    (get_flag(Pred, visibility, imported) ->
		true
	    ;
		(import Pred from Module)
	    )
	).
make_callback(Pred, Module):-
	(is_predicate(Pred) ->
		true
	;
		export_body(Pred, Module),
		(import Pred from Module)
	).


/*****************************************************************************
* define error and interrupt handlers
*****************************************************************************/
:- set_default_error_handler(330, error_handler/2).
:- set_default_error_handler(331, error_handler/2).
:- reset_error_handler(330).
:- reset_error_handler(331).

kegi_error_handler(_, init_pce_remote(_, _)):- 
	!, kegi_general_error.
kegi_error_handler(_, init_pce_local(_)):- 
	!, kegi_general_error.
kegi_error_handler(_, writeq(_, pce_out)):-
	!, kegi_general_error.
kegi_error_handler(_, read(_, _)):-
	!, kegi_general_error.
kegi_error_handler(_, writeq_(pce_out, _, kegi)):-
	!, kegi_general_error.
kegi_error_handler(_, read_(_, _, kegi)):-
	!, kegi_general_error.
kegi_error_handler(_, pce_has_data):- !.
kegi_error_handler(E, Goal):-
	E == 170 ->
	    system_error_handler(E, Goal, kegi, kegi)
	;
	    error_handler(E, Goal)
	.

kegi_general_error:-
	close_pce,
	error(330, "PCE is not currently running").

kegi_pipe_error:-
	close_pce,
	error(330, "PCE connection has been closed illegally").

:- set_default_error_handler(170, kegi_error_handler/2).
:- set_default_error_handler(192, kegi_error_handler/2).
:- set_default_error_handler(193, kegi_error_handler/2).
:- reset_error_handler(170).
:- reset_error_handler(192).
:- reset_error_handler(193).

close_pce:-
	is_running("PCE"),
	(speed_pce(0) -> true; true),		% ensure connection is in slow mode
	stop_running("PCE"),
	unset_interrupt_handlers.

setup_interrupt_handlers:-
	((is_running("CGI 2D") ; is_running("CGI TTY 2D")) ->
		true
	;
		set_interrupt_handler(pipe, kegi_pipe_error/0), 
		set_interrupt_handler(io, pce_look/0)
	).

unset_interrupt_handlers:-
	((is_running("CGI 2D") ; is_running("CGI TTY 2D")) ->
		true
	;
		set_interrupt_handler(pipe, true/0),
		set_interrupt_handler(io, true/0),
		close_and_wait_for_pce,
		set_interrupt_handler(pipe, it_handler/1),
		set_interrupt_handler(io, it_handler/1)
	).

/*****************************************************************************
* this is all the pce related stuff for starting and handling interrupts etc
*****************************************************************************/
pce_setup:-
	setup_interrupt_handlers,
	set_flag(print_depth, 1000),
	get(@pce, window_system, X),
	kegi_dir(Dir),
%	speed_pce(1),		%do as quickly as possible
	pce_get_fonts(X),
	pce_standard_icons(X, Dir), 
	pce_standard_cursors(X, Dir), 
	pce_olwm_cursors(X, Dir), 
	speed_pce(0),		%switch back to slow mode
	reset_pce.

font(A, B, C) :-
	recorded(font, font(A, B, C)).

font_families(F) :-
	recorded(font_families, F).

font_styles(F) :-
	recorded(font_styles, F).

font_sizes(F) :-
	recorded(font_sizes, F).

pce_get_fonts(sunview):-
	!,
	record(font_styles, [bold, roman]),
	record(font_sizes, [7, 10, 11, 12, 14, 16, 18, 19, 24]),
	record(font_families, [apl, cmr, courier, gallant, screen, serif]),
	enumerate(@fonts, Fonts),
	record_them(Fonts).
pce_get_fonts(xview):-
	record(font_styles, [bold, roman]),
	record(font_sizes, [7, 8, 10, 11, 12, 14, 18, 19, 24]),
	record(font_families, [courier, lucida, gallant, screen, serif]),
	enumerate(@fonts, Fonts),
	record_them(Fonts).

record_them([]).
record_them([H|T]):-
	record(font, H),
	record_them(T).

pce_standard_icons(X, Dir):-
	load_new_icon(X, Dir, @mac_off, 'mac_off.marker'), 
	load_new_icon(X, Dir, @mac_on, 'mac_on.marker'), 
	load_new_icon(X, Dir, @toggle_on, 'toggle_on.marker'), 
	load_new_icon(X, Dir, @toggle_off, 'toggle_off.marker'), 
	load_new_icon(X, Dir, @kegi_icon, 'kegi.icon'), 
	load_new_icon(X, Dir, @question_icon, 'question.icon'), 
	load_new_icon(X, Dir, @stop_icon, 'stop.icon'), 
	load_new_icon(X, Dir, @skull_icon, 'skull.icon'), 
	load_new_icon(X, Dir, @warning_icon, 'warning.icon'), 
	load_new_icon(X, Dir, @print_icon, 'print.icon'), 
	load_new_icon(X, Dir, @noprint_icon, 'noprint.icon').

pce_standard_cursors(X, Dir):-
	load_new_cursor(X, Dir, @lightening_cursor, 'lightening.cursor'), 
	load_new_cursor(X, Dir, @glasses_cursor, 'glasses.cursor'), 
	load_new_cursor(X, Dir, @point_left_cursor, 'point_left.cursor'), 
	load_new_cursor(X, Dir, @hourglass_cursor, 'hourglass.cursor').

pce_olwm_cursors(sunview, _):-
	!.
pce_olwm_cursors(xview, Dir):-
	load_new_cursor(xview, Dir, @busy_cursor, 'busy.cursor'),
	load_new_cursor(xview, Dir, @move_cursor, 'move.cursor').

load_new_icon(sunview, Dir, Name, File):-
	!,
	concat_atom([Dir, '/graphic/sicon/', File], Icon),
	new(Name, bitmap(0, 0)),
	send(Name, load_icon, Icon).
load_new_icon(xview, Dir, Name, File):-
	concat_atom([Dir, '/graphic/xicon/', File], Icon),
	new(Name, bitmap(0, 0)),
	send(Name, load_icon, Icon).

load_new_cursor(sunview, Dir, Cursor, File):-
	!,
	concat_atom([Dir, '/graphic/sicon/', File], Icon),
	new(Bitmap, bitmap(0, 0)),
	send(Bitmap, load_icon, Icon),
	new(Cursor, cursor(Bitmap)).
load_new_cursor(xview, Dir, Cursor, File):-
	concat_atom([Dir, '/graphic/xicon/', File], Icon),
	new(Bitmap, bitmap(0, 0)),
	send(Bitmap, load_icon, Icon),
	new(Cursor, cursor(Bitmap)).

:- tool(untraced_block/3, (block)/4).

% the next part must be compiled in debug mode to enable tracing
% of the user interrupt handler which is called in pce_collect/0.
:- system_debug.

pce_look:-
	is_running("PCE"),		%to avoid 2d conflicts
	untraced_block(pce_collect, Tag, reset_pce_exit(Tag)).

reset_pce_exit(Tag) :-
	reset_pce -> exit_block(Tag) ; exit_block(Tag).

if_version(3, (
	pce_collect:-			% take care of untraceability
		(pce_has_data ->	% if-then-else is better in 3.0
		    read_message(Goal),
		    (untraced_call(Goal,kegi) -> pce_collect ; pce_collect)
		;
		    untraced_reset_pce
		)
),(
	pce_collect:-			% traceability does not matter
		pce_has_data, !,	% don't use if-then-else
		read(message(Object, Behaviour, Value), pce_msg),
		Goal =.. [Behaviour, Object, Value],
		(call(Goal) -> pce_collect ; pce_collect)
)).
if_version(3, (
	read_message(Goal) :-
		read(message(Object, Behaviour, Value), pce_msg),
		Goal =.. [Behaviour, Object, Value]
),(
	pce_collect:-
		reset_pce
)).
if_version(3, (
	untraced_reset_pce :- reset_pce
),(
	:- true
)).
if_version(3, (
	:- (skipped read_message/1, untraced_reset_pce/0),
	   (untraceable read_message/1, untraced_reset_pce/0)
),(
	:- true
)).


:- system.	% switch off the system_debug mode

/*****************************************************************************
* Establishes whether an object exists
* Object(+Object)
* Object(+Object, ?Class)
*	 +Object	The name of the object
*	 ?Class		The returned class definition
*****************************************************************************/
object(@Object):-
	object(@Object, _).

object(@Object, Term):-
	writeq(pce_object(@Object), pce_out),
	nl(pce_out), flush(pce_out),	% nl and flush to pce is necessary
	read(Term, pce_in),
	Term \== pred_fail.


/*****************************************************************************
* establishes a new object
* new(-Object, +Term)
*	 -Object	Returns the identifier for the object
*	 +Term		Type of object requested
*****************************************************************************/
new(Object, Term):-
	var(Object),
	!,
	writeq(pce_newv(Term), pce_out),
	nl(pce_out), flush(pce_out),	% nl and flush to pce is necessary
	read(Object, pce_in),
	Object \== pred_fail.
new(@Object, Term):-
	var(Object),
	!,
	writeq(pce_newv(Term), pce_out),
	nl(pce_out), flush(pce_out),	% nl and flush to pce is necessary
	read(@Object, pce_in).
new(@Object, Term):-
	writeq(pce_new(@Object, Term), pce_out),
	pce_replied_obj.		% check to see if reply was an object


/*****************************************************************************
* Sends a list of behaviours to a valid object or list of objects
* send(+[Objects], +[Behavoiurs])
* send(+[Objects], +[Behavoiurs], +Value)
*	 +[Object]	List of objects to be sent to
*	 +[Behaviour]	List of behavoiurs to be changed
*	 +Value	  	The value to change the behaviour to
*****************************************************************************/
send([Head|Tail], Behaviour):-
	!,
	send1(Head, Behaviour),
	send(Tail, Behaviour).
send([], _):- !.
send(Object, Behaviour):-
	send1(Object, Behaviour).

send1(Object, [Behaviour:Value|Tail]):-
	!,
	send1(Object, Behaviour, Value),
	send(Object, Tail).
send1(Object, [Behaviour|Tail]):-
	!,
	send1(Object, Behaviour),
	send(Object, Tail).
send1(_, []):-!.
send1(Object, Behaviour):-
	writeq(pce_send2(Object, Behaviour), pce_out),
	pce_replied_yes.		% check to see if reply was yes

send([Head|Tail], Behaviour, Value):-
	!,
	send1(Head, Behaviour, Value),
	send(Tail, Behaviour, Value).
send([], _, _):- !.
send(Object, Behaviour, Value):-
	send1(Object, Behaviour, Value).

send1(Object, [Behaviour|Tail], Value):-
	!,
	send2(Object, Behaviour, Value),
	send1(Object, Tail, Value).
send1(_, [], _):- !.
send1(Object, Behaviour, Value):-
	send2(Object, Behaviour, Value).

send2(_, _, []).
send2(Object, Behaviour, Value):-
	writeq(pce_send(Object, Behaviour, Value), pce_out),
	pce_replied_yes.		% check to see if reply was yes

fsend(Object, Behaviour):-
	writeq(pce_send2(Object, Behaviour), pce_out),
	pce_replied_yes.		% check to see if reply was yes

fsend(Object, Behaviour, Value) :-
	writeq(pce_send(Object, Behaviour, Value), pce_out),
	pce_replied_yes.		% check to see if reply was yes


/*****************************************************************************
* gets a behaviour from a valid object
* get(+Object, +Behavoiur, ?Result)
*	 +Object	The name of the object
*	 +Behaviour  	The behaviour requested
*	 ?Result	The returned value from the object
*****************************************************************************/
get(Object, Behaviour, Result):-
	var(Result),
	!,
	writeq(pce_get(Object, Behaviour), pce_out),
	nl(pce_out), flush(pce_out),	% nl and flush to pce is necessary
	read(Result, pce_in),
	Result \== pred_fail.
get(Object, Behaviour, @Result):-
	!,
	writeq(pce_getr(Object, Behaviour), pce_out),
	nl(pce_out), flush(pce_out),	% nl and flush to pce is necessary
	read(@Result, pce_in),
	Result \== pred_fail.
get(Object, Behaviour, Result):-
	writeq(pce_get(Object, Behaviour), pce_out),
	nl(pce_out), flush(pce_out),	% nl and flush to pce is necessary
	read(Result, pce_in),
	Result \== pred_fail.


/*****************************************************************************
* low-level primitives which used to be in C
*****************************************************************************/

:- make_local_array(speed).

:- setval(speed, 0).

init_pce_local(Pce) :-
    concat_string([Pce, " 1 0 3"], Exec),
    exec(Exec, [pce_out, pce_in, null, out(sigio(pce_msg))], _).

reset_pce :-
    printf(pce_out, "pce_send(@pce, async, 1)%n%b", []).

pce_set_speed(Speed) :-
    (Speed = 1 ; Speed = 0),
    !,
    getval(speed, OldSpeed),
    (Speed \== OldSpeed ->
	printf(pce_out, "%n%bpce_send(@pce, fast, %d)%n%b", [Speed]),
	(Speed == 0 ->
	    read(yes, pce_in)
	;
	    true
	),
	setval(speed, Speed)
    ;
	true
    ).


pce_replied_yes :-
    printf(pce_out, "%n", []),
    getval(speed, Speed),
    (Speed = 0 ->
	flush(pce_out),
	read(yes, pce_in)
    ;
	true
    ).

pce_replied_obj :-
    printf(pce_out, "%n", []),
    getval(speed, Speed),
    (Speed = 0 ->
	flush(pce_out),
	read(Token, pce_in),
	Token \== pred_fail
    ;
	true
    ).

pce_has_data :-
    select([pce_msg], 0, [pce_msg]).

close_and_wait_for_pce :-
    close(pce_in),
    close(pce_out),
    close(pce_msg),
    wait(_, _), 
    !.

init_pce_remote(Host, Pce) :-
    socket(internet, datagram, S),
    connect(S, Host/3311),
    printf(S, "%s%b", [Pce]),
    read_token(S, Port1, _),
    read_token(S, Port2, _),
    close(S),
    socket(internet, stream, pce_in),
    bind(pce_in, _/0),
    connect(pce_in, Host/Port1),
    socket(internet, stream, pce_out),
    bind(pce_out, _/0),
    connect(pce_in, Host/Port1),
    socket(internet, stream, sigio(pce_msg)),
    bind(pce_msg, _/0),
    connect(pce_msg, Host/Port2).

/*****************************************************************************
* compiled declarations
*****************************************************************************/
:- skipped
	reset_pce/0,
	reset_pce_exit/1,
	close_pce/0,
	make_callback/2,
	new/2, 
	send/2,
	send/3, 
	fsend/2,
	fsend/3, 
	get/3, 
	object/1, 
	object/2.

:- untraceable
	pce_look/0,
	pce_collect/0,
	is_running/1,
	pce_has_data/0,
	reset_pce_exit/1,
	untraced_block/3.

/*****************************************************************************
* End of k_pce.pl
*****************************************************************************/
