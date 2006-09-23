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
% Version:	$Id: khs.pl,v 1.1 2006/09/23 01:55:25 snovello Exp $
% ----------------------------------------------------------------------

/*
 * SEPIA PROLOG SOURCE MODULE
 */

/*
 * GRAPHICS
 *
 * IDENTIFICATION:      khs.pl 
 *
 * AUTHOR:		Micha Meier, partly from code by Kenji Watanabe
 *
 * DESCRIPTION:         This library changes the pce interface to a KHS
			interface.

 * CONTENTS:		Predicates to redefine in the kegi module.

 */

:- begin_module(kegi).

:- pragma(system).

:- local
	get_flag/2.

:- import
	get_flag_body/3
    from sepia_kernel.

:- tool(get_flag/2, khs_get_flag/3).

:- export
	reset_khs/0,
	close_khs/0,
	close_gp/0,
	open_khs/0,
	open_khs/1,
	open_gp/0,
	open_gp/1,
	khs_demo/0.
	khs_flag/2.

reset_khs :-
	printf(pce_out, "send(@khs,async,1)%n%b", []).

close_khs :- close_pce.

close_gp  :- close_pce.

khs_demo :-
	get_flag(installation_directory, Inst),
	concat_string([Inst, "/khs/demo/demo.pl"], Demo),
	compile(Demo),
	demo_go.

pce_setup:-
	setup_interrupt_handlers,
	set_flag(print_depth, 1000),
	speed_pce(0),		%switch back to slow mode
	reset_khs.

khs_flag(Flag, Val) :-
	(var(Flag); var(Val)),
	!,
	get_khs_flag(Flag, Val).
khs_flag(Flag, Val) :-
	set_khs_flag(Flag, Val).

get_khs_flag(type, Val) :-
	getval(khs_type, Val).
get_khs_flag(display, Val) :-
	getval(khs_display, Val).

set_khs_flag(type, Val) ?-
	setval(khs_type, Val).
set_khs_flag(display, Val) ?-
	setval(khs_display, Val).

khs_get_flag(sepiadir, D, _) :-
	get_flag(installation_directory, I),
	concat_string([I, "/khs"], D).
khs_get_flag(khs_type, Val, _) :-
	get_khs_flag(type, Val).
khs_get_flag(extension, kegi, M) :-
	get_flag_body(extension, kegi_xview, M).
khs_get_flag(A, B, M) :-
	get_flag_body(A, B, M).

object(@Object, Term):-
	writeq(object(@Object), pce_out),
	nl(pce_out), flush(pce_out),	% nl and flush to pce is necessary
	read(Term, pce_in),
	Term \== pred_fail.

new(Object, Term):-
	var(Object),
	!,
	writeq(newv(Term), pce_out),
	nl(pce_out), flush(pce_out),	% nl and flush to pce is necessary
	read(Object, pce_in),
	Object \== pred_fail.
new(@Object, Term):-
	var(Object),
	!,
	writeq(newv(Term), pce_out),
	nl(pce_out), flush(pce_out),	% nl and flush to pce is necessary
	read(@Object, pce_in).
new(@Object, Term):-
	writeq(new(@Object, Term), pce_out),
	pce_replied_obj.		% check to see if reply was an object


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
	writeq(send2(Object, Behaviour), pce_out),
	pce_replied_yes.		% check to see if reply was yes

send2(_, _, []).
send2(Object, Behaviour, Value):-
	writeq(send(Object, Behaviour, Value), pce_out),
	pce_replied_yes.		% check to see if reply was yes

fsend(Object, Behaviour):-
	writeq(send2(Object, Behaviour), pce_out),
	pce_replied_yes.		% check to see if reply was yes

fsend(Object, Behaviour, Value) :-
	writeq(send(Object, Behaviour, Value), pce_out),
	pce_replied_yes.		% check to see if reply was yes


get(Object, Behaviour, Result):-
	var(Result),
	!,
	writeq(get(Object, Behaviour), pce_out),
	nl(pce_out), flush(pce_out),	% nl and flush to pce is necessary
	read(Result, pce_in),
	Result \== pred_fail.
get(Object, Behaviour, @Result):-
	!,
	writeq(getr(Object, Behaviour), pce_out),
	nl(pce_out), flush(pce_out),	% nl and flush to pce is necessary
	read(@Result, pce_in),
	Result \== pred_fail.
get(Object, Behaviour, Result):-
	writeq(get(Object, Behaviour), pce_out),
	nl(pce_out), flush(pce_out),	% nl and flush to pce is necessary
	read(Result, pce_in),
	Result \== pred_fail.

:- setval(khs_type, 1).

open_khs:-
	init_running("PCE"),
	guibin(Pce),
	start_pce(Pce).

open_khs(Hostname):-
	init_running("PCE"),
	guibin(Pce),
	start_pce(Hostname, Pce).


open_pce :-
	open_khs.

open_pce(H) :-
	open_khs(H).

open_gp:-
	setval(khs_type,3),
	open_khs.

open_gp(Hostname):-
	setval(khs_type,3),
	open_khs(Hostname).

guibin(B) :-
	getval(khs_type, N),
	guibin(B, N).

guibin(khs1, 1).
guibin(khs2, 2).
guibin(gpo, 3) :-
	setval(gui, o).

:- system_debug.

reset_pce_exit(Tag) :-
	reset_khs -> exit_block(Tag) ; exit_block(Tag).

read_message(Goal) :-
	read(message(Object, Behaviour, Value), pce_msg),
	Goal =.. [Behaviour, Object, Value].

untraced_reset_pce :- reset_khs.

:- skipped
	read_message/1,
	untraced_reset_pce/0.

:- untraceable
	read_message/1,
	untraced_reset_pce/0.
