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
% Copyright (C) 1993-2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): ECRC GmbH
% 
% END LICENSE BLOCK
%
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: facile.pl,v 1.1 2006/09/23 01:55:15 snovello Exp $
% ----------------------------------------------------------------------

%
% SEPIA PROLOG SOURCE MODULE
%
% IDENTIFICATION:       facile.pl
%
% DESCRIPTION:  Interface to the Facile system.
%
% CONTENTS:     
%	connect_facile(+Hostname, -FID)
%	new_channel(+FID, -Channel)
%	publish_channel(+FID, +Channel, +Name, -Status)
%	find_channel(+FID, +Name, -Channel, -Status)
%	send(+FID, +ToChannel, +Data, ?ChannelData, +Timeout, -Status)
%	recv(+FID, +FromChannel, +Timeout, -Data, -ChannelData, -Status)
%	choose_recv(+FID, +FromChannel1, +FromChannel2, +Timeout,
%		-Data, -ChannelData, -Which, -Status)
%	choose_send(+FID, +ToChannel1, +ToChannel2, +Data1, ?ChannelData1,
%		+Data2, ?ChannelData2, +Timeout, -Which, -Status)
%	             

:- module_interface(facile).

:- export
	connect_facile/2,
	new_channel/2,
	publish_channel/4,
	find_channel/4,
	send/6,
	recv/6,
	choose_recv/8,
	choose_send/10.
:- external(connect_facile/2).

:- begin_module(facile).

:- use_module(library(foreign)).
:- import
	term_to_bytes/2,
	bytes_to_term/2
    from sepia_kernel.

%
% Foreign declarations that generate automatically an interface
% to the Facile C file.
%
foreign(ct_facile_channels_connect, c,
	connect_facile(+string, [-address(ct_object)])).
foreign(ct_facile_channels_chan, c,
	channel(+address(ct_object), -integer, -integer)).
foreign(ct_facile_channels_publish_channel, c,
	publish_channel(+address(ct_object), +integer, +integer,
		+user(ct_string), -integer)).
foreign(ct_facile_channels_find_channel, c,
	find_channel(+address(ct_object), +user(ct_string), -integer, -integer,
		-integer)). 
foreign(ct_facile_channels_transmit, c,
	send(+address(ct_object), +integer, +integer, +user(ct_string),
		+integer, +integer, +integer, -integer)).
foreign(ct_facile_channels_recv, c,
	recv(+address(ct_object), +integer, +integer, +integer,
	-user(ct_string), -integer, -integer, -integer)).
foreign(ct_facile_channels_choose_recv, c,
	choose_recv(+address(ct_object), +integer, +integer, +integer, +integer,
		+integer, -user(ct_string), -integer, -integer, -integer,
		-integer)).
foreign(ct_facile_channels_choose_send, c,
	choose_send(+address(ct_object), +integer, +integer, +integer, +integer,
	    +user(ct_string), +integer, +integer, +user(ct_string), +integer,
	    +integer, +integer, -integer, -integer)).

foreign_user_type(+ct_string, I, 'ct_string *', LocalDecl, '', InConv, Call,
    '', '') :-
	concat_string(["ct_string\tct", I, ';'], LocalDecl),
	concat_string(['Name_To_Ctstring(val', I, ', tag', I, ', ct', I, ')'],
		InConv),
	concat_string(['&ct', I], Call).
foreign_user_type(-ct_string, I, 'ct_string *', LocalDecl,
    'Check_Output_String', InConv, Call, OutConv, Unif) :-
	concat_string(['value\t\tvct', I, ';\n\tct_string\tout', I, ';'], LocalDecl),
	concat_string(['out', I, '.buf = 0;\n\tout', I, '.length = 0;'], InConv),
	concat_string(['&out', I], Call),
	concat_string(['Ctstring_To_Prolog(vct', I, ', out', I, ');'], OutConv),
	concat_string(['Unify_String(val', I, ', tag', I, ', vct', I, '.ptr)'],
		Unif).

% connect_facile(+Hostname, -FID)
%	external

% new_channel(+FID, -Channel)
new_channel(FID, facile_channel(A, B)) :-
	channel(FID, A, B).

% publish_channel(+FID, +Channel, +Name, -Status)
publish_channel(FID, facile_channel(A, B), Name, Status) :-
	publish_channel(FID, A, B, Name, Status).

% find_channel(+FID, +Name, -Channel, -Status)
find_channel(FID, Name, facile_channel(A, B), Status) :-
	find_channel(FID, Name, A, B, Status).

% send(+FID, +ToChannel, +Data, ?ChannelData, +Timeout, -Status)
send(FID, facile_channel(A, B), Data, ChannelData, Timeout, Status) :-
	term_to_bytes(Data, String),
	channel_data(ChannelData, C, D),
	send(FID, A, B, String, C, D, Timeout, Status).

% recv(+FID, +FromChannel, +Timeout, -Data, -ChannelData, -Status)
recv(FID, facile_channel(A, B), Timeout, Data, facile_channel(C, D), Status) :-
	recv(FID, A, B, Timeout, String, C, D, Status),
	data_to_term(String, Data).

% choose_recv(+FID, +FromChannel1, +FromChannel2, +Timeout,
%	-Data, -ChannelData, -Which, -Status)
choose_recv(FID, facile_channel(A1, B1), facile_channel(A2, B2), Timeout,
    Data, facile_channel(C, D), Which, Status) :-
	choose_recv(FID, A1, B1, A2, B2, Timeout, String, C, D, Which, Status),
	data_to_term(String, Data).

% choose_send(+FID, +ToChannel1, +ToChannel2, +Data1, ?ChannelData1,
%	+Data2, ?ChannelData2, +Timeout, -Which, -Status)
choose_send(FID, facile_channel(A1, B1), facile_channel(A2, B2),
    Data1, ChannelData1, Data2, ChannelData2, Timeout, Which, Status) :-
	term_to_bytes(Data1, String1),
	term_to_bytes(Data2, String2),
	channel_data(ChannelData1, C1, D1),
	channel_data(ChannelData2, C2, D2),
	choose_send(FID, A1, B1, A2, B2, String1, C1, D1, String2, C2, D2,
	Timeout, Which, Status).

channel_data(X, -1, -1) :-
	var(X).
channel_data(facile_channel(A, B), C, D) :-
	-?->
	A = C,
	B = D.

data_to_term("", []) :- !.
data_to_term(String, Term) :-
	bytes_to_term(String, Term).

:- (exists('e_interface.c') ->
	get_flag(hostarch, Arch),
	cd(..),
	cd(Arch),
	(get_flag(object_suffix, "o") ->
	    load_foreign_files([
		'channels.client.gen.o',
		'/home/magic/local/contract2/lib/sun4/ct_c_runtime.o'
		], [
		'/home/magic/local/contract2/lib/sun4/libcontract.a'
		])
	;
	    load_foreign_files(['facile.so'], [])
	)
    ;
	make_simple_interface(['ct_private.h', 'facile.h'])
    ).
