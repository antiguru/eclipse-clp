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
% Version:	$Id: kegi.pl,v 1.1 2006/09/23 01:55:22 snovello Exp $
% ----------------------------------------------------------------------

/*****************************************************************************
* IDENTIFICATION	kegi.pl
* DESCRIPTION		control predicates for the complete kegi system
* CONTENTS		this is for all systems
* AUTHOR		Philip Kay ECRC GmbH
*****************************************************************************/


/*****************************************************************************
* compiled declarations
*****************************************************************************/
:- module_interface(kegi).
:- system.

:- begin_module(kegi).

:- export open_pce/0, open_pce/1.

:- global_op(500, fx,  @).	% introduces pce object reference

:- import
	ensure_loaded/2
    from sepia_kernel.

:- import write / 2 from oldio.
:- import writeq / 2 from oldio.
:- import read / 2 from oldio.
:- import tyo / 2 from oldio.
:- import tyi / 2 from oldio.
:- import seek / 2 from oldio.
:- import at / 2 from oldio.
:- import get_char / 2 from oldio.
:- import get / 2 from oldio.
:- import put_char / 2 from oldio.
:- import put / 2 from oldio.
:- import display / 2 from oldio.
:- import readvar / 3 from oldio.
:- import writeln / 2 from oldio.
:- import print / 2 from oldio.
:- import writeclause / 2 from oldio.

/*****************************************************************************
* compatibility predicate for sepia3.0, sepia2.12 and chip2.0
*****************************************************************************/
:-	get_flag(version, Version),
	atom_string(Version, SVersion),
	(substring(SVersion, "2", 1) ->
		lib(db, db),
		import_from(db, current_predicate_body/2),
		setval(kegi_sepia, 2),
		set_error_handler(138, true/0)
	;
		use_module(library(oldio)),
		(current_predicate(_) -> true ; true),	% force loading
		(import current_predicate_body/2 from sepia_kernel),
		local_record(kegi_running),
		local_record(kegi_history),
		make_local_array(sepiadir),
		make_local_array(kegidir),
		make_local_array(kegi_sepia),
		setval(kegi_sepia, 3)
	).

% simple ifdef facility

:-	assert((t_if(if_version(3, Term, _), Term) :- !)),
	assert((t_if(if_version(2, _, Term), Term))),
	( getval(kegi_sepia,3)->
	    define_macro(if_version/3, t_if/2, [top_only])
	;
	    define_local_macro(if_version/3, t_if/2, [top_only])
	),
	set_flag(macro_expansion, on).


/*****************************************************************************
* pce start predicates
*
* open_pce
* open_pce(+Client)
*    +Client       The name of the local client for graphical display
*
*****************************************************************************/
open_pce:-
	init_running("PCE"),
	start_pce(xpce).

open_pce(Hostname):-
	init_running("PCE"),
	start_pce(Hostname, xpce).

start_pce(Pce):-
	init_pce_local(Pce), 
	pce_setup.

start_pce(Hostname, Pce):-
	init_pce_remote(Hostname, Pce), 
	pce_setup.

init_running(System):-
	not(recorded(kegi_running, System)),
	record(kegi_running, System).

stop_running(System):-
	recorded(kegi_running, System),
	erase(kegi_running, System).

is_running(System):-
	recorded(kegi_running, System).


/*****************************************************************************
* preds to locate sepia and kegi installation directories
*****************************************************************************/
:-  get_flag(installation_directory, S),
    atom_string(A, S),
    setval(sepiadir, A).

% use KEGIDIR if it is set otherwise use /usr/local/KEGI
:-  (getenv("KEGIDIR", S) ->
	atom_string(A, S),
	setval(kegidir, A)
    ;
	setval(kegidir, '/usr/local/KEGI')
    ).	

% override the above for kegi if it has been merged in with sepia
:-  getval(sepiadir, S),
    concat_atoms(S, '/graphic', D),
    (exists(D) ->
    	setval(kegidir, S)
    ;
	true
    ).

% these preds and the arrays are local to the kegi module
:- export sepia_dir/1,
	kegi_dir/1.

sepia_dir(Dir):-
	getval(sepiadir, Dir).

kegi_dir(Dir):-
	getval(kegidir, Dir).

/*****************************************************************************
* End of kegi.pl
*****************************************************************************/

:- compile(library(k_pce)) -> true.	% ptags
:- compile(library(k_gps)) -> true.
