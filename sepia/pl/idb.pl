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
% Version:	$Id: idb.pl,v 1.1 2006/09/23 01:55:20 snovello Exp $
% ----------------------------------------------------------------------

/*
 * SEPIA PROLOG SOURCE MODULE
 */

/*
 * IDENTIFICATION:	idb.pl
 *
 * DESCRIPTION: 	This file contains all the Prolog predicates 
 *			that handle the internal indexed database.
 *
 * CONTENTS:     
 *
 * REVISION HISTORY:
 *
 * AUTHOR	VERSION	 DATE	REASON
 * micha		20.3.89	the file db.pl was split into this file,
 *				dynamic.pl (dynamic predicates) and db.pl
 *				(other database predicates)
 */

/*
 * OTHER DIRECTIVES
 */

:- system.		% compiler directive to add the SYSTEM flag

:- export
	is_record/1,
	current_record/1,
	erase/2,
	rerecord/2,
	recorded/2,
	recorded/3.

:- export
	current_record_body/2,
	erase_body/3,
	recorded_body/3,
	recorded_body/4,
	rerecord_body/3.

:-
	tool(is_record/1, is_record_body/2),
	tool(current_record/1, current_record_body/2),
	tool(erase/2, erase_body/3),
	tool(recorded/2, recorded_body/3),
	tool(recorded/3, recorded_body/4),
	tool(rerecord/2, rerecord_body/3).

is_record_body(Key, Module) :-
	is_record_(Key, visible, [_], Module).

% current_record_body/2 succeeds iff Key is a key of the indexed database
% (This is terribly inefficient if Key is uninstantiated)

current_record_body(Key, Module):-
	var(Key), !,
	current_functor(Functor, Arity, 1, 0),
	functor(Key, Functor, Arity),
	is_record_body(Key, Module).
current_record_body(Key, Module):-
	valid_key(Key) ->
	    is_record_body(Key, Module)
	;
	    bip_error(current_record(Key), Module).


% rerecord_body/3 removes all values associated with the first argument before 
% associating the second argument with the first

rerecord_body(Key, Value, Module):-
	valid_key(Key) ->
	    erase_all_body(Key, Module),
	    recorda_body(Key, Value, Module)
	;
	    bip_error(rerecord(Key, Value), Module).


% recorded_body/3 unifies the second argument with items that have been
% associated with the first through record/2, rerecord/2. 
% Backtracking will unify the second argument with successive values
% associated with the first in the order they were associated.

recorded_body(Key, Value, Module):-
	valid_key(Key) ->
	    recorded_list_body(Key, List, Module),
	    member(Value, List)
	;
	    bip_error(recorded(Key, Value), Module).

% erase_body/3 removes an indexed database entry that has been asserted 
% by record or rerecord. Nothing is said (BSI) on the instantiation of the
% Value, then the first value that matches will be erased. 

erase_body(Key, Value, Module):-
	valid_key(Key) ->
	    get_recordlist(Key, List, [], Module),
	    get_record(List, Value, Ref),
	    !,
	    erase_record(Key, Ref, Module)
	;
	    bip_error(erase(Key, Value), Module).

% recorded_body/4 is like recorded_body/3, but:
% - returns a database refererence that can be used for erase/1
% - immediate update semantics

recorded_body(Key, Value, Ref, Module) :-
	valid_key(Key) ->
	    get_recordlist(Key, List, Ref, Module),
	    get_record(List, Value, Ref)
	;
	    bip_error(recorded(Key, Value, Ref), Module).

:- skipped
	current_record/1,
	current_record_body/2,
	erase/2,
	erase_body/3,
	is_record/1,
	recorded/2,
	recorded_body/3,
	recorded/3,
	recorded_body/4,
	rerecord/2,
	rerecord_body/3.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
