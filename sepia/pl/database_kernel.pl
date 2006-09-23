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
% Copyright (C) 1992-2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): ECRC GmbH
% Contributor(s): IC-Parc, Imperal College London
% 
% END LICENSE BLOCK
%
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: database_kernel.pl,v 1.1 2006/09/23 01:55:10 snovello Exp $
% ----------------------------------------------------------------------

/*****************************************************************************

File   : database_kernel.pl
Author : Michael Dahmen
Content: Database - Prolog interface : Prolog defined part

Note   : This file is for the MegaLog-Sepia integration. It was forked
	 from "@(#)bang_builtins.pl  1.13         2/26/92"

	 The file has been modified such that the MegaLog compatibility 
	 package for Sepia (megalog.pl) is not needed. Predicates
	 that use metacalls are declared as tools.

In client module do :

:- import database_kernel.

*****************************************************************************/


:- begin_module(database_kernel).	% module already created in C !
:- pragma(system).

/* transaction/1 and bang_retrieve_delete/4 take a goal as argument */
:- pragma(debug).
:- tool(transaction/1,transaction/2).
:- pragma(nodebug).
:- tool(bang_retrieve_delete/4,bang_retrieve_delete_body/5).
:- tool(bang_retrieve_delete_db/5,bang_retrieve_delete_db_body/6).

/* bang_[select|join|diff] take a rule as argument */
/* The new versions with _db allow to specify the database,
** but: The version exists only with action function, which
** has to be filled with '0' if the user don't want to use 
** this parameter.
** Ex: bang_select_db(Rel, CondT, ProjL, RelOut, 0, Database)
*/
:- tool(bang_select/5, bang_select_body/6).
:- tool(bang_select_db/6, bang_select_db_body/7).
:- tool(bang_join/6, bang_join_body/7).
:- tool(bang_join_db/7, bang_join_db_body/8).
:- tool(bang_diff/6, bang_diff_body/7).
:- tool(bang_diff_db/7, bang_diff_db_body/8).



/*
** C externals - they are created and exported by
** the C initialisation. Some must be made local now.
*/

:- local
	bang_all_relations/2,
	bang_all_temp_relations/2,
	'$start_trans'/0,
	'$commit_trans'/0,
	'$undo_trans'/0,
	'$bang_select'/6,
	'$bang_join'/7,
	'$bang_diff'/7,
	bang_retr_prepare/5,
	bang_retr_page/3,
	bang_del_prepare/5,
	bang_del_tuple/3,
	bang_del_previous/2,
	bang_del_more/2.

/*
** Built-ins with database handle argument
*/

:- export
%	bang_arity_db/3,
%	bang_attribute_db/4, 
%	bang_cardinality_db/3,
%	bang_createrel_db/4, 
%	bang_delete_db/3, 
%	bang_delete_tup_db/3,
%	bang_delete_tup_db/4,
%	bang_destroyrel_db/2,
	bang_diff_db/6,
	bang_diff_db/7,
	bang_diff_db_body/8,		% for ondb/2
%	bang_exist_db/3,
%     	bang_existrel_db/2,
%	bang_format_db/3,
%	bang_format_db/4,
%	bang_insert_db/3,
%	bang_insert_db/4,
	bang_join_db/6,
	bang_join_db/7,
	bang_join_db_body/8,		% for ondb/2
%	bang_free_cursor_db/1,
%	bang_register_db/3,
%	bang_renamerel_db/3,
%	bang_renamerel_db/4,
	bang_retrieve_db/4,
	bang_retrieve_delete_db/4,
	bang_retrieve_delete_db/5,
	bang_retrieve_delete_db_body/6,	% for ondb/2
	bang_retrieve_lazy_db/4,
%	bang_retrieve_list_db/4,
	bang_select_db/5,
	bang_select_db/6, 
	bang_select_db_body/7, 		% for ondb/2
%	closedb/1,
%	createdb/2,
	current_relation/2,
	current_temp_relation/2,
%	current_time/2,
%	database_parameter/2,
%	delta_time/2,
%	destroydb/1,
%	destroy_temprels/1,
%	opendb/2,
%	resource/3,
%	statistics_bang/0,
%	statistics_lock/0,
%	statistics_desc/1,
%	statistics_relation/2,

	transaction/1, 
	transaction_start/0, transaction_commit/0, transaction_undo/0.


/*
** Built-ins without database handle argument,
** just mapped to their counterpart with handle.
*/

:- export
	bang_arity/2,
	bang_attribute/3, 
	bang_cardinality/2,
	bang_createrel/3, 
	bang_delete/2, 
	bang_delete_tup/2,
	bang_delete_tup/3,
	bang_destroyrel/1,
	bang_diff/5,
	bang_diff/6,
	bang_exist/2,
     	bang_existrel/1,
	bang_format/2,
	bang_format/3,
	bang_insert/2,
	bang_insert/3,
	bang_join/5,
	bang_join/6,
	bang_free_cursor/0,
	bang_register/2,
	bang_renamerel/2,
	bang_renamerel/3,
	bang_retrieve/3,
	bang_retrieve_delete/3,
	bang_retrieve_delete/4,
	bang_retrieve_lazy/3,
	bang_retrieve_list/3,
	bang_select/4,
	bang_select/5, 
	closedb/0,
	createdb/1,
	current_relation/1,
	current_temp_relation/1,
	destroydb/0,
	destroy_temprels/0,
	opendb/1,
	statistics_desc/0,
	statistics_relation/1.


/*
** versions of the C builtins without database handle
*/

opendb(Database)  :-
	opendb(Database, 0).

createdb(Database) :-
	createdb(Database, 0).

closedb  :-
	closedb(0).

destroydb :-
	destroydb(0).

destroy_temprels  :-
	destroy_temprels(0).

bang_createrel( X, Y, Z)  :-
	bang_createrel_db( X, Y, Z, 0).

bang_renamerel(Oldname, Newname) :-
	bang_renamerel_db(Oldname, Newname, 0).
bang_renamerel(Oldname, Newname, AttNameList)  :-
	bang_renamerel_db(Oldname, Newname, AttNameList, 0).

bang_existrel(Name)  :-
	bang_existrel_db(Name, 0).

bang_destroyrel( Name)  :-
	bang_destroyrel_db( Name, 0).

bang_arity(Relname, Number )  :-
	bang_arity_db(Relname, Number, 0).

bang_cardinality(Relname, Number )  :-
	bang_cardinality_db(Relname, Number, 0).
 
bang_attribute( Relname, Position, Format) :-
	bang_attribute_db( Relname, Position, Format, 0).
 
bang_format (RelName, Format) :-
	bang_format_db(RelName, Format,0).
bang_format (RelName, Format, Atts) :-
	bang_format_db (RelName, Format, Atts, 0).

bang_insert ( Relname, Tuple) :-
	bang_insert_db ( Relname, Tuple, 0).
bang_insert ( Relname, Tuple, Status)   :-
	bang_insert_db ( Relname, Tuple, Status, 0).

bang_delete_tup (X, Y )  :-
	bang_delete_tup_db (X, Y ,0).
bang_delete_tup (X, Y, Z)  :-
	bang_delete_tup_db (X, Y , Z, 0).

bang_retrieve_list (Relname, CondTree, List) :-
	bang_retrieve_list_db(Relname, CondTree, List, 0).

bang_exist( Rel, CondTree) :-
	bang_exist_db( Rel, CondTree, 0).

bang_delete(Rel, CondTree)  :-
	bang_delete_db(Rel, CondTree, 0).

bang_free_cursor  :-
	bang_free_cursor_db(0).

bang_register( Reg, Val)  :-
	bang_register_db( Reg, Val, 0).

statistics_desc  :-
	statistics_desc( 0).

statistics_relation( Name)  :-
	statistics_relation(Name,  0).


/*
** Transaction(Goal, Module)
**
** Calls 'Goal' in a transaction context. This include that
**	- 'Goal' is executed deterministic i.e. implicit cut
**	- if 'Goal' fails transaction aborts
**	- if 'Goal' succeeds transactions commits
**	- if transaction is aborted by lock manager 'Goal' is restarted
**
** In case of a deadlock the lock manager invokes exit_block(abort_transaction),
** which is caught by the block/3 statement below. If the transaction is 
** aborted due to  a deadlock it will be started again, however only N times, 
** where N is a number given below (currently 10).
**
** This is a tool predicate, the caller provides a module argument.
*/

/*
in sepia/events.pl
'$transaction_deadlock'(317,_) :- exit_block(abort_transaction).

:- set_error_handler(317,'$transaction_deadlock'/2).
*/

:- import untraced_call/2 from sepia_kernel.
:- import block/4 from sepia_kernel.

transaction(Goal,Module) :-
	transaction(Goal,Module,10).

transaction(Goal,Module,N) :-
	block(	(
			'$start_trans',
			untraced_call(Goal,Module)
		),
		abort_transaction,
		transaction_catch(Goal,Module,N),
		database_kernel
	     ) ->
		'$commit_trans'.

transaction_catch(Goal,Module,N) :-
	transaction_dec(Goal,Module,N,N1) ->
		transaction(Goal,Module,N1).


transaction_dec(Goal,Module,N,N1) :-
	(N > 1 ->
		N1 is N - 1
	;
		error(320, transaction(Goal), Module)
		/* ERROR_CODE(320) : TOO MANY DEADLOCKS */
	).

:- untraceable
	transaction/2,
	transaction/3,
	transaction_catch/3,
	transaction_dec/4.

/*
** Alternative transaction syntax (backward compatibility)
**
** transaction_start.
** transaction_commit.
** transaction_undo.
**
** The use of this interface requires that the exit_block(abort_transaction)
** is handled by the user.
*/

transaction_start  :- '$start_trans'.
transaction_commit :- '$commit_trans'.
transaction_undo   :- '$undo_trans'.


/*
** current_relation(Name/Arity)
**
** Enumerates all permanent relations in the database.
*/

current_relation(RelName) :-
	current_relation(RelName, 0).

current_relation(Name/Arity, Database) :-
	bang_all_relations(NameList, Database),
	member(Name,NameList),
	bang_arity_db(Name,Arity, Database).

/*
** current_temp_relation(Name/Arity)
**
** Enumerates all temporary relations in the database.
*/

current_temp_relation(RelName) :-
	current_temp_relation(RelName, 0).

current_temp_relation(Name/Arity, Database) :-
	bang_all_temp_relations(NameList, Database),
	member(Name,NameList),
	bang_arity_db(Name,Arity, Database).


/*
** bang_retrieve_lazy(Rel, CondTree, LazyList)
** 
** This currently does not work when failure occurs across
** a bang_retr_page, since the cursor is not backtrackable.
**
** It would be better if bang_retr_page/3 would return
** a difference list directly.
*/

bang_retrieve_lazy(Rel, CondTree, LazyList) :-
	bang_retrieve_lazy_db(Rel, CondTree, LazyList, 0).

bang_retrieve_lazy_db(Rel, CondTree, LazyList, Database) :-
	bang_retr_prepare(Rel, _Tuple, CondTree, Cursor, Database),
	bang_retrieve_lazy_cursor(Cursor, LazyList, Database).

delay bang_retrieve_lazy_cursor(_, LazyList, Database) if var(LazyList).
bang_retrieve_lazy_cursor(Cursor, LazyList, Database) :-
	bang_retr_page(Cursor, TupleList, Database),
	( TupleList == [] ->
	    LazyList = []
	;
	    bang_retrieve_lazy_cursor(Cursor, Tail, Database),
	    list_to_dlist(TupleList, LazyList, Tail)
	).

list_to_dlist([], Tail, Tail).
list_to_dlist([X|Xs], [X|Ys], Tail) :-
	list_to_dlist(Xs, Ys, Tail).


/*
** bang_retrieve(Rel, Tuple, CondTree)
**
** Tuple at a time retrieval. Actually done on a page by page basis.
*/

bang_retrieve(Rel, Tuple, CondTree) :-
	bang_retrieve_db(Rel, Tuple, CondTree, 0).

bang_retrieve_db(Rel, Tuple, CondTree, Database) :-
	/* schedule cursor removal for cut and fail is done in 'C' */
        bang_retr_prepare(Rel, Tuple, CondTree, Cursor, Database),
        bang_retrieve_1(Cursor, Tuple,[], Database).

bang_retrieve_1(_, Tuple, TupleList, _Database) :-
	member(Tuple, TupleList).
bang_retrieve_1(Cursor, Tuple, _, Database) :-
	bang_retr_page(Cursor, TupleList, Database),
	TupleList \= [],
	bang_retrieve_1(Cursor, Tuple, TupleList, Database).

/*
** bang_retrieve_delete(Rel, Tuple, CondTree)
** bang_retrieve_delete(Rel, Tuple, CondTree, Test, Module)
**
** Tuple at a time retrieval with deletion. The second form executes
** a test (any Prolog goal) before the deletion and deletes only
** if the test succeeds. bang_retrieve_delete/4 is declared as tool,
** the caller provides the Module where to execute the Test.
*/

bang_retrieve_delete(Rel, Tuple, CondTree) :-
	bang_retrieve_delete_db_body(Rel, Tuple, CondTree, true, 0, database_kernel).

bang_retrieve_delete_db(Rel, Tuple, CondTree, Database) :-
	bang_retrieve_delete_db_body(Rel, Tuple, CondTree, true, Database, database_kernel).

bang_retrieve_delete_body(Rel, Tuple, CondTree, Goal, Module) :-
	bang_retrieve_delete_db_body(Rel, Tuple, CondTree, Goal, 0, Module).

bang_retrieve_delete_db_body(Rel, Tuple, CondTree, Goal, Database, Module) :-
	/* schedule cursor removal for cut and fail is done in 'C' */
        bang_del_prepare(Rel, Tuple, CondTree, Cursor, Database),
        bang_retrieve_delete_1(Cursor, Tuple, Goal, Module, Database).

bang_retrieve_delete_1(Cursor, Tuple, Goal, Module, Database) :-
	bang_del_tuple(Cursor, Tuple, Database),
	% extra test on Tuple is possible here, not deleted yet
	call((Goal,!), Module),
	bang_del_previous(Cursor, Database).     % now it is deleted
bang_retrieve_delete_1(Cursor, Tuple, Goal, Module, Database) :-
	bang_del_more(Cursor, Database),
	bang_retrieve_delete_1(Cursor, Tuple, Goal, Module, Database).



/* Some explanation on what's going on there :

   - bang_retr_prepare/5 rsp. bang_del_prepare/5 create a cursor i.e.
     allocate memory and open relation
   - bang_retr_page/3 retrieves a page from a given cursor
   - bang_del_tuple/3 retrieves a tuple from a given cursor
   - bang_del_previous/2 deleted the last tuple retrieved with a cursor
   - bang_del_more/2 checks whether the cursor given has moved thru

   The important point here is that a cursor must always be removed
   after the last access to the relation for a call of bang_retrieve/3.

   The difficulty is to known which access is the last one. Definitly
   if bang_retrieve_1/3 fails the last tuple is delivered. The last tuple 
   is however also delivered, if there comes a cut sometimes after the call 
   to bang_retrieve/3. This cut can not be detected with standard
   Prolog methods and therefore it would no be possible to free the 
   memory areas and close the relation. 
   
   This problem is handled on the 'C' level. When the cursor is created
   a cursor removal for cut and fail is scheduled. This implementation
   uses a Sepia feature (schedule_cut_fail_action()) that was developed
   as part of the MegaLog integration. The MegaLog implementation was
   slightly different (see file bang_builtins.pl) but the semantics is
   the same.
*/

/*
** Support for action function handling
*/

bang_action(action(Module,Head,Body),Tuple) :-
	arg(1,Head,Tuple),
	call(Body,Module).

:- mode create_action(?,?,-).
create_action(0,_,0) :- !.
create_action((H :- B),Module,action(Module,H,B)) :- !.
create_action(Action,_,_) :- error(5,Action).	% type error


/*
** Support for set oriented operation :
** Create the output relation if required according to projection list.
**
** RelOut is atom/string
**		if already exists okay
**		if not exist relation is created as temporary
** RelOut is vairable
**              relation is created as temporary
**
** RelIn is atom/string for one input relation
** RelIn is list [R1,R2] for two input relations
*/

create_output(_RelIn,_ProjL,RelOut, _Database) :- 
	% case 1 : no output relation required
	RelOut == 0,
	!.
create_output(_RelIn,_ProjL,RelOut, Database) :-
	% case 2 : relation name given and relation exists
	nonvar(RelOut),
	bang_existrel_db(RelOut, Database),
	!.
create_output(RelIn,[],RelOut, Database) :-
	% case 3 : output relation to be created with all attributes
        !,
        full_format(RelIn, FormatList, Database),
        bang_createrel_db(RelOut, FormatList, [temporary], Database).
create_output(RelIn,ProjL,RelOut, Database) :-
	% case 4 : output relation to be created with projected attributes
	projection_format(RelIn, ProjL, FormatList, Database),
        bang_createrel_db(RelOut, FormatList, [temporary], Database).

/* Auxiliary : get a full format list for one or two relations */

full_format([R1,R2],F, Database) :-
	!,
	bang_format_db(R1,F1, Database),
	bang_format_db(R2,F2, Database),
	append(F1,F2,F),
	!.
full_format(R,F, Database) :- 
	bang_format_db(R,F, Database).

/* Auxiliary : get a projected format list for one or two relations */

projection_format(_, [], [], _) :- 
	!.
projection_format([R1,R2], [att(1,AttId)|More],[Format|MoreFormat], Database) :-
	!,
	bang_attribute_db(R1, AttId, Format, Database),
	projection_format([R1,R2], More,MoreFormat, Database).
projection_format([R1,R2], [att(2,AttId)|More],[Format|MoreFormat], Database) :-
	!,
	bang_attribute_db(R2, AttId, Format, Database),
	projection_format([R1,R2], More,MoreFormat, Database).
projection_format(R, [att(1,AttId)|More],[Format|MoreFormat], Database) :-
	bang_attribute_db(R, AttId, Format, Database),
	projection_format(R, More,MoreFormat, Database).


/*
** Set oriented operations : selection, join, difference
**
** They all exist in two form, depending on what is to be done with the
** output relation (see above).
**
** bang_select(Rel, CondT, ProjL, RelOut, Action, Module)
** bang_select_temp(Rel, CondT, ProjL, RelOut, Action, Module)
** bang_join(R1, R2, CondT, ProjL, RelOut, Action, Module)
** bang_join_temp(R1, R2, CondT, ProjL, RelOut, Action, Module)
** bang_diff(R1, R2, CondT, ProjL, RelOut, Action, Module)
** bang_diff_temp(R1, R2, CondT, ProjL, RelOut, Action, Module)
**
** The last two arguments are optional and may be left out if no action
** is to be performed. The predicates are defined as tools, so the caller
** will provide the module argument. Action must be a rule or the integer 0 
** to indicate that there is no rule.
*/


bang_select(Rel, CondT, ProjL, RelOut) :-
	bang_select_db_body(Rel, CondT, ProjL, RelOut, 0, 0, _).

bang_select_db(Rel, CondT, ProjL, RelOut, Database) :-
	bang_select_db_body(Rel, CondT, ProjL, RelOut, 0, Database, _).

bang_select_body(Rel, CondT, ProjL, RelOut, Action, Module) :-
	bang_select_db_body(Rel, CondT, ProjL, RelOut, Action, 0, Module).

bang_select_db_body(Rel, CondT, ProjL, RelOut, Action, Database, Module) :-
	create_output(Rel,ProjL,RelOut, Database),
	create_action(Action,Module,AC),
	'$bang_select'(Rel, CondT, ProjL, RelOut, AC, Database).


bang_join(R1, R2, CondT, ProjL, RelOut) :-
	bang_join_db_body(R1, R2, CondT, ProjL, RelOut, 0, 0, _).

bang_join_db(R1, R2, CondT, ProjL, RelOut, Database) :-
	bang_join_db_body(R1, R2, CondT, ProjL, RelOut, 0, Database, _).

bang_join_body(R1, R2, CondT, ProjL, RelOut, Action, Module) :-
	bang_join_db_body(R1, R2, CondT, ProjL, RelOut, Action, 0, Module).

bang_join_db_body(R1, R2, CondT, ProjL, RelOut, Action, Database, Module) :-
	create_output([R1,R2],ProjL,RelOut, Database),
	create_action(Action,Module,AC),
        '$bang_join'(R1, R2, CondT, ProjL, RelOut, AC, Database).


bang_diff(R1, R2, CondT, ProjL, RelOut) :-
	bang_diff_db_body(R1, R2, CondT, ProjL, RelOut, 0, 0, _).

bang_diff_db(R1, R2, CondT, ProjL, RelOut, Database) :-
	bang_diff_db_body(R1, R2, CondT, ProjL, RelOut, 0, Database, _).

bang_diff_body(R1, R2, CondT, ProjL, RelOut, Action, Module) :-
	bang_diff_db_body(R1, R2, CondT, ProjL, RelOut, Action, 0, Module).

bang_diff_db_body(R1, R2, CondT, ProjL, RelOut, Action, Database, Module) :-
	create_output(R1,ProjL,RelOut, Database),
	create_action(Action,Module,AC),
        '$bang_diff'(R1, R2, CondT, ProjL, RelOut, AC, Database).

