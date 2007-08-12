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
% Version:	$Id: dynamic.pl,v 1.2 2007/08/12 19:40:41 jschimpf Exp $
% ----------------------------------------------------------------------

/*
 * SEPIA PROLOG SOURCE MODULE
 */

/*
 * IDENTIFICATION:	dynamic.pl
 *
 * DESCRIPTION: 	This file contains all the Prolog predicates
 *			that handle dynamic predicates.
 *
 * CONTENTS:     
 *
 * REVISION HISTORY:
 *
 * AUTHOR	VERSION	 DATE	REASON
 * periklis		26.9.89	Major revision for the logical update semantics.
 * micha		20.3.89	Moved all the dynamic-related predicates
 *				from db.pl into this file.
 */

:- begin_module(sepia_kernel).

:- export
	(abolish)/1,
	clause/1,clause/2,
	(dynamic)/1,(is_dynamic)/1,
	(listing)/0,(listing)/1,
	retract/1,
	retract_all/1,
	writeclause/1,
	writeclause/2.

/*
 * TOOL DIRECTIVES
 */

:- tool( (abolish)/1, abolish_body/2).
:- tool( clause/1, clause_body/2).
:- tool( clause/2, clause_body/3).
:- tool( (dynamic)/1, dynamic_body/2).
:- tool( is_dynamic/1, is_dynamic_body/2).
:- tool( (listing)/0, listing_body/1).
:- tool( (listing)/1, listing_body/2).
:- tool( retract/1, retract_body/2).
:- tool( retract_all/1, retract_all_body/2).
:- tool( write_goal/3, write_goal/4).		% exported, for opium

:- system.		% compiler directive to add the SYSTEM flag


% We allow several variants:
%	dynamic n/a
%	dynamic n/a, n/a, n/a		% Sepia
%	dynamic [n/a, n/a, n/a]		% Quintus, ISO, ...

dynamic_body((F1, F2), Module) ?-
	dynamic_body_enum((F1,F2), Module),
	!.
dynamic_body([F|Fs], Module) ?-
	dynamic_body_list([F|Fs], Module),
	!.
dynamic_body(F, Module) :-
	dynamic_body_single(F, Module),
	!.
dynamic_body(Preds, Module) :-
	get_bip_error(E),
	error(E, dynamic(Preds), Module).

    dynamic_body_enum((F1,F2), Module) ?- !,
    	dynamic_body_enum(F1, Module),
    	dynamic_body_enum(F2, Module).
    dynamic_body_enum(F, Module) :-
    	dynamic_body_single(F, Module).

    dynamic_body_list(Fs, _Module) :- var(Fs), !,
    	set_bip_error(4).
    dynamic_body_list([], _Module) :- !.
    dynamic_body_list([F|Fs], Module) :- !,
    	dynamic_body_single(F, Module),
	dynamic_body_list(Fs, Module).
    dynamic_body_list(_, _) :-
    	set_bip_error(5).

    dynamic_body_single(Name/Arity, Module) ?-
    	atom(Name), integer(Arity), Arity >= 0, !,
	dynamic_(Name, Arity, Module).
    dynamic_body_single(Pred, _) :-
	nonground(Pred) -> set_bip_error(4) ; set_bip_error(5).


is_dynamic_body(Functor, Module) :-
	( check_predspec(Functor, Module) ->
		Functor = Name/Arity,
		is_dynamic_(Name, Arity, Module)
	;
		bip_error(is_dynamic(Functor), Module)
	).


% abolish/1 gets rid of the definition of the predicate specified 
% by the argument. Name must be fully instantiated.
% Arity must be fully instantiated (we differ from BSI). 

abolish_body(Name/Arity, Module ) :- !,
	( abolish_(Name,Arity,Module) ->
	    true
	;
	    get_bip_error(Error),
	    error(Error, abolish(Name/Arity), Module)
	).
abolish_body( (F1, F2), Module ) :- !,
        abolish_body( F1, Module ),
        abolish_body( F2, Module ).
abolish_body(Functor, Module ) :-
	error(5, abolish(Functor), Module).


% clause/4 is just an interface to clause/5, works for facts and rules

clause((Head:-Body), Error, Ref, Module) :-
	!,
	clause(Head, Body, Ref, Module, Error).
clause(Fact, Error, Ref, Module) :-
	clause(Fact, true, Ref, Module, Error).


% clause/1 finds clauses whose head unifies with the head of the argument.
% unifies the body of the clause with the body of the argument.

clause_body(Clause, Module) :-
	clause(Clause, Error, _, Module),
	(var(Error) ->
	    true
	;
	    error(Error, clause(Clause), Module)
	).


% clause/2
% clause(Head,Body) <==> clause((Head/Body))

clause_body(Head, Body, Module) :-
	clause(Head, Body, _, Module, Error),
	(var(Error) ->
	    true
	;
	    error(Error, clause(Head, Body), Module)
	).


% retract/1 removes the first clause that matches the argument 
% from the database. It is a backtrackable retract/1 (BSI)

retract_body(Clause, Module) :-
	clause(Clause, Error, Ref, Module),
	(var(Error) ->
	    kill_pair(Ref)
	;
	    error(Error, retract(Clause), Module)
	).


% retract_all/1 retracts from the database all clauses 
% whose head matches the argument

retract_all_body(Head, Module):-
	clause(Head, _, Ref, Module, Error),
	(var(Error) ->
	    kill_pair(Ref),
	    fail
	;
	    !,
	    error(Error, retract_all(Head), Module)
	).
retract_all_body(_, _).


% listing/0, listing/1 prints the definition of a predicate to the standard 
% output. Functor and Arity must be defined. (difference from BSI). 

listing_body(Module):-
	(gen_listing(_, Module) ->
	    true
	;
	    get_bip_error(E),
	    error(E, listing, Module)
	).


listing_body(PredList, Module) :-
	var(PredList),
	!,
	error(4, listing(PredList), Module).
listing_body(PredList, Module) :-
	illegal_unlocked_module(Module, Error),
	!,
	error(Error, listing(PredList), Module).
listing_body((A, B), Module) :-
	!,
	listing_body(A, Module),
	listing_body(B, Module).
listing_body(Name, Module) :-
	atom(Name),	% listing(p) = list all clauses with head p/AnyArity
	!,
	(gen_listing(Name/_, Module) ->
	    true
	;
	    get_bip_error(E),
	    error(E, listing(Name), Module)
	).
listing_body(Name, Module) :-
	check_predspec(Name),		% mail fail with bip_error
	Name = N/A,
	pred_listing(N, A, Module),	% mail fail with bip_error
	!.
listing_body(Name, Module) :-
	bip_error(listing(Name), Module).


% fails on error, setting the global error variable
gen_listing(Pred, Module) :-
	Pred = F/A,
	current_predicate_body(Pred, Module),
	is_dynamic_(F, A, Module),
	proc_flags(Pred, 0, Module, Module), % definition module = Module
	(pred_listing(F, A, Module) ->
	    nl(output),
	    fail
	;
	    !,
	    fail	% bip_error set by pred_listing/3
	).
gen_listing(_, _).

% fails on error, setting the global error variable
pred_listing(Functor,Arity, Module):- 
	functor(Head, Functor, Arity),
	clause(Head, Body, _, Module, Error),
	(var(Error) ->
	    writeclause_body((Head:-Body), Module),
	    fail
	;
	    !,
	    set_bip_error(Error)
	).
pred_listing(_,_,_).

% some predicates to output a clause (used in listing)

writeclause_body(C,M):-	
	writeclause_body(output, C, M).

writeclause_body(Stream, Clause, Module) :-
	write_clause(Stream, Clause, Module),
	put_separated(Stream, 0'., Module),
	nl(Stream).

put_separated(Stream, Char, Module) :-
	get_chtab(Char, Class),
	(
	    get_stream_info(Stream, last_written, LastChar),	% may fail
	    get_chtab(LastChar, Class)@Module
	->
	    put(Stream, 0' )
	;
	    true
	),
	put(Stream, Char).

write_clause(Str, (H?-B), M) ?- !,
	write_bracketed_if_needed(Str, H, 1199, M), 
	write_(Str, ' ?-', M), 
	nl(Str),
	write_goal(Str, B, 2, 1199, M).
write_clause(Str, (H:-B), M) ?- !,
	(B == true ->
		writeq_(Str, H, M)
	;
		write_bracketed_if_needed(Str, H, 1199, M), 
		write_(Str, ' :-', M), 
		nl(Str),
		write_goal(Str, B, 2, 1199, M)
	).
write_clause(Str, (H-->B), M) ?- !,
	write_bracketed_if_needed(Str, H, 1199, M), 
	write_(Str, '-->', M), 
	nl(Str),
	write_goal(Str, B, 2, 1199, M).
write_clause(Str, H, M):-
	writeq_(Str, H, M).


write_goal(Str, Term, Indent, M) :-
	write_goal(Str, Term, Indent, 1200, M).

% be careful not to instantiate input arguments!
write_goal(Str, B, Indent, _Prec, M):-
	var(B), !,
	indent(Str, Indent, M),
	writeq_(Str, B, M).
write_goal(Str, (B,C), Indent, _Prec, M):- !,
	write_goal(Str, B, Indent, 999, M), 
	put(Str, 0',), nl(Str),
	write_goal(Str, C, Indent, 1000, M).
write_goal(Str, (IfThen;D), Indent, _Prec, M) :-
	nonvar(IfThen),
	IfThen = (B->C),
	!,
	Ind1 is Indent+1,
	indent(Str, Indent, M),
	put(Str, 0'(), nl(Str),
	write_goal(Str, B, Ind1, 1049, M), 
	nl(Str),
	indent(Str, Indent, M),
	write_(Str, '->', M), 
	nl(Str),
	write_goal(Str, C, Ind1, 1050, M),
	nl(Str),
	indent(Str, Indent, M),
	put(Str, 0';), nl(Str),
	write_goal(Str, D, Ind1, 1100, M),
	nl(Str),
	indent(Str, Indent, M),
	put(Str, 0')).
write_goal(Str, (B;C), Indent, _Prec, M):- !,
	Ind1 is Indent+1,
	indent(Str, Indent, M),
	put(Str, 0'(), nl(Str),
	write_goal(Str, B, Ind1, 1099, M), 
	nl(Str),
	indent(Str, Indent, M),
	put(Str, 0';), nl(Str),
	write_goal(Str, C, Ind1, 1100, M),
	nl(Str),
	indent(Str, Indent, M),
	put(Str, 0')).
write_goal(Str, (B->C), Indent, _Prec, M):- !,
	Ind1 is Indent+1,
	indent(Str, Indent, M),
	put(Str, 0'(), nl(Str),
	write_goal(Str, B, Ind1, 1049, M), 
	nl(Str),
	indent(Str, Indent, M),
	write_(Str, '->', M), 
	nl(Str),
	write_goal(Str, C, Ind1, 1050, M),
	nl(Str),
	indent(Str, Indent, M),
	put(Str, 0')).
write_goal(Str, (B do C), Indent, _Prec, M):- !,
	Ind1 is Indent+1,
	indent(Str, Indent, M),
	put(Str, 0'(), nl(Str),
	write_goal(Str, B, Ind1, 1099, M), 
	nl(Str),
	indent(Str, Indent, M),
	write_(Str, do, M), 
	nl(Str),
	write_goal(Str, C, Ind1, 1100, M),
	nl(Str),
	indent(Str, Indent, M),
	put(Str, 0')).
write_goal(Str, (-?-> B), Indent, _Prec, M):- !,
	indent(Str, Indent, M),
	write_(Str, '-?->', M), 
	nl(Str),
	write_goal(Str, B, Indent, 1179, M).
write_goal(Str, '{}'(B), Indent, _Prec, M):- !,
	Ind1 is Indent+1,
	indent(Str, Indent, M),
	put(Str, 0'{), nl(Str),
	write_goal(Str, B, Ind1, 1200, M), 
	nl(Str),
	indent(Str, Indent, M),
	put(Str, 0'}).
write_goal(Str, B, Indent, Prec, M):-
	indent(Str, Indent, M),
	write_bracketed_if_needed(Str, B, Prec, M).


% this is just to fix the bugs, better code is in the public domain write.pl

write_bracketed_if_needed(Str, Term, MaxPrec, M) :-
	compound(Term),
	functor(Term, F, A),
	current_op_body(Prec, Assoc, F, M),
	atom_length(Assoc) =:= A + 1,	% Functor is an operator
	Prec > MaxPrec,
	!,				% Term might needs brackets
	put(Str, 0'(),
	writeq_(Str, Term, M),
	put(Str, 0')).
write_bracketed_if_needed(Str, Term, _Prec, M) :-
	writeq_(Str, Term, M).


indent(_, 0, _) :- !.
indent(Str, 1, M) :- !,
	write_(Str, '    ', M).	%  write 4 spaces
indent(Str, N, M) :-
	N >= 2, N1 is N-2,
	write_(Str, '\t', M),
	indent(Str, N1, M).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

?- skipped
	(abolish)/1,
	abolish_body/2,
	clause/1,
	clause/2,
	clause_body/2,
	clause_body/3,
	(dynamic)/1,
	dynamic_body/2,
	is_dynamic/1,
	is_dynamic_body/2,
	(listing)/0,
	(listing)/1,
	listing_body/1,
	listing_body/2,
	retract/1,
	retract_all/1,
	retract_all_body/2,
	retract_body/2,
	writeclause/1,
	writeclause/2,
	writeclause_body/2,
	writeclause_body/3.
