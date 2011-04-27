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
% Copyright (C) 2010 Cisco Systems, Inc.  All Rights Reserved.
% 
% END LICENSE BLOCK
%
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: iso_strict.ecl,v 1.1 2011/04/27 12:25:57 jschimpf Exp $
% ----------------------------------------------------------------------

%
% ECLiPSe PROLOG LIBRARY MODULE
%
% IDENTIFICATION:	iso_strict.pl
%
% AUTHOR:		Joachim Schimpf
%
% CONTENTS:		see export directive
%
% DESCRIPTION:		Strict ISO 13211-1 standard langauge module
%

% TODO:
%	- restrict arithmetic functions in expansion and is/2
%	- disallow non-standard directives
%	- disallow ?-/1 queries
%	- document implementation-defined things
%	- strictly, dynamic and discontiguous should only be directives

:- module(iso_strict).

:- export
	chtab(0'`, string_quote),
	chtab(0'", list_quote).

:- comment(categories, [`Compatibility`]).
:- comment(summary, `Strict ISO Prolog compatibility library`).
:- comment(author, `Joachim Schimpf`).
:- comment(copyright, 'Cisco Systems, Inc').
:- comment(date, `$Date: 2011/04/27 12:25:57 $`).
:- comment(see_also, [library(multifile),lib(iso)]).
:- comment(desc, html('
    This library aims at providing a "strict mode" as required by
    ISO 13211-1.  Standard Prolog is defined in ISO/IEC 13211-1
    (Information Technology, Programming Languages, Prolog,
    Part 1: General Core, 1995).
    <P>
    A pure ISO-program should be contained in a separate module starting
    with a directive like
    <PRE>
    :- module(myisomodule, [], iso_strict).
    </PRE>
    The effect of the compatibility library is local to this module.
    No Eclipse-specific language constructs will be available.
    ')).

:- reexport
   	dynamic/1,			% directives
	discontiguous/1,
	op/3,
	true/0,				% built-ins
	fail/0,
	call/1,
	!/0,
	(',')/2,
	(;)/2,
	(->)/2,
	(=)/2,				% 8.2
	(\=)/2,
	var/1,				% 8.3
	atom/1,
	integer/1,
	float/1,
	atomic/1,
	compound/1,
	nonvar/1,
	number/1,
	(@=<)/2,			% 8.4
	(==)/2,
	(\==)/2,
	(@<)/2,
	(@>)/2,
	(@>=)/2,
	functor/3,			% 8.5
	arg/3,
	(=..)/2,
	copy_term/2,
	(is)/2,				% 8.6
	(=:=)/2,			% 8.7
	(=\=)/2,
	(<)/2,
	(=<)/2,
	(>)/2,
	(>=)/2,
	clause/2,			% 8.8
	current_predicate/1,
	asserta/1,			% 8.9
	assertz/1,
	retract/1,
	findall/3,			% 8.10
	bagof/3,
	setof/3,
	open/3,				% 8.11
	open/4,
	close/1,
	nl/0,				% 8.12
	nl/1,
	read_term/2,			% 8.14
	read_term/3,
	read/1,
	read/2,
	write_term/2,
	write_term/3,
	write/1,
	write/2,
	writeq/1,
	writeq/2,
	write_canonical/1,
	write_canonical/2,
	current_op/3,
	(\+)/1,				% 8.15
	once/1,
	repeat/0,
	atom_length/2,			% 8.16
	char_code/2,
	halt/0,				% 8.17

	(:)/2,				% for modules

	op(_,_,(:-)),			% operators
	op(_,_,(-->)),
	op(_,_,(?-)),
	op(_,_,(;)),
	op(_,_,(->)),
	op(_,_,(',')),
	op(_,_,(=)),
	op(_,_,(\=)),
	op(_,_,(==)),
	op(_,_,(\==)),
	op(_,_,(@<)),
	op(_,_,(@>)),
	op(_,_,(@=<)),
	op(_,_,(@>=)),
	op(_,_,(=..)),
	op(_,_,(is)),
	op(_,_,(=:=)),
	op(_,_,(=\=)),
	op(_,_,(<)),
	op(_,_,(>)),
	op(_,_,(=<)),
	op(_,_,(>=)),
	op(_,_,(+)),
	op(_,_,(-)),
	op(_,_,(/\)),
	op(_,_,(\/)),
	op(_,_,(/)),
	op(_,_,(//)),
	op(_,_,(*)),
	op(_,_,(mod)),
	op(_,_,(rem)),
	op(_,_,(<<)),
	op(_,_,(>>)),
	op(_,_,(^)),
	op(_,_,(-)),
	op(_,_,(\))

   from eclipse_language.


:- export
	syntax_option(not(nl_in_quotes)),
	syntax_option(iso_escapes),
	syntax_option(iso_base_prefix),
	syntax_option(iso_restrictions),
	syntax_option(plus_is_no_sign),
	syntax_option(doubled_quote_is_quote),
	syntax_option(no_array_subscripts),
	syntax_option(bar_is_no_atom),
	syntax_option(no_attributes),
	syntax_option(no_curly_arguments),
	syntax_option(blanks_after_sign),
	syntax_option(float_needs_point),
	syntax_option(limit_arg_precedence).


:- export
	op(200, xfx, (**)).

:- reexport
	(**)/3,				% HIDE
	(abolish)/1,
	at_end_of_stream/0,
	at_end_of_stream/1,
	atom_concat/3,
	atom_codes/2,
	atom_chars/2,
	catch/3,
	catch/4,			% HIDE
	ceiling/2,			% HIDE
	char_conversion/2,
	close/2,
	current_char_conversion/2,
	current_input/1,
	current_output/1,
	current_prolog_flag/2,
	ensure_loaded/1,
	float_integer_part/2,		% HIDE
	float_fractional_part/2,	% HIDE
	floor/2,			% HIDE
	flush_output/0,
	flush_output/1,
	get_byte/1,
	get_byte/2,
	get_char/1,
	get_char/2,
	get_code/1,
	get_code/2,
	halt/1,
	initialization/1,
	iso_recover/4,			% HIDE
	log/2,				% HIDE
	number_chars/2,
	number_codes/2,
	peek_byte/1,
	peek_byte/2,
	peek_char/1,
	peek_char/2,
	peek_code/1,
	peek_code/2,
	put_byte/1,
	put_byte/2,
	put_code/1,
	put_code/2,
	round/2,			% HIDE
	set_input/1,
	set_output/1,
	set_prolog_flag/2,
	set_stream_position/2,
	sign/2,				% HIDE
	stream_property/2,
	sub_atom/5,
	throw/1,
	truncate/2,			% HIDE
	unify_with_occurs_check/2
    from iso.

:- reexport multifile.

:- ensure_loaded(library(iso_error)).
