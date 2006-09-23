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
% Version:	$Id: inst.pl,v 1.1 2006/09/23 01:55:20 snovello Exp $
% ----------------------------------------------------------------------

%
% SEPIA PROLOG SOURCE MODULE
%
% IDENTIFICATION:	inst.pl
%
% DESCRIPTION:		Utilities for ECLiPSe installation
%

:- set_error_handler(133, true/0).	% suppress loading messages
:- set_error_handler(139, true/0).
:- lib(strings).			% load before we change cd

%
% Set up installation date and number
% It reads the old version file and overwrites it with new data.
%

init_version(NewFile):-
	date(D),			% the installation date
	substring(D, 1, 16, D1),
	substring(D, 20, 5, D2),
	concat_strings(D1, D2, DD),

	open(NewFile, write, V),	% create the version file
	writeq(V, sepia_date(DD)), writeln(V, .),
	writeq(V, sepia_patch(".pre")), writeln(V, .),
	close(V).

update_version(VersionFile, NewFile):-
	(compile(VersionFile) -> true),	% update the patch identifier % ptags
	sepia_patch(OldPatch),
	(VersionFile = NewFile ->
		OldPatch = Patch
	;
	    update_patch(OldPatch, Patch)
	),

	date(D),			% the installation date
	substring(D, 1, 16, D1),
	substring(D, 20, 5, D2),
	concat_strings(D1, D2, DD),

	open(NewFile, write, V),	% overwrite the version file
	writeq(V, sepia_date(DD)),
	write(V, '.\n'),
	writeq(V, sepia_patch(Patch)),
	write(V, '.\n'),
	close(V).

update_patch(OldPatch, NewPatch) :-
	get_flag(version, Version),
	printf("\nThe current version is %s%s\n", [Version, OldPatch]),
	printf("\nThe current patch identifier is %q\n", [OldPatch]),
	write("Do you want to specify a different one? (y/n) "),
	flush(output),
	tyi(A),
	nl,
	( A == 0'y ->
		write("\nPlease type the new patch string\n"),
		write("(without quotes, followed by <return>): "),
		flush(output),
		read_string(end_of_line, _, Patch),
		update_patch(Patch, NewPatch)
	; A == 0'n ->
		OldPatch = NewPatch
	;
		update_patch(OldPatch, NewPatch)
	).


dump_file(File) :-
	set_flag(variable_names,off),
	dump(File).

dump_file_del(File) :-
	exists(File),
	!,
	set_flag(variable_names,off),
	dump(File),
	delete(File).
dump_file_del(_).

dump_all :-
	read_directory(., "*.pl", _, F),
	set_flag(variable_names,off),
	dump(F).

dump_all_del :-
	read_directory(., "*.pl", _, F),
	set_flag(variable_names,off),
	dump(F),
	( member(File, F),
	  delete(File),
	  fail
	;
	  true
	).

