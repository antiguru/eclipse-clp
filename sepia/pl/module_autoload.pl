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
% Copyright (C) 1990-2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): ECRC GmbH
% Contributor(s): IC-Parc, Imperal College London
% 
% END LICENSE BLOCK
%
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: module_autoload.pl,v 1.1 2006/09/23 01:55:29 snovello Exp $
% ----------------------------------------------------------------------

/*
 * SEPIA PROLOG SOURCE MODULE
 *
 * IDENTIFICATION:	module_autoload.pl 
 *
 * AUTHOR:		Joachim Schimpf
 *
 * DESCRIPTION:		Autoloading mechanism for modules
 *
 *	This utility is described in more detail in the report
 *	"A Guide to SEPIA Customisation and Advanced Programming"
 *	(ECRC TR-LP-50)
 *
 *	The idea is to give the declaration part of a module, ie. the
 *	module directive, all export, global, tool and external declarations
 *	together with a module_body_file/1 directive to tell the system
 *	in what file the actual definitions are.
 *	When one of the declared (but still undefined) predicates is called,
 *	an appropriate error handler compiles the file that holds the
 *	definitions and re-calls the goal.
 *
 *	Note: There is no need for the definition part to repeat the module
 *	directive. If it does, all declarations must be repeated as well,
 *	since compiling a module directive erases the module.
 *
 * CONTENTS:
 *
 *	module_body_file(+File)
 *		specify where the module mody is located
 *
 *	load_module_handler/3
 *		error handler for #68 (calling an undefined procedure)
 *
 */


%
% Predicates for compiling modules on demand
%

:- module(module_autoload).

:- global module_body_file/1.
:- dynamic not_yet_loaded/2.
:- tool(module_body_file/1, module_body_file/2).
 
module_body_file(File, Module) :-
        retract_all(not_yet_loaded(_, Module)),
        assert(not_yet_loaded(File, Module)).    

:- import get_flag_body/4 from sepia_kernel.
 
load_module_handler(_, Goal, Caller) :-
        functor(Goal, N, A),
        get_flag_body(N/A, definition_module, Module, Caller),
        not_yet_loaded(File, Module),
        !,
        compile(File, Module),
        retract_all(not_yet_loaded(_, Module)),
        call(Goal, Caller).
load_module_handler(Err, Goal, Caller) :-
        error(default(Err), Goal, Caller).
 
:- set_error_handler(68, load_module_handler/3).
