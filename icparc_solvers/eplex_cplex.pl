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
% Copyright (C) 1995 - 2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): Joachim Schimpf, IC-Parc
% 
% END LICENSE BLOCK

% ----------------------------------------------------------------------
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: eplex_cplex.pl,v 1.1 2006/09/23 01:53:26 snovello Exp $
%
% Description:		ECLiPSe/CPLEX interface
%
% Authors:		J.Schimpf, IC-Parc
% ----------------------------------------------------------------------

:- module(eplex_cplex, [], [empty_language]).

:- comment(summary, "Load lib(eplex) with the CPLEX solver").
:- comment(author, "Joachim Schimpf, IC-Parc").
:- comment(copyright, "Cisco Systems, Inc.").
:- comment(date, "$Date: 2006/09/23 01:53:26 $").

% eplex setup checks for existence of module eplex_cplex!
:- local initialization(ensure_loaded(eplex)).
:- export initialization(import(eplex)).

