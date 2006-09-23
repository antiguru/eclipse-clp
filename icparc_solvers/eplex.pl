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
% Contributor(s): Joachim Schimpf, Kish Shen, IC-Parc
% 
% END LICENSE BLOCK
%
% Description:	ECLiPSe/CPLEX interface
%
% System:	ECLiPSe Constraint Logic Programming System
% Author/s:	Joachim Schimpf, IC-Parc
%               Kish Shen,       IC-Parc
% Version:	$Id: eplex.pl,v 1.1 2006/09/23 01:53:26 snovello Exp $

:- module(eplex). % needed to keep comment processing happy

:- ( current_module(s_eplex) ->
       writeln(warning_output, "Standalone eplex should be loaded directly"
               " with lib(eplex)."),
       ensure_loaded(eplex_standalone)
   ; current_module(range_eplex) ->
       writeln(error, "Range eplex no longer supported."),
       abort
   ; current_module(ic_eplex) ->
       writeln(error, "IC eplex no longer supported."),
       abort
   ;
	ensure_loaded(eplex_standalone) % default 
   ).

%------------------------------------------------------------------------

:- comment(summary, "Interface to external Simplex or MIP solvers").
:- comment(author, "Joachim Schimpf and Kish Shen").
:- comment(date, "$Date: 2006/09/23 01:53:26 $").
:- comment(copyright, "Cisco Systems, Inc.").

:- comment(include, s_eplex_comments).

