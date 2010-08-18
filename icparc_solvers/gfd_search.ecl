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
% Copyright (C) 2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): Warwick Harvey and Kish Shen, IC-Parc
% 
% END LICENSE BLOCK
%---------------------------------------------------------------------
%
% IC search module.
%
% System:       ECLiPSe Constraint Logic Programming System
% Author/s:     Warwick Harvey, IC-Parc
%               Kish Shen,      IC-Parc
%
%	This module is essentially an extract of the search-related
%	predicates from the RIA module, written by Joachim Schimpf and
%	Stefano Novello; and from the fd_search module, by Helmut Simonis.
%
% This module provides the search-related components of the IC library, a
% combined finite domain and floating point interval propagation solver.
%
%---------------------------------------------------------------------

:- module(gfd_search).

%---------------------------------------------------------------------
%
% Imports and exports.
%

:- comment(categories, ["Constraints"]).
:- comment(summary, "This library provides the search-related components of the IC-library").

:-comment(desc,html("\
  This library provides the search-related components of the IC-library,
  essentially those adapted from fd_search, and RIA.  Provided is a generic
  search routine (for integer domain) which implements a number of partial
  search methods (complete, credit, lds, bbs, dbs) and some of their
  combinations. For floating point domains, the search facilities from RIA
  are provided.
")).


%---------------------------------------------------------------------
% generalised search/6 and friends originally from fd_search
%
:- use_module(gfd_for_search).
:- lib(gfd_generic_interface).

:- include(generic_search).
:- comment(include, generic_search_comments).

