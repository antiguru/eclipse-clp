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
% Version:	$Id: eplex.ecl,v 1.1 2012/07/31 02:17:06 jschimpf Exp $

%------------------------------------------------------------------------
:- module(eplex_).

:- include(eplex_).

%------------------------------------------------------------------------
:- module(eplex).

:- comment(categories, ["Constraints","Interfacing"]).
:- comment(summary, "Interface to external Simplex or MIP solvers").
:- comment(author, "Joachim Schimpf and Kish Shen").
:- comment(date, "$Date: 2012/07/31 02:17:06 $").
:- comment(copyright, "Cisco Systems, Inc.").

:- comment(include, eplex_comments).

:- reexport eplex_ except
	add_constraint/1,
	lp_eq/3,
	lp_ge/3,
	lp_le/3,
        lp_interval/3,
        lp_int_interval/3,
        lp_real_interval/3,
        reals/2,
        integers/2,
	lp_var_non_monotonic_set_bounds/4,
        eplex_get/3,
	eplex_cleanup/1,
	eplex_probe/3,
	eplex_solve/2,
	eplex_var_get/4,
        eplex_set/3,
        eplex_read/3,
        eplex_write/3,
        eplex_get_iis/6.

:- eplex_instance(eplex).

