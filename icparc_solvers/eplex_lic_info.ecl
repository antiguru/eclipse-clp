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
% Contributor(s): 
% 
% END LICENSE BLOCK
%
% This file tells lib(eplex) what optimizer version to expect
% on a particular machine. Add lines of the form:
%
%	licence(Hostname, Solver, Version, LicStr, LicNum).
% E.g.
%	licence('breeze',  xpress, '1326icp', default, 0). % OEM XPRESS-MP Version 13.26
%	licence('cow.ic.ac.uk',  cplex, '80', '', 0).	% CPLEX Version 8.0
%
% The hostname must match the result of get_flag(hostname,H),
% converted to an atom. On some machines, this is just a name,
% on others it is the complete internet domain name.
%
% Version number is the concatenation of the major and minor version
% numbers, with a trailing icp indicating an OEM version.
%
% The meaning of LicStr and LicNum depends on the optimizer:
%
% CPLEX:
%	LicStr:	environment settings for runtime licences, e.g.
%			"CPLEXLICENSE=/usr/local/cplexlic.ptr"
%	LicNum:	serial number for runtime licences
%
% XPRESS-MP:
%	LicStr:	atom default if OEM version used.
%               Otherwise: directory where the licence (.pwd) files are located
%			   (overrides value of XPRESS environment variable)
%	LicNum:	unused
%
% If a machine has both optimizers and lib(eplex) is called (rather than
% lib(eplex_cplex) or lib(eplex_xpress)) the first one will be loaded.
%

% Examples
licence('breeze.icparc.ic.ac.uk', cplex, '90', '', 0).
licence('morden.icparc.ic.ac.uk', xpress, '1427', default, 0).

% Insert your hosts here

% By default, use COIN/OR OSI solvers
licence(_, osi, clpcbc, '', 0).
licence(_, osi, symclp, '', 0).
licence(_, osi, glpk, '', 0).

% Defaults for other solvers
licence(_Default, cplex, '121', '', 0).
licence(_Default, xpress, '2000', '/opt/xpressmp/bin', 0).
licence(_Default, xpress, '1427icp', default, 0).
licence(_Default, xpress, '1326icp', default, 0).
