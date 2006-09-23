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
% Version:	$Id: demo_fact.pl,v 1.1 2006/09/23 01:55:11 snovello Exp $
% ----------------------------------------------------------------------

/*****************************************************************************
* IDENTIFICATION	demo_fact.pl
* DESCRIPTION		general interface to all demo programs
* CONTENTS		contains demo facts 
* AUTHOR		Micha Meier
*****************************************************************************/

demo_menu_data(sepia,
    'SEPIA'-[
	['General SEPIA demo', 'sepia/general_demo/gdemo.pl', start_demo, stop_demo],
	'Graphics'-[
	    ['Sepia puzzle', 'sepia/sepia_puzzle/sepia_puzzle.pl',
		puzzle, close_puzzle]
	],
	'Event mechanism'-[
	    ['Sepia perfmeter', lib(perfmeter), perfmeter, close_perfmeter],
	    ['Sepia statistics display', lib(meter), meter, close_meter]
	],
	'Coroutining'-[
	    ['Arithmetic crossword puzzle', 'sepia/cross_add.pl',
					    cross_add, close_cross_add],
	    ['Cartographic problem', 'sepia/corse.pl', go_corsica, close_corsica]
	]|DemoExt
    ]) :-
	get_flag(installation_directory, D),
	concat_strings(D, "/lib_private", LibEcrc),
	(exists(LibEcrc) ->
		DemoExt = []
%		DemoExt = [['GIS', lib('gis/gis'), gis_demo, gis_close]]
	;
		DemoExt = []
	).
demo_menu_data(sepia,
    'Extensions'-[
	'Finite domains'-[
	    ['Arithmetic puzzle', 'extensions/fd/money.pl', money, close_money],
	    ['N Queens', 'extensions/fd/queens.pl', go_queens, close_queens],
	    ['Map coloring', 'extensions/fd/map.pl', go_map, close_map],
	    ['Bridge construction', 'extensions/fd/bridge.pl',
		go_bridge, close_bridge],
	    ['Car sequencing', 'extensions/fd/c_excar.pl',
		go_car, close_car],
	    ['Warehouse location', 'extensions/fd/dora.pl', go_dora, close_dora]
	],
	'PROPIA'- [
	    ['Crosswords', lib('propia/crossword'), go_crossword, quit_method(_, 'Confirm')]
	],
	'CHR' - [
	    ['Cellular telephones', 'extensions/chr_demo.pl', chr_demo, quit_method(_, _)]
	]
    ]).

:- (['general_demo/gdemo'] -> true).	% because of ptags
