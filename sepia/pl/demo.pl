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
% Version:	$Id: demo.pl,v 1.1 2006/09/23 01:55:11 snovello Exp $
% ----------------------------------------------------------------------

/*****************************************************************************
* IDENTIFICATION	demo.pl
* DESCRIPTION		general interface to all demo programs
* CONTENTS		entry point is demo_go
* AUTHOR		Philip Kay ECRC GmbH
*****************************************************************************/

% Switch on coroutine if possible because some Sepia demos need it.
:- (is_predicate(get_file_info/3),
	get_flag(version, V),
	V @< '3.3' ->		% check if version is high and low enough
	    (import may_switch_coroutine/0 from sepia_kernel),
	    (may_switch_coroutine ->
		coroutine
	    ;
		true
	    )
    ;
	    true
    ).

?- make_callback(demo_pressed/2).
?- make_callback(demo_select/2).
?- make_callback(demo_view/2).

?- dynamic
	demo_fact/5,
	close_demo/0,
	demo_menu_data/2.

:- ensure_loaded(library(lists)).


/*****************************dialog build facts*****************************/
demo_panel(@demo_back, button('Back up', demo_pressed),  point(10, 10), []).
demo_panel(@demo_menu, menu('Show source:', cycle, demo_view, [off, on]),  point(10, 250), [layout: horizontal]).
demo_panel(@demo_quit, button('Quit all', demo_pressed), point(390, 10), []).
demo_panel(@demo_quit1, button('Quit demo', demo_pressed), point(280, 10), []).
demo_panel(_, label(feedback, 'Please select a category.'), point(10, 300), []).


/*********************************entry point********************************/
demo_go :-
	send(@pce, warnings, off),
	retract_all(demo_fact(_, _, _, _, _)),
	retract_all(demo_menu_data(_, _)),
	% Load the fact files
	load_facts(sepia),
	(get_flag(extension, chip) -> load_facts(chipc) ; load_facts(kegi)),
	new_dialog(@demo_dialog, 'KEGI V2.0 - Demo Driver Window', demo_panel),
	new(@topmenu, menu('Available Demos', marked, demo_select)),
	send(@topmenu, [mark: @mac_on, nomark: @mac_off, show: off, position: point(50, 60)]),
	send(@demo_dialog, append, @topmenu),
	demo_make_menus,
	send(@demo_dialog, [size: size(500, 330), open: point(0, 0)]),
	send(@topmenu, show, on),
	% we need a small pause here (because of SunView speed)
	% (current_functor(X), fail; true),
	setval(active_menu, @topmenu),
	send(@demo_back, [greyed: on, active: off]),
	send(@demo_quit1, [greyed: on, active: off]).

load_facts(System) :-
	directory(System, Dir),
	concat_atom([Dir, '/demo/', System, '/demo_fact.pl'], Demo),
	(exists(Demo) -> compile(Demo) ; true).

directory(kegi, D) :-
	!, call(kegi_dir(D), kegi).
directory(sepia, D) :-
	!, call(sepia_dir(D), kegi).
directory(chipc, D) :-
	call(sepia_dir(D), kegi).

demo_make_menus :-
	demo_menu_data(System, Title-Contents),
	new(Menu, menu(Title, marked, demo_select)),
	send(Menu, [mark: @mac_on,  nomark: @mac_off, show: off, position: point(50, 60)]),
	send(@demo_dialog, append, Menu),
	assert(demo_fact(@topmenu, Menu, Title, [], System)),
	new(Item, menu_item(Title, 0)),
	send(@topmenu, append, Item),
	demo_make_menus(@topmenu, Menu, Contents, System),
	fail.
demo_make_menus.

demo_make_menus(_, _, [], _).
demo_make_menus(Parent, Menu, [[Title, File, Call, CloseGoal]|Tail], System):-
	!,
	new(Item, menu_item(Title, 0)),
	send(Menu, append, Item),
	assert(demo_fact(Parent, Item, Title, [File, Call, CloseGoal], System)),
	demo_make_menus(Parent, Menu, Tail, System).
demo_make_menus(Parent, Menu, [Title-Contents|Tail], System):-
	new(Submenu, menu(Title, marked, demo_select)),
	send(Submenu, [mark: @mac_on,  nomark: @mac_off, show: off, position: point(50, 60)]),
	send(@demo_dialog, append, Submenu),
	assert(demo_fact(Menu, Submenu, Title, [], System)),
	demo_make_menus(Menu, Submenu, Contents, System),
	new(Item, menu_item(Title, 0)),
	send(Menu, append, Item),
	demo_make_menus(Parent, Menu, Tail, System).



/*******************************interface actions****************************/
demo_select(_, Name):-		% top level menu pressed
	demo_fact(Parent, Menu, Name, [], _), !,
	send(@demo_back, [active: on, greyed: off]),
	send(Parent, show, off),
	setval(active_menu, Menu),
	send(Menu, show, on),
	please_select.
demo_select(_, Name):-		% sub level menu pressed
	demo_fact(_, _, Name, [File, Call, CloseGoal], System),
	change_cursor(@demo_dialog, @hourglass_cursor),
	(demo_load(Name, Call, CloseGoal, File, System) -> true; true),
	change_cursor(@demo_dialog, @point_left_cursor).

please_select :-
	getval(active_menu, Menu),
	demo_fact(_, Menu, Name, [], _),
	concat_atom(['Please select a ', Name, ' demo.'], Text),
	demo_inform(Text),
	(clause(close_demo :- _) ->
	    send(@demo_quit1, [greyed: off, active: on])
	;
	    send(@demo_quit1, [greyed: on, active: off])
	).

demo_pressed(_, 'Back up'):-
	getval(active_menu, Menu),
	demo_fact(Parent, Menu, _, _, _), !,
	send(Menu, show, off),
	setval(active_menu, Parent),
	send(Parent, show, on),
	(Parent = @topmenu ->
	    send(@demo_back, [active: off, greyed: on]),
	    demo_inform( 'Please select a category.')
	;
	    true
	).
demo_pressed(_, 'Quit demo'):-
	close_all_demo,
	(is_predicate(close_2d/0), close_2d -> true ; true),
	retract_all(close_demo),
	please_select,
	exit_block(abort).
demo_pressed(_, 'Quit all'):-
	close_all_demo,
	(is_predicate(close_2d/0), close_2d -> true ; true),
	retract_all(close_demo),
	(object(@demo_view) ->
	    send(@demo_view, destroy)
	;
	    true
	),
	send(@demo_dialog, destroy).

close_all_demo :-
	close_demo,
	fail.
close_all_demo :-
	send(@demo_quit1, [greyed: on, active: off]).

demo_view(_, on):-
	new(@demo_view, view('KEGI V2.0 - Demo Source Window')),
	send(@demo_view, [font: font(courier, roman, 10), size: size(500, 530), open: point(0, 350)]).
demo_view(_, off):-
	object(@demo_view),
	send(@demo_view, destroy).

demo_inform(Text):-
	send(@demo_dialog, feedback, Text).

demo_load(Name, Call, CloseGoal, File, System):-
	% does the top predicate exist if not compile file
	find_file(File, System, Path),
	(is_predicate(Call/0) ->
	    true
	;
	    concat_atom(['Loading ', Name, ' demo please wait.'], Text),
	    demo_inform(Text),
	    block(compile(Path), _, demo_recover_c)
	),
	% if the view is open display the file
	(object(@demo_view) ->
		send(@demo_view, [load: Path, editable: off])
	;
		true
	),
	% add the closing goal in close_demo/0
	assert((close_demo :- CloseGoal, please_select)),
	% call the top predicate
	concat_atom(['Running ', Name, ' demo.'], TextR),
	demo_inform(TextR),
	send(@demo_quit1, [greyed: off, active: on]),
	(block(Call, _, fail) -> true; demo_recover_r(CloseGoal)).
	
find_file(lib(File), _, Path) :-
	!,
	get_flag(library_path, List),
	find_lib(File, List, Path).
find_file(File, System, Path) :-
	directory(System, Dir),
	concat_atom([Dir, '/demo/', File], Path).

find_lib(File, LibPath, Path) :-
	member(Lib, LibPath),
	atom_string(File, FileStr),
	(
		Fpl = FileStr
	;
		get_flag(prolog_suffix, SuffixList),
		member(Suffix, SuffixList),
		concat_strings(FileStr, Suffix, Fpl)
	),
	concat_string([Lib, '/', Fpl], Path),
	get_file_info(Path, mode, Mode),
	8'40000 =\= Mode /\ 8'170000,	/* must not be a directory */
	!.
find_lib(File, _, File).	% So that it is not found in block/3
	
demo_recover_c :-
	demo_inform('Problem compiling demo.'),
	fail.

demo_recover_r(CloseGoal) :-
	demo_inform('Problem running demo.'),
	call(CloseGoal),
	retract((close_demo :- CloseGoal, please_select)),
	fail.
