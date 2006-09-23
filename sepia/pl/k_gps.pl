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
% 
% END LICENSE BLOCK
%
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: k_gps.pl,v 1.1 2006/09/23 01:55:26 snovello Exp $
% ----------------------------------------------------------------------

/*****************************************************************************
* IDENTIFICATION	k_gps.pl
* DESCRIPTION		common graphical predicates code
* CONTENTS		this is for all systems
* AUTHOR		Philip Kay ECRC GmbH
*****************************************************************************/


/*****************************************************************************
* compiled declarations
*****************************************************************************/
:- system.

:- begin_module(kegi).
:- export
	speed_pce/1,
	make_file_popup/3, 
	make_file_popup/4, 
	make_tree_proto/3, 
	show_tree/4, 
	show_tree/5, 
	new_node/4, 
	destroy_obj/1, 
	fit_tree/2, 
	fit_tree/4, 
	create_font_popup/1, 
	enumerate/2, 
	enumerate_ref/2, 
	get_chain/3, 
	get_chain_ref/3, 
	warning/3, 
	warning/5, 
	alert/3, 
	alert/4, 
	alert/6, 
	prompt/4, 
	prompt/7, 
	prompt/8, 
	prompt/9, 
	new_icon/2, 
	new_cursor/2, 
	dump_window/3, 
	change_cursor/2, 
	new_dialog/3, 
	init_colours/0,
	init_colourmap/5,
	new_bar/3.


/*****************************************************************************
* speed_pce(Speed)
*	+Speed        1 = fast or 0 = slow
*****************************************************************************/
speed_pce(Speed):-
	(getval(kegi_sepia, 2) ->
		disable_interrupts,
		(pce_set_speed(Speed) -> true; true),
		enable_interrupts
	;
		set_flag(enable_interrupts, off),
		(pce_set_speed(Speed) -> true; true),
		set_flag(enable_interrupts, on)
	).


/*****************************************************************************
* new_dialog(?Ref, +Title, +Key)
*	?Ref		The name of the dialog window e.g @win or Win
*	+Title		The title of the frame of the dialog window
*	+Key		Construction clause as described below
*
* Key(?Ref, +Item, +Position, +[Attributes])
*	?Ref		Object reference of the item e.g @toto, A or _
*	+Item		Type of item to be built as described below
*	+Placement	One of right, below, point(X, Y) or popup
*	+[Attributes]	List of the additional attributes for the item
*
* The +Item to be built is defined as one of the following
*	label(+Feedback, +Label)
*	button(+Label, +Message)
*	slider(+Label, +Message)
*	menu(+Label, +Type, +Message, +[Contents])
*	menu(+Label, +Type, +Message, +Predicate)
*	db_menu(+Label, +Type, +Message, +Predicate)
*	new_mac_menu(+Label, +Type, +Message, +[Contents])
*	db_mac_menu(+Label, +Type, +Message, +Predicate)
*****************************************************************************/
:- tool(new_dialog/3, new_dialog/4).

new_dialog(Ref, Label, Key, Module):-
	new(Ref, dialog(Label)), 
	findall(X, find_dialog_item(Key, X, Module), List), 
	make_dialog_item(Ref, List, Module).

find_dialog_item(Key, item(R, T, D, A), Module):-
	Clause =.. [Key, R, T, D, A], 
	call(Clause, Module).

make_dialog_item(_, [], Module):- !.
make_dialog_item(Dialog, [item(Ref, Term, Dir, Attrs)|T], Module):-
	new_item(Ref, Term, Module), !, 
	send(Ref, Attrs), 
	append_item(Dialog, Ref, Dir), 
	make_dialog_item(Dialog, T, Module), !.
make_dialog_item(Dialog, [item(Ref, Goal, Dir, Attrs)|T], Module):-
	Goal =.. [Functor | Args], 
	append(Args, [Module], Args1), 
	Clause =.. [Functor,  Ref | Args1], 
	call(Clause), !, 
	send(Ref, Attrs), 
	append_item(Dialog, Ref, Dir), 
	make_dialog_item(Dialog, T, Module), !.

new_item(Ref, label(A, B), Module):-
	new(Ref, label(A, B)), !.
new_item(Ref, button(A, B), Module):-
	new(Ref, button(A, B)), !.
new_item(Ref, slider(A, B, C, D, E), Module):-
	new(Ref, slider(A, B, C, D, E)), !.
new_item(Ref, text_item(A, B, C), Module):-
	new(Ref, text_item(A, B, C)), !.
new_item(Ref, num_textitem(A, B, C), Module):-
	new(Ref, num_textitem(A, B, C)), !.
new_item(Ref, scrollinglist(A, B), Module):-
	new(Ref, scrollinglist(A, B)), !.
new_item(Ref, menu(A, B, C, D), Module):-
	new_menu(Ref, A, B, C, D, Module).
	
new_menu(Ref, Label, Type, Message, [], Module):- 
	new(Ref, menu(Label, Type, Message)), !.
new_menu(Ref, Label, Type, Message, Contents, Module):-
	atom(Contents), 
	findall(X, make_menu_item(Contents, X, Module), List), !, 
	new_menu(Ref, Label, Type, Message, List, Module), !.
new_menu(Ref, Label, popup, Message, Contents, Module):- 
	new(Ref, popup(Label, Message)), 
	send(Ref, append, Contents), !.
new_menu(Ref, Label, Type, Message, Contents, Module):- 
	new(Ref, menu(Label, Type, Message)), 
	send(Ref, append, Contents), !.

make_menu_item(Key, @X, Module):-
	Clause =.. [Key, Label, Message, Attrs], 
	call(Clause, Module), 
	new(@X, menu_item(Label, Message)), 
	send(@X, Attrs).

append_item(Dialog, Ref, below):-
	get(Dialog, current, @Item), 
	send(Ref, below, @Item).
append_item(Dialog, Ref, right):-
	get(Dialog, current, @Item), 
	send(Ref, right, @Item).
append_item(Dialog, Ref, point(X, Y)):-
	send(Ref, position, point(X, Y)), 
	send(Dialog, append, Ref).
append_item(Dialog, Ref, popup):-
	send(Dialog, popup, Ref).
append_item(Dialog, Ref, _):-
	send(Dialog, append, Ref).

new_mac_menu(Ref, Title, marked, Message, Contents, Module):-
	new_menu(Ref, Title, marked, Message, Contents, Module), 
	send(Ref, [mark: @mac_on,  nomark: @mac_off]), !.
new_mac_menu(Ref, Title, toggle, Message, Contents, Module):-
	new_menu(Ref, Title, toggle, Message, Contents, Module), !, 
	send(Ref, [mark: @toggle_on,  nomark: @toggle_off]), !.
new_mac_menu(Ref, Title, Type, Message, Contents, Module):-
	new_menu(Ref, Title, Type, Message, Contents, Module).

db_mac_menu(Ref, Title, Type, Message, Goal, Module):-
	Clause =.. [Goal, Elements], 
	call(Clause, Module), !, 
	new_mac_menu(Ref, Title, Type, Message, Elements, Module), !.

db_menu(Ref, Title, Type, Message, Goal, Module):-
	Clause =.. [Goal, Elements], 
	call(Clause, Module), !, 
	new_menu(Ref, Title, Type, Message, Elements, Module), !.


/*****************************************************************************
* new_bar(?Bar, +Name, +Key)
*	?Bar		  The name of the menu bar e.g Bar @bar
*	+Name		 Name of the title of the bar this does not appear
*	+Key		  Construction clause as described below
*
* Key(?Ref, +Item, +[Attributes])
*	?Ref		  Reference for the popup e.g@ toto or _
*	+Item		 Popup definition as given below
*	+[Attributes] List of the additional attributes for the item
*
* The +Item is made up as the folowing
*	 popup(+Label, +Message, +[Contents])
*****************************************************************************/
:- tool(new_bar/3, new_bar/4).

new_bar(Bar, Label, Key, Module):-
	new(Bar, menu_bar(Label)), 
	findall(X, find_bar_item(Key, X, Module), List), 
	make_bar_item(Bar, List, Module).

find_bar_item(Key, item(Popup, Spec, Attributes), Module):-
	Clause =.. [Key, Popup, Spec, Attributes], 
	call(Clause, Module).

make_bar_item(_, [], Module):- !.
make_bar_item(Bar, [item(Popup, popup(Name, Mess, List), Attr)|Tail], Module):-
	new(Popup, popup(Name, Mess)), 
	append_menu(Popup, List), 
	send(Popup, Attr), 
	send(Bar, append, Popup), 
	make_bar_item(Bar, Tail, Module).

append_menu(_, []).
append_menu(Popup, List):-
	send(Popup, append, List).


/*****************************************************************************
* make_file_popup(+Directory, +Pathname, +Popup)
* make_file_popup(+Directory, +Pathname, +Popup, +Pred)
*	+Directory	Scan from directory  e.g '.'
*	+Pathname	 Full pathname of above directory e.g /usr/fred
*	+Popup		Name of the popup this must already exist
*	+Pred		 The predicate name to cascade when selected
*
* When selected the default cascade is:
*
* file_menu_popped(?Popup, ?File)
*	 ?popup	   The popup object e.g @popup
*	 ?File		The relative filename e.g './toto'
*****************************************************************************/
make_file_popup(Name, Directory, Parent_popup):-
	make_file_popup(Name, Directory, Parent_popup, file_menu_popped).
make_file_popup(Name, Directory, Parent_popup, Pred):-
	get_dir_contents(Directory, Directories, Files), 
	make_file_sub_popup(Name, Directory, Directories, List, Parent_popup, Pred), 
	append_menu(Parent_popup, List), 
	append_popups(Name, Files, Parent_popup, Pred).

if_version(3, (
    get_dir_contents(Directory, Directories, Files):-
	    read_directory(Directory, "*", D, F),
	    convert_to_atoms(D, Directories),
	    convert_to_atoms(F, Files)
),(
    get_dir_contents(Direct, Directories, Files):-
	    atom_string(Direct, Directory),
	    new(Dir, directory(Directory)),
	    send(Dir, open),
	    get(Dir, directories, @Chain1),
	    send(@Chain1, sort, message(0, smaller, 0)),
	    get_chain(Dir, directories, Directories),
	    get(Dir, files, @Chain2),
	    send(@Chain2, sort, message(0, smaller, 0)),
	    get_chain(Dir, files, Files),
	    send(Dir, free)
)).

convert_to_atoms([], []).
convert_to_atoms([SH|ST], [AH|AT]):-
	atom_string(AH, SH),
	convert_to_atoms(ST, AT).

make_file_sub_popup(_, _, [], [], _, _).
make_file_sub_popup(Name, Directory, [H|T], [H1|T1], Parent_popup, Pred):-
	concat_atom([Name, '/', H], Subname), 
	concat_atom([Directory, '/', H], Subdirectory), 
	new(H1, popup(H, cascade(Parent_popup, Pred, 0))), 
	make_file_popup(Subname, Subdirectory, H1, Pred), 
	make_file_sub_popup(Name, Directory, T, T1, Parent_popup, Pred).

append_popups(_, [], _, _).
append_popups('.', [Filename|T], Popup, Pred):-
	send(Popup, append, menu_item(Filename, cascade(Popup, Pred, Filename))), 
	append_popups('.', T, Popup, Pred).
append_popups(Name, [H|T], Popup, Pred):-
	concat_atom([Name, '/', H], Filename), 
	send(Popup, append, menu_item(H, cascade(Popup, Pred, Filename))), 
	append_popups(Name, T, Popup, Pred).


/*****************************************************************************
* make_tree_proto(-Proto, +Left_msg, +Middle_msg)
*	-Proto		returns the name of the proto
*	+Left_msg	Left button message e.g cascade(@win, left, 0) or 0
*	+Middle_msg	Middle button message e.g cascade(@win, left, 0) or 0
*****************************************************************************/
make_tree_proto(Proto, Left_msg, Middle_msg) :-
	new(Proto, proto(node, 0)), 
	new(Line, line(0, 0, 0, 0)), 
	new(Text, text('', 0, 0, left)), 
	send(Text, message, name), 
	send(Proto, left_click, Left_msg), 
	send(Proto, middle_click, Middle_msg), 
	send(Text, font, font(screen, roman, 12)), 
	send(Proto, append, [Text,
		handle(link, parent, percentage(0, 50), Line, link), 
		handle(link, son, percentage(100, 50), Line, link)]), !.


/*****************************************************************************
* show_tree(+Rootname-Siblings, +Proto, +Tree_obj, +Pic)
* show_tree(+Rootname-Siblings, +Proto, +Tree_obj, +Pic, +Pos)
*	+Rootname-Siblings Tree_term which 
*			Tree_term =:: Atomic - List of [Atomic|Tree_term]
*	+Proto		The proto object on which to make the tree
*	+Tree_obj	The tree 
*	+Pic		The picture window on which the tree is to be shown
*	+Pos		Optional fourth argument for postion e.g point(5, 5)
*
* ?-dynamic: node_name_tree(Name, Obj, Tree_obj)
*	 is generated for each node containing
*	 its name, its object and the tree to which it belongs
*****************************************************************************/
?-dynamic node_name_tree/3.

show_tree(Rootname-Siblings, Proto, Tree_obj, Pic):-
	show_tree(Rootname-Siblings, Proto, Tree_obj, Pic, point(5, 5)).
show_tree(Rootname-Siblings, Proto, Tree_obj, Pic, Pos):-
	destroy_obj(Tree_obj), 
	new(Tree_obj, tree(Pic, Pos)), 
	retract_all(node_name_tree(_, _, Tree_obj)), 
	send(Tree_obj, [blocked: on, x_gap: 50, y_gap: 2]), 
	new_node(Proto, Rootname, Root_obj, Tree_obj), 
	send(Tree_obj, root, Root_obj), 
	show_siblings(Proto, Root_obj, Tree_obj, Siblings), 
	send(Tree_obj, blocked, off).

destroy_obj(Obj):-
	object(Obj, _), 
	send(Obj, free), !.
destroy_obj(Obj).

show_siblings(_, _, _, []).
show_siblings(Proto, Root, Tree, [Son|Sons]):-
	atomic(Son), !, 
	new_node(Proto, Son, Node, Tree), 
	send(Root, son, Node), 
	show_siblings(Proto, Root, Tree, Sons).
show_siblings(Proto, Root, Tree, [Son-Grandsons|Sons]):-
	new_node(Proto, Son, Node, Tree), 
	send(Root, son, Node), 
	show_siblings(Proto, Node, Tree, Grandsons), 
	show_siblings(Proto, Root, Tree, Sons).


/*****************************************************************************
* new_node(-Proto, +Name, -Obj, +Tree_obj)
*	-Proto			 The name of the proto - must exist
*	+Name			  The text of the new 
*	-Obj			   The object name of the node
*	+Tree_obj		  The tree object
*****************************************************************************/
new_node(Proto, Name, Obj, Tree_obj) :-
	new( Obj, node(figure(Proto, 0, 0), Tree_obj) ), 
	assert(node_name_tree(Name, Obj, Tree_obj)), 
	send(Obj, name, Name).


/*****************************************************************************
* fit_tree(+Picture_obj, +Tree_obj)
* fit_tree(+Picture, +Tree, +MaxW, +MaxH)
*	+Picture_obj	  The name of the picture window
*	+Tree			 The name of the tree object
*	+MaxW, +MaxH	  Optional maximums to resize the window to
*****************************************************************************/
fit_tree(Picture, Tree):-
	fit_tree(Picture, Tree, 1000, 1000).

fit_tree(Picture, Tree, MaxW, MaxH):-
	get(Tree, root, @Root), 
	find_dimensions(@Root, Width, Height), 
	RealW is min(Width + 35, MaxW), 
	RealH is min(Height + 35, MaxH), 
	send(Picture, [display_size:size(RealW, RealH), fit:0]).

find_dimensions(Node, Width, Height):-
	get_chain_ref(Node, sons, List),
	List \== [], !, 
	traverse(List, 0, Width, Height).
find_dimensions(Node, Width, Height):-
	get(Node, area, area(X, Y, W, H)), 
	Width is X + W, 
	Height is Y + H.

traverse([Node], Width, W2, Height):-
	find_dimensions(Node, W1, Height), 
	W2 is max(Width, W1).
traverse([Node, Node1|Rest], W, Width, Height):-
	find_dimensions(Node, W1, _), 
	W2 is max(W, W1), 
	traverse([Node1|Rest], W2, Width, Height).


/*****************************************************************************
* create_font_popup(+Popup_obj)
*	 +Popup_obj		The name of the popup this must already exist
*
* ?-dynamic: font_info(Atom, Family, Type, Size)
*	 is generated to contain all the fonts known
*
* needs bagof/3 to work
*****************************************************************************/
?-dynamic font_info/4.

create_font_popup(Popup):-
	call(retract_all(font_info(_, _, _, _)), kegi), 
	call(bagof(Font, font_name(Font), Fontlist), kegi), 
	append_menu(Popup, Fontlist).
	
font_name(Name):-
	font(X, Y, Z), 
	concat_atom([X, ' ', Y, ' ', Z], Name), 
	asserta(font_info(Name, X, Y, Z)).


/*****************************************************************************
*  get_chain(+Object, +Behaviour, ?List)
*  get_chain_ref(+Object, +Behaviour, ?List)
*  Given an existing object and its chain behaviour the above will return a
*  list of named objects or storage objects respectively.
*
*  enumerate(+Chain, -List)
*  enumerat_ref(+Chain, -List)
*  Given a Chain object these will return the chains' named objects or
*  storage objects respectively.
*****************************************************************************/
get_chain(@Object, Name, List):-
	get(@Object/Name, list, List), !.
get_chain_ref(@Object, Name, List):-
	get(@Object/Name, list_refs, List), !.

enumerate(Chain, List):-
	get(Chain, list, List), !.

enumerate_ref(Chain, List):-
	get(Chain, list_refs, List), !.


/*****************************************************************************
* warning(+Statement, +Text, ?Dialog)
* warning(+Image, +Statement, +Text, ?Dialog, +Pos)
*	 +Image		Optional bitmap object alredy created @warning_icon
*	 +Statement	The initial text in gallant.r.19 font
*	 +Text		The text below the statement for more explanation
*	 ?Dialog	The name of the window created so you can kill it
*	 +Pos		Optional position of warning box e.g point(400, 400)
*****************************************************************************/
warning(Statement, Text, Dialog):-
	warning(@warning_icon, Statement, Text, Dialog, point(400, 400)).

warning(Image, Statement, Text, Dialog, Pos):-
	(get(@pce, window_system, sunview) ->
		new(Dialog, dialog('')), 
		send(Dialog, [show_label: off, gap: point(10, 10)]), 
		new(Icon, label(xx, xx)), 
		send(Icon, [image: Image, position: point(3, 0)]), 
		new(State, label(xx, Statement)), 
		send(State, label_font, font(gallant, roman, 19)),
		send(State, position, point(80, 25)), 
		new(Mess, label(xx, Text)),
		send(Mess, position, point(10, 70)), 
		send(Dialog, append, [Icon, State, Mess]), 
		send(Dialog, [open: Pos, can_delete: true, flash])
	;
		new(Dialog, dialog('')), 
		send(Dialog, show_label, off),
		new(State, label(xx, Statement)), 
		send(State, label_font, font(gallant, roman, 19)),
		send(Dialog, append, [State, label(xx, Text)]), 
		send(Dialog, [open: Pos, can_delete: true, flash])
	).



/*****************************************************************************
* alert(+Text, +[Buttons], ?Ans)
* alert(+Text, +[Buttons], ?Ans, +Pos)
* alert(+Image, +Statement, +Text, +[Buttons], ?Ans, +Pos)
*	 +Image		Optional bitmap object alredy created @question_icon
*	 +Statement	The initial text in gallant.r.19 font
*	 +Text		The text below the statement for more explanation
*	 +[Buttons]	List of button names
*	 ?Ans		The returned button answer as selected by user
*	 +Pos		Optional position of alert box e.g point(400, 400)
*****************************************************************************/
alert(Text, Buttons, Ans) :-
	alert(Text, Buttons, Ans, point(400, 400)).
alert(Text, Buttons, Ans, Pos) :-
	alert(@question_icon, '', Text, Buttons, Ans, Pos).
alert(Image, Statement, Text, Buttons, Ans, Pos) :-
	(get(@pce, window_system, sunview) ->
		new(Dialog, dialog('')), 
		send(Dialog, [show_label: off, gap: point(10, 10)]), 
		new(Icon, label(xx, xx)), 
		send(Icon, [image: Image, position: point(3, 0)]), 
		send(Dialog, append, Icon), 
		new(State, label(xx, Statement)), 
		send(State, label_font, font(gallant, roman, 19)),
		send(State, position, point(80, 25)), 
		send(Dialog, append, State), 
		new(Mess, label(feedback, Text)), 
		send(Mess, position, point(10, 70)), 
		send(Dialog, append, Mess), 
		buttons_alert(Dialog, Buttons, below), 
		send(Dialog, [open: Pos, can_delete: false, flash]), 
		alert_reply(Dialog, Ans), 
		send(Dialog, destroy), !
	;
		new(Dialog, dialog('')), 
		send(Dialog, show_label, off),
		new(State, label(xx, Statement)), 
		send(State, label_font, font(gallant, roman, 19)),
		send(Dialog, append, [State, label(feedback, Text)]), 
		buttons_alert(Dialog, Buttons, below), 
		send(Dialog, [open: Pos, can_delete: true, flash]),
		alert_reply(Dialog, Ans), 
		send(Dialog, destroy), !	
	).

buttons_alert( _, [], _ ) :- !.
buttons_alert( Dialog, [Label|Tail], Where ) :-
	new( Button, button( Label, alert_button ) ), 
	get( Dialog, current, @Current ), 
	send( Button, Where, @Current ), 
	buttons_alert( Dialog, Tail, right ).

alert_reply( Dialog, Reply ) :-
	repeat, 
	get_alert_message( Dialog, Reply ).

get_alert_message( Dialog, Reply ) :-
	get(@pce, wait, message(Dialog, alert_button, Reply)).
get_alert_message( Dialog, _ ) :-
	send( Dialog, feedback, 'Please press one of the buttons.' ), 
	fail.


/*****************************************************************************
* prompt(+Item, +[Buttons], ?Typed, ?Ans)
* prompt(+Image, +Statement, +Text, +Item, +[Buttons], ?Typed, ?Ans)
* prompt(+Image, +Statement, +Text, +Item, +[Buttons], ?Typed, ?Ans, +Pos)
* prompt(+Image, +Statement,+Text,+Item,+Initial,+[Buttons],?Typed,?Ans,+Pos)
*	 +Image		Optional bitmap object alredy created @question_icon
*	 +Statement	The initial text in gallant.r.19 font
*	 +Text		The text below the statement for more explanation
*	 +Item		Title of the textitem
*	 +Initial	Initial text in textitem
*	 +[Buttons]	List of button names
*	 ?Typed		Answer in textitem
*	 ?Ans		The returned button answer as selected by user
*	 +Pos		Optional position of alert box e.g point(400, 400)
*****************************************************************************/
prompt(Item, Buttons, Typed, Ans):-
	prompt(@question_icon, '', '', Item, '', Buttons, Typed, Ans, point(400, 400)).
prompt(Image, Statement, Text, Item, Buttons, Typed, Ans):-
	prompt(Image, Statement, Text, Item, '', Buttons, Typed, Ans, point(400, 400)).
prompt(Image, Statement, Text, Item, Buttons, Typed, Ans, Pos):-
	prompt(Image, Statement, Text, Item, '', Buttons, Typed, Ans, Pos).
prompt(Image, Statement, Text, Item, Initial, Buttons, Typed, Ans, Pos):-
	(get(@pce, window_system, sunview) ->
		new(Dialog, dialog('')), 
		send(Dialog, [show_label: off, gap: point(10, 10)]), 
		new(Icon, label(xx, xx)), 
		send(Icon, [image: Image, position: point(3, 0)]), 
		new(State, label(statement, Statement)), 
		send(State, label_font, font(gallant, roman, 19)),
		send(State, position, point(80, 25)), 
		new(Mess, label(feedback, Text)), 
		send(Mess, position, point(10, 70)), 
		send(Dialog, append, [Icon, State, Mess]),
		send(Dialog, append, text_item(Item, Initial, prompt_typed)), 
		buttons_prompt(Dialog, Buttons, below), 
		send(Dialog, [open: Pos, can_delete: false, flash]), 
		prompt_button(Dialog, Ans), 
		get(Dialog, Item, Typed),
		send(Dialog, destroy), !
	;
		new(Dialog, dialog('')), 
		send(Dialog, show_label, off),
		new(State, label(xx, Statement)), 
		send(State, label_font, font(gallant, roman, 19)),
		send(Dialog, append, [State, label(feedback, Text)]),
		send(Dialog, append, text_item(Item, Initial, prompt_typed)), 
		buttons_prompt(Dialog, Buttons, below), 
		send(Dialog, [open: Pos, can_delete: true, flash]),
		prompt_button(Dialog, Ans), 
		get(Dialog, Item, Typed),
		send(Dialog, destroy), !	
	).

prompt_button(Dialog, Ans):-
	repeat, 
	get_prompt_message(Dialog, Ans).

buttons_prompt(_, [], _):- !.
buttons_prompt(Dialog, [Label|Tail], Where) :-
	new(Button, button(Label, prompt_button)), 
	get(Dialog, current, @Current), 
	send(Button, Where, @Current), 
	buttons_prompt(Dialog, Tail, right).

get_prompt_message(Dialog, Pressed):-
	get(@pce, wait, message(Dialog, Behaviour, Value)), 
	(Behaviour == prompt_button ->
		Pressed = Value
	;
		Pressed = 'OK'
	).


/*****************************************************************************
* new_cursor(?Cursor, +File)
*	 ?Cursor	The returned cursor object name
*	 +File		Filename of cursor can be local or in icons directory
*
* new_icon(?Bitmap, +File)
*	 ?Bitmap	The returned bitmap object name
*	 +File		Filename of icon can be local or in icons directory
*****************************************************************************/
new_cursor(Cursor, File):-
	new_icon(X, File), 
	new( Cursor, cursor(X)).

new_icon(Bitmap, File):-
	new(Bitmap, bitmap(0, 0)), !,
	load_bitmap(Bitmap, File).

load_bitmap(Bitmap, File) :-
	find_bitmap(File, Path),
	send(Bitmap, load_icon, Path).

find_bitmap(File, Path):-
	kegi_dir(Dir),
	(get(@pce, window_system, sunview) ->
		concat_atom([Dir, '/graphic/sicon/', File], Path)
	;
		concat_atom([Dir, '/graphic/xicon/', File], Path)
	).
find_bitmap(File, File).


/*****************************************************************************
* change_cursor(+window_object, +cursor_object)
*	+window_object   One of the windows within the frame
*	+cursor_object   The name of the cursor object
*****************************************************************************/
change_cursor(Window, Cursor):-
	get(Window, frame, @Frame), 
	get(@Frame, members, @Chain), 
	send(@Chain, for_all, message(0, cursor, Cursor)).


/*****************************************************************************
* dump_window(+Window, +File, +Flag)
*	 +Window	The name of the window to be dumped
*	 +File		The file in which to save the screendump
*	 +Flag		Flag is one of all, frame or noframe
*			- all gives you evrything contained within 1 frame
*			- frame gives only the specified window
*			- noframe removes frame and scrollbars leaving 1 pixel
*****************************************************************************/
dump_window(Window, File, S) :-
	(get(@pce, window_system, sunview) ->
		dump_window1(Window, File, S)
	;
		writeln("Warning: dump_window/3 not supported under X11"),
		fail
	).

dump_window1(Window, File, all) :-
	new(Screen, bitmap(0, 0)), 
	send(Screen, open, '/dev/fb'), 
	get(Window, area, area(X,Y,W,H)), 
	send(Screen, clip_area, area(X,Y,W,H)), 
	get(Screen, clip, @Dump), 
	send(@Dump, save, File), 
	send([Screen, @Dump], free).
dump_window1(Window, File, frame) :-
	new(Screen, bitmap(0, 0)), 
	send(Screen, open, '/dev/fb'), 
	get(Window, area, area(X1, Y1, _, _)), 
	get(Window, display_position, point(X2, Y2)), 
	get(Window, display_size, size(W1, H1)), 
	X is X1 + X2, 
	Y is Y1 + Y2, 
	W is W1 + 10,		/* 10 is 2 side borders(5) */
	H is H1 + 19,		/* 19 is title bar(14) + bottom border(5) */
	send(Screen, clip_area, area(X, Y, W, H)), 
	get(Screen, clip, @Dump), 
	send(@Dump, save, File), 
	send([Screen, @Dump], free).
dump_window1(Window, File, noframe) :-
	send(Window,[vertical_scrollbar,horizontal_scrollbar],off),
	new(Screen, bitmap(0, 0)), 
	send(Screen, open, '/dev/fb'), 
	get(Window, area, area(X1, Y1, _, _)), 
	get(Window, display_position, point(X2, Y2)), 
	get(Window, display_size, size(W1, H1)), 
	X is X1 + X2 + 4,	/* 4 is the left border - 1 pixel */
	Y is Y1 + Y2 + 13,	/* 13 is the title bar - 1 pixel */
	W is W1 + 2,		/* 2 is the top pixel + the bottom pixel */
	H is H1 + 2,		/* 2 is the left pixel + the right pixel */
	send(Screen, clip_area, area(X, Y, W, H)), 
	get(Screen, clip, @Dump), 
	send(@Dump, save, File), 
	send([Screen, @Dump], free),
	send(Window,[vertical_scrollbar,horizontal_scrollbar],on).


/*****************************************************************************
* absolute_item_position(+Item, +Dialog, ?X, ?Y)
*	 +Item		   The name of the Item  object
*	 +Dialog		 The name of the dialog window holding object
*	 ?X			  The returned X coordinate
*	 ?Y			  The returned Y coordinate
*****************************************************************************/
absolute_item_position(Item, Dialog, X, Y) :-
	get(Item, area, area(ItemX, ItemY, _, _)), 
	get(Dialog, display_position, point(DialogX, DialogY)), 
	get(Dialog, area, area(WindowX, WindowY, _, _)), 
	X is ItemX + DialogX + WindowX, 
	Y is ItemY + DialogY + WindowY.


/*****************************************************************************
* align_figure(+First, +Second)
*	 +First		  The object name of the first picture object
*	 +Second		 The object name of the second picture object
*****************************************************************************/
align_figure(First, Second) :-
	get(First, center, point(X1, Y1)), 
	get(Second, center, point(X2, Y2)), 
	X is abs(X1 - X2), 
	Y is abs(Y1 - Y2), 
	( X < Y ->
		send(Second, center, point(X1, Y2))
	;
		send(Second, center, point(X2, Y1))
	).


/*****************************************************************************
* colour map stuff
*****************************************************************************/
:- tool(init_colours/0, init_colours/1).

init_colours(M):-
	call(colour_map(Map, Size, Back, Fore), M),
	new(@Map, colour_map(Map, Size)),
	call(findall(colour(Name, R, G, B), colour(Name, R, G, B), List), M),
	new_colour(List, Map, Back, Fore).

new_colour([], _, _, _).
new_colour([colour(Back, R, G, B)|Rest], Map, Back, Fore):-
	!,
	new(C, colour(Back, R, G, B)),
	send(@Map, background_colour, C),
	new_colour(Rest, Map, Back, Fore).
new_colour([colour(Fore, R, G, B)|Rest], Map, Back, Fore):-
	!,
	new(C, colour(Fore, R, G, B)),
	send(@Map, foreground_colour, C),
	new_colour(Rest, Map, Back, Fore).
new_colour([colour(Name, R, G, B)|Rest], Map, Back, Fore):-
	new(C, colour(Name, R, G, B)),
	send(@Map, append, C),
	new_colour(Rest, Map, Back, Fore).

colour(black, 0, 0, 0).
colour(snow, 255, 250, 250).
colour(ghostwhite, 248, 248, 255).
colour(whitesmoke, 245, 245, 245).
colour(gainsboro, 220, 220, 220).
colour(floralwhite, 255, 250, 240).
colour(oldlace, 253, 245, 230).
colour(linen, 250, 240, 230).
colour(antiquewhite, 250, 235, 215).
colour(papayawhip, 255, 239, 213).
colour(blanchedalmond, 255, 235, 205).
colour(bisque, 255, 228, 196).
colour(peachpuff, 255, 218, 185).
colour(navajowhite, 255, 222, 173).
colour(moccasin, 255, 228, 181).
colour(cornsilk, 255, 248, 220).
colour(ivory, 255, 255, 240).
colour(lemonchiffon, 255, 250, 205).
colour(seashell, 255, 245, 238).
colour(honeydew, 240, 255, 240).
colour(mintcream, 245, 255, 250).
colour(azure, 240, 255, 255).
colour(aliceblue, 240, 248, 255).
colour(lavender, 230, 230, 250).
colour(lavenderblush, 255, 240, 245).
colour(mistyrose, 255, 228, 225).
colour(darkslategray, 47, 79, 79).
colour(dimgray, 105, 105, 105).
colour(slategray, 112, 128, 144).
colour(lightslategray, 119, 136, 153).
colour(gray, 192, 192, 192).
colour(lightgrey, 211, 211, 211).
colour(midnightblue, 25, 25, 112).
colour(navy, 0, 0, 128).
colour(cornflowerblue, 100, 149, 237).
colour(darkslateblue, 72, 61, 139).
colour(slateblue, 106, 90, 205).
colour(mediumslateblue, 123, 104, 238).
colour(lightslateblue, 132, 112, 255).
colour(mediumblue, 0, 0, 205).
colour(royalblue, 65, 105, 225).
colour(blue, 0, 0, 255).
colour(dodgerblue, 30, 144, 255).
colour(deepskyblue, 0, 191, 255).
colour(skyblue, 135, 206, 235).
colour(lightskyblue, 135, 206, 250).
colour(steelblue, 70, 130, 180).
colour(lightsteelblue, 176, 196, 222).
colour(lightblue, 173, 216, 230).
colour(powderblue, 176, 224, 230).
colour(paleturquoise, 175, 238, 238).
colour(darkturquoise, 0, 206, 209).
colour(mediumturquoise, 72, 209, 204).
colour(turquoise, 64, 224, 208).
colour(cyan, 0, 255, 255).
colour(lightcyan, 224, 255, 255).
colour(cadetblue, 95, 158, 160).
colour(mediumaquamarine, 102, 205, 170).
colour(aquamarine, 127, 255, 212).
colour(darkgreen, 0, 100, 0).
colour(darkolivegreen, 85, 107, 47).
colour(darkseagreen, 143, 188, 143).
colour(seagreen, 46, 139, 87).
colour(mediumseagreen, 60, 179, 113).
colour(lightseagreen, 32, 178, 170).
colour(palegreen, 152, 251, 152).
colour(springgreen, 0, 255, 127).
colour(lawngreen, 124, 252, 0).
colour(green, 0, 255, 0).
colour(chartreuse, 127, 255, 0).
colour(mediumspringgreen, 0, 250, 154).
colour(greenyellow, 173, 255, 47).
colour(limegreen, 50, 205, 50).
colour(yellowgreen, 154, 205, 50).
colour(forestgreen, 34, 139, 34).
colour(olivedrab, 107, 142, 35).
colour(darkkhaki, 189, 183, 107).
colour(khaki, 240, 230, 140).
colour(palegoldenrod, 238, 232, 170).
colour(lightgoldenrodyellow, 250, 250, 210).
colour(lightyellow, 255, 255, 224).
colour(yellow, 255, 255, 0).
colour(gold, 255, 215, 0).
colour(lightgoldenrod, 238, 221, 130).
colour(goldenrod, 218, 165, 32).
colour(darkgoldenrod, 184, 134, 11).
colour(rosybrown, 188, 143, 143).
colour(indianred, 205, 92, 92).
colour(saddlebrown, 139, 69, 19).
colour(sienna, 160, 82, 45).
colour(peru, 205, 133, 63).
colour(burlywood, 222, 184, 135).
colour(beige, 245, 245, 220).
colour(wheat, 245, 222, 179).
colour(sandybrown, 244, 164, 96).
colour(tan, 210, 180, 140).
colour(chocolate, 210, 105, 30).
colour(firebrick, 178, 34, 34).
colour(brown, 165, 42, 42).
colour(darksalmon, 233, 150, 122).
colour(salmon, 250, 128, 114).
colour(lightsalmon, 255, 160, 122).
colour(orange, 255, 165, 0).
colour(darkorange, 255, 140, 0).
colour(coral, 255, 127, 80).
colour(lightcoral, 240, 128, 128).
colour(tomato, 255, 99, 71).
colour(orangered, 255, 69, 0).
colour(red, 255, 0, 0).
colour(hotpink, 255, 105, 180).
colour(deeppink, 255, 20, 147).
colour(pink, 255, 192, 203).
colour(lightpink, 255, 182, 193).
colour(palevioletred, 219, 112, 147).
colour(maroon, 176, 48, 96).
colour(mediumvioletred, 199, 21, 133).
colour(violetred, 208, 32, 144).
colour(magenta, 255, 0, 255).
colour(violet, 238, 130, 238).
colour(plum, 221, 160, 221).
colour(orchid, 218, 112, 214).
colour(mediumorchid, 186, 85, 211).
colour(darkorchid, 153, 50, 204).
colour(darkviolet, 148, 0, 211).
colour(blueviolet, 138, 43, 226).
colour(thistle, 216, 191, 216).
colour(white, 255, 255, 255).

/*****************************************************************************
* compiled declarations
*****************************************************************************/
:- skipped
	speed_pce/1,
	make_file_popup/3, 
	make_file_popup/4, 
	make_tree_proto/3, 
	show_tree/4, 
	show_tree/5, 
	new_node/4, 
	destroy_obj/1, 
	fit_tree/2, 
	fit_tree/4, 
	create_font_popup/1, 
	enumerate/2, 
	enumerate_ref/2, 
	get_chain/3, 
	get_chain_ref/3, 
	warning/3, 
	warning/5, 
	alert/3, 
	alert/4, 
	alert/6, 
	prompt/4, 
	prompt/7, 
	prompt/8, 
	prompt/9, 
	new_icon/2, 
	new_cursor/2, 
	dump_window/3, 
	change_cursor/2, 
	new_dialog/3, 
	init_colours/0,
	init_colourmap/5,
	new_bar/3.

/*****************************************************************************
* End of k_gps.pl
*****************************************************************************/
