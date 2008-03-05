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
% Contributor(s): Joachim Schimpf.
% 
% END LICENSE BLOCK
% ----------------------------------------------------------------------
% System:	ECLiPSe Constraint Logic Programming System
% Component:	ECLiPSe III compiler
% Version:	$Id: compiler_regassign.ecl,v 1.2 2008/03/05 03:59:54 jschimpf Exp $
% ----------------------------------------------------------------------

:- module(compiler_regassign).

:- comment(summary, "ECLiPSe III Compiler - register allocator").
:- comment(copyright, "Cisco Technology Inc").
:- comment(author, "Joachim Schimpf").
:- comment(date, "$Date: 2008/03/05 03:59:54 $").

:- lib(hash).
:- use_module(compiler_common).


%----------------------------------------------------------------------
% Data structures
%----------------------------------------------------------------------

% A struct(location) is created locally in a chunk for each varid
% encountered there. It is used to maintain the current register
% locations of the variable, and in case of permanents to cache its y(I).

:- local struct(location(
	current,		% "IsIn" \/ "Assigned"
	desirable		% "Destination"
    )).



%----------------------------------------------------------------------
% 
%----------------------------------------------------------------------

:- comment(assign_am_registers/3, [
    summary:"Assign concrete registers in the WAM code",
    amode:assign_am_registers(+,-,?),
    args:[
	"AnnotatedCodeList":"List of annotated code with unassigned registers",
	"Code":"Head of resulting register-assigned annotated code",
	"CodeEnd":"Tail of resulting register-assigned annotated code"
    ],
    see_also:[struct(code)],
    desc:ascii("
    	This is the register allocator. It takes as input a list of annotated
	WAM code for one chunk, which still contains unassigned registers.
	It produces a difference list of WAM code with all registers assigned,
	and possibly additional move-instructions inserted.

	See module description for details.
    ")
]).

:- export assign_am_registers/3.

assign_am_registers(AnnotatedCodeList, Code, Code0) :-

	% build a reverse list of register descriptors
	(
	    foreach(code{regs:RegDescs}, AnnotatedCodeList),
	    fromto([],RRD1,RRD3,ReverseRegDescs)
	do
	    (
		foreach(RegDesc, RegDescs),
		fromto(RRD1,RRD2,[RegDesc|RRD2],RRD3)
	    do
		true
	    )
	),

	% backward-pass, using the just constructed reverse list:
	%  - mark last occurrences
	%  - collect original locations
	%  - collect desirable locations

	init_location_table(Locations0),
	init_contents_table(Contains0),
	hash_create(LastSeen0),
	(
	    loop_name(backwards_pass),
	    foreach(r(VarId,Reg,Type,Last), ReverseRegDescs),
	    fromto(Desirables,Desirable2,Desirable3,[]),
	    fromto(LastSeen0,LastSeen1,LastSeen2,_),
	    fromto([], Stack1, Stack2, _),
	    param(Locations0,Contains0)
	do
	    verify nonvar(Type),
	    ( Type = split ->
		Stack2 = Stack1,
		LastSeen2 = LastSeen1,
		Desirable2 = Desirable3
	    ; Type = restore ->
		Stack1 = [LastSeen2|Stack2],
		Desirable2 = Desirable3
	    ; Type = join ->
		hash_clone(LastSeen1, LastSeen2),
		Stack2 = [LastSeen1|Stack1],
		Desirable2 = Desirable3
	    ;
		Stack2 = Stack1,
		LastSeen2 = LastSeen1,
		( Type = orig ->
		    verify nonvar(Reg),
		    verify register_is_free(Contains0, Reg),
		    Desirable2 = Desirable3,
		    set_current_content(Contains0, Reg, VarId),
		    add_current_location(Locations0, VarId, Reg)
		; Type = dest ->
		    verify nonvar(Reg),
		    Desirable2 = [Reg|Desirable3],
		    add_desirable_location(Locations0, VarId, Reg)
		; Type = perm ->
		    Desirable2 = Desirable3
		;
		    verify var(Reg),
		    Desirable2 = Desirable3
		),
		( hash_contains(LastSeen1, VarId) ->
		    true
		;
		    Last = last,
		    hash_set(LastSeen1, VarId, seen)
		)
	    )
	),

	% main pass, assigning concrete registers
	(
	    loop_name(assign_registers_outer),
	    foreach(AnnotatedInstr, AnnotatedCodeList),
	    fromto(Code, Code1, Code9, Code0),
	    fromto(Locations0, Locations1, Locations4, _),
	    fromto(Contains0, Contains1, Contains4, _),
	    fromto([], Stack1, Stack4, _),
	    param(Desirables)
	do
	    AnnotatedInstr = code{regs:RegDescs},
	    Code8 = [AnnotatedInstr|Code9],

	    % RegDescs is a list of register descriptors, either:
	    % - orig*
	    % - dest*
	    % - perm* use* def* perm*

	    (
		loop_name(assign_registers_inner),
		foreach(r(VarId,Reg,Type,Last), RegDescs),
		fromto([],Committed1,Committed2,_),
		fromto(Code1, Code2, Code6, Code8),
		fromto(Locations1, Locations, Locations3, Locations4),
		fromto(Contains1, Contains, Contains3, Contains4),
		fromto(Stack1, Stack2, Stack3, Stack4),
		param(Desirables)
	    do

		% assign_one_register(VarId, Type, Last, Committed, Code2, Code6, Locations, Contains, Desirables, Reg)

		( Type = use ->
		    % not the first occurrence
		    Locations3 = Locations,
		    Contains3 = Contains,
		    Stack3 = Stack2,
		    Committed2 = [Reg|Committed1],
		    certainly_once current_location(Locations, VarId, RegOrSlot),
		    ( RegOrSlot = a(_) ->
			% value is in a register, use it
			Reg = RegOrSlot,
			Code2 = Code6
		    ;
			% value was spilled: find a register now and reload from environment
			% Cannot use a register that is already committed for the current instruction
			find_any_register_for(VarId, Locations, Contains, Desirables, Committed1, [], Code2, Code5, Reg),
			Code5 = [code{instr:move(RegOrSlot,Reg),comment:unspill(VarId)}|Code6]
		    )

		; Type = def ->
		    Locations3 = Locations,
		    Contains3 = Contains,
		    Stack3 = Stack2,
		    Committed2 = Committed1,
		    % Assume that the current instruction's used registers can be reused already
		    find_any_register_for(VarId, Locations, Contains, Desirables, [], Committed1, Code2, Code6, Reg)

		; Type = orig ->
		    verify current_location(Locations, VarId, Reg),
		    Locations3 = Locations,
		    Contains3 = Contains,
		    Stack3 = Stack2,
		    Committed2 = Committed1,
		    Code2 = Code6

		; Type = dest ->
		    Locations3 = Locations,
		    Contains3 = Contains,
		    Stack3 = Stack2,
		    Committed2 = [Reg|Committed1],
		    ( current_location(Locations, VarId, Reg) ->
			% already in the right place
			Code2 = Code6
		    ; current_location(Locations, VarId, RegOrSlot) ->
			% move from a different register or env slot
			( register_is_free(Contains, Reg) ->
			    Code2 = Code5
			; % RegOrSlot = a(_) ->
			    % To vacate Reg, we cannot employ regs that are already committed for
			    % the current instruction, nor RegOrSlot, which is the register from
			    % where we are going to move data into Reg.
			    vacate_register(Reg, Locations, Contains, Desirables, [RegOrSlot|Committed1], Code2, Code5)
			/*
			;
			    % IS THIS TRUE?
			    % this cannot occur with the current allocation strategy (vars that
			    % have a destination are always allocated there and remain there.
			    % vacate_register(Reg, Locations, Contains, Desirables, Committed1, Code2, Code5)
			    verify false
			    */
			),
			set_current_content(Contains, Reg, VarId),
			add_current_location(Locations, VarId, Reg),
			Code5 = [code{instr:move(RegOrSlot,Reg)}|Code6]
		    ;
			% first occurrence
			verify false
		    )

		; Type = perm ->
		    % Informs allocator where a permanent variable is located
		    verify (nonvar(Reg), Reg = y(_)),
		    Locations3 = Locations,
		    Contains3 = Contains,
		    Stack3 = Stack2,
		    add_permanent_location(Locations, VarId, Reg),
		    Committed2 = Committed1,
		    Code2 = Code6

		; Type = split ->
		    Code2 = Code6,
		    Committed2 = Committed1,
		    Locations3 = Locations,
		    Contains3 = Contains,
		    hash_clone(Locations, LocationsSave),
		    hash_clone(Contains, ContainsSave),
		    Stack3 = [state(LocationsSave,ContainsSave,Committed1)|Stack2]

		; Type = restore ->
		    Code2 = Code6,
		    Stack2 = [state(Locations3,Contains3,Committed2)|Stack3]

		; Type = join ->
		    Code2 = Code6,
		    Committed2 = Committed1,
		    Locations3 = Locations,
		    Contains3 = Contains,
		    Stack3 = Stack2

		;
		    verify false
		),
%		( Last == last, Type \= dest, Type \== perm ->
		( last_occurrence(Type, Last) ->
		    % No need to clean up the Locations table,
		    % since VarId won't occur any more
		    % Next two lines are only to make debugging easier!
		    certainly_once current_content(Contains, Reg, NoLongerUsedVarId),
		    clear_locations(Locations, NoLongerUsedVarId),

		    clear_current_content(Contains, Reg)
		;
		    true
		)
	    )
	).

	last_occurrence(def, last) ?- true.
	last_occurrence(orig, last) ?- true.
	last_occurrence(use, last) ?- true.


find_good_register_for(VarId, Locations, Contains, Desirables, DontUse, DontFree, Code, Code0, Reg) :-
	% DontUse: don't use even if free
	% DontFree: these register's contents cannot be changed
	%	until after the current instruction
	verify \+(current_location(Locations, VarId, a(_))),
	( desirable_location(Locations, VarId, Reg), register_is_free(Contains, Reg), nonmember(Reg, DontUse) ->
	    % found a free&desirable one
	    Code = Code0
	; desirable_location(Locations, VarId, Reg), nonmember(Reg, DontUse), nonmember(Reg,DontFree) ->
	    % use a desirable and free it up
	    verify \+(register_is_free(Contains, Reg)),
	    append(DontFree, DontUse, DontFree1),
	    vacate_register(Reg, Locations, Contains, Desirables, DontFree1, Code, Code0)
	; free_register(Contains, Reg), nonmember(Reg, Desirables), nonmember(Reg, DontUse) ->
	    % there are no desirables, use any free one
	    Code = Code0
	; redundant_source_register(Contains, Locations, Reg), nonmember(Reg, Desirables), nonmember(Reg, DontUse) ->
	    % or one that can be freed without losing anything
	    Code = Code0
	;
	    % 
	    fail
	).


find_any_register_for(VarId, Locations, Contains, Desirables, DontUse, DontFree, Code2, Code6, Reg) :-
	( find_good_register_for(VarId, Locations, Contains, Desirables, DontUse, DontFree, Code2, Code6, Reg) ->
	    true
	; single_source_register(Contains, Locations, Reg), nonmember(Reg, Desirables), nonmember(Reg, DontUse), nonmember(Reg, DontFree) ->
	    % last resort: free a single-source register
	    % this should always exist if the AM has enough registers
	    append(DontFree, DontUse, DontFree1),
	    vacate_register(Reg, Locations, Contains, Desirables, DontFree1, Code2, Code6)
	;
	    verify false
	),
	set_current_content(Contains, Reg, VarId),
	add_current_location(Locations, VarId, Reg).


vacate_register(Reg, Locations, Contains, Desirables, DontFree, Code, Code0) :-
	% PRE: Reg is not free
	% PRE: Reg is a destination for VarId or for no one
	certainly_once current_content(Contains, Reg, OldVarId),
	remove_current_location(Locations, OldVarId, Reg),
	clear_current_content(Contains, Reg),
	( current_location(Locations, OldVarId, _RegOrSlot) ->
	    % there is still an alternative location (a(_) or y(_))
	    Code = Code0
	; find_good_register_for(OldVarId, Locations, Contains, Desirables, [Reg|DontFree], [], Code, Code1, AltReg) ->
	    verify AltReg \== Reg,
	    Code1 = [code{instr:move(Reg,AltReg)}|Code0],
	    add_current_location(Locations, OldVarId, AltReg),
	    set_current_content(Contains, AltReg, OldVarId)
	;
	    % spill Reg into memory
	    Slot = y(_Y),
	    Code = [code{instr:move(Reg,Slot),comment:spill(OldVarId)}|Code0],
	    add_current_location(Locations, OldVarId, Slot)
	).


% contents table

init_contents_table(Contains) :-
	hash_create(Contains).

register_is_free(Contains, Reg) :-
	\+ hash_get(Contains, Reg, _).

current_content(Contains, Reg, Content) :-
	hash_get(Contains, Reg, Content).

set_current_content(Contains, Reg, Content) :-
	hash_set(Contains, Reg, Content).

clear_current_content(Contains, Reg) :-
	hash_delete(Contains, Reg).


% location table

init_location_table(Locations) :-
	hash_create(Locations).

add_desirable_location(Locations, VarId, Reg) :-
	( hash_get(Locations, VarId, Entry) ->
	    Entry = location{desirable:Desirable},
	    update_struct(location, desirable:[Reg|Desirable], Entry, NewEntry)
	;
	    NewEntry = location{current:[],desirable:[Reg]}
	),
	hash_set(Locations, VarId, NewEntry).

desirable_location(Locations, VarId, Reg) :-
	hash_get(Locations, VarId, Entry),
	Entry = location{desirable:Desirables},
	member(Reg, Desirables).

add_current_location(Locations, VarId, Reg) :-
	( hash_get(Locations, VarId, Entry) ->
	    Entry = location{current:Current},
	    update_struct(location, current:[Reg|Current], Entry, NewEntry)
	;
	    NewEntry = location{current:[Reg],desirable:[]}
	),
	hash_set(Locations, VarId, NewEntry).

add_permanent_location(Locations, VarId, Slot) :-
	( hash_get(Locations, VarId, Entry) ->
	    Entry = location{current:Current},
	    append(Current, [Slot], Current1),
	    update_struct(location, current:Current1, Entry, NewEntry)
	;
	    NewEntry = location{current:[Slot],desirable:[]}
	),
	hash_set(Locations, VarId, NewEntry).

remove_current_location(Locations, VarId, Reg) :-
	( hash_get(Locations, VarId, Entry) ->
	    Entry = location{current:Current},
	    once delete(Reg, Current, Current1),
	    verify nonmember(Reg, Current1),
	    update_struct(location, current:Current1, Entry, NewEntry)
	;
	    verify false
	),
	hash_set(Locations, VarId, NewEntry).


current_locations(Locations, VarId, Currents) :-
	hash_get(Locations, VarId, Entry),
	Entry = location{current:Currents}.

current_location(Locations, VarId, Reg) :-
	hash_get(Locations, VarId, Entry),
	Entry = location{current:Currents},
	member(Reg, Currents).

clear_locations(Locations, VarId) :-
	hash_delete(Locations, VarId).

% Enumerate all free registers
% This should be sped up using an explicit free-list
free_register(Contains, Reg) :-
	Reg = a(N),
	wam_registers(Max),
	between(1, Max, 1, N),
	register_is_free(Contains, Reg).

% Enumerate all non-free registers that are only a second source
redundant_source_register(Contains, Locations, Reg) :-
	Reg = a(I),
	% replace next 3 lines with
	% hash_current(Contains, Reg, VarId)
	wam_registers(Max),
	between(1, Max, 1, I),
	current_content(Contains, Reg, VarId),
	current_locations(Locations, VarId, [_,_|_]).

% Enumerate all non-free registers that are the only source for their value
single_source_register(Contains, Locations, Reg) :-
	Reg = a(I),
	% replace next 3 lines with
	% hash_current(Contains, Reg, VarId)
	wam_registers(Max),
	between(1, Max, 1, I),
	current_content(Contains, Reg, VarId),
	current_locations(Locations, VarId, [_]).



:- comment(desc, ascii("

Input format:

    The input to the register allocator is annotated code of the form:

	    code{
		instr		the AM instruction
		regs		a list of value/register descriptors
		comment
	    }

    The allocator reads only the regs-field and instantiates it further!
    It does not look at the actual instruction. The registers to be alloacted
    are variables which are shared between instruction and descriptor, e.g.
    R1 and R2 in

    	code{instr:move(R1,R2), regs:[r(73,R1,use,_),r(45,R2,def,_)}

    The allocator produces a copy of the input code list, consisting of the
    (further instantiated) input elements, and possibly with move instructions
    inserted (to resolve register conflicts and for spilling/unspilling).

    Register descriptors can be of one of the following forms:

	r(+VarId, ?Reg, +Type, -LastFlag)

	r(33, a(2), orig, _)
		says that variable 33 is in register 2 from the beginning
		of the chunk.
		This occurs in a pseudo-instruction for the chunk head.

	r(33, a(2), dest, _)
		instruction expects variable 33 in register 2 and that
		register will hold that value until the end of the chunk.
		the register allocator will insert move instructions
		if the variable could not be placed in register 2 previously.
		This may be followed only by more occurrences of the 'dest'
		type for this variable.
		This occurs in call-instruction at the end of the chunk.

	r(33, R, def, L)
		instruction puts variable 33 in register R.
		value for R can be chosen by the register allocator.
		This must be the first occurrence of variable 33 in the chunk.

	r(33, R, use, L)
		instruction expects variable 33 in register R.
		value for R can be chosen by the register allocator.
		This can be a middle/last occurrence of variable 33 in the chunk.

	r(33, y(Y), perm, L)
		not actually a register descriptor. It is used to inform the
		allocator about the location of a permanent variable. This is
		useful when a permanent variable is also available in a register.
		It is then treated like an unspilled variable, i.e. it always
		has a second source.
 
    Within an instruction, the RegDescs list must be ordered such that
    used regs are first, defined ones later (if they can be reused), reflecting
    the order in which the registers are accessed by the abstract machine.

Assumptions about the abstract machine:

    There must be enough argument registers to hold all arguments
    of a regular call plus all arguments of a simple call, i.e.
    max_regular_arity + max_simple_arity (e.g. 255 + 16).
    If this is the case, we never need to move things out of desirable
    registers (since there can be no more than max_regular_arity
    desirable registers). We can still make a simple call even if
    all arguments for the following regular call are already in place.
    We may however need to spill other temps.
	
Register overflow:
	
    We call 'unimportant' a register which is not desirable for any
    variable, and which is not the only source for any variable.
    When we run out of free registers and unimportant registers that
    can be freed easily, then we need to spill an important register
    into an environment slot.
    Because of the above requirement on register numbers,
    we can restrict ourselves to only spilling undesirable registers.


find_any_register_for(+VarId, ..., -Reg)
	(1) currently free and a destination for VarId
	(2) A destination for VarId which is not already used in the current instruction
		(this is not free and needs to be freed)
		vacate_register_for(Reg, VarId)
	(3) A non-destination (for anyone) which is free and not already used in the current instruction
	(4) An unimportant (second-source) register which is not a destination for anyone (possibly already used in the current instruction)
		set it free
	(5) An only-source, non-destination (for anyone) which is not already used in the current instruction
		(this is not free and needs to be freed)
		vacate_register_for(Reg, VarId)
	(x) Does not happen because (destinations+used_in_current_instruction) is less
	    or equal than the number of machine registers.

find_free_location_for(+VarId, ..., -RegOrSlot)
	(1) - (4) as above
	(s) Spill VarId into new env slot RegOrSlot



Overview of the different cases:

	free	dest/me		dest/other	used-here

(1)	y	y		(n)		dc	ideal, cheap
(2)	n	y		(n)		n	ideal, unavoidable cost
(3)	y	n		n		dc	cheap
(4)	red.	n		n		dc	cheap
(5)	n	n		n		n	costly

(6)	y	n		y		dc	keep it free for final value
	n	n		y		dc	keep it for final value
	n	dc		n		y	impossible to reuse yet


TODO: case (6) should probably be used as well (between 4 and 5 or after 5)
because it may cost nothing (if the lifetime ends before it it needed as a
destination).

TODO: tell the register allocator about the fact that a value is also available
in an environment slot (as a second source).

")).

%----------------------------------------------------------------------
% Unit tests
%----------------------------------------------------------------------

:- import print_annotated_code/1 from compiler_codegen.

:- export regtest/1.
regtest(N) :-
	sample(N, ACL),
	writeln(test(N)),
	assign_am_registers(ACL, Code, []),
	print_annotated_code(Code).

:- export regtest_all/0.
regtest_all :-
	open("compiler_regassign.res", write, output),
	(
	    regtest(_),
	    fail
	;
	    true
	),
	close(output).


sample(1, ACL) :-
	ACL = [
	    code{instr:nop,regs:[]},
	    code{instr:put_variable(R),regs:[r(1,R,def,_)]},
	    code{instr:call(foo/1),regs:[r(1,a(1),dest,_)]}
	].
sample(1001, ACL) :-
	ACL = [
	    code{instr:nop,regs:[r(2,a(1),orig,_)]},
	    code{instr:put_variable(R),regs:[r(1,R,def,_)]},
	    code{instr:call(foo/1),regs:[r(1,a(1),dest,_)]}
	].
sample(1002, ACL) :-
	ACL = [
	    code{instr:nop,regs:[r(2,a(1),orig,_)]},
	    code{instr:put_variable(R),regs:[r(1,R,def,_)]},
	    code{instr:call(foo/1),regs:[r(1,a(1),dest,_),r(2,a(2),dest,_)]}
	].
sample(1003, ACL) :-
	ACL = [
	    code{instr:nop,regs:[r(2,a(1),orig,_)]},
	    code{instr:call(foo/1),regs:[r(2,a(1),dest,_),r(2,a(2),dest,_),r(2,a(3),dest,_)]}
	].
sample(2, ACL) :-
	ACL = [
	    code{instr:nop,regs:[r(1,a(1),orig,_)]},
	    code{instr:call(foo/1),regs:[r(1,a(1),dest,_)]}
	].
sample(3, ACL) :-
	ACL = [
	    code{instr:nop,regs:[r(1,a(1),orig,_),r(2,a(2),orig,_),r(3,a(3),orig,_)]},
	    code{instr:call(foo/1),regs:[r(2,a(1),dest,_),r(3,a(3),dest,_)]}
	].
sample(4, ACL) :-
	ACL = [
	    code{instr:nop,regs:[r(1,a(1),orig,_),r(2,a(2),orig,_),r(3,a(3),orig,_)]},
	    code{instr:call(foo/1),regs:[r(2,a(1),dest,_),r(1,a(2),dest,_),r(3,a(3),dest,_)]}
	].
sample(5, ACL) :-
	ACL = [
	    code{instr:nop,regs:[r(1,a(1),orig,_),r(2,a(2),orig,_),r(3,a(3),orig,_)]},
	    code{instr:call(foo/1),regs:[r(3,a(1),dest,_),r(1,a(2),dest,_),r(2,a(3),dest,_)]}
	].
sample(6, ACL) :-
	ACL = [
	    code{instr:nop,regs:[r(1,a(1),orig,_),r(2,a(2),orig,_),r(3,a(3),orig,_)]},
	    code{instr:call(foo/1),regs:[r(3,a(1),dest,_),r(1,a(2),dest,_),r(1,a(3),dest,_)]}
	].
sample(7, ACL) :-
	ACL = [
	    code{instr:nop,regs:[r(1,a(1),orig,_),r(2,a(2),orig,_),r(3,a(3),orig,_)]},
	    code{instr:call(foo/1),regs:[r(1,a(1),dest,_),r(1,a(2),dest,_),r(1,a(3),dest,_)]}
	].
sample(8, ACL) :-
	ACL = [
	    code{instr:nop,regs:[r(1,a(1),orig,_),r(2,a(2),orig,_),r(3,a(3),orig,_)]},
	    code{instr:call(foo/1),regs:[r(3,a(1),dest,_),r(3,a(2),dest,_),r(3,a(3),dest,_)]}
	].
sample(9, ACL) :-
	ACL = [
	    code{instr:nop,regs:[r(1,a(1),orig,_),r(2,a(2),orig,_),r(3,a(3),orig,_)]},
	    code{instr:call(foo/1),regs:[r(3,a(1),dest,_),r(3,a(2),dest,_),r(3,a(3),dest,_)]}
	].
sample(10, ACL) :-
	ACL = [
	    code{instr:nop,regs:[r(1,a(1),orig,_),r(2,a(2),orig,_),r(3,a(3),orig,_)]},
	    code{instr:add(A,B,C),regs:[r(1,A,use,_),r(3,B,use,_),r(4,C,def,_)]},
	    code{instr:call(foo/1),regs:[r(4,a(1),dest,_),r(2,a(2),dest,_)]}
	].
sample(11, ACL) :-
	ACL = [
	    code{instr:nop,regs:[r(1,a(1),orig,_),r(2,a(2),orig,_),r(3,a(3),orig,_)]},
	    code{instr:add(A,B,C),regs:[r(1,A,use,_),r(3,B,use,_),r(4,C,def,_)]},
	    code{instr:add(D,E,F),regs:[r(1,D,use,_),r(2,E,use,_),r(5,F,def,_)]},
	    code{instr:call(foo/1),regs:[r(4,a(1),dest,_),r(2,a(2),dest,_),r(5,a(3),dest,_)]}
	].
sample(12, ACL) :-
	ACL = [
	    code{instr:nop,regs:[r(1,a(1),orig,_),r(2,a(2),orig,_),r(3,a(3),orig,_)]},
	    code{instr:add(A,B,C),regs:[r(1,A,use,_),r(3,B,use,_),r(4,C,def,_)]},
	    code{instr:add(D,E,F),regs:[r(1,D,use,_),r(4,E,use,_),r(5,F,def,_)]},
	    code{instr:call(foo/1),regs:[r(5,a(1),dest,_),r(2,a(2),dest,_)]}
	].

% checks for register pressure (to be run with limit of 5 registers)
sample(100, ACL) :-
	ACL = [
	    code{instr:nop,regs:[r(1,a(1),orig,_),r(2,a(2),orig,_),r(3,a(3),orig,_),r(4,a(4),orig,_)]},
	    code{instr:put_variable(R1),regs:[r(5,R1,def,_)]},
	    code{instr:put_variable(R3),regs:[r(6,R3,def,_)]},
	    code{instr:use_variable(R4),regs:[r(6,R4,use,_)]},
	    code{instr:use_variable(R5),regs:[r(6,R5,use,_)]},
	    code{instr:use_variable(R2),regs:[r(5,R2,use,_)]},
	    code{instr:use_variable(R6),regs:[r(5,R6,use,_)]},
	    code{instr:call(foo/5),regs:[r(1,a(1),dest,_),r(2,a(2),dest,_),r(3,a(3),dest,_),r(4,a(4),dest,_)]}
	].
sample(101, ACL) :-
	ACL = [
	    code{instr:nop,regs:[r(1,a(1),orig,_),r(2,a(2),orig,_),r(3,a(3),orig,_)]},
	    code{instr:put_variable(R1),regs:[r(5,R1,def,_)]},
	    code{instr:put_variable(R2),regs:[r(6,R2,def,_)]},
	    code{instr:put_variable(R3),regs:[r(7,R3,def,_)]},
	    code{instr:put_variable(R4),regs:[r(8,R4,def,_)]},
	    code{instr:use_variable(R5),regs:[r(7,R5,use,_)]},
	    code{instr:use_variables(R7,R8),regs:[r(5,R7,use,_),r(6,R8,use,_)]},
	    code{instr:call(foo/5),regs:[r(1,a(1),dest,_),r(2,a(2),dest,_),r(3,a(3),dest,_)]}
	].
sample(102, ACL) :-
	ACL = [
	    code{instr:nop,regs:[r(1,a(1),orig,_),r(2,a(2),orig,_),r(3,a(3),orig,_)]},
	    code{instr:put_variable(R1),regs:[r(5,R1,def,_)]},
	    code{instr:put_variable(R2),regs:[r(6,R2,def,_)]},
	    code{instr:put_variable(R3),regs:[r(7,R3,def,_)]},
	    code{instr:put_variable(R4),regs:[r(8,R4,def,_)]},
	    code{instr:use_variable(R5),regs:[r(7,R5,use,_)]},
	    code{instr:use_variable(R6),regs:[r(8,R6,use,_)]},
	    code{instr:use_variables(R7,R8),regs:[r(5,R7,use,_),r(6,R8,use,_)]},
	    code{instr:call(foo/5),regs:[r(1,a(1),dest,_),r(2,a(2),dest,_),r(3,a(3),dest,_)]}
	].
sample(103, ACL) :-
	ACL = [
	    code{instr:nop,regs:[r(1,a(1),orig,_),r(2,a(2),orig,_),r(3,a(3),orig,_)]},
	    code{instr:put_variable(R1),regs:[r(5,R1,def,_)]},
	    code{instr:put_variable(R2),regs:[r(6,R2,def,_)]},
	    code{instr:put_variable(R3),regs:[r(7,R3,def,_)]},
	    code{instr:put_variable(R4),regs:[r(8,R4,def,_)]},
	    code{instr:use_variables(R7,R8),regs:[r(7,R7,use,_),r(8,R8,use,_)]},
	    code{instr:use_variables(R9,RA),regs:[r(5,R9,use,_),r(6,RA,use,_)]},
	    code{instr:call(foo/5),regs:[r(1,a(1),dest,_),r(2,a(2),dest,_),r(3,a(3),dest,_)]}
	].
sample(104, ACL) :-
	ACL = [
	    code{instr:nop,regs:[r(4,a(1),orig,_),r(3,a(2),orig,_),r(2,a(3),orig,_)]},
	    code{instr:move(R1,R2),regs:[r(4,R1,use,_),r(1,R2,def,_)]},
	    code{instr:call(foo/3),regs:[r(2,a(1),dest,_),r(1,a(2),dest,_),r(3,a(3),dest,_)]}
	].

sample(200, ACL) :-
	ACL = [
	    code{instr:put_structure(R1,s/1),regs:[r(out(3),R1,def,_)]},
	    code{instr:put_variable(R2),regs:[r(out(2),R2,def,_)]},
	    code{instr:put_atom(R3,a),regs:[r(out(1),R3,def,_)]},
	    code{instr:call(foo/5),regs:[r(out(1),a(1),dest,_),r(out(2),a(2),dest,_),r(out(3),a(3),dest,_)]}
	].
sample(201, ACL) :-
	ACL = [
	    code{instr:put_structure(R1,s/1),regs:[r(out(3),R1,def,_)]},
	    code{instr:put_variable(R2),regs:[r(out(2),R2,def,_)]},
	    code{instr:put_atom(R3,a),regs:[r(out(1),R3,def,_)]},
	    code{instr:call(foo/5),regs:[r(out(1),a(1),dest,_),r(out(3),a(3),dest,_)]}
	].

sample(300, ACL) :-
	ACL = [
	    code{instr:nop,regs:[r(1,a(1),orig,_),r(2,a(2),orig,_)]},
	    code{instr:nop,regs:[r(_,_,split,_)]},
	    code{instr:nop,regs:[r(2,_R2a,use,_)]},
	    code{instr:nop,regs:[r(3,_R3a,def,_)]},
	    code{instr:nop,regs:[r(_,_,restore,_)]},
	    code{instr:nop,regs:[r(2,_R2b,use,_)]},
	    code{instr:nop,regs:[r(3,_R3b,def,_)]},
	    code{instr:nop,regs:[r(_,_,join,_)]},
	    code{instr:nop,regs:[r(3,_R3c,use,_)]},
	    code{instr:call(foo/1),regs:[r(1,a(1),dest,_)]}
	].
