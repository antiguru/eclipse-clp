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
% Version:	$Id: compiler_peephole.ecl,v 1.3 2007/05/17 23:59:43 jschimpf Exp $
% ----------------------------------------------------------------------

:- module(compiler_peephole).

:- comment(summary, "ECLiPSe III compiler - peephole optimizer").
:- comment(copyright, "Cisco Technology Inc").
:- comment(author, "Joachim Schimpf").
:- comment(date, "$Date: 2007/05/17 23:59:43 $").

:- comment(desc, ascii("
    This is very preliminary!

    This pass does simple code improvements like:

	 - eliminating nop
	 - eliminating move(X,X)
	 - merging instructions (e.g. call+ret -> chain)

    Takes a list of register-allocated, annotated code (but ignores
    all annotations).  Annotations are stripped, code simplified,
    and a plain list of instructions is returned, which can be fed
    into the assembler.
")).

:- use_module(compiler_common).

:- import meta_index/2 from sepia_kernel.

:- comment(simplify_code/2, [
    summary:"Strip annotations and do peephole optimizations",
    amode:simplify_code(+,-),
    args:[
	"AnnotatedCodeIn":"A list of annotated WAM code (struct(code))",
	"WamCodeOut":"A list of WAM instructions in lib(asm) format"
    ],
    see_also:[struct(code)]
]).

:- export simplify_code/2.


% simplify and strip annotations

simplify_code([], []).
simplify_code([code{instr:Instr}|More], SimplifiedCode) :-
	( simplify(Instr, More, SimplifiedCode0) ->
	    simplify_code(SimplifiedCode0, SimplifiedCode)
	;
	    SimplifiedCode = [Instr|SimplifiedCode0],
	    simplify_code(More, SimplifiedCode0)
	).


% simplify(+Instr, +Follow, -New)

simplify(nop,		More, New) ?- !, New = More.

simplify(move(X,X),	More, New) ?- !, New = More.

simplify(initialize(y([])),	More, New) ?- !, New = More.

simplify(callf(P,eam(0)),	[Next|More], New) ?- !,
	Next = code{instr:Instr},
	simplify_call(P, Instr, NewInstr),
	update_struct(code, [instr:NewInstr], Next, NewCode),
	New = [NewCode|More].
simplify(call(P,eam(0)),	[Next|More], New) ?- !,
	Next = code{instr:Instr},
	simplify_call(P, Instr, NewInstr),
	update_struct(code, [instr:NewInstr], Next, NewCode),
	New = [NewCode|More].

	% the code generator compiles attribute unification as it it were
	% unifying a meta/N structure. Since attribute_name->slot mapping
	% can change between sessions, we transform sequences like
	%	read_attriubute suspend		(where suspend->1)
	%	read_void*			(N times)
	%	read_xxx			(match actual attribute)
	%	read_void*			(M times)
	% into
	%	read_attriubute name		(where name->N)
	%	read_xxx			(match actual attribute)
	% to make the code session-independent. Note that this cannot cope
	% with multiple attributes being matched at once. This restriction
	% also exists in the old compiler; lifting it requires a different
	% compilation scheme with probably new instructions.
simplify(read_attribute(FirstName),	Code0, New) ?-
	meta_index(FirstName, I0),
	skip_read_void(Code0, I0, I, Code1),
	I > I0,
	Code1 = [Read|Code2],
	is_read_instruction(Read),
	!,
	% we have read_voids followed by another read: simplify
	(
	    meta_index(Name, I),
	    skip_read_void(Code2, 1, _, Code3),
	    Code3 = [After|_],
	    \+ is_read_instruction(After)
	->
	    New = [code{instr:read_attribute(Name)},Read|Code3]
	;
	    warning("Implementation limit: cannot make attribute matching code"),
	    warning("session-independent if matching more than one attribute."),
	    fail
	).

    is_read_instruction(code{instr:Instr}) :-
	functor(Instr, Name, _),
    	atom_string(Name, NameS),
	substring(NameS, "read_", 1).

    skip_read_void(Codes, N0, N, Rest) :-
    	( Codes = [code{instr:read_void}|Codes1] ->
	    N1 is N0+1,
	    skip_read_void(Codes1, N1, N, Rest) 
	;
	    Rest = Codes, N = N0
	).

    :- mode simplify_call(+,+,-).
    simplify_call(P, ret, jmp(P)).
    simplify_call(P, exit, chain(P)).


end_of_file.




Requirements
------------

Process and simplify a WAM code list.  The main problems are:

    - how to substitute patterns that are not consecutive,
	i.e. contain jumps

    - how to make sure that all new substitutions opportunities arising
    	from performed substitutions are found

    - how to detect unreachable labels

It might be useful to transform the code sequence into a graph and work on
that. Read up on some implementation techniques.



Sample substitution patterns:
-----------------------------

Pattern 1:	(eliminate instr)

	nop

    -> replace with nothing


Pattern 1a:

    	move X X

    -> replace with nothing

Pattern 1b:

    	branch lab
    otherlab:
    lab:
	...

    ->
    otherlab:
    lab:
	...


Pattern 2:	(merge instr sequence)

	move(B,A)
	move(C,B)
    ->
    	shift(A,B,C)

	move(Yi,Aj)
	move(Yk,Al)
    ->
    	move2(Yi,Aj,Yk,Al)

Pattern 2a:

	call	P N
	ret
    ->
    	jmp 	P


Pattern 3:	(merge broken instr sequence)

	call	P N
	branch	l1
	...
    l1:
	ret
    ->
	jmp	P
	...
    l1:			(might now be unreachable)
	ret


Pattern 4:	(eliminate unreachable code)

	...a...
	branch/jmp
    l1:			(not a jump target)
	...b...
    l2:
	...c...
    ->
	...a...
	branch/jmp
    l2:
	...c...


Pattern 5:	(skip subsumed instruction)

	Atom_switch A1 [a->alab, b->blab]
	...
    alab:
	Get_atom A1 a
	...
    blab:
	Get_atom A1 b
	...

    -> change the Atom_switch to jump beyond the Get_atom instruction directly.


Pattern 5a:	(skip subsumed instruction)

	List_switch A1 llab ...
	...
    llab:
	Get_list A1 rlab
	...
    rlab:
	Read_xxx

    -> Here the List_switch should be changed to jump directly to rlab.

Pattern 5a:	(redirect to shared code)

	List_switch A1 llab ...
	...
    llab:
	Failure

    -> Here the List_switch should be changed to jump directly to the
    	global fail label.





Remove Res instruction when an event-triggering instruction follows
before failure can occur (but probably better done earlier):

    Res,...,Call
    Res,...,Metacall
    Res,...,Jmp
    Res,...,Chain
    Res,...,Ret
    Res,...,Exit


Various Patterns:

    cut(y(1),N), exit		-->	exitc

    savecut(a(A)),cut(a(A))	-->	savecut(a(A))

    read_void,read_void+	-->	read_void N
    read_void/[^read_xxx]	-->	

    push_structure(N+1),write_did(F/N)  --> write_structure(F/N)

