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
% Version:	$Id: compiler_peephole.ecl,v 1.4 2007/06/10 22:10:30 jschimpf Exp $
% ----------------------------------------------------------------------

:- module(compiler_peephole).

:- comment(summary, "ECLiPSe III compiler - peephole optimizer").
:- comment(copyright, "Cisco Technology Inc").
:- comment(author, "Joachim Schimpf").
:- comment(date, "$Date: 2007/06/10 22:10:30 $").

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


:- local struct(chunk(
    	cont,	% index of continuation chunk
	len,	% length of code list
	code,	% code list
	done)).	% 'done' if chunk already in final code list, else uninstantiated


% maximum size of code chunks that should be duplicated to save a branch
max_joined_len(2).


:- comment(simplify_code/3, [
    summary:"Strip annotations and do peephole optimizations",
    amode:simplify_code(+,-,+),
    args:[
	"AnnotatedCodeIn":"A list of annotated WAM code (struct(code))",
	"WamCodeOut":"A list of WAM instructions in lib(asm) format",
	"Options":"struct(options)"
    ],
    see_also:[struct(code)]
]).

:- export simplify_code/3.
simplify_code(CodeList, WamList, options{opt_level:OptLevel}) :-
	( OptLevel > 0 ->
	    flat_code_to_basic_blocks(CodeList, BasicBlockArray),
	    ( for(_,1,max_joined_len), param(BasicBlockArray) do
		join_short_continuations(BasicBlockArray)
	    ),
	    ( foreacharg(chunk{code:Chunk,cont:Cont},BasicBlockArray,I), param(BasicBlockArray) do
		simplify_chunk(Chunk, SimplChunk),
		setarg(I, BasicBlockArray, chunk{code:SimplChunk,len:(-1),cont:Cont})
	    ),
	    basic_blocks_to_flat_instr(BasicBlockArray, WamList)
	;
	    ( foreach(code{instr:Instr},CodeList), foreach(Instr,Instrs) do
		true
	    ),
	    simplify_chunk(Instrs, WamList)
	).



% Take a simple list of annotated code, and cut it up at the labels.
% The result are code chunks that correspond to basic blocks.
% Number each chunk and instantiate the corresponding Label to this number.
% Enter the chunk into an array indexed by the chunk number.
%
% We already do some opportunistic simplification here:
% - removing the code{} wrapper
% - eliminating nops
% - eliminating redundant consecutive labels (unifying the Labels)
% - eliminating unreachable code between branch() and the next label()
%
% During code traversal, we maintain a State variable with the values:
%  labels:	the previous instruction was a label
%  normal:	we are in the middle of a chunk
%  unreachable:	the current code is unreachable


:- export flat_code_to_basic_blocks/2.
flat_code_to_basic_blocks(AnnCode, BasicBlockArray) :-
	(
	    fromto(AnnCode, [code{instr:Instr}|Code1], Code1, []),
	    fromto(FirstChunk,Chunk0,Chunk1,LastChunk),
	    fromto(FirstChunk,Tail0,Tail1,[]),
	    fromto(Chunks,Chunks0,Chunks1,Chunks2),
	    fromto(1,N0,N1,N),			% chunk number
	    fromto(0,Len0,Len1,Len),		% chunk length
	    fromto(labels,State,State1,EndState)
	do
	    ( Instr = label(L) ->
		verify var(L),
		L = N1,				% instantiate the label
		State1 = labels,
		( State == labels ->
		    N1 = N0,			% a redundant label
		    Len1 = Len0,
		    Chunk1 = Chunk0,
		    Tail0 = Tail1,
		    Chunks0 = Chunks1
		; State == normal ->
		    Len1 = 0,
		    succ(N0, N1),		% new chunk number
		    Chunk1 = Tail1,		% start a new chunk
		    Tail0 = [],			% terminate the previous chunk
		    Chunks0 = [chunk{code:Chunk0,len:Len0,cont:L}|Chunks1]	% collect finished chunk
		; % State == unreachable ->
		    Len1 = 0,
		    succ(N0, N1),		% new chunk number
		    Chunk1 = Tail1,		% start a new chunk
		    Tail0 = [],			% terminate the previous chunk
		    Chunks0 = Chunks1		% dont't collect finished chunk
		)

	    ; Instr = branch(ref(L)) ->
		N1 = N0,
		State1 = unreachable,
		( State == unreachable ->
		    succ(Len0, Len1),
		    Chunk1 = Chunk0,
		    Chunks0 = Chunks1,
		    Tail0 = Tail1
		; atom(L) ->
		    Len1 = 0,
		    succ(Len0, Len2),
		    Chunk1 = Tail1,		% start a new chunk
		    Tail0 = [Instr],		% terminate the previous chunk
		    Chunks0 = [chunk{code:Chunk0,len:Len2,cont:0}|Chunks1]	% collect finished chunk
		;
		    Len1 = 0,
		    Chunk1 = Tail1,		% start a new chunk
		    Tail0 = [],			% terminate the previous chunk
		    Chunks0 = [chunk{code:Chunk0,len:Len0,cont:L}|Chunks1]	% collect finished chunk
		)

	    ; Instr = nop ->
		N1 = N0,
		Len1 = Len0,
	    	Chunk1 = Chunk0,
		Chunks0 = Chunks1,
	    	Tail0 = Tail1,
		next_state(State, State1)

	    ; unconditional_transfer(Instr) ->
		N1 = N0,
		State1 = unreachable,
		( State == unreachable ->
		    succ(Len0, Len1),
		    Chunk1 = Chunk0,
		    Chunks0 = Chunks1,
		    Tail0 = Tail1
		;
		    Len1 = 0,
		    succ(Len0, Len2),
		    Chunk1 = Tail1,		% start a new chunk
		    Tail0 = [Instr],		% terminate the previous chunk
		    Chunks0 = [chunk{code:Chunk0,len:Len2,cont:0}|Chunks1]	% collect finished chunk
		)

	    ;
		N1 = N0,			% still the same chunk
		succ(Len0, Len1),
	    	Chunk1 = Chunk0,
		Chunks0 = Chunks1,
	    	Tail0 = [Instr|Tail1],		% append this instruction
		next_state(State, State1)
	    )
	),
	( EndState = unreachable ->
	    Chunks2 = []
	;
	    Chunks2 = [chunk{code:LastChunk,len:Len,cont:0}]
	),
	verify length(Chunks, N),
	BasicBlockArray =.. [[]|Chunks].

    next_state(unreachable, unreachable).
    next_state(labels, normal).
    next_state(normal, normal).

    % Unconditional control transfer instructions
    % Only needs to list instructions generated by the code generator
    unconditional_transfer(branch(_)).
    unconditional_transfer(exit).
    unconditional_transfer(exitd).
    unconditional_transfer(failure).
    unconditional_transfer(ret).
    unconditional_transfer(retd).
    unconditional_transfer(retn).
    unconditional_transfer(jmp(_)).
    unconditional_transfer(jmpd(_)).
    unconditional_transfer(chain(_)).
    unconditional_transfer(chaind(_)).
    unconditional_transfer(trust(_,_)).
    unconditional_transfer(trust_inline(_,_)).


join_short_continuations(BasicBlockArray) :-
	(
	    foreacharg(chunk{cont:Cont,len:Len,code:Code},BasicBlockArray,I),
	    param(BasicBlockArray)
	do
	    ( Cont == 0 ->
		true
	    ;
		arg(Cont, BasicBlockArray, NextChunk),
		NextChunk = chunk{len:ContLen,code:ContCode,cont:ContCont},
		( ContLen > max_joined_len ->
		    true
		;
		    append(Code, ContCode, NewCode),
		    NewLen is Len+ContLen,
		    setarg(I, BasicBlockArray, chunk{code:NewCode,len:NewLen,cont:ContCont})
		)
	    )
	).


% Flatten the BasicBlockArray into a WAM code list.
% We emit only the reachable chunks, by collecting all ref()s in the code.
% The done-flag in the array indicates whether the chunk has already been
% processed.

:- export basic_blocks_to_flat_instr/2.

basic_blocks_to_flat_instr(BasicBlockArray, Instrs) :-
	(
	    fromto(1,I,NextI,0),			% current chunk
	    fromto(1,PrevCont,Cont,_),			% prev. chunk's continuation
	    fromto(TargetsT0,Targets1,Targets2,_),	% ref-targets (queue)
	    fromto(TargetsT0,TargetsT1,TargetsT2,_),	% queue tail
	    fromto(Instrs,Instrs0,Instrs2,[]),		% result list
	    param(BasicBlockArray)
	do
	    arg(I, BasicBlockArray, Chunk),
	    Chunk = chunk{code:Code,done:Done,cont:Cont0},
	    ( var(Done) ->
		% process chunk: extract refs, append code to final code list
		Done = done,
		Cont = Cont0,
		find_targets(Code, BasicBlockArray, TargetsT1, TargetsT2),
		Instrs0 = [label(I)|Instrs1],
		append(Code, Instrs2, Instrs1)
	    ; PrevCont == I ->
		% previous chunk continues into this one, but it has already
		% been emitted, so we need a branch (or a copy if it is short)
		( Cont0 == 0 , length(Code) =< max_joined_len ->
		    append(Code, Instrs2, Instrs0)
		;
		    Instrs0 = [branch(ref(I))|Instrs2]
		),
		Cont = 0,
		TargetsT2 = TargetsT1
	    ;
		Cont = 0,
		TargetsT2 = TargetsT1,
		Instrs0 = Instrs2
	    ),
	    % Choose the next chunk to process: prefer the current chunk's
	    % continuation, otherwise pick one from the queue
	    ( Cont > 0 ->
		NextI = Cont, Targets1 = Targets2	% use continuation
	    ; var(Targets1) ->
	    	NextI = 0				% queue empty, finished
	    ;
		Targets1 = [NextI|Targets2]		% pick from queue
	    )
	).

    % Find all ref()s that refer to unprocessed chunks and queue the labels
    find_targets(X, _BasicBlockArray, TargetsT, TargetsT) :- var(X), !.
    find_targets([X|Xs], BasicBlockArray, TargetsT0, TargetsT2) :- !,
	find_targets(X, BasicBlockArray, TargetsT0, TargetsT1),
	find_targets(Xs, BasicBlockArray, TargetsT1, TargetsT2).
    find_targets(ref(L), BasicBlockArray, TargetsT0, TargetsT1) :- !,
	(
	    integer(L),
	    arg(L, BasicBlockArray, Chunk),
	    Chunk = chunk{done:Done},
	    var(Done)
	->
	    TargetsT0 = [L|TargetsT1]
	;
	    TargetsT0 = TargetsT1
	).
    find_targets(Xs, BasicBlockArray, TargetsT0, TargetsT3) :-
    	compound(Xs), !,
	(
	    foreacharg(X,Xs),
	    fromto(TargetsT0,TargetsT1,TargetsT2,TargetsT3),
	    param(BasicBlockArray)
	do
	    find_targets(X, BasicBlockArray, TargetsT1, TargetsT2)
	).
    find_targets(_, _BasicBlockArray, TargetsT, TargetsT).
    	

%----------------------------------------------------------------------
% simplify a basic block
%----------------------------------------------------------------------

simplify_chunk([], []).
simplify_chunk([Instr|More], SimplifiedCode) :-
	( simplify(Instr, More, SimplifiedCode0) ->
	    simplify_chunk(SimplifiedCode0, SimplifiedCode)
	;
	    SimplifiedCode = [Instr|SimplifiedCode0],
	    simplify_chunk(More, SimplifiedCode0)
	).


% simplify(+Instr, +Follow, -New)

simplify(nop,		More, New) ?- !, New = More.

simplify(move(X,X),	More, New) ?- !, New = More.

simplify(initialize(y([])),	More, New) ?- !, New = More.

simplify(callf(P,eam(0)),	[Instr|More], New) ?- !,
	simplify_call(P, Instr, NewInstr),
	New = [NewInstr|More].

simplify(call(P,eam(0)),	[Instr|More], New) ?- !,
	simplify_call(P, Instr, NewInstr),
	New = [NewInstr|More].

	% the code generator compiles attribute unification as if it were
	% unifying a meta/N structure. Since attribute_name->slot mapping
	% can change between sessions, we transform sequences like
	%	read_attribute suspend		(where suspend->1)
	%	read_void*			(N times)
	%	read_xxx			(match actual attribute)
	%	read_void*			(M times)
	% into
	%	read_attribute name		(where name->N)
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
	    New = [read_attribute(Name),Read|Code3]
	;
	    warning("Implementation limit: cannot make attribute matching code"),
	    warning("session-independent if matching more than one attribute."),
	    fail
	).

    is_read_instruction(Instr) :-
	functor(Instr, Name, _),
    	atom_string(Name, NameS),
	substring(NameS, "read_", 1).

    skip_read_void(Codes, N0, N, Rest) :-
    	( Codes = [read_void|Codes1] ->
	    N1 is N0+1,
	    skip_read_void(Codes1, N1, N, Rest) 
	;
	    Rest = Codes, N = N0
	).

    :- mode simplify_call(+,+,-).
    simplify_call(P, ret, jmp(P)).
    simplify_call(P, exit, chain(P)).


%----------------------------------------------------------------------
end_of_file.
%----------------------------------------------------------------------




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

