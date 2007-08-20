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
% Contributor(s): Joachim Schimpf
%                 Kish Shen
% 
% END LICENSE BLOCK
% ----------------------------------------------------------------------
% System:	ECLiPSe Constraint Logic Programming System
% Component:	ECLiPSe III compiler
% Version:	$Id: compiler_peephole.ecl,v 1.5 2007/08/20 01:24:12 kish_shen Exp $
% ----------------------------------------------------------------------

:- module(compiler_peephole).

:- comment(summary, "ECLiPSe III compiler - peephole optimizer").
:- comment(copyright, "Cisco Technology Inc").
:- comment(author, "Joachim Schimpf").
:- comment(date, "$Date: 2007/08/20 01:24:12 $").

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
	    flat_code_to_basic_blocks(CodeList, BasicBlockArray, Rejoins),
            interchunk_simplify(BasicBlockArray, Rejoins, ReachedArray, Branches),
            make_nonreplicate_array(BasicBlockArray, Rejoins, NonRepArray),
            ( for(_,1,max_joined_len), 
              param(BasicBlockArray, NonRepArray, ReachedArray) 
            do
		join_short_continuations(BasicBlockArray, ReachedArray, NonRepArray)
	    ),
	    ( foreacharg(chunk{code:Chunk,cont:Cont,len:Len,done:Done},
                         BasicBlockArray,I), 
              param(BasicBlockArray) do
                ( var(Done) ->
                    simplify_chunk(Chunk, SimplChunk),
                    % Len is approximate after simplify! 
                    setarg(I, BasicBlockArray, chunk{code:SimplChunk,len:Len,cont:Cont})
                ;
                    true
                )
            ),
	    basic_blocks_to_flat_instr(BasicBlockArray, Branches, WamList)
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
% Also determine if two consecutive chunks are `contiguous' chunks, i.e.
% the instructions at the splitting of the chunks should be contiguous in
% the final code if possible. These chunks will be rejoined as soon as 
% possible, unless the earlier chunk is unreachable. The first chunk numbers 
% for each of these contigueous chunks are collected in Rejoins
%
% 
% We already do some opportunistic simplification here:
% - removing the code{} wrapper
% - eliminating nops
% - eliminating redundant consecutive labels (unifying the Labels)
% - eliminating unreachable code between branch() and the next label()
% - make indirect branch() (branch() to another branch()) direct
%
% During code traversal, we maintain a State variable with the values:
%  labels:	the previous instruction was a label (not in rejoin state)
%  normal:	we are in the middle of a chunk
%  unreachable:	the current code is unreachable
%  rejoin:      the previous instruction was a `contiguous' instruction, i.e.
%               it should be contiguous with the following instruction
%  rejoinlabels: the previous instruction was a label, encountered while
%               state was rejoin

flat_code_to_basic_blocks(AnnCode, BasicBlockArray, Rejoins) :-
	(
	    fromto(AnnCode, [code{instr:Instr}|Code1], Code1, []),
	    fromto(FirstChunk,Chunk0,Chunk1,LastChunk),
	    fromto(FirstChunk,Tail0,Tail1,[]),
            fromto(1,Label0,Label1,_),	% last label (while in
                                        % labels/rejoinlabels state)
	    fromto(Chunks,Chunks0,Chunks1,Chunks2),
	    fromto(0,N0,N1,N),			% chunk number
	    fromto(0,Len0,Len1,Len),		% chunk length
            fromto([],Rejoins0,Rejoins1,Rejoins), % rejoin chunk numbers
            fromto(labels,State,State1,EndState)
	do
            ( Instr = label(L) ->
		verify var(L),
                ( State == rejoin ->
                    State1 = rejoinlabels 
                ; State == rejoinlabels ->
                    State1 = rejoinlabels
                ; 
                    State1 = labels
                ),
                Label1 = L,
                Rejoins0 = Rejoins1,
                N1 = N0,
                ( (State == labels ; State == rejoinlabels) ->
                    Label1 = Label0,		% a redundant label
                    Len1 = Len0,
                    Chunk1 = Chunk0,
                    Tail0 = Tail1,
                    Chunks0 = Chunks1
                ; State == unreachable ->
                    Len1 = 0,
                    Chunk1 = Tail1,	% start a new chunk
                    Tail0 = [],		% terminate the previous chunk
                    Chunks0 = Chunks1	% dont't collect finished chunk
                ; % State == normal ; State == rejoin
                    Len1 = 0,
                    Chunk1 = Tail1,	% start a new chunk
                    Tail0 = [],		% terminate the previous chunk
                    % collect finished chunk (L is uninstantiated)
                    Chunks0 = [chunk{code:Chunk0,len:Len0,cont:L}|Chunks1]
                )

	    ; Instr = branch(ref(L)) ->
                N1 = N0,
                Label1 = none,
                Rejoins1 = Rejoins0,
                State1 = unreachable,
                ( (State == labels ; State == rejoinlabels) ->
                    % branch follows immediately from a label
                    Label0 = L,		% get rid of indirect label
                    Len0 = Len1,
		    Chunk0 = Chunk1,
		    Chunks0 = Chunks1,
		    Tail0 = Tail1
                ; State == unreachable ->
		    succ(Len0, Len1),
		    Chunk0 = Chunk1,
		    Chunks0 = Chunks1,
		    Tail0 = Tail1
		; atom(L)  ->
		    Len1 = 0,
		    succ(Len0, Len2),
		    Chunk1 = Tail1,		% start a new chunk
		    Tail0 = [Instr],		% terminate the previous chunk
		    Chunks0 = [chunk{code:Chunk0,len:Len2,cont:0}|Chunks1] % collect finished chunk
		;
                    Len1 = 0,
		    Chunk1 = Tail1,		% start a new chunk
		    Tail0 = [],			% terminate the previous chunk
		    Chunks0 = [chunk{code:Chunk0,len:Len0,cont:L}|Chunks1]	% collect finished chunk
		)

	    ; Instr = nop ->
                Rejoins0 = Rejoins1,
                Label0 = Label1,
		N1 = N0,
		Len1 = Len0,
	    	Chunk1 = Chunk0,
		Chunks1 = Chunks0,
	    	Tail1 = Tail0,
                State = State1   % keep same state

            ; 
                Label1 = none,
                ( (State == labels ; State == rejoinlabels) ->
                    % init. current chunk -- we are in code following a label
                    % that we want to keep
                    Label0 = N1,	% instantiate the previous label
                    succ(N0, N1),	% current chunk number
                    (State == rejoinlabels ->
                        Rejoins1 = [N0|Rejoins0] % current is a rejoin chunk
                    ;
                        Rejoins1 = Rejoins0
                    )
                ;
                    N0 = N1,
                    Rejoins1 = Rejoins0
                ),
                ( unconditional_transfer(Instr) ->
                    State1 = unreachable,
                    ( State == unreachable ->
                        succ(Len0, Len1),
                        Chunk1 = Chunk0,
                        Chunks1 = Chunks0,
                        Tail1 = Tail0
                    ;
                        Len1 = 0,
                        succ(Len0, Len2),
                        Chunk1 = Tail1,		% start a new chunk
                        Tail0 = [Instr],	% terminat current chunk
			% collect finished chunk
                        Chunks0 = [chunk{code:Chunk0,len:Len2,cont:0}|Chunks1]
                    )
                
                ; 
                    succ(Len0, Len1),
                    Chunk1 = Chunk0,
                    Chunks0 = Chunks1,
                    Tail0 = [Instr|Tail1],	% append this instruction
                    next_state(Instr, State, State1)
                )
            )
        ),
	( EndState = unreachable ->
	    Chunks2 = []
	;
	    Chunks2 = [chunk{code:LastChunk,len:Len,cont:0}]
	),
	verify length(Chunks, N),
	BasicBlockArray =.. [[]|Chunks].

% determine the next state while in the middle of traversing code
next_state(Instr, State, NextState) :-
        ( State == unreachable -> 
            NextState = unreachable 
        ; 
            ( indexing_branch(Instr) -> 
                NextState = rejoin % following code should be contiguous
            ; 
                NextState = normal
            )
        ).

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

    % these are indexing branch instructions with a default fall-through
    % case. It is desirable that the fall-through code is contiguous with
    % the instruction rather than a branch to somewhere else. However, if 
    % code following is split into a new chunk, the two chunks should be 
    % rejoined as soon as possible to ensure the final code is contiguous.
    indexing_branch(try_me_else(_,_,_)).
    indexing_branch(try(_,_,_)).
    indexing_branch(retry_me_else(_,_)).
    indexing_branch(retry(_,_)).
    indexing_branch(trust_me(_)).
    indexing_branch(retry_me_inline(_,_,_)).
    indexing_branch(trust_me_inline(_,_)).
    indexing_branch(retry_inline(_,_)).


% interchunk_simplify is intended to do peephole optimisations across
% different chunks, connected by refs.
% mark all reachable chunks by following the continuations and refs.
% Rejoin any contiguous chunks, unless its first chunk is unreachable

interchunk_simplify(BasicBlockArray, Rejoins, ReachedArray, Targets) :-
        find_reached_chunks(BasicBlockArray, ReachedArray, Targets),
        rejoin_contiguous_chunks(BasicBlockArray, ReachedArray, Rejoins).

find_reached_chunks(BasicBlockArray, ReachedArray, Targets) :-
        functor(BasicBlockArray, F, N),
        functor(ReachedArray, F, N),
        functor(TargetArray, F, N), 
        arg(1, ReachedArray, []), % first chunk
        arg(1, BasicBlockArray, Chunk),
        find_reached_chunks_(Chunk, BasicBlockArray, ReachedArray, 
                             Targets, Targets, TargetArray).

find_reached_chunks_(Chunk, BasicBlockArray, ReachedArray, Targets,
                     TargetsT0, TargetArray) :-
        Chunk = chunk{cont:Cont,code:Code},
        ( integer(Cont), Cont > 0 -> 
            arg(Cont, ReachedArray, []),
            arg(Cont, BasicBlockArray, ContChunk)
        ; 
            true
        ),
        find_targets(Code, TargetArray, TargetsT0, TargetsT1),
        ( nonvar(ContChunk) ->
            find_reached_chunks_(ContChunk, BasicBlockArray, ReachedArray, 
                                 Targets, TargetsT1, TargetArray)
        ;
            find_chunks_in_branch(Targets, BasicBlockArray, ReachedArray,
                                  TargetsT1, TargetArray)
        ).

find_chunks_in_branch(Targets, BasicBlockArray, ReachedArray, 
                      TargetsT, TargetArray) :-
        ( var(Targets) ->
            true % queue empty, done
        ;
            Targets = [Target|Targets0],
            arg(Target, ReachedArray, Ref),
            ( var(Ref) ->  % not yet processed
                Ref = [], % process it now
                arg(Target, BasicBlockArray, Chunk),
                find_reached_chunks_(Chunk, BasicBlockArray, ReachedArray,
                                     Targets0, TargetsT, TargetArray)
            ;
                find_chunks_in_branch(Targets0, BasicBlockArray,
                                      ReachedArray, TargetsT, TargetArray)
            )
        ).


% rejoin adjacent chunks that should be contiguous if the first chunk
% is reached. Rejoins must have later chunks first in the list because more 
% after rejoining two chunks, the rejoined chunk can be rejoined with the
% previous chunk
rejoin_contiguous_chunks(BasicBlockArray, ReachedArray, Rejoins) :-
        (foreach(R, Rejoins), param(BasicBlockArray, ReachedArray) do
            arg(R, BasicBlockArray, chunk{len:Len,code:Code}),
            arg(R, ReachedArray, Reached),
            ( nonvar(Reached) -> % first chunk of rejoin chunks reached? 
                succ(R, NextC),  % yes, rejoin with succeeding chunk
                % succeeding chunk mark as processed
                arg(NextC, BasicBlockArray, chunk{len:NextLen,code:NextCode,
                                                  cont:NextCont,done:done}),
                append(Code, [label(NextC)|NextCode],NewCode),
                NewLen is Len + NextLen,
                setarg(R, BasicBlockArray, chunk{len:NewLen,code:NewCode,
                                                 cont:NextCont})
            ;
                % first chunk not reached, so don't join
                true
            )
        ).

% NonRepArray indicates which chunks should not be replicated -- currently
% chunks that contains labels (i.e. rejoined chunks)
make_nonreplicate_array(BasicBlockArray, Rejoins, NonRepArray) :-
        functor(BasicBlockArray, F, A),
        functor(NonRepArray, F, A),
        ( foreach(R, Rejoins), param(NonRepArray) do
            R1 is R + 1,
            arg(R, NonRepArray, []), 
            arg(R1, NonRepArray, [])
        ).

% Joins a chunk to its continuation if the continuation is short, and can
% be replicated -- i.e. there are no labels inside the continuation chunk.
% An optimisation is that if the continuation immediately jumps elsewhere,
% the continuation of the chunk is simply updated.
join_short_continuations(BasicBlockArray, ReachedArray, NonRepArray) :-
	(
	    foreacharg(Chunk,BasicBlockArray,I),
	    param(BasicBlockArray,NonRepArray,ReachedArray)
	do
            Chunk = chunk{cont:Cont,len:Len,code:Code,done:Done},
            ( arg(I, ReachedArray, ReachedI), var(ReachedI) ->
                % chunk not reached, don't join
                true
            ; nonvar(Done) ->
                % nonvar if chunk discarded, don't join
                true
            ; Cont == 0 ->
		true
	    ; (arg(Cont, NonRepArray, NonRepC), nonvar(NonRepC) ) ->
                true  % next chunk should not be replicated -- don't join
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

basic_blocks_to_flat_instr(BasicBlockArray, Reached, Instrs) :-
	(
	    fromto(1,I,NextI,0),			% current chunk
	    fromto(1,PrevCont,Cont,_),			% prev. chunk's continuation
	    fromto(Reached,Reached1,Reached2,_),	% ref-targets (queue)
	    fromto(Instrs,Instrs0,Instrs2,[]),		% result list
	    param(BasicBlockArray)
	do
	    arg(I, BasicBlockArray, Chunk),
	    Chunk = chunk{code:Code,done:Done,cont:Cont0},
	    ( var(Done) ->
		% process chunk: append code to final code list
		Done = done,
		Cont = Cont0,
		Instrs0 = [label(I)|Instrs1],
		append(Code, Instrs2, Instrs1)
	    ; PrevCont == I ->
		% previous chunk continues into this one, but it has already
		% been emitted, so we need a branch 
                % can't copy because 
                %  1) chunk may have labels
                %  2) no length info for chunk because of simplification
                Instrs0 = [branch(ref(I))|Instrs2],
		Cont = 0
	    ;
		Cont = 0,
		Instrs0 = Instrs2
	    ),
	    % Choose the next chunk to process: prefer the current chunk's
	    % continuation, otherwise pick one from the queue
	    ( Cont > 0 ->
		NextI = Cont, Reached1 = Reached2	% use continuation
	    ; var(Reached1) ->
	    	NextI = 0				% queue empty, finished
	    ;
		Reached1 = [NextI|Reached2]		% pick from queue
	    )
	).

        
    % Find all ref()s that refer to unprocessed chunks and queue the labels
    find_targets(X, _, TargetsT, TargetsT) :- var(X), !.
    find_targets([X|Xs], TargetArray, TargetsT0, TargetsT2) ?- !,
	find_targets(X, TargetArray, TargetsT0, TargetsT1),
	find_targets(Xs, TargetArray, TargetsT1, TargetsT2).
    find_targets(ref(L), TargetArray, TargetsT0, TargetsT1) :- !, 
        (
            integer(L),
            arg(L, TargetArray, Ref),
	    var(Ref)
	->
	    TargetsT0 = [L|TargetsT1],
            Ref = []
	;
	    TargetsT0 = TargetsT1
	).
    find_targets(Xs, TargetArray, TargetsT0, TargetsT3) :-
    	compound(Xs), !,
	(
	    foreacharg(X,Xs),
	    fromto(TargetsT0,TargetsT1,TargetsT2,TargetsT3),
	    param(TargetArray)
	do
	    find_targets(X, TargetArray, TargetsT1, TargetsT2)
	).
    find_targets(_, _TargetArray, TargetsT, TargetsT).
    	

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

simplify(cut(y(1),_N), [exit|More], New) ?- !,
        New = [exitc|More].

simplify(cut(y(1), N), More, New) ?- !,
        New = [cut1(N)|More].

simplify(savecut(a(A)), [cut(a(A))|More], New) ?- !,
        New = [savecut(a(A))|More].
/*
simplify(push_structure(B), [write_did(F/A)|More], New) ?- !,
        B is A + 1,
        New = [write_structure(F/A)|More].
*/
simplify(allocate(N), [move(a(I),y(J))|More], New) ?- !,
        New = [get_variable(N, a(I), y(J))|More].

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

simplify(read_void, [Instr0|Rest0], New) ?- !,
        (Instr0 == read_void ->
            skip_read_void(Rest0, 2, N, Rest),
            (Rest = [Instr|_], is_in_read_struct(Instr) ->
                New = [read_void(N)|Rest]
            ;
                New = Rest % skip trailing read_voids
            )
        ;
            % do not simplify single read_void except a trailing one
            \+ is_in_read_struct(Instr0), 
            New = [Instr0|Rest0] 
        ).

is_read_instruction(Instr) :-
	functor(Instr, Name, _),
    	atom_string(Name, NameS),
	substring(NameS, "read_", 1).

    % is_in_read_struct(+Instr) checks if Instr, which follows a read_void, 
    % is still (possibly) part of instructions reading the structure, i.e.
    % if it is a read_*, or a move (which should be followed by a
    % read_variable, which we don't check for) 
    is_in_read_struct(Instr) :-
        is_read_instruction(Instr), !.
    is_in_read_struct(move(_,_)).
        
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
    read_void/[^(read_xxx|move)]	-->	

    push_structure(N+1),write_did(F/N)  --> write_structure(F/N)

    allocate n, move Ai,Yj      -->     get_variable(n,Ai,Yj)
                                        