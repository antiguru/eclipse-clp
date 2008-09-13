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
% Version:	$Id: compiler_codegen.ecl,v 1.24 2008/09/13 10:46:41 jschimpf Exp $
% ----------------------------------------------------------------------

:- module(compiler_codegen).

:- comment(summary, "ECLiPSe III compiler - code generation").
:- comment(copyright, "Cisco Technology Inc").
:- comment(author, "Joachim Schimpf").
:- comment(date, "$Date: 2008/09/13 10:46:41 $").


:- lib(hash).

:- use_module(compiler_common).

:- include(compiler_compound).


%----------------------------------------------------------------------
% Chunk data
% This data structure holds information that evolves along the chunk.
%----------------------------------------------------------------------

:- local struct(chunk_data(
	occurred,		% hash table varid->bool (vars already seen in chunk)
	aux_count,		% number of auxiliary temporaries
	need_global,		% space needed on global stack at this point
	allocated,		% environment size at this point (-1 no env)
	eam			% environment activity map at chunk entry
    )).


init_chunk_data(EAM, ESize, chunk_data{aux_count:0,occurred:Init,eam:EAM,allocated:ESize}) :-
	hash_create(Init).

start_new_chunk(EAM, ChunkData0, ChunkData) :-
	update_struct(chunk_data, [aux_count:0,occurred:Init,eam:EAM], ChunkData0, ChunkData),
	hash_create(Init).

print_chunk_data(_,_).


%----------------------------------------------------------------------
% Register a variable occurrence within a chunk and
% returns a "variable occurrence descriptor" of the form:
%
%	void				void variable
%	tmp_first			first occurrence of a temporary in its chunk
%	tmp				repeat occurrence of a temporary in its chunk
%	perm_first(y(Y))		first occurrence of a perm in its 1st chunk
%	perm_first_in_chunk(y(Y))	first occurrence of perm in a later chunk
%	perm(y(Y))			repeat occurrence of perm
%
% Special case: head perms that are still waiting to be moved into the environment
% at the end of the initial chunk (delayed_perm) are classified as tmp.

variable_occurrence(variable{varid:VarId,class:Class}, ChunkData0, ChunkData, Code0, Code, Descriptor) :-
	ChunkData0 = chunk_data{occurred:OccurredInChunk,eam:EAM},
	variable_occurrence1(Class, EAM, VarId, OccurredInChunk, Descriptor),
	( Descriptor = perm_first(y(Y)) ->
	    env_allocate_if_needed(Y, ChunkData0, ChunkData, Code0, Code)
	;
	    ChunkData0 = ChunkData, Code0 = Code
	).

    variable_occurrence1(void, _EAM, _VarId, _OccurredInChunk, Descriptor) ?-
    	Descriptor = void.
    variable_occurrence1(nonvoid(y(Y)), EAM, VarId, OccurredInChunk, Descriptor) ?- !,
	( hash_get(OccurredInChunk, VarId, Type) ->
	    ( Type == delayed_perm ->
		Descriptor = tmp
	    ;
		Descriptor = perm(y(Y))
	    )
	;
	    hash_set(OccurredInChunk, VarId, true),
	    ( 0 is getbit(EAM, Y-1) ->
		Descriptor = perm_first(y(Y))
	    ;
		Descriptor = perm_first_in_chunk(y(Y))
	    )
	).
    variable_occurrence1(nonvoid(_Tmp), _EAM, VarId, OccurredInChunk, Descriptor) ?-
	( hash_contains(OccurredInChunk, VarId) ->
	    Descriptor = tmp
	;
	    hash_set(OccurredInChunk, VarId, true),
	    Descriptor = tmp_first
	).


potential_first_temp_occurrence(variable{varid:VarId,class:nonvoid(temp)}, ChunkData) :-
	ChunkData = chunk_data{occurred:OccurredInChunk},
	\+ hash_contains(OccurredInChunk, VarId).
	

new_aux_temp(ChunkData0, ChunkData, aux(AuxCount)) :-
	AuxCount is ChunkData0[aux_count of chunk_data] + 1,
	update_struct(chunk_data, [aux_count:AuxCount], ChunkData0, ChunkData).


%----------------------------------------------------------------------
% Code generation 
%----------------------------------------------------------------------

:- comment(generate_code/5, [
    summary:"Generate WAM code from normalised source for one predicate",
    amode:generate_code(+,-,?,+,+),
    args:[
	"Body":"Normalised and fully annotated source of the predicate",
	"Code":"Resulting annotated code",
	"CodeEnd":"Tail of resulting annotated code",
	"Options":"Options structure",
	"ModulePred":"Context module and Name/Arity"
    ],
    see_also:[assign_am_registers/3,struct(code)]
]).

:- export generate_code/5.

generate_code(Clause, Code, AuxCode, Options, ModPred) :-
	% Execute under priority 3 or less to make sure delayed goals exeute
	sepia_kernel:call_relaxed_prio(
		generate_code1(Clause, Code, AuxCode, Options, ModPred), 3).

generate_code1(Clause, Code, AuxCode, Options, ModPred) :-
	init_chunk_data(0, -1, ChunkData0),
	Code = [code{instr:label(Start)}|Code1],
	alloc_check_start(ChunkData0, ChunkData1, Code1, Code2),
	generate_branch(Clause, [], ChunkData1, _ChunkData, 0, -1, AuxCode, [], Code2, [code{instr:ret}|next([])], Options, ModPred@Start).


generate_branch(AllChunks, HeadPerms, ChunkData0, ChunkData, BranchExitInitMap, ExitEnvSize, AuxCode0, AuxCode, Code0, Code, Options, SelfInfo) :-
	% first chunk in branch
	generate_chunk(AllChunks, OtherChunks, HeadPerms, ChunkData0, ChunkData2, AuxCode0, AuxCode1, Code0, Code1, Options, SelfInfo),
	(
	    fromto(OtherChunks,ThisChunk,NextChunk,[]),
	    fromto(ChunkData2,ChunkData3,ChunkData6,ChunkData7),
	    fromto(Code1,next(Code2),Code4,Code5),
	    fromto(AuxCode1,AuxCode2,AuxCode3,AuxCode),
	    param(Options,SelfInfo)
	do
	    alloc_check_start(ChunkData3, ChunkData5, Code2, Code3),
	    generate_chunk(ThisChunk, NextChunk, [], ChunkData5, ChunkData6, AuxCode2, AuxCode3, Code3, next(Code4), Options, SelfInfo)
	),
	% Make sure all branches have ExitEnvSize allocated (or all deallocated)
	env_allocate_last_chance(ExitEnvSize, ChunkData7, ChunkData, Code5, Code6),
	% Generate initialization code for any variables which did not occur
	% in or before the branch, but have a non-first occurrence after it.
	emit_initialize(BranchExitInitMap, Code6, next(Code)).


:- mode generate_chunk(+,-,+,+,-,?,-,-,?,+,+).
generate_chunk([], [], HeadPerms, ChunkData0, ChunkData, AuxCode, AuxCode, Code, Code1, _Options, _Module) :-
	% end of chunk (non-regular end of branch or clause)
	move_head_perms(HeadPerms, ChunkData0, ChunkData1, Code, Code1),
	alloc_check_end(ChunkData1),
	start_new_chunk(0, ChunkData1, ChunkData).

generate_chunk([Goal|Goals], NextChunk, HeadPerms0, ChunkData0, ChunkData, AuxCode, AuxCode0, Code, Code0, Options, SelfInfo) :-
	( Goal = goal{kind:simple} ->		 % special goals
	    SelfInfo = Module:_@_,
	    generate_simple_goal(Goal, ChunkData0, ChunkData1, Code, Code1, Options, Module),
	    generate_chunk(Goals, NextChunk, HeadPerms0, ChunkData1, ChunkData, AuxCode, AuxCode0, Code1, Code0, Options, SelfInfo)

	; Goal = goal{kind:head,args:Args} ->	% clause-head or pseudo-head

	    verify HeadPerms0 == [],
	    generate_head_info([](Args), 1, ChunkData0, ChunkData3, HeadPerms3, [], OrigRegDescs),
	    Code = [code{instr:nop,regs:OrigRegDescs}|Code1],
	    generate_chunk(Goals, NextChunk, HeadPerms3, ChunkData3, ChunkData, AuxCode, AuxCode0, Code1, Code0, Options, SelfInfo)

	; Goal = goal{kind:regular,functor:true/0,definition_module:sepia_kernel}, (Goals = [] ; Goals = [goal{kind:regular}|_] ) ->
	    % Normally, true/0 should be eliminated in the normalisation phase.
	    % But due to its legacy semantics (it is a regular goal and can
	    % cause waking), we only eliminate it here when it occurs at the
	    % end of a branch or just before another regular goal.
	    generate_chunk(Goals, NextChunk, HeadPerms0, ChunkData0, ChunkData, AuxCode, AuxCode0, Code, Code0, Options, SelfInfo)

	; Goal = goal{kind:regular,functor:P,args:Args,lookup_module:LM,envmap:EAM,envsize:ESize} ->
	    move_head_perms(HeadPerms0, ChunkData0, ChunkData1, Code, Code1),
	    SelfInfo = Module:Self@SelfLab,
	    generate_regular_puts(Args, ChunkData1, ChunkData2, Code1, Code2, OutArgs, Module),
	    ( LM\==Module ->
	    	Pred = LM:P, Dest = Pred	% calling non-visible
	    ; P==Self ->
	    	Pred = P, Dest = ref(SelfLab)	% direct recursive call
	    ;
	    	Pred = P, Dest = Pred		% calling visible pred
	    ),
	    call_instr(ESize, Dest, EAM, ChunkData2, ChunkData3, Code2, Code3, CallInstr),
	    emit_call_regular(CallInstr, OutArgs, Pred, Goal, Code3, Code0, Options),
	    NextChunk = Goals,
	    AuxCode = AuxCode0,
	    % end of chunk
	    alloc_check_end(ChunkData3),
	    start_new_chunk(EAM, ChunkData3, ChunkData)

	; Goal = disjunction{branches:Branches, branchlabels:BranchLabelArray, determinism:Det,
		entrymap:_EAM, entrysize:EntryESize, exitmap: DisjExitEAM, exitsize:ExitESize,
		arity:TryArity, args:Args, branchheadargs:HeadArgsArray,
		branchentrymaps:BranchEamArray, branchinitmaps:BranchExitInits,
		indexes:IndexDescs} ->

	    arity(BranchLabelArray, NBranches),
	    make_retry_me_activity_maps(BranchEamArray, RetryEamArray),
	    NextChunk = Goals,

	    % Pre-disjunction: move pseudo-arguments into place and make switches
	    move_head_perms(HeadPerms0, ChunkData0, ChunkData00, Code, Code101),
	    generate_regular_puts(Args, ChunkData00, ChunkData1, Code101, Code102, ArgDests, []),
	    Code102 = [code{instr:nop,regs:ArgDests}|Code103],
	    generate_indexing(IndexDescs, BranchLabelArray, BranchEamArray, TryArity, ChunkData1, Code103, next(Code104), AuxCode, AuxCode1, Options),
	    alloc_check_split(ChunkData1, [GAlloc1|GAllocs2toN]),
	    env_set_allocate_size(EntryESize, ChunkData1),
	    ChunkData1 = chunk_data{allocated:ActualESize},

	    % TRY (first alternative)
	    Branches = [Branch1|Branches2toN],
	    BranchExitInits = [BranchExitInit1|BranchExitInits2toN],
	    arg(1, BranchLabelArray, BrLabel1),
	    arg(1, BranchEamArray, EAM1),
	    Code104 = [
		code{instr:try_me_else(#no_port,TryArity,ref(Label2)),regs:ArgOrigs},
		code{instr:label(BrLabel1),regs:[]}|Code106],
	    start_new_chunk(EAM1, ChunkData1, ChunkData2),
	    alloc_check_start_branch(det, ChunkData2, ChunkData3, Code106, Code107, GAlloc1),
	    generate_head_info(HeadArgsArray, 1, ChunkData3, ChunkData4, PseudoHeadPerms, [], ArgOrigs),
	    generate_branch(Branch1, PseudoHeadPerms, ChunkData4, ChunkDataE1, BranchExitInit1, ExitESize, AuxCode1, AuxCode2, Code107, Code2, Options, SelfInfo),
	    Code2 = [code{instr:branch(ref(LabelJoin)),regs:[]}|Code3],

	    % RETRY (middle alternatives)
	    (
		for(I, 2, NBranches-1),
		fromto(Branches2toN, [Branch|Branches], Branches, [BranchN]),
		fromto(BranchExitInits2toN, [BranchExitInit|BEIs], BEIs, [BranchExitInitN]),
		fromto(GAllocs2toN, [GAllocI|GAs], GAs, [GAllocN]),
		fromto(ChunkDataE2toN, [ChunkDataE|CDEs], CDEs, [ChunkDataEN]),
		fromto(Code3, Code4, Code7, Code8),
		fromto(AuxCode2, AuxCode3, AuxCode4, AuxCode5),
		fromto(Label2, LabelI, LabelI1, LabelN),
		param(LabelJoin,BranchLabelArray,BranchEamArray,RetryEamArray,Options,SelfInfo,Det,ChunkData1,HeadArgsArray,ActualESize,ExitESize)
	    do
		arg(I, BranchLabelArray, BrLabelI),
		arg(I, BranchEamArray, EAM),
		arg(I, RetryEamArray, RetryEAM),
		retry_me_instr(Options, ActualESize, ref(LabelI1), eam(RetryEAM), RetryMeInstr),
		Code4 = [
		    code{instr:label(LabelI),regs:[]},
		    code{instr:RetryMeInstr,regs:ArgOrigs},
		    code{instr:label(BrLabelI),regs:[]}
		    |Code5],
		start_new_chunk(EAM, ChunkData1, ChunkData2),
		alloc_check_start_branch(Det, ChunkData2, ChunkData3, Code5, Code51, GAllocI),
		generate_head_info(HeadArgsArray, I, ChunkData3, ChunkData4, PseudoHeadPerms, [], ArgOrigs),
		generate_branch(Branch, PseudoHeadPerms, ChunkData4, ChunkDataE, BranchExitInit, ExitESize, AuxCode3, AuxCode4, Code51, Code6, Options, SelfInfo),
		Code6 = [code{instr:branch(ref(LabelJoin)),regs:[]}|Code7]
	    ),

	    % TRUST (last alternative)
	    arg(NBranches, BranchLabelArray, BrLabelN),
	    arg(NBranches, BranchEamArray, EAMN),
	    arg(NBranches, RetryEamArray, RetryEAMN),
	    trust_me_instr(Options, ActualESize, eam(RetryEAMN), TrustMeInstr),
	    Code8 = [
		code{instr:label(LabelN),regs:[]},
		code{instr:TrustMeInstr,regs:ArgOrigsN},
		code{instr:label(BrLabelN),regs:[]}
		|Code9],
	    start_new_chunk(EAMN, ChunkData1, ChunkData2N),
	    alloc_check_start_branch(Det, ChunkData2N, ChunkData3N, Code9, Code91, GAllocN),
	    generate_head_info(HeadArgsArray, NBranches, ChunkData3N, ChunkData4N, PseudoHeadPermsN, [], ArgOrigsN),
	    generate_branch(BranchN, PseudoHeadPermsN, ChunkData4N, ChunkDataEN, BranchExitInitN, ExitESize, AuxCode5, AuxCode0, Code91, Code10, Options, SelfInfo),

	    % Post-disjunction
	    Code10 = [code{instr:label(LabelJoin),regs:[]}|Code0],
	    init_chunk_data(DisjExitEAM, ExitESize, ChunkData),
	    alloc_check_join([ChunkDataE1|ChunkDataE2toN], ChunkData)

	;
	    printf(error, "ERROR: unexpected goal in generate_chunk", []),
	    abort
	).


% Select retry/trust instructions according to debug mode,
% and whether an environment exists or not (-1).

retry_me_instr(options{debug:off}, -1, Else, EAM, Instr) ?- !, Instr = retry_me_else(#no_port,Else), verify EAM==eam(0).
retry_me_instr(options{debug:on},  -1, Else, EAM, Instr) ?- !, Instr = retry_me_else(#next_port,Else), verify EAM==eam(0).
retry_me_instr(options{debug:off},  _, Else, EAM, Instr) ?- !, Instr = retry_me_inline(#no_port,Else,EAM).
retry_me_instr(options{debug:on},   _, Else, EAM, Instr) ?- !, Instr = retry_me_inline(#else_port,Else,EAM).

trust_me_instr(options{debug:off}, -1, EAM, Instr) ?- !, Instr = trust_me(#no_port), verify EAM==eam(0).
trust_me_instr(options{debug:on},  -1, EAM, Instr) ?- !, Instr = trust_me(#next_port), verify EAM==eam(0).
trust_me_instr(options{debug:off},  _, EAM, Instr) ?- !, Instr = trust_me_inline(#no_port,EAM).
trust_me_instr(options{debug:on},   _, EAM, Instr) ?- !, Instr = trust_me_inline(#else_port,EAM).

retry_instr(options{debug:off}, -1, Alt, _EAM, Instr) ?- !, Instr = retry(#no_port,Alt).
retry_instr(options{debug:on},  -1, Alt, _EAM, Instr) ?- !, Instr = retry(#next_port,Alt).
retry_instr(options{debug:off},  _, Alt,  EAM, Instr) ?- !, Instr = retry_inline(#no_port,Alt,EAM).
retry_instr(options{debug:on},   _, Alt,  EAM, Instr) ?- !, Instr = retry_inline(#else_port,Alt,EAM).

trust_instr(options{debug:off}, -1, Alt, _EAM, Instr) ?- !, Instr = trust(#no_port,Alt).
trust_instr(options{debug:on},  -1, Alt, _EAM, Instr) ?- !, Instr = trust(#next_port,Alt).
trust_instr(options{debug:off},  _, Alt,  EAM, Instr) ?- !, Instr = trust_inline(#no_port,Alt,EAM).
trust_instr(options{debug:on},   _, Alt,  EAM, Instr) ?- !, Instr = trust_inline(#else_port,Alt,EAM).


% Environment activity at retry/trust instructions is the union of
% the activities of this and all following branches still to be tried
make_retry_me_activity_maps(BranchEamArray,RetryEamArray) :-
	arity(BranchEamArray, NBranches),
	dim(RetryEamArray, [NBranches]),
	(
	    for(I,NBranches,1,-1),
	    fromto(0,RetryEAM0,RetryEAM,_),
	    param(BranchEamArray,RetryEamArray)
	do
	    arg(I, BranchEamArray, EAM),
	    arg(I, RetryEamArray, RetryEAM),
	    RetryEAM is RetryEAM0 \/ EAM
	).

make_retry_activity_maps(RevGroup, BranchEamArray, RetryEams) :-
	(
	    foreach(I, RevGroup),
	    foreach(RetryEAM, RetryEams),
	    fromto(0,RetryEAM0,RetryEAM,_),
	    param(BranchEamArray)
	do
	    arg(I, BranchEamArray, EAM),
	    RetryEAM is RetryEAM0 \/ EAM
	).


generate_head_info([], _BranchI, ChunkData, ChunkData, HeadPerms, HeadPerms, []) :- !.
generate_head_info(HeadArgsArray, BranchI, ChunkData0, ChunkData3, HeadPerms3, HeadPerms0, OrigRegDescs) :-
	arg(BranchI, HeadArgsArray, Args),
	(
	    foreach(VarDesc, Args),
	    foreach(r(VarId,a(I),orig,_), OrigRegDescs),
	    fromto(ChunkData0, ChunkData1, ChunkData1, ChunkData3),
	    fromto(HeadPerms3, HeadPerms2, HeadPerms1, HeadPerms0),
	    count(I,1,_)
	do
	    VarDesc = variable{varid:VarId,class:C},
	    ChunkData1 = chunk_data{occurred:OccurredInChunk},
	    verify \+(hash_contains(OccurredInChunk, VarId)),
	    ( C = nonvoid(y(Y)) ->
		hash_set(OccurredInChunk, VarId, delayed_perm),
		HeadPerms2 = [delayed_move(VarId,y(Y))|HeadPerms1]
	    ;
		hash_set(OccurredInChunk, VarId, true),
		HeadPerms2 = HeadPerms1
	    )
	).


emit_call_regular(CallInstr, RegDescs, QPred, Goal, Code, Code0, options{debug:Debug}) :-
	    ( Debug == off ->
		Code = [code{instr:CallInstr,regs:RegDescs}|Code0]
	    ;
		Goal = goal{path:Path,line:Line,from:From,to:To},
		Code = [code{instr:debug_call(QPred,#call_port,Path,Line,From,To),regs:RegDescs},
			code{instr:CallInstr,regs:[]}|Code0]
	    ).


%----------------------------------------------------------------------
% Environment allocation/deallocation

% Lazily insert an allocate instruction just before the first access of y(MinY).
% The allocation size is filled in later when we reach a point where the
% needed size is known, the next regular goal, the next cut, end of branch,
% or start of disjunction. 
env_allocate_if_needed(MinY, ChunkData0, ChunkData, Code0, Code) :-
	% not really using MinY here, only for (incomplete) consistency check
	ChunkData0 = chunk_data{allocated:ExistingESize},
	( var(ExistingESize) ->
	    % allocate instruction already emitted, waiting for size
	    Code0 = Code, ChunkData0 = ChunkData
	; ExistingESize >= 0 ->
	    % already allocated and sized
	    verify ExistingESize >= MinY,
	    Code0 = Code, ChunkData0 = ChunkData
	;
	    % allocate here, size will be inserted later (at least MinY)
	    Code0 = [code{instr:allocate(SizeFilledInLater)}|Code],
	    update_struct(chunk_data, [allocated:SizeFilledInLater], ChunkData0, ChunkData)
	).


% Generate the allocate instruction that is required between the two ChunkData
env_allocate_delta(chunk_data{allocated:Before}, chunk_data{allocated:After}, Code0, Code) ?-
	( Before == After -> Code0 = Code
	; Code0 = [code{instr:allocate(After)}|Code]
	).


% If there was an earlier allocate, make sure it allocates at least ESizeHere.
% If no allocate was emitted so far, don't do anything now.
env_set_allocate_size(-1, chunk_data{allocated:ExistingESize}) :- !,
	verify ExistingESize < 0.	% should have no environment anyway
env_set_allocate_size(ESizeHere, chunk_data{allocated:ExistingESize}) :-
    	( var(ExistingESize) ->
	    ExistingESize = ESizeHere
	; ExistingESize >= 0 ->
	    % already allocated and sized
	    verify ExistingESize >= ESizeHere
	; 
	    true	 % don't allocate here
	).


% Allocate/deallocate, if not yet done
env_allocate_last_chance(-1, ChunkData0, ChunkData, Code0, Code) :- !,
	% deallocation request
	ChunkData0 = chunk_data{allocated:ExistingESize},
    	( var(ExistingESize) ->
	    unreachable("unexpected allocate..deallocate sequence"), abort,
	    ExistingESize = 0,
	    Code0 = [code{instr:deallocate}|Code],
	    update_struct(chunk_data, [allocated: -1], ChunkData0, ChunkData)
	; ExistingESize >= 0 ->
	    % deallocate existing environment
	    Code0 = [code{instr:deallocate}|Code],
	    update_struct(chunk_data, [allocated: -1], ChunkData0, ChunkData)
	; 
	    % no environment anyway
	    Code0 = Code, ChunkData0 = ChunkData
	).
env_allocate_last_chance(ESizeHere, ChunkData0, ChunkData, Code0, Code) :-
	ChunkData0 = chunk_data{allocated:ExistingESize},
	( var(ExistingESize) ->
	    % allocate instruction already emitted, fill in size
	    ExistingESize = ESizeHere,
	    Code0 = Code, ChunkData0 = ChunkData
	; ExistingESize >= 0 ->
	    % already allocated and sized
	    verify ExistingESize >= ESizeHere,
	    Code0 = Code, ChunkData0 = ChunkData
	;
	    % allocate here, for ESizeHere
	    Code0 = [code{instr:allocate(ESizeHere)}|Code],
	    update_struct(chunk_data, [allocated:ESizeHere], ChunkData0, ChunkData)
	).


% Select a call instruction and allocate/deallocate as required
%	EnvAllocated	CallESize	CallInstr
%	-1		-1		jmp
%	 N		-1		chain (= deallocate,jmp)
%	-1		 N		allocate,call
%	 N		 N		call
call_instr(-1, Dest, EAM, ChunkData0, ChunkData, Code, Code, CallInstr) :- !,
	% deallocation request
	ChunkData0 = chunk_data{allocated:ExistingESize},
	verify (EAM==0, nonvar(ExistingESize)),
	( ExistingESize >= 0 ->
	    % deallocate existing environment
	    CallInstr = chain(Dest),
	    update_struct(chunk_data, [allocated: -1], ChunkData0, ChunkData)
	; 
	    % no environment anyway
	    CallInstr = jmp(Dest),
	    ChunkData0 = ChunkData
	).
call_instr(CallESize, Dest, EAM, ChunkData0, ChunkData, Code0, Code, CallInstr) :-
	CallInstr = callf(Dest,eam(EAM)),
	env_allocate_last_chance(CallESize, ChunkData0, ChunkData, Code0, Code).


%----------------------------------------------------------------------
% Indexing code generation
% 
% Compilation scheme: We generate code for all indexes that the indexing
% analysis has discovered, in order of their quality. When an index cannot
% exclude any branches of the disjunction, we fall through and try the next
% best index. If any reduction is achieved, we don't try further indexes
% (although we could) - this prevents index code explosion.
% 
% 1. Main indexes in order of quality
% 
%     These look at one argument register, and jump either
% 	- directly to one alternative
% 	- to a sub-index
% 	- to a try-sequence
% 	- to fail
%     All index instructions fall through for variables. In the unusual
%     case that the variable case filters out any alternatives, a jump
%     follows which effectively extends the switch instruction with a
%     variable case (to avoid falling through to the next index).
% 
% 2. Main indexes are followed by Try_me_else/retry_me_else/trust_me
%	sequence with code for alternatives 1..N
% 
% 3. Followed by continuation after the disjunction.
% 
% 4. Sub-indexes and Try-sequences go into separate AuxCode sequence and
%	are eventually appended to the end of the whole predicate code.
%	These are all short, independent sequences of either a single sub-
%	index instruction (integer_switch etc), or try-retry*-trust. The
%	variable-fall-through cases of the secondary switches are never used.
%	This code doesn't need the register allocator run over it (its
%	register positions are shared with the main code sequence).
%	The reason it goes at the end of the code is so we don't need
%	to jump over it.
% 
% 
% Each Index consists of one or more switch instructions that operate
% on the same variable (argument register or permanent variable).
% Possible combinations, with optional parts in brackets:
% 
%     switch_on_type TypeLabel1...TypeLabelN
%     [branch VarLabel]
%     ...
%     [AtomLabel:	atom_switch ValueLabel1...ValueLabelN]
%     [IntLabel:	integer_switch ValueLabel1...ValueLabelN]
%     [FunctorLabel:	functor_switch ValueLabel1...ValueLabelN]
% 
%     atom_switch ValueLabel1...ValueLabelN DefaultLabel
%     [branch VarLabel]
% 
%     integer_switch ValueLabel1...ValueLabel DefaultLabel
%     [branch VarLabel]
% 
%     functor_switch ValueLabel1...ValueLabelN DefaultLabel
%     [branch VarLabel]
% 
%     list_switch ListLabel NilLabel DefaultLabel
%     [branch VarLabel]
% 
% The indexing code should not move any data around, so register and
% environment slot contents remain untouched. This is because it
% contains jumps to the beginnings of the other alternatives, which
% all expect the same starting state as the first alternative
% before any indexing code.
%----------------------------------------------------------------------

% generate_indexing
% Input:	IndexDescs - ordered list of index descriptors
%		BranchLabelArray - labels for alternative branches
%		BranchEamArray - entry EAMs for alternative branches
%		TryArity - number of args to save in choicepoints
% Output:	Code - main indexing code
%		AuxCode - sub-index and try-sequence code

generate_indexing(IndexDescs, BranchLabelArray, BranchEamArray, TryArity, ChunkData, Code0, Code, AuxCode0, AuxCode, Options) :-
	arity(BranchLabelArray, NBranches),
	( for(I,1,NBranches), foreach(I,AllBranches) do true ),
	hash_create(LabelTable),
	(
	    foreach(index{quality:Quality,variable:VarDesc,partition:DecisionTree},IndexDescs),
	    fromto(Code0,Code1,Code3,Code),
	    fromto(AuxCode0,AuxCode1,AuxCode2,AuxCode),
	    param(LabelTable,BranchLabelArray,BranchEamArray,AllBranches,TryArity,NBranches,ChunkData,Options)
	do
	    ( Quality < NBranches ->
		% Create label for "all branches of the disjunction". This is
		% re-created for each index, and is the address of the next
		% index, or the try_me-sequence respectively.
		hash_set(LabelTable, AllBranches, NextIndexLabel),

		generate_index(VarDesc, DecisionTree, LabelTable, BranchLabelArray, BranchEamArray, NextIndexLabel,
		    TryArity, ChunkData, Code1, Code2, AuxCode1, AuxCode2, Options),
		Code2 = [code{instr:label(NextIndexLabel),regs:[]}|Code3]
	    ;
		% Omit really bad indexes
	    	Code1=Code3, AuxCode1=AuxCode2
	    )
	).


% Precompute a sorted list of the non-variable tags
:- local variable(tagnames).
:- local initialization((
    	sepia_kernel:decode_code(tags,TagArray),
	TagArray=..[_|TagList0],
	once delete(meta, TagList0, TagList1),
	sort(TagList1, TagList),
	setval(tagnames, TagList)
    )).


% Generate code for the index characterised by VarDesc and DecisionTree

generate_index(VarDesc, DecisionTree, LabelTable, BranchLabelArray, BranchEamArray, NextIndexLabel,
	    	TryArity, ChunkData, Code0, Code, AuxCode0, AuxCode, Options) :-
	VarDesc = variable{varid:VarId},
	ChunkData = chunk_data{allocated:Allocated},

	% Create a label for this index's default case
	dt_lookup2(DecisionTree, [], DefaultGroup, _),
	create_group(DefaultGroup, LabelTable, BranchLabelArray, BranchEamArray, TryArity, DefaultLabel, Allocated, Options, AuxCode0, AuxCode1),

	% First go through the non-variable tags: generate switch_on_values,
	% try-sequences for branch-groups and a hash table of their labels,
	% and a table for use by switch_on_type.
	getval(tagnames, TagNames),
	(
	    foreach(TagName,TagNames),				% in: tag name
	    foreach(TagName:ref(TagLabel),Table0),		% out: partial table for switch_on_type
	    fromto(UsedTags,UsedTags1,UsedTags0,[]),		% out: tags that need to be distinguished
	    fromto(SubDefaults,SubDefaults1,SubDefaults0,[]),	% out: default labels of subswitches
	    fromto(AuxCode1,AuxCode2,AuxCode6,AuxCode7),	% out: code for try-sequences
	    fromto(TmpCode0,TmpCode1,TmpCode3,TmpCode4),	% out: code for sub-switches
	    param(DecisionTree,BranchLabelArray,BranchEamArray,TryArity,DefaultLabel,VarId,Allocated,Options),	% in
	    param(LabelTable),					% inout: labels of try-groups
	    param(VarLoc,SubRegDesc)				% out: parameters for sub-switches
	do
	    ( dt_lookup2(DecisionTree, [TagName], TagDefaultGroup, TagExceptions) ->
		% we have entries for this tag
		UsedTags1 = [TagName|UsedTags0],
		( TagExceptions = [] ->
		    % need only a try sequence for this tag
		    verify TagDefaultGroup \== [],
		    % group: all alternatives for this tag
		    SubDefaults1 = SubDefaults0,
		    TmpCode1 = TmpCode3,
		    create_group(TagDefaultGroup, LabelTable, BranchLabelArray, BranchEamArray, TryArity, TagLabel, Allocated, Options, AuxCode2, AuxCode6)
		;
		    % we could use a switch_on_value
		    ( TagDefaultGroup == [] ->
			TagDefaultLabel = DefaultLabel,
			AuxCode2 = AuxCode3
		    ;
			% group: default alternatives for this type
			create_group(TagDefaultGroup, LabelTable, BranchLabelArray, BranchEamArray, TryArity, TagDefaultLabel, Allocated, Options, AuxCode2, AuxCode3)
		    ),
		    % make a value switch, unless it is trivial
		    ( TagDefaultLabel == fail, DefaultLabel == fail, TagExceptions = [_Value-ValueGroup], ValueGroup = [_] ->
			% omit singleton value switches
			% (although they could lead to earlier failure)
			SubDefaults1 = SubDefaults0,
			TmpCode1 = TmpCode3,
			create_group(ValueGroup, LabelTable, BranchLabelArray, BranchEamArray, TryArity, TagLabel, Allocated, Options, AuxCode3, AuxCode6)
		    ;
			% do use a value switch
			SubDefaults1 = [TagDefaultLabel|SubDefaults0],
			(
			    foreach(Value-ValueGroup,TagExceptions),
			    foreach(Value-ref(ValueLabel),ValueLabels),
			    fromto(AuxCode3,AuxCode4,AuxCode5,AuxCode6),
			    param(LabelTable,BranchLabelArray,BranchEamArray,TryArity,Allocated,Options)
			do
			    % group: alternatives for this value
			    create_group(ValueGroup, LabelTable, BranchLabelArray, BranchEamArray, TryArity, ValueLabel, Allocated, Options, AuxCode4, AuxCode5)
			),
			TmpCode1 = [code{instr:label(TagLabel),regs:[]}|TmpCode2],
			emit_switch_on_value(VarId, TagName, ValueLabels, TagDefaultLabel, VarLoc, SubRegDesc, TmpCode2, TmpCode3)
		    )
		)
	    ;
		% no entries for this tag, use global default label
		TagLabel = DefaultLabel,
		AuxCode2 = AuxCode6,
		TmpCode1 = TmpCode3,
		UsedTags1 = UsedTags0,
		SubDefaults1 = SubDefaults0
	    )
	),

	% Now consider the variable tags (var/meta/free)
	( dt_lookup2(DecisionTree, [var], VarDefaultGroup, VarExceptions) ->
	    ( VarExceptions == [] ->
		% no distinction free/meta
		create_group(VarDefaultGroup, LabelTable, BranchLabelArray, BranchEamArray, TryArity, VarLabel, Allocated, Options, AuxCode7, AuxCode9),
		Table = [meta:ref(VarLabel)|Table0]
	    ;
		% need to distinguish free/meta
		( member(meta-MetaGroup, VarExceptions) -> true ; MetaGroup = VarDefaultGroup ),
		( member(free-FreeGroup, VarExceptions) -> true ; FreeGroup = VarDefaultGroup ),
		create_group(FreeGroup, LabelTable, BranchLabelArray, BranchEamArray, TryArity, VarLabel, Allocated, Options, AuxCode7, AuxCode8),
		create_group(MetaGroup, LabelTable, BranchLabelArray, BranchEamArray, TryArity, MetaLabel, Allocated, Options, AuxCode8, AuxCode9),
		Table = [meta:ref(MetaLabel)|Table0]
	    )
	;
	    % no var cases (rare)
	    Table = [meta:ref(DefaultLabel)|Table0],
	    VarLabel = DefaultLabel,
	    AuxCode7 = AuxCode9
	),

	% Get the location of the switch-variable
	reg_or_perm(VarDesc, ChunkData, FirstRegDesc, VarLoc),
	% Create switch_on_type if useful
	( var(MetaGroup), UsedTags=[_ValueSwitchTag], SubDefaults==[DefaultLabel] ->
	    % We don't need a switch_on_type
	    % hook (single) subswitch code into main sequence
	    Code0 = TmpCode0, TmpCode4 = Code1, AuxCode9 = AuxCode,
	    SubRegDesc = FirstRegDesc

	; var(MetaGroup), list_tags_only(UsedTags) ->
	    % A list_switch is sufficient
	    verify TmpCode0 == TmpCode4,	% should have no subswitches
	    emit_switch_on_list(Table, DefaultLabel, VarLoc, FirstRegDesc, Code0, Code1),
	    AuxCode9 = AuxCode
	;
	    % Need the full switch_on_type, possibly with subswitches
	    emit_switch_on_type(Table, VarLoc, FirstRegDesc, Code0, Code1),
	    % hook subswitches (zero or more) into aux sequence
	    AuxCode9 = TmpCode0, TmpCode4 = AuxCode,
	    SubRegDesc = r(VarId,VarLoc,use,_)
	),
	emit_var_jmp(VarLabel, NextIndexLabel, Code1, Code).


list_tags_only([[]]) :- !.
list_tags_only([list]) :- !.
list_tags_only([[],list]) :- !.


% A "group" is a sequence of clauses linked by try/retry/trust-instructions.
% Get the label for the given group. Create a try sequence if necessary.
create_group(Group, LabelTable, BranchLabelArray, BranchEamArray, TryArity, GroupLabel, Allocated, Options, AuxCode1, AuxCode) :-
	( Group = [] ->
	    AuxCode1 = AuxCode,
	    GroupLabel = fail
	; hash_get(LabelTable, Group, GroupLabel) ->
	    AuxCode1 = AuxCode
	;
	    hash_set(LabelTable, Group, GroupLabel),
	    emit_try_sequence(Group, BranchLabelArray, BranchEamArray, TryArity, GroupLabel, Allocated, Options, AuxCode1, AuxCode)
	).


% Emit the switch_on_type instruction or its simpler version list_switch.
%	Table		List of Tagname:ref(Label)
%	DefaultLabel	Label for tags that do not occur in Types

emit_switch_on_type(Table, VarLoc, RegDesc, Code0, Code) :-
	Code0 = [code{instr:switch_on_type(VarLoc,Table),
		    regs:[RegDesc]}|Code].


emit_switch_on_list(Table, DefaultLabel, VarLoc, RegDesc, Code0, Code) :-
	memberchk([]:NilRef, Table),
	memberchk(list:ListRef, Table),
	Code0 = [code{instr:list_switch(VarLoc,ListRef,NilRef,ref(DefaultLabel)),
		    regs:[RegDesc]}|Code].



% Emit a jump to VarLabel, unless it is the (subsequent) NextIndexLabel
emit_var_jmp(VarLabel, NextIndexLabel, Code0, Code) :-
	( VarLabel == NextIndexLabel ->
	    Code0 = Code
	;
	    Code0 = [code{instr:branch(ref(VarLabel)),regs:[]}|Code]
	).


% Emit switches on constants (can be main index or sub-index).
% Note: if this is used to generate a sub-index, then the code goes
% into the AuxCode sequence, and the register allocator will not run
% over it. In this case, VarLoc gets instantiated as a side effect of
% the register allocator running over the corresponding main index.
% RegDesc is ignored in this case.
emit_switch_on_value(_VarId, integer, Table, DefaultLabel, VarLoc, RegDesc,
	    [code{instr:integer_switch(VarLoc,Table,ref(DefaultLabel)),
		    regs:[RegDesc]}|Code], Code).
emit_switch_on_value(_VarId, atom, Table, DefaultLabel, VarLoc, RegDesc,
	    [code{instr:atom_switch(VarLoc,Table,ref(DefaultLabel)),
		    regs:[RegDesc]}|Code], Code).
emit_switch_on_value(_VarId, structure, Table, DefaultLabel, VarLoc, RegDesc,
	    [code{instr:functor_switch(VarLoc,Table,ref(DefaultLabel)),
		    regs:[RegDesc]}|Code], Code).


emit_try_sequence(Group, BranchLabelArray, BranchEamArray, TryArity, TryLabel, Allocated, Options, Code1, Code6) :-
	( Group = [BranchNr1|BranchNrs2toN] ->
	    arg(BranchNr1, BranchLabelArray, BranchLabel1),
	    ( BranchNrs2toN == [] ->
		% only one alternative, no try sequence needed
		TryLabel = BranchLabel1,
		Code1 = Code6
	    ;
		Code1 = [code{instr:label(TryLabel),regs:[]},
			code{instr:try(#no_port,TryArity,ref(BranchLabel1)),regs:[]}
			|Code2],
		(
		    fromto(BranchNrs2toN,[BranchNr|BranchNrs],BranchNrs,[BranchNrN]),
		    fromto([],RevGroup1,[BranchNr|RevGroup1],RevGroup),
		    fromto([],RetryEams1,[RetryEam|RetryEams1],RetryEams),
		    fromto(Code2,Code3,Code4,Code5),
		    param(BranchLabelArray,Allocated,Options)
		do
		    Code3 = [code{instr:RetryInstr,regs:[]}|Code4],
		    arg(BranchNr, BranchLabelArray, BranchLabel),
		    retry_instr(Options, Allocated, ref(BranchLabel), eam(RetryEam), RetryInstr)
		),
		Code5 = [code{instr:TrustInstr,regs:[]}|Code6],
		arg(BranchNrN, BranchLabelArray, BranchLabelN),
		trust_instr(Options, Allocated, ref(BranchLabelN), eam(TrustEam), TrustInstr),
		make_retry_activity_maps([BranchNrN|RevGroup], BranchEamArray, [TrustEam|RetryEams])
	    )
	;
	    TryLabel = fail, Code1 = Code6
	).


% Var is expected either in a temporary or a perm (not first).
% return a corresponding register descriptor
reg_or_perm(Var, ChunkData, RegDesc, VarLoc) :-
	Var = variable{varid:VarId},
	variable_occurrence(Var, ChunkData, ChunkData1, Code0, Code1, VarOccDesc),
	verify (ChunkData==ChunkData1, Code0==Code1),
	( VarOccDesc = tmp ->
	    RegDesc = r(VarId,VarLoc,use,_)
	; VarOccDesc = perm_first_in_chunk(VarLoc) ->
	    RegDesc = r(VarId,VarLoc,perm,_)
	; verify VarOccDesc = perm(_Y),
	    RegDesc = r(VarId,VarLoc,use,_)
	).


% Initialize environment slots according to the bitmap given.  We can't
% use the current initialize instruction because we want global variables.
% This code doesn't need register allocation run over it!
emit_initialize(EAM, Code, Code0) :-
	decode_activity_map(EAM, Ys),
	length(Ys, N),
	% We always generate a gc_test, assuming we are in a separate
	% pseudo-chunk at the end of a branch. In this case, we must
	% establish a stack margin at the end of the branch, because
	% the following chunk will assume the availability of it.
	% This is ugly, but could be simply folded into an initialize
	% instruction.
	Code = [code{instr:gc_test(N)}|Code3],
	(
	    foreach(Y,Ys),
	    fromto(Code3,Code1,Code2,Code0)
	do
	    Code1 = [code{instr:put_global_variable(y(Y)),regs:[],comment:initialize}|Code2]
	).


%----------------------------------------------------------------------
% Regular goal arguments
% We first "put" arguments that have the most first occurrences
% of variables within compound terms. Reason:
% If a variable occurs directly on an argument position and also
% within a structure in another argument, the structure should be
% put first so the variable is located inside the structure.
% In addition, temps should be freed as soon as possible, so
% arguments with lots of temporaries should be put first.
%----------------------------------------------------------------------

generate_regular_puts(Args, ChunkData0, ChunkData, Code0, Code, CallRegDescs, Module) :-

	% determine an order (this should be an option)
	heuristic_put_order(Args, ChunkData0, Ordered),

	% construct the arguments in the determined order
	(
	    foreach(put(_,I,Arg), Ordered),
	    foreach(r(ArgId,a(I),dest,_), CallRegDescs),
	    fromto(ChunkData0, ChunkData1, ChunkData2, ChunkData),
	    fromto(Code0, Code1, Code2, Code),
	    param(Module)
	do
	    put_term(Arg, ChunkData1, ChunkData2, Code1, Code2, ArgId, Module)
	).


heuristic_put_order(Args, ChunkData, SortedWeightsIs) :-
	(
	    count(I,1,_),
	    foreach(Arg,Args),
	    foreach(put(Weight,I,Arg), WeightsIs),
	    param(ChunkData)
	do
	    heuristic_argument_weight(Arg, 0, ChunkData, 0, Weight)
	),
	sort(1, >=, WeightsIs, SortedWeightsIs),
%	( WeightsIs==SortedWeightsIs-> true ; writeln(SortedWeightsIs) ),
	true.


    :- mode heuristic_argument_weight(+,+,+,+,-).
    heuristic_argument_weight(Var, InStruct, ChunkData, VN0, VN) :-
	Var = variable{class:C},
	( potential_first_temp_occurrence(Var, ChunkData) ->
	    % first occurrences of temp variables inside compound terms
	    % count towards the weight because they require a new register.
	    VN is VN0 + InStruct
	; C = nonvoid(y(_)) ->
	    % perms are treated like constants
	    VN is VN0 - 1 + InStruct
	;
	    VN = VN0
	).
    heuristic_argument_weight(structure{args:Args}, _, ChunkData, VN0, VN) :-
	heuristic_argument_weight(Args, 1, ChunkData, VN0, VN).
    heuristic_argument_weight([X|Xs], _, ChunkData, VN0, VN) :-
	heuristic_argument_weight(X, 1, ChunkData, VN0, VN1),
	heuristic_argument_weight(Xs, 1, ChunkData, VN1, VN).
    heuristic_argument_weight(Term, InStruct, _ChunkData, VN0, VN) :-
    	atomic(Term),
	% constants should be put last because putting them definitely
	% uses up one register
	VN is VN0 - 1 + InStruct.


/*

A different method...

% The interesting point here is computing the order in which the
% arguments for the call will be constructed. There are two aspects:
% Dataflow: every put overwrites an argument register, so this
% register must not be the only source for something still needed.
% We therefore compute a dependency graph and sort it topologically.
% Heuristics: if a variable occurs both on its own and in a compound
% term, the compound terms should be put first because that locates
% the variable within the term and saves an instruction.

generate_regular_puts(goal{args:Args,functor:F/N},
		ChunkData0, ChunkData, Code0, Code) :-
	Call =.. [F|Args],	%%% preliminary

	% For each argument of the call, find out which current argument
	% register's content is needed to construct it (if any).
	% Also, compute a heuristic argument weight.
	functor(NeededRegs, F, N),	% array of register lists
	functor(OccupiedBy, F, N),	% array of varids
	(
	    for(I,1,N),
	    foreach(NVars-I, VarWeights),
	    param(Call,NeededRegs,ChunkData0,OccupiedBy,N)
	do
	    arg(I, Call, Arg),
	    arg(I, NeededRegs, Regs),
	    collect_arg_regs_needed_in_term(Arg, I, N, ChunkData0, OccupiedBy, [], Regs, 0, NVars)
	),

	% Preorder the arguments heuristically: sort them according to
	% the number of variables that occur within structures.
	% (the order is reversed because the subsequent topsort will
	% reverse it again!)
	sort(1, =<, VarWeights, SortedVarWeights),
	( foreach(_-I,SortedVarWeights), foreach(I,RevPreOrder) do true ),

	% By topological sorting of the "needs" graph, find a good order
	% to construct the call arguments. CycleBreakers are graph edges
	% that need to be removed to allow topological sorting.
	top_sort(NeededRegs, RevPreOrder, Order, CycleBreakers),
	printf("Order: %w, Breakers: %w%n", [Order, CycleBreakers]),

	% We move the "needed" register for every problematic edge
	% to an alternative location.
	(
	    foreach(_PutPos->NeededPos, CycleBreakers),
	    fromto(ChunkData0, ChunkData1, ChunkData2, ChunkData3),
	    fromto(Code0, Code1, Code2, Code3),
	    param(OccupiedBy)
	do
	    arg(NeededPos, OccupiedBy, VarId),
	    replace_current_location(VarId, a(NeededPos), Tmp, ChunkData1, ChunkData2),
	    Code1 = [move(a(NeededPos),Tmp)|Code2]
	),

	% Finally construct the arguments in the topological order
	(
	    foreach(I,Order),
	    fromto(ChunkData3, ChunkData4, ChunkData5, ChunkData),
	    fromto(Code3, Code4, Code5, Code),
	    param(Call)
	do
	    arg(I, Call, Arg),
	    % TODO: could lookup  (J needs I)  here and move I away
	    % instead of doing eager previous loop 
	    body(I, Arg, ChunkData4, ChunkData5, Code4, Code5)
	).


    % Term is the I-th argument of Max arguments to a call.
    % We compute a list of those registers whose contents is absolutely
    % needed to construct this argument. These registers come from variables
    % that occur in Term and have only a single location which is a
    % register =< Max (with the trivial exception of the correct
    % register occuring already in the correct call position).
    % As an unrelated extra, we count the number of variables that occur
    % within structures - this will be used as an ordering heuristics.
    :- mode collect_arg_regs_needed_in_term(+,+,+,+,+,+,-,+,-).
    collect_arg_regs_needed_in_term(variable{varid:VarId}, I, Max, ChunkData, OccupiedBy, Regs0, Regs, VN0, VN) :-
	VN is VN0+1-sgn(I),	% I::1..Max for topmost, 0 other occurrences
	(
	    get_current_locations(VarId, ChunkData, CurrentLocations),
	    CurrentLocations = [SingleLocation],
	    nonvar(SingleLocation),
	    SingleLocation = a(J),
	    J =\= I,		% not topmost (I=0), or wrong register
	    J =< Max
	->
	    arg(J, OccupiedBy, VarId),
	    Regs = [J|Regs0]
	;
	    Regs = Regs0
	).
    collect_arg_regs_needed_in_term(structure{args:Args}, _, Max, ChunkData, OccupiedBy, Regs0, Regs, VN0, VN) :-
	collect_arg_regs_needed_in_term(Args, 0, Max, ChunkData, OccupiedBy, Regs0, Regs, VN0, VN).
    collect_arg_regs_needed_in_term([X|Xs], _, Max, ChunkData, OccupiedBy, Regs0, Regs, VN0, VN) :-
	collect_arg_regs_needed_in_term(X, 0, Max, ChunkData, OccupiedBy, Regs0, Regs1, VN0, VN1),
	collect_arg_regs_needed_in_term(Xs, 0, Max, ChunkData, OccupiedBy, Regs1, Regs, VN1, VN).
    collect_arg_regs_needed_in_term(Term, _, _Max, _ChunkData, _OccupiedBy, Regs, Regs, VN, VN) :-
    	atomic(Term).
*/


%----------------------------------------------------------------------
% Simple goals with flexible argument registers
% These are compiled using dedicated instructions of the form
%	instr Ai ... Ak
%----------------------------------------------------------------------

generate_simple_goal(Goal, ChunkData0, ChunkData, Code0, Code, Options, Module) :-
	Goal = goal{functor: (=)/2, args:[Arg1,Arg2],definition_module:sepia_kernel},
	generate_unify(Arg1, Arg2, ChunkData0, ChunkData, Code1, Code2, Module), % may fail
	emit_debug_noarg(Goal, Code0, Code1, Code2, Code, Options, Module),
	!.

generate_simple_goal(Goal, ChunkData0, ChunkData, Code0, Code, Options, Module) :-
	Goal = goal{functor: (==)/2, args:[Arg1,Arg2],definition_module:sepia_kernel},
	generate_identity(Arg1, Arg2, ChunkData0, ChunkData, Code1, Code2), % may fail
	emit_debug_noarg(Goal, Code0, Code1, Code2, Code, Options, Module),
	!.

generate_simple_goal(Goal, ChunkData0, ChunkData, Code0, Code, _Options, _Module) :-
	Goal = goal{functor: (?=)/2, args:[Arg1,Arg2],definition_module:sepia_kernel},
	!,
	generate_in_unify(Arg1, Arg2, ChunkData0, ChunkData, Code0, Code).

generate_simple_goal(goal{functor: get_cut/1, args:[Arg],definition_module:sepia_kernel}, ChunkData0, ChunkData, Code0, Code, _Options, _Module) ?- !,
	Arg = variable{varid:VarId},
	variable_occurrence(Arg, ChunkData0, ChunkData, Code0, Code1, VarOccDesc),
	( VarOccDesc = void ->
	    Code1 = Code
	; VarOccDesc = tmp_first ->
	    Code1 = [code{instr:savecut(R),regs:[r(VarId,R,def,_)]}|Code]
	; VarOccDesc = perm_first(Y) ->
	    Code1 = [code{instr:savecut(Y),regs:[r(VarId,Y,perm,_)]}|Code]
	;
	    verify false	% require a first occurrence!
	).

generate_simple_goal(goal{functor: cut_to/1, args:[Arg],definition_module:sepia_kernel,envsize:ESize}, ChunkData0, ChunkData, Code0, Code, _Options, _Module) ?- !,
	Arg = variable{varid:VarId},
	variable_occurrence(Arg, ChunkData0, ChunkData1, Code0, Code1, VarOccDesc),
	ChunkData1 = chunk_data{allocated:ExistingESize},
	( ESize >= 0 ->
	    % cut (and trim if necessary)
	    ( nonvar(ExistingESize) -> true ;
		ExistingESize = ESize	% fill in previous alloc
	    ),
	    ( ExistingESize >= 0 ->
		% cut and trim existing environment
		verify ExistingESize >= ESize,
		update_struct(chunk_data, [allocated:ESize], ChunkData1, ChunkData),
		( VarOccDesc = perm_first_in_chunk(Y) ->
		    Code1 = [code{instr:cut(Y,ESize),regs:[r(VarId,Y,perm,_)]}|Code]
		; VarOccDesc = perm(_Y) ->
		    Code1 = [code{instr:cut(RY,ESize),regs:[r(VarId,RY,use,_)]}|Code]
		; verify VarOccDesc = tmp,
		    Code1 = [code{instr:cut(RY,ESize),regs:[r(VarId,RY,use,_)]}|Code]
		)
	    ;
		% no environment, just cut (allocation expected later)
		verify VarOccDesc == tmp,
		Code1 = [code{instr:cut(R),regs:[r(VarId,R,use,_)]}|Code],
		ChunkData1 = ChunkData
	    )
	;
	    % deallocation request
	    ( nonvar(ExistingESize) -> true ;
		unreachable("unexpected allocate..cut..deallocate sequence"),
		ExistingESize = 0	% fill in previous alloc
	    ),
	    ( ExistingESize >= 0 ->
		% cut and deallocate existing environment
		update_struct(chunk_data, [allocated: -1], ChunkData1, ChunkData),
		( VarOccDesc = perm_first_in_chunk(Y) ->
		    Code1 = [code{instr:cut(Y,0),regs:[r(VarId,Y,perm,_)]}, code{instr:deallocate}|Code]
		; VarOccDesc = perm(_Y) ->
		    Code1 = [code{instr:cut(RY,0),regs:[r(VarId,RY,use,_)]}, code{instr:deallocate}|Code]
		; verify VarOccDesc = tmp,
		    Code1 = [code{instr:cut(RY,0),regs:[r(VarId,RY,use,_)]}, code{instr:deallocate}|Code]
		)
	    ; 
		% no environment anyway, just cut
		verify VarOccDesc == tmp,
		Code1 = [code{instr:cut(R),regs:[r(VarId,R,use,_)]}|Code],
		ChunkData1 = ChunkData
	    )
	).

generate_simple_goal(Goal, ChunkData0, ChunkData, Code0, Code, Options, Module) ?-
	Goal = goal{functor: (-)/3, args:[A1,A2in,A3], definition_module:sepia_kernel},
	integer(A2in), A2 is -A2in, smallint(A2),
	!,
	update_struct(goal, [functor:(+)/3,args:[A1,A2,A3]], Goal, Goal1),
	generate_simple_goal(Goal1, ChunkData0, ChunkData, Code0, Code, Options, Module).

generate_simple_goal(Goal, ChunkData0, ChunkData, Code0, Code, Options, Module) ?-
	Goal = goal{functor: (+)/3, args:[A1,A2,A3], definition_module:sepia_kernel},
	smallint(A1), \+smallint(A2),
	!,
	update_struct(goal, [args:[A2,A1,A3]], Goal, Goal1),
	generate_simple_goal(Goal1, ChunkData0, ChunkData, Code0, Code, Options, Module).

generate_simple_goal(Goal, ChunkData0, ChunkData, Code0, Code, Options, Module) ?-
	Goal = goal{functor:Name/Arity, args:Args,definition_module:sepia_kernel},
	inlined_builtin(Name, Arity, GlobalAlloc, InstrTemplate),	% nondet
	functor(InstrTemplate, InstrName, N),
	functor(Instr, InstrName, N),
	% We have to generate the code in the correct order because of the
	% occurrence information in ChunkData. First the input arguments:
	(
	    foreach(Arg,Args),
	    count(I,1,_),
	    fromto(RegDescs,RegDescs1,RegDescs2,RegDescs3),
	    fromto(ChunkData0, ChunkData1, ChunkData2, ChunkData3),
	    fromto(Code0, Code1, Code2, Code3),
	    fromto(0,ArgDesc1,ArgDesc2,ArgDesc),
	    param(Instr,InstrTemplate,Module)
	do
	    arg(I, Instr, Reg),
	    arg(I, InstrTemplate, Expect),
	    add_arg_desc(I, Expect, ArgDesc1, ArgDesc2),
	    ( Expect == int ->
	    	smallint(Arg),			% may fail
		Reg=Arg, RegDescs1=RegDescs2, ChunkData1=ChunkData2, Code1=Code2
	    ; Expect == mod ->
	    	atom(Arg),			% may fail
		Reg=Arg, RegDescs1=RegDescs2, ChunkData1=ChunkData2, Code1=Code2
	    ; Expect == arg ->
		RegDescs1 = [r(ValId,Reg,use_a,_)|RegDescs2],
		put_term(Arg, ChunkData1, ChunkData2, Code1, Code2, ValId, Module)
	    ;
		% ignore here - treat in second pass
		RegDescs1=RegDescs2, ChunkData1=ChunkData2, Code1=Code2
	
	    )
	),
	!,	% Commit to this instruction variant and emit it
	( N>0, arg(N, InstrTemplate, desc) ->
	    arg(N, Instr, ArgDesc), DbgArgDesc = -1, NArgs is N-1
	;
	    DbgArgDesc = ArgDesc, NArgs = N
	),
	emit_call_simple(Instr, NArgs, DbgArgDesc, RegDescs, Goal, Code3, Code4, DbgLabel, Options, Module),
	alloc_check_after(GlobalAlloc, ChunkData3, ChunkData4, Code4, Code5),
	% Now generate code for result unification, if necessary
	(
	    foreach(Arg,Args),
	    count(I,1,_),
	    fromto(RegDescs3,RegDescs4,RegDescs5,[]),
	    fromto(ChunkData4, ChunkData5, ChunkData7, ChunkData),
	    fromto(Code5, Code6, Code8, Code9),
	    param(Instr,InstrTemplate)
	do
	    arg(I, Instr, Reg),
	    arg(I, InstrTemplate, Expect),
	    ( Expect == uarg ->
		RegDescs4 = [RegDesc|RegDescs5],
		unify_result(Arg, Reg, RegDesc, ChunkData5, ChunkData6, Code6, Code7, GlobalAlloc2),
		alloc_check_after(GlobalAlloc2, ChunkData6, ChunkData7, Code7, Code8)
	    ;
		% these were treated in the first pass
		RegDescs4=RegDescs5, ChunkData5=ChunkData7, Code6=Code8
	    )
	),
	emit_exit_simple(InstrTemplate, DbgLabel, Code9, Code, Options).

generate_simple_goal(goal{functor: P, args:Args}, ChunkData0, ChunkData, Code0, Code, _Options, Module) ?-
	P = _/Arity,
	dim(RegArr, [Arity]),
	dim(RegDescs, [Arity]),
	heuristic_put_order(Args, ChunkData0, OrderedPuts),
	(
	    foreach(put(_,I,Arg),OrderedPuts),
	    fromto(ChunkData0, ChunkData2, ChunkData3, ChunkData4),
	    fromto(Code0, Code1, Code2, Code3),
	    param(RegArr,RegDescArr,Module)
	do
	    arg(I, RegArr, Reg),
	    arg(I, RegDescArr, r(ValId,Reg,use_a,_)),
	    put_term(Arg, ChunkData2, ChunkData3, Code1, Code2, ValId, Module)
	),
	RegArr =.. [_|Regs],
	RegDescArr =.. [_|RegDescs],
	Code3 = [code{instr:escape(P,Regs),regs:RegDescs}|Code4],
	alloc_check_after(unbounded, ChunkData4, ChunkData, Code4, Code).


% All the builtins that can be implemented by an instruction like
%	bi_inst Ai ... Ak [desc]

% inlined_builtin(+Name, +Arity, -MaxGlobalAlloc, -InstructionTemplate)
%
% MaxGlobalAlloc could be made more precise:
%	arithmetics	can create bignums, therefore unbounded
%	others		can only delay or make exception -> const+arity
%
% InstructionTemplate has the instruction name as its functor, and its
% arguments describe how the instruction interprets its arguments.
%	arg	pointer to argument register
%	uarg	pointer to uninitialised argument register
%	int	32-bit integer
%	mod	module did
%	desc	runtime descriptor for the preceding arguments
%
% If a builtin has special instruction templates for special cases,
% they must be listed before the general template (see e.g. +/3).
%
inlined_builtin(fail,		0,	0,		failure).
inlined_builtin(false,		0,	0,		failure).
inlined_builtin(free,		1,	0,		bi_free(arg)).
inlined_builtin(is_suspension,	1,	0,		bi_is_suspension(arg)).
inlined_builtin(is_event,	1,	0,		bi_is_event(arg)).
inlined_builtin(is_handle,	1,	0,		bi_is_handle(arg)).
inlined_builtin(nonvar,		1,	0,		bi_nonvar(arg)).
inlined_builtin(var,		1,	0,		bi_var(arg)).
inlined_builtin(meta,		1,	0,		bi_meta(arg)).
inlined_builtin(atom,		1,	0,		bi_atom(arg)).
inlined_builtin(integer,	1,	0,		bi_integer(arg)).
inlined_builtin(bignum,		1,	0,		bi_bignum(arg)).
inlined_builtin(rational,	1,	0,		bi_rational(arg)).
inlined_builtin(real,		1,	0,		bi_real(arg)).
inlined_builtin(float,		1,	0,		bi_float(arg)).
inlined_builtin(breal,		1,	0,		bi_breal(arg)).
inlined_builtin(string,		1,	0,		bi_string(arg)).
inlined_builtin(number,		1,	0,		bi_number(arg)).
inlined_builtin(atomic,		1,	0,		bi_atomic(arg)).
inlined_builtin(callable,	1,	0,		bi_callable(arg)).
inlined_builtin(compound,	1,	0,		bi_compound(arg)).
inlined_builtin(is_list,	1,	0,		bi_is_list(arg)).
inlined_builtin(==,		2,	0,		get_matched_value(arg,arg)).
inlined_builtin(\==,		2,	0,		bi_not_identical(arg,arg)).
inlined_builtin(set_bip_error,	1,	0,		bi_set_bip_error(arg)).
inlined_builtin(cont_debug,	0,	0,		bi_cont_debug).
inlined_builtin(sys_return,	1,	0,		bi_exit(arg)).
inlined_builtin(\==,		3,	unbounded,	bi_not_ident_list(arg,arg,arg)).
inlined_builtin(~=,		2,	unbounded,	bi_inequality(arg,arg)).
inlined_builtin(make_suspension, 4,	unbounded,	bi_make_suspension(arg,arg,arg,arg,desc)).
inlined_builtin(=:=,		3,	unbounded,	bi_eq(arg,arg,mod,desc)).
inlined_builtin(=:=,		3,	unbounded,	bi_eq(arg,arg,arg,desc)).
inlined_builtin(=\=,		3,	unbounded,	bi_ne(arg,arg,mod,desc)).
inlined_builtin(=\=,		3,	unbounded,	bi_ne(arg,arg,arg,desc)).
inlined_builtin(<,		3,	unbounded,	bi_lt(arg,arg,mod,desc)).
inlined_builtin(<,		3,	unbounded,	bi_lt(arg,arg,arg,desc)).
inlined_builtin(>,		3,	unbounded,	bi_gt(arg,arg,mod,desc)).
inlined_builtin(>,		3,	unbounded,	bi_gt(arg,arg,arg,desc)).
inlined_builtin(=<,		3,	unbounded,	bi_le(arg,arg,mod,desc)).
inlined_builtin(=<,		3,	unbounded,	bi_le(arg,arg,arg,desc)).
inlined_builtin(>=,		3,	unbounded,	bi_ge(arg,arg,mod,desc)).
inlined_builtin(>=,		3,	unbounded,	bi_ge(arg,arg,arg,desc)).
inlined_builtin(-,		2,	unbounded,	bi_minus(arg,uarg,desc)).
inlined_builtin(+,		3,	unbounded,	bi_addi(arg,int,uarg,desc)).
inlined_builtin(+,		3,	unbounded,	bi_add(arg,arg,uarg,desc)).
inlined_builtin(-,		3,	unbounded,	bi_sub(arg,arg,uarg,desc)).
inlined_builtin(*,		3,	unbounded,	bi_mul(arg,arg,uarg,desc)).
inlined_builtin(/,		3,	unbounded,	bi_quot(arg,arg,uarg,desc)).
inlined_builtin(//,		3,	unbounded,	bi_div(arg,arg,uarg,desc)).
inlined_builtin(rem,		3,	unbounded,	bi_rem(arg,arg,uarg,desc)).
inlined_builtin(div,		3,	unbounded,	bi_fdiv(arg,arg,uarg,desc)).
inlined_builtin(mod,		3,	unbounded,	bi_mod(arg,arg,uarg,desc)).
inlined_builtin(/\,		3,	unbounded,	bi_and(arg,arg,uarg,desc)).
inlined_builtin(\/,		3,	unbounded,	bi_or(arg,arg,uarg,desc)).
inlined_builtin(xor,		3,	unbounded,	bi_xor(arg,arg,uarg,desc)).
inlined_builtin(><,		3,	unbounded,	bi_xor(arg,arg,uarg,desc)).
inlined_builtin(\,		2,	unbounded,	bi_bitnot(arg,uarg,desc)).
inlined_builtin(arg,		3,	unbounded,	bi_arg(int,arg,uarg,desc)).
inlined_builtin(arg,		3,	unbounded,	bi_arg(arg,arg,uarg,desc)).
inlined_builtin(arity,		2,	unbounded,	bi_arity(arg,uarg,desc)).
inlined_builtin(get_bip_error,	1,	0,		bi_get_bip_error(uarg)).

% Encode argument descriptors in bitmask, 2 bits per argument
add_arg_desc(_,  arg, Desc, Desc).
add_arg_desc(I, uarg, Desc0, Desc) :- Desc is Desc0 + 1 << (2*(I-1)).
add_arg_desc(I,  int, Desc0, Desc) :- Desc is Desc0 + 2 << (2*(I-1)).
add_arg_desc(I,  mod, Desc0, Desc) :- Desc is Desc0 + 3 << (2*(I-1)).


% Tracing inlined builtins:
% A fundamental problem with generating a trace that is close to the source code
% is that for inlined predicates the arguments (especially output) do not exist
% at call time, and may well never exist (e.g. when the result unification is
% compiled into a get_structure-sequence). We could have a debug-compilation
% mode that generates completely different unoptimized code, but we are concerned
% here with generating a usable trace for optimized code, by just adding extra
% debug information.  Because of the output argument problem, we display those
% arguments at call ports as '...', but show them at exit ports.
% To generate the ports, we rely on the argument positions of the main instruction
% implementing the builtin. The debug_call/exit instructions have a reference
% that points *behind* this argument block, possibly terminated by an argument
% descriptor (desc).  If the instruction has no argument descriptor itself,
% we supply one in the debug instruction itself.
%
% Builtins without output arguments:
% 
% 	debug_call_simple ... -1 NArgs
%	bi_xxx  a1 ... an desc			e.g. >/2
% 	debug_exit_simple
% 
% 	debug_call_simple ... desc NArgs
%	bi_xxx  a1 ... an			e.g. atom/1
% 	debug_exit_simple
%
% Instructions with uninitialised output registers: the debug_exit instruction
% receives additional parameters, used to display output values at exit ports.
%
% L:	debug_call_simple ... -1 NArgs
%	bi_xxx  a1 ... uan desc			e.g. +/3, arg/3
% 	<possible output unifications>
%	debug_exit_simple ref(L)
%
% L:	debug_call_simple ... desc NArgs
%	bi_xxx  a1 ... uan			e.g. get_bip_error/1
% 	<possible output unifications>
%	debug_exit_simple ref(L)
%
% where NArgs indicates the number of arguments to the bi_xxx instruction,
% and a desc of -1 means that the bi_xxx instruction has its own desc.

emit_call_simple(BiInstr, NArgs, DbgArgDesc, RegDescs, Goal, Code, Code0, DbgLabel, options{debug:Debug}, Module) :-
	( Debug == off ->
	    Code = [code{instr:BiInstr,regs:RegDescs}|Code0]
	;
	    % CAUTION: the RegDescs must be given to the label instruction instead
	    % of the bi_xxx instructions to make sure that the register allocator
	    % (which may need to insert moves) puts all values in place *before*
	    % the debug instruction is executed, and to make sure nothing goes
	    % between the label and the actual debug_call_simple instruction.
	    Goal = goal{functor:Pred,lookup_module:LM,path:Path,line:Line,from:From,to:To},
	    ( LM\==Module -> QPred = LM:Pred ; QPred = Pred ),
	    Code = [
		    code{instr:label(DbgLabel),regs:RegDescs},
		    code{instr:debug_call_simple(QPred,#call_port,Path,Line,From,To,DbgArgDesc,NArgs)},
		    code{instr:BiInstr}|Code0]
	).


emit_exit_simple(InstrTmpl, DbgLabel, Code, Code0, options{debug:Debug}) :-
	( Debug == off ->
	    Code = Code0
	; (foreacharg(Arg,InstrTmpl) do Arg \= uarg) ->
	    Code = [code{instr:debug_exit_simple}|Code0]
	;
	    % The instruction has uninitialised output arguments: Improve the
	    % trace for this special case by giving extra parameters to
	    % debug_exit_simple that allow "patching" the debug-exit frame.
	    Code = [code{instr:debug_exit_simple(0/*unused*/,ref(DbgLabel))}|Code0]
	).

emit_debug_noarg(Goal, CallCode, CallCode0, ExitCode, ExitCode0, options{debug:Debug}, Module) :-
	( Debug == off ->
	    CallCode = CallCode0, ExitCode = ExitCode0
	;
	    Goal = goal{functor:Pred,lookup_module:LM,path:Path,line:Line,from:From,to:To},
	    ( LM == sepia_kernel ->
		% If the LM is sepia_kernel, it is most likely a goal that was
		% inserted by the normalisation phase - don't trace it!
		CallCode = CallCode0, ExitCode = ExitCode0
	    ;
		% Create debug instructions with all arguments hidden (by
		% pretending they are all uargs - hack)
		( LM\==Module -> QPred = LM:Pred ; QPred = Pred ),
		Pred = _/N,
		( for(I,1,N), fromto(0,ArgDesc1,ArgDesc2,ArgDesc) do
		    add_arg_desc(I, uarg, ArgDesc1, ArgDesc2)
		),
		CallCode = [code{instr:debug_call_simple(QPred,#call_port,Path,Line,From,To,ArgDesc,0),regs:[]}|CallCode0],
		ExitCode = [code{instr:debug_exit_simple}|ExitCode0]
	    )
	).


%----------------------------------------------------------------------
% The special case of =/2 goal
% Due to normalisation, all should be in the form Var=Term!
% Fail if no special treatment possible (shouldn't happen)

generate_unify(Arg1, Arg2, ChunkData0, ChunkData, Code0, Code, _Module) :-
	Arg1 = variable{varid:VarId1},
	Arg2 = variable{varid:VarId2},
	!,
	( VarId1 = VarId2 ->
	    ChunkData = ChunkData0, Code0 = Code
	;
	    variable_occurrence(Arg1, ChunkData0, ChunkData1, Code0, Code1, VarOccDesc1),
	    variable_occurrence(Arg2, ChunkData1, ChunkData2, Code1, Code2, VarOccDesc2),
	    unify_variables(VarOccDesc1, VarId1, VarOccDesc2, VarId2, Code2, Code3, GAlloc),
	    alloc_check_after(GAlloc, ChunkData2, ChunkData, Code3, Code)
	).
generate_unify(Arg1, Arg2, ChunkData0, ChunkData, Code0, Code, Module) :-
	Arg1 = variable{varid:VarId},
	!,
	variable_occurrence(Arg1, ChunkData0, ChunkData1, Code0, Code1, VarOccDesc),
	bind_variable(VarOccDesc, VarId, Arg2, ChunkData1, ChunkData, Code1, Code, Module).
generate_unify(Arg1, Arg2, ChunkData0, ChunkData, Code0, Code, Module) :-
	Arg2 = variable{},
	!,
	generate_unify(Arg2, Arg1, ChunkData0, ChunkData, Code0, Code, Module).
generate_unify(Arg1, Arg2, ChunkData0, ChunkData, Code0, Code, _Module) :-
	atomic(Arg1), atomic(Arg2), !,
	ChunkData0 = ChunkData,
	( Arg1 = Arg2 ->
	    Code0 = Code
	;
	    Code0 = [code{instr:failure,regs:[]}|Code]
	).
generate_unify(_Arg1, _Arg2, _ChunkData0, _ChunkData, _Code0, _Code, _Module) :-
	writeln(warning_output,
	    "WARNING: nonvar = nonvar unification should be unwrapped by preprocessing"),
	fail.

%
% Generate code for the unification of a variable and a nonvariable.
%
% bind_variable(VarOccDesc, VarId, Term, ChunkData0, ChunkData, Code, Code0, Module)
%

bind_variable(void, _VarId, _Term, ChunkData, ChunkData, Code, Code, _Module).
bind_variable(tmp_first, VarId, Term, ChunkData0, ChunkData, Code, Code0, Module) :-
	body(VarId, Term, ChunkData0, ChunkData, Code, Code0, Module).
bind_variable(tmp, VarId, Term, ChunkData0, ChunkData, Code, Code0, _Module) :-
	head(VarId, Term, ChunkData0, ChunkData, Code, Code0).
bind_variable(perm_first(Y), VarId, Term, ChunkData0, ChunkData, Code, Code0, Module) :-
	Code1 = [code{instr:move(R,Y),regs:[r(VarId,R,use_a,_),r(VarId,Y,perm,_)]}|Code0],
	body(VarId, Term, ChunkData0, ChunkData, Code, Code1, Module).
bind_variable(perm_first_in_chunk(Y), VarId, Term, ChunkData0, ChunkData, Code, Code0, _Module) :-
	Code = [code{instr:nop,regs:[r(VarId,Y,perm,_)]}|Code1],
	head(VarId, Term, ChunkData0, ChunkData, Code1, Code0).
bind_variable(perm(_Y), VarId, Term, ChunkData0, ChunkData, Code, Code0, _Module) :-
	head(VarId, Term, ChunkData0, ChunkData, Code, Code0).


%
% Generate code for unifying the value in register R with Arg
%

unify_result(Arg, R, RegDesc, ChunkData0, ChunkData, Code0, Code, GAlloc) :-
	Arg = variable{varid:VarId}, !,
	variable_occurrence(Arg, ChunkData0, ChunkData1, Code0, Code1, VarOccDesc),
	( VarOccDesc = void ->
	    RegDesc = r(VarId,R,def,_last),
	    ChunkData1 = ChunkData, Code1 = Code, GAlloc = 0
	; VarOccDesc = tmp_first ->
	    RegDesc = r(VarId,R,def,_),
	    ChunkData1 = ChunkData, Code1 = Code, GAlloc = 0
	; VarOccDesc = tmp ->
	    new_aux_temp(ChunkData1, ChunkData, TVarId),
	    RegDesc = r(TVarId,R,def,_), GAlloc = unbounded,
	    Code1 = [code{instr:get_value(R1,R2), regs:[r(TVarId,R1,use,_),r(VarId,R2,use,_)]}|Code]
	; VarOccDesc = perm(_Y) ->
	    new_aux_temp(ChunkData1, ChunkData, TVarId),
	    RegDesc = r(TVarId,R,def,_), GAlloc = unbounded,
	    Code1 = [code{instr:get_value(R1,RY2), regs:[r(TVarId,R1,use,_),r(VarId,RY2,use,_)]}|Code]
	; VarOccDesc = perm_first(Y) ->
	    new_aux_temp(ChunkData1, ChunkData, TVarId),
	    RegDesc = r(TVarId,R,def,_), GAlloc = 0,
	    Code1 = [code{instr:move(R1,Y), regs:[r(TVarId,R1,use,_),r(VarId,Y,perm,_)]}|Code]
	; VarOccDesc = perm_first_in_chunk(Y) ->
	    new_aux_temp(ChunkData1, ChunkData, TVarId),
	    RegDesc = r(TVarId,R,def,_), GAlloc = unbounded,
	    Code1 = [code{instr:get_value(R1,Y), regs:[r(TVarId,R1,use,_),r(VarId,Y,perm,_)]}|Code]
	;
	    unreachable("unify_result")
	).
unify_result(Arg, R, RegDesc, ChunkData0, ChunkData, Code0, Code, 0) :-
	new_aux_temp(ChunkData0, ChunkData1, TVarId),
	RegDesc = r(TVarId,R,def,_),
	head(TVarId, Arg, ChunkData1, ChunkData, Code0, Code).


%
% Generate code for the unification of two variables. Each variable is in one of
% the following states, according to its Variable Occurrence Descriptor:
%
%	tmp
%	tmp_first
%	void
%	perm(y(Y))
%	perm_first(y(Y))
%	perm_first_in_chunk(y(Y))
%

unify_variables(VarOccDesc1, VarId1, VarOccDesc2, VarId2, Code, Code0, GAlloc) :-
	% order the two descriptors, so we need only half a matrix below
	( VarOccDesc1 @> VarOccDesc2 ->
	    unify_variables_ord(VarOccDesc2, VarId2, VarOccDesc1, VarId1, Code, Code0, GAlloc)
	;
	    unify_variables_ord(VarOccDesc1, VarId1, VarOccDesc2, VarId2, Code, Code0, GAlloc)
	).

% PRE: VarOccDesc1 @=< VarOccDesc2
:- mode unify_variables_ord(+,+,+,+,-,?,-).
unify_variables_ord(tmp, VarId1, tmp, VarId2, Code, Code0, unbounded) :- !,
	Code = [code{instr:get_value(R1,R2), regs:[r(VarId1,R1,use,_),r(VarId2,R2,use,_)]}
		|Code0].
unify_variables_ord(tmp, VarId1, tmp_first, VarId2, Code, Code0, 0) :- !,
	Code = [code{instr:move(R1,R2), regs:[r(VarId1,R1,use,_),r(VarId2,R2,def,_)]}
		|Code0].
unify_variables_ord(tmp, _VarId1, void, _VarId2, Code, Code, 0) :- !.
unify_variables_ord(tmp, VarId1, perm(_), VarId2, Code, Code0, unbounded) :- !,
	Code = [code{instr:get_value(R1,RY2), regs:[r(VarId1,R1,use,_),r(VarId2,RY2,use,_)]}
		|Code0].
unify_variables_ord(tmp, VarId1, perm_first(Y2), VarId2, Code, Code0, 0) :- !,
	Code = [code{instr:move(R1,Y2), regs:[r(VarId1,R1,use,_),r(VarId2,Y2,perm,_)]}
		|Code0].
unify_variables_ord(tmp, VarId1, perm_first_in_chunk(Y2), VarId2, Code, Code0, unbounded) :- !,
	Code = [code{instr:get_value(R1,Y2), regs:[r(VarId1,R1,use,_),r(VarId2,Y2,perm,_)]}
		|Code0].

unify_variables_ord(tmp_first, VarId1, tmp_first, VarId2, Code, Code0, 1) :- !,
	Code = [code{instr:put_variable(R1), regs:[r(VarId1,R1,def,_)]},
		code{instr:move(R11,R2), regs:[r(VarId1,R11,use_a,_),r(VarId2,R2,def,_)]}
		|Code0].
unify_variables_ord(tmp_first, VarId1, void, _VarId2, Code, Code0, 1) :- !,
	Code = [code{instr:put_variable(R1), regs:[r(VarId1,R1,def,_)]}
		|Code0].
unify_variables_ord(tmp_first, VarId1, perm(Y2), _VarId2, Code, Code0, 0) :- !,
%	Code = [code{instr:move(RY2,R1), regs:[r(_VarId2,RY2,use,_),r(VarId1,R1,def,_)]}|Code0].
	Code = [code{instr:nop, regs:[r(VarId1,Y2,perm,_)]} |Code0].
unify_variables_ord(tmp_first, VarId1, perm_first(Y2), VarId2, Code, Code0, 1) :- !,
	Code = [code{instr:put_global_variable(R1,Y2), regs:[r(VarId2,Y2,perm,_),r(VarId1,R1,def,_)]}
		|Code0].
unify_variables_ord(tmp_first, VarId1, perm_first_in_chunk(Y2), VarId2, Code, Code0, 0) :- !,
%	Code = [code{instr:move(Y2,R1), regs:[r(VarId2,Y2,perm,_),r(VarId1,R1,def,_)]}|Code0].
	Code = [code{instr:nop, regs:[r(VarId1,Y2,perm,_),r(VarId2,Y2,perm,_)]} |Code0].

unify_variables_ord(void, _VarId1, void, _VarId2, Code, Code, 0) :- !.
unify_variables_ord(void, _VarId1, perm(_), _VarId2, Code, Code, 0) :- !.
unify_variables_ord(void, _VarId1, perm_first(Y2), VarId2, Code, Code0, 1) :- !,
	Code = [code{instr:put_global_variable(Y2), regs:[r(VarId2,Y2,perm,_)]}
		|Code0].
unify_variables_ord(void, _VarId1, perm_first_in_chunk(Y2), VarId2, Code, Code0, 0) :- !,
	Code = [code{instr:nop, regs:[r(VarId2,Y2,perm,_)]} |Code0].

unify_variables_ord(perm(_Y1), VarId1, perm(_Y2), VarId2, Code, Code0, unbounded) :- !,
	Code = [code{instr:get_value(RY1,RY2), regs:[r(VarId1,RY1,use,_),r(VarId2,RY2,use,_)]}
		|Code0].
unify_variables_ord(perm(_Y1), VarId1, perm_first(Y2), VarId2, Code, Code0, 0) :- !,
	Code = [code{instr:move(RY1,Y2), regs:[r(VarId1,RY1,use,_),r(VarId2,Y2,perm,_)]}
		|Code0].
unify_variables_ord(perm(_Y1), VarId1, perm_first_in_chunk(Y2), VarId2, Code, Code0, unbounded) :- !,
	Code = [code{instr:get_value(RY1,Y2), regs:[r(VarId1,RY1,use,_),r(VarId2,Y2,perm,_)]}
		|Code0].

unify_variables_ord(perm_first(Y1), VarId1, perm_first_in_chunk(Y2), VarId2, Code, Code0, 0) :- !,
	Code = [code{instr:move(Y2,Y1), regs:[r(VarId2,Y2,perm,_),r(VarId1,Y1,perm,_)]}
		|Code0].
unify_variables_ord(perm_first(Y1), VarId1, perm_first(Y2), VarId2, Code, Code0, 1) :- !,
	Code = [code{instr:put_global_variable(Y1), regs:[r(VarId1,Y1,perm,_)]},
		code{instr:move(Y1,Y2), regs:[r(VarId2,Y2,perm,_)]}
		|Code0].

unify_variables_ord(perm_first_in_chunk(Y1), VarId1, perm_first_in_chunk(Y2), VarId2, Code, Code0, unbounded) :- !,
	Code = [code{instr:get_value(Y1,Y2), regs:[r(VarId1,Y1,perm,_),r(VarId2,Y2,perm,_)]}
		|Code0].



%
% Implementation of one way unification (head matching)
% The Arg1 ?= Arg2 goal is only created during head normalisation,
% so only certain special cases occur.
%
% p(nonvar) ?- ...	was normalised into p(T) :- T ?= nonvar, ...
%	and T ?= nonvar implemented via specialised head unification code
%
% p(X,X) ?- ...		was normalised into p(X,T) :- X=X, X==T, ...
%
% p(X{A}) ?- ...	was normalised into p(X) :- X?=X{A}, ...
% p(X{A},X{A}) ?- ...	was normalised into p(X,T) :- X?=X{A}, T==X, ...
%
generate_in_unify(Arg1, Arg2, ChunkData0, ChunkData, Code0, Code) :-
	Arg1 = variable{varid:VarId},
	( Arg2 = variable{} ->
	    verify false
    	;
	    variable_occurrence(Arg1, ChunkData0, ChunkData1, Code0, Code1, VarOccDesc),
	    verify VarOccDesc == tmp,
	    in_head(VarId, Arg2, ChunkData1, ChunkData, Code1, Code)
	).


%
% Identity ==/2
% implement some cases via in_get_xxx instructions, otherwise fail
%

generate_identity(Arg1, Arg2, ChunkData0, ChunkData, Code0, Code) :-
	atomic(Arg1),
	Arg2 = variable{},
	!,
	generate_identity(Arg2, Arg1, ChunkData0, ChunkData, Code0, Code).
generate_identity(Arg1, Arg2, ChunkData0, ChunkData, Code0, Code) :-
	Arg1 = variable{varid:VarId},
	atomic(Arg2),
	!,
	put_variable(Arg1, ChunkData0, ChunkData, Code0, Code1),
	Code1 = [code{instr:Instr,regs:[r(VarId,RI,use_a,_)]}|Code],
	in_get_const(RI, Arg2, Instr).
generate_identity(Arg1, Arg2, ChunkData0, ChunkData, Code0, Code) :-
	atomic(Arg1), atomic(Arg2),
	ChunkData0 = ChunkData,
	( Arg1 = Arg2 ->
	    Code0 = Code
	;
	    Code0 = [code{instr:failure,regs:[]}|Code]
	).



%----------------------------------------------------------------------
% Generate code for constructing an arbitrary term
%----------------------------------------------------------------------

put_term(Term, ChunkData0, ChunkData, Code, Code0, VarId, _Module) :-
	Term = variable{varid:VarId}, !,
	put_variable(Term, ChunkData0, ChunkData, Code, Code0).
put_term(Term, ChunkData0, ChunkData, Code, Code0, ValId, Module) :-
	new_aux_temp(ChunkData0, ChunkData1, ValId),
	body(ValId, Term, ChunkData1, ChunkData, Code, Code0, Module).


%
% Generate code that makes sure that a variable physically exists
% (it might need to be initialised if it is the first occurrence)
% and its location is available somewhere (register or env slot).
% Generate register annotations to tell the reg allocator about
% the location. A concrete register (plus possibly extra move
% instructions) will be assigned by the reg allocator later.
%
% put_variable(+VarDesc, +ChunkData0, -ChunkData, -Code, ?Code0).
%

:- mode put_variable(+,+,-,-,?).

put_variable(Var, ChunkData0, ChunkData, Code0, Code) :-
	Var = variable{varid:VarId},
	variable_occurrence(Var, ChunkData0, ChunkData1, Code0, Code1, VarOccDesc),
	put_va_code(VarOccDesc, VarId, Code1, Code, GAlloc),
	alloc_check_pwords(GAlloc, ChunkData1, ChunkData).

    put_va_code(void, VarId, Code, Code0, 1) :-
	Code = [code{instr:put_variable(R),regs:[r(VarId,R,def,_)]}|Code0].
    put_va_code(tmp_first, VarId, Code, Code0, 1) :-
	Code = [code{instr:put_variable(R),regs:[r(VarId,R,def,_)]}|Code0].
    put_va_code(tmp, _VarId, Code, Code0, 0) :-
	% Variable already known in this chunk: The register allocator will
	% move it to the correct register as necessary (triggered by the dest
	% descriptor that comes with the call instruction).
	Code = Code0.
    put_va_code(perm_first(Y), VarId, Code, Code0, 1) :-
	% First ever occurrence of this permanent variable. Emit code to
	% initialise it and tell the reg allocator about the two locations.
	Code = [code{instr:put_global_variable(R,Y),regs:[r(VarId,Y,perm,_),r(VarId,R,def,_)]}|Code0].
    put_va_code(perm_first_in_chunk(Y), VarId, Code, Code0, 0) :-
	% First occurrence of this permanent variable in this chunk.
	% Tell the reg allocator about the permanent location. It will then
	% move it to the correct register as necessary (triggered by the dest
	% descriptor that comes with the call instruction).
	Code = [code{instr:nop,regs:[r(VarId,Y,perm,_)]}|Code0].
    put_va_code(perm(_Y), _VarId, Code, Code0, 0) :-
	% Variable already known in this chunk. The register allocator will
	% move it to the correct register as necessary.
	Code = Code0.


% Generate code to move head occurrences of permanent variables
% into their environment slots.
move_head_perms([], ChunkData, ChunkData, Code, Code) :- !.
move_head_perms(HeadPerms, ChunkData0, ChunkData, Code0, Code) :-
	env_allocate_if_needed(1/*dummy*/, ChunkData0, ChunkData, Code0, Code1),
	(
	    foreach(delayed_move(VarId,Y),HeadPerms),
	    fromto(Code1,[Move|Code2],Code2,Code)
	do
	    Move = code{instr:move(R,Y),regs:[r(VarId,R,use,_),r(VarId,Y,perm,_)]}
	).


%----------------------------------------------------------------------
% Global stack allocation checks
%
% We distinguish the following points in the WAM code:
%
% start: a point where we are guaranteed to have the standard margin
%	available on the global stack (this is the case at predicate
%	entry or after returning from a regular call).
%
% allocation(maximum):
%	a (potential) allocation point, we know the maximum used.
%	These are instructions like put_structure, write_list, etc
%	We insert no checks at these points.
%
% after_unbounded_alloc(certainly/maybe reached):
%	a (potential) unbounded allocation point, after which we
%	have no guarantee except the standard margin (either we
%	have the same as before, or standard margin). Examples are:
%	- get_value (because of attributed variable unification, which
%	  builds up the MU-list). It is certainly reached.
%	- read_value, same as get_value, but not certainly reached.
%	- arithmetic builtins, because of bignums+rationals
%	We may need to insert a check after this (potential) allocation.
%
% split: before a disjunction - we promote the max allocation
%	requirement of the branches left over split-point.

% start_branch(det):
%	promote check to the left. This is used for the first branch,
%	or all branches in case of deterministic switch.
%
% start_branch(nondet):
%	If less than margin needed, promote check to the left (because
%	we may enter the branch directly via switch). If more than
%	margin needed, insert check here (because we may enter
%	via retry/trust and have only guarantee for standard margin),
%	and promote nothing left.
%
% end: a point where an implicit check follows (call, ret, ...)
%
%
%
% This code uses delayed goals to fill in the size-arguments in the
% gc_test instructions once they become known.  This results in lots
% of gc_test <small> which must be removed later, but it does not
% leave gaps in the code, which is a bit nicer for debugging.
% Note that this code is independent of the chunk structure.
%----------------------------------------------------------------------

alloc_check_pwords(0, ChunkData0, ChunkData) :- !,
	ChunkData = ChunkData0.
alloc_check_pwords(N, ChunkData0, ChunkData) :-
	ChunkData0 = chunk_data{need_global:N0},
	suspend(+(N1,N,N0), 0, N1->inst),
	update_struct(chunk_data, [need_global:N1], ChunkData0, ChunkData).

alloc_check_start(ChunkData0, ChunkData, [code{instr:gc_test(N)}|Code0], Code0) :-
	update_struct(chunk_data, [need_global:N], ChunkData0, ChunkData).

delay alloc_check_split(_, Ms) if nonground(Ms).
alloc_check_split(chunk_data{need_global:N}, Ms) :-
	N is max(Ms).

alloc_check_start_branch(Det, ChunkData0, ChunkData, Code, Code0, NeedBefore) :-
	( Det == det ->
	    % we have come here directly via switch from code before,
	    % so promote left.
	    Code = Code0,
	    update_struct(chunk_data, [need_global:NeedBefore], ChunkData0, ChunkData)
	;
	    % we have to promote left because we have no guarantee here:
	    % we may have come here directly via switch from code before
	    % (-> rely on left-promoted amount), or we may have had a
	    % retry/trust (-> we can rely on standard margin).
	    Code = [code{instr:gc_test(N)}|Code0],
	    update_struct(chunk_data, [need_global:NeedAfter], ChunkData0, ChunkData),
	    suspend(test_or_promote(reached, NeedAfter, NeedBefore, N), 0, NeedAfter->inst)
	).

alloc_check_join(_ChunkDataEs, _).	% disabled
%alloc_check_join(ChunkDataEs, chunk_data{need_global:N}) ?-
%	( foreach(chunk_data{need_global:N},ChunkDataEs), param(N) do true ).


% N is integer, 'unbounded' or 'unbounded_maybe'
alloc_check_after(N, ChunkData0, ChunkData, Code, Code) :-
	integer(N), !,
	alloc_check_pwords(N, ChunkData0, ChunkData).
alloc_check_after(UnbReach, ChunkData0, ChunkData, [code{instr:gc_test(N)}|Code0], Code0) :- !,
	ChunkData0 = chunk_data{need_global:NeedBefore},
	update_struct(chunk_data, [need_global:NeedAfter], ChunkData0, ChunkData),
	suspend(test_or_promote(UnbReach, NeedAfter, NeedBefore, N), 0, NeedAfter->inst).

    % we are just after a potentially unbounded allocation+check
    test_or_promote(UnbReach, NeedAfter, NeedBefore, TestHere) :-
	( NeedAfter > #wam_max_global_push ->
	    ( UnbReach == unbounded_maybe ->
		% since the check might not be reached,
		% check for enough space in the previous test
		TestHere=NeedAfter, NeedBefore=NeedAfter
	    ;
		% since the check is certainly reached,
		% it has the responsibility for NeedAfter
		TestHere=NeedAfter, NeedBefore=0
	    )
	;
	    % no check needed because either:
	    % - no allocate&check, previous check covers
	    % - after-bip, bip doesn't allocate, and previous check covers
	    % - after-bip, bip allocates&checks, and we have Guarantee
	    TestHere=0, NeedBefore=NeedAfter
	).

alloc_check_end(chunk_data{need_global:0}).


%----------------------------------------------------------------------
% Debugging and testing
%----------------------------------------------------------------------

:- comment(print_annotated_code/1, [
    summary:"Debugging: print annotated WAM code",
    amode:print_annotated_code(+),
    args:[
	"Code":"A list of struct(code)"
    ],
    see_also:[generate_code/5,struct(code)]
]).

:- export print_annotated_code/1.

print_annotated_code(Code) :-
	writeln("------ Code ------"),
	( fromto(Code,Code1,Code4,[]) do
	    ( fromto(Code1,[InstrDesc|Code2],Code3,next(Code4)) do
		( InstrDesc = code{instr:Instr,regs:Regs,comment:C} ->
		    ( Instr = label(_) ->
			printf("%Vw%t", [Instr])
		    ;
			printf("%t%Vw", [Instr])
		    ),
		    ( nonvar(Regs) -> printf("%t%t%_w", [Regs]) ; true ),
		    ( nonvar(C) -> printf("%t%% %Vw", [C]) ; true ),
		    nl
		;
		    ( InstrDesc = label(_) ->
			printf("%Vw%n", [InstrDesc])
		    ;
			printf("%t%Vw%n", [InstrDesc])
		    )
		),
		% allow termination by [] or next([])
		( Code2 == [] -> Code3 = next([]) ; Code3 = Code2 )
	    )
	).

