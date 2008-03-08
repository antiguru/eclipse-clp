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
% Version:	$Id: compiler_codegen.ecl,v 1.10 2008/03/08 02:20:58 jschimpf Exp $
% ----------------------------------------------------------------------

:- module(compiler_codegen).

:- comment(summary, "ECLiPSe III compiler - code generation").
:- comment(copyright, "Cisco Technology Inc").
:- comment(author, "Joachim Schimpf").
:- comment(date, "$Date: 2008/03/08 02:20:58 $").


:- lib(hash).

:- use_module(compiler_common).
:- use_module(compiler_regassign).

:- include(compiler_compound).


%----------------------------------------------------------------------
% Chunk data
% This data structure holds information that evolves along the chunk.
%----------------------------------------------------------------------

:- local struct(chunk_data(
	occurred,		% hash table varid->bool
	aux_count		% number of auxiliary temporaries
    )).


init_chunk_data(chunk_data{aux_count:0,occurred:Init}) :-
	hash_create(Init).

print_chunk_data(_,_).


%
% Registers a variable occurrence within a chunk and
% returns a "variable occurrence descriptor" of the form:
%
%	void
%	tmp_first
%	tmp
%	perm_first(y(Y))
%	perm_first_in_chunk(y(Y))
%	perm(y(Y))
%

variable_occurrence(variable{varid:VarId,isafirst:FirstChunk,class:Class},
		ChunkData, ChunkData, Descriptor) :-
	ChunkData = chunk_data{occurred:OccurredInChunk},
	variable_occurrence1(Class, FirstChunk, VarId, OccurredInChunk, Descriptor).

    variable_occurrence1(void, FirstChunk, _VarId, _OccurredInChunk, Descriptor) :- !,
    	Descriptor = void,
	verify FirstChunk == first.
    variable_occurrence1(nonvoid(y(Y)), FirstChunk, VarId, OccurredInChunk, Descriptor) :- !,
	( hash_get(OccurredInChunk, VarId, Type) ->
	    ( Type == delayed_perm ->
		Descriptor = tmp
	    ;
		Descriptor = perm(y(Y))
	    )
	;
	    hash_set(OccurredInChunk, VarId, true),
	    ( FirstChunk == first ->
		Descriptor = perm_first(y(Y))
	    ;
		Descriptor = perm_first_in_chunk(y(Y))
	    )
	).
    variable_occurrence1(nonvoid(_Tmp), _FirstChunk, VarId, OccurredInChunk, Descriptor) :-
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

:- comment(generate_code/6, [
    summary:"Generate WAM code from normalised source for one predicate",
    amode:generate_code(+,+,-,?,+,+),
    args:[
	"Body":"Normalised and fully annotated source of the predicate",
	"EnvSize":"Environment size needed",
	"Code":"Head of resulting annotated code",
	"CodeEnd":"Tail of resulting annotated code",
	"Options":"Options structure",
	"ModulePred":"Context module and Name/Arity"
    ],
    see_also:[assign_am_registers/3,struct(code)]
]).

:- export generate_code/6.

generate_code(Clause, EnvSize, Code, CodeEnd, Options, ModPred) :-
	Code = [code{instr:label(Start)}|Code0],
	( EnvSize >= 0 ->
	    Code0 = [code{instr:allocate(EnvSize)}|Code1],
	    Code4 = [code{instr:exit}|AuxCode]
	;
	    Code0 = Code1,
	    Code4 = [code{instr:ret}|AuxCode]
	),
	generate_branch(Clause, 0, AuxCode, CodeEnd, Code1, Code4, Options, ModPred@Start).


generate_branch(Goals, BranchExitInitMap, AuxCode0, AuxCode, Code0, Code, Options, SelfInfo) :-
	(
	    fromto(Goals,ThisChunk,NextChunk,[]),
	    fromto(Code0,Code1,Code2,Code3),
	    fromto(AuxCode0,AuxCode1,AuxCode2,AuxCode),
	    param(Options,SelfInfo)
	do
	    init_chunk_data(ChunkData0),
	    generate_chunk(ThisChunk, NextChunk, [], ChunkData0, AuxCode1, AuxCode2, ChunkCode, ChunkCode, Code1, Code2, 0, _ArityUsed, Options, SelfInfo)
	),
	% Generate initialization code for any variables which did not occur
	% in or before the branch, but have a non-first occurrence after it.
	emit_initialize(BranchExitInitMap, Code3, Code).


:- mode generate_chunk(+,-,+,+,?,-,-,?,-,?,+,-,+,+).
generate_chunk([], [], HeadPerms, _ChunkData, AuxCode, AuxCode, AllChunkCode, Code, FinalCode, FinalCode0, Arity, Arity, _Options, _Module) :-
	% end of chunk, finalize the code
	move_head_perms(HeadPerms, Code, []),
	assign_am_registers(AllChunkCode, FinalCode, FinalCode0).

generate_chunk([Goal|Goals], NextChunk, HeadPerms0, ChunkData0, AuxCode, AuxCode0, AllChunkCode, Code, FinalCode, FinalCode0, MaxArity0, MaxArity, Options, SelfInfo) :-

	( Goal = goal{kind:simple} ->		 % special goals
	    generate_simple_goal(Goal, ChunkData0, ChunkData1, Code, Code1),
	    generate_chunk(Goals, NextChunk, HeadPerms0, ChunkData1, AuxCode, AuxCode0, AllChunkCode, Code1, FinalCode, FinalCode0, MaxArity0, MaxArity, Options, SelfInfo)

	; Goal = goal{kind:head,functor:(_/HeadArity),args:Args} ->	% clause-head or pseudo-head
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
	    ),
	    Code = [code{instr:nop,regs:OrigRegDescs}|Code1],
	    generate_chunk(Goals, NextChunk, HeadPerms3, ChunkData3, AuxCode, AuxCode0, AllChunkCode, Code1, FinalCode, FinalCode0, HeadArity, MaxArity, Options, SelfInfo)

	; Goal = goal{kind:regular,functor:true/0,definition_module:sepia_kernel}, (Goals = [] ; Goals = [goal{kind:regular}|_] ) ->
	    % Normally, true/0 should be eliminated in the normalisation phase.
	    % But due to its legacy semantics (it is a regular goal and can
	    % cause waking), we only eliminate it here when it occurs at the
	    % end of a branch or just before another regular goal.
	    generate_chunk(Goals, NextChunk, HeadPerms0, ChunkData0, AuxCode, AuxCode0, AllChunkCode, Code, FinalCode, FinalCode0, MaxArity0, MaxArity, Options, SelfInfo)

	; Goal = goal{kind:regular,functor:P,lookup_module:LM,envmap:EAM} ->
	    P = _/CallArity,
	    MaxArity is max(MaxArity0,CallArity),
	    move_head_perms(HeadPerms0, Code, Code1),
	    generate_regular_puts(Goal, ChunkData0, _ChunkData, Code1, Code2, OutArgs),
	    SelfInfo = Module:Self@SelfLab,
	    ( LM\==Module ->
	    	Pred = LM:P, Dest = Pred	% calling non-visible
	    ; P==Self ->
	    	Pred = P, Dest = ref(SelfLab)	% direct recursive call
	    ;
	    	Pred = P, Dest = Pred		% calling visible pred
	    ),
	    emit_debug_call_port(Options, Pred, OutArgs, OutArgs1, Goal, Code2, Code3),
	    Code3 = [
		% PRELIMINARY: always use callf instead of call (to reset DET flag)
		code{instr:callf(Dest,eam(EAM)),regs:OutArgs1}],
	    NextChunk = Goals,
	    AuxCode = AuxCode0,
	    % end of chunk, finalize the code
	    assign_am_registers(AllChunkCode, FinalCode, FinalCode0)

	; Goal = indexpoint{indexes:IndexDescs,envmap:EAM,nextaltlabel:Label2,
			disjunction:disjunction{arity:TryArity,branchlabels:BranchLabelArray,branchentrymaps:BranchEamArray}} ->
	    MaxArity1 is max(MaxArity0,TryArity),
	    generate_indexing(IndexDescs, BranchLabelArray, BranchEamArray, TryArity, ChunkData0, ChunkData1, Code, Code2, AuxCode, AuxCode1),
	    arg(1, BranchLabelArray, BrLabel1),
	    Code2 = [code{instr:try_me_else(0,TryArity,ref(Label2)),regs:[]},
		    code{instr:label(BrLabel1),regs:[]}|Code3],
	    generate_chunk(Goals, NextChunk, HeadPerms0, ChunkData1, AuxCode1, AuxCode0, AllChunkCode, Code3, FinalCode, FinalCode0, MaxArity1, MaxArity, Options, SelfInfo)

	; Goal = disjunction{branches:Branches,branchlabels:BranchLabelArray,
		entrymap:EAM,arity:TryArity,
		branchentrymaps:BranchEamArray,
		branchinitmaps:BranchExitInits,
		index:indexpoint{nextaltlabel:Label2}} ->

	    length(Branches, NBranches),
	    MaxArity is max(MaxArity0,TryArity),
	    NextChunk = Goals,

	    % end of chunk, finalize the code (auxiliary code doesn't need registers assigned)
	    move_head_perms(HeadPerms0, Code, []),
	    assign_am_registers(AllChunkCode, FinalCode, Code1),

	    Branches = [Branch1|Branches2toN],
	    BranchExitInits = [BranchExitInit1|BranchExitInits2toN],
	    % indexing and try_me are generated inside the 1st branch,
	    % triggered by indexpoint{} pseudo-goal
	    generate_branch(Branch1, BranchExitInit1, AuxCode, AuxCode2, Code1, Code2, Options, SelfInfo),
	    Code2 = [code{instr:branch(ref(LabelJoin)),regs:[]}|Code3],
	    (
		for(I, 2, NBranches-1),
		fromto(Branches2toN, [Branch|Branches], Branches, [BranchN]),
		fromto(BranchExitInits2toN, [BranchExitInit|BEIs], BEIs, [BranchExitInitN]),
		fromto(Code3, Code4, Code7, Code8),
		fromto(AuxCode2, AuxCode3, AuxCode4, AuxCode5),
		fromto(Label2, LabelI, LabelI1, LabelN),
		param(LabelJoin,BranchLabelArray,BranchEamArray,Options,SelfInfo)
	    do
		arg(I, BranchLabelArray, BrLabelI),
		arg(I, BranchEamArray, EAM),
		Code4 = [
		    code{instr:label(LabelI),regs:[]},
		    code{instr:retry_me_inline(0,ref(LabelI1),eam(EAM)),regs:[]},
		    code{instr:label(BrLabelI),regs:[]}
		    |Code5],
		generate_branch(Branch, BranchExitInit, AuxCode3, AuxCode4, Code5, Code6, Options, SelfInfo),
		Code6 = [code{instr:branch(ref(LabelJoin)),regs:[]}|Code7]
	    ),
	    arg(NBranches, BranchLabelArray, BrLabelN),
	    arg(NBranches, BranchEamArray, EAMN),
	    Code8 = [
		code{instr:label(LabelN),regs:[]},
		code{instr:trust_me_inline(0,eam(EAMN)),regs:[]},
		code{instr:label(BrLabelN),regs:[]}
		|Code9],
	    generate_branch(BranchN, BranchExitInitN, AuxCode5, AuxCode0, Code9, Code10, Options, SelfInfo),
	    Code10 = [code{instr:label(LabelJoin),regs:[]}|FinalCode0]

	;
	    printf(error, "ERROR: unexpected goal in generate_chunk", []),
	    abort
	).


% Optionally generate a call-port debug instruction
emit_debug_call_port(options{debug:off}, _Pred, OutArgs, OutArgs, _Goal, Code, Code) :- !.
emit_debug_call_port(options{debug:on}, Pred, OutArgs, [], goal{path:Path,line:Line,from:From,to:To}, Code, Code0) :-
	(var(Path) -> Path1 = ''; concat_atom([Path],Path1)),
	(var(Line) -> Line = 0 ; true),
	(var(From) -> From = 0 ; true),
	(var(To) -> To = 0 ; true),
	Code = [code{instr:debug_scall(Pred,1,Path1,Line,From,To),regs:OutArgs}|Code0].


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

generate_indexing(IndexDescs, BranchLabelArray, BranchEamArray, TryArity, ChunkData0, ChunkData, Code0, Code, AuxCode0, AuxCode) :-
	functor(BranchLabelArray, _, NBranches),
	( for(I,1,NBranches), foreach(I,AllBranches) do true ),
	hash_create(LabelTable),
	(
	    foreach(index{quality:Quality,variable:VarDesc,partition:DecisionTree},IndexDescs),
	    fromto(Code0,Code1,Code3,Code),
	    fromto(AuxCode0,AuxCode1,AuxCode2,AuxCode),
	    fromto(ChunkData0,ChunkData1,ChunkData2,ChunkData),
	    param(LabelTable,BranchLabelArray,BranchEamArray,AllBranches,TryArity,NBranches)
	do
	    ( Quality < NBranches ->
		% Create label for "all branches of the disjunction". This is
		% re-created for each index, and is the address of the next
		% index, or the try_me-sequence respectively.
		hash_set(LabelTable, AllBranches, NextIndexLabel),

		generate_index(VarDesc, DecisionTree, LabelTable, BranchLabelArray, BranchEamArray, NextIndexLabel,
		    TryArity, ChunkData1, ChunkData2, Code1, Code2, AuxCode1, AuxCode2),
		Code2 = [code{instr:label(NextIndexLabel),regs:[]}|Code3]
	    ;
		% Omit really bad indexes
	    	Code1=Code3, AuxCode1=AuxCode2, ChunkData2=ChunkData1
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
	    	TryArity, ChunkData0, ChunkData, Code0, Code, AuxCode0, AuxCode) :-
	VarDesc = variable{varid:VarId},

	% Create a label for this index's default case
	dt_lookup2(DecisionTree, [], DefaultGroup, _),
	create_group(DefaultGroup, LabelTable, BranchLabelArray, BranchEamArray, TryArity, DefaultLabel, AuxCode0, AuxCode1),

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
	    param(DecisionTree,BranchLabelArray,BranchEamArray,TryArity,DefaultLabel,VarId),	% in
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
		    create_group(TagDefaultGroup, LabelTable, BranchLabelArray, BranchEamArray, TryArity, TagLabel, AuxCode2, AuxCode6)
		;
		    % we could use a switch_on_value
		    ( TagDefaultGroup == [] ->
			TagDefaultLabel = DefaultLabel,
			AuxCode2 = AuxCode3
		    ;
			% group: default alternatives for this type
			create_group(TagDefaultGroup, LabelTable, BranchLabelArray, BranchEamArray, TryArity, TagDefaultLabel, AuxCode2, AuxCode3)
		    ),
		    % make a value switch, unless it is trivial
		    ( TagDefaultLabel == fail, DefaultLabel == fail, TagExceptions = [_Value-ValueGroup], ValueGroup = [_] ->
			% omit singleton value switches
			% (although they could lead to earlier failure)
			SubDefaults1 = SubDefaults0,
			TmpCode1 = TmpCode3,
			create_group(ValueGroup, LabelTable, BranchLabelArray, BranchEamArray, TryArity, TagLabel, AuxCode3, AuxCode6)
		    ;
			% do use a value switch
			SubDefaults1 = [TagDefaultLabel|SubDefaults0],
			(
			    foreach(Value-ValueGroup,TagExceptions),
			    foreach(Value-ref(ValueLabel),ValueLabels),
			    fromto(AuxCode3,AuxCode4,AuxCode5,AuxCode6),
			    param(LabelTable,BranchLabelArray,BranchEamArray,TryArity)
			do
			    % group: alternatives for this value
			    create_group(ValueGroup, LabelTable, BranchLabelArray, BranchEamArray, TryArity, ValueLabel, AuxCode4, AuxCode5)
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
		create_group(VarDefaultGroup, LabelTable, BranchLabelArray, BranchEamArray, TryArity, VarLabel, AuxCode7, AuxCode9),
		Table = [meta:ref(VarLabel)|Table0]
	    ;
		% need to distinguish free/meta
		( member(meta-MetaGroup, VarExceptions) -> true ; MetaGroup = VarDefaultGroup ),
		( member(free-FreeGroup, VarExceptions) -> true ; FreeGroup = VarDefaultGroup ),
		create_group(FreeGroup, LabelTable, BranchLabelArray, BranchEamArray, TryArity, VarLabel, AuxCode7, AuxCode8),
		create_group(MetaGroup, LabelTable, BranchLabelArray, BranchEamArray, TryArity, MetaLabel, AuxCode8, AuxCode9),
		Table = [meta:ref(MetaLabel)|Table0]
	    )
	;
	    % no var cases (rare)
	    Table = [meta:ref(DefaultLabel)|Table0],
	    VarLabel = DefaultLabel,
	    AuxCode7 = AuxCode9
	),

	% Get the location of the switch-variable
	reg_or_perm(VarDesc, ChunkData0, ChunkData, FirstRegDesc, VarLoc),
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
create_group(Group, LabelTable, BranchLabelArray, BranchEamArray, TryArity, GroupLabel, AuxCode1, AuxCode) :-
	( Group = [] ->
	    AuxCode1 = AuxCode,
	    GroupLabel = fail
	; hash_get(LabelTable, Group, GroupLabel) ->
	    AuxCode1 = AuxCode
	;
	    hash_set(LabelTable, Group, GroupLabel),
	    emit_try_sequence(Group, BranchLabelArray, BranchEamArray, TryArity, GroupLabel, AuxCode1, AuxCode)
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


emit_try_sequence(Group, BranchLabelArray, BranchEamArray, TryArity, TryLabel, Code1, Code6) :-
	( Group = [BranchNr1|BranchNrs2toN] ->
	    arg(BranchNr1, BranchLabelArray, BranchLabel1),
	    ( BranchNrs2toN == [] ->
		% only one alternative, no try sequence needed
		TryLabel = BranchLabel1,
		Code1 = Code6
	    ;
		Code1 = [code{instr:label(TryLabel),regs:[]},
			code{instr:try(0,TryArity,ref(BranchLabel1)),regs:[]}
			|Code2],
		(
		    fromto(BranchNrs2toN,[BranchNr|BranchNrs],BranchNrs,[BranchNrN]),
		    fromto(Code2,Code3,Code4,Code5),
		    param(BranchLabelArray,BranchEamArray)
		do
		    arg(BranchNr, BranchLabelArray, BranchLabel),
		    arg(BranchNr, BranchEamArray, BranchEam),
		    Code3 = [code{instr:retry_inline(0,ref(BranchLabel),eam(BranchEam)),regs:[]}|Code4]
		),
		arg(BranchNrN, BranchLabelArray, BranchLabelN),
		arg(BranchNrN, BranchEamArray, BranchEamN),
		Code5 = [code{instr:trust_inline(0,ref(BranchLabelN),eam(BranchEamN)),regs:[]}|Code6]
	    )
	;
	    TryLabel = fail, Code1 = Code6
	).


% Var is expected either in a temporary or a perm (not first).
% return a corresponding register descriptor
reg_or_perm(Var, ChunkData0, ChunkData, RegDesc, VarLoc) :-
	Var = variable{varid:VarId},
	variable_occurrence(Var, ChunkData0, ChunkData, VarOccDesc),
	( VarOccDesc = tmp ->
	    RegDesc = r(VarId,VarLoc,use,_)
	; VarOccDesc = perm_first_in_chunk(VarLoc) ->
	    RegDesc = r(VarId,VarLoc,perm,_)
%	; VarOccDesc = perm_first(VarLoc) ->
%	    RegDesc = r(VarId,VarLoc,perm,_)
	; verify VarOccDesc = perm(_Y),
	    RegDesc = r(VarId,VarLoc,use,_)
	).


% Initialize environment slots according to the bitmap given.  We can't
% use the current initialize instruction because we want global variables.
emit_initialize(EAM, Code, Code0) :-
	decode_activity_map(EAM, Ys),
	(
	    foreach(Y,Ys),
	    fromto(Code,Code1,Code2,Code0)
	do
	    Code1 = [code{instr:put_global_variable(y(Y)),regs:[]}|Code2]
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

generate_regular_puts(goal{args:Args},
		ChunkData0, ChunkData, Code0, Code, CallRegDescs) :-

	% determine an order (this should be an option)
	heuristic_put_order(Args, ChunkData0, Ordered),

	% construct the arguments in the determined order
	(
	    foreach(put(_,I,Arg), Ordered),
	    foreach(r(ArgId,a(I),dest,_), CallRegDescs),
	    fromto(ChunkData0, ChunkData1, ChunkData2, ChunkData),
	    fromto(Code0, Code1, Code2, Code)
	do
	    put_term(Arg, ChunkData1, ChunkData2, Code1, Code2, ArgId)
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

generate_simple_goal(goal{functor: (=)/2, args:[Arg1,Arg2],definition_module:sepia_kernel}, ChunkData0, ChunkData, Code0, Code) ?-
	generate_unify(Arg1, Arg2, ChunkData0, ChunkData, Code0, Code), % may fail
	!.

generate_simple_goal(goal{functor: (==)/2, args:[Arg1,Arg2],definition_module:sepia_kernel}, ChunkData0, ChunkData, Code0, Code) ?-
	generate_identity(Arg1, Arg2, ChunkData0, ChunkData, Code0, Code), % may fail
	!.

generate_simple_goal(Goal, ChunkData0, ChunkData, Code0, Code) :-
	Goal = goal{functor: (?=)/2, args:[Arg1,Arg2],definition_module:sepia_kernel},
	!,
	generate_in_unify(Arg1, Arg2, ChunkData0, ChunkData, Code0, Code).

generate_simple_goal(goal{functor: get_cut/1, args:[Arg],definition_module:sepia_kernel}, ChunkData0, ChunkData, Code0, Code) ?- !,
	Arg = variable{varid:VarId},
	variable_occurrence(Arg, ChunkData0, ChunkData, VarOccDesc),
	( VarOccDesc = void ->
	    Code0 = Code
	; VarOccDesc = tmp_first ->
	    Code0 = [code{instr:savecut(R),regs:[r(VarId,R,def,_)]}|Code]
	; VarOccDesc = perm_first(Y) ->
	    Code0 = [code{instr:savecut(Y),regs:[r(VarId,Y,perm,_)]}|Code]
	;
	    verify false	% require a first occurrence!
	).

generate_simple_goal(goal{functor: cut_to/1, args:[Arg],definition_module:sepia_kernel,envsize:ESize}, ChunkData0, ChunkData, Code0, Code) ?- !,
	Arg = variable{varid:VarId},
%	verify nonvar(ESize),
	variable_occurrence(Arg, ChunkData0, ChunkData, VarOccDesc),
	( VarOccDesc = void ->
	    Code0 = Code
	; VarOccDesc = tmp ->
	    Code0 = [code{instr:cut(R),regs:[r(VarId,R,use_a,_)]}|Code]
	; VarOccDesc = perm_first_in_chunk(Y) ->
	    Code0 = [code{instr:cut(Y,ESize),regs:[r(VarId,Y,perm,_)]}|Code]
	; VarOccDesc = perm(_Y) ->
	    % this should be use_y, otherwise if the value somehow got
	    % into a register, it will be used from there
	    Code0 = [code{instr:cut(RY,ESize),regs:[r(VarId,RY,use,_)]}|Code]
	;
	    verify false
	).

generate_simple_goal(goal{functor: Name/Arity, args:Args,definition_module:sepia_kernel}, ChunkData0, ChunkData, Code0, Code) ?-
	inlined_builtin(Name, Arity, InstrName),
	!,
	functor(Instr, InstrName, Arity),
	Code3 = [code{instr:Instr,regs:RegDescs}|Code],
	% could also use heuristic_put_order here, but these builtins
	% are unlikely to be called with constructed structure arguments.
	(
	    foreach(Arg,Args),
	    foreacharg(Reg,Instr),
	    foreach(r(ValId,Reg,use_a,_),RegDescs),
	    fromto(ChunkData0, ChunkData2, ChunkData3, ChunkData),
	    fromto(Code0, Code1, Code2, Code3)
	do
	    put_term(Arg, ChunkData2, ChunkData3, Code1, Code2, ValId)
	).

generate_simple_goal(goal{functor: P, args:Args}, ChunkData0, ChunkData, Code0, Code) ?-
	Code3 = [code{instr:escape(P,Regs),regs:RegDescs}|Code],
	RegArr =.. [_|Regs],
	RegDescArr =.. [_|RegDescs],
	heuristic_put_order(Args, ChunkData0, OrderedPuts),
	(
	    foreach(put(_,I,Arg),OrderedPuts),
	    fromto(ChunkData0, ChunkData2, ChunkData3, ChunkData),
	    fromto(Code0, Code1, Code2, Code3),
	    param(RegArr,RegDescArr)
	do
	    arg(I, RegArr, Reg),
	    arg(I, RegDescArr, r(ValId,Reg,use_a,_)),
	    put_term(Arg, ChunkData2, ChunkData3, Code1, Code2, ValId)
	).


% All the builtins that can be implemented by an instruction like
%	bi_inst Ai ... Ak

% inlined_builtin(+Name, +Arity, -Instruction)
inlined_builtin(fail,		0,	failure).
inlined_builtin(free,		1,	bi_free).
inlined_builtin(is_suspension,	1,	bi_is_suspension).
inlined_builtin(is_event,	1,	bi_is_event).
inlined_builtin(is_handle,	1,	bi_is_handle).
inlined_builtin(nonvar,		1,	bi_nonvar).
inlined_builtin(var,		1,	bi_var).
inlined_builtin(meta,		1,	bi_meta).
inlined_builtin(atom,		1,	bi_atom).
inlined_builtin(integer,	1,	bi_integer).
inlined_builtin(rational,	1,	bi_rational).
inlined_builtin(real,		1,	bi_real).
inlined_builtin(float,		1,	bi_float).
inlined_builtin(breal,		1,	bi_breal).
inlined_builtin(string,		1,	bi_string).
inlined_builtin(number,		1,	bi_number).
inlined_builtin(atomic,		1,	bi_atomic).
inlined_builtin(compound,	1,	bi_compound).
inlined_builtin(is_list,	1,	bi_is_list).
inlined_builtin(==,		2,	get_matched_value).
inlined_builtin(\==,		2,	bi_not_identical).
inlined_builtin(\==,		3,	bi_not_ident_list).
inlined_builtin(~=,		3,	bi_inequality).
inlined_builtin(set_bip_error,	1,	bi_set_bip_error).
inlined_builtin(get_bip_error,	1,	bi_get_bip_error).
inlined_builtin(cont_debug,	0,	bi_cont_debug).
inlined_builtin(-,		2,	bi_minus).
inlined_builtin(succ,		2,	bi_succ).
inlined_builtin(+,		3,	bi_add).
inlined_builtin(-,		3,	bi_sub).
inlined_builtin(*,		3,	bi_mul).
inlined_builtin(/,		3,	bi_quot).
inlined_builtin(//,		3,	bi_div).
inlined_builtin(rem,		3,	bi_rem).
inlined_builtin(div,		3,	bi_fdiv).
inlined_builtin(mod,		3,	bi_mod).
inlined_builtin(/\,		3,	bi_and).
inlined_builtin(\/,		3,	bi_or).
inlined_builtin(xor,		3,	bi_xor).
inlined_builtin(\,		2,	bi_bitnot).
inlined_builtin(=:=,		3,	bi_eq).
inlined_builtin(=\=,		3,	bi_ne).
inlined_builtin(<,		3,	bi_lt).
inlined_builtin(>,		3,	bi_gt).
inlined_builtin(=<,		3,	bi_le).
inlined_builtin(>=,		3,	bi_ge).
inlined_builtin(arg,		3,	bi_arg).
inlined_builtin(make_suspension, 4,	bi_make_suspension).
inlined_builtin(sys_return,	1,	bi_exit).



% The special case of =/2 goal
% Fail if no special treatment possible (shouldn't happen)

generate_unify(Arg1, Arg2, ChunkData0, ChunkData, Code0, Code) :-
	Arg1 = variable{varid:VarId1},
	Arg2 = variable{varid:VarId2},
	!,
	( VarId1 = VarId2 ->
	    ChunkData = ChunkData0, Code0 = Code
	;
	    variable_occurrence(Arg1, ChunkData0, ChunkData1, VarOccDesc1),
	    variable_occurrence(Arg2, ChunkData1, ChunkData, VarOccDesc2),
	    unify_variables(VarOccDesc1, VarId1, VarOccDesc2, VarId2, Code0, Code)
	).
generate_unify(Arg1, Arg2, ChunkData0, ChunkData, Code0, Code) :-
	Arg2 = variable{},
	!,
	generate_unify(Arg2, Arg1, ChunkData0, ChunkData, Code0, Code).
generate_unify(Arg1, Arg2, ChunkData0, ChunkData, Code0, Code) :-
	Arg1 = variable{varid:VarId},
	!,
	variable_occurrence(Arg1, ChunkData0, ChunkData1, VarOccDesc),
	bind_variable(VarOccDesc, VarId, Arg2, ChunkData1, ChunkData, Code0, Code).
generate_unify(Arg1, Arg2, ChunkData0, ChunkData, Code0, Code) :-
	atomic(Arg1), atomic(Arg2), !,
	ChunkData0 = ChunkData,
	( Arg1 = Arg2 ->
	    Code0 = Code
	;
	    Code0 = [code{instr:failure,regs:[]}|Code]
	).
generate_unify(_Arg1, _Arg2, _ChunkData0, _ChunkData, _Code0, _Code) :-
	writeln(warning_output,
	    "WARNING: nonvar = nonvar unification should be unwrapped by preprocessing"),
	fail.


%
% Generate code for the unification of a variable and a nonvariable.
%
% bind_variable(VarOccDesc, VarId, Term, ChunkData0, ChunkData, Code, Code0)
%

bind_variable(void, _VarId, _Term, ChunkData, ChunkData, Code, Code).
bind_variable(tmp_first, VarId, Term, ChunkData0, ChunkData, Code, Code0) :-
	body(VarId, Term, ChunkData0, ChunkData, Code, Code0).
bind_variable(tmp, VarId, Term, ChunkData0, ChunkData, Code, Code0) :-
	head(VarId, Term, ChunkData0, ChunkData, Code, Code0).
bind_variable(perm_first(Y), VarId, Term, ChunkData0, ChunkData, Code, Code0) :-
	Code1 = [code{instr:move(R,Y),regs:[r(VarId,R,use_a,_),r(VarId,Y,perm,_)]}|Code0],
	body(VarId, Term, ChunkData0, ChunkData, Code, Code1).
bind_variable(perm_first_in_chunk(Y), VarId, Term, ChunkData0, ChunkData, Code, Code0) :-
	Code = [code{instr:nop,regs:[r(VarId,Y,perm,_)]}|Code1],
	head(VarId, Term, ChunkData0, ChunkData, Code1, Code0).
bind_variable(perm(_Y), VarId, Term, ChunkData0, ChunkData, Code, Code0) :-
	head(VarId, Term, ChunkData0, ChunkData, Code, Code0).


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

unify_variables(VarOccDesc1, VarId1, VarOccDesc2, VarId2, Code, Code0) :-
	% order the two descriptors, so we need only half a matrix below
	( VarOccDesc1 @> VarOccDesc2 ->
	    unify_variables_ord(VarOccDesc2, VarId2, VarOccDesc1, VarId1, Code, Code0)
	;
	    unify_variables_ord(VarOccDesc1, VarId1, VarOccDesc2, VarId2, Code, Code0)
	).

% PRE: VarOccDesc1 @=< VarOccDesc2
unify_variables_ord(tmp, VarId1, tmp, VarId2, Code, Code0) :- !,
	Code = [code{instr:get_value(R1,R2), regs:[r(VarId1,R1,use,_),r(VarId2,R2,use,_)]}
		|Code0].
unify_variables_ord(tmp, VarId1, tmp_first, VarId2, Code, Code0) :- !,
	Code = [code{instr:move(R1,R2), regs:[r(VarId1,R1,use,_),r(VarId2,R2,def,_)]}
		|Code0].
unify_variables_ord(tmp, _VarId1, void, _VarId2, Code, Code) :- !.
unify_variables_ord(tmp, VarId1, perm(_), VarId2, Code, Code0) :- !,
	Code = [code{instr:get_value(R1,RY2), regs:[r(VarId1,R1,use,_),r(VarId2,RY2,use,_)]}
		|Code0].
unify_variables_ord(tmp, VarId1, perm_first(Y2), VarId2, Code, Code0) :- !,
	Code = [code{instr:move(R1,Y2), regs:[r(VarId1,R1,use,_),r(VarId2,Y2,perm,_)]}
		|Code0].
unify_variables_ord(tmp, VarId1, perm_first_in_chunk(Y2), VarId2, Code, Code0) :- !,
	Code = [code{instr:get_value(R1,Y2), regs:[r(VarId1,R1,use,_),r(VarId2,Y2,perm,_)]}
		|Code0].


unify_variables_ord(tmp_first, VarId1, tmp_first, VarId2, Code, Code0) :- !,
	Code = [code{instr:put_variable(R1), regs:[r(VarId1,R1,def,_)]},
		code{instr:move(R11,R2), regs:[r(VarId1,R11,use_a,_),r(VarId2,R2,def,_)]}
		|Code0].
unify_variables_ord(tmp_first, _VarId1, void, _VarId2, Code, Code) :- !.
unify_variables_ord(tmp_first, VarId1, perm(Y2), _VarId2, Code, Code0) :- !,
%	Code = [code{instr:move(RY2,R1), regs:[r(_VarId2,RY2,use,_),r(VarId1,R1,def,_)]}|Code0].
	Code = [code{instr:nop, regs:[r(VarId1,Y2,perm,_)]} |Code0].
unify_variables_ord(tmp_first, VarId1, perm_first(Y2), VarId2, Code, Code0) :- !,
	Code = [code{instr:put_global_variable(R1,Y2), regs:[r(VarId2,Y2,perm,_),r(VarId1,R1,def,_)]}
		|Code0].
unify_variables_ord(tmp_first, VarId1, perm_first_in_chunk(Y2), _VarId2, Code, Code0) :- !,
%	Code = [code{instr:move(Y2,R1), regs:[r(_VarId2,Y2,perm,_),r(VarId1,R1,def,_)]}|Code0].
	Code = [code{instr:nop, regs:[r(VarId1,Y2,perm,_)]} |Code0].

unify_variables_ord(void, _VarId1, void, _VarId2, Code, Code) :- !.
unify_variables_ord(void, _VarId1, perm(_), _VarId2, Code, Code) :- !.
unify_variables_ord(void, _VarId1, perm_first(Y2), VarId2, Code, Code0) :- !,
	Code = [code{instr:put_global_variable(Y2), regs:[r(VarId2,Y2,perm,_)]}
		|Code0].
unify_variables_ord(void, _VarId1, perm_first_in_chunk(_), _VarId2, Code, Code) :- !.

unify_variables_ord(perm(_Y1), VarId1, perm(_Y2), VarId2, Code, Code0) :- !,
	Code = [code{instr:get_value(RY1,RY2), regs:[r(VarId1,RY1,use,_),r(VarId2,RY2,use,_)]}
		|Code0].
unify_variables_ord(perm(_Y1), VarId1, perm_first(Y2), VarId2, Code, Code0) :- !,
	Code = [code{instr:move(RY1,Y2), regs:[r(VarId1,RY1,use,_),r(VarId2,Y2,perm,_)]}
		|Code0].
unify_variables_ord(perm(_Y1), VarId1, perm_first_in_chunk(Y2), VarId2, Code, Code0) :- !,
	Code = [code{instr:get_value(RY1,Y2), regs:[r(VarId1,RY1,use,_),r(VarId2,Y2,perm,_)]}
		|Code0].

unify_variables_ord(perm_first(Y1), VarId1, perm_first_in_chunk(Y2), VarId2, Code, Code0) :- !,
	Code = [code{instr:move(Y2,Y1), regs:[r(VarId2,Y2,perm,_),r(VarId1,Y1,perm,_)]}
		|Code0].
unify_variables_ord(perm_first(Y1), VarId1, perm_first(Y2), VarId2, Code, Code0) :- !,
	Code = [code{instr:put_global_variable(Y1), regs:[r(VarId1,Y1,perm,_)]},
		code{instr:move(Y1,Y2), regs:[r(VarId2,Y2,perm,_)]}
		|Code0].

unify_variables_ord(perm_first_in_chunk(Y1), VarId1, perm_first_in_chunk(Y2), VarId2, Code, Code0) :- !,
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
	    variable_occurrence(Arg1, ChunkData0, ChunkData1, VarOccDesc),
	    verify VarOccDesc == tmp,
	    in_head(VarId, Arg2, ChunkData1, ChunkData, Code0, Code)
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

put_term(Term, ChunkData0, ChunkData, Code, Code0, VarId) :-
	Term = variable{varid:VarId}, !,
	put_variable(Term, ChunkData0, ChunkData, Code, Code0).
put_term(Term, ChunkData0, ChunkData, Code, Code0, ValId) :-
	new_aux_temp(ChunkData0, ChunkData1, ValId),
	body(ValId, Term, ChunkData1, ChunkData, Code, Code0).


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

put_variable(Var, ChunkData0, ChunkData, Code, Code0) :-
	Var = variable{varid:VarId},
	variable_occurrence(Var, ChunkData0, ChunkData, VarOccDesc),
	put_va_code(VarOccDesc, VarId, Code, Code0).

    put_va_code(void, VarId, Code, Code0) :-
	Code = [code{instr:put_variable(R),regs:[r(VarId,R,def,_)]}|Code0].
    put_va_code(tmp_first, VarId, Code, Code0) :-
	Code = [code{instr:put_variable(R),regs:[r(VarId,R,def,_)]}|Code0].
    put_va_code(tmp, _VarId, Code, Code0) :-
	% Variable already known in this chunk: The register allocator will
	% move it to the correct register as necessary (triggered by the dest
	% descriptor that comes with the call instruction).
	Code = Code0.
    put_va_code(perm_first(Y), VarId, Code, Code0) :-
	% First ever occurrence of this permanent variable. Emit code to
	% initialise it and tell the reg allocator about the two locations.
	Code = [code{instr:put_global_variable(R,Y),regs:[r(VarId,Y,perm,_),r(VarId,R,def,_)]}|Code0].
    put_va_code(perm_first_in_chunk(Y), VarId, Code, Code0) :-
	% First occurrence of this permanent variable in this chunk.
	% Tell the reg allocator about the permanent location. It will then
	% move it to the correct register as necessary (triggered by the dest
	% descriptor that comes with the call instruction).
	Code = [code{instr:nop,regs:[r(VarId,Y,perm,_)]}|Code0].
    put_va_code(perm(_Y), _VarId, Code, Code0) :-
	% Variable already known in this chunk. The register allocator will
	% move it to the correct register as necessary.
	Code = Code0.


% Generate code to move head occurrences of permanent variables
% into their environment slots.
move_head_perms(HeadPerms, Code, Code0) :-
	(
	    foreach(delayed_move(VarId,Y),HeadPerms),
	    fromto(Code,[Move|Code1],Code1,Code0)
	do
	    Move = code{instr:move(R,Y),regs:[r(VarId,R,use,_),r(VarId,Y,perm,_)]}
	).


%----------------------------------------------------------------------
% Debugging and testing
%----------------------------------------------------------------------

:- comment(print_annotated_code/1, [
    summary:"Debugging: print annotated WAM code",
    amode:print_annotated_code(+),
    args:[
	"Code":"A list of struct(code)"
    ],
    see_also:[generate_code/6,struct(code)]
]).

:- export print_annotated_code/1.

print_annotated_code(Code) :-
	writeln("------ Code ------"),
	( foreach(InstrDesc,Code) do
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
	    )
	).

