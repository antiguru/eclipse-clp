
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
% Copyright (C) 2006,2007 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): Joachim Schimpf.
% 
% END LICENSE BLOCK
% ----------------------------------------------------------------------
% System:	ECLiPSe Constraint Logic Programming System
% Component:	ECLiPSe III compiler
% Version:	$Id: ecl_compiler.ecl,v 1.2 2008/06/16 00:54:36 jschimpf Exp $
% ----------------------------------------------------------------------

:- module(ecl_compiler).

:- comment(summary,	"ECLiPSe III compiler - toplevel predicates").
:- comment(copyright,	"Cisco Technology Inc").
:- comment(author,	"Joachim Schimpf").
:- comment(date,	"$Date: 2008/06/16 00:54:36 $").

:- comment(desc, html("
    This module contains the toplevel predicates for invoking the
    compiler. This is where the different compiler passes are chained
    together, and where the compiler options are defined.  It also
    contains the code to process source files, and to interpret
    directives and queries.
    <P>
    The top-level interfaces to the compiler are: compile/1,2 for
    compilation from files, compile_term/1,2 for compiling data.
")).

:- use_module(compiler_common).
:- use_module(compiler_normalise).
:- use_module(compiler_analysis).
:- use_module(compiler_peephole).
:- use_module(compiler_codegen).
:- use_module(compiler_varclass).
:- use_module(compiler_indexing).
:- use_module(compiler_regassign).
:- use_module(source_processor).

:- lib(asm).
:- lib(hash).
:- lib(module_options).

:- import
	set_default_error_handler/2
   from sepia_kernel.

:- pragma(system).


%----------------------------------------------------------------------
% Compiler Options
%----------------------------------------------------------------------

compiler_options_setup(File, OptionList, Options) :-
	( atom(File) -> atom_string(File, FileS)
	; string(File) -> FileS = File
	; term_string(File, FileS)
	),

	% Consider global settings for the compiler options
	get_flag(debug_compile, Dbgcomp),
	set_default_option(debug, Dbgcomp)@compiler_common,

	( OptionList = options{} ->
	    Options0 = OptionList
	; get_options(OptionList, Options0)@compiler_common ->
	    true
	;
	    printf(error, "Invalid option list: %w%n", [OptionList]),
	    print_default_options(error)@compiler_common,
	    abort
	),

	Options0 = options{outdir:OutDir},
	( Options0 = options{output:listing(LstFile)} ->
	    open(LstFile,write,Stream),
	    update_struct(options, [output:print(Stream)], Options0, Options)
	; Options0 = options{output:listing} ->
	    pathname(FileS, Dir, Base, _Suffix),
	    ( concat_string([OutDir], "") -> 
		concat_string([Dir,Base,'.lst'], LstFile)
	    ;
		concat_string([OutDir,/,Base,'.lst'], LstFile)
	    ),
	    open(LstFile,write,Stream),
	    update_struct(options, [output:print(Stream)], Options0, Options)
	; Options0 = options{output:eco(EcoFile)} ->
	    open(EcoFile,write,Stream),
	    update_struct(options, [output:eco_to_stream(Stream)], Options0, Options)
	; Options0 = options{output:eco} ->
	    pathname(FileS, Dir, Base, _Suffix),
	    get_flag(eclipse_object_suffix, ECO),
	    ( concat_string([OutDir], "") -> 
		concat_string([Dir,Base,ECO], EcoFile)
	    ;
		concat_string([OutDir,/,Base,ECO], EcoFile)
	    ),
	    open(EcoFile,write,Stream),
	    update_struct(options, [output:eco_to_stream(Stream)], Options0, Options)
	; Options0 = options{output:asm} ->
	    concat_string([File], FileS),
	    pathname(FileS, Dir, Base, _Suffix),
	    ( concat_string([OutDir], "") -> 
		concat_string([Dir,Base,'.asm'], AsmFile)
	    ;
		concat_string([OutDir,/,Base,'.asm'], AsmFile)
	    ),
	    open(AsmFile,write,Stream),
	    update_struct(options, [output:asm_to_stream(Stream)], Options0, Options)
	;
	    Options = Options0
	).


compiler_options_cleanup(Options) :-
    	( Options = options{output:print(Stream)} ->
	    close(Stream)
    	; Options = options{output:eco_to_stream(Stream)} ->
	    close(Stream)
    	; Options = options{output:asm_to_stream(Stream)} ->
	    close(Stream)
	;
	    true
	).


% ----------------------------------------------------------------------
% Compile a single predicate.
% 
% Takes a list of clauses (which must all be for the same predicate),
% In case of error, succeed with Size = -1.0Inf.
% ----------------------------------------------------------------------

compile_predicate(ModulePred, Clauses, AnnClauses, SourcePos, PredsSeen, Options, Size) ?-
	block(
	    compile_predicate1(ModulePred, Clauses, AnnClauses, SourcePos,
                               PredsSeen, Options, Size),
	    abort_compile_predicate,
	    Size = -1.0Inf),
	( var(Size) -> Size = 0 ; true ).


compile_predicate1(_, [], _, _, _, _, CodeSize) :- !, CodeSize = 0.
compile_predicate1(ModulePred, Clauses, AnnClauses, SourcePos, PredsSeen, Options, CodeSize) :-
	message(compiling(ModulePred), Options),
	ModulePred = Module:Pred,
	Pred = N/A,
	( atom(N), integer(A) -> true
	; compiler_event(#illegal_head, SourcePos, _Ann, N, Module)
	),
	verify (Clauses = [Clause|_], extract_pred(Clause, Pred)),
	check_flags(Pred, Module, Options),
	( get_flag(Pred, stability, dynamic)@Module ->
	    % preliminary handling of dynamic code
	    CodeSize = 0,
	    ( foreach(Clause, Clauses), param(Module) do
		assert(Clause)@Module
	    )

	% treat non-consecutive and multifile here

	; check_redefinition(ModulePred, PredsSeen, SourcePos, Options) ->
	    compile_pred_to_wam(Clauses, AnnClauses, WAM, Options, ModulePred),
	    pred_flags(Options, Flags),
	    ( ( Options = options{load:all} ; Options = options{load:new}, \+ is_predicate(Pred)@Module) ->
		% double negation, because asm binds the labels
		\+ \+ block(asm(Pred, WAM, Flags)@Module, _, true),
                get_flag(Pred, code_size, CodeSize)@Module,
                set_pred_pos(Pred, SourcePos, Module)
	    ;
		true % don't clobber existing code if not loading
	    ),
	    ( Options = options{output:print} ->
                printf("%w:%n", [Pred]),
		print_wam(WAM)
	    ; Options = options{output:print(Stream)} ->
                writeclauses(Stream, Clauses),
		get_stream(output, OldOut),
		set_stream(output, Stream),
		print_wam(WAM),
		set_stream(output, OldOut),
		writeln(Stream, --------------------)
	    ; Options = options{output:eco_to_stream(Stream)} ->
                pasm(WAM, CodeSize, BTPos, Codes),
		( portable_object_code(Codes) ->
		    true
		;
		    get_flag(eclipse_object_suffix, ECO),
		    machine_bits(BPW),
		    printf(warning_output,
			"WARNING: the generated %w file will only work reliably on %w bit machines!%n",
			[ECO,BPW])
		),
		CodeArr =.. [[]|Codes],
                get_pred_pos(SourcePos, File, Line, Offset),
                ( Module == sepia_kernel ->
		    % call locally, because :/2 may not be defined yet
		    StorePred = store_pred(Pred,CodeArr,CodeSize,BTPos,Flags,File,Line,Offset)
		;
		    StorePred = sepia_kernel:store_pred(Pred,CodeArr,CodeSize,BTPos,Flags,File,Line,Offset)
		),
		printf(Stream, "%ODQKw.%n", [:-StorePred])@Module
	    ; Options = options{output:asm_to_stream(Stream)} ->
                pretty_print_asm(WAM, Stream, Pred, Flags, Module)
            ; Options = options{output:none} ->
                true
	    ;
		Options = options{output:Junk},
		printf(error, "Invalid output option: %w%n", [Junk]),
		abort
	    )
	;
	    CodeSize = 0
	).


    writeclauses(Stream, Clauses) :-
	get_stream_info(Stream, output_options, Opt),
	( delete(numbervars(NV), Opt, Opt0) -> true ; NV=false,Opt0=Opt ),
	set_stream_property(Stream, output_options, [numbervars(true)|Opt0]),
	( foreach(Clause,Clauses),param(Stream) do
	    \+ \+ (
		numbervars:numbervars(Clause, 0, _),
		writeclause(Stream, Clause)
	    )
	),
	nl(Stream),
	set_stream_property(Stream, output_options, [numbervars(NV)|Opt0]).


    pretty_print_asm(WAM, Stream, Pred, Flags, Module) :-
        printf(Stream, ":- asm:asm(%DQKw, [%n", [Pred])@Module,
        ( fromto(WAM, [Instr|Rest],Rest, []), param(Stream, Module) do
            ( Instr = label(_) ->
                printf(Stream, "%DQKw", [Instr])@Module % no indent for labels
            ;
                printf(Stream, "	%DQKw", [Instr])@Module
            ),
            (Rest \== [] -> writeln(Stream, ",") ; nl(Stream))
        ),
        printf(Stream, "], %DQKw).%n%n", [Flags]).


            
    pred_flags(options{debug:Debug,system:System,skip:Skip}, Flags) ?-
	( Debug==on -> Flags0 = 16'00080000 ; Flags0 = 0 ),			%'% DEBUG_DB
	( System==on -> Flags1 is Flags0 \/ 16'40000000 ; Flags1 = Flags0 ),	%'% SYSTEM
	( Skip==on -> Flags is Flags1 \/ 16'00040000 ; Flags = Flags1 ).	%'% DEBUG_SK


    set_pred_flags(options{debug:Debug,system:System,skip:Skip}, Pred, Module) ?-
	set_flag(Pred, debugged, Debug)@Module,
	set_flag(Pred, skip, Skip)@Module,
	( System==on -> Type = built_in ; Type = user ),
	set_flag(Pred, type, Type)@Module.


    set_pred_pos(Pred, source_position{file:File,line:Line,offset:Offset}, Module) :-
	( string(File), atom_string(FileAtom, File)
	; atom(File), FileAtom = File
	),
	!,
    	set_flag(Pred, source_file, FileAtom)@Module,
    	set_flag(Pred, source_line, Line)@Module,
    	set_flag(Pred, source_offset, Offset)@Module.
    set_pred_pos(_Pred, _Pos, _Module).

    get_pred_pos(none, 0, 0, 0).
    get_pred_pos(term, 0, 0, 0).
    get_pred_pos(source_position{file:File0,line:Line,offset:Offset}, File, Line, Offset) :-
        concat_atom([File0], File).

    % Fail if this is a redefinition that we want to ignore
    check_redefinition(ModulePred, PredsSeen, SourcePos, Options) :-
	ModulePred = Module:Pred,
    	( hash_contains(PredsSeen, ModulePred) ->
	    % Non-consecutive clauses: if handler fails, don't redefine
	    compiler_event(#consecutive, SourcePos, _Ann, Pred, Module)
	; 	
	    get_flag(Pred, source_file, OldFile)@Module,
	    SourcePos = source_position{file:NewFile,line:Line},
	    concat_atom([NewFile], NewFileAtom),
	    OldFile \== NewFileAtom
	->
	    % Seen in other file: if handler fails, don't redefine
	    error(#multifile, (Pred,OldFile,NewFile:Line), Module)
	;
	    true
	),
	hash_set(PredsSeen, ModulePred, []).


    check_flags(Pred, Module, Options) :-
    	( get_flag(Pred, tool, on)@Module ->
	    error(#tool_redef, Pred, Module)
	; true ),
    	( get_flag(Pred, parallel, on)@Module,  Options = options{warnings:on} ->
	    printf(warning_output, "Parallel-declaration ignored for %w%n", [Module:Pred])
	; true ).


% Compile a predicate (list of clauses) to WAM code list
% This chains together the various stages of the comiler

compile_pred_to_wam(Clauses, AnnCs, FinalCode, Options, Module:Pred) :-

	% Create our normal form
	message("Normalize", 2, Options),
	normalize_clauses_annotated(Clauses, AnnCs, [], NormPred0, _NVars, Module),
%	print_normalized_clause(output, NormPred0),

	% Do some intra-predicate flow analysis
	message("Analysis", 2, Options),
	binding_analysis(NormPred0),

	% Here we could have a simplification pass, exploiting
	% information from the analysis phase.

	% Indexing_transformation (needs info from binding_analysis)
	message("Indexing", 2, Options),
	indexing_transformation(NormPred0, NormPred, Options),

	% Variable classification
	% classify_variables must be after indexing transformation, because
	% indexing_transformation introduces extra variable occurrences.
	% Classifies void, temp and permanent vaiables, and assigns environment
	% slots to the permanent ones. Also adds disjunction pseudo-args.
	message("Varclass", 2, Options),
	classify_variables(NormPred, 0, Options),

	( Options = options{print_normalised:on} ->
	    print_normalized_clause(output, NormPred)
	;
	    true
	),

	% Code generation
	message("Codegen", 2, Options),
	generate_code(NormPred, RawCode, AuxCode, Options, Module:Pred),
	( Options = options{print_raw_code:on} ->
	    print_annotated_code(RawCode)
	;
	    true
	),

	% Register allocation
	message("Regassign", 2, Options),
	assign_am_registers(RawCode, RegCode, AuxCode),
	( Options = options{print_raw_code:on} ->
	    print_annotated_code(RegCode)
	;
	    true
	),

	% WAM level postprocessing
	message("Simplify", 2, Options),
	simplify_code(RegCode, FinalCode, Options),
	( Options = options{print_final_code:on} ->
	    print_annotated_code(FinalCode)
	;
	    true
	).


%----------------------------------------------------------------------
% Error handling
%----------------------------------------------------------------------

:- set_default_error_handler(#consecutive, compiler_err_fail_handler/2).
:- set_default_error_handler(#illegal_head, compiler_err_abort_handler/2).
:- set_default_error_handler(#illegal_goal, compiler_err_abort_handler/2).

compiler_err_abort_handler(Error, Culprit) :-
	print_compiler_message(error, Error, Culprit),
	exit_block(abort_compile_predicate).

compiler_err_fail_handler(Error, Culprit) :-
	print_compiler_message(error, Error, Culprit),
	fail.

compiler_warn_cont_handler(Error, Culprit) :-
	print_compiler_message(warning_output, Error, Culprit).

    print_compiler_message(Stream, Error, Term@Location) ?-
	print_location(Stream, Location),
	error_id(Error, Message), 
	printf(Stream, "%w: ", [Message]),
	get_flag(output_options, OutputOptions),
	write_term(Stream, Term, OutputOptions),
	nl(Stream),
	flush(Stream).


:- set_default_error_handler(#multifile, redef_other_file_handler/2).

redef_other_file_handler(_, (Pred, OldFile0, Location)) :-
	print_location(warning_output, Location),
	local_file_name(OldFile0, OldFile),
	printf(warning_output, "WARNING: %Kw replaces previous definition in file %w%n",
		 [Pred,OldFile]).


compiler_warning(Ann, SourcePos, String, Params, options{warnings:on}) :- !,
	compiler_message(warning_output, Ann, SourcePos, String, Params).
compiler_warning(_, _, _, _, _).


compiler_error(_Ann, SourcePos, String, Params) :-
	compiler_message(error, _Ann, SourcePos, String, Params),
	exit_block(abort_compile_predicate).


compiler_message(Stream, Ann, SourcePos, String, Params) :-
	get_error_location(Ann, SourcePos, Location),
	print_location(Stream, Location),
	printf(Stream, String, Params),
	nl(Stream).


print_location(Stream, File:Line) ?- !,
	local_file_name(File, LocalFile),
	printf(Stream, "File %w, line %d:%n  ", [LocalFile,Line]).
print_location(Stream, Location) :-
	printf(Stream, "In compiling %w:%n  ", [Location]).
    	

local_file_name(File, LocalF) :-
	getcwd(Cwd),
	atom_string(File, FileS),
	( append_strings(Cwd, LocalF, FileS) ->
	    true
	;
	    LocalF = File
	).


%----------------------------------------------------------------------
% From-file compiler
%----------------------------------------------------------------------

:- export
	compile/1, compile_/2,
	compile/2, compile_/3,
	compile_stream/1, compile_stream_/2,
	compile_stream/2, compile_stream_/3.

:- comment(compile/1, [
    summary:"Compile a file",
    args:["File":"File name (atom or string) or stream(Stream)"],
    amode:compile(++),
    see_also:[compile/2,compile_stream/1,compile_term/1,compile_term/2,struct(options)],
    desc:html("
    Compile a file with default options.  The resulting code is directly
    loaded into memory and ready for execution.  Equivalent to
    <PRE>
	compile(File, [])
    </PRE>
    ")
]).
:- tool(compile/1, compile_/2).
:- set_flag(compile/1, type, built_in).
compile_(File, Module) :-
    compile_(File, [], Module).


:- comment(compile_stream/1, [
    summary:"Compile from an input stream",
    args:["Stream":"Stream name or handle"],
    amode:compile_stream(++),
    see_also:[compile/1,compile/2,compile_term/1,compile_term/2,compile_stream/2,struct(options)],
    desc:html("
    Compile from an (already opened) input stream with default options.
    The resulting code is directly loaded into memory and ready for execution.
    Equivalent to
    <PRE>
	compile(stream(Stream), [])
    </PRE>
    ")
]).
:- tool(compile_stream/1, compile_stream_/2).
:- set_flag(compile_stream/1, type, built_in).
compile_stream_(Stream, Module) :-
    compile_stream_(Stream, [], Module).


:- comment(compile_stream/2, [
    summary:"Compile from an input stream",
    args:["Stream":"Stream name or handle",
    	"Options":"List of compiler options"],
    amode:compile_stream(++,++),
    see_also:[compile/1,compile/2,compile_term/1,compile_term/2,compile_stream/1,struct(options)],
    desc:html("
    Compile from an (already opened) input stream with the given options.
    Equivalent to
    <PRE>
	compile(stream(Stream), Options)
    </PRE>
    ")
]).
:- tool(compile_stream/2, compile_stream_/3).
:- set_flag(compile_stream/2, type, built_in).
compile_stream_(Stream, Options, Module) :-
    compile_source(stream(Stream), Options, Module).


:- comment(compile/2, [
    summary:"Compile a file",
    args:["File":"File name (atom or string) or structure stream(Stream)",
    	"Options":"List of compiler options"],
    amode:compile(++,++),
    see_also:[compile/1,compile_stream/1,compile_term/1,compile_term/2,struct(options)],
    desc:html("
    Compile a file, with output according to options.
    <P>
    For backward compatibility, we allow a module name in place
    of the Options-list.
    ")
]).
:- tool(compile/2, compile_/3).
:- set_flag(compile/2, type, built_in).

compile_(Sources, OptionListOrModule, CM) :- Sources = [_|_], !,
	( foreach(Source,Sources), param(OptionListOrModule, CM) do
	    compile_source(Source, OptionListOrModule, CM)
	).
compile_(Source, OptionListOrModule, CM) :-
	compile_source(Source, OptionListOrModule, CM).

compile_source(Source, OptionListOrModule, CM) :-
	valid_source(Source),
	!,
	% for backward compatibility, allow compile(Source, Module)
	% with the module being created if it does not exist
	( atom(OptionListOrModule), OptionListOrModule \== [] ->
	    Module = OptionListOrModule, OptionList = [],
	    ( current_module(Module) -> true ; create_module(Module) )
	;
	    Module = CM, OptionList = OptionListOrModule
	
	),
	compiler_options_setup(Source, OptionList, Options),
	( Options = options{load:all} ->
	    OpenOptions = [recreate_modules],
	    CloseOptions = [keep_modules]
	;
	    OpenOptions = [],
	    CloseOptions = []
	),
	error(#start_compiler, Source, CM),
	cputime(Tstart),
	( source_open(Source, [with_annotations,goal_expansion|OpenOptions], SourcePos0)@Module ->
	    sepia_kernel:register_compiler(args(Term,Ann)-(ecl_compiler:compile_term_annotated(Term,Ann,Options))),
	    hash_create(PredsSeen),
	    (
		fromto(begin, _, Class, end),
		fromto(SourcePos0, SourcePos1, SourcePos2, SourcePosEnd),
		fromto(SourcePos0, PredPos1, PredPos2, _),
		fromto(ClauseTail, Clauses0, Clauses1, []),
		fromto(ClauseTail, ClauseTail0, ClauseTail1, []),
		fromto(AnnClauseTail, AnnClauses0, AnnClauses1, []),
		fromto(AnnClauseTail, AnnClauseTail0, AnnClauseTail1, []),
                fromto(0, Size0, Size2, Size), 
		fromto(none, Pred0, Pred1, none),
		param(PredsSeen,Options,Module)
	    do
		source_read(SourcePos1, SourcePos2, Class, SourceTerm),
		SourcePos1 = source_position{module:PosModule},
		SourceTerm = source_term{term:Term,annotated:Ann},

		( Class = clause ->
		    accumulate_clauses(Term, Ann, PosModule, Options, SourcePos1, PredsSeen,
			Size0, Pred0, PredPos1, Clauses0, ClauseTail0, AnnClauses0, AnnClauseTail0,
			Size2, Pred1, PredPos2, Clauses1, ClauseTail1, AnnClauses1, AnnClauseTail1)

		; Class = comment ->		% comment, ignore
                    Size0 = Size2,
                    Pred1 = Pred0,
		    ClauseTail1 = ClauseTail0,
		    Clauses1 = Clauses0,
		    AnnClauseTail1 = AnnClauseTail0,
		    AnnClauses1 = AnnClauses0,
		    PredPos2 = PredPos1

		; % other classes are taken as predicate separator
		    ClauseTail0 = [],		% compile previous predicate
		    AnnClauseTail0 = [],
		    compile_predicate(Pred0, Clauses0, AnnClauses0, PredPos1, PredsSeen, Options, CSize),
                    Size1 is Size0 + CSize,
                    Clauses1 = ClauseTail1,
		    AnnClauses1 = AnnClauseTail1,
		    Pred1 = none,
		    PredPos2 = none,

		    ( Class = directive ->
			Size2 = Size1,
			( old_compiler_directive(Term, Options) ->
			    true
			;
			    process_directive(SourcePos1, Term, Options, PosModule)
			)
		    ; Class = query ->
			Size2 = Size1,
			process_query(SourcePos1, Term, Options, PosModule)
		    ; Class = handled_directive ->
			Size2 = Size1,
			( consider_pragmas(Term, Options, PosModule) ->
			    emit_directive_or_query(Term, Options, PosModule)
			; handle_module_boundary(Term, Options, PosModule, Module) ->
			    emit_directive_or_query(Term, Options, PosModule)
			; Term = (:-meta_attribute(Name,Handlers)) ->
			    % already partially handled in source_processor
			    emit_directive_or_query((:-meta_attribute(Name,[])), Options, PosModule),
			    process_directive(SourcePos1, (:-local initialization(meta_attribute(Name,Handlers))), Options, PosModule)
			;
			    emit_directive_or_query(Term, Options, PosModule)
			)
		    ; (Class = var ; Class = other) ->
			( block(compiler_event(#illegal_head, SourcePos1, Ann, Term, Module), abort_compile_predicate, true) -> true ; true ),
			Size2 = -1.0Inf
		    ;
			Size2 = Size0
		    )
		)
	    ),

	    % If the compilation was successful, raise various events
	    ( Size >= 0 ->
		% Raise event 149, which executes initialization goals, etc.
		% This must be done before cd-ing back in source_close below.
		% This event is also raised when the module changes mid-file!
		SourcePosEnd = source_position{module:EndModule},
		( Options = options{load:none} ->
		    true
		; EndModule == Module ->
		    error(#code_unit_loaded, [], EndModule)
		;
		    error(#code_unit_loaded, [check], EndModule)
		),

		% Raise event 139, which prints the compilation statistics
		Tcompile is cputime-Tstart,
		words_to_bytes(Size, SizeInBytes),
		SourcePos0 = source_position{file:File},
		concat_atom([File], FileAtom),
		error(#compiled_file, (FileAtom, SizeInBytes, Tcompile), EndModule),

		% Raise event 166, which records the compiled_file information
		% (used for recompilation, make/0 etc)
		( Options = options{load:none} ->
		    true
		;
		    error(#record_compiled_file, File-(ecl_compiler:compile(File, OptionList)), Module)
		)
	    ;
		true
	    ),
	    sepia_kernel:deregister_compiler,
	    source_close(SourcePosEnd, CloseOptions),
            compiler_options_cleanup(Options),
	    ( Size >= 0 -> true ;
		printf(error, "Error(s) occurred during %Qw%n", [compile(Source)]),
		abort
	    )
	;
	    compiler_options_cleanup(Options),
	    printf(error, "No such file in %Qw%n", [compile(Source)]),
	    abort
	).
compile_source(Source, OptionListOrModule, CM) :- var(Source), !,
	error(#inst_fault, compile(Source, OptionListOrModule), CM).
compile_source(Source, OptionListOrModule, CM) :-
	error(#type_error, compile(Source, OptionListOrModule), CM).

    valid_source(Source) :- atom(Source).
    valid_source(Source) :- string(Source).
    valid_source(stream(S)) ?- true.


% Add a single clause or a list of clauses to what we already have.
% If a predicate is finished, compile it.
:- mode accumulate_clauses(+,+,+,+,+,+,+,+,+,?,-,?,-,-,-,-,-,-,-,-).
accumulate_clauses([], [], _Module, _Options, _ClausePos, _PredsSeen,
		Size0, Pred0, PredPos0, PredCl0, PredClTl0, PredClAnn0, PredClAnnTl0,
		Size0, Pred0, PredPos0, PredCl0, PredClTl0, PredClAnn0, PredClAnnTl0) :-
	!.
accumulate_clauses([Term|Terms], [AnnTerm|AnnTerms], Module, Options, ClausePos, PredsSeen,
		Size0, Pred0, PredPos0, PredCl0, PredClTl0, PredClAnn0, PredClAnnTl0,
		Size, Pred, PredPos, PredCl, PredClTl, PredClAnn, PredClAnnTl) :-
	!,
	extract_pred(Term, NA),
	Pred1 = Module:NA,
	( Pred0 == Pred1 ->
	    % another clause for Pred0
	    PredClTl0 = [Term|PredClTl1],
	    PredClAnnTl0 = [AnnTerm|PredClAnnTl1],
	    accumulate_clauses(Terms, AnnTerms, Module, Options, ClausePos, PredsSeen,
	    	Size0, Pred0, PredPos0, PredCl0, PredClTl1, PredClAnn0, PredClAnnTl1,
		Size, Pred, PredPos, PredCl, PredClTl, PredClAnn, PredClAnnTl)
	;
	    % first clause for next predicate Pred1, compile Pred0
	    PredClTl0 = [], PredClAnnTl0 = [],
	    compile_predicate(Pred0, PredCl0, PredClAnn0, PredPos0, PredsSeen, Options, CSize),
            Size1 is Size0 + CSize,
            PredCl1 = [Term|PredClTl1],
	    PredClAnn1 = [AnnTerm|PredClAnnTl1],
	    accumulate_clauses(Terms, AnnTerms, Module, Options, ClausePos, PredsSeen,
	    	Size1, Pred1, ClausePos, PredCl1, PredClTl1, PredClAnn1, PredClAnnTl1,
		Size, Pred, PredPos, PredCl, PredClTl, PredClAnn, PredClAnnTl)
	).
accumulate_clauses(Term, AnnTerm, Module, Options, ClausePos, PredsSeen,
		Size0, Pred0, PredPos0, PredCl0, PredClTl0, PredClAnn0, PredClAnnTl0,
		Size, Pred, PredPos, PredCl, PredClTl, PredClAnn, PredClAnnTl) :-
	accumulate_clauses([Term], [AnnTerm], Module, Options, ClausePos, PredsSeen,
		Size0, Pred0, PredPos0, PredCl0, PredClTl0, PredClAnn0, PredClAnnTl0,
		Size, Pred, PredPos, PredCl, PredClTl, PredClAnn, PredClAnnTl).

    extract_pred((Head :- _), N/A) :- !,
    	functor(Head, N, A).
    extract_pred(Fact, N/A) :-
    	functor(Fact, N, A).



%----------------------------------------------------------------------
% Queries, directives and pragmas
%----------------------------------------------------------------------

process_directive(SourcePos, Term, Options, Module) :-
	( Options = options{load:none} ->
	    true
	;
	    % new/all
	    call_directive(SourcePos, Term, Module)
	),
	emit_directive_or_query(Term, Options, Module).


process_query(SourcePos, Term, Options, Module) :-
	( Options = options{load:all} ->
	    call_directive(SourcePos, Term, Module)
	;
	    % new/none
	    true
	),
	emit_directive_or_query(Term, Options, Module).


call_directive(SourcePos, Dir, Module) :-
	arg(1, Dir, Goal),
    	block(
	    % negate the Goal - don't bind variables!
	    ( \+ call(Goal)@Module ->
		compiler_message(warning_output, _Ann, SourcePos,
		    "Compiler warning: query failed: %w", Dir)
	    ;
	    	true
	    ),
	    Tag,
	    compiler_error(_Ann, SourcePos,
		    "Compiler error: query exited (%w)%n%w%n", [Tag,Dir])
	).



% If we see the beginning of a new module, then finalize OldModule
% (unless it is the compilation's context module, in which case this
% is the first module directive we encounter)
handle_module_boundary((:-module(Module,_,_)), Options, OldModule, TopModule) ?- !,
	handle_module_boundary((:-module(Module)), Options, OldModule, TopModule).
handle_module_boundary((:-module(_Module)), Options, OldModule, TopModule) ?- !,
	( Options = options{load:none} ->
	    true
	; OldModule == TopModule ->
	    true
	;
	    error(#code_unit_loaded, [check], OldModule)
	).


% Adjust compiler options according to pragmas

consider_pragmas((:-pragma(Pragma)), Options, M) ?-
	consider_pragma(Pragma, Options, M).

consider_pragma(debug, Options, _) :- !,
	setarg(debug of options, Options, on).
consider_pragma(nodebug, Options, _) :- !,
	setarg(debug of options, Options, off).
consider_pragma(system, Options, _) :- !,
	setarg(system of options, Options, on).
consider_pragma(skip, Options, _) :- !,
	setarg(skip of options, Options, on).
consider_pragma(noskip, Options, _) :- !,
	setarg(skip of options, Options, off).
consider_pragma(warnings, Options, _) :- !,
	setarg(warnings of options, Options, on).
consider_pragma(nowarnings, Options, _) :- !,
	setarg(warnings of options, Options, off).
consider_pragma(expand, Options, _) :- !,
	setarg(expand_goals of options, Options, on).
consider_pragma(noexpand, Options, _) :- !,
	setarg(expand_goals of options, Options, off).
consider_pragma(Pragma, _, M) :-
	error(#bad_pragma, pragma(Pragma), M).	% make accessible via current_pragma/1


% For compatibility with old compiler
old_compiler_directive((:-system), Options) :- !,
	setarg(system of options, Options, on),
	setarg(debug of options, Options, off),
	setarg(skip of options, Options, on),
	setarg(expand_goals of options, Options, on).
old_compiler_directive((:-system_debug), Options) :- !,
	setarg(system of options, Options, on),
	setarg(debug of options, Options, on),
	setarg(skip of options, Options, off).


% copy directives and queries to the eco file
% omit comments and includes
% do copy pragmas, since some of them have load-time effect
% (e.g. suppress deprecation warnings)
emit_directive_or_query((:-comment(_,_)), _Options, _Module) ?- !.
emit_directive_or_query((:-include(_)), _Options, _Module) ?- !.
emit_directive_or_query((:-[_|_]), _Options, _Module) ?- !.
emit_directive_or_query(Dir, Options, Module) :-
	( Options = options{output:print} ->
	    printf("%w.%n", [Dir])
	; Options = options{output:print(Stream)} ->
	    printf(Stream, "%w.%n", [Dir])
	; Options = options{output:eco_to_stream(Stream)} ->
	    printf(Stream, "%ODQKw.%n", [Dir])@Module
	; Options = options{output:asm_to_stream(Stream)} ->
	    printf(Stream, "%DQKw.%n", [Dir])@Module
	; Options = options{output:none} ->
	    true
	;
	    Options = options{output:Junk},
	    printf(error, "Invalid output option: %w%n", [Junk]),
	    abort
	).


%----------------------------------------------------------------------
% Compile term/list
%----------------------------------------------------------------------

:- export
	compile_term/1, compile_term_/2,
	compile_term/2, compile_term_/3,
	compile_term_annotated/3, compile_term_annotated_/4.

:- comment(compile_term/1, [
    summary:"Compile a list of terms",
    args:["Clauses":"List of clauses and/or directives"],
    amode:compile_term(+),
    see_also:[compile/1,compile/2,compile_stream/1,compile_term/2],
    desc:html("
    Compile a list of clauses and queries.
    Handling of directives: include/1, ./2 and currently module/1
    are not allowed.  Pragmas are interpreted. Others are called
    as goals.
    ")
]).

:- tool(compile_term/1, compile_term_/2).
:- set_flag(compile_term/1, type, built_in).
compile_term_(Clauses, Module) :-
	compile_term_(Clauses, [], Module).


:- comment(compile_term/2, [
    summary:"Compile a list of terms",
    args:["Clauses":"List of clauses and/or directives",
    	"Options":"List of compiler options"],
    amode:compile_term(+,++),
    see_also:[compile/1,compile/2,compile_stream/1,compile_term/1,struct(options)],
    desc:html("
    Compile a list of clauses and queries.
    Handling of directives: include/1, ./2 and currently module/1
    are not allowed.  Pragmas are interpreted. Others are treated
    according to the setting of the 'load' option.
    ")
]).
:- tool(compile_term/2, compile_term_/3).
:- set_flag(compile_term/2, type, built_in).

compile_term_(List, OptionList, Module) :-
        compile_term_annotated_(List, _, OptionList, Module).

:- comment(compile_term_annotated/3, [
    summary:"Compile a list of terms, possibly annotated with source information",
    args:["Clauses":"List of clauses and/or directives",
        "Annotated":"Annotated form of Clauses, or variable",
    	"Options":"List of compiler options"],
    amode:compile_term_annotated(+,?,++),
    see_also:[compile/1,compile_term/2,compile/2,compile_term/1,read_annotated/2,read_annotated/3,struct(options)],
    desc:html("
    Compile a list of clauses and queries, possibly with source information,
    supplied by Annotated. This predicate is provided to allow the user to
    compile clauses that the user generate from processing some source file,
    with the mapping to the original unprocessed source information
    supplied by Annotated. If Annotated is a variable, then no source 
    information is available, and the predicate behaves exactly like 
    compile_term/2. If Annotated is instantiated, it must corresponded to
    the annotated form of Clauses, i.e. as returned by read_annotated/2,3. 
    Handling of directives: include/1, ./2 and currently module/1
    are not allowed.  Pragmas are interpreted. Others are treated
    according to the setting of the 'load' option.
    ")
]).
:- tool(compile_term_annotated/3, compile_term_annotated_/4).
:- set_flag(compile_term_annotated/3, type, built_in).

compile_term_annotated_(List, AnnList, OptionList, Module) :-
%	writeln(compile_term(List,OptionList)),
	compiler_options_setup('_term', OptionList, Options),
	hash_create(PredsSeen),
	compile_list(List, AnnList, first, Clauses, Clauses, AnnC, AnnC,
                     0, Size, PredsSeen, Options, Module),
%	compiler_options_cleanup(Options).	% don't close files
	( Size < 0 ->
	    exit_block(abort)	% because of errors during compile
	;
	    true
	).


compile_list(Term, _, _, _, _, _, _, _, _, _PredsSeen, Options, Module) :- var(Term), !,
	error(#inst_fault, compile_term(Term, Options), Module).
compile_list([], _, Pred, Clauses, Tail, AnnC, AnnCTail, Size0, Size, PredsSeen, Options, _Module) :- !,
	Tail = [],
        AnnCTail = [],
	compile_predicate(Pred, Clauses, AnnC, term, PredsSeen, Options, Size1),
	Size is Size0+Size1.
compile_list([Term|Terms], AnnTermList, Pred, Clauses, Tail, AnnC, AnnCTail, Size0, Size, PredsSeen, Options, Module) :- !,
        (nonvar(AnnTermList) -> 
            AnnTermList = annotated_term{term:[AnnTerm|AnnTerms]}
        ;
            true
        ),
        ( var(Term) ->
	    error(#inst_fault, compile_term([Term|Terms], Options), Module)

	; Term = (:-_) ->
	    % separator, compile the preceding predicate
	    Tail = [],
            AnnCTail = [],
	    compile_predicate(Pred, Clauses, AnnC, term, PredsSeen, Options, Size1),
	    Size2 is Size0+Size1,
	    % unlike compile(file), interpret only pragmas,
	    % not directives like module/1, include/1, etc
	    ( consider_pragmas(Term, Options, Module) ->
		true
	    ;
		process_directive(no_source, Term, Options, Module)
	    ),
	    compile_list(Terms, AnnTerms, none, Clauses1, Clauses1,
                         AnnC1, AnnC1, Size2, Size, PredsSeen, Options, Module)

        ; Term = (?-_) ->
	    % separator, compile the preceding predicate
	    Tail = [],
            AnnCTail = [],
	    compile_predicate(Pred, Clauses, AnnC, term, PredsSeen, Options, Size1),
	    Size2 is Size0+Size1,
	    process_query(no_source, Term, Options, Module),
	    compile_list(Terms, AnnTerms, none, Clauses1, Clauses1,
                         AnnC1, AnnC1, Size2, Size, PredsSeen, Options, Module)
	; callable(Term) ->
	    optional_expansion(Term, AnnTerm, TransTerm, AnnTrans, Options, Module),
	    % TransTerm may be a list of clauses!
	    accumulate_clauses(TransTerm, AnnTrans, Module, Options, term, PredsSeen,
		    Size0, Pred, term, Clauses, Tail, AnnC, AnnCTail,
		    Size1, Pred1, _Pos, Clauses1, Tail1, AnnC1, AnnCTail1),
	    compile_list(Terms, AnnTerms, Pred1, Clauses1, Tail1, 
                    AnnC1, AnnCTail1, Size1, Size, PredsSeen, Options, Module)
	;
	    ( block(compiler_event(#illegal_head, term, AnnTerm, Term, Module), abort_compile_predicate, true) -> true ; true ),
	    Size = -1.0Inf
	).
compile_list(Term, AnnTerm, Pred, Clauses, Tail, AnnC, AnnCTail, Size0, Size, PredsSeen, Options, Module) :-
	( Pred == first ->
	    % allow to omit list brackets for single term
            (nonvar(AnnTerm) ->
                AnnTermList = annotated_term{term:[annotated_term{term:AnnTerm}|annotated_term{term:[]}]}
            ;
                true
            ),
	    compile_list([Term], AnnTermList, none, Clauses, Tail, AnnC, AnnCTail, Size0, Size, PredsSeen, Options, Module)
	;
	    error(#type_error, compile_term(Term, Options), Module)
	).

    optional_expansion(Term, AnnTerm, TransTerm, AnnTransTerm, options{expand_clauses:CFlag,expand_goals:GFlag}, Module) :-
	expand_clause_and_goals(Term, AnnTerm, TransTerm, AnnTransTerm, CFlag, GFlag)@Module.

