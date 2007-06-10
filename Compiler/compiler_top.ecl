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
% Version:	$Id: compiler_top.ecl,v 1.11 2007/06/10 22:10:30 jschimpf Exp $
% ----------------------------------------------------------------------

:- module(compiler_top).

:- comment(summary,	"ECLiPSe III compiler - toplevel predicates").
:- comment(copyright,	"Cisco Technology Inc").
:- comment(author,	"Joachim Schimpf").
:- comment(date,	"$Date: 2007/06/10 22:10:30 $").

:- comment(desc, html("
    This module contains the toplevel predicates for invoking the
    compiler. This is where the different compiler passes are chained
    together, where the compiler options are defined, and the code
    to process source files, and to interpret directives and queries.
    <P>
    The top-level interfaces to the compiler are: compile/1,2 for
    compilation from files, compile_term/1,2 for compiling data, and
    fcompile/1,2 for backward compatibility.
")).


:- use_module(compiler_common).
:- use_module(compiler_normalise).
:- use_module(compiler_analysis).
:- use_module(compiler_peephole).
:- use_module(compiler_codegen).
:- use_module(compiler_varclass).
:- use_module(compiler_indexing).

:- lib(asm).
:- lib(lists).
:- lib(module_options).
:- lib(source_processor).


%----------------------------------------------------------------------
% Compiler Options
%----------------------------------------------------------------------

compiler_options_setup(File, OptionList, Options) :-

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
	    concat_string([File], FileS),
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
	    concat_string([File], FileS),
	    pathname(FileS, Dir, Base, _Suffix),
	    ( concat_string([OutDir], "") -> 
		concat_string([Dir,Base,'.eco'], EcoFile)
	    ;
		concat_string([OutDir,/,Base,'.eco'], EcoFile)
	    ),
	    open(EcoFile,write,Stream),
	    update_struct(options, [output:eco_to_stream(Stream)], Options0, Options)
	;
	    Options = Options0
	).


compiler_options_cleanup(Options) :-
    	( Options = options{output:print(Stream)} ->
	    close(Stream)
    	; Options = options{output:eco_to_stream(Stream)} ->
	    close(Stream)
	;
	    true
	).


% ----------------------------------------------------------------------
% Compile a single predicate.
% 
% Takes a list of clauses (which must all be for the same predicate),
% compile them, and store/output the result according to options.
% ----------------------------------------------------------------------

compile_predicate(_, [], _, _) :- !.
compile_predicate(Module:NA, Clauses, AnnClauses, Options) :-
	message(compiling(Module:NA), Options),
	compile_pred_to_wam(Clauses, AnnClauses, WAM, Options, Module),
	Clauses = [Clause|_],
	extract_pred(Clause, F, A),
	pred_flags(Options, Flags),
	(
	    ( Options = options{load:all}
	    ; Options = options{load:new}, \+ is_predicate(F/A)@Module )
	->
	    % double negation, because asm binds the labels
	    \+ \+ block(asm(F/A, WAM, Flags, Module), _, true)
	;
	    true % don't clobber existing code if not loading
	),
	( Options = options{output:print} ->
	    printf("%w:%n", [F/A]),
	    print_wam(WAM)
	; Options = options{output:print(Stream)} ->
	    writeclauses(Stream, Clauses),
	    get_stream(output, OldOut),
	    set_stream(output, Stream),
	    print_wam(WAM),
	    set_stream(output, OldOut),
	    writeln(Stream, --------------------)
	; Options = options{output:eco_to_stream(Stream)} ->
	    pasm(WAM, Size, Codes),
	    CodeArr =.. [[]|Codes],
	    ( Module == sepia_kernel ->
		% call locally, because :/2 may not be defined yet
		StorePred = store_pred(F/A,CodeArr,Size,Flags)
	    ;
		StorePred = sepia_kernel:store_pred(F/A,CodeArr,Size,Flags)
	    ),
	    printf(Stream, "%ODQKw.%n", [:-StorePred])@Module
	; Options = options{output:none} ->
	    true
	;
	    Options = options{output:Junk},
	    printf(error, "Invalid output option: %w%n", [Junk]),
	    abort
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


    pred_flags(options{debug:Debug,system:System,skip:Skip}, Flags) ?-
	( Debug==on -> Flags0 = 16'00008000 ; Flags0 = 0 ),			% DEBUG_DB
	( System==on -> Flags1 is Flags0 \/ 16'40000000 ; Flags1 = Flags0 ),	% SYSTEM
	( Skip==on -> Flags is Flags1 \/ 16'00040000 ; Flags = Flags1 ).	% DEBUG_SK


    set_pred_flags(options{debug:Debug,system:System,skip:Skip}, Pred, Module) ?-
	set_flag(Pred, debugged, Debug)@Module,
	set_flag(Pred, skip, Skip)@Module,
	( System==on -> Type = built_in ; Type = user ),
	set_flag(Pred, type, Type)@Module.



% Compile a predicate (list of clauses) to WAM code list
% This chains together the various stages of the comiler

compile_pred_to_wam(Clauses, AnnCs, FinalCode, Options, Module) :-

	% Create our normal form
	normalize_clauses_annotated(Clauses, AnnCs, [], NormPred0, _NVars, Module),
%	print_normalized_clause(output, NormPred0),

	% Do some intra-predicate flow analysis
	binding_analysis(NormPred0),

	% Here we could have a simplification pass, exploiting
	% information from the analysis phase.

	% Indexing_transformation (needs info from binding_analysis)
	indexing_transformation(NormPred0, NormPred, Options),

	% Variable classification
	% compute_lifetimes must be after indexing transformation, because
	% indexing_transformation introduces extra variable occurrences.
	compute_lifetimes(NormPred, Lifetimes),
	( Options = options{print_lifetimes:on} ->
	    printf("------ Lifetime results ------%n", []),
	    print_occurrences(Lifetimes)
	;
	    true
	),
	assign_env_slots(NormPred, Lifetimes, EnvSize, Options),
	( Options = options{print_normalised:on} ->
	    print_normalized_clause(output, NormPred)
	;
	    true
	),
	( Options = options{print_lifetimes:on} ->
	    printf("------ Environment size %d ------%n", [EnvSize]),
	    print_occurrences(Lifetimes)
	;
	    true
	),

	% Code generation
	generate_code(NormPred, EnvSize, Code, [], Options, Module),
	( Options = options{print_raw_code:on} ->
	    print_annotated_code(Code)
	;
	    true
	),

	% WAM level postprocessing
	simplify_code(Code, FinalCode, Options),
	( Options = options{print_final_code:on} ->
	    print_annotated_code(FinalCode)
	;
	    true
	).


%----------------------------------------------------------------------
% From-file compiler
%----------------------------------------------------------------------

:- export compile/1.
:- comment(compile/1, [
    summary:"Compile a file",
    args:["File":"File name (atom or string)"],
    amode:compile(++),
    see_also:[compiler_top:compile/2,compile_term/1,compile_term/2,struct(options)],
    desc:html("
    Compile a file with default options.  The resulting code is directly
    loaded into memory and ready for execution.  Equivalent to
    <PRE>
	compile(File, [])
    </PRE>
    ")
]).
:- tool(compile/1, compile_/2).
compile_(File, Module) :-
    compile_(File, [], Module).


:- export compile/2.
:- comment(compile/2, [
    summary:"Compile a file",
    args:["File":"File name (atom or string)",
    	"Options":"List of compiler options"],
    amode:compile(++,++),
    see_also:[compiler_top:compile/1,compile_term/1,compile_term/2,struct(options)],
    desc:html("
    Compile a file, with output according to options.
    ")
]).
:- tool(compile/2, compile_/3).

compile_(File, OptionListOrModule, CM) :-
	% for backward compatibility, allow compile(File, Module)
	( atom(OptionListOrModule), OptionListOrModule \== [] ->
	    Module = OptionListOrModule, OptionList = []
	;
	    Module = CM, OptionList = OptionListOrModule
	
	),
	compiler_options_setup(File, OptionList, Options),
	sepia_kernel:register_compiler(args(Term,Ann)-(compiler_top:compile_term_annotated(Term,Ann,Options))),
	( Options = options{load:all} ->
	    OpenOptions = [recreate_modules],
	    CloseOptions = [keep_modules]
	;
	    OpenOptions = [],
	    CloseOptions = []
	),
	source_open(File, [with_annotations,goal_expansion|OpenOptions], SourcePos0)@Module,
	(
	    fromto(begin, _, Class, end),
	    fromto(SourcePos0, SourcePos1, SourcePos2, SourcePosEnd),
	    fromto(ClauseTail, Clauses0, Clauses1, []),
	    fromto(ClauseTail, ClauseTail0, ClauseTail1, []),
	    fromto(AnnClauseTail, AnnClauses0, AnnClauses1, []),
	    fromto(AnnClauseTail, AnnClauseTail0, AnnClauseTail1, []),
	    fromto(none, Pred0, Pred1, none),
	    param(Options)
	do
	    source_read(SourcePos1, SourcePos2, Class, SourceTerm),
            SourcePos1 = source_position{module:PosModule},
            SourceTerm = source_term{term:Term,annotated:Ann},

            ( Class = clause ->
		accumulate_clauses(Term, Ann, PosModule, Options,
		    Pred0, Clauses0, ClauseTail0, AnnClauses0, AnnClauseTail0,
		    Pred1, Clauses1, ClauseTail1, AnnClauses1, AnnClauseTail1)

	    ; Class = comment ->		% comment, ignore
		Pred1 = Pred0,
		ClauseTail1 = ClauseTail0,
		Clauses1 = Clauses0,
		AnnClauseTail1 = AnnClauseTail0,
		AnnClauses1 = AnnClauses0

	    ; % other classes are taken as predicate separator
		ClauseTail0 = [],		% compile previous predicate
                AnnClauseTail0 = [],
                compile_predicate(Pred0, Clauses0, AnnClauses0, Options),
		Clauses1 = ClauseTail1,
                AnnClauses1 = AnnClauseTail1,
		Pred1 = none,

		( Class = directive ->
		    ( old_compiler_directive(Term, Options) ->
			true
		    ;
			process_directive(SourcePos1, Term, Options, PosModule)
		    )
		; Class = query ->
		    process_query(SourcePos1, Term, Options, PosModule)
		; Class = handled_directive ->
		    ( consider_pragmas(Term, Options, PosModule) -> true ; true ),
		    emit_directive_or_query(Term, Options, PosModule)
		; Class = var ->
		    compiler_error(4, SourcePos1, SourceTerm)
		;
		   true
		)
	    )
	),
	source_close(SourcePosEnd, CloseOptions),
	sepia_kernel:deregister_compiler,
	compiler_options_cleanup(Options).


% Add a single clause or a list of clauses to what we already have.
% If a predicate is finished, compile it.
:- mode accumulate_clauses(+,+,+,+,+,?,-,?,-,-,-,-,-,-).
accumulate_clauses([], [], _Module, _Options,
		Pred0, PredCl0, PredClTl0, PredClAnn0, PredClAnnTl0,
		Pred0, PredCl0, PredClTl0, PredClAnn0, PredClAnnTl0) :-
	!.
accumulate_clauses([Term|Terms], [AnnTerm|AnnTerms], Module, Options,
		Pred0, PredCl0, PredClTl0, PredClAnn0, PredClAnnTl0,
		Pred, PredCl, PredClTl, PredClAnn, PredClAnnTl) :-
	!,
	extract_pred(Term, N, A),
	Pred1 = Module:N/A,
	( Pred0 == Pred1 ->
	    % another clause for Pred0
	    PredClTl0 = [Term|PredClTl1],
	    PredClAnnTl0 = [AnnTerm|PredClAnnTl1],
	    accumulate_clauses(Terms, AnnTerms, Module, Options, 
	    	Pred0, PredCl0, PredClTl1, PredClAnn0, PredClAnnTl1,
		Pred, PredCl, PredClTl, PredClAnn, PredClAnnTl)
	;
	    % first clause for next predicate Pred1, compile Pred0
	    PredClTl0 = [], PredClAnnTl0 = [],
	    compile_predicate(Pred0, PredCl0, PredClAnn0, Options),
	    PredCl1 = [Term|PredClTl1],
	    PredClAnn1 = [AnnTerm|PredClAnnTl1],
	    accumulate_clauses(Terms, AnnTerms, Module, Options,
	    	Pred1, PredCl1, PredClTl1, PredClAnn1, PredClAnnTl1,
		Pred, PredCl, PredClTl, PredClAnn, PredClAnnTl)
	).
accumulate_clauses(Term, AnnTerm, Module, Options,
		Pred0, PredCl0, PredClTl0, PredClAnn0, PredClAnnTl0,
		Pred, PredCl, PredClTl, PredClAnn, PredClAnnTl) :-
	accumulate_clauses([Term], [AnnTerm], Module, Options,
		Pred0, PredCl0, PredClTl0, PredClAnn0, PredClAnnTl0,
		Pred, PredCl, PredClTl, PredClAnn, PredClAnnTl).


    extract_pred(Head :- _, N, A) :- !,
    	functor(Head, N, A).
    extract_pred(Fact, N, A) :-
    	functor(Fact, N, A).


compiler_error(N, source_position{file:F,line:L},
    		source_term{term:Term}) :-
	error_id(N, Message),
	printf(error, "Compiler: %w in file %w, line %d:%n%Qw%n",
		[Message,F,L,Term]).



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
	    ( call(Goal)@Module ->
	    	true
	    ;
		write(warning_output, "Compiler warning: query failed"),
		print_location(warning_output, SourcePos, Dir)
	    ),
	    Tag,
	    (
		printf(warning_output, "Compiler warning: query exited (%w)", [Tag]),
		print_location(warning_output, SourcePos, Dir)
	    )
	).

    print_location(Stream, source_position{file:F,line:L}, _Culprit) :- !,
	printf(Stream, " in file %w, line %d.%n", [F,L]).
    print_location(Stream, _, Culprit) :-
	printf(Stream, " %w%n", [Culprit]).
    	

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
consider_pragma(expand, Options, _) :- !,
	setarg(expand_goals of options, Options, on).
consider_pragma(noexpand, Options, _) :- !,
	setarg(expand_goals of options, Options, off).
consider_pragma(Pragma, _, M) :-
	error(148, pragma(Pragma), M).	% make accessible via current_pragma/1


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

:- export compile_term/1.
:- comment(compile_term/1, [
    summary:"Compile a list of terms",
    args:["Clauses":"List of clauses and/or directives"],
    amode:compile_term(+),
    see_also:[compiler_top:compile/1,compile/2,compile_term/2],
    desc:html("
    Compile a list of clauses and queries.
    Handling of directives: include/1, ./2 and currently module/1
    are not allowed.  Pragmas are interpreted. Others are called
    as goals.
    ")
]).

:- tool(compile_term/1, compile_term_/2).
compile_term_(Clauses, Module) :-
	compile_term_(Clauses, [], Module).


:- export compile_term/2.
:- comment(compile_term/2, [
    summary:"Compile a list of terms",
    args:["Clauses":"List of clauses and/or directives",
    	"Options":"List of compiler options"],
    amode:compile_term(+,++),
    see_also:[compiler_top:compile/1,compile/2,compile_term/1,struct(options)],
    desc:html("
    Compile a list of clauses and queries.
    Handling of directives: include/1, ./2 and currently module/1
    are not allowed.  Pragmas are interpreted. Others are treated
    according to the setting of the 'load' option.
    ")
]).
:- tool(compile_term/2, compile_term_/3).

compile_term_(List, OptionList, Module) :-
        compile_term_annotated_(List, _, OptionList, Module).

:- export compile_term_annotated/3.
:- comment(compile_term_annotated/3, [
    summary:"Compile a list of terms, possibly annotated with source information",
    args:["Clauses":"List of clauses and/or directives",
        "Annotated":"Annotated form of Clauses, or variable"
    	"Options":"List of compiler options"],
    amode:compile_term_annotated(+,?,++),
    see_also:[compiler_top:compile/1,compiler_top:compile_term/2,compile/2,compile_term/1,read_annotated/2,read_annotated/3,struct(options)],
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

compile_term_annotated_(List, AnnList, OptionList, Module) :-
%	writeln(compile_term(List,OptionList)),
	compiler_options_setup('_term', OptionList, Options),
	compile_list(List, AnnList, first, Clauses, Clauses, AnnC, AnnC,
                     Options, Module),
%	compiler_options_cleanup(Options).
	true.	% don't close files


compile_list(Term, _, _, _, _, _, _, Options, Module) :- var(Term), !,
	error(4, compile_term(Term, Options), Module).
compile_list([], _, Pred, Clauses, Tail, AnnC, AnnCTail, Options, _Module) :- !,
	Tail = [],
        AnnCTail = [],
	compile_predicate(Pred, Clauses, AnnC, Options).
compile_list([Term|Terms], AnnTermList, Pred, Clauses, Tail, AnnC, AnnCTail, Options, Module) :- !,
        (nonvar(AnnTermList) -> 
            AnnTermList = annotated_term{term:[AnnTerm|AnnTerms]}
        ;
            true
        ),
        ( var(Term) ->
	    error(4, compile_term([Term|Terms], Options), Module)

	; Term = (:-_) ->
	    % separator, compile the preceding predicate
	    Tail = [],
            AnnCTail = [],
	    compile_predicate(Pred, Clauses, AnnC, Options),
	    % unlike compile(file), interpret only pragmas,
	    % not directives like module/1, include/1, etc
	    ( consider_pragmas(Term, Options, Module) ->
		true
	    ;
		process_directive(no_source, Term, Options, Module)
	    ),
	    compile_list(Terms, AnnTerms, none, Clauses1, Clauses1,
                         AnnC1, AnnC1, Options, Module)

        ; Term = (?-_) ->
	    % separator, compile the preceding predicate
	    Tail = [],
            AnnCTail = [],
	    compile_predicate(Pred, Clauses, AnnC, Options),
	    process_query(no_source, Term, Options, Module),
	    compile_list(Terms, AnnTerms, none, Clauses1, Clauses1,
                         AnnC1, AnnC1, Options, Module)
	;
	    optional_expansion(Term, AnnTerm, TransTerm, AnnTrans, Options, Module),
	    % TransTerm may be a list of clauses!
	    accumulate_clauses(TransTerm, AnnTrans, Module, Options,
		    Pred, Clauses, Tail, AnnC, AnnCTail,
		    Pred1, Clauses1, Tail1, AnnC1, AnnCTail1),
	    compile_list(Terms, AnnTerms, Pred1, Clauses1, Tail1, 
                    AnnC1, AnnCTail1, Options, Module)
	).
compile_list(Term, AnnTerm, Pred, Clauses, Tail, AnnC, AnnCTail, Options, Module) :-
	( Pred == first ->
	    % allow to omit list brackets for single term
            (nonvar(AnnTerm) ->
                AnnTermList = annotated_term{term:[annotated_term{term:AnnTerm}|annotated_term{term:[]}]}
            ;
                true
            ),
	    compile_list([Term], AnnTermList, none, Clauses, Tail, AnnC, AnnCTail, Options, Module)
	;
	    error(5, compile_term(Term, Options), Module)
	).

    optional_expansion(Term, AnnTerm, TransTerm, AnnTransTerm, options{expand_clauses:CFlag,expand_goals:GFlag}, Module) :-
	expand_clause_and_goals(Term, AnnTerm, TransTerm, AnnTransTerm, CFlag, GFlag)@Module.


%----------------------------------------------------------------------
% Compatibility: fcompile
%----------------------------------------------------------------------

:- export fcompile/1.
:- tool(fcompile/1, fcompile_/2).
fcompile_(File, Module) :-
	compile_(File, [output:eco,load:new], Module).

:- export fcompile/2.
:- tool(fcompile/2, fcompile_/3).
fcompile_(File, Options0, Module) :-
	% translate old fcompile's compile-option into load-option
	( delete(compile:CompileOpt, Options0, Options1) ->
	    ( memberchk(load:LoadOpt, Options1) ->
	        ( option_compile_load(CompileOpt, LoadOpt) ->
		    Options2 = Options1
		;
		    printf(error, "Incompatible compile/load options: %w%n",
		    	[fcompile(File, Options0)]),
		    abort
		)
	    ; CompileOpt == yes ->
		Options2 = [load:all|Options1]
	    ;
		Options2 = [load:new|Options1]
	    )
	;
	    ( memberchk(load:_LoadOpt, Options0) ->
		Options2 = Options0
	    ;
		Options2 = [load:new|Options0]
	    )
	),
	( delete(output:_, Options2, Options3) ->
	    printf(error, "Ignoring output: option, using output:eco%n",[])
	;
	    Options3 = Options2
	),
	Options = [output:eco|Options3],
	compile_(File, Options, Module).

    option_compile_load(yes, all).
    option_compile_load(no, new).


%----------------------------------------------------------------------
% Compiler bootstrapping
%----------------------------------------------------------------------

:- export comp/0, list/0, load/0.

comp :-
	Options = [output:eco,load:new,verbose:0],
	compile(compiler_common,	Options),
	compile(compiler_normalise,	Options),
	compile(compiler_analysis,	Options),
	compile(compiler_peephole,	Options),
	compile(compiler_codegen,	Options),
	compile(compiler_varclass,	Options),
	compile(compiler_indexing,	Options),
	compile(compiler_regassign,	Options),
	compile(compiler_top,		Options),
	true.

list :-
	Options = [output:listing,load:new,verbose:1],
	compile(compiler_common,	Options),
	compile(compiler_normalise,	Options),
	compile(compiler_analysis,	Options),
	compile(compiler_peephole,	Options),
	compile(compiler_codegen,	Options),
	compile(compiler_varclass,	Options),
	compile(compiler_indexing,	Options),
	compile(compiler_regassign,	Options),
	compile(compiler_top,		Options),
	true.

load :-
	sepia_kernel:compile("compiler_common.eco"),
	sepia_kernel:compile("compiler_normalise.eco"),
	sepia_kernel:compile("compiler_analysis.eco"),
	sepia_kernel:compile("compiler_peephole.eco"),
	sepia_kernel:compile("compiler_codegen.eco"),
	sepia_kernel:compile("compiler_varclass.eco"),
	sepia_kernel:compile("compiler_indexing.eco"),
	sepia_kernel:compile("compiler_regassign.eco"),
	sepia_kernel:compile("compiler_top.eco"),
	true
