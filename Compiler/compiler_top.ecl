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
% Version:	$Id: compiler_top.ecl,v 1.5 2007/02/22 01:31:56 jschimpf Exp $
% ----------------------------------------------------------------------

:- module(compiler_top).

:- comment(summary, "ECLiPSe III compiler - toplevel predicates").
:- comment(copyright, "Cisco Technology Inc").
:- comment(author, "Joachim Schimpf").
:- comment(date, "$Date: 2007/02/22 01:31:56 $").

:- comment(desc, html("
	This module contains the toplevel predicates for invoking the
	compiler. This is where the different compiler passes are chained
	together, and where the compiler options are defined. When compiling
	clause lists, there is not much more to it.
	<P>
	Moreover, this module currently contains the \"from file\" compiler,
	i.e. the code to process source files, interpret directives and
	invoke the compiler proper for each predicate in the file.
    ")).


:- use_module(compiler_common).
:- use_module(compiler_normalise).
:- use_module(compiler_analysis).
:- use_module(compiler_peephole).
:- use_module(compiler_codegen).
:- use_module(compiler_varclass).
:- use_module(compiler_indexing).

:- lib(asm).
:- lib(module_options).


% ----------------------------------------------------------------------

:- comment(compile_pred/2, [
    args:[
	"Clauses":"A list of clauses for a single predicate",
	"Options":"A list of options of the form name:value"
    ],
    amode:compile_pred(+,++),
    summary:"Compile a single predicate",
    desc:html("
    	Takes a list of clauses (which must all be for the same predicate),
	and compile them. The result can either be stored as code for the
	predicate in memory (option output:store, the default), or the
	resulting WAM code can simply be printed (option output:print).
    "),
    see_also:[struct(options)],
    eg:"
    ?- compile_pred([p(99) :- q], [output:print]).
    p / 1:
	    get_integer              a(1)     99 
	    jmp                      q / 0 
    "
]).

:- export compile_pred/2.
:- tool(compile_pred/2, compile_pred/3).
compile_pred(Clauses, OptionList, Module) :-
        compile_pred_annotated(Clauses, OptionList, _Ann, _Files, Module).

compile_pred_annotated(Clauses, OptionList, AnnClauses, Files, Module) :-
	( OptionList = options{} ->
	    Options = OptionList
	; get_options(OptionList, Options)@compiler_common ->
	    true
	;
	    printf(error, "Invalid option list: %w%n", [OptionList]),
	    print_default_options(error)@compiler_common,
	    abort
	),
	compile_pred_to_wam_annotated(Clauses, AnnClauses, Files, WAM, OptionList, Module),
	Clauses = [Clause|_],
	extract_pred(Clause, F, A),
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
	;
	    block(
		asm(F/A, WAM, Module),
		_,
		true
	    )
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



% Compile a predicate (list of clauses) to WAM code list

:- export compile_pred_to_wam/3.
:- tool(compile_pred_to_wam/3, compile_pred_to_wam/4).
compile_pred_to_wam(Clauses, FinalCode, OptionList, Module) :-
        compile_pred_to_wam_annotated(Clauses, _Ann, _Files, FinalCode, OptionList, Module).

compile_pred_to_wam_annotated(Clauses, AnnCs, Files, FinalCode, OptionList,
                              Module) :-
	( OptionList = options{} ->
	    Options = OptionList
	; get_options(OptionList, Options)@compiler_common ->
	    true
	;
	    printf(error, "Invalid option list: %w%n", [OptionList]),
	    print_default_options(error)@compiler_common,
	    abort
	),

	% Create our normal form
	normalize_clauses_annotated(Clauses, AnnCs, [], Files, NormPred0, _NVars, Module),
%	print_normalized_clause(output, NormPred0),

	% Do some intra-predicate flow analysis
	binding_analysis(NormPred0),

	% Here we could have a simplification pass, exploiting
	% information from the analysis phase.

	% Indexing_transformation needs info from binding_analysis
	indexing_transformation(NormPred0, NormPred),

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
	assign_env_slots(NormPred, Lifetimes, EnvSize),
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
	simplify_code(Code, FinalCode),
	( Options = options{print_final_code:on} ->
	    print_annotated_code(FinalCode)
	;
	    true
	).


%----------------------------------------------------------------------
% From-file compiler
%----------------------------------------------------------------------

:- lib(source_processor).

:- export compile_file/2.

:- tool(compile_file/2, compile_file/3).

compile_file(File, OptionList, Module) :-
	compiler_options_setup(File, OptionList, Options),
	source_open(File, [goal_expansion], SourcePos0)@Module,
	(
	    fromto(begin, _, Class, end),
	    fromto(SourcePos0, SourcePos1, SourcePos2, SourcePosEnd),
	    fromto(ClauseTail, Clauses0, Clauses1, []),
	    fromto(ClauseTail, ClauseTail0, ClauseTail1, []),
	    fromto(AnnClauseTail, AnnClauses0, AnnClauses1, []),
	    fromto(AnnClauseTail, AnnClauseTail0, AnnClauseTail1, []),
	    fromto(FileTail, Files0, Files1, []),
	    fromto(FileTail, FileTail0, FileTail1, []),
	    fromto(none, Pred0, Pred1, none),
	    param(Options)
	do
	    source_read(SourcePos1, SourcePos2, Class, SourceTerm),
            SourcePos1 = source_position{module:PosModule,file:CFile},
            SourceTerm = source_term{term:Term,annotated:Ann},
            
	    ( Class = clause ->
		extract_pred(Term, N, A),
		Pred1 = PosModule:N/A,
		( Pred1 = Pred0 ->		% new clause for same pred
		    ClauseTail0 = [Term|ClauseTail1],
                    Clauses1 = Clauses0,
                    AnnClauseTail0 = [Ann|AnnClauseTail1],
                    AnnClauses1 = AnnClauses0,
                    FileTail0 = [CFile|FileTail1],
                    Files1 = Files0
		;
		    ClauseTail0 = [],		% new pred, compile previous
                    AnnClauseTail0 = [],
                    FileTail0 = [],
                    compile_predicate(Pred0, Clauses0, AnnClauses0, Files0, Options),
		    Clauses1 = [Term|ClauseTail1],
                    AnnClauses1 = [Ann|AnnClauseTail1],
                    Files1 = [CFile|FileTail1]
		)

	    ; Class = comment ->		% comment, ignore
		Pred1 = Pred0,
		ClauseTail1 = ClauseTail0,
		Clauses1 = Clauses0,
		AnnClauseTail1 = AnnClauseTail0,
		AnnClauses1 = AnnClauses0,
		FileTail1 = FileTail0,
		Files1 = Files0

	    ; % other classes are taken as predicate separator
		ClauseTail0 = [],		% compile previous predicate
                AnnClauseTail0 = [],
                FileTail0 = [],
                compile_predicate(Pred0, Clauses0, AnnClauses0, Files0, Options),
		Clauses1 = ClauseTail1,
                AnnClauses1 = AnnClauseTail1,
                Files1 = FileTail1,
		Pred1 = none,

		( Class = directive ->
		    call_directive(SourcePos1, Term, PosModule)
		; Class = query ->
		    call_directive(SourcePos1, Term, PosModule)
		; Class = handled_directive ->
		    consider_pragmas(Term, Options)
		; Class = var ->
		    compiler_error(4, SourcePos1, SourceTerm)
		;
		   true
		)
	    )
	),
	source_close(SourcePosEnd, [keep_modules]),
	compiler_options_cleanup(Options).


    compile_predicate(_, [], _, _, _) :- !.
    compile_predicate(M:NA, Clauses, AClauses, Files, Options) :-
	writeln(log_output, compiling(M:NA)),
        compile_pred_annotated(Clauses, Options, AClauses, Files, M).


    extract_pred(Head :- _, N, A) :- !,
    	functor(Head, N, A).
    extract_pred(Head ?- _, N, A) :- !,
    	functor(Head, N, A).
    extract_pred((Head if _), N, A) :- !,
    	functor(Head, N, A).
    extract_pred(Fact, N, A) :-
    	functor(Fact, N, A).


    call_directive(source_position{file:F,line:L}, Dir, Module) :-
	arg(1, Dir, Goal),
    	block(
	    ( call(Goal)@Module ->
	    	true
	    ;
		printf(warning_output, "Compiler: query failed in file %w, line %d.%n", [F,L])
	    ),
	    Tag,
	    printf(warning_output, "Compiler: query exited (%w) in file %w, line %d.%n", [Tag, F,L])
	).


    compiler_options_setup(File, OptionList, Options) :-

	% Consider global settings for the compiler options
	get_flag(debug_compile, Dbgcomp),
	set_default_option(dbgcomp, Dbgcomp)@compiler_common,

	( get_options(OptionList, Options0)@compiler_common ->
	    true
	;
	    printf(error, "Invalid option list: %w%n", [OptionList]),
	    print_default_options(error)@compiler_common,
	    abort
	),
	( Options0 = options{output:listing(File)} ->
	    open(File,write,ListingStream),
	    update_struct(options, [output:print(ListingStream)], Options0, Options)
	; Options0 = options{output:listing} ->
	    concat_string([File], FileS),
	    pathname(FileS, _Dir, _Base, Suffix),
	    string_length(Suffix, SufLen),
	    substring(FileS, _, _, SufLen, Base),
	    concat_string([Base,'.lst'], LstFile),
	    open(LstFile,write,ListingStream),
	    update_struct(options, [output:print(ListingStream)], Options0, Options)
	;
	    Options = Options0
	).


    compiler_options_cleanup(Options) :-
    	( Options = options{output:print(ListingStream)} -> close(ListingStream) ; true ).


    compiler_error(N, source_position{file:F,line:L},
    		source_term{term:Term}) :-
	error_id(N, Message),
	printf(error, "Compiler: %w in file %w, line %d:%n%Qw%n",
		[Message,F,L,Term]).


% Adjust compiler options according to pragmas

consider_pragmas(:-(Dir), Options) :-
	consider_pragmas1(Dir, Options).

consider_pragmas1((D1,D2), Options) ?- !,
	consider_pragmas1(D1, Options),
	consider_pragmas1(D2, Options).
consider_pragmas1(pragma(Pragma), Options) ?- !,
	consider_pragma(Pragma, Options).
consider_pragmas1(_, _).

consider_pragma(debug, Options) :- setarg(dbgcomp of options, Options, on).
consider_pragma(nodebug, Options) :- setarg(dbgcomp of options, Options, off).


