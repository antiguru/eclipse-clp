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
% Copyright (C) 1992-2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): ECRC GmbH
% Contributor(s): IC-Parc, Imperal College London
% 
% END LICENSE BLOCK
%
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: fcompile.pl,v 1.1 2006/09/23 01:55:15 snovello Exp $
% ----------------------------------------------------------------------

%
% SEPIA PROLOG LIBRARY MODULE
%
% IDENTIFICATION:	fcompile.pl
%
% AUTHORS:		Kish Shen
%
% CONTENTS:		fcompile(+FileName)
%                       fcompile(+FileName, +OptionsList)
%              
% DESCRIPTION:
%
%         Compiles the file FileName and generates a file with .eco extension.
%         The .eco file is a platform-independent object representation of 
%         Filename, with source predicates replaced by directives that loads 
%         the object code for the predicates into memory. Source directives
%         remain in place. The order of the code is the same as that in the
%         source file for compatibility. Auxiliary predicates are generated
%         as they are encountered, and also at the end.


:- module(fcompile).

:- lib(asm).

:- lib(hash).

:- export fcompile/1, fcompile/2.


:- local struct(fc_options(
	outdir,		% string/atom, Output directory
	verbose,        % yes/no, messages printed per predicate, etc.
	format,         % byte/text, if output is in byte or text format
	compile         % yes/no, if fcompile will try to compile file first
    )).

:- tool(fcompile/1, fcompile_body/2).

:- tool(fcompile/2, fcompile_body/3).


:- comment(summary, "Generate platform independent object files").

:- comment(desc, 
html("The fcompile library is used to generate object files from 
ECLiPSe source files.  
<P>
An object file has suffix 'eco', and is a platform independent object
format which can be loaded into other sessions of ECLiPSe, much as an
ECLiPSe source file. The may however not be portable between different
versions of ECLiPSe.
")).
   
:- import get_bip_error/1,
	  set_bip_error/1,
          dump_term/3,
	  dump_header/1
   from sepia_kernel.





:- comment(fcompile/1, 
[summary: "Generates a byte-form object file from the ECLiPSe source File.",
 args: ["File":"Name of source file (Atom or string)"], 
 amode: fcompile(++),
 see_also: [fcompile/2], 
 desc: html("\
  <P>Same as <TT>fcompile/2</TT> with all options set to their default
     values: the object file is placed in the current working directory,
     byte format is generated, non-verbose, and fcompile tries to
     compile the program first.</P>")
]).

:- comment(fcompile/2, 
[
summary: "Generates a object file from the ECLiPSe source File with specified options.",
args: ["File":"Name of source file (Atom or string)",
        "Options":"List of valid options and their values"],
see_also: [fcompile/1], 
amode: fcompile(++,++), 
eg:    "\
   fcompile(my_prog, []).   % equivalent to fcompile(my_prog)

   fcompile(my_prog, [format:text, outdir:'../']).
   % generate the object file in text format, and place it in the parent dir
",
desc: html("\
<P>
   Compiles the specified Prolog source file and generates an object file
   with the same base name as the source file but with suffix <TT>.eco</TT>.
   Object files can be loaded by the built-in predicates <TT>use_module/1</TT>,
   <TT>lib/1</TT>, <TT>compile/1</TT> and <TT>ensure_loaded/1</TT>, and also
   with the eclipse -b command-line option.
</P><P>
   File must be instantiated to a legitimate specification for an existing
   file except for the suffix, which may be omitted. Options is a list of
   Option:Value pairs where Option and value can be:
</P>
<DL>
   <DT>compile: YesOrNo

       <DD> YesOrNo is either the  atom yes or no. For 'yes', fcompile will
       try to first compile File (checking that it has not already been
       compiled first).  This is usually what is required, as it ensures
       that File can be properly read to generate the object file. The
       default is 'yes'.

   <DT>format: ByteOrText

      <DD>ByteOrText is either the atom <TT>byte</TT> or <TT>text</TT>.  If 'text', the
      object file will be in a textual format.  If 'byte', the object file
      will be in a binary format which is larger, but will load faster
      and is not human readable. The default is byte.

   <DT>outdir: OutputDirectory

       <DD> OutputDirectory is the directory where the generated object
       file will be placed. It can be an atom or a string. The default is
       the current working directory.

   <DT>verbose: YesOrNo

       <DD> YesOrNo is either the  atom yes or no. For 'yes', fcompile will
       report in detail the predicates it is dumping out. This is probably
       only needed for debugging fcompile, to trace its progress. The default
       is 'no'.
</DL><P>
   The predicate will look for File with a `source' suffix (i.e. no
   suffix, <TT>.ecl</TT> or <TT>.pl</TT>), compile the file by calling compile, and
   generate an object form of the source file with suffix <TT>.eco</TT>.
   The user should use <TT>include/1</TT> directives to include all files that are
   in the same module as the master file, and <TT>use_module/1</TT> directives for
   files that define a different module. Files mentioned in include
   directives will not need to be fcompiled separately for their object
   form.
</P><P>
   This object form can be loaded into an ECLiPSe session even on a
   different operating system/hardware platform from the one on which
   it was generated.  However, the object format may change incompatibly
   between different releases of ECLiPSe.
</P><P>
   The fcompile library does not need to be loaded in order to load the object
   file.  The built-in predicates <TT>ensure_loaded/1</TT>, <TT>use_module/1</TT>
   and <TT>lib/1,2</TT> will try to load an object file in preference to a
   source file, if both are present.  The compile built-ins on the
   other hand will prefer source files, unless the <TT>.eco</TT>
   suffix is explicitly given.
</P><P>
   It is recommended that object files always contain proper modules.
   If an object file contains module-free code, then loading it into
   an existing module that already has code can cause conflicts with
   auxiliary predicates (e.g. from <TT>do/2</TT> loop constructs).
</P><P>
   <EM>Restrictions:</EM>
<UL>
     <LI>macro definitions should be quoted using 
     <TT>no_macro_expansion/1</TT>, e.g.
     <PRE>
         :- local macro(no_macro_expansion(maxint), 9999).
     </PRE>

     <LI>directives in the module should not change the state 
         of compiled code.

     <LI>big integer constants between -2^63 and -2^31 and between
     	2^31 and 2^63 should not appear directly in the source, and
	will provoke a warning because the generated object code will
	not be portable between 32 and 64 bit machines.
</UL>
</P>")
]).


fcompile_body(File, Module) :-
	% same as calling fcompile/2 with Options uninstantiated
	fcompile_body(File, [], Module).


fcompile_body(File, OptionsList, Module) :-
	% compile must be done before streams are set
	atom_or_string(File), 
        % fill in any unspecified options with defaults
	set_options(OptionsList, fc_options with [ 
             outdir:  OutDir,
	     verbose: Verbose,
	     format:  Format,
	     compile: Compile
        ]), 
	get_inout_names(File, OutDir, InDir, InFile, OutFile), 

	( Compile == no -> true
	; ensure_compiled(InFile, Module) ),

	check_open(read, InFile, In),
	open("", string, OutS),
	getcwd(Cwd), cd_if_possible(InDir),
	block((fcompile1(In, OutS, OutFile, Format, Verbose, Module) ->
	           cd(Cwd), close(OutS), close(In)
	       ;   cd(Cwd), close(OutS), close(In), fail
	      ), Tag,
	      (cd(Cwd), close(OutS), close(In), 
	       printf(error, "fcompile(%w, %w) aborted.%n", [File,OptionsList]),
	       exit_block(Tag)
	      )
        ), !.
fcompile_body(File, OptionsList, _Module) :-
	get_bip_error(E),
	error(E, fcompile(File, OptionsList)).

set_options(List, Options) :-
	Options = fc_options with [],
	(fill_specified_options(List, Options) ->
	    fill_default_options(Options) 
	;
	    set_bip_error(6)
	).


fill_specified_options([], _).
fill_specified_options([Op|Ops], Options) :-
	set_valid_option(Op, Options),
	fill_specified_options(Ops, Options).

set_valid_option(outdir:OutDir, Options) ?-
	atom_or_string(OutDir),
        arg(outdir of fc_options, Options, OutDir).
set_valid_option(verbose:Verbose, Options) ?-
	(Verbose == yes ; Verbose == no), !,
	arg(verbose of fc_options, Options, Verbose).
set_valid_option(format:Format, Options) ?-
	(Format == text ; Format == byte), !,
	arg(format of fc_options, Options, Format).
set_valid_option(compile:Compile, Options) ?-
	(Compile == yes ; Compile == no), !,
	arg(compile of fc_options, Options, Compile).

fill_default_options(Options) :-
	Options = fc_options with [outdir:OutDir,verbose:Verbose,
                    format:Format,compile:Compile],
	(var(OutDir) -> getcwd(OutDir) ; true),
	(var(Verbose) -> Verbose = no ; true),
	(var(Format) -> Format = byte ; true),
	(var(Compile) -> Compile = yes ; true).


fcompile1(In, Out, OutFile, Format, Verbose, Module) :-
	T0 is cputime,
	hash_create(Hash),
	hash_add(Hash, Module, []),
	gen_wam([], In, Out, Hash, Format, Verbose, Module),
	hash_list(Hash, Ms, Preds), 
	gen_aux_preds(Out, Ms, Hash, Format, Verbose, Preds), 
	write_outfile(Out, Format, OutFile), 
	!,
	CT is cputime - T0,
	printf(log_output, "%w generated in %.2f seconds%n", [OutFile,CT]).


% only compile file if needed. File is a string
ensure_compiled(File, _) :-
	get_file_info(File, mtime, MTime),
	(atom(File) -> AFile = File ; atom_string(AFile, File)),
	% current_compiled_file expects atoms for file names
	current_compiled_file(AFile, CTime, _Module),
        % compiled *somewhere* in the system
	MTime =< CTime, !.  % no need to compile it
ensure_compiled(File, Module) :-
	compile(File)@Module.



write_outfile(Out, Format, OutFile) :-
	get_stream_info(Out, name, OutCode),	% get string stream contents
	check_open(write, OutFile, OutStream),
	init_outfile(Format, OutStream),
	write(OutStream, OutCode),
	close(OutStream).

init_outfile(text, _).
init_outfile(byte, Out) :-
	dump_header(Out).


finclude(InFile, InDir, Out, Preds, Format, Verbose, Module) :-
	printf(log_output, "including %w%n", [InFile]),
	check_open(read, InFile, In),
	get_file_info(InDir, executable, on),
	getcwd(CWD),
	cd_if_possible(InDir),
	(gen_wam([], In, Out, Preds, Format, Verbose, Module) -> 
	    close(In), cd(CWD) ; close(In), cd(CWD), fail
	).


gen_aux_preds(Out, Modules, PHash, Format, Verbose, MPreds) :-
	% MPreds are the predicates for module M defined in the file
	(foreach(M,Modules), foreach(Ps, MPreds), fromto(0, N0, N1, _),
         param([Out,PHash,Format,Verbose]) do
           gen_aux_for_module(M, Ps, PHash, Format, Verbose, N0, N1, Out)
        ).


gen_aux_for_module(M, FPreds0, PHash, Format, Verbose, NDefin0, NDefin, Out) :-
	(FPreds0 == [] -> 
	    % no preds. defined in module, no need to generate auxs
	    NDefin0 = NDefin
	;   NDefin is NDefin0 + 1,
	    (NDefin = 2 ->
		% predicates in more than one module...
		writeln(warning_output, "Warning: found predicates defined in more than one module during fcompile.")
	    ;   true
	    ),
	    get_all_defined_pred(M, MPreds),
	    sort(FPreds0, FPreds),  % remove any duplicates.....
	    append(FPreds, MPreds, All0),
	    msort(All0, All),
	    gen_aux(All, Out, PHash, Format, Verbose, M)
	).


% This is a hopefully temporary solution to the problem that flags which are
% set via compiler directives would not be restored when loading .eco format.
% We now remember them as an extra argument to store_pred/4

pred_flag(Pred, Flags, Module) :-
	( get_flag(Pred, type, built_in)@Module ->
	    F0 is 16'40000000			% SYSTEM
	;   F0 = 0 ),
	( get_flag(Pred, auxiliary, on)@Module ->
	    F1 is F0 \/ 16'00000800		% AUXILIARY
	;   F1 = F0 ),
	( get_flag(Pred, skip, on)@Module ->
	    F2 is F1 \/ 16'00040000		% DEBUG_SK
	;   F2 = F1 ),
	( get_flag(Pred, debugged, on)@Module ->
	    Flags is F2 \/ 16'0008000		% DEBUG_DB
	;   Flags = F2 ).


gen_aux([], _, _, _, _, _) ?- true.
gen_aux([F/A,F/A|Ps], Out, PHash, Format, Verbose, M) ?-  !,
% duplicated means encountered when fcompiling file
	gen_aux(Ps, Out, PHash, Format, Verbose, M).
gen_aux([F/A|Ps], Out, PHash, Format, Verbose, M) ?- 
% not duplicated
	(get_flag(F/A, source_file, File)@M ->
	    % this is most likely an error, e.g. not using include/1
	    printf(warning_output, "Warning: extra predicate %a/%d from file %w found in module %w while fcompiling.%n",
               [F,A,File,M])
	; get_flag(F/A, auxiliary, on)@M ->
             % a compiler auxiliary, need to be flagged explicitly
	     (Verbose == yes ->
		 printf(log_output, "dumping extra auxiliary %w%n",[M:F/A])
	     ;   true
	     ),
	     output_wam_for_pred(F,A,M, Out, PHash, Format, Verbose)
	;
	    % other: external, runtime generated, ...
	    true
        ),
        gen_aux(Ps, Out, PHash, Format, Verbose, M).


/* excluding dynamic predicates */
get_all_defined_pred(M, Preds) :-
	findall(P,
	     (
	       \+ is_locked(M),
	       current_module_predicate(defined, P),
	       \+ is_dynamic(P)
	     ), 
	     Preds)@M.

check_if_not_generated(P, Preds, M) :-
	hash_find(Preds, M, MPreds),
	\+memberchk(P, MPreds).

gen_one_aux(F,A,M, Out, PHash, Format, Verbose) :-
	(check_if_not_generated(F/A, PHash, M) ->
	     (Verbose == yes ->
		 printf(log_output, "dumping auxiliary %w%n",[M:F/A])
	     ;   true
	     ),
	     output_wam_for_pred(F,A,M, Out, PHash, Format, Verbose)
	;
	     true
	).


try_extension(Base, FullName) :-
	get_flag(prolog_suffix, PExts),
	existing_file(Base, PExts, [readable], FullName), !.

get_inout_names(File0, OutDir, InDir, IFile, WFile) :-
        (File0 = library(FName) -> true ; FName = File0),
	pathname(FName, _, Base, Suffix),
	(Suffix == "" -> 
	    % need to append extension
	    try_extension(File0, IFile0)
	;   existing_file(File0, [""], [readable], IFile0), !
	), 
	canonical_path_name(IFile0, IFile), % full path for input file
	pathname(IFile, InDir, _, _),  
	get_flag(eclipse_object_suffix,ECO),
	canonical_path_name(OutDir, AbsOutDir),
	get_file_info(AbsOutDir, mode, DirMode),
	8'040000 =:= DirMode /\ 8'170000, !, % is a directory...
	concat_string([AbsOutDir, Base, ECO], WFile).
get_inout_names(_, _, _, _, _) :-
	set_bip_error(171).


gen_wam(Prev, In, Out, Preds, Format, Verbose, M) :-
	read_next_item(Prev, In, Item, Next, M),
	process_item(Item, In, Out, Next, Preds, Format, Verbose, M).


% `item' is either a directive, or a predicate (which may have multiple clauses)
read_next_item(Last, In, Item, Next, M) :- 
	(Last == [] ->	read(In, Current)@M ; Current = Last),
	read_restitem(Current, In, Item, Next, M).

read_restitem(end_of_file, _, Eof, Next, _) :- !, Eof = end_of_file, Next = [].
read_restitem(Item, In, Pred, Next, M) :- 
	is_pred(Item, F,A), !,
	% at the first clause of a predicate...
	(is_dynamic(F/A)@M ->
	    Pred = dynamic(Item), Next = []
	;   Pred = pred(F/A),
            read_restpred(F,A, In, Next, M)
	).
read_restitem(:-(Goals), _In, Directive, Next, _) ?- 
	Directive = directive(Goals), Next = [].
read_restitem(?-(Goals), _In, Directive, Next, _) ?- 
	Directive = directive(Goals), Next = [].


% consume all clauses of predicate
read_restpred(F,A, In, Next, M) :-
	read(In, Current)@M, 
	(is_pred(Current, F,A) ->
	    read_restpred(F,A, In, Next, M) 
	;   Next = Current
        ).

is_pred(:-(Head,_Body), F,A) ?- !,
	functor(Head, F,A). 
is_pred(?-(Head,_Body), F,A) ?- !,
	functor(Head, F,A).
is_pred(if(delay(Head),_), F, A) ?- !,
	functor(Head, F, A).
is_pred(-->(Head, _Body), F, A) ?- !,   % gramma rules!
	functor(Head, F, A0),
	A is A0 + 2.
is_pred(:-(_Directive), _, _) ?- !, fail.  % directives are not predicates!
is_pred(?-(_Directive), _, _) ?- !, fail.  % directives are not predicates!
is_pred(Fact, F,A) :-  % assume what is left are facts
	functor(Fact, F,A).

output_term(text, Out, Term, M) :-
	printf(Out, "%ODQKw.%n", [Term])@M.

output_term(byte, Out, Term, M) :-
	dump_term(Out, Term, M).

output_wam_for_pred(F,A,M, Out, PHash, Format, Verbose) :-
	disasm(F/A,WAM0, M),
	massage_code(WAM0, WAM),  % remove some word-size dependencies...
	% remember that predicate has been fcompiled...must be before aux check
        hash_find(PHash, M, MPreds),
	hash_add(PHash, M, [F/A|MPreds]),
	pasm(WAM, Size, WList),
	check_and_gen_auxs(WList, Out, PHash, Format, Verbose, M),
	pred_flag(F/A, Flags, M),
	WArr =.. [[]|WList],
	( M == sepia_kernel ->
	    output_term(Format, Out, (:- store_pred(F/A,WArr,Size,Flags)), M)
	;
	    output_term(Format, Out, (:- sepia_kernel:store_pred(F/A,WArr,Size,Flags)), M)
	).



process_item(Pred, In, Out, Next, Preds, Format, Verbose, M) :- 
        Pred = pred(F/A), % workaround compiler useless indexing bug
	(Verbose == yes ->
	    printf(log_output, "dumping %w%n",[M:F/A]) ; true
	),
	output_wam_for_pred(F,A,M, Out, Preds, Format, Verbose),
        gen_wam(Next, In, Out, Preds, Format, Verbose, M).
process_item(dynamic(Clause), In, Out, Next, Preds, Format, Verbose, M) :- 
	output_term(Format, Out, :-assert(Clause), M), 
        gen_wam(Next, In, Out, Preds, Format, Verbose, M).
process_item(directive(Goals), In, Out, Next, Preds, Format, Verbose, M) :- 
	output_directive(Out, Goals, Format, M),
        directive_action(Goals, Out, Preds, Format, Verbose, M, M1),
        gen_wam(Next, In, Out, Preds, Format, Verbose, M1).
process_item(end_of_file, _, _, _, _, _, _, _).


% filter out some directives 
output_directive(Out, Goals0, Format, M) :-
	filter_directives(Goals0, Goals),
	(nonvar(Goals) ->
	    output_term(Format, Out, :-(Goals), M)
	;   true
        ).

% filter out directives that should not be printed in output file
% before directive action is called 
% a directive that is not to be printed will be replaced by a variable,
% which is then filtered out by the first clause
filter_directives((DL0,DR0), D) :- !,
	filter_directives(DL0, DL),
	filter_directives(DR0, DR),
	(var(DL) -> 
	    D = DR
	;nonvar(DR) ->
	   /* both DR and DL nonvars */
           D = (DL,DR)
        ;  true
        ).
filter_directives(include(_), _) :- !.
filter_directives(comment(_,_), _) :- !.
filter_directives(D, D).


% may need to act on directives
directive_action((D1, D2), Out, Preds, F, V, M0, M) ?- !,
	directive_action(D1, Out, Preds, F, V, M0, M1),
	directive_action(D2, Out, Preds, F, V, M1, M).
directive_action(module(M0), _, Preds, _, _, M1, M) ?-  !, 
	% use current module if var.
	change_module(M0, M1, Preds, init, M).
directive_action(module(M0,_), _, Preds, _, _, M1, M) ?-  !, 
	% use current module if var.
	change_module(M0, M1, Preds, init, M).
directive_action(module(M0,_,_), _, Preds, _, _, M1, M) ?-  !, 
	% use current module if var.
	change_module(M0, M1, Preds, init, M).
directive_action(module_interface(M0), _, Preds, _, _, M1, M) ?-  !, 
	% use current module if var.
	change_module(M0, M1, Preds, init, M).
directive_action(begin_module(M0), _, Preds, _, _, M1, M) ?-  !, 
	% use current module if var.
	change_module(M0, M1, Preds, keep, M).
directive_action(tool(P1,_P2), _, Preds, _, _, M, M) :-
	P1 = F/A, !, % need to remember P1 so it would not be regenerated
	hash_find(Preds, M, Defs),
	hash_add(Preds, M, [F/A|Defs]).
directive_action(include(Files0), Out, Preds, Format, Verbose, M, M) :-
	!,
	(Files0 = [_|_] -> Files = Files0 ; Files = [Files0]),
	(foreach(F, Files), param([Preds,M,Out,Format,Verbose]) do
	    (get_inout_names(F, ".", InDir, InName, _) ->
                % only want the input name
		finclude(InName, InDir, Out, Preds, Format, Verbose, M) 
	    ;   get_bip_error(Er),
	        error(Er, include(F))
	    )
	).
directive_action(_D, _, _Preds, _, _, M, M).


change_module(NewM, OldM, Hash, Cond, M) :-
	((NewM == OldM ; var(NewM)) -> 
	    (var(NewM) -> 
		printf(error, "Warning: Unable to change module for fcompile as module name uninstantiated.%n")
	    ; true
	    ),
	    M = OldM   % same module as before
	;   M = NewM,
	    (Cond == init ->   
		hash_add(Hash, M, []) % destroy any old definitions

	    ;   (hash_find(Hash, M, _) -> 
                    true 
                ;   hash_add(Hash, M, [])
                )
	        % create a new entry if module has not been encountered before
	    )
	       
        ).

%---------------------------------------------------------------------------

:- mode massage_code(+, -).

massage_code([], WAM) ?- WAM = [].
massage_code([integer_range_switch(a(A),RT0,ref(Le),RLd)|WAM0], WAM) ?- !,
% if integer_range_switch's format changes, this needs to be updated too
	try_trim_range(RT0, Le, RT1),
	WAM = [integer_range_switch(a(A),RT1,ref(Le),RLd)|WAM1],
	massage_code(WAM0, WAM1).
massage_code([I|WAM0], WAM) ?-
	WAM = [I|WAM1],
	massage_code(WAM0, WAM1).

check_and_gen_auxs([], _, _, _, _, _).
check_and_gen_auxs([W|Ws], Out, PHash, Format, Verbose, M) :-
	(W = proc(F/A), get_flag(F/A, auxiliary, on)@M -> 
	     gen_one_aux(F,A,M, Out, PHash, Format, Verbose) ; true
	),
	check_and_gen_auxs(Ws, Out, PHash, Format, Verbose, M).


try_trim_range([Min0-MinR,Max0-MaxR|Table], Le, TRTable) :-
	try_trim_min(Min0, MinR, Table, Max0, Le, Min),
	try_trim_max(Max0, MaxR, Table, Min, Le, Max),
	TRTable = [Min-MinR,Max-MaxR|Table].

try_trim_min(Min0, ref(MinL), Table, Max, Le, Min) :-  
	(MinL == Le ->
	    % can trim min range
	    (Table = [TMin-_|_] -> Min = TMin ; Min = Max)
	;   Min0 = Min
        ).

try_trim_max(Max0, ref(MaxL), Table, Min, Le, Max) :-  
	(MaxL == Le ->
	    % can trim max range
	    (max_table(Table, TMax) -> Max = TMax ; Max = Min)
	;   Max0 = Max
        ).

max_table([TMax-_], TMax) :- !.
max_table([_|Ts], TMax) :-
	max_table(Ts, TMax).

	

check_open(read, FileName, Stream) ?-
	((get_file_info(FileName, readable, on),
	 get_file_info(FileName, mode, Mode),
	 8'040000 =\= Mode /\ 8'170000 /* not directory*/) ->
             open(FileName, read, Stream)
	 ;
	     set_bip_error(171)
	).
check_open(write, FileName, Stream) ?-
	(exists(FileName) ->
	    % file exists, check can overwrite it
	    ((get_file_info(FileName, writable, on), 
	     get_file_info(FileName, mode, Mode),
	     8'040000 =\= Mode /\ 8'170000 /*not directory*/) ->
	         open(FileName, write, Stream)
	     ;
	         set_bip_error(171)
	     )
        ;
             % file does not exist, check can write in directory
	     canonical_path_name(FileName, FullName),
	     pathname(FullName, ParentDir, _, _),
	     (get_file_info(ParentDir, writable, on) ->
		 open(FileName, write, Stream)
	     ;   set_bip_error(171)
	     )
	 ).

atom_or_string(Term) :- atom(Term), !.
atom_or_string(Term) :- string(Term).

cd_if_possible(Path) :-
	get_file_info(Path, mode, Mode),
	Mode /\ 8'170000 =:= 8'40000,
	get_file_info(Path, executable, on), !,
	cd(Path).
cd_if_possible(_) :-
	set_bip_error(171).
