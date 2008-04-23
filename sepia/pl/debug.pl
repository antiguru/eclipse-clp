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
% Copyright (C) 1989-2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): ECRC GmbH
% Contributor(s): IC-Parc, Imperal College London
% 
% END LICENSE BLOCK
%
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: debug.pl,v 1.2 2008/04/23 13:38:30 kish_shen Exp $
% ----------------------------------------------------------------------

/*
 * SEPIA PROLOG SOURCE MODULE
 */

/*
 * IDENTIFICATION:	debug.pl
 *
 * DESCRIPTION: 	
 *
 *
 * CONTENTS:     
 *
 */


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_module(sepia_kernel).
:- system.		% compiler directive to add the SYSTEM flag

:- export
	set_leash/2,
	get_leash/2,
	(spy)/1,
	(nospy)/1,
	debug_reset/0,
	debugging/0.

:- tool( debugging/0, debugging_body/1).
:- tool( (spy)/1, spy_body/2).
:- tool( (nospy)/1, nospy_body/2).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/********* Conditional definition of GUI debugging predicates ***************/

:- export
	make_display_matrix/2,
	make_display_matrix/5,
	kill_display_matrix/1.

:- tool(make_display_matrix/2, make_display_matrix_body/3).
:- tool(make_display_matrix/5, make_display_matrix_body/6).
:- tool(kill_display_matrix/1, kill_display_matrix_body/2).

make_display_matrix_body(Matrix, Name, Module) :-
	( current_module(remote_tools) ->   
		(remote_tools: attached(_) -> % only when attached
		    tracer_tcl:make_display_matrix_body(Matrix, Name, Module)
		; true
	        )
	; current_module(tracer_tcl) ->
	    tracer_tcl:make_display_matrix_body(Matrix, Name, Module)
	;
	    true
	).

make_display_matrix_body(Matrix, Prio, Type, SList, Name0, Module) :-
	( current_module(remote_tools) ->   
		(remote_tools: attached(_) -> % only when attached
		    tracer_tcl:make_display_matrix_body(Matrix, Prio, Type, SList, Name0, Module)
		; true
	        )
	; current_module(tracer_tcl) ->
	    tracer_tcl:make_display_matrix_body(Matrix, Prio, Type, SList, Name0, Module)
	;
	    true
	).

kill_display_matrix_body(Name0, Module) :-
	( current_module(tracer_tcl) ->
	    tracer_tcl:kill_display_matrix_body(Name0, Module)
	;
	    true
	).

%----------------------------------------------------------------------


debug_reset :-
	trace_mode(12, 0).


debugging_body(Module) :-
	getval(toplevel_trace_mode, Mode),
	debug_info(Mode, Module).


debug_info(nodebug, _) :-
	!,
	writeln(log_output, 'Debugger is switched off').
debug_info(Mode, Module) :-
	write(log_output, 'Debug mode is '),
	writeln(log_output, Mode),
	(
	    current_spied_predicate(F/A, M),
	    ( Module = M -> true ; printf(log_output, "%w:", [M]) ),
	    printf(log_output, "%a/%d\t is being spied%n", [F, A]),
		fail
	;
		true
	),
        (
            current_breakpoint((Line,File), P, DM),
            printf(log_output, "Line %d in file %w (in predicate %w:%w) has"
                               " a breakpoint set.%n", [Line,File,DM,P]),
            fail
        ;
            true
        ).

%
% add spypoint(s)
%
spy_body(Y, _):-
	var(Y), !,
	error(4, 'spy'( Y)).
spy_body(P, M) :-
	set_spypoints(P, M),
	( getval(toplevel_trace_mode, nodebug) ->
	    setval(toplevel_trace_mode, leap),
	    writeln(log_output, "Debugger switched on - leap mode")
	;
	    true	% leave debugger in whatever mode it is
	).

set_spypoints((A, B), M) :-
	!,
	set_spypoints(A, M),
	set_spypoints(B, M).
set_spypoints(F, M) :-
	atom(F),
	!,
	set_spypoints(M:F/_, M).
set_spypoints(F/N, M) ?- !,
	set_spypoints(M:F/N, M).
set_spypoints(PM:F, M) ?- atom(F), !,
	set_spypoints(PM:F/_, M).
set_spypoints(F:L, M) ?- integer(L), atomic(F), !,
        ( get_portlist_from_file(F, FullName, PortsList),
          find_best_port(PortsList, L, FullName, none, DM, PortPred, PortLine),
          get_flag(PortPred, port_info, PInfo)@DM ->
            ( memberchk(p(PortLine,FullName,0), PInfo) -> 
                set_proc_flags(PortPred, break, PortLine, DM),
                printf(log_output, "breakpoint added to line %d of file %w in"
                                   " predicate %w%n", [PortLine,FullName,PortPred])
            ;
                true
            )
        ;
            error(6, spy(F:L), M)
        ).
set_spypoints(QualPredSpec, M) :-
	check_partial_qualpredspec(QualPredSpec),
	!,
	QualPredSpec = PM:PredSpec,
	% look first for preds defined locally
	find_matching_predicates(PredSpec, PM, defined, LocalPreds),
	( LocalPreds = [] ->
	    % if none, look for exported preds elsewhere
	    find_matching_predicates(PredSpec, _, exported, ExpElsewherePreds),
	    ( ExpElsewherePreds = [] ->
		% if none, look for local preds elsewhere
		find_matching_predicates(PredSpec, _, defined, Preds),
		( Preds = [] ->
		    error(60, spy(PredSpec), M)
		;
		    true
		)
	    ;
		Preds = ExpElsewherePreds
	    )
	;
	    Preds = LocalPreds
	),
	(
	    member(DM:F/N, Preds),
	    not spied_(F, N, DM),
		( set_proc_flags(F/N, spy, on, DM) ->
		    write(log_output, "spypoint added to "),
		    ( DM = M -> true ; printf(log_output, "%w:", [DM]) ),
		    printf(log_output, "%w/%d.%n", [F,N])
		;
		    !,
		    bip_error(spy(PredSpec), M)
		),
	    fail
	;
	    true
	).
set_spypoints(U, M):-
	bip_error('spy'(U), M).

%
% remove spypoint(s)
%
nospy_body(Y, M):-
	var(Y), !,			% remove all spy points
	nospy_body(_/_, M).
nospy_body((A,B), M) :-
	!,
	nospy_body(A, M),
	nospy_body(B, M).
nospy_body(F, M) :-
	atom(F),
	!,
	nospy_body(M:F/_, M).
nospy_body(F/N, M) ?- !,
	nospy_body(_Any:F/N, M).
nospy_body(PM:F, M) ?- atom(F), !,
	nospy_body(PM:F/_, M).
nospy_body(F:L, _M) ?- integer(L), atomic(F), !,
        ( get_portlist_from_file(F, File, PortsList),
          find_best_port(PortsList, L, File, none, DM, PortPred, PortLine),
          get_flag(PortPred, port_info, PInfo)@DM,
          memberchk(p(PortLine,File,1), PInfo) ->
            set_proc_flags(PortPred, break, PortLine, DM),
            printf(log_output, "breakpoint removed from line %d of file %w in"
                               " predicate %w%n", [PortLine,File,PortPred])
        ;
            true
        ).
nospy_body(QualPredSpec, M) :-
	check_partial_qualpredspec(QualPredSpec),
	!,
	QualPredSpec = DM:PredSpec,
	(
	    current_spied_predicate(PredSpec, DM),
		(set_proc_flags(PredSpec, spy, off, DM) ->
		    PredSpec = F/N,
		    write(log_output, "spypoint removed from "),
		    ( DM = M -> true ; printf(log_output, "%w:", [DM]) ),
		    printf(log_output, "%w/%d.%n", [F,N])
		;
		    !,
		    bip_error('nospy'(PredSpec), M)
		),
	    fail
	;
	    true
	).
nospy_body(U, M):-
	bip_error('nospy'(U), M).


    find_matching_predicates(P, M, Class, List) :-
	findall(M:P, (
		current_module(M),
		( is_locked(M) ->
		    % only see the exported ones (but not
		    % reexported ones, to avoid duplicates)
		    current_module_predicate(exported, P)@M
		;
		    current_module_predicate(Class, P)@M
		)
	    ), List).

    current_spied_predicate(P, M) :-
	    current_module(M),
	    ( is_locked(M) ->
		current_module_predicate(exported, P)@M
	    ;
		current_module_predicate(defined, P)@M
	    ),
	    P \= trace_body/2,
	    P = F/A,
	    spied_(F, A, M).

    check_partial_qualpredspec(X) :- var(X), !,
	    set_bip_error(4).
    check_partial_qualpredspec(N/A) :- !,
	    check_var_or_atom(N),
	    check_var_or_arity(A).
    check_partial_qualpredspec(M:P) :- !,
	    check_var_or_atom(M),
	    check_partial_predspec(P).
    check_partial_qualpredspec(_) :-
	    set_bip_error(5).


% dummies, no longer supported
set_leash(_Port, _Mode).
get_leash(_Port, stop).


current_predicate_with_ports(PredSpec, Module, PortInfos) :-
        current_module(Module),
        \+ is_locked(Module),
        current_module_predicate(defined, PredSpec)@Module,
        get_flag(PredSpec, debugged, on)@Module,
        get_flag(PredSpec, port_info, PortInfos)@Module.
        
ports_in_file(File, List) :-
        findall(port(Line,M,P), (
                current_predicate_with_ports(P, M, PInfos),
                member(p(Line,File,_), PInfos)
             ), List0),
        sort(1,<, List0, List).

find_best_port([], Line, File, LastPort, DM, PortPred, PortLine) :-
        LastPort = port(PortLine,DM,PortPred).
find_best_port([Port0|PortList0], Line, File, LastPort, DM, PortPred, PortLine) :-
        Port0 = port(Line0,DM0,Pred0),
        ( Line0 =:= Line ->
            DM = DM0, PortPred = Pred0, PortLine = Line0
        ; Line0 > Line ->
           /* decide if LastPort or Port0 is the better port */
            ( LastPort = port(LastLine,LastDM,LastPred) ->
                ( Pred0 \= LastPred, get_flag(Pred0, source_file, File) ->
                    % start of predicate is in same file as Port0 
                    get_flag(Pred0, source_line, StartLine0),
                    (StartLine0 > Line ->
                        /* assume Pred0 starts after Line, use LastPort */
                        DM = LastDM, PortPred = LastPred, PortLine = LastLine
                                                                   
                    ;
                        DM = DM0, PortPred = Pred0, PortLine = Line0
                    )
                ;
                    DM = DM0, PortPred = Pred0, PortLine = Line0
                )
            ;
                DM = DM0, PortPred = Pred0, PortLine = Line0
            )
        ; /* Line0 < Line */
           find_best_port(PortList0, Line, File, Port0, DM, PortPred, PortLine)
        ).

get_portlist_from_file(F, File, PortsList) :-
        concat_atom([F], File0),
        canonical_path_name(File0, File1),
        get_flag(prolog_suffix, Ss),
        once(existing_file(File1, Ss, [readable], File)), 
        ports_in_file(File, PortsList).

current_breakpoint(BP,P,M) :- 
        current_predicate_with_ports(P, M, PInfos),
        member(p(Line,File,1), PInfos),
        BP = (Line,File).   

        
        
%--------------------------------------------------------

?- untraceable
	debugging/0,
	set_leash/2,
	get_leash/2,
	(spy)/1,
	(nospy)/1.

?- skipped
	debugging/0,
	set_leash/2,
	set_flag/3,
	get_leash/2,
	(spy)/1,
	(nospy)/1.

