%----------------------------------------------------------------------
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
% The Original Code is  The Zinc Modelling interface for ECLiPSe
% The Initial Developer of the Original Code is  Joachim Schimpf
% with support from Cisco Systems and NICTA Victoria.
% Portions created by the Initial Developer are
% Copyright (C) 2009 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): Kish Shen
% 
% END LICENSE BLOCK
%----------------------------------------------------------------------
:- module(gfd).

:- lib(lists).

:- import set_bip_error/1, get_bip_error/1 from sepia_kernel.

:- import
	timestamp_init/2,
        timestamp_update/2,
	timestamp_age/3,
	request_fail_event/3 
   from sepia_kernel.

:- export op(700, xfx, [#::]).
:- export op(750, fx, [neg]).
:- export op(760, yfx, [and]).
:- export op(770, yfx, [or,xor]).
:- export op(790, yfx, [<=>]).
:- export op(780, yfx, [=>]).

:- export (#::)/2, (::)/2.
:- export (#::)/3, (::)/3.
:- export (#\=)/2, (#=)/2, (#<)/2, (#>)/2, (#>=)/2, (#=<)/2.
:- export alldifferent/1, occurrences/3, atmost/3, count/4.
:- export labeling/1, indomain/1, indomain_min/1.
:- export is_in_domain/2, is_in_domain/3.
:- export search/6.
:- export (and)/2, (or)/2, (xor)/2, (<=>)/2, (=>)/2, neg/1.

:- local reference(prob_handle).
:- local variable(varray_size, 100).
:- local store(stats).

:- export gfd_var_print/2.

:- meta_attribute(gfd, [
        set_bounds:gfd_set_var_bounds/3,  
        get_bounds:gfd_get_var_bounds/3,  
        print:gfd_var_print/2,
        unify:gfd_unify/2]).

:- 
        get_flag(hostarch, Arch),
        get_flag(object_suffix, O),
        concat_string([Arch,/,"gfd.", O], SolverObj),
        load(SolverObj),
        writeln(log_output, "Loaded Gecode solver"),
        external(g_init/1, p_g_init),
        external(g_check_handle/3, p_g_check_handle),
        external(g_trail_undo_for_event/1, p_g_trail_undo_for_event),
        external(g_delete/1, p_g_delete),
        external(g_add_newbool/3, p_g_add_newbool),
        external(g_add_newvars_interval/4, p_g_add_newvars_interval),
        external(g_add_newvars_dom/3, p_g_add_newvars_dom),
        external(g_post_interval/5, p_g_post_interval),
        external(g_post_dom/4, p_g_post_dom),
        external(g_setvar/4, p_g_setvar),
        external(g_post_linrel_cstr/3, p_g_post_linrel_cstr),
        external(g_post_bool_connectives/3, p_g_post_bool_connectives),
        external(g_post_alldiff/3, p_g_post_alldiff),
        external(g_post_count/6, p_g_post_count),
        external(g_post_sum/5, p_g_post_sum),
        external(g_propagate/3, p_g_propagate),
        external(g_check_val_is_in_var_domain/3, p_g_check_val_is_in_var_domain),
        external(g_get_var_bounds/4, p_g_get_var_bounds),
        external(g_get_var_value/3, p_g_get_var_value),
        external(g_get_var_domain/3, p_g_get_var_domain),
        external(g_get_var_lwb/3, p_g_get_var_lwb),
        external(g_get_var_lwb_after_update/5, p_g_get_var_lwb_after_update),
        external(g_setup_search/7, p_g_setup_search),
        external(g_do_search/5, p_g_do_search).

:- export struct(
        prob(
             cp_stamp,
             id,
             nvars,
             nlevels,
             nevents,
             vars,
             min,
             max,
             prop,
             last_anc,
             space,
             events
        )
   ).

:- export struct( gfd_space(handle, stamp) ).

:- export struct(
        gfd(
            idx,
            bool,
            prob,
            set
        )
   ).

constraint(_ #= _) ?- true.
constraint(_ #\= _) ?- true.
constraint(_ #< _) ?- true.
constraint(_ #=< _) ?- true.
constraint(_ #> _) ?- true.
constraint(_ #>= _) ?- true.
constraint(_ and _) ?- true.
constraint(_ or _) ?- true.
constraint(_ xor _) ?- true.
constraint(_ <=> _) ?- true.
constraint(_ => _) ?- true.
constraint(neg(_)) ?- true.

rel_op('#=').
rel_op('#\\=').
rel_op('#<').
rel_op('#=<').
rel_op('#>').
rel_op('#>=').

% low level representation of variable
gfdvar(I,B,'_ivar'(I,B)).

gfd_unify(_Term, Attr) :-
        var(Attr).
gfd_unify(Term, Attr) :-
        compound(Attr),
        unify_term_gfd(Term, Attr).

:- mode unify_term_gfd(?, +).
unify_term_gfd(Y{gfd:AttrY}, AttrX) ?-
        unify_gfd_gfd(Y, AttrX, AttrY).
unify_term_gfd(X, Attr) :-
        integer(X),
        Attr = gfd{prob:H,idx:I},
        Attr = gfd{prob:H,set:S,idx:I},
%        post_new_event(setvar(I, X), H).
%        check_and_update_handle(H),
        ( S == [] -> true ; post_new_event(setvar(I, X), H)).
/*        H = prob{space:gfd_space{handle:SpH}},
        ( g_get_var_value(SpH, I, Y) -> % fail if not assigned
            X == Y
        ;
            post_new_event(setvar(I, X), H)
        ).*/

unify_gfd_gfd(_Y, AttrX, AttrY) :-
        var(AttrY),
        AttrX = AttrY.
unify_gfd_gfd(_Y, AttrX, AttrY) :-
        nonvar(AttrY),
        AttrX = gfd{idx:IdxX,bool:BX,prob:HX},
        AttrY = gfd{idx:IdxY,bool:BY,prob:HY},
        % the variables must belong to the same problem, else fail
        HX = prob{id:Id}, 
        HY = prob{id:Id},
        ( IdxX == IdxY ->
            true   % same variable, do nothing
        ;
            % post an equality constraint for the two variables to gecode
            gfdvar(IdxX,BX,GX),
            gfdvar(IdxY,BY,GY),
            post_new_event(post_rc(GX #= GY), HY)
        ).

gfd_set_var_bounds(_{gfd:Attr}, Lo0, Hi0) ?-
        nonvar(Attr),
        Lo is fix(Lo0),
        Hi is fix(Hi0),
        Attr = gfd{prob:H, idx:I, bool:BI},
        gfdvar(I,BI,GV),
        post_new_event(post_interval([](GV), Lo, Hi), H).

gfd_get_var_bounds(_{gfd:Attr}, Lo, Hi) ?-
        nonvar(Attr),
        Attr = gfd{prob:H, idx:I},
        % make sure there is a valid space
        restore_space_if_needed(H, SpH),
        g_get_var_bounds(SpH, I, Lo, Hi).

get_prob_handle(H) :-
        getval(prob_handle, H0),
        (H0 \= prob{} ->
            new_prob_handle(H),
            setval(prob_handle, H)
        ;
            H0 = H
        ).
/* addto_varray(+ProbHandle, ++Idx, ?V)
     Add new problem variable V with index Idx to the variable array of 
     ProbHandle, expanding the array if required
*/
addto_varray(H, Idx, V) :-
  H = prob{vars:VArr},
  ( Idx > arity(VArr) ->
      expand_and_copy_array(VArr, NewVArr),
      setarg(vars of prob, H, NewVArr),
      arg(Idx, NewVArr, V)
  ;
      arg(Idx, VArr, V)
  ).

expand_and_copy_array(Old, New) :-
        arity(Old, OldSize),
        NewSize is OldSize + getval(varray_size),  % for now
        dim(New, [NewSize]),
        ( foreacharg(A, Old, Idx), param(New) do
            arg(Idx, New, A)
        ).

:- tool('::'/2, '::_body'/3).
:- tool('#::'/2, '::_body'/3).

'::_body'(X, Domain, Module):-
        get_prob_handle(H),
        H = prob{nvars:NV0},
        normalise_vars(X, NX),
	process_domain_domain(Domain, NormalDomain, Module),
        (NormalDomain = [B..B] ->
            % singleton, just assign
            (foreach(V,NX), param(B) do V = B)
        ;
            process_domain_vars(NX, NormalDomain, H, NV0,NV, [],OldGVs),
            assign_domain(NormalDomain, H, NV, OldGVs)
        ).

:- tool('::'/3, '::_body'/4).
:- tool('#::'/3, '::_body'/4).

'::_body'(X, Domain, Bool, Module):-
        get_prob_handle(H),
        H = prob{nvars:NV0},
        normalise_vars(X, NX),
	process_domain_domain(Domain, NormalDomain, Module),
        (NormalDomain = [B..B] ->
            % singleton, just assign
            (foreach(V,NX), param(B) do V = B)
        ;
            process_domain_vars(NX, NormalDomain, H, NV0,NV, [],OldGVs),
            assign_domain(NormalDomain, H, NV, OldGVs)
        ).

normalise_vars(V, N) :-
        var(V), !,
        N = [V].
normalise_vars(I, N) :-
        integer(I), !,
        N = [I].
normalise_vars(Xs, NXs) :-
        collection_to_list(flatten(Xs), NXs).

split_first_domain([H0|T0], H, T) ?- !,
        H0 = H, T0 = T.
split_first_domain(Dom, H, T) :-
        nonvar(Dom),
        H = Dom,
        T = [].

process_domain_domain(Domain, NormalDomain, Module) :- 
        split_first_domain(Domain, H, T),
        subdomain(H, Lo, Hi, Module),
	( T \== [] ->
	    ( Lo =< Hi ->
		Domain1 = [Lo..Hi | Domain0]
	    ;
		Domain1 = Domain0
	    ),
	    (
		foreach(Sub, T),
		fromto(Domain0, Out, In, []),
		param(Module)
	    do
		subdomain(Sub, Lo, Hi, Module),
		% Filter empty ranges (Lo > Hi).
		( Lo =< Hi ->
		    Out = [Lo..Hi | In]
		;
		    Out = In
		)
	    ),
	    % Order the intervals.
	    number_sort(2, =<, Domain1, SortedUpperBoundsDomain),
	    number_sort(1, =<, SortedUpperBoundsDomain, SortedIntervalDomain),
	    [Lo0..Hi0 | SortedRest] = SortedIntervalDomain,
	    % Collapse zero width intervals to constants and merge
	    % overlapping/adjacent subdomains.  
	    (
		foreach(Lo..Hi, SortedRest),
		fromto(Lo0..Hi0, LoIn..HiIn, LoOut..HiOut, FinalSubDomain),
		fromto(NormalDomain, In, Out, [FinalSubDomain])
	    do
		( HiIn + 1 >= Lo ->
		    % There is no gap between HiIn and Lo so merge
		    In = Out,
		    LoOut = LoIn,
		    HiOut is max(Hi, HiIn)
		;
		    % There is a gap between HiIn and Lo
		    In = [LoIn..HiIn | Out],
		    LoOut = Lo,
		    HiOut = Hi
		)
	    )
	;
	    NormalDomain = [Lo..Hi]
	).

bound(I, B, _Module) :-
        integer(I), !,
        B = I.
bound(A, B, Module) :-
        compound(A),
        subcall(B is A, [])@Module,
        integer(B).

subdomain(Lo..Hi, Lo1, Hi1, Module) ?- !,
        bound(Lo, Lo1, Module),
        bound(Hi, Hi1, Module).
subdomain(I, Lo1, Hi1, Module) :-
        Hi1 = Lo1,
        bound(I, Lo1, Module).

process_domain_vars([V1|Vs], Domain, H, NV0,NV, OldGVs0,OldGVs) :-
        var(V1), !,
        ec_to_gecode_var(V1, H, NV0,NV1, OldGVs0,OldGVs1, _),
        process_domain_vars(Vs, Domain, H, NV1,NV, OldGVs1,OldGVs).
process_domain_vars([I|Vs], Domain, H, NV0,NV, OldGVs0,OldGVs) :-
        integer(I), !,
        is_in_given_domain(I, Domain),
        process_domain_vars(Vs, Domain, H, NV0,NV, OldGVs0,OldGVs).
process_domain_vars([], _D, _H, NV,NV, OldGVs,OldGVs).

is_in_given_domain(I, [Lo..Hi|Ds]) :-
        (I >= Lo, I =< Hi -> true ; is_in_given_domain(I, Ds)).

assign_domain(Domain, H, NV, OldGVs) :- 
        Domain = [Lo..Hi|T], !,
        (T == [] ->
            Hi >= Lo,
            assign_domain_interval(H, NV, OldGVs, Lo, Hi)
        ;
            assign_multi_domain_intervals(H, NV, OldGVs, Domain)
        ).

assign_domain_interval(H, NV, OldGVs, Lo, Hi) :-
        ( OldGVs == [] ->
            true
        ;
            GVArr =.. [[]|OldGVs],
            post_new_event(post_interval(GVArr,Lo,Hi), H)
        ),
        update_newvars_with_domain_interval(H, NV, Lo, Hi).

update_newvars_with_domain_interval(H, NV, Lo, Hi) :-
        ( NV =:= arg(nvars of prob, H) -> % no new vars 
            true
        ;
            setarg(nvars of prob, H, NV),
            post_new_event(newvars_interval(NV,Lo,Hi), H)
        ).

assign_multi_domain_intervals(H, NV, OldGVs, Domain) :-
        DArray =.. [[]|Domain],
        ( OldGVs == [] ->
            true
        ;
            GVArr =.. [[]|OldGVs],
            post_new_event(post_dom(GVArr,DArray), H)
        ),
        update_newvars_with_multi_domain_intervals(H, NV, DArray).

update_newvars_with_multi_domain_intervals(H, NV, DArray) :-
        ( NV =:= arg(nvars of prob, H) -> % no new vars 
            true
        ;
            setarg(nvars of prob, H, NV),
            post_new_event(newvars_dom(NV,DArray), H)
        ).


is_not_boolvar(_{gfd:Attr}) ?-
        nonvar(Attr), !,
        Attr = gfd{bool:Link},
        var(Link).
is_not_boolvar(_).

link_var_to_boolvar(V, H) :-
        ( var(V) ->
            is_not_boolvar(V),
            V :: [0,1],
            post_new_event(newbool(V), H)
        ;
            true
        ).
        

is_in_domain(Val, Var) :-
        integer(Val),
        ( var(Var) ->
            get_gecode_attr(Var, H, Attr),
            Attr = gfd{idx:Idx},
            restore_space_if_needed(H, SpH),
            g_check_val_is_in_var_domain(SpH, Idx, Val)
        ;
            Val == Var
        ).

is_in_domain(Val, Var, Result) :-
        (is_in_domain(Val, Var) -> Result = yes ; Result = no).

:- tool((and)/2, and_body/3).
:- tool((or)/2, or_body/3).
:- tool((xor)/2, xor_body/3).
:- tool(neg/1, neg_body/2).
:- tool('<=>'/2, '<=>_body'/3).
:- tool('=>'/2, '=>_body'/3).

and_body(EX, EY, _Module) :-
        get_prob_handle(H),
        ec_to_gecode_expr((EX and EY), H, GE),
        post_bool_connectives(H, GE).

or_body(EX, EY, _Module) :-
        get_prob_handle(H),
        ec_to_gecode_expr((EX or EY), H, GE),
        post_bool_connectives(H, GE).

xor_body(EX, EY, _Module) :-
        get_prob_handle(H),
        ec_to_gecode_expr((EX xor EY), H, GE),
        post_bool_connectives(H, GE).

'<=>_body'(EX, EY, _Module) :-
        get_prob_handle(H),
        link_var_to_boolvar(EX, H),
        link_var_to_boolvar(EY, H),
        ec_to_gecode_expr((EX <=> EY), H, GE),
        post_bool_connectives(H, GE).

'=>_body'(EX, EY, _Module) :-
        get_prob_handle(H),
        ec_to_gecode_expr((EX => EY), H, GE),
        post_bool_connectives(H, GE).

neg_body(EX, _Module) :-
        get_prob_handle(H),
        ec_to_gecode_expr(neg(EX), H, GE),
        post_bool_connectives(H, GE).

post_bool_connectives(H, GBCon) :-
        post_new_event(post_bool_connectives(GBCon), H).


:- tool('#\\='/2, '#\\=_body'/3).
:- tool('#='/2, '#=_body'/3).
:- tool('#<'/2, '#<_body'/3).
:- tool('#>'/2, '#>_body'/3).
:- tool('#>='/2, '#>=_body'/3).
:- tool('#=<'/2, '#=<_body'/3).

'#\\=_body'(EX, EY, _Module) :-
        get_prob_handle(H),
        ec_to_gecode_expr((EX #\= EY), H, GE),
        post_rel_cstr(H, GE).

'#=_body'(EX, EY, Module) :-
        ( (constraint(EX) ; constraint(EY) ) ->
            '<=>_body'(EX, EY, Module)
        ;
            get_prob_handle(H),
/*        (EX = sum(Vs), integer(EY) ->
            ec_to_gecode_sum(Vs, (#=), EY, H)
        ;
*/
            ec_to_gecode_expr((EX #= EY), H, GE),
            post_rel_cstr(H, GE)
        ).

'#>_body'(EX, EY, _Module) :-
        get_prob_handle(H),
        ec_to_gecode_expr((EX #> EY), H, GE),
        post_rel_cstr(H, GE).

'#<_body'(EX, EY, _Module) :-
        get_prob_handle(H),
        ec_to_gecode_expr((EX #< EY), H, GE),
        post_rel_cstr(H, GE).

'#>=_body'(EX, EY, _Module) :-
        get_prob_handle(H),
        ec_to_gecode_expr((EX #>= EY), H, GE),
        post_rel_cstr(H, GE).

'#=<_body'(EX, EY, _Module) :-
        get_prob_handle(H),
        ec_to_gecode_expr((EX #=< EY), H, GE),
        post_rel_cstr(H, GE).

post_rel_cstr(H, GExpr) :-
        post_new_event(post_rc(GExpr), H).

ec_to_gecode_sum(Vars, Rel, C, H) :-
        collection_to_list(Vars, List),
        ec_to_gecode_expr(List, H, GList),
        GArray =.. [[]|GList],
        post_new_event(post_sum(GArray, Rel, C), H).

alldifferent(Vars) :-
        collection_to_list(Vars, List),
        get_prob_handle(H),
        ec_to_gecode_expr(List, H, GList),
        GArray =.. [[]|GList],
        post_new_event(post_alldiff(GArray), H).

count(Value, Vars, Rel, N) :-
        integer(Value),
        atomic(Rel),
        rel_op(Rel),
        collection_to_list(Vars, List),
        get_prob_handle(H),
        ec_to_gecode_expr(List, H, GList),
        GArray =.. [[]|GList],
        ( integer(N) ->
            N =< arity(GArray),
            GN = N
        ; var(N) ->
            N :: 0..arity(GArray),
            ec_to_gecode_expr(N, H, GN)
%       ; fail
        ),
        post_new_event(post_count(Value,GArray,Rel,GN), H).

% compatibility
occurrences(Value, Vars, N) :-
        count(Value, Vars, '#=', N).

% compatibility
atmost(N, Vars, Value) :-
        count(Value, Vars, '#=<', N).

atleast(N, Vars, Value) :-
        count(Value, Vars, '#>=', N).

% to do: global_cardinality etc.

new_solver_id(1).    %DUMMY FOR NOW

get_gecode_attr(_{gfd:Attr0}, H, Attr) ?-
        nonvar(Attr0),
        Attr = Attr0,
        Attr = gfd{prob:H}. % check we have same problem

add_gecode_attr(X{gfd:Attr}, H, Idx, BI) ?-
        var(Attr),
        new_gecode_attr(X, H, Idx, BI, Attr).
add_gecode_attr(X, H, Idx, BI) :-
        free(X),
        new_gecode_attr(X, H, Idx, BI, _Attr).

get_gecode_domain(X, Domain) :-
        get_gecode_attr(X, H, Attr),
        Attr = gfd{idx:Idx},
        H = prob{space:gfd_space{handle:SpH}},
        g_get_var_domain(SpH, Idx, Domain).

gfd_var_print(X, Domain) :-
        get_gecode_domain(X, Domain).

:- mode new_gecode_attr(?,+,+,?,-).
new_gecode_attr(X, H, N, BN, Attr) :-
        Attr = gfd{prob:H,idx:N, bool:BN},
        add_attribute(X, Attr, gfd).

update_space_with_events(H) :-
        H = prob{events:Es},
%        H = prob{nlevels:NL},
%        writeln("******updating new clone, current level":NL-Es),
        update_space_with_events1(Es, H),
        do_event(propagate, H, 0, _).

update_space_with_events1([], _) :- 
%        writeln("*****done updating clone"), 
        !.
update_space_with_events1([E|Es], H) :-
        update_space_with_events1(Es, H),
        % recomputing => First = 0
        do_event(E, H, 0, _).


new_prob_handle(H) :-
        new_solver_id(I),
        getval(varray_size, VSz),
        dim(VArr, [VSz]),
        H = prob{id:I,nvars:0,last_anc:[],space:Sp,events:[],vars:VArr,
                 nlevels:0,nevents:0,min: -1000000,max:1000000, prop:Susp}, 
        timestamp_update(H, cp_stamp of prob),
        make_suspension(gfd_do_propagate(H), 10, Susp),
        new_space_handle(Sp).

new_space_handle(Sp) :-
        Sp = gfd_space{handle:SH},
        timestamp_init(Sp, stamp of gfd_space),
        g_init(SH).
        
% Events

post_new_event(E, H) :-
        set_new_event(E, H),
        do_event(E, H, 1, DoProp), % First = 1 (not recomputing)
        try_propagate(DoProp, H).

try_propagate(0, _).
try_propagate(1, H) ?-
        H = prob{prop:Susp},
        schedule_suspensions(1, s([Susp])),
        wake.

:- demon gfd_do_propagate/1.
gfd_do_propagate(H) :-
        do_event(propagate, H, 1, _).
/*        H = prob{events:Es},
        (Es = [propagate|_] ->
            % do nothing 
            true
        ;
            setarg(events of prob, H, [propagate|Es]),
            do_event(propagate, H, 1, _)
        ).
*/

set_new_event(E, H) :-
        check_and_update_handle(H),
        % access H *only* after possible update of handle!
        H = prob{events:Es,nevents:NE0,space:Sp},
        g_trail_undo_for_event(Sp),
        NE1 is NE0+1,
        setarg(nevents of prob, H, NE1),
        setarg(events of prob, H, [E|Es]).

% should only be called with a new event
check_and_update_handle(H) :-
        restore_space_if_needed(H, _SpH),
        check_and_update_ancestors(H).

% can be called outside of a new event (e.g. when state is required)
restore_space_if_needed(H, SpH) :-
        H = prob{space:gfd_space{handle:SpH},last_anc:Anc},
        % pass Anc rather than the C handle, because Anc can be []
        g_check_handle(SpH, Anc, Cloned),
        update_space_if_cloned(Cloned, H).

% should only be called with a new event
check_and_update_ancestors(H) :-
        timestamp_age(H, cp_stamp of prob, Age),
        check_and_update_ancestors1(H, Age).

:- mode check_and_update_ancestors1(+,+).
check_and_update_ancestors1(_H, current).
check_and_update_ancestors1(H, old) :-
        % first event after a choicepoint
        H = prob{nlevels:NL},
        NL1 is NL + 1,
        setarg(nlevels of prob, H, NL1),
        consider_update_ancestor(H),
        timestamp_update(H, cp_stamp of prob).


consider_update_ancestor(H) :-
        H = prob{nlevels:NL,events:E},
        ((NL mod 3 =:= 1 ; E == update) ->
            do_update_ancestor(H)
        ;
            true
        ).

% clone the current space and make it the last ancestor
do_update_ancestor(H) :-
        H = prob{space:Current},
%        trace_clone(H),
        new_space_handle(New),
        setarg(last_anc of prob, H, Current),
        setarg(space of prob, H, New),
        setarg(events of prob, H, []),
        New = gfd_space{handle:NewH},
        g_check_handle(NewH, Current, _Cloned).

trace_clone(H) :-
        H = prob{nlevels:N},
        printf("New ancestor, cloning at level %d...\n", [N]).

update_space_if_cloned(1, H) :-
        update_space_with_events(H).
update_space_if_cloned(0, _H).

do_event(E, H, First, DoProp) :-
%        writeln(doing-E),
        H = prob{space:gfd_space{handle:SpH}},
        do_event1(E, SpH, First, DoProp).

do_event1(post_rc(GExpr), SpH, First, DoProp) ?-
        DoProp = 1,
        g_post_linrel_cstr(SpH, First, GExpr).
do_event1(post_bool_connectives(GBCon), SpH, First, DoProp) ?-
        DoProp = 1,
        g_post_bool_connectives(SpH, First, GBCon).
do_event1(post_alldiff(GArray), SpH, First, DoProp) ?-
        DoProp = 1,
        g_post_alldiff(SpH, First, GArray).
do_event1(post_count(Value,GArray,Rel, N), SpH, First, DoProp) ?-
        DoProp = 1,
        g_post_count(SpH, First, Value, GArray, Rel, N).
do_event1(post_interval(GArray,Lo,Hi), SpH, First, DoProp) ?-
        DoProp = 1,
        g_post_interval(SpH, First, GArray, Lo, Hi).
do_event1(post_dom(GArray,DArray), SpH, First, DoProp) ?-
        DoProp = 1,
        g_post_dom(SpH, First, GArray, DArray).
do_event1(newvars_interval(NV,Lo,Hi), SpH, _First, DoProp) ?-
        DoProp = 0,
        g_add_newvars_interval(SpH, NV, Lo, Hi).
do_event1(newvars_dom(NV,DArray), SpH, _First, DoProp) ?-
        DoProp = 0,
        g_add_newvars_dom(SpH, NV, DArray).
do_event1(setvar(Idx, Val), SpH, First, DoProp) ?-
        DoProp = 1,
        g_setvar(SpH, First, Idx, Val).
do_event1(post_sum(GArray, Rel, C), SpH, First, DoProp) ?-
        DoProp = 1,
        g_post_sum(SpH, First, GArray, Rel, C).
do_event1(newbool(V), SpH, _First, DoProp) ?-
        DoProp = 0,
        get_gecode_attr(V, _H, Attr),
        Attr = gfd{idx:Idx,bool:BIdx},
        g_add_newbool(SpH, Idx, BIdx).
do_event1(propagate, SpH, First, DoProp) ?-
        DoProp = 0,
%        g_propagate(SpH).
        get_prob_handle(prob{vars:VArr}),
%        g_propagate(SpH, First, VArr).
        g_propagate(SpH, First, InstList),
        ( InstList == [] ->
%            store_inc(stats, pg0)
            true
        ;
%            length(InstList, Len), concat_atom([pg, Len], Key), store_inc(stats, Key), 
%            get_prob_handle(prob{vars:VArr}),
            ( foreach(Idx, InstList), param(VArr, SpH) do
                arg(Idx, VArr, V),
                (integer(V) -> true ; mark_var_as_set(V),g_get_var_value(SpH, Idx, V))
%                (integer(V) -> true ; g_get_var_value(SpH, Idx, V))
            )
        ).

mark_var_as_set(_{gfd{set:S}}) ?- S = [].

ec_to_gecode_expr(E, H, GE) :-
        H = prob{nvars:N0,min:Min,max:Max},
        ec_to_gecode_expr1(E, H, N0,N, [],_, GE),
        ( N > N0 ->
            % have new variables, add them
            update_newvars_with_domain_interval(H, N, Min, Max)
        ;
            true
        ).

ec_to_gecode_var(V, H, N0,N, OldGVs0,OldGVs, GV) :-
        ( get_gecode_attr(V, H, Attr) ->
            % already a gecode var
            Attr = gfd{idx:I,bool:BI},
            N0 = N,
            gfdvar(I,BI, GV),
            (I =< arg(nvars of prob, H) ->
                % already existing Gecode variable
                OldGVs = [GV|OldGVs0]
            ;
                OldGVs = OldGVs0
            )
        ; 
            N is N0 + 1,
            gfdvar(N, BN, GV),
            OldGVs = OldGVs0,
            add_gecode_attr(V, H, N, BN),  % may fail!
            addto_varray(H, N, V)
        ).

ec_to_gecode_expr1(V, H, N0,N1, OldGVs0,OldGVs, GV) :-
        var(V), !,
        ec_to_gecode_var(V, H, N0,N1, OldGVs0,OldGVs, GV).
ec_to_gecode_expr1(I, _H, N0,N1, OldGVs0,OldGVs, GI) :-
        integer(I), !,
        N0 = N1,
        OldGVs0 = OldGVs,
        GI = I.
ec_to_gecode_expr1([], _H, N0,N1, OldGVs0,OldGVs, GNIL) ?-
        GNIL = [], !,
        N0 = N1,
        OldGVs = OldGVs0.
ec_to_gecode_expr1(eval(E), H, N0,N, OldGVs0,OldGVs, GE) ?- !,
        ec_to_gecode_expr1(E, H, N0,N, OldGVs0,OldGVs, GE).
ec_to_gecode_expr1(subscript(T,S), H, N0,N, OldGVs0,OldGVs, GE) ?- !,
        subscript(T,S,E),
        ec_to_gecode_expr1(E, H, N0,N, OldGVs0,OldGVs, GE).
ec_to_gecode_expr1(sum(L0), H, N0,N, OldGVs0,OldGVs, GL) ?- !,
        collection_to_list(flatten(L0),L), 
        ec_to_gecode_sumlist(L, H, N0,N, OldGVs0,OldGVs, GL).
ec_to_gecode_expr1(E, H, N0,N, OldGVs0,OldGVs, GE) :-
        compound(E),
        functor(E, Name, Arity),
        functor(GE, Name, Arity),
        ( foreacharg(Arg, E), foreacharg(GArg, GE),
          fromto(N0, N1,N2, N), 
          fromto(OldGVs0, OldGVs1,OldGVs2, OldGVs),
          param(H)
        do
            ec_to_gecode_expr1(Arg, H, N1,N2, OldGVs1,OldGVs2, GArg)
        ).

ec_to_gecode_sumlist([E|L], H, N0,N, OldGVs0,OldGVs, GL) :-
        ec_to_gecode_expr1(E, H, N0,N1, OldGVs0,OldGVs1, GE1),
        ( L == [] ->
            GL = GE1,
            N = N1,
            OldGVs = OldGVs1
        ;
            GL = GE1 + GL1,
            ec_to_gecode_sumlist(L, H, N1,N, OldGVs1,OldGVs, GL1)
        ).

/*
indomain(I) :- integer(I), !.
indomain(V{gfd:Attr}) ?-
        nonvar(Attr),
        Attr = gfd{prob:H,idx:Idx},
        H = prob{space:eg_space{handle:SpH}},
        g_get_var_domain(SpH, Idx, Domain),
        set_domain_value(Domain, V).

set_domain_value([D|_Ds], V) ?-
        set_a_domain_value(D, V).
set_domain_value([_|Ds], V) ?-
        set_domain_value(Ds, V).

set_a_domain_value(Lo..Hi, V) ?- !,
        between(Lo, Hi, 1, Val),
        set_a_domain_value(Val, V).
%        Val = V.
set_a_domain_value(I, V) :-
        V = I.
/*/

indomain(V) :-
        do_indomain(V, 1).

indomain_min(V) :-
        do_indomain(V, 1).

labeling(Vars) :-
        collection_to_list(Vars, List),
        ( foreach(Var, List) do indomain(Var) ).


do_indomain(I, _) :- integer(I), !.
do_indomain(V{gfd:Attr}, Prop) ?-
        nonvar(Attr),
        Attr = gfd{prob:H, idx:Idx},
        restore_space_if_needed(H, SpH),
        g_get_var_lwb(SpH, Idx, Lo),
        indomain_and_prop(Idx, V, H, Prop, Lo).

indomain_and_prop(_Idx, V, _H, _P, Val) :-
        V = Val.
indomain_and_prop(Idx, V, H, Prop, Val) :-
        restore_space_if_needed(H, SpH), 
        g_get_var_lwb_after_update(SpH, Idx, Val, Prop, Lo),
        indomain_and_prop(Idx, V, H, Prop, Lo).

/* search/6 maps to Gecode's branching and search engines. 
   As cloning is done before and after the use of the search engine,
   we cannot treat the search as a normal gfd event. Instead, the 
   event queue is marked, so that the space returned after the search
   will become an ancestor if further events are posted. 
*/   
search(Vars, Pos, Select, Choice, Method, _Option) :-
        collection_to_list(Vars, List),
        get_prob_handle(H),
        H = prob{space:gfd_space{handle:SpH}},
        ec_to_gecode_expr(List, H, GList),
        translate_method(Method, TMethod),
        GArray =.. [[]|GList],
        g_setup_search(SpH, GArray, Select, Choice, TMethod, EngH, MethodCode),
        new_space_handle(SP),
        setarg(space of prob, H, SP),
        do_search(SpH, EngH, MethodCode, H).

:- mode translate_method(+, -).
translate_method(complete, complete).
translate_method(lds(D), lds(D)) :- integer(D).
translate_method(bb_min(CV),  bb_min(CIdx)) :-
        get_gecode_attr(CV, _, gfd{idx:CIdx}).
translate_method(bb_min(CV),  bb_min(CIdx)) :-
        get_gecode_attr(CV, _, gfd{idx:CIdx}).
translate_method(restart_min(CV),  restart_min(CIdx)) :-
        get_gecode_attr(CV, _, gfd{idx:CIdx}).

mark_handle_for_ancestor_update(H) :-
        setarg(events of prob, H, update).

do_search(LastSpH, EngH, MethodCode, H) :-
        mark_handle_for_ancestor_update(H),
        H = prob{space:SP},
        repeat,
        (g_do_search(SP, EngH, MethodCode, LastSpH, InstList) -> % fails if no more solution
            % a clone is created automatically by gecode's search
            % make sure an ancestor will be created on new event
            H = prob{vars:VArr},
            SP = gfd_space{handle:SpH},
            ( InstList == [] ->
                %store_inc(stats, pg0)
                true
            ;
                %length(InstList, Len), concat_atom([pg, Len], Key), store_inc(stats, Key), 
                ( foreach(Idx, InstList), param(VArr, SpH) do
                    arg(Idx, VArr, V),
                    (integer(V) -> true ; mark_var_as_set(V),g_get_var_value(SpH, Idx, V))
                )
            )
        ;
            !, fail
        ).


