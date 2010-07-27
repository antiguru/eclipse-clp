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
:- export alldifferent/1, occurrences/3, atmost/3, count/4, element/3.
:- export labeling/1, indomain/1, indomain_min/1, indomain/2.
:- export is_in_domain/2, is_in_domain/3.
:- export search/6.
:- export (and)/2, (or)/2, (xor)/2, (<=>)/2, (=>)/2, neg/1.

:- export get_min/2, get_max/2.

:- tool('#\\='/2, '#\\=_body'/3).
:- tool('#='/2, '#=_body'/3).
:- tool('#<'/2, '#<_body'/3).
:- tool('#>'/2, '#>_body'/3).
:- tool('#>='/2, '#>=_body'/3).
:- tool('#=<'/2, '#=<_body'/3).

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
        external(g_state_is_stable/1, p_g_state_is_stable),
        external(g_check_handle/3, p_g_check_handle),
        external(g_trail_undo_for_event/1, p_g_trail_undo_for_event),
        external(g_delete/1, p_g_delete),
        external(g_add_newbool/3, p_g_add_newbool),
        external(g_add_newvars_interval/4, p_g_add_newvars_interval),
        external(g_add_newvars_dom/3, p_g_add_newvars_dom),
        external(g_post_interval/5, p_g_post_interval),
        external(g_post_var_interval_reif/6, p_g_post_var_interval_reif),
        external(g_post_dom/4, p_g_post_dom),
        external(g_post_var_dom_reif/5, p_g_post_var_dom_reif),
        external(g_post_var_val_reif/5, p_g_post_var_val_reif),
        external(g_post_setvar/4, p_g_post_setvar),
        external(g_post_linrel_cstr/3, p_g_post_linrel_cstr),
        external(g_post_bool_connectives/3, p_g_post_bool_connectives),
        external(g_post_alldiff/3, p_g_post_alldiff),
        external(g_post_count/6, p_g_post_count),
        external(g_post_element/5, p_g_post_element),
        external(g_post_sum/5, p_g_post_sum),
        external(g_post_maxlist/4, p_g_post_maxlist),
        external(g_post_minlist/4, p_g_post_minlist),
        external(g_post_sqrt/4, p_g_post_sqrt),
        external(g_post_sq/4, p_g_post_sq),
        external(g_post_abs/4, p_g_post_abs),
        external(g_post_div/5, p_g_post_div),
        external(g_post_mult/5, p_g_post_mult),
        external(g_post_mod/5, p_g_post_mod),
        external(g_post_min2/5, p_g_post_min2),
        external(g_post_max2/5, p_g_post_max2),
        external(g_propagate/3, p_g_propagate),
        external(g_check_val_is_in_var_domain/3, p_g_check_val_is_in_var_domain),
        external(g_get_var_bounds/4, p_g_get_var_bounds),
        external(g_get_var_value/3, p_g_get_var_value),
        external(g_get_var_domain/3, p_g_get_var_domain),
        external(g_get_var_lwb/3, p_g_get_var_lwb),
        external(g_get_var_lwb_after_update/5, p_g_get_var_lwb_after_update),
        external(g_get_var_upb/3, p_g_get_var_upb),
        external(g_setup_search/7, p_g_setup_search),
        external(g_do_search/5, p_g_do_search).

:- export struct(
        gfd_prob(
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

:- local 
     variable(interval_min, -1000000), 
     variable(interval_max,1000000).

:- export gfd_set_default_interval/2.

gfd_set_default_interval(min, Min) ?-
        integer(Min), !,
        setval(interval_min, Min).
gfd_set_default_interval(max, Max) ?-
        integer(Max), !,
        setval(interval_max, Max).

:- export gfd_get_default_interval/2.

gfd_get_default_interval(min, Min) ?- !,
        getval(interval_min, Min).
gfd_get_default_interval(max, Max) ?- !,
        getval(interval_max, Max).



boolean_expr(E) :-
        reifiable_constraint(E), !.
boolean_expr(E) :-
        connective(E, _, _).


reifiable_constraint(E) :-
        relation_constraint(E, _, _, _), !.
reifiable_constraint(E) :-
        domain_constraint(E,_, _), !.

domain_constraint((Vs0 :: Dom0), Vs, Dom) ?- !,
        Vs = Vs0, Dom = Dom0.
domain_constraint((Vs0 #:: Dom0), Vs, Dom) ?- !,
        Vs = Vs0, Dom = Dom0.

relation_constraint(X0 #= Y0, RelOp, X, Y) ?- !,
        RelOp = (#=), 
        X0 = X,
        Y0 = Y.
relation_constraint(X0 #\= Y0, RelOp, X, Y) ?- !,
        RelOp = (#\=), 
        X0 = X,
        Y0 = Y.
relation_constraint(X0 #< Y0, RelOp, X, Y) ?- !,
        RelOp = (#<), 
        X0 = X,
        Y0 = Y.
relation_constraint(X0 #=< Y0, RelOp, X, Y) ?- !,
        RelOp = (#=<), 
        X0 = X,
        Y0 = Y.
relation_constraint(X0 #> Y0, RelOp, X, Y) ?- !,
        RelOp = (#>), 
        X0 = X,
        Y0 = Y.
relation_constraint(X0 #>= Y0, RelOp, X, Y) ?- !,
        RelOp = (#>=), 
        X0 = X,
        Y0 = Y.


connective(X and Y, ConOp, Args) ?- !,
        ConOp = and,
        Args = [X,Y].
connective(X or Y, ConOp, Args) ?- !,
        ConOp = or,
        Args = [X,Y].
connective(X xor Y, ConOp, Args) ?- !,
        ConOp = xor,
        Args =[X,Y].
connective(X <=> Y, ConOp, Args) ?- !,
        ConOp = (<=>),
        Args = [X,Y].
connective(X => Y, ConOp, Args) ?- !,
        ConOp = (=>),
        Args = [X,Y].
connective(neg(X), ConOp, Args) ?- 
        ConOp = neg,
        Args = [X].

rel_op('#=').
rel_op('#\\=').
rel_op('#<').
rel_op('#=<').
rel_op('#>').
rel_op('#>=').

% low level representation of variable
gfdvar(I,B,'_ivar'(I,B)).

% get the low-level representation of an existing gecide variable
get_gecode_var(_{gfd:Attr}, GV) ?-
        nonvar(Attr),
        Attr = gfd{idx:I,bool:B},
        gfdvar(I, B, GV).


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
/*        H = gfd_prob{space:gfd_space{handle:SpH}},
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
        HX = gfd_prob{id:Id}, 
        HY = gfd_prob{id:Id},
        ( IdxX == IdxY ->
            true   % same variable, do nothing
        ;
            % unless both variables have their own boolean already, make
            % sure they share the same boolean
            ( integer(BX), integer(BY) -> 
                true
            ;
                BX = BY
            ),
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
        (H0 \= gfd_prob{} ->
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
  H = gfd_prob{vars:VArr},
  ( Idx > arity(VArr) ->
      expand_and_copy_array(VArr, NewVArr),
      setarg(vars of gfd_prob, H, NewVArr),
      arg(Idx, NewVArr, V)
  ;
      arg(Idx, VArr, V)
  ).

expand_and_copy_array(Old, New) :-
        arity(Old, OldSize),
        NewInc is getval(varray_size)*2,
        setval(varray_size, NewInc),
        NewSize is OldSize + NewInc,  
        dim(New, [NewSize]),
        ( foreacharg(A, Old, Idx), param(New) do
            arg(Idx, New, A)
        ).

:- tool('::'/2, '::_body'/3).
:- tool('#::'/2, '::_body'/3).

'::_body'(X, Domain, Module):-
        get_prob_handle(H),
        H = gfd_prob{nvars:NV0},
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
        H = gfd_prob{nvars:N0,min:Min,max:Max},
        ec_to_gecode_domain_reified1(X, Domain, Bool, H, N0,N, [],Bs, Event, _, Module),
        update_vars_for_gecode(N0, N, Bs, H, Min, Max),
        post_new_event(Event, H).

ec_to_gecode_domain_reified1(X, Domain, Bool, H, N0,N, Bs0,Bs, Event, GBool, Module) :-
        process_domain_domain(Domain, NDomain, Module),
        ( var(Bool) ->
            ec_to_gecode_var1(Bool, H, N0,N1, [],_Old, GBool),
            Bs = [Bool|Bs0]
        ;
            GBool = Bool,
            N0 = N1,
            Bs = Bs0
        ),
        ( var(X) ->
            ec_to_gecode_var1(X, H, N1, N, [],_, GX)
        ; integer(X) ->
            GX = X,
            N1 = N
        ;
            fail
        ),
        ( NDomain = [I..I] ->
            Event = post_var_val_reif(GX, I, GBool)
        ; 
            NDomain = [Lo..Hi|T],
            ( T == [] ->
                % Domain is a simple interval
                Event = post_var_interval_reif(GX,Lo,Hi, GBool)
            ;
                DArray =.. [[]|NDomain],
                Event = post_var_dom_reif(GX,DArray,GBool)
            )
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
%        ground(A),
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
        ec_to_gecode_var1(V1, H, NV0,NV1, OldGVs0,OldGVs1, _),
        process_domain_vars(Vs, Domain, H, NV1,NV, OldGVs1,OldGVs).
process_domain_vars([I|Vs], Domain, H, NV0,NV, OldGVs0,OldGVs) :-
        integer(I), !,
        is_in_given_domain(I, Domain),
        process_domain_vars(Vs, Domain, H, NV0,NV, OldGVs0,OldGVs).
process_domain_vars([], _D, _H, NV,NV, OldGVs,OldGVs).

is_in_given_domain(I, [Lo..Hi|Ds]) :-
        (I >= Lo, I =< Hi -> true ; is_in_given_domain(I, Ds)).


assign_domain(Domain, H, NV, OldGVs) ?- 
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
        ( NV =:= arg(nvars of gfd_prob, H) -> % no new vars 
            true
        ;
            setarg(nvars of gfd_prob, H, NV),
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
        ( NV =:= arg(nvars of gfd_prob, H) -> % no new vars 
            true
        ;
            setarg(nvars of gfd_prob, H, NV),
            post_new_event(newvars_dom(NV,DArray), H)
        ).


is_not_boolvar(_{gfd:Attr}) ?-
        nonvar(Attr), !,
        Attr = gfd{bool:Link},
        var(Link).
is_not_boolvar(_).

link_var_to_boolvar(V, H) :-
        ( (var(V), is_not_boolvar(V)) ->
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


update_vars_for_gecode(N0, N, Bs, H, Min, Max) :-
        ( N > N0 ->
            % have new variables, add them (and link any booleans to their var)
            update_newvars_with_domain_interval(H, N, Min, Max)
        ;
            true
        ),
        ( foreach(B, Bs), param(H) do % link the boolean vars
            link_var_to_boolvar(B, H) 
        ).

post_connectives(Conn, Module) :-
        get_prob_handle(H),
        H = gfd_prob{nvars:N0,min:Min,max:Max},
        ec_to_gecode_bool_expr1(Conn, H, N0,N, [],Bs, Auxs0,AuxsT, GConn, Module),
%        ec_to_gecode_connectives1(Conn, H, N0,N, [],Bs, Auxs0,AuxsT, GConn, Module),
        update_vars_for_gecode(N0, N, Bs, H, Min, Max),
        post_new_event_with_aux([post_bool_connectives(GConn)|Auxs0],AuxsT, H).



:- tool((and)/2, and_body/3).
:- tool((or)/2, or_body/3).
:- tool((xor)/2, xor_body/3).
:- tool(neg/1, neg_body/2).
:- tool('<=>'/2, '<=>_body'/3).
:- tool('=>'/2, '=>_body'/3).

and_body(EX, EY, Module) :-
        post_connectives((EX and EY), Module).

or_body(EX, EY, Module) :-
        post_connectives((EX or EY), Module).

xor_body(EX, EY, Module) :-
        post_connectives((EX xor EY), Module).

'<=>_body'(EX, EY, Module) :-
        post_connectives((EX <=> EY), Module).

'=>_body'(EX, EY, Module) :-
        post_connectives((EX => EY), Module).

neg_body(EX, Module) :-
        post_connectives(neg(EX), Module).

post_bool_connectives(H, GBCon) :-
        post_new_event(post_bool_connectives(GBCon), H).


        
'#\\=_body'(EX, EY, Module) :-
        post_rel_cons((#\=), EX, EY, Module).

'#=_body'(EX, EY, Module) :-
        % optimisation for top-level non-linear expressions
        ( nonlin_op(EX, EXTemp, EXType) ->
            % EX nonlinear (EY may be)
            post_nonlin_eq(EX, EXType, EXTemp, EY, Module)
        ; nonlin_op(EY, EYTemp, EYType) ->
            % EY nonlinear EX linear op
            post_nonlinlin_eq(EY, EYType, EYTemp, EX, Module)
        ;
            post_rel_cons((#=), EX, EY, Module)
        ).

'#<_body'(EX, EY, Module) :-
        post_rel_cons((#<), EX, EY, Module).

'#>_body'(EX, EY, Module) :-
        post_rel_cons((#>), EX, EY, Module).

'#>=_body'(EX, EY, Module) :-
        post_rel_cons((#>=), EX, EY, Module).

'#=<_body'(EX, EY, Module) :-
        post_rel_cons((#=<), EX, EY, Module).

post_nonlin_eq(NLExpr, NLType, NLEvent, Expr2, Module) :-
        ( nonlin_op(Expr2, E2Event, E2Type) ->
            get_prob_handle(H),
            H = gfd_prob{nvars:N0,min:Min,max:Max},
            new_gfdvar(_NewV, H, N0,N1, GNewV),
            arg(1, E2Event, GNewV),
            arg(1, NLEvent, GNewV),
            ec_to_gecode_nonlin_op1(NLType, NLExpr, H, N1,N2, [],Bs1, Auxs0,
                                    Auxs1, NLEvent, Module),
            ec_to_gecode_nonlin_op1(E2Type, Expr2, H, N2,N, Bs1,Bs, Auxs1,
                                    AuxsT, E2Event, Module),
            update_vars_for_gecode(N0, N, Bs, H, Min, Max),
            post_new_event_with_aux([NLEvent,E2Event|Auxs0], AuxsT, H)
        ; % Expr2 has linear operator
            post_nonlinlin_eq(NLExpr, NLType, NLEvent, Expr2, Module)
        ).

post_nonlinlin_eq(NLExpr, NLType, NLEvent, LinOpExpr, Module) :-
        get_prob_handle(H),
        H = gfd_prob{nvars:N0,min:Min,max:Max},
        ( var(LinOpExpr) ->
            ec_to_gecode_var1(LinOpExpr, H, N0,N1, [],_, GV),
            arg(1, NLEvent, GV),
            ec_to_gecode_nonlin_op1(NLType, NLExpr, H, N1,N, [],Bs, Auxs0,
                                    AuxsT, NLEvent, Module),
            Events = [NLEvent|Auxs0]
        ; integer(LinOpExpr) ->
            arg(1, NLEvent, LinOpExpr),
            ec_to_gecode_nonlin_op1(NLType, NLExpr, H, N0,N, [],Bs, Auxs0,
                                    AuxsT, NLEvent, Module),
            Events = [NLEvent|Auxs0]
        ; 
            new_gfdvar(_NewV, H, N0,N1, GNewV),
            arg(1, NLEvent, GNewV),
            ec_to_gecode_nonlin_op1(NLType, NLExpr, H, N1,N2, [],Bs1, Auxs0,
                                    Auxs1, NLEvent, Module),
            ec_to_gecode_arith_expr1(LinOpExpr, H, 0, N2,N, Bs1,Bs, Auxs1,
                                     AuxsT, GLinOpExpr, Module),
            Events = [post_rc(GNewV #= GLinOpExpr),NLEvent|Auxs0]
        ),
        update_vars_for_gecode(N0, N, Bs, H, Min, Max),
        post_new_event_with_aux(Events, AuxsT, H).

post_rel_cons(RelOp, EX, EY, Module) :-
        get_prob_handle(H),
        H = gfd_prob{nvars:N0,min:Min,max:Max},
        ec_to_gecode_expr1(EX, H, N0,N1, [],Bs1, Auxs0,Auxs1, GEX, Module),
        ec_to_gecode_expr1(EY, H, N1,N2, Bs1,Bs2, Auxs1,Auxs2, GEY, Module),
        construct_relcons_event1(RelOp, EX, EY, GEX, GEY, H, N2,N, Bs2,Bs,
                                 Auxs2,AuxsT, Event), 
        update_vars_for_gecode(N0, N, Bs, H, Min, Max),
        post_new_event_with_aux([Event|Auxs0],AuxsT, H).

construct_relcons_event1((#=), EX, EY, GEX, GEY, H, N0,N, Bs0,Bs, Auxs0,AuxsT, Event) ?-
        !,
        ( boolean_expr(EX) ->
            ( boolean_expr(EY) ->
                Bs = Bs0,
                Event = post_bool_connectives(GEX<=>GEY),
                Auxs0 = AuxsT
            ; var(EY) ->
                Bs = [EY|Bs0],
                N = N0,
                Auxs0 = AuxsT,
                Event = post_bool_connectives(GEY<=>GEX)
            ; integer(EY) ->
                Bs = Bs0,
                N = N0,
                Auxs0 = AuxsT,
                ( EY == 1 ->
                    Event = post_bool_connectives(GEX)
                ; EY == 0 ->
                    % let Gecode do the optimisation if EY == 0
                    Event = post_bool_connectives(GEY<=>GEX)
                ;
                    abort
                )
            ; 
                new_gfdvar(BVar, H, N0,N, GBVar),
                Bs = [BVar|Bs0],
                Event = post_bool_connectives(GBVar<=>GEX),
                Auxs0 = [post_rc(GBVar #= GEY)|AuxsT]
            )
        ; boolean_expr(EY) ->
            ( var(EX) ->
                Bs = [EX|Bs0],
                N = N0,
                Auxs0 = AuxsT,
                Event = post_bool_connectives(GEX<=>GEY)
            ; integer(EX) ->
                Bs = Bs0,
                N = N0,
                Auxs0 = AuxsT,
                ( EX == 1 ->
                    Event = post_bool_connectives(GEY)
                ; EX == 0 ->
                    Event = post_bool_connectives(GEX<=>GEY)
                ;
                    abort
                )
            ;
                new_gfdvar(BVar, H, N0,N, GBVar),
                Bs = [BVar|Bs0],
                Event = post_bool_connectives(GBVar<=>GEY),
                Auxs0 = [post_rc(GBVar #= GEX)|AuxsT]
            )
        ;
            Bs = Bs0,
            N = N0,
            Auxs0 = AuxsT,
            Event = post_rc(GEX #= GEY)
        ).
/*ec_to_gecode_relcons1((#\=), EX, EY, GEX, GEY, H, N0,N, Bs0,Bs, Auxs0,AuxsT, Event) ?-
        !,
        ( boolean_expr(EX) ->
            ( var(EY) ->*/
construct_relcons_event1(RelOp, EX, EY, GEX, GEY, H, N0,N, Bs0,Bs, Auxs0,AuxsT, Event) ?-
        ( boolean_expr(EX) ->
            new_gfdvar(BXV, H, N0,N1, GBXV),
            Bs1 = [BXV|Bs0],
            Auxs0 = [post_bool_connectives(GBXV<=>GEX)|Auxs1],
            GNewX = GBXV
        ;
            N1 = N0,
            Bs1 = Bs0,
            Auxs0 = Auxs1,
            GNewX = GEX
        ),
        ( boolean_expr(EY) ->
            new_gfdvar(BYV, H, N1,N, GBYV),
            Bs = [BYV|Bs1],
            Auxs1 = [post_bool_connectives(GBYV<=>GEY)|AuxsT],
            GNewY = GBYV
        ;
            N = N1,
            Bs = Bs1,
            Auxs1 = AuxsT,
            GNewY = GEY
        ),
        RC =.. [RelOp, GNewX, GNewY],
        Event = post_rc(RC).

alldifferent(Vars) :-
        collection_to_list(Vars, List),
        get_prob_handle(H),
        ec_to_gecode_varlist(List, H, GList),
        GArray =.. [[]|GList],
        post_new_event(post_alldiff(GArray), H).



count(Value, Vars, Rel, N) :-
        integer(Value),
        atomic(Rel),
        rel_op(Rel),
        collection_to_list(Vars, List),
        get_prob_handle(H),
        ec_to_gecode_varlist(List, H, GList),
        GArray =.. [[]|GList],
        ( integer(N) ->
            N =< arity(GArray),
            GN = N
        ; var(N) ->
            N :: 0..arity(GArray),
            get_gecode_var(N, GN)
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

element(Index, Collection, Value) :-
        % Gecode's element constraint index starts from 0!
        ( integer(Index) ->
            GIndex is Index - 1
        ; var(Index) ->
            Index0 #= Index - 1,
            get_gecode_var(Index0, GIndex) 
        ;
            fail
        ),
        collection_to_list(Collection, List),
        get_prob_handle(H),
        ec_to_gecode_varlist([Value|List], H, [GValue|GList]),
        Array =.. [[]|GList],
        post_new_event(post_element(GIndex, Array, GValue), H).

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
        H = gfd_prob{space:gfd_space{handle:SpH}},
        g_get_var_domain(SpH, Idx, Domain).

gfd_var_print(X, Domain) :-
        get_gecode_domain(X, Domain).

:- mode new_gecode_attr(?,+,+,?,-).
new_gecode_attr(X, H, N, BN, Attr) :-
        Attr = gfd{prob:H,idx:N, bool:BN},
        add_attribute(X, Attr, gfd).

update_space_with_events(H) :-
        H = gfd_prob{events:Es},
%        H = gfd_prob{nlevels:NL},
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
        getval(interval_min, Min),
        getval(interval_max, Max),
        H = gfd_prob{id:I,nvars:0,last_anc:[],space:Sp,events:[],vars:VArr,
                 nlevels:0,nevents:0,min: Min,max:Max, prop:Susp}, 
        timestamp_update(H, cp_stamp of gfd_prob),
        make_suspension(gfd_do_propagate(H), 10, Susp),
        new_space_handle(Sp).

new_space_handle(Sp) :-
        Sp = gfd_space{handle:SH},
        timestamp_init(Sp, stamp of gfd_space),
        g_init(SH).
        
% Events

% posting an event that may have additional "auxillary" events
post_new_event_with_aux(Es, EsTail, H) :-
        set_new_event(Es, EsTail0, H),
        post_new_events1(Es, H, _First, DoProp),
        EsTail = EsTail0, % join posted events to head of events list 
        ( var(DoProp) -> true ; try_propagate(1, H) ).


post_new_events1(Es, H, First, DoProp) :-
        ( var(Es) -> % at end
            First = 1
        ; 
            Es = [E|Es1],
            % do events in reverse order (as during recomputation)
            % and only first event done has First = 1
            First = 0, 
            post_new_events1(Es1, H, First1, DoProp),
            do_event(E, H, First1, DoProp0), 
            (DoProp0 == 1 -> DoProp = 1 ; true)
        ).


post_new_event(E, H) :-
        Es = [E|ET],
        set_new_event(Es, ET, H),
        do_event(E, H, 1, DoProp), % First = 1 (not recomputing)
        try_propagate(DoProp, H).

try_propagate(0, _).
try_propagate(1, H) ?-
        H = gfd_prob{prop:Susp},
        schedule_suspensions(1, s([Susp])),
        wake.

:- demon gfd_do_propagate/1.
gfd_do_propagate(H) :-
        do_event(propagate, H, 1, _).
/*        H = gfd_prob{events:Es},
        (Es = [propagate|_] ->
            % do nothing 
            true
        ;
            setarg(events of gfd_prob, H, [propagate|Es]),
            do_event(propagate, H, 1, _)
        ).
*/

set_new_event(Es, EsT, H) :-
        check_and_update_handle(H),
        % access H *only* after possible update of handle!
        H = gfd_prob{events:EsT,nevents:NE0,space:Sp},
        g_trail_undo_for_event(Sp),
        NE1 is NE0+1,
        setarg(nevents of gfd_prob, H, NE1),
        setarg(events of gfd_prob, H, Es).

% should only be called with a new event
check_and_update_handle(H) :-
        restore_space_if_needed(H, _SpH),
        check_and_update_ancestors(H).

% can be called outside of a new event (e.g. when state is required)
restore_space_if_needed(H, SpH) :-
        H = gfd_prob{space:gfd_space{handle:SpH},last_anc:Anc},
        % pass Anc rather than the C handle, because Anc can be []
        g_check_handle(SpH, Anc, Cloned),
        update_space_if_cloned(Cloned, H).

% should only be called with a new event
check_and_update_ancestors(H) :-
        timestamp_age(H, cp_stamp of gfd_prob, Age),
        check_and_update_ancestors1(H, Age).

:- mode check_and_update_ancestors1(+,+).
check_and_update_ancestors1(_H, current).
check_and_update_ancestors1(H, old) :-
        % first event after a choicepoint
        H = gfd_prob{nlevels:NL, events:E,space:Current},
        ( g_state_is_stable(Current) ->
            % only clone if state is stable, i.e. have propagation done
            % this may not be the case if propagation has been delayed
            NL1 is NL + 1,
            setarg(nlevels of gfd_prob, H, NL1),
            consider_update_ancestor(H, NL1, E)
        ;
            true
        ),
        timestamp_update(H, cp_stamp of gfd_prob).


consider_update_ancestor(H, NL, E) :-
        ( NL mod 1 =:= 0 ->
            do_update_ancestor(H)
        ; E == update ->
            do_update_ancestor(H)
        ;
            true
        ).


% clone the current space and make it the last ancestor
do_update_ancestor(H) :-
        H = gfd_prob{space:Current},
%        trace_clone(H),
        new_space_handle(New),
        setarg(last_anc of gfd_prob, H, Current),
        setarg(space of gfd_prob, H, New),
        setarg(events of gfd_prob, H, []),
        New = gfd_space{handle:NewH},
        g_check_handle(NewH, Current, _Cloned).

trace_clone(H) :-
        H = gfd_prob{nlevels:N},
        printf("New ancestor, cloning at level %d...\n", [N]).

update_space_if_cloned(1, H) :-
        update_space_with_events(H).
update_space_if_cloned(0, _H).

do_event(E, H, First, DoProp) :-
%        writeln(doing-E-First),
        H = gfd_prob{space:gfd_space{handle:SpH}},
%        arg(space of gfd_prob, H, Space),
%        arg(handle of gfd_space, Space, SpH),
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
do_event1(post_element(GI,GArray,GValue), SpH, First, DoProp) ?-
        DoProp =1,
        g_post_element(SpH, First, GI, GArray, GValue).
do_event1(post_interval(GArray,Lo,Hi), SpH, First, DoProp) ?-
        DoProp = 1,
        g_post_interval(SpH, First, GArray, Lo, Hi).
do_event1(post_var_interval_reif(GV,Lo,Hi,GBool), SpH, First, DoProp) ?-
        DoProp = 1,
        g_post_var_interval_reif(SpH, First, GV, Lo, Hi, GBool).
do_event1(post_dom(GArray,DArray), SpH, First, DoProp) ?-
        DoProp = 1,
        g_post_dom(SpH, First, GArray, DArray).
do_event1(post_var_dom_reif(GV,DArray,GBool), SpH, First, DoProp) ?-
        DoProp = 1,
        g_post_var_dom_reif(SpH, First, GV, DArray, GBool).
do_event1(post_var_val_reif(GV,Value,GBool), SpH, First, DoProp) ?-
        DoProp = 1,
        g_post_var_val_reif(SpH, First, GV, Value, GBool).
do_event1(newvars_interval(NV,Lo,Hi), SpH, _First, DoProp) ?-
        DoProp = 0,
        g_add_newvars_interval(SpH, NV, Lo, Hi).
do_event1(newvars_dom(NV,DArray), SpH, _First, DoProp) ?-
        DoProp = 0,
        g_add_newvars_dom(SpH, NV, DArray).
do_event1(setvar(Idx, Val), SpH, First, DoProp) ?-
        DoProp = 1,
        g_post_setvar(SpH, First, Idx, Val).
do_event1(post_sum(GArray, Rel, C), SpH, First, DoProp) ?-
        DoProp = 1,
        g_post_sum(SpH, First, GArray, Rel, C).
do_event1(post_maxlist(GV, GArray), SpH, First, DoProp) ?-
        DoProp = 1,
        g_post_maxlist(SpH, First, GV, GArray).
do_event1(post_minlist(GV, GArray), SpH, First, DoProp) ?-
        DoProp = 1,
        g_post_minlist(SpH, First, GV, GArray).
do_event1(post_sqrt(GRes, GX), SpH, First, DoProp) ?-
        DoProp = 1,
        g_post_sqrt(SpH, First, GRes, GX).
do_event1(post_sq(GRes, GX,_Power), SpH, First, DoProp) ?-
        DoProp = 1,
        g_post_sq(SpH, First, GRes, GX).
do_event1(post_sq(GRes, GX), SpH, First, DoProp) ?-
        DoProp = 1,
        g_post_sq(SpH, First, GRes, GX).
do_event1(post_abs(GRes, GX), SpH, First, DoProp) ?-
        DoProp = 1,
        g_post_abs(SpH, First, GRes, GX).
do_event1(post_div(GRes, GX, GY), SpH, First, DoProp) ?-
        DoProp = 1,
        g_post_div(SpH, First, GRes, GX, GY).
do_event1(post_mult(GRes, GX, GY), SpH, First, DoProp) ?-
        DoProp = 1,
        g_post_mult(SpH, First, GRes, GX, GY).
do_event1(post_mod(GRes, GX, GY), SpH, First, DoProp) ?-
        DoProp = 1,
        g_post_mod(SpH, First, GRes, GX, GY).
do_event1(post_min2(GRes, GX, GY), SpH, First, DoProp) ?-
        DoProp = 1,
        g_post_min2(SpH, First, GRes, GX, GY).
do_event1(post_max2(GRes, GX, GY), SpH, First, DoProp) ?-
        DoProp = 1,
        g_post_max2(SpH, First, GRes, GX, GY).
do_event1(newbool(V), SpH, _First, DoProp) ?-
        DoProp = 0,
        get_gecode_attr(V, _H, Attr),
        Attr = gfd{idx:Idx,bool:BIdx},
        g_add_newbool(SpH, Idx, BIdx).
do_event1(propagate, SpH, First, DoProp) ?-
        DoProp = 0,
%        g_propagate(SpH).
        get_prob_handle(gfd_prob{vars:VArr}),
%        g_propagate(SpH, First, VArr).
        g_propagate(SpH, First, InstList),
        ( InstList == [] ->
%            store_inc(stats, pg0)
            true
        ;
%            length(InstList, Len), concat_atom([pg, Len], Key), store_inc(stats, Key), 
%            get_prob_handle(gfd_prob{vars:VArr}),
            ( foreach(Idx, InstList), param(VArr, SpH) do
                arg(Idx, VArr, V),
                (integer(V) -> true ; mark_var_as_set(V),g_get_var_value(SpH, Idx, V))
%                (integer(V) -> true ; g_get_var_value(SpH, Idx, V))
            )
        ).

mark_var_as_set(_{gfd{set:S}}) ?- S = [].

ec_to_gecode_varlist(L, H, GL) :-
        H = gfd_prob{nvars:N0,min:Min,max:Max},
        ec_to_gecode_varlist1(L, H, N0,N, GL),
        ( N > N0 ->
            % have new variables, add them
            update_newvars_with_domain_interval(H, N, Min, Max)
        ;
            true
        ).

 ec_to_gecode_varlist1(L, H, N0,N, GL) :-
        ( foreach(V, L), 
          fromto(N0, N1,N2, N),
          param(H),
          foreach(GV, GL)
        do
            ( var(V) ->
                ec_to_gecode_var1(V, H, N1,N2, [],_, GV)
            ; integer(V) ->
                GV = V,
                N1 = N2
            ;
                fail
            )
        ).

/* sum(Vs) is not supported in all versions of Gecode (added 3.3.0)
ec_to_gecode_arith_exprlist1(List, H, Lin, N0,N, Bs0,Bs, Auxs0,Auxs, GList, Module) :-
        ( foreach(E,List), fromto(N0, N1,N2, N), 
          fromto(Auxs0, As1,As2, Auxs), fromto(Bs0, Bs1,Bs2, Bs),
          param(H,Module,Lin),
          foreach(GE,GList)
        do
            ( var(E) ->
                As2 = As1,
                Bs2 = Bs1,
                ec_to_gecode_var1(E, H, N1,N2, [],_, GE)
            ; integer(E) ->
                N2 = N1,
                GE = E,
                Bs2 = Bs1,
                As2 = As1
            ; bound(E, GE, Module) ->
                N2 = N1,
                Bs2 = Bs1,
                As2 = As1
            ; 
                new_gfdvar(_NewV, H, N1,N3, GE),
                ec_to_gecode_arith_expr1(E, H, Lin, N3,N2, Bs1,Bs2, As1,As3, GExpr, Module),
                As3 = [post_rc(GE #= GExpr)|As2]
            )
        ).
*/
ec_to_gecode_arith_exprlist1([E|L1], H, Lin, N0,N, Bs0,Bs, Auxs0,Auxs, GL, Module) :-
        ec_to_gecode_arith_expr1(E, H, Lin, N0,N1, Bs0,Bs1, Auxs0,Auxs1, GE, Module),
        ( L1 == [] ->
            GL = GE,
            N1 = N,
            Bs1 = Bs,
            Auxs1 = Auxs
        ;
            GL = GE + GL1,
            ec_to_gecode_arith_exprlist1(L1, H, Lin, N1,N, Bs1,Bs, Auxs1,Auxs, GL1, Module)
        ).


linear_op(X+Y, Out, _M) ?- !, Out = X+Y.
linear_op(X-Y, Out, _M) ?- !, Out = X-Y.
linear_op(X*Y, Out, _Module) ?- 
/*
        ( bound(X, OutX, Module) -> 
            Out = OutX*Y 
        ; bound(Y, OutY, Module)  ->
            Out = X*OutY
        ;
            fail
        ).
*/
        (integer(X) ; integer(Y)), !,
        Out = X * Y.
linear_op(-X, Out, _M) ?- !, Out = -X.


nonlin_op(max(_L), Aux, Type) ?- !,
        Aux = post_maxlist(_GMaxV, _GL),
        Type = listexpr.
nonlin_op(min(_L), Aux, Type) ?- !,
        Aux = post_minlist(_GMinV, _GL),
        Type = listexpr.
nonlin_op(sqrt(_X), Aux, Type) ?- !,
        Aux = post_sqrt(_GV, _GX),
        Type = exprargs.
nonlin_op(_X^2, Aux, Type) ?- !,
        Aux = post_sq(_GV, _GX, 2),
        Type = exprargs.
nonlin_op(sqr(_X), Aux, Type) ?- !,
        Aux = post_sq(_GV, _GX),
        Type = exprargs.
nonlin_op(abs(_X), Aux, Type) ?- !,
        Aux = post_abs(_GV, _GX),
        Type = exprargs.
nonlin_op(_X/_Y, Aux, Type) ?- !,
        Aux = post_div(_GNewV,_GNewXV,_GNewYV),
        Type = exprargs.
nonlin_op(X*Y, Aux, Type) ?- !,
        % neither X or Y are integers (linear otherwise)
        \+ integer(X), 
        \+ integer(Y),
        Aux = post_mult(_GNewV,_GNewXV,_GNewYV),
        Type = exprargs.
nonlin_op(_X mod _Y, Aux, Type) ?- !,
        Aux = post_mod(_GNewV,_GNewXV,_GNewYV),
        Type = exprargs.
nonlin_op(max(_X,_Y), Aux, Type) ?- !,
        Aux = post_max2(_GNewV,_GNewXV,_GNewYV),
        Type = exprargs.
nonlin_op(min(_X,_Y), Aux, Type) ?- !,
        Aux = post_min2(_GNewV,_GNewXV,_GNewYV),
        Type = exprargs.


ec_to_gecode_nonlin_op1(listexpr, Expr, H, N0,N, Bs0,Bs, Auxs0,AuxsT,
                          EventTemp, Module) ?- !,
        % operator has 1 arg that is a list of expressions
        arg(1, Expr, Collect),
        collection_to_list(Collect, List),
        ec_to_gecode_arith_exprlist1(List, H, 0, N0,N, Bs0,Bs, Auxs0,AuxsT, GList, Module),
        GArray =.. [[]|GList],
        arg(2, EventTemp, GArray).
ec_to_gecode_nonlin_op1(exprargs, Expr, H, N0,N, Bs0,Bs, Auxs0,AuxsT,
                          EventTemp, Module) ?- !,
        % operator has multiple arguments that are expressions
        % length(GArgList) == number of arguments for operator
        % Res in EventTemp is filled by caller
        EventTemp =.. [_Name,_Res|GArgList], 
        % fill in the arguments in event template EventTemp
        ( foreacharg(Arg, Expr),
          param(H,Module),
          foreach(GArg, GArgList),
          fromto(Auxs0, Auxs1,Auxs2, AuxsT),
          fromto(Bs0, Bs1,Bs2, Bs),
          fromto(N0, N1,N2, N)
        do
            ( var(Arg) ->
                Auxs1 = Auxs2,
                Bs1 = Bs2,
                ec_to_gecode_var1(Arg, H, N1,N2, [], _, GArg)
            ; integer(Arg) ->
                Auxs1 = Auxs2,
                Bs1 = Bs2,
                N1 = N2,
                GArg = Arg
            ; % Arg is an expression, process it and add an auxillary
              % constraint linking it to the argument variable in EventTemp
                new_gfdvar(_NewV, H, N1,N3, GArg),
                Auxs1 = [post_rc(GArg #= GExpr)|Auxs3],
                ec_to_gecode_arith_expr1(Arg, H, 0, N3,N2, Bs1,Bs2, Auxs3,
                                         Auxs2, GExpr, Module)
            )
        ).



ec_to_gecode_reified1(C, H, N0,N, Bs0,Bs, Auxs0,AuxsT, Event, GBool, Module) :-
        relation_constraint(C, RelOp, E1, E2), !,
        new_gfdvar(Bool, H, N0, N1, GBool),
        Bs1 = [Bool|Bs0],
        Event = post_bool_connectives(GBool<=>GRC),
        ec_to_gecode_reifiedrc1(RelOp, E1, E2, H, N1,N, Bs1,Bs, Auxs0,AuxsT, GRC, Module).
ec_to_gecode_reified1(C, H, N0,N, Bs0,Bs, Auxs0,AuxsT, Event, GBool, Module) :-
        domain_constraint(C, Vs, Dom),
        Auxs0 = AuxsT,
        ec_to_gecode_domain_reified1(Vs, Dom, _Bool, H, N0,N, Bs0,Bs, Event, GBool, Module).


ec_to_gecode_reifiedrc1(RelOp, E1, E2, H, N0,N, Bs0,Bs, Auxs0,AuxsT, GRC, Module) :-
        % the subexpressions must be linear here
        ec_to_gecode_arith_expr1(E1, H, 1, N0,N1, Bs0,Bs1, Auxs0,Auxs1, GE1, Module),
        ec_to_gecode_arith_expr1(E2, H, 1, N1,N, Bs1,Bs, Auxs1,AuxsT, GE2, Module),
        GRC =.. [RelOp,GE1,GE2].





ec_to_gecode_arith_expr(E, H, Auxs0,AuxsT, GE, Module) :-
        H = gfd_prob{nvars:N0,min:Min,max:Max},
        ec_to_gecode_arith_expr1(E, H, 0, N0,N, [],Bs, Auxs0,AuxsT, GE, Module),
        update_vars_for_gecode(N0, N, Bs, H, Min, Max).

ec_to_gecode_var1(V, H, N0,N, OldGVs0,OldGVs, GV) :-
        ( get_gecode_attr(V, H, Attr) ->
            % already converted to a gecode var 
            Attr = gfd{idx:I,bool:BI},
            N0 = N,
            gfdvar(I,BI, GV),
            (I =< arg(nvars of gfd_prob, H) ->
                % already existing Gecode variable
                OldGVs = [GV|OldGVs0]
            ;
                OldGVs = OldGVs0
            )
        ; 
            OldGVs = OldGVs0,
            new_gfdvar(V, H, N0,N, GV) % convert to a new gecode var
        ).

% create a new gfd variable at the ECLiPSe level. Note: variable need to
% be added to Gecode
new_gfdvar(V, H, N0, N, GV) :-
        N is N0 + 1,
        gfdvar(N, BN, GV),
        add_gecode_attr(V, H, N, BN),  % may fail!
        addto_varray(H, N, V).

ec_to_gecode_expr1(E, H, N0,N, Bs0,Bs, Auxs0,AuxsT, GE, Module) :-
        boolean_expr(E), !,
        ec_to_gecode_bool_expr1(E, H, N0,N, Bs0,Bs, Auxs0,AuxsT, GE, Module).
ec_to_gecode_expr1(E, H, N0,N, Bs0,Bs, Auxs0,AuxsT, GE, Module) :-
        ec_to_gecode_arith_expr1(E, H, 0, N0,N, Bs0,Bs, Auxs0,AuxsT, GE, Module).
                                
ec_to_gecode_bool_expr1(V, H, N0,N, Bs0,Bs, Auxs0,AuxsT, GV, _Module) :- 
        var(V), !,
        Bs = [V|Bs0],
        Auxs0 = AuxsT,
        ec_to_gecode_var1(V, H, N0,N, [],_, GV).
ec_to_gecode_bool_expr1(E, H, N0,N, Bs0,Bs, Auxs0,AuxsT, GE, Module) :- 
        connective(E, Connective, SubExprs), !,
        ( foreach(SubE, SubExprs), 
          fromto(N0, N1,N2, N),
          fromto(Bs0, Bs1,Bs2, Bs),
          fromto(Auxs0, Auxs1,Auxs2, AuxsT),
          param(H,Module),
          foreach(GSubE, GSubExprs)
        do
            ec_to_gecode_bool_expr1(SubE, H, N1,N2, Bs1,Bs2, Auxs1,Auxs2, GSubE, Module)
        ),
        GE =.. [Connective|GSubExprs].
ec_to_gecode_bool_expr1(E, H, N0,N, Bs0,Bs, Auxs0,AuxsT, GE, Module) :-
        relation_constraint(E, Op, EX, EY), !, 
        ec_to_gecode_reifiedrc1(Op, EX, EY, H, N0,N, Bs0,Bs, Auxs0,AuxsT, GE, Module).
ec_to_gecode_bool_expr1(E, H, N0,N, Bs0,Bs, Auxs0,AuxsT, GBool, Module) :-
        domain_constraint(E, Vs, Dom),
        Auxs0 = [Event|AuxsT],
        ec_to_gecode_domain_reified1(Vs, Dom, _Bool, H, N0,N, Bs0,Bs, Event, GBool, Module).


ec_to_gecode_arith_expr1(V, H, _Lin, N0,N, Bs0,Bs, Auxs,AuxsT, GV, _Module) :-
        var(V), !,
        Auxs = AuxsT,
        Bs = Bs0,
        ec_to_gecode_var1(V, H, N0,N, [],_, GV).
ec_to_gecode_arith_expr1(I, _H, _Lin, N0,N, Bs0,Bs, Auxs,AuxsT, GI, _Module) :-
        integer(I), !,
        N0 = N,
        Bs0 = Bs,
        Auxs = AuxsT,
        GI = I.
ec_to_gecode_arith_expr1(eval(E), H, Lin, N0,N, Bs0,Bs, Auxs,AuxsT, GE, Module) ?- !,
        % ic compatibility: gfd expressions are always evaluated at runtime
        ec_to_gecode_arith_expr1(E, H, Lin, N0,N, Bs0,Bs, Auxs,AuxsT, GE, Module).
ec_to_gecode_arith_expr1(subscript(T,S), H, Lin, N0,N, Bs0,Bs, Auxs,AuxsT, GE, Module) ?- !,
        subscript(T,S,E),
        ec_to_gecode_arith_expr1(E, H, Lin, N0,N, Bs0,Bs, Auxs,AuxsT, GE, Module).
ec_to_gecode_arith_expr1(sum(L0), H, Lin, N0,N, Bs0,Bs, Auxs,AuxsT, GSum, Module) ?- !,
        collection_to_list(flatten(L0),L), 
        ec_to_gecode_arith_exprlist1(L, H, Lin, N0,N, Bs0,Bs, Auxs,AuxsT, GL, Module),
        GL = GSum.
%        GArray =.. [[]|GL],
%        GSum = sum(GArray).
ec_to_gecode_arith_expr1(E0, H, Lin, N0,N, Bs0,Bs, Auxs,AuxsT, GE, Module) :-
        linear_op(E0, E, Module), !,
        functor(E, Name, Arity),
        functor(GE, Name, Arity),
        ( foreacharg(Arg, E), foreacharg(GArg, GE),
          fromto(N0, N1,N2, N), 
          fromto(Bs0, Bs1,Bs2, Bs),
          param(H,Module,Lin),
          fromto(Auxs, Auxs1,Auxs2, AuxsT)
        do
            ec_to_gecode_arith_expr1(Arg, H, Lin, N1,N2, Bs1,Bs2, Auxs1,Auxs2,  GArg, Module)
        ).
% following clauses are for non-linear expressions only
ec_to_gecode_arith_expr1(E, H, 0, N0,N, Bs0,Bs, Auxs0,AuxsT, GNewV, Module) :-
        nonlin_op(E, EventTemp, ArgType), !,
        new_gfdvar(_NewV, H, N0,N1, GNewV),
        arg(1, EventTemp, GNewV), % fill in the result variable in template
        Auxs0 = [EventTemp|Auxs1],
        ec_to_gecode_nonlin_op1(ArgType, E, H, N1,N, Bs0,Bs, Auxs1,AuxsT, EventTemp, Module).
ec_to_gecode_arith_expr1(C, H, 0, N0,N, Bs0,Bs, Auxs0,AuxsT, GB, Module) :-
        reifiable_constraint(C), !, 
        Auxs0 = [RCEvent|Auxs1],
        ec_to_gecode_reified1(C, H, N0,N, Bs0,Bs, Auxs1,AuxsT, RCEvent, GB, Module).
/*
ec_to_gecode_arith_expr1(E, H, N0,N, Auxs,AuxsT, GI, Module) :-
        try_evaluate(E), !,
        blah.


ec_to_gecode_sumlist([E|L], H, N0,N, OldGVs0,OldGVs, GL) :-
        ec_to_gecode_arith_expr1(E, H, N0,N1, OldGVs0,OldGVs1, GE1),
        ( L == [] ->
            GL = GE1,
            N = N1,
            OldGVs = OldGVs1
        ;
            GL = GE1 + GL1,
            ec_to_gecode_sumlist(L, H, N1,N, OldGVs1,OldGVs, GL1)
        ).
*/
/*
indomain(I) :- integer(I), !.
indomain(V{gfd:Attr}) ?-
        nonvar(Attr),
        Attr = gfd{prob:H,idx:Idx},
        H = gfd_prob{space:eg_space{handle:SpH}},
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

indomain(V, min) :-
        do_indomain(V, 1).

indomain(V) :-
        do_indomain(V, 1).

indomain_min(V) :-
        do_indomain(V, 1).

labeling(Vars) :-
        collection_to_list(Vars, List),
        ( foreach(Var, List) do do_indomain(Var, 1) ).


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
        H = gfd_prob{space:gfd_space{handle:SpH}},
        ( Pos > 0 ->
            ( foreach(E, List), foreach(V, VList),
              param(Pos)
            do
                arg(Pos, E, V)
            )
        ;
            VList = List
        ),
        ec_to_gecode_varlist(VList, H, GList),
        translate_method(Method, TMethod),
        GArray =.. [[]|GList],
        g_setup_search(SpH, GArray, Select, Choice, TMethod, EngH, MethodCode),
        new_space_handle(SP),
        setarg(space of gfd_prob, H, SP),
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
        setarg(events of gfd_prob, H, update).

do_search(LastSpH, EngH, MethodCode, H) :-
        mark_handle_for_ancestor_update(H),
        H = gfd_prob{space:SP},
        repeat,
        (g_do_search(SP, EngH, MethodCode, LastSpH, InstList) -> % fails if no more solution
            % a clone is created automatically by gecode's search
            % make sure an ancestor will be created on new event
%            writeln(succ:g_do_search(SP, EngH, MethodCode, LastSpH, InstList)),
            H = gfd_prob{vars:VArr},
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
%            writeln(failed:g_do_search(SP, EngH, MethodCode, LastSpH, InstList)),
            !, fail
        ).


get_min(I, Lo) :- 
        integer(I), !,
        Lo = I.
get_min(_{gfd:Attr}, Lo) ?-
        nonvar(Attr),
        Attr = gfd{prob:H, idx:Idx},
        restore_space_if_needed(H, SpH),
        g_get_var_lwb(SpH, Idx, Lo).

get_max(I, Hi) :- 
        integer(I), !,
        Hi = I.
get_max(_{gfd:Attr}, Hi) ?-
        nonvar(Attr),
        Attr = gfd{prob:H, idx:Idx},
        restore_space_if_needed(H, SpH),
        g_get_var_upb(SpH, Idx, Hi).


