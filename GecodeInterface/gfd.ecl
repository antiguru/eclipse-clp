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
:- lib(constraint_pools).
:- lib(module_options).


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
:- export (#\=)/3, (#=)/3, (#<)/3, (#>)/3, (#>=)/3, (#=<)/3.

:- export alldifferent/1, alldifferent_cst/2, 
          occurrences/3, atmost/3, count/4, element/3, element_g/3, gcc/2,
          sorted/2, sorted/3, circuit/1, circuit/3, circuit/4,
          sorted_g/3, circuit_g/1, circuit_g/3, circuit_g/4,
          disjunctive/2, disjunctive_optional/3, 
          cumulatives/5, cumulatives_min/5, cumulative/4, 
	  cumulatives_g/5, cumulatives_min_g/5, 
          sequence/5, sequence/4.
:- export plus/3, mult/3, divide/3, mod/3, divmod/4, 
          abs/2, sqr/2, sqrt/2.
:- export minlist/2, maxlist/2, sum/2, max/2, min/2, max/3, min/3.
:- export sum/3, sum/4, scalar_product/4, scalar_product/5.
:- export bool_channeling/3, inverse/2, inverse/4, inverse_g/2, inverse_g/4.
:- export ordered/2.
:- export labeling/1, indomain/1, indomain/2, delete/5.
:- export is_in_domain/2, is_in_domain/3.
:- export search/6.
:- export (and)/2, (or)/2, (xor)/2, (<=>)/2, (=>)/2, neg/1.
:- export (and)/3, (or)/3, (xor)/3, (<=>)/3, (=>)/3, neg/2.

:- export get_min/2, get_max/2.
:- export get_bounds/3, get_integer_bounds/3, get_finite_integer_bounds/3.
:- export get_domain/2, get_domain_as_list/2, get_domain_size/2, 
          get_delta/2, get_median/2.
:- export get_constraints_number/2, get_weighted_degree/2.
:- export get_regret_lwb/2, get_regret_upb/2.
:- export impose_min/2, impose_max/2, impose_bounds/3, 
          exclude/2, exclude_range/3.
:- export is_solver_type/1, is_solver_var/1, is_exact_solver_var/1, integers/1.
:- export msg/3.

:- export gfd_maxint/1, gfd_minint/1.

:- export gfd_get_default/2, gfd_set_default/2.

:- tool('#\\='/2, '#\\=_body'/3).
:- tool('#='/2, '#=_body'/3).
:- tool('#<'/2, '#<_body'/3).
:- tool('#>'/2, '#>_body'/3).
:- tool('#>='/2, '#>=_body'/3).
:- tool('#=<'/2, '#=<_body'/3).

:- tool('#\\='/3, '#\\=_reif_body'/4).
:- tool('#='/3, '#=_reif_body'/4).
:- tool('#<'/3, '#<_reif_body'/4).
:- tool('#>'/3, '#>_reif_body'/4).
:- tool('#>='/3, '#>=_reif_body'/4).
:- tool('#=<'/3, '#=<_reif_body'/4).

:- tool('#\\=_c'/3, '#\\=_c'/4).
:- tool('#=_c'/3, '#=_c'/4).
:- tool('#<_c'/3, '#<_c'/4).
:- tool('#>_c'/3, '#>_c'/4).
:- tool('#>=_c'/3, '#>=_c'/4).
:- tool('#=<_c'/3, '#=<_c'/4).

:- tool('#\\=_reif_c'/4, '#\\=_reif_c'/5).
:- tool('#=_reif_c'/4, '#=_reif_c'/5).
:- tool('#<_reif_c'/4, '#<_reif_c'/5).
:- tool('#>_reif_c'/4, '#>_reif_c'/5).
:- tool('#>=_reif_c'/4, '#>=_reif_c'/5).
:- tool('#=<_reif_c'/4, '#=<_reif_c'/5).

/*
:- tool((=\=)/2, '#\\=_body'/3).
:- tool((=:=)/2, '#=_body'/3).
:- tool((<)/2, '#<_body'/3).
:- tool((>)/2, '#>_body'/3).
:- tool((>=)/2, '#>=_body'/3).
:- tool((=<)/2, '#=<_body'/3).

:- tool((=\=)/3, '#\\=_reif_body'/4).
:- tool((=:=)/3, '#=_reif_body'/4).
:- tool((<)/3, '#<_reif_body'/4).
:- tool((>)/3, '#>_reif_body'/4).
:- tool((>=)/3, '#>=_reif_body'/4).
:- tool((=<)/3, '#=<_reif_body'/4).
*/

:- tool('::'/2, '::_body'/3).
:- tool('#::'/2, '::_body'/3).

:- tool(delete/5, delete/6).

:- local reference(prob_handle).
:- local store(stats).

:- export gfd_var_print/2.
:- export gfd_copy_var/2.

load_gfd_solver(Arch) :- 
        get_flag(object_suffix, O),
        ( Arch = "x86_64_linux" ->
            concat_string(["gfd.", O], SolverObj),
            getcwd(Current),
            cd(Arch),
            block((load(SolverObj) -> cd(Current) ; cd(Current), fail), Tag,
                  (cd(Current), exit_block(Tag)))
        ;
            concat_string([Arch,/,"gfd.", O], SolverObj),
            load(SolverObj)
        ).

:- meta_attribute(gfd, [
        set_bounds:gfd_set_var_bounds/3,  
        get_bounds:gfd_get_var_bounds/3,  
        print:gfd_var_print/2,
        unify:gfd_unify/2,
        copy_term:gfd_copy_var/2]).

:- 
        get_flag(hostarch, Arch),
        load_gfd_solver(Arch),
        external(g_init/1, p_g_init),
        external(g_state_is_stable/1, p_g_state_is_stable),
        external(g_check_handle/3, p_g_check_handle),
        external(g_trail_undo_for_event/1, p_g_trail_undo_for_event),
        external(g_delete/1, p_g_delete),
        external(g_add_newbool/3, p_g_add_newbool),
        external(g_add_newvars_interval/4, p_g_add_newvars_interval),
        external(g_add_newvars_dom/3, p_g_add_newvars_dom),
        external(g_add_newvars_dom_union/4, p_g_add_newvars_dom_union),
        external(g_add_newvar_copy/3, p_g_add_newvar_copy),
        external(g_post_interval/5, p_g_post_interval),
        external(g_post_var_interval_reif/6, p_g_post_var_interval_reif),
        external(g_post_dom/4, p_g_post_dom),
        external(g_post_var_dom_reif/5, p_g_post_var_dom_reif),
        external(g_post_var_val_reif/5, p_g_post_var_val_reif),
        external(g_post_setvar/4, p_g_post_setvar),
        external(g_post_intrel_cstr/4, p_g_post_intrel_cstr),
        external(g_post_bool_connectives/4, p_g_post_bool_connectives),
        external(g_post_alldiff/4, p_g_post_alldiff),
        external(g_post_alldiff_offsets/5, p_g_post_alldiff_offsets),
        external(g_post_count/7, p_g_post_count),
        external(g_post_gcc/6, p_g_post_gcc),
        external(g_post_element/6, p_g_post_element),
        external(g_post_sorted2/5, p_g_post_sorted2),
        external(g_post_sorted/6, p_g_post_sorted),
        external(g_post_sequence/8, p_g_post_sequence),
        external(g_post_sequence_01/7, p_g_post_sequence_01),
        external(g_post_circuit/4, p_g_post_circuit),
        external(g_post_circuit_cost/7, p_g_post_circuit_cost),
        external(g_post_disj/5, p_g_post_disj),
        external(g_post_cumulatives/9, p_g_post_cumulatives),
        external(g_post_sum/6, p_g_post_sum),
        external(g_post_lin/7, p_g_post_lin),
        external(g_post_sum_reif/7, p_g_post_sum_reif),
        external(g_post_lin_reif/8, p_g_post_lin_reif),
        external(g_post_maxlist/5, p_g_post_maxlist),
        external(g_post_minlist/5, p_g_post_minlist),
        external(g_post_sqrt/5, p_g_post_sqrt),
        external(g_post_sq/5, p_g_post_sq),
        external(g_post_abs/5, p_g_post_abs),
        external(g_post_div/5, p_g_post_div),
        external(g_post_mult/6, p_g_post_mult),
        external(g_post_mod/5, p_g_post_mod),
        external(g_post_min2/6, p_g_post_min2),
        external(g_post_max2/6, p_g_post_max2),
        external(g_post_divmod/6, p_g_post_divmod),
        external(g_post_boolchannel/6, p_g_post_boolchannel),
        external(g_post_inverse/5, p_g_post_inverse),
        external(g_post_inverse_offset/7, p_g_post_inverse_offset),
        external(g_propagate/4, p_g_propagate),
        external(g_check_val_is_in_var_domain/3, p_g_check_val_is_in_var_domain),
        external(g_get_var_bounds/4, p_g_get_var_bounds),
        external(g_get_var_value/3, p_g_get_var_value),
        external(g_get_var_domain/3, p_g_get_var_domain),
        external(g_get_var_lwb/3, p_g_get_var_lwb),
        external(g_update_and_get_var_bound/5, p_g_update_and_get_var_bound),
        external(g_get_var_upb/3, p_g_get_var_upb),
        external(g_get_var_domain_size/3, p_g_get_var_domain_size),
        external(g_get_var_domain_width/3, p_g_get_var_domain_width),
        external(g_get_var_degree/3, p_g_get_var_degree),
        external(g_get_var_median/3, p_g_get_var_median),
        external(g_get_var_afc/3, p_g_get_var_afc),
        external(g_get_var_regret_lwb/3, p_g_get_var_regret_lwb),
        external(g_get_var_regret_upb/3, p_g_get_var_regret_upb),
        external(g_setup_search/10, p_g_setup_search),
        external(g_do_search/7, p_g_do_search),
        external(g_get_gfd_maxint/1, p_g_get_gfd_maxint),
        external(g_get_gfd_minint/1, p_g_get_gfd_minint),
        external(g_get_var_domain_handle/3, p_g_get_var_domain_handle),

	external(g_gecode_version/1, p_g_gecode_version),
	g_gecode_version(Version),
        printf(log_output, "Loaded Gecode solver %s%n", [Version]).

:- export struct(
        gfd_prob(
             cp_stamp,
             nvars,
             nlevels,
             nevents,
             vars,
             prop,
             last_anc,
             space,
             events   % store in reverse chronological order, i.e last first
        )
   ).

:- export portray(gfd_prob/(property(arity) of gfd_prob), gfd_handle_tr_out/2, []).
:- export gfd_handle_tr_out/2.
gfd_handle_tr_out(gfd_prob{nvars:N},gfd_prob(nvars(N))).

:- export struct( gfd_space(handle, stamp) ).

:- export struct(
        gfd(
            idx,
            bool,
            prob,
            any,
            set
        )
   ).

:- local struct(options(interval_min,
                        interval_max,
                        array_size,
                        cloning_distance)
               ).

valid_option_field(interval_min, interval_min of options).
valid_option_field(interval_max, interval_max of options).
valid_option_field(array_size,   array_size of options).
valid_option_field(cloning_distance, cloning_distance of options).

valid_option_value(interval_min, Min) :- integer(Min), Min >= gfd_minint.
valid_option_value(interval_max, Max) :- integer(Max), Max =< gfd_maxint.
valid_option_value(array_size, Size) :- integer(Size), Size > 0.
valid_option_value(cloning_distance, D) :- integer(D), D > 0.

default_options(options{interval_min: -1000000,
                        interval_max:  1000000,
                        array_size: 100,
                        cloning_distance: 2}
               ).


gfd_get_default(Option, Value) :-
        valid_option_field(Option, OptPos),
        get_options([], Defaults),
        arg(OptPos, Defaults, Value).

gfd_set_default(Option, Value) :-
        set_default_option(Option, Value),
        ( Option == cloning_distance ->
            % cloning distance is treated specially, as it is used often
            % and need to be accessed quickly
            setval(cloning_distance, Value)
        ;
            true
        ).

?- gfd_get_default(cloning_distance, Dist), 
   (local variable(cloning_distance, Dist)).

gfd_default_interval(Min, Max) :-
        valid_option_field(interval_min, MinPos),
        valid_option_field(interval_max, MaxPos),
        get_options([], Defaults),
        arg(MinPos, Defaults, Min),
        arg(MaxPos, Defaults, Max).

boolean_expr(E) :-
        reifiable_constraint(E), !.
boolean_expr(E) :-
        connective(E, _, _).


reifiable_constraint(sum(_Vs,_Rel,_S)) ?- !.
reifiable_constraint(scalar_product(_Cs,_Vs,_Rel,_P)) ?- !.
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


inline_op(X+Y, Out) ?- !, Out = X+Y.
inline_op(X-Y, Out) ?- !, Out = X-Y.
inline_op(X*Y, Out) ?- !, Out = X * Y.
inline_op(-X, Out) ?- !, Out = -X.
inline_op(sqrt(X), Out) ?- !, Out = sqrt(X).
inline_op(X^2, Out) ?- !, Out = sqr(X).
inline_op(sqr(X), Out) ?- !, Out = sqr(X).
inline_op(abs(X), Out) ?- !, Out = abs(X).
inline_op(X/Y, Out) ?- !, Out = X/Y.
inline_op(X//Y, Out) ?- !, Out = X/Y.
inline_op(X mod Y, Out) ?- !, Out = X mod Y.
inline_op(max(X,Y), Out) ?- !, Out = max(X,Y).
inline_op(min(X,Y), Out) ?- !, Out = min(X,Y).
/* inline_op(element(Index, Collection, Out) -- not supported due to
   indexing differences
*/

/* aux_op are 'operators' that cannot be inlined in a Gecode expression.
   They are 'functional constraints' -- constraints written in a functional
   notation in an expression, without the last argument of the constraint,
   which is the value of the function. Only constraints that have a last
   argument as a domain variable can be functional constraint.
*/
aux_op(divmod(_X,_Y,_Q), Aux, _Res, GRes, Type, ConLev) ?- !,
        Aux = post_divmod(ConLev,_GX,_GY,_GQ,GRes),
        Type = args([var(1,2),var(2,3),var(3,4)]).
aux_op(moddiv(_X,_Y,_M), Aux, _Q, GQ, Type, ConLev) ?- !,
        Aux = post_divmod(ConLev,_GX,_GY,GQ,_GRes),
        Type = args([var(1,2),var(2,3),var(3,5)]).
aux_op(sum(_Vs,_Rel), Aux, S, _GS, Type, _ConLev) ?- !,
	Aux = linsum,
	Type = aux_cstr(S).
aux_op(scalar_product(_Cs,_Vs,_Rel), Aux, S, _GS, Type, _ConLev) ?- !,
	Aux = scalar_product,
	Type = aux_cstr(S).
aux_op(occurrences(_Value,_Vs), Aux, N, _GN, Type, _ConLev) ?- !,
	Aux = occurrences,
	Type = aux_cstr(N).
aux_op(count(_Value,_Vs,_Rel), Aux, N, _GN, Type, _ConLev) ?- !,
	Aux = count,
	Type = aux_cstr(N).
aux_op(element(_Idx,_Vs), Aux, Val, _GVal, Type, _ConLev) ?- !,
	Aux = element,
        Type = aux_cstr(Val).
aux_op(element_g(_Idx,_Vs), Aux, Val, _GVal, Type, _ConLev) ?- !,
	Aux = element_g,
        Type = aux_cstr(Val).


%------------------------------------------------------------------------
% Support

% low level representation of variable
gfdvar(I,B,'_ivar'(I,B)).

% get the low-level representation of an existing gecide variable
get_gecode_var(_{gfd:Attr}, GV) ?-
        nonvar(Attr),
        Attr = gfd{idx:I,bool:B},
        gfdvar(I, B, GV).


gfd_copy_var(_X{gfd:AttrX}, Copy) ?-
        ( var(AttrX) ->
            true
        ;
            AttrX = gfd{prob:H,idx:XIdx},
            restore_space_if_needed(H, _),
            H = gfd_prob{nvars:N0},
            new_gfdvar(Copy0, H, N0,N, _GCopy0),
            setarg(nvars of gfd_prob, H, N),
            post_new_event(copyvar(N,XIdx), H),
            Copy0 = Copy
        ).


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
%        Attr = gfd{prob:H,idx:I},
        Attr = gfd{prob:H,set:S,idx:I},
%        post_new_event(setvar(I, X), H).
%        check_and_update_handle(H),
        schedule_suspensions(any of gfd, Attr),
        ( S == [] -> 
            true 
        ; 
            post_new_event_no_wake(setvar(I, X), H)
        ).
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
        AttrX = gfd{idx:IdxX,bool:BX,prob:H},
        AttrY = gfd{idx:IdxY,bool:BY,prob:H},
        % the variables must belong to the same problem, else fail
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
            merge_suspension_lists(any of gfd, AttrY, any of gfd, AttrX),
            % post an equality constraint for the two variables to gecode
            gfdvar(IdxX,BX,GX),
            gfdvar(IdxY,BY,GY),
            post_new_event_no_wake(post_rc(default,GX #= GY), H)
        ).

gfd_set_var_bounds(_{gfd:Attr}, Lo0, Hi0) ?-
        nonvar(Attr),
        Lo is fix(Lo0),
        Hi is fix(Hi0),
        Attr = gfd{prob:H, idx:I, bool:BI},
        gfdvar(I,BI,GV),
        post_new_event_no_wake(post_interval([](GV), Lo, Hi), H).

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
        NewSize is OldSize*2,
        dim(New, [NewSize]),
        ( foreacharg(A, Old, Idx), param(New) do
            arg(Idx, New, A)
        ).

create_and_add_default_gfdvar(V, H) :-
        H = gfd_prob{nvars:N0},
        gfd_default_interval(Min, Max),
        new_gfdvar(V, H, N0,N1, _),
        update_newvars_with_domain_interval(H, N1, Min, Max).

is_not_boolvar(_{gfd:Attr}) ?-
        nonvar(Attr), !,
        Attr = gfd{bool:Link},
        var(Link).
is_not_boolvar(_).

link_var_to_boolvar(V, H) :-
        ( (var(V), is_not_boolvar(V)) ->
            V :: [0,1],
	    get_gecode_attr(V, H, Attr),
            Attr = gfd{idx:Idx,bool:BIdx},
            post_new_event(newbool(Idx,BIdx), H)
        ;
            true
        ).
        
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

% utility predicate for connecting indices/positions in ECLiPSe tradition
% (which starts from 1) to that used by Gecode (which starts from 0)
connect_ecl_to_gecode_indices(ConLev, EV, GV) :-
        ( var(EV) -> 
            '#=_c'(GV, (EV - 1), ConLev)
        ; integer(EV) ->
            GV is EV -1
        ;
            fail
        ).

:- mode convert_index_vars(+, +, +, -).
convert_index_vars(ecl, ConLev, SList, SList1) ?-
        ( foreach(V,SList), foreach(V1,SList1),
	  param(ConLev) do
            connect_ecl_to_gecode_indices(ConLev,V,V1)
        ).
convert_index_vars(gc, _, SList, SList1) ?-
        SList = SList1.


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
        init_suspension_list(any of gfd, Attr),
        add_attribute(X, Attr, gfd).


new_prob_handle(H) :-
        gfd_get_default(array_size, VSz),
        dim(VArr, [VSz]),
        H = gfd_prob{nvars:0,last_anc:[],space:Sp,events:[],vars:VArr,
                 nlevels:0,nevents:0, prop:Susp}, 
        timestamp_update(H, cp_stamp of gfd_prob),
        make_suspension(gfd_do_propagate(H), 10, Susp),
        new_space_handle(Sp).

new_space_handle(Sp) :-
        Sp = gfd_space{handle:SH},
        timestamp_init(Sp, stamp of gfd_space),
        g_init(SH).


% create a new gfd variable at the ECLiPSe level. Note: variable need to
% be added to Gecode
new_gfdvar(V, H, N0, N, GV) :-
        N is N0 + 1,
        gfdvar(N, BN, GV),
        add_gecode_attr(V, H, N, BN),  % may fail!
        addto_varray(H, N, V).


%------------------------------------------------------------------------
% Expression support

post_connectives(Conn, ConLev, Module) :-
        get_prob_handle(H),
        H = gfd_prob{nvars:N0},
        gfd_default_interval(Min, Max),
        ec_to_gecode_bool_expr1(Conn, H, N0,N, [],Bs, Auxs0,AuxsT, GConn, ConLev, Module),
%        ec_to_gecode_connectives1(Conn, H, N0,N, [],Bs, Auxs0,AuxsT, GConn, Module),
        update_vars_for_gecode(N0, N, Bs, H, Min, Max),
        post_new_event_with_aux([post_bool_connectives(ConLev,GConn)|Auxs0],AuxsT, H).


:- tool((and)/2, and_body/3).
:- tool((or)/2, or_body/3).
:- tool((xor)/2, xor_body/3).
:- tool(neg/1, neg_body/2).
:- tool('<=>'/2, '<=>_body'/3).
:- tool('=>'/2, '=>_body'/3).

:- tool((and)/3, and_reif_body/4).
:- tool((or)/3, or_reif_body/4).
:- tool((xor)/3, xor_reif_body/4).
:- tool(neg/2, neg_reif_body/3).
:- tool('<=>'/3, '<=>_reif_body'/4).
:- tool('=>'/3, '=>_reif_body'/4).

:- tool(and_c/3, and_c/4).
:- tool(or_c/3, or_c/4).
:- tool(xor_c/3, xor_c/4).
:- tool(neg_c/2, neg_c/3).
:- tool('<=>_c'/3, '<=>_c'/4).
:- tool('=>_c'/3, '=>_c'/4).

:- tool(and_reif_c/4, and_reif_c/5).
:- tool(or_reif_c/4, or_reif_c/5).
:- tool(xor_reif_c/4, xor_reif_c/5).
:- tool(neg_reif_c/3, neg_reif_c/4).
:- tool('<=>_reif_c'/4, '<=>_reif_c'/5).
:- tool('=>_reif_c'/4, '=>_reif_c'/5).

and_body(EX, EY, Module) :-
        and_c(EX, EY, default, Module).

and_c(EX, EY, ConLev, Module) :-
        post_connectives((EX and EY), ConLev, Module).

or_body(EX, EY, Module) :-
        or_c(EX, EY, default, Module).

or_c(EX, EY, ConLev, Module) :-
        post_connectives((EX or EY), ConLev, Module).

xor_body(EX, EY, Module) :-
        xor_c(EX, EY, default, Module).

xor_c(EX, EY, ConLev, Module) :-
        post_connectives((EX xor EY), ConLev, Module).

'<=>_body'(EX, EY, Module) :-
        '<=>_c'(EX, EY, default, Module).

'<=>_c'(EX, EY, ConLev, Module) :-
        post_connectives((EX <=> EY), ConLev, Module).

'=>_body'(EX, EY, Module) :-
        '=>_c'(EX, EY, default, Module).

'=>_c'(EX, EY, ConLev, Module) :-
        post_connectives((EX => EY), ConLev, Module).

neg_body(EX, Module) :-
        neg_c(EX, default, Module).

neg_c(EX, ConLev, Module) :-
        post_connectives(neg(EX), ConLev, Module).


and_reif_body(EX, EY, Bool, Module) :-
        and_reif_c(EX, EY, Bool, default, Module).

and_reif_c(EX, EY, Bool, ConLev, Module) :-
        post_connectives((Bool <=> (EX and EY)), ConLev, Module).

or_reif_body(EX, EY, Bool, Module) :-
        or_reif_c(EX, EY, Bool, default, Module).

or_reif_c(EX, EY, Bool, ConLev, Module) :-
        post_connectives((Bool <=> (EX or EY)), ConLev, Module).

xor_reif_body(EX, EY, Bool, Module) :-
        xor_reif_c(EX, EY, Bool, default, Module).

xor_reif_c(EX, EY, Bool, ConLev, Module) :-
        post_connectives((Bool <=> (EX xor EY)), ConLev, Module).

'<=>_reif_body'(EX, EY, Bool, Module) :-
        '<=>_reif_c'(EX, EY, Bool, default, Module).

'<=>_reif_c'(EX, EY, Bool, ConLev, Module) :-
        post_connectives((Bool <=> (EX <=> EY)), ConLev, Module).

'=>_reif_body'(EX, EY, Bool, Module) :-
        '=>_reif_c'(EX, EY, Bool, default, Module).

'=>_reif_c'(EX, EY, Bool, ConLev, Module) :-
        post_connectives((Bool <=> (EX => EY)), ConLev, Module).

neg_reif_body(EX, Bool, Module) :-
        neg_reif_c(EX, Bool, default, Module).

neg_reif_c(EX, Bool, ConLev, Module) :-
        post_connectives((Bool <=> neg(EX)), ConLev, Module).


'#\\=_body'(EX, EY, Module) :-
        '#\\=_c'(EX, EY, default, Module).

'#\\=_c'(EX, EY, ConLev, Module) :-
        post_rel_cons((#\=), EX, EY, ConLev, Module).

'#=_body'(EX, EY, Module) :-
        '#=_c'(EX, EY, default, Module).

'#=_c'(EX, EY, ConLev, Module) :-
        % optimisation for top-level aux. expressions
        ( (aux_op(EX, EXTemp, Res, GRes, EXType, ConLev),
	   aux_op(EY, EYTemp, Res, GRes, EYType,ConLev)) ->
            % EX and EY both aux 
	    get_prob_handle(H),
	    H = gfd_prob{nvars:N0},
	    gfd_default_interval(Min, Max),
	    new_gfdvar(Res, H, N0,N1, GRes),
	    ec_to_gecode_aux_op1(EXType, EX, H, N1,N2, [],Bs1, Auxs0,Auxs1, EXTemp, ConLev, Module),
	    ec_to_gecode_aux_op1(EYType, EY, H, N2,N3, Bs1,Bs, Auxs1,AuxsT, EYTemp, ConLev, Module),
	    update_vars_for_gecode(N0, N3, Bs, H, Min, Max),
            post_new_event_with_aux(Auxs0,AuxsT, H)

        ;
            post_rel_cons((#=), EX, EY, ConLev, Module)
        ).

'#<_body'(EX, EY, Module) :-
        '#<_c'(EX, EY, default, Module).

'#<_c'(EX, EY, ConLev, Module) :-
        post_rel_cons((#<), EX, EY, ConLev, Module).

'#>_body'(EX, EY, Module) :-
        '#>_c'(EX, EY, default, Module).

'#>_c'(EX, EY, ConLev, Module) :-
        post_rel_cons((#>), EX, EY, ConLev, Module).

'#>=_body'(EX, EY, Module) :-
        '#>=_c'(EX, EY, default, Module).

'#>=_c'(EX, EY, ConLev, Module) :-
        post_rel_cons((#>=), EX, EY, ConLev, Module).

'#=<_body'(EX, EY, Module) :-
        '#=<_c'(EX, EY, default, Module).

'#=<_c'(EX, EY, ConLev, Module) :-
        post_rel_cons((#=<), EX, EY, ConLev, Module).

'#\\=_reif_body'(EX, EY, Bool, Module) :-
        '<=>_c'((EX #\= EY), Bool, default, Module).

'#=_reif_body'(EX, EY, Bool, Module) :-
        '<=>_c'((EX #= EY), Bool, default, Module).

'#<_reif_body'(EX, EY, Bool, Module) :-
        '<=>_c'((EX #< EY), Bool, default, Module).

'#>_reif_body'(EX, EY, Bool, Module) :-
        '<=>_c'((EX #> EY), Bool, default, Module).

'#>=_reif_body'(EX, EY, Bool, Module) :-
        '<=>_c'((EX #>= EY), Bool, default, Module).

'#=<_reif_body'(EX, EY, Bool, Module) :-
        '<=>_c'((EX #=< EY), Bool, default, Module).

'#\\=_reif_c'(EX, EY, Bool, ConLev, Module) :-
        '<=>_c'((EX #\= EY), Bool, ConLev, Module).

'#=_reif_c'(EX, EY, Bool, ConLev, Module) :-
        '<=>_c'((EX #= EY), Bool, ConLev, Module).

'#<_reif_c'(EX, EY, Bool, ConLev, Module) :-
        '<=>_c'((EX #< EY), Bool, ConLev, Module).

'#>_reif_c'(EX, EY, Bool, ConLev, Module) :-
        '<=>_c'((EX #> EY), Bool, ConLev, Module).

'#>=_reif_c'(EX, EY, Bool, ConLev, Module) :-
        '<=>_c'((EX #>= EY), Bool, ConLev, Module).

'#=<_reif_c'(EX, EY, Bool, ConLev, Module) :-
        '<=>_c'((EX #=< EY), Bool, ConLev, Module).




post_rel_cons(RelOp, EX, EY, ConLev, Module) :-
        get_prob_handle(H),
        H = gfd_prob{nvars:N0},
        gfd_default_interval(Min, Max),
        ec_to_gecode_expr1(EX, H, N0,N1, [],Bs1, Auxs0,Auxs1, GEX, ConLev, Module),
        ec_to_gecode_expr1(EY, H, N1,N2, Bs1,Bs2, Auxs1,Auxs2, GEY, ConLev, Module),
        construct_relcons_event1(RelOp, EX, EY, GEX, GEY, H, N2,N, Bs2,Bs,
                                 Auxs2,AuxsT, Event, ConLev), 
        update_vars_for_gecode(N0, N, Bs, H, Min, Max),
        post_new_event_with_aux([Event|Auxs0],AuxsT, H).

construct_relcons_event1((#=), EX, EY, GEX, GEY, H, N0,N, Bs0,Bs, Auxs0,AuxsT, Event, ConLev) ?-
        !,
        ( boolean_expr(EX) ->
            ( boolean_expr(EY) ->
                Bs = Bs0,
                Event = post_bool_connectives(ConLev,GEX<=>GEY),
                Auxs0 = AuxsT
            ; var(EY) ->
                Bs = [EY|Bs0],
                N = N0,
                Auxs0 = AuxsT,
                Event = post_bool_connectives(ConLev,GEY<=>GEX)
            ; integer(EY) ->
                Bs = Bs0,
                N = N0,
                Auxs0 = AuxsT,
                ( EY == 1 ->
                    Event = post_bool_connectives(ConLev,GEX)
                ; EY == 0 ->
                    % let Gecode do the optimisation if EY == 0
                    Event = post_bool_connectives(ConLev,GEY<=>GEX)
                ;
                    abort
                )
            ; 
                new_gfdvar(BVar, H, N0,N, GBVar),
                Bs = [BVar|Bs0],
                Event = post_bool_connectives(ConLev,GBVar<=>GEX),
                Auxs0 = [post_rc(ConLev,GBVar #= GEY)|AuxsT]
            )
        ; boolean_expr(EY) ->
            ( var(EX) ->
                Bs = [EX|Bs0],
                N = N0,
                Auxs0 = AuxsT,
                Event = post_bool_connectives(ConLev,GEX<=>GEY)
            ; integer(EX) ->
                Bs = Bs0,
                N = N0,
                Auxs0 = AuxsT,
                ( EX == 1 ->
                    Event = post_bool_connectives(ConLev,GEY)
                ; EX == 0 ->
                    Event = post_bool_connectives(ConLev,GEX<=>GEY)
                ;
                    abort
                )
            ;
                new_gfdvar(BVar, H, N0,N, GBVar),
                Bs = [BVar|Bs0],
                Event = post_bool_connectives(ConLev,GBVar<=>GEY),
                Auxs0 = [post_rc(ConLev,GBVar #= GEX)|AuxsT]
            )
        ;
            Bs = Bs0,
            N = N0,
            Auxs0 = AuxsT,
            Event = post_rc(ConLev,GEX #= GEY)
        ).
construct_relcons_event1(RelOp, EX, EY, GEX, GEY, H, N0,N, Bs0,Bs, Auxs0,AuxsT, Event, ConLev) ?-
        ( boolean_expr(EX) ->
            new_gfdvar(BXV, H, N0,N1, GBXV),
            Bs1 = [BXV|Bs0],
            Auxs0 = [post_bool_connectives(ConLev,GBXV<=>GEX)|Auxs1],
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
            Auxs1 = [post_bool_connectives(ConLev,GBYV<=>GEY)|AuxsT],
            GNewY = GBYV
        ;
            N = N1,
            Bs = Bs1,
            Auxs1 = AuxsT,
            GNewY = GEY
        ),
        RC =.. [RelOp, GNewX, GNewY],
        Event = post_rc(ConLev,RC).


ec_to_gecode_var(V, H, GV) :-
        H = gfd_prob{nvars:N0},
        gfd_default_interval(Min, Max),
        ec_to_gecode_var1(V, H, N0,N, [],_, GV),
        ( N > N0 ->
            % V is a new gecode var, add it to gecode
            update_newvars_with_domain_interval(H, N, Min, Max)
        ;
            true
        ).

ec_to_gecode_varlist(L, H, GL) :-
        H = gfd_prob{nvars:N0},
        gfd_default_interval(Min, Max),
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

ec_to_gecode_arith_exprlist1(List, H, Inline, N0,N, Bs0,Bs, Auxs0,Auxs, GList, ConLev, Module) :-
        ( foreach(E,List), fromto(N0, N1,N2, N), 
          fromto(Auxs0, As1,As2, Auxs), fromto(Bs0, Bs1,Bs2, Bs),
          param(H,Module,Inline,ConLev),
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
	    ; E = subscript(T,S) ->
	        subscript(T, S, V),
		As2 = As1,
		Bs2 = Bs1,
		ec_to_gecode_var1(V, H, N1,N2, [],_, GE)
            ; 
	        Inline == 0, % only parse for subexpressions if non-inline allowed
                new_gfdvar(_NewV, H, N1,N3, GE),
                ec_to_gecode_arith_expr1(E, H, Inline, N3,N2, Bs1,Bs2, As1,As3, GExpr, ConLev, Module),
                As3 = [post_rc(ConLev,GE #= GExpr)|As2]
            )
        ).



ec_to_gecode_aux_op1(args(Specs), AuxOp, H, N0,N, Bs0,Bs, Auxs0,AuxsT, EventTemp, ConLev, Module) ?- !,
        % operator has multiple arguments of different types, with the ones
        % that can be expressions specified in the list Specs 
        Auxs0 = [EventTemp|Auxs1],
        ( foreach(Spec, Specs),
	  fromto(N0, N1,N2, N), 
	  fromto(Bs0, Bs1,Bs2, Bs), 
	  fromto(Auxs1, Auxs2,Auxs3, AuxsT),
	  param([EventTemp,AuxOp,H,ConLev,Module])
	do 
            ( Spec = list(AuxArg,EvArg) ->
                arg(AuxArg, AuxOp, Collect),
                collection_to_list(Collect, List),
                ec_to_gecode_arith_exprlist1(List, H, 0, N1,N2, Bs1,Bs2, Auxs2,Auxs3, GList, ConLev, Module),
                GArray =.. [[]|GList],
                arg(EvArg, EventTemp, GArray)
            ; Spec = var(AuxArg,EvArg) ->
                arg(AuxArg, AuxOp, Arg),
                arg(EvArg, EventTemp, GArg),
                ( var(Arg) ->
                    Auxs3 = Auxs2,
                    Bs2 = Bs1,
                    ec_to_gecode_var1(Arg, H, N1,N2, [], _, GArg)
                ; integer(Arg) ->
                    Auxs3 = Auxs2,
                    Bs1 = Bs2,
                    N1 = N2,
                    GArg = Arg
                ; % Arg is an expression, process it and add an auxillary
                  % constraint linking it to the argument variable in EventTemp
                    new_gfdvar(_NewV, H, N1,N3, GArg),
                    Auxs2 = [post_rc(ConLev, GArg #= GExpr)|Auxs4],
                    ec_to_gecode_arith_expr1(Arg, H, 0, N3,N2, Bs1,Bs2,
                                             Auxs4,Auxs3, GExpr, ConLev, Module)
                )
            ;
                fail
            )
	     
        ).
ec_to_gecode_aux_op1(aux_cstr(Val), Cstr, H, N0,N, Bs0,Bs, Auxs0,AuxsT, EventTemp, ConLev, _Module) ?- !,
        cstr_aux_events1(EventTemp, Cstr, Val, H, N0,N, Bs0,Bs, Auxs0,AuxsT, ConLev).


cstr_aux_events1(linsum, sum(Vs,Rel), Sum, H, N0,N, Bs0,Bs, Auxs0, AuxsT, ConLev) :-
        Bs0 = Bs,
        linsum_event1(Vs, Rel, Sum, ConLev, H, N0,N, Auxs0,AuxsT).
cstr_aux_events1(scalar_product, scalar_product(Cs,Vs,Rel), Prod, H, N0,N, Bs0,Bs, Auxs0, AuxsT, ConLev) :-
        Bs0 = Bs,
        scalar_product_event1(Cs, Vs, Rel, Prod, ConLev, H, N0,N, Auxs0,AuxsT).
cstr_aux_events1(element, element(Idx,Vs), Val, H, N0,N, Bs0,Bs, Auxs0,AuxsT, ConLev) :-
	Bs0 = Bs,
        element_events1(Idx, Vs, Val, ecl, ConLev, H, N0,N, Auxs0,AuxsT).
cstr_aux_events1(element_g, element_g(Idx,Vs), Val, H, N0,N, Bs0,Bs, Auxs0,AuxsT, ConLev) :-
	Bs0 = Bs,
        element_events1(Idx, Vs, Val, gc, ConLev, H, N0,N, Auxs0,AuxsT).
cstr_aux_events1(occurrences, occurrences(Value,Vs), Count, H, N0,N, Bs0,Bs, Auxs0,AuxsT, ConLev) :-
	Bs0 = Bs,
        count_events1(Value, Vs, '#=', Count, ConLev, H, N0,N, Auxs0,AuxsT).
cstr_aux_events1(count, count(Value,Vs,Rel), Val, H, N0,N, Bs0,Bs, Auxs0,AuxsT, ConLev) :-
	Bs0 = Bs,
        count_events1(Value, Vs, Rel, Val, ConLev, H, N0,N, Auxs0,AuxsT).


ec_to_gecode_reified1(C, H, N0,N, Bs0,Bs, Auxs0,AuxsT, Event, GBool, ConLev, Module) :-
        relation_constraint(C, RelOp, E1, E2), !,
        new_gfdvar(Bool, H, N0, N1, GBool),
        Bs1 = [Bool|Bs0],
        Event = post_bool_connectives(ConLev,GBool<=>GRC),
        ec_to_gecode_reifiedrc1(RelOp, E1, E2, H, N1,N, Bs1,Bs, Auxs0,AuxsT, GRC, ConLev, Module).
ec_to_gecode_reified1(C, H, N0,N, Bs0,Bs, Auxs0,AuxsT, Event, GBool, _ConLev, Module) :-
        domain_constraint(C, Vs, Dom), !,
        Auxs0 = AuxsT,
        ec_to_gecode_domain_reified1(Vs, Dom, _Bool, H, N0,N, Bs0,Bs, Event, GBool, Module).
ec_to_gecode_reified1(C, H, N0,N, Bs0,Bs, Auxs0,AuxsT, Event, GBool, ConLev, _Module) :-
        C = scalar_product(Cs,Vs,Rel,S), !,
        Auxs0 = AuxsT,
        scalar_product_reif_event1(Cs,Vs, Rel, S, _Bool, ConLev, H, N0,N, Bs0,Bs, Event, GBool).
ec_to_gecode_reified1(C, H, N0,N, Bs0,Bs, Auxs0,AuxsT, Event, GBool, ConLev, _Module) :-
        C = sum(Vs,Rel,S), !,
        Auxs0 = AuxsT,
        linsum_reif_event1(Vs, Rel, S, _Bool, ConLev, H, N0,N, Bs0,Bs, Event, GBool).


ec_to_gecode_reifiedrc1(RelOp, E1, E2, H, N0,N, Bs0,Bs, Auxs0,AuxsT, GRC, ConLev, Module) :-
        % the subexpressions must be inline here
        ec_to_gecode_arith_expr1(E1, H, 1, N0,N1, Bs0,Bs1, Auxs0,Auxs1, GE1, ConLev, Module),
        ec_to_gecode_arith_expr1(E2, H, 1, N1,N, Bs1,Bs, Auxs1,AuxsT, GE2, ConLev, Module),
        GRC =.. [RelOp,GE1,GE2].


ec_to_gecode_arith_expr(E, H, Auxs0,AuxsT, GE, ConLev, Module) :-
        H = gfd_prob{nvars:N0},
        gfd_default_interval(Min, Max),
        ec_to_gecode_arith_expr1(E, H, 0, N0,N, [],Bs, Auxs0,AuxsT, GE, ConLev, Module),
        update_vars_for_gecode(N0, N, Bs, H, Min, Max).

% convert existing gfd variable (or integer) to gecode representation
ec_to_gecode_oldvar(V, H, GV) :-
        ( integer(V) ->
            GV = V
        ; get_gecode_attr(V, H, gfd{idx:I,bool:B}) ->
            gfdvar(I, B, GV)
        ;
            fail
        ).


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

ec_to_gecode_expr1(E, H, N0,N, Bs0,Bs, Auxs0,AuxsT, GE, ConLev, Module) :-
        boolean_expr(E), !,
        ec_to_gecode_bool_expr1(E, H, N0,N, Bs0,Bs, Auxs0,AuxsT, GE, ConLev, Module).
ec_to_gecode_expr1(E, H, N0,N, Bs0,Bs, Auxs0,AuxsT, GE, ConLev, Module) :-
        ec_to_gecode_arith_expr1(E, H, 0, N0,N, Bs0,Bs, Auxs0,AuxsT, GE, ConLev, Module).
                                
ec_to_gecode_bool_expr1(V, H, N0,N, Bs0,Bs, Auxs0,AuxsT, GV, _ConLev, _Module) :- 
        var(V), !,
        Bs = [V|Bs0],
        Auxs0 = AuxsT,
        ec_to_gecode_var1(V, H, N0,N, [],_, GV).
ec_to_gecode_bool_expr1(E, H, N0,N, Bs0,Bs, Auxs0,AuxsT, GE, ConLev, Module) :- 
        connective(E, Connective, SubExprs), !,
        ( foreach(SubE, SubExprs), 
          fromto(N0, N1,N2, N),
          fromto(Bs0, Bs1,Bs2, Bs),
          fromto(Auxs0, Auxs1,Auxs2, AuxsT),
          param(H,Module,ConLev),
          foreach(GSubE, GSubExprs)
        do
            ec_to_gecode_bool_expr1(SubE, H, N1,N2, Bs1,Bs2, Auxs1,Auxs2, GSubE, ConLev, Module)
        ),
        GE =.. [Connective|GSubExprs].
ec_to_gecode_bool_expr1(1, _H, N0,N, Bs0,Bs, Auxs0,AuxsT, GE, _ConLev, _Module) :-
        N0 = N,
        Bs0 = Bs,
        Auxs0 = AuxsT,
        GE = 1.
ec_to_gecode_bool_expr1(0, _H, N0,N, Bs0,Bs, Auxs0,AuxsT, GE, _ConLev, _Module) :-
        N0 = N,
        Bs0 = Bs,
        Auxs0 = AuxsT,
        GE = 0.
ec_to_gecode_bool_expr1(E, H, N0,N, Bs0,Bs, Auxs0,AuxsT, GE, ConLev, Module) :-
        % rel. constraints can be inlined in bool. expr,
        relation_constraint(E, Op, EX, EY), !, 
        ec_to_gecode_reifiedrc1(Op, EX, EY, H, N0,N, Bs0,Bs, Auxs0,AuxsT, GE, ConLev, Module).
ec_to_gecode_bool_expr1(C, H, N0,N, Bs0,Bs, Auxs0,AuxsT, GB, ConLev, Module) :-
        reifiable_constraint(C), !, 
        Auxs0 = [RCEvent|Auxs1],
        ec_to_gecode_reified1(C, H, N0,N, Bs0,Bs, Auxs1,AuxsT, RCEvent, GB, ConLev, Module).


ec_to_gecode_arith_expr1(V, H, _Lin, N0,N, Bs0,Bs, Auxs,AuxsT, GV, _ConLev, _Module) :-
        var(V), !,
        Auxs = AuxsT,
        Bs = Bs0,
        ec_to_gecode_var1(V, H, N0,N, [],_, GV).
ec_to_gecode_arith_expr1(I, _H, _Lin, N0,N, Bs0,Bs, Auxs,AuxsT, GI, _ConLev, _Module) :-
        integer(I), !,
        N0 = N,
        Bs0 = Bs,
        Auxs = AuxsT,
        GI = I.
ec_to_gecode_arith_expr1(eval(E), H, Lin, N0,N, Bs0,Bs, Auxs,AuxsT, GE, ConLev, Module) ?- !,
        % ic compatibility: gfd expressions are always evaluated at runtime
        ec_to_gecode_arith_expr1(E, H, Lin, N0,N, Bs0,Bs, Auxs,AuxsT, GE, ConLev, Module).
ec_to_gecode_arith_expr1(subscript(T,S), H, Lin, N0,N, Bs0,Bs, Auxs,AuxsT, GE, ConLev, Module) ?- !,
        subscript(T,S,E),
        ec_to_gecode_arith_expr1(E, H, Lin, N0,N, Bs0,Bs, Auxs,AuxsT, GE, ConLev, Module).
ec_to_gecode_arith_expr1(sum(Cs0*L0), H, Lin, N0,N, Bs0,Bs, Auxs,AuxsT, GSP, ConLev, Module) ?- !,
        collection_to_list(flatten(L0),L), 
        collection_to_list(flatten(Cs0),Cs),
	(foreach(C, Cs) do integer(C)), 
        ec_to_gecode_arith_exprlist1(L, H, Lin, N0,N, Bs0,Bs, Auxs,AuxsT, GL, ConLev, Module),
        GArr =.. [[]|GL],
	CArr =.. [[]|Cs],
	arity(GArr) =:= arity(CArr),
        GSP = sum(CArr,GArr).
ec_to_gecode_arith_expr1(sum(L0), H, Lin, N0,N, Bs0,Bs, Auxs,AuxsT, GSum, ConLev, Module) ?- !,
        collection_to_list(flatten(L0),L),
	ec_to_gecode_arith_exprlist1(L, H, Lin, N0,N, Bs0,Bs, Auxs,AuxsT, GL, ConLev, Module),
        GArr =.. [[]|GL],
        GSum = sum(GArr).
ec_to_gecode_arith_expr1(sumlist(L0), H, Lin, N0,N, Bs0,Bs, Auxs,AuxsT, GSum, ConLev, Module) ?- !,
        ec_to_gecode_arith_expr1(sum(L0), H, Lin, N0,N, Bs0,Bs, Auxs,AuxsT, GSum, ConLev, Module).
ec_to_gecode_arith_expr1(min(L0), H, Lin, N0,N, Bs0,Bs, Auxs,AuxsT, GSum, ConLev, Module) ?- !,
        collection_to_list(flatten(L0),L), 
        ec_to_gecode_arith_exprlist1(L, H, Lin, N0,N, Bs0,Bs, Auxs,AuxsT, GL, ConLev, Module),
        GArr =.. [[]|GL],
        GSum = min(GArr).
ec_to_gecode_arith_expr1(minlist(L0), H, Lin, N0,N, Bs0,Bs, Auxs,AuxsT, GSum, ConLev, Module) ?- !,
        ec_to_gecode_arith_expr1(min(L0), H, Lin, N0,N, Bs0,Bs, Auxs,AuxsT, GSum, ConLev, Module).
ec_to_gecode_arith_expr1(max(L0), H, Lin, N0,N, Bs0,Bs, Auxs,AuxsT, GSum, ConLev, Module) ?- !,
        collection_to_list(flatten(L0),L), 
        ec_to_gecode_arith_exprlist1(L, H, Lin, N0,N, Bs0,Bs, Auxs,AuxsT, GL, ConLev, Module),
        GArr =.. [[]|GL],
        GSum = max(GArr).
ec_to_gecode_arith_expr1(maxlist(L0), H, Lin, N0,N, Bs0,Bs, Auxs,AuxsT, GSum, ConLev, Module) ?- !,
        ec_to_gecode_arith_expr1(max(L0), H, Lin, N0,N, Bs0,Bs, Auxs,AuxsT, GSum, ConLev, Module).
ec_to_gecode_arith_expr1(element_g(Idx,Vs), H, Lin, N0,N, Bs0,Bs, Auxs,AuxsT, GElm, ConLev, Module) ?- !,
        ec_to_gecode_arith_expr1(Idx, H, Lin, N0,N1, Bs0,Bs, Auxs,AuxsT, GIdx, ConLev, Module),
	collection_to_list(flatten(Vs), List),
        ec_to_gecode_varlist1(List, H, N1,N, GList),
	GArr =.. [[]|GList],
        % order of arg in Gecode is reverse of ECLiPSe 
	GElm = element(GArr, GIdx).
ec_to_gecode_arith_expr1(E0, H, Lin, N0,N, Bs0,Bs, Auxs,AuxsT, GE, ConLev, Module) :-
        inline_op(E0, E), !,
        functor(E, Name, Arity),
        functor(GE, Name, Arity),
        ( foreacharg(Arg, E), foreacharg(GArg, GE),
          fromto(N0, N1,N2, N), 
          fromto(Bs0, Bs1,Bs2, Bs),
          param(H,Module,Lin,ConLev),
          fromto(Auxs, Auxs1,Auxs2, AuxsT)
        do
            ec_to_gecode_arith_expr1(Arg, H, Lin, N1,N2, Bs1,Bs2, Auxs1,Auxs2,  GArg, ConLev, Module)
        ).
% following clauses are for expressions that needs to be factored out as aux. 
% these used to be the non-linear parts but Gecode can now handle most
% non-linear parts directly
ec_to_gecode_arith_expr1(E, H, 0, N0,N, Bs0,Bs, Auxs0,AuxsT, GNewV, ConLev, Module) :-
        aux_op(E, EventTemp, NewV, GNewV, ArgType, ConLev), !,
        new_gfdvar(NewV, H, N0,N1, GNewV),
        ec_to_gecode_aux_op1(ArgType, E, H, N1,N, Bs0,Bs, Auxs0,AuxsT, EventTemp, ConLev, Module).
ec_to_gecode_arith_expr1(C, H, 0, N0,N, Bs0,Bs, Auxs0,AuxsT, GB, ConLev, Module) :-
        reifiable_constraint(C), !, 
        Auxs0 = [RCEvent|Auxs1],
        ec_to_gecode_reified1(C, H, N0,N, Bs0,Bs, Auxs1,AuxsT, RCEvent, GB, ConLev, Module).

%------------------------------------------------------------------------
% Constraints


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
        ), !.
'::_body'(X, Domain, _Module) :-
        get_bip_error(E),
        error(E,(X :: Domain)).


:- tool('::'/3, '::_body'/4).
:- tool('#::'/3, '::_body'/4).


'::_body'(X, Domain, Bool, Module):-
        get_prob_handle(H),
        H = gfd_prob{nvars:N0},
        gfd_default_interval(Min, Max),
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
        integer(B), !.
bound(_A, _B, _M) :-
        set_bip_error(5).

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
process_domain_vars([_|_], _,_,_,_,_,_) :- !,
        set_bip_error(5).
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



alldifferent(Vars) :-
        alldifferent_c(Vars, default).


alldifferent_c(Vars, ConLev) :-
        collection_to_list(Vars, List),
        get_prob_handle(H),
        ec_to_gecode_varlist(List, H, GList),
        GArray =.. [[]|GList],
        post_new_event(post_alldiff(ConLev,GArray), H).


alldifferent_cst(Vars, Offsets) :-
        alldifferent_cst_c(Vars, Offsets, default).


alldifferent_cst_c(Vars, Offsets, ConLev) :-
        collection_to_list(Vars, List),
        collection_to_list(Offsets, OffList),
        get_prob_handle(H),
        ec_to_gecode_varlist(List, H, GList),
        ec_to_gecode_varlist(OffList, H, GOffList),
        GArray =.. [[]|GList],
        OArray =.. [[]|GOffList],
        arity(GArray, N),
        arity(OArray, N),
        post_new_event(post_alldiff_offsets(ConLev,GArray,OArray), H).


count(Value, Vars, Rel, N) :-
        count_c(Value, Vars, Rel, N, default).

count_c(Value, Vars, Rel, N, ConLev) :-
	get_prob_handle(H),
        H = gfd_prob{nvars:NV0},
	count_events1(Value, Vars, Rel, N, ConLev, H, NV0,NV, CountEvents, EventsT),
	gfd_default_interval(Min, Max),
	update_vars_for_gecode(NV0, NV, [], H, Min, Max),
        post_new_event_with_aux(CountEvents, EventsT, H).

count_events1(Value, Vars, Rel, N, ConLev, H, NV0,NV, Events, EventsT) :-
        atomic(Rel),
        rel_op(Rel),
        collection_to_list(Vars, List),
        ec_to_gecode_varlist1([Value,N|List], H, NV0,NV, [GValue,GN|GList]),
        GArray =.. [[]|GList],
	CEvent = post_count(ConLev,GValue,GArray,Rel,GN), 
        ( integer(N) ->
            N =< arity(GArray),
	    Events = [CEvent|EventsT]
        ; var(N) ->
	    Hi is arity(GArray),
	    % GN will be in Gecode by the time this event is executed
	    Events = [CEvent,post_interval([](GN), 0, Hi)|EventsT] 
%       ; fail
        ).


% compatibility
occurrences(Value, Vars, N) :-
        count_c(Value, Vars, '#=', N, default).

occurrences_c(Value, Vars, N, ConLev) :-
        count_c(Value, Vars, '#=', N, ConLev).

% compatibility
atmost(N, Vars, Value) :-
        count_c(Value, Vars, '#=<', N, default).

atmost_c(N, Vars, Value, ConLev) :-
        count_c(Value, Vars, '#=<', N, ConLev).

atleast(N, Vars, Value) :-
        count_c(Value, Vars, '#>=', N, default).

atleast_c(N, Vars, Value, ConLev) :-
        count_c(Value, Vars, '#>=', N, ConLev).

element(Index, Collection, Value) :-
        element_body(Index, Collection, Value, ecl, default).

element_c(Index, Collection, Value, ConLev) :-
        element_body(Index, Collection, Value, ecl, ConLev).

element_g(Index, Collection, Value) :-
        element_body(Index, Collection, Value, gc, default).

element_g_c(Index, Collection, Value, ConLev) :-
        element_body(Index, Collection, Value, gc, ConLev).

element_body(Index, Collection, Value, IndexType, ConLev) :-
	get_prob_handle(H),
        H = gfd_prob{nvars:NV0},
	element_events1(Index, Collection, Value, IndexType, ConLev, H, NV0,NV, Events, EventsT),
	gfd_default_interval(Min, Max),
	update_vars_for_gecode(NV0, NV, [], H, Min, Max),
        post_new_event_with_aux(Events, EventsT, H).

element_events1(Index, Collection, Value, IndexType, ConLev, H, NV0,NV, Es, EsT) :-
        collection_to_list(Collection, List),
        ec_to_gecode_varlist1([Index,Value|List], H, NV0,NV, [GIndex,GValue|GList]),
	( IndexType == ecl ->
	    Array =.. [[],0|GList],  % add a dummy first element for index 0
	    Lo = 1
	;
	    Array =.. [[]|GList],
	    Lo = 0
        ),
        Hi is arity(Array)-1,
        EEvent = post_element(ConLev, GIndex, Array, GValue),  
	( integer(Index) ->
	   Index >= Lo,
	   Index =< Hi,
	   Es = [EEvent|EsT] 
	;
	   
	   Es = [EEvent,post_interval([](GIndex), Lo, Hi)|EsT]
	).

:- export struct(gcc(low,high,value)),
          struct(occ(occ,value)).


gcc(BoundsList, Vars) :-
        gcc_c(BoundsList, Vars, default).

gcc_c(BoundsList, Vars, ConLev) :-        
        collection_to_list(Vars, VList),
        get_prob_handle(H),
        ec_to_gecode_varlist(VList, H, GVList),
        GVs =.. [[]|GVList],
        Bounds =.. [[]|BoundsList],
        arity(Bounds, M),
        dim(Vals, [M]),
        dim(Occurrences, [M]),
        ( foreacharg(Spec, Bounds), 
          foreacharg(Value, Vals),
          foreacharg(Occ, Occurrences)
        do
            translate_gcc_spec(Spec, Value, Occ)
        ),
        post_new_event(post_gcc(ConLev, Vals, Occurrences, GVs), H).

translate_gcc_spec(gcc{low:Lo,high:Hi,value:Val0}, Val, Occ) ?-
        integer(Val0),
        Val0 = Val,
        integer(Hi),
        integer(Lo),
        Occ :: Lo..Hi.
translate_gcc_spec(occ{occ:Occ0,value:Val0}, Val, Occ) ?-
        integer(Val0),
        Val0 = Val,
        is_solver_type(Occ0),
        Occ0 = Occ.

sorted(Us0, Ss0) :-
        sorted_c(Us0, Ss0, default).

sorted_c(Us0, Ss0, ConLev) :-
        ( var(Us0) -> 
            Us0 = Us
        ;
            collection_to_list(Us0, Us)
        ),
        ( var(Ss0) -> 
            nonvar(Us0),
            Ss0 = Ss
        ;
            collection_to_list(Ss0, Ss)
        ),
	( foreach(_,Us), foreach(_,Ss) do true ),
        Ss \== [],
        !,
        get_prob_handle(H),
        ec_to_gecode_varlist(Ss, H, GSs),
        SsArr =.. [[]|GSs],
        ec_to_gecode_varlist(Us, H, GUs),
        UsArr =.. [[]|GUs],
        post_new_event(post_sorted2(ConLev, UsArr, SsArr), H).

sorted(Us0, Ss0, Ps0) :-
        sorted_body(Us0, Ss0, Ps0, ecl, default).

sorted_c(Us0, Ss0, Ps0, ConLev) :-
        sorted_body(Us0, Ss0, Ps0, ecl, ConLev).

sorted_g(Us0, Ss0, Ps0) :-
        sorted_body(Us0, Ss0, Ps0, gc, default).

sorted_g_c(Us0, Ss0, Ps0, ConLev) :-
        sorted_body(Us0, Ss0, Ps0, gc, ConLev).

sorted_body(Us0, Ss0, Ps0, IndexType, ConLev) :-
        ( var(Us0) -> 
            Us0 = Us
        ;
            collection_to_list(Us0, Us)
        ),
        ( var(Ss0) -> 
            Ss0 = Ss
        ;
            collection_to_list(Ss0, Ss)
        ),
        ( var(Ps0) -> 
            Ps0 = Ps
        ;
            collection_to_list(Ps0, Ps)
        ),
	get_prob_handle(H),
        convert_sorted_lists(IndexType, H, Us,Ss,Ps, UsArr,SsArr,PsArr), 
        post_new_event(post_sorted(ConLev, UsArr, SsArr, PsArr), H).

  convert_sorted_lists(ecl, H, Us, Ss, Ps,  UsArr, SsArr, PsArr) :-
        gfd_maxint(Max),
	( foreach(U,Us), foreach(_,Ss), foreach(_,Ps),
          fromto(Max, Min0,Min1, Min)
        do 
           get_min(U, UMin),
           (UMin < Min0 -> Min1 is UMin - 1 ; Min1 = Min0)
        ),
        Ss \== [],
        ec_to_gecode_varlist(Ss, H, GSs),
        SsArr =.. [[],Min|GSs],
        ec_to_gecode_varlist(Us, H, GUs),
        UsArr =.. [[],Min|GUs],
        ec_to_gecode_varlist(Ps, H, GPs),
        PsArr =.. [[],0|GPs],
        Ps :: [1..(arity(PsArr)-1)].
  convert_sorted_lists(gc, H, Us, Ss, Ps,  UsArr, SsArr, PsArr) :-
	( foreach(_,Us), foreach(_,Ss), foreach(_,Ps) do true),
        Ss \== [],
        ec_to_gecode_varlist(Ss, H, GSs),
        SsArr =.. [[]|GSs],
        ec_to_gecode_varlist(Us, H, GUs),
        UsArr =.. [[]|GUs],
        ec_to_gecode_varlist(Ps, H, GPs),
        PsArr =.. [[]|GPs],
        Ps :: [0..(arity(PsArr)-1)].


circuit(Succ) :-
        circuit_body(Succ, ecl, default).

circuit_g(Succ) :-
        circuit_body(Succ, gc, default).

circuit_c(Succ, ConLev) :-
        circuit_body(Succ, ecl, ConLev).

circuit_g_c(Succ, ConLev) :-
        circuit_body(Succ, gc, ConLev).

circuit_body(Succ, IndexType, ConLev) :-
        collection_to_list(Succ, SList),
        SList \== [],
        length(SList, N), 
        SList :: 1..N,
        get_prob_handle(H),
        convert_index_vars(IndexType, ConLev, SList, SList1),
        ec_to_gecode_varlist(SList1, H, GSs),
        SArr =.. [[]|GSs],
        post_new_event(post_circuit(ConLev,SArr), H).

circuit(Succ, CostMatrix, Cost) :-
        circuit_body(Succ, CostMatrix, [], Cost, ecl, default).

circuit(Succ, CostMatrix, ArcCosts, Cost) :-
        circuit_body(Succ, CostMatrix, ArcCosts, Cost, ecl, default).

circuit_g(Succ, CostMatrix, Cost) :-
        circuit_body(Succ, CostMatrix, [], Cost, gc, default).

circuit_g(Succ, CostMatrix, ArcCosts, Cost) :-
        circuit_body(Succ, CostMatrix, ArcCosts, Cost, gc, default).


circuit_c(Succ, CostMatrix, Cost, ConLev) :-
        circuit_body(Succ, CostMatrix, [], Cost, ecl, ConLev).

circuit_c(Succ, CostMatrix, ArcCosts, Cost, ConLev) :-
        circuit_body(Succ, CostMatrix, ArcCosts, Cost, ecl, ConLev).

circuit_g_c(Succ, CostMatrix, Cost, ConLev) :-
        circuit_body(Succ, CostMatrix, [], Cost, gc, ConLev).

circuit_g_c(Succ, CostMatrix, ArcCosts, Cost, ConLev) :-
        circuit_body(Succ, CostMatrix, ArcCosts, Cost, gc, ConLev).

circuit_body(Succ, CostMatrix, ArcCosts, Cost, IndexType, ConLev) :-
        collection_to_list(CostMatrix, CMList),
        CMArr =.. [[]|CMList],
        collection_to_list(Succ, SList),
        SList \== [],
        length(SList, N), 
        SList :: 1..N,
        get_prob_handle(H),
        convert_index_vars(IndexType, ConLev, SList, SList1),
        ec_to_gecode_varlist([Cost|SList1], H, [GCost|GSs]),
        SArr =.. [[]|GSs],
        ( ArcCosts == [] ->
            GAC = []
        ;
            collection_to_list(ArcCosts, ACList),
            ec_to_gecode_varlist(ACList, H, GACList),
            GAC =.. [[]|GACList]
        ),
        post_new_event(post_circuit_cost(ConLev,SArr,CMArr,GAC,GCost), H).


sequence(Lo, Hi, K, Vars, Values) :-
        sequence_c(Lo, Hi, K, Vars, Values, default).

sequence_c(Lo, Hi, K, Vars, Values, ConLev) :-
        integer(Lo), integer(Hi), 
        Hi >= Lo, Lo >= 0,
        integer(K), K > 0,
        collection_to_list(Vars, VarList),
        collection_to_list(Values, ValList),
        (foreach(V, ValList) do integer(V)),
        get_prob_handle(H),
        ec_to_gecode_varlist(VarList, H, GVars),
        VarArr =.. [[]|GVars],
        ValArr =.. [[]|ValList],
        K =< arity(VarArr),
        post_new_event(post_sequence(ConLev, Lo, Hi, K, VarArr, ValArr), H).

sequence(Lo, Hi, K, ZeroOnes) :-
        sequence_c(Lo, Hi, K, ZeroOnes, default).

sequence_c(Lo, Hi, K, ZeroOneVars, ConLev) :-
        integer(Lo), integer(Hi), 
        Hi >= Lo, Lo >= 0,
        integer(K), K > 0,
        collection_to_list(ZeroOneVars, VarList),
        VarList :: [0..1],
        get_prob_handle(H),
        ( foreach(B, VarList), param(H),
          foreach(GB, GBList)
        do
            link_var_to_boolvar(B, H),
            ( var(B) ->
                get_gecode_var(B, GB)
            ; B == 1 ->
                GB = 1
            ; B == 0 ->
                GB = 0
            ;
                fail
            )
        ),
        VarArr =.. [[]|GBList],
        K =< arity(VarArr),
        post_new_event(post_sequence_01(ConLev, Lo, Hi, K, VarArr), H).

cumulatives_min(Starts, Durations, Usages, UsedMachines, MachineMin) :-
        cumulatives_c(Starts, Durations, Usages, UsedMachines, MachineMin, 0,
                      ecl, default).

cumulatives_min_g(Starts, Durations, Usages, UsedMachines, MachineMin) :-
        cumulatives_c(Starts, Durations, Usages, UsedMachines, MachineMin, 0,
                      gc, default).

cumulatives(Starts, Durations, Usages, UsedMachines, MachineLimits) :-
        cumulatives_c(Starts, Durations, Usages, UsedMachines, MachineLimits,
                      1, ecl, default).

cumulatives_g(Starts, Durations, Usages, UsedMachines, MachineLimits) :-
        cumulatives_c(Starts, Durations, Usages, UsedMachines, MachineLimits,
                      1, gc, default).

cumulatives_min_c(Starts, Durations, Usages, UsedMachines, MachineMin, ConLev) :-
        cumulatives_c(Starts, Durations, Usages, UsedMachines, MachineMin, 0, ecl, ConLev).

cumulatives_min_g_c(Starts, Durations, Usages, UsedMachines, MachineMin, ConLev) :-
        cumulatives_c(Starts, Durations, Usages, UsedMachines, MachineMin, 0, gc, ConLev).

cumulatives_c(Starts, Durations, Usages, UsedMachines, MachineMin, ConLev) :-
        cumulatives_c(Starts, Durations, Usages, UsedMachines, MachineMin, 1, ecl, ConLev).

cumulatives_g_c(Starts, Durations, Usages, UsedMachines, MachineMin, ConLev) :-
        cumulatives_c(Starts, Durations, Usages, UsedMachines, MachineMin, 1, gc, ConLev).

cumulatives_c(Starts, Durations, Usages, UsedMachines, Limits, AtMost, IndexType, ConLev) :-
        collection_to_list(Starts, StartsL),
        ec_to_gecode_varlist(StartsL, H, GStartsL),
        StartsArr =.. [[]|GStartsL],
        arity(StartsArr, N),
        collection_to_list(Durations, DurationsL),
        ec_to_gecode_varlist(DurationsL, H, GDurationsL),
        DurationsArr =.. [[]|GDurationsL],
        arity(DurationsArr,N),
        collection_to_list(Usages, UsagesL),
        ec_to_gecode_varlist(UsagesL, H, GUsagesL),
        UsagesArr =.. [[]|GUsagesL],
        arity(UsagesArr,N),
        collection_to_list(UsedMachines, UsedL),
        ec_to_gecode_varlist(UsedL, H, GUsedL),
        UsedArr =.. [[]|GUsedL],
        arity(UsedArr,N),
        functor(EndsArr, [], N),
        ( foreacharg(S, StartsArr), foreacharg(D, DurationsArr),
          foreacharg(E, EndsArr)
        do
            S + D #= E
        ),

        collection_to_list(Limits, LimitsL),
	(IndexType == ecl ->
            LimitsArr =.. [[],0|LimitsL], % dummy limit for 0'th machine 
	    First = 1
        ;
            LimitsArr =.. [[]|LimitsL],
	    First = 0
	),
	UsedL :: First..arity(LimitsArr) - 1,
        post_new_event(post_cumulatives(ConLev, StartsArr, DurationsArr, EndsArr,
                                        UsagesArr, UsedArr, LimitsArr, AtMost), H).

% compatibility for 3.3 -- no specialised cumulative
cumulative(Starts, Durations, Usages, Limit) :-
        cumulative_c(Starts, Durations, Usages, Limit, default).

cumulative_c(Starts, Durations, Usages, Limit, ConLev) :-
        collection_to_list(Starts, StartsL),
        ec_to_gecode_varlist(StartsL, H, GStartsL),
        StartsArr =.. [[]|GStartsL],
        arity(StartsArr, N),
        collection_to_list(Durations, DurationsL),
        ec_to_gecode_varlist(DurationsL, H, GDurationsL),
        DurationsArr =.. [[]|GDurationsL],
        arity(DurationsArr,N),
        collection_to_list(Usages, UsagesL),
        ec_to_gecode_varlist(UsagesL, H, GUsagesL),
        UsagesArr =.. [[]|GUsagesL],
        arity(UsagesArr,N),
        functor(UsedArr, [], N),
        ( foreacharg(0, UsedMachines) do true),

        functor(EndsArr, [], N),
        ( foreacharg(S, StartsArr), foreacharg(D, DurationsArr),
          foreacharg(E, EndsArr)
        do
            S + D #= E
        ),
        integer(Limit),
        LimitsArr = [](Limit),
        post_new_event(post_cumulatives(ConLev, StartsArr, DurationsArr, EndsArr,
                                        UsagesArr, UsedArr, LimitsArr), H).

% gecode 3.3 allows ground Durations only
disjunctive(Starts, Durations) :-
        collection_to_list(Starts, SList),
        collection_to_list(Durations, DList),
        SList \== [],
        get_prob_handle(H),
        ec_to_gecode_varlist(SList, H, GSs),
%        ec_to_gecode_varlist(DList, H, GDs),
        SsArr =.. [[]|GSs],
%        DsArr =.. [[]|GDs],
        DsArr =.. [[]|DList],
        arity(SsArr, N),
        arity(DsArr, N),  % same size
        post_new_event(post_disj(default, SsArr,DsArr,[]), H).

disjunctive_optional(Starts, Durations, Scheduled) :-
        collection_to_list(Starts, SList),
        collection_to_list(Durations, DList),
        collection_to_list(Scheduled, BList),
        SList \== [],
        BList :: 0..1,
        get_prob_handle(H),
        ec_to_gecode_varlist(SList, H, GSs),
%        ec_to_gecode_varlist(DList, H, GDs),
        SsArr =.. [[]|GSs],
%        DsArr =.. [[]|GDs],
        DsArr =.. [[]|DList],
        BsArr =.. [[]|BList],
        arity(SsArr, N),
        arity(DsArr, N),  % same size
        arity(BsArr, N),
        post_new_event(post_disj(default, SsArr,DsArr,BsArr), H).


bool_channeling(V, Bools, Min) :-
        bool_channeling_c(V, Bools, Min, default).

bool_channeling_c(V, Bools, Min, ConLev) :-
        integer(Min),
        collection_to_list(Bools, BList),
        BList :: [0..1],
        V :: [Min..(Min+length(BList))],
        get_prob_handle(H),
        ( foreach(B, BList), param(H),
          foreach(GB, GBList)
        do
            link_var_to_boolvar(B, H),
            ( var(B) ->
                get_gecode_var(B, GB)
            ; B == 1 ->
                GB = 1
            ; B == 0 ->
                GB = 0
            ;
                fail
            )
        ),
        GBArr =.. [[]|GBList],
        ( var(V) ->
            get_gecode_var(V, GV)
        ; % V must be integer as :: succeeded 
            GV = V
        ),
        post_new_event(post_boolchannel(ConLev,GV,GBArr,Min), H). 

inverse(XL, YL) :-
        inverse_body(XL, YL, ecl, default).

inverse_c(XL, YL, ConLev) :-
        inverse_body(XL, YL, ecl, ConLev).

inverse_g(XL, YL) :-
        inverse_body(XL, YL, gc, default).

inverse_g_c(XL, YL, ConLev) :-
        inverse_body(XL, YL, gc, ConLev).

inverse_body(Vs1, Vs2, IndexType, ConLev) :-
        ( var(Vs1) -> 
            Vs1 = Vars1
        ;
            collection_to_list(Vs1, Vars1)
        ),
        ( var(Vs2) -> 
            nonvar(Vs1),
            Vs2 = Vars2
        ;
            collection_to_list(Vs2, Vars2)
        ),
	( foreach(_,Vars1), foreach(_,Vars2) do true ),
        Vars1 \== [],
        get_prob_handle(H),
        ec_to_gecode_varlist(Vars1, H, GVars1),
        ec_to_gecode_varlist(Vars2, H, GVars2),
        % add 0 to match ECLiPSe index (starting from 1)
        (IndexType == ecl ->
            Arr1 =.. [[],0|GVars1],
            Arr2 =.. [[],0|GVars2]
        ;
            Arr1 =.. [[]|GVars1],
            Arr2 =.. [[]|GVars2]
        ),
        post_new_event(post_inverse(ConLev, Arr1, Arr2), H).

inverse(XL, XOff, YL, YOff) :-
        inverse_body(XL, XOff, YL, YOff, ecl, default).

inverse_c(XL, XOff, YL, YOff, ConLev) :-
        inverse_body(XL, XOff, YL, YOff, ecl, ConLev).

inverse_g(XL, XOff, YL, YOff) :-
        inverse_body(XL, XOff, YL, YOff, gc, default).

inverse_g_c(XL, XOff, YL, YOff, ConLev) :-
        inverse_body(XL, XOff, YL, YOff, gc, ConLev).

inverse_body(Vs1, Off1, Vs2, Off2, IndexType, ConLev) :-
        integer(Off1),
        integer(Off2),
	(IndexType == ecl ->
	    GOff1 is Off1 - 1,
            GOff2 is Off2 - 1
	;
	    GOff1 is Off1,
	    GOff2 is Off2
	),
        ( var(Vs1) -> 
            Vs1 = Vars1
        ;
            collection_to_list(Vs1, Vars1)
        ),
        ( var(Vs2) -> 
            nonvar(Vs1),
            Vs2 = Vars2
        ;
            collection_to_list(Vs2, Vars2)
        ),
	( foreach(_,Vars1), foreach(_,Vars2) do true ),
        Vars1 \== [],
        get_prob_handle(H),
        ec_to_gecode_varlist(Vars1, H, GVars1),
        ec_to_gecode_varlist(Vars2, H, GVars2),
        Arr1 =.. [[]|GVars1],
        Arr2 =.. [[]|GVars2],
        post_new_event(post_inverse_offset(ConLev, Arr1, GOff1, Arr2, GOff2), H).

min(Xs, Min) :-
        minlist_c(Xs, Min, default).

minlist(Xs, Min) :-
        minlist_c(Xs, Min, default).

minlist_c(Xs, Min, ConLev) :-
        collection_to_list(Xs, XLs),
        get_prob_handle(H),
        ec_to_gecode_varlist([Min|XLs], H, [GMin|GLs]),
        GArray =.. [[]|GLs],
        post_new_event(post_minlist(ConLev, GMin, GArray), H).

max(Xs, Max) :-
        maxlist_c(Xs, Max, default).

maxlist(Xs, Max) :-
        maxlist_c(Xs, Max, default).

maxlist_c(Xs, Max, ConLev) :-
        collection_to_list(Xs, XLs),
        get_prob_handle(H),
        ec_to_gecode_varlist([Max|XLs], H, [GMax|GLs]),
        GArray =.. [[]|GLs],
        post_new_event(post_maxlist(ConLev, GMax, GArray), H).

sum(Xs, Sum) :-
        sum_c(Xs, Sum, default).

sum_c(Xs, Sum, ConLev) :-
        nonvar(Xs),
        ( Xs = Cs * Vs ->
            scalar_product_c(Cs, Vs, (#=), Sum, ConLev)
        ;
            sum_c(Xs, (#=), Sum, ConLev)
        ).


sum(Xs, Rel, Sum) :-
        sum_c(Xs, Rel, Sum, default).

sum_c(Xs, Rel, Sum, ConLev) :-
	get_prob_handle(H),
        H = gfd_prob{nvars:NV0},
	linsum_event1(Xs, Rel, Sum, ConLev, H, NV0,NV, Events,EventsT),
	gfd_default_interval(Min, Max),
	update_vars_for_gecode(NV0, NV, [], H, Min, Max),
        post_new_event_with_aux(Events, EventsT, H).


linsum_event1(Xs, Rel, Sum, ConLev, H, NV0,NV, Events,EventsT) :-
        linsum_body1(Xs, Rel, Sum, GArray, GSum, H, NV0,NV),
        Events = [post_sum(ConLev, GArray, Rel, GSum)|EventsT].

linsum_body1(Xs, Rel, Sum, GArray, GSum, H, NV0,NV) :-
	atomic(Rel),
	rel_op(Rel),
        collection_to_list(Xs, XLs),
        ec_to_gecode_varlist1([Sum|XLs], H, NV0,NV, [GSum|GLs]),
        GArray =.. [[]|GLs].


sum(Xs, Rel, Sum, Bool) :-
        sum_reif_c(Xs, Rel, Sum, Bool, default).

sum_reif_c(Xs, Rel, Sum, Bool, ConLev) :-
	get_prob_handle(H),
        H = gfd_prob{nvars:NV0},
	linsum_reif_event1(Xs, Rel, Sum, Bool, ConLev, H, NV0,NV, [],
                           Bs, Event, _GBool),
	gfd_default_interval(Min, Max),
	update_vars_for_gecode(NV0, NV, Bs, H, Min, Max),
        post_new_event_with_aux([Event|EventsT], EventsT, H).

linsum_reif_event1(Xs, Rel, Sum, Bool, ConLev, H, NV0,NV, Bs0,Bs, Event, GBool) :-
        linsum_body1(Xs, Rel, Sum, GArray, GSum, H, NV0,NV1),
        ( var(Bool) ->
            ec_to_gecode_var1(Bool, H, NV1,NV, [],_Old, GBool),
            Bs = [Bool|Bs0]
        ;
            GBool = Bool,
            NV1 = NV,
            Bs = Bs0
        ;
            fail
        ),
        Event = post_sum_reif(ConLev, GArray, Rel, GSum, GBool).


scalar_product(Cs, Xs, Rel, P) :-
        scalar_product_c(Cs, Xs, Rel, P, default).

scalar_product_c(Cs, Xs, Rel, P, ConLev) :-
	get_prob_handle(H),
        H = gfd_prob{nvars:NV0},
	scalar_product_event1(Cs, Xs, Rel, P, ConLev, H, NV0,NV, Events,EventsT),
	gfd_default_interval(Min, Max),
	update_vars_for_gecode(NV0, NV, [], H, Min, Max),
        post_new_event_with_aux(Events, EventsT, H).

scalar_product_event1(Cs, Xs, Rel, P, ConLev, H, NV0,NV, Events,EventsT) :-
	scalar_product_body1(Cs, Xs, Rel, P, CArray, GArray, GP, H, NV0,NV),
	Events = [post_lin(ConLev, GArray, CArray, Rel, GP)|EventsT].
		  
scalar_product_body1(Cs, Xs, Rel, P, CArray, GArray, GP, H, NV0,NV) :-
	atomic(Rel),
	rel_op(Rel),
        collection_to_list(Xs, XLs),
        collection_to_list(Cs, CLs),
        ec_to_gecode_varlist1([P|XLs], H, NV0,NV, [GP|GLs]),
        GArray =.. [[]|GLs],
	CArray =.. [[]|CLs],
	arity(GArray) =:= arity(CArray),
	(foreach(C, CLs) do integer(C)).


scalar_product(Cs, Xs, Rel, P, Bool) :-
        scalar_product_reif_c(Cs, Xs, Rel, P, Bool, default).

scalar_product_reif_c(Cs, Xs, Rel, P, Bool, ConLev) :-
	get_prob_handle(H),
        H = gfd_prob{nvars:NV0},
	scalar_product_reif_event1(Cs, Xs, Rel, P, Bool, ConLev, H,
                                   NV0,NV, [],Bs, Event, _GBool),
	gfd_default_interval(Min, Max),
	update_vars_for_gecode(NV0, NV, Bs, H, Min, Max),
        post_new_event_with_aux([Event|EventsT], EventsT, H).

scalar_product_reif_event1(Cs, Xs, Rel, P, Bool, ConLev, H, NV0,NV, Bs0,Bs, Event, GBool) :-
	scalar_product_body1(Cs, Xs, Rel, P, CArray, GArray, GP, H, NV0,NV1),
        ( var(Bool) ->
            ec_to_gecode_var1(Bool, H, NV1,NV, [],_Old, GBool),
            Bs = [Bool|Bs0]
        ;
            GBool = Bool,
            NV1 = NV,
            Bs = Bs0
        ;
            fail
        ),
	Event = post_lin_reif(ConLev, GArray, CArray, Rel, GP, GBool).


plus(X, Y, Sum) :-
     plus_c(X, Y, Sum, default).

plus_c(X, Y, Sum, ConLev) :-
        get_prob_handle(H),
        ec_to_gecode_varlist([Sum,X,Y], H, [GSum|GXY]),
        GArray =.. [[]|GXY],
        post_new_event(post_sum(ConLev, GArray, (#=), GSum), H).

 
sqrt(X, Y) :-
        sqrt_c(X, Y, default).

sqrt_c(X, Y, ConLev) :-
        get_prob_handle(H),
        ec_to_gecode_varlist([X,Y], H, [GX,GY]),
        post_new_event(post_sqrt(ConLev, GY,GX), H).


sqr(X, Y) :-
        sqr_c(X, Y, default).

sqr_c(X, Y, ConLev) :-
        get_prob_handle(H),
        ec_to_gecode_varlist([X,Y], H, [GX,GY]),
        post_new_event(post_sq(ConLev, GY,GX), H).

abs(X, Y) :-
       abs_c(X, Y, default).

abs_c(X, Y, ConLev) :-
        get_prob_handle(H),
        ec_to_gecode_varlist([X,Y], H, [GX,GY]),
        post_new_event(post_abs(ConLev, GY,GX), H).

divide(X, Y, Z) :-
       div_c(X, Y, Z, default).

div_c(X, Y, Z, ConLev) :-
        get_prob_handle(H),
        ec_to_gecode_varlist([X,Y,Z], H, [GX,GY,GZ]),
        post_new_event(post_div(ConLev, GZ, GX,GY), H).


divmod(X, Y, Q, M) :-
	divmod_c(X, Y, Q, M, default).

divmod_c(X, Y, Q, M, ConLev) :-
        get_prob_handle(H),
        ec_to_gecode_varlist([X,Y,Q,M], H, [GX,GY,GQ,GM]),
        post_new_event(post_divmod(ConLev, GX, GY, GQ, GM), H).


mult(X, Y, Z) :-
	mult_c(X, Y, Z, default).

mult_c(X, Y, Z, ConLev) :-
        get_prob_handle(H),
        ec_to_gecode_varlist([X,Y,Z], H, [GX,GY,GZ]),
        post_new_event(post_mult(ConLev, GZ, GX,GY), H).


mod(X, Y, Z) :-
	mod_c(X, Y, Z, default).

mod_c(X, Y, Z, ConLev) :-
        get_prob_handle(H),
        ec_to_gecode_varlist([X,Y,Z], H, [GX,GY,GZ]),
        post_new_event(post_mod(ConLev, GZ, GX,GY), H).


min(X, Y, Z) :-
	min_c(X, Y, Z, default).

min_c(X, Y, Z, ConLev) :-
        get_prob_handle(H),
        ec_to_gecode_varlist([X,Y,Z], H, [GX,GY,GZ]),
        post_new_event(post_min2(ConLev, GZ, GX,GY), H).

max(X, Y, Z) :-
	max_c(X, Y, Z, default).

max_c(X, Y, Z, ConLev) :-
        get_prob_handle(H),
        ec_to_gecode_varlist([X,Y,Z], H, [GX,GY,GZ]),
        post_new_event(post_max2(ConLev, GZ, GX,GY), H).


%------------------------------------------------------------------------
% Events


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
        

% posting an event that may have additional "auxillary" events
post_new_event_with_aux(Es, EsTail, H) :-
        set_new_event(Es, EsTail0, H),
        post_new_events1(Es, H, _First, DoProp),
        EsTail = EsTail0, % join posted events to head of events list 
        ( var(DoProp) -> true ; try_propagate(1, H), wake ).


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

/* post_new_event call wake at the end, so that gfd_do_propagate will
   be woken and executed. This may not be appropriate if the posted 
   event is part of an atomic operation, e.g. in the unify handler, when
   goals only woken later when all the handlers have been executed
*/
post_new_event(E, H) :-
        post_new_event_no_wake(E, H),
        wake.

post_new_event_no_wake(E, H) :-
        Es = [E|ET],
        set_new_event(Es, ET, H),
        do_event(E, H, 1, DoProp), % First = 1 (not recomputing)
        try_propagate(DoProp, H).

try_propagate(0, _).
try_propagate(1, H) ?-
        H = gfd_prob{prop:Susp},
        schedule_suspensions(1, s([Susp])).


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
        ( NL mod getval(cloning_distance) =:= 0 ->
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

/* do_event execute a Gecode event. This can be when the event is first
   executed (First = 1), or when the state is being recomputed (First = 0).
   DoProp is returned to indicate if propagation should be done after the
   event (on first execution only). 
   Handling of events should not assume that any arguments that are 
   variables in first execution will be variables during recomputation
*/
do_event(E, H, First, DoProp) :-
%        writeln(doing-E-First),
        H = gfd_prob{space:gfd_space{handle:SpH}},
%        arg(space of gfd_prob, H, Space),
%        arg(handle of gfd_space, Space, SpH),
        do_event1(E, SpH, First, DoProp).

do_event1(post_rc(ConLev, GExpr), SpH, First, DoProp) ?-
        DoProp = 1,
        g_post_intrel_cstr(SpH, First, GExpr, ConLev).
do_event1(post_bool_connectives(ConLev, GBCon), SpH, First, DoProp) ?-
        DoProp = 1,
        g_post_bool_connectives(SpH, First, GBCon, ConLev).
do_event1(post_alldiff(ConLev, GArray), SpH, First, DoProp) ?-
        DoProp = 1,
        g_post_alldiff(SpH, First, GArray, ConLev).
do_event1(post_alldiff_offsets(ConLev, GArray, OArray), SpH, First, DoProp) ?-
        DoProp = 1,
        g_post_alldiff_offsets(SpH, First, GArray, OArray, ConLev).
do_event1(post_count(ConLev, Value,GArray,Rel, N), SpH, First, DoProp) ?-
        DoProp = 1,
        g_post_count(SpH, First, Value, GArray, Rel, N, ConLev).
do_event1(post_gcc(ConLev, Vals,Occs,GVs), SpH, First, DoProp) ?-
        DoProp = 1,
        g_post_gcc(SpH, First, Vals, Occs, GVs, ConLev).
do_event1(post_element(ConLev, GI,GArray,GValue), SpH, First, DoProp) ?-
        DoProp =1,
        g_post_element(SpH, First, GI, GArray, GValue, ConLev).
do_event1(post_sequence(ConLev, Lo, Hi, K, VarArray, ValArray), SpH, First, DoProp) ?-
        DoProp =1,
        g_post_sequence(SpH, First, Lo, Hi, K, VarArray, ValArray, ConLev).
do_event1(post_sequence_01(ConLev, Lo, Hi, K, VarArray), SpH, First, DoProp) ?-
        DoProp =1,
        g_post_sequence_01(SpH, First, Lo, Hi, K, VarArray, ConLev).
do_event1(post_sorted2(ConLev, UsArray, SsArray), SpH, First, DoProp) ?-
        DoProp = 1,
        g_post_sorted2(SpH, First, UsArray, SsArray, ConLev).
do_event1(post_sorted(ConLev, UsArray, SsArray, PsArray), SpH, First, DoProp) ?-
        DoProp = 1,
        g_post_sorted(SpH, First, UsArray, SsArray, PsArray, ConLev).
do_event1(post_circuit(ConLev, SArray), SpH, First, DoProp) ?-
        DoProp = 1,
        g_post_circuit(SpH, First, SArray, ConLev).
do_event1(post_circuit_cost(ConLev, SArray, CMArray, ACArray, GCost), SpH, First, DoProp) ?-
        DoProp = 1,
        g_post_circuit_cost(SpH, First, SArray, CMArray, ACArray, GCost, ConLev).
do_event1(post_disj(_ConLev,StartArray,DurArray,SchArray), SpH, First, DoProp) ?-
        DoProp = 1,
        % ConLev not supported for this constraint
        g_post_disj(SpH, First, StartArray, DurArray,SchArray).
do_event1(post_cumulatives(_ConLev,Starts,Durations,Ends,Usages,Used,Limits,AtMost), SpH, First, DoProp) ?-
        DoProp = 1,
        g_post_cumulatives(SpH, First, Starts, Durations, Ends, Usages, Used, Limits, AtMost).
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
do_event1(newvars_dom_union(GX,GY,NV), SpH, _First, DoProp) ?-
        DoProp = 0,
        g_add_newvars_dom_union(SpH, NV, GX, GY).
do_event1(copyvar(NV,OldIdx), SpH, _First, DoProp) ?-
        DoProp = 0,
        g_add_newvar_copy(SpH, NV, OldIdx).
do_event1(setvar(Idx, Val), SpH, First, DoProp) ?-
        DoProp = 1,
        g_post_setvar(SpH, First, Idx, Val).
do_event1(post_sum(ConLev, GArray, Rel, C), SpH, First, DoProp) ?-
        DoProp = 1,
        g_post_sum(SpH, First, GArray, Rel, C, ConLev).
do_event1(post_sum_reif(ConLev, GArray, Rel, C, GBool), SpH, First, DoProp) ?-
        DoProp = 1,
        g_post_sum_reif(SpH, First, GArray, Rel, C, GBool, ConLev).
do_event1(post_lin(ConLev, GArray, CArray, Rel, C), SpH, First, DoProp) ?-
        DoProp = 1,
        g_post_lin(SpH, First, GArray, CArray, Rel, C, ConLev).
do_event1(post_lin_reif(ConLev, GArray, CArray, Rel, C, GBool), SpH, First, DoProp) ?-
        DoProp = 1,
        g_post_lin_reif(SpH, First, GArray, CArray, Rel, C, GBool, ConLev).
do_event1(post_maxlist(ConLev, GV, GArray), SpH, First, DoProp) ?-
        DoProp = 1,
        g_post_maxlist(SpH, First, GV, GArray, ConLev).
do_event1(post_minlist(ConLev, GV, GArray), SpH, First, DoProp) ?-
        DoProp = 1,
        g_post_minlist(SpH, First, GV, GArray, ConLev).
do_event1(post_sqrt(ConLev, GRes, GX), SpH, First, DoProp) ?-
        DoProp = 1,
        g_post_sqrt(SpH, First, GRes, GX, ConLev).
do_event1(post_sq(ConLev, GRes, GX,_Power), SpH, First, DoProp) ?-
        DoProp = 1,
        g_post_sq(SpH, First, GRes, GX, ConLev).
do_event1(post_sq(ConLev, GRes, GX), SpH, First, DoProp) ?-
        DoProp = 1,
        g_post_sq(SpH, First, GRes, GX, ConLev).
do_event1(post_abs(ConLev, GRes, GX), SpH, First, DoProp) ?-
        DoProp = 1,
        g_post_abs(SpH, First, GRes, GX, ConLev).
do_event1(post_div(_ConLev, GRes, GX, GY), SpH, First, DoProp) ?-
        DoProp = 1,
        g_post_div(SpH, First, GRes, GX, GY).
do_event1(post_mult(ConLev, GRes, GX, GY), SpH, First, DoProp) ?-
        DoProp = 1,
        g_post_mult(SpH, First, GRes, GX, GY, ConLev).
do_event1(post_mod(_ConLev, GRes, GX, GY), SpH, First, DoProp) ?-
        DoProp = 1,
        g_post_mod(SpH, First, GRes, GX, GY).
do_event1(post_divmod(_ConLev, GX, GY, GQ, GM), SpH, First, DoProp) ?-
        DoProp = 1,
        g_post_divmod(SpH, First, GX, GY, GQ, GM).
do_event1(post_min2(ConLev, GRes, GX, GY), SpH, First, DoProp) ?-
        DoProp = 1,
        g_post_min2(SpH, First, GRes, GX, GY, ConLev).
do_event1(post_max2(ConLev, GRes, GX, GY), SpH, First, DoProp) ?-
        DoProp = 1,
        g_post_max2(SpH, First, GRes, GX, GY, ConLev).
do_event1(newbool(Idx,BIdx), SpH, _First, DoProp) ?-
        DoProp = 0,
        g_add_newbool(SpH, Idx, BIdx).
do_event1(post_boolchannel(ConLev,GV,GBArr,Min), SpH, First, DoProp) ?-
        DoProp = 1,
        g_post_boolchannel(SpH, First, GV, GBArr, Min, ConLev).
do_event1(post_inverse(ConLev,Arr1,Arr2), SpH, First, DoProp) ?-
        DoProp = 1,
        g_post_inverse(SpH, First, Arr1, Arr2, ConLev).
do_event1(post_inverse_offset(ConLev,Arr1,Off1,Arr2,Off2), SpH, First, DoProp) ?-
        DoProp = 1,
        g_post_inverse_offset(SpH, First, Arr1, Off1, Arr2, Off2, ConLev).
do_event1(propagate, SpH, First, DoProp) ?-
        DoProp = 0,
%        g_propagate(SpH).
        get_prob_handle(gfd_prob{vars:VArr}),
%        g_propagate(SpH, First, VArr).
        g_propagate(SpH, First, InstList, ChgList),
        propagate_gecode_changes(SpH, VArr, InstList, ChgList).


propagate_gecode_changes(SpH, VArr, InstList, ChgList) :-
        ( InstList == [] ->
%            store_inc(stats, pg0)
            true
        ;
%            length(InstList, Len), concat_atom([pg, Len], Key), store_inc(stats, Key), 
            ( foreach(Idx, InstList), param(VArr, SpH) do
                arg(Idx, VArr, V),
                (integer(V) -> true ; mark_var_as_set(V),g_get_var_value(SpH, Idx, V))
%                (integer(V) -> true ; g_get_var_value(SpH, Idx, V))
            )
        ),
        ( ChgList == [] ->
            true
        ;
            ( foreach(CIdx, ChgList), param(VArr) do
                arg(CIdx, VArr, U),
/*                ( integer(U) -> 
                    % may be integer from previous unifications not yet
                    % sync'ed with Gecode
                    true
                ;
*/
                    get_gecode_attr(U, _H, Attr),
                    % assuming only one single problem, otherwise need H
                    schedule_suspensions(any of gfd, Attr)
%                )
            )
        ).
           

mark_var_as_set(_{gfd{set:S}}) ?- !, S = [].



%------------------------------------------------------------------------
% labelling and search

indomain(I) :- integer(I), !.
indomain(V{gfd:Attr}) ?-
        nonvar(Attr),
        Attr = gfd{prob:H,idx:Idx},
        restore_space_if_needed(H, SpH),
        g_get_var_domain(SpH, Idx, Domain),
        set_domain_value(Domain, V).

set_domain_value([D|_Ds], V) ?-
        set_a_domain_value(D, V).
set_domain_value([_|Ds], V) ?-
        set_domain_value(Ds, V).

set_a_domain_value(Lo..Hi, V) ?- !,
        between(Lo, Hi, 1, Val),
%        set_a_domain_value(Val, V).
        Val = V.
set_a_domain_value(I, V) :-
        V = I.


indomain(V, min) ?- !,
        do_indomain_min(V).
indomain(V, max) ?- !,
        do_indomain_max(V).
indomain(V, median) ?- !,
        do_indomain_median(V).
indomain(V, middle) ?- !,
        do_indomain_middle(V).
indomain(V, enum) ?- !,
        indomain(V).
indomain(V, I) ?- 
        integer(I), !,
        do_indomain_from_value(V, I).



labeling(Vars) :-
        collection_to_list(Vars, List),
        ( foreach(Var, List) do do_indomain_min(Var) ).


do_indomain_min(I) :- integer(I), !.
do_indomain_min(V{gfd:Attr}) ?-
        nonvar(Attr),
        Attr = gfd{prob:H, idx:Idx},
        restore_space_if_needed(H, SpH),
        g_get_var_lwb(SpH, Idx, Lo),
        % -1 for lower bound
        indomain_and_prop(Idx, V, H, -1, Lo).

do_indomain_max(I) :- integer(I), !.
do_indomain_max(V{gfd:Attr}) ?-
        nonvar(Attr),
        Attr = gfd{prob:H, idx:Idx},
        restore_space_if_needed(H, SpH),
        g_get_var_upb(SpH, Idx, Lo),
        % 1 for upper bound
        indomain_and_prop(Idx, V, H, 1, Lo).

indomain_and_prop(_Idx, V, _H, _W, Val) :-
        V = Val.
indomain_and_prop(Idx, V, H, Which, Val) :-
        restore_space_if_needed(H, _SpH), 
        % -1 for lower bound
        arg(space of gfd_prob, H, Sp),
        g_update_and_get_var_bound(Sp, Idx, Val, Which, Lo),
        indomain_and_prop(Idx, V, H, Which, Lo).

do_indomain_median(I) :- integer(I), !.
do_indomain_median(V{gfd:Attr}) ?-
        nonvar(Attr),
        Attr = gfd{prob:H, idx:Idx},
        restore_space_if_needed(H, SpH),
        g_get_var_median(SpH, Idx, Med),
        arg(space of gfd_prob, H, Sp),
        indomain_from(V, Med, H, Sp, Idx).

do_indomain_middle(I) :- integer(I), !.
do_indomain_middle(V{gfd:Attr}) ?-
        nonvar(Attr),
        Attr = gfd{prob:H, idx:Idx},
        restore_space_if_needed(H, SpH),
        g_get_var_bounds(SpH, Idx, Lo, Hi),
        Mid is (Hi+Lo) // 2,
        arg(space of gfd_prob, H, Sp),
        indomain_from(V, Mid, H, Sp, Idx).

do_indomain_from_value(I, Value) :- 
        integer(I), !,
        Value == I.
do_indomain_from_value(V{gfd:Attr}, Value) ?-
        nonvar(Attr),
        Attr = gfd{prob:H, idx:Idx},
        restore_space_if_needed(H, _SpH),
        arg(space of gfd_prob, H, Sp),
        indomain_from(V, Value, H, Sp, Idx).


/* label V starting with Val, and then trying values next to Val,
   alternating between higher and lower values. OldHi and OldLo
   are used to record the last Hi and Lo values tried.
*/
indomain_from(V, Val, H, Sp, Idx) :- 
        Val0 is Val + 1,
        shelf_create(last(Val), OldHi),
        shelf_create(last(Val0), OldLo),
        indomain_from1(1, V, H, Sp, Idx, OldHi, OldLo).

indomain_from1(-1, V, H, Sp, Idx, OldHi, OldLo) :-
        % trying larger values
        shelf_get(OldHi, 1, Old),
        ( g_update_and_get_var_bound(Sp, Idx, Old, -1, Next) ->
            % propagate OldHi value as lower bound, to get next lowest
            % value in domain to try
            shelf_set(OldHi, 1, Next),
            V = Next
        ;
            !,
            % no higher values left in domain, try smaller values only
            shelf_get(OldLo, 1, Lo0),
            restore_space_if_needed(H, _SpH),
            g_update_and_get_var_bound(Sp, Idx, Lo0, 1, Lo), % may fail!
            indomain_and_prop(Idx, V, H, 1, Lo)
        ).
indomain_from1(1, V, H, Sp, Idx, OldHi, OldLo) :-
        % trying smaller values
        shelf_get(OldLo, 1, Old),
        ( g_update_and_get_var_bound(Sp, Idx, Old, 1, Next) ->
            shelf_set(OldLo, 1, Next),
            V = Next
        ;
            !,
            shelf_get(OldHi, 1, Hi0),
            restore_space_if_needed(H, _SpH),
            g_update_and_get_var_bound(Sp, Idx, Hi0, -1, Hi), % may fail
            indomain_and_prop(Idx, V, H, -1, Hi)
        ).
indomain_from1(Which, V, H, Sp, Idx, OldHi, OldLo) :-
        restore_space_if_needed(H, _SpH),
        NewWhich is Which * -1,
        indomain_from1(NewWhich, V, H, Sp, Idx, OldHi, OldLo).

% Based on delete/5 in generic_search.ecl
% delete(-X,+List:non_empty_list,-R:list,++Arg:integer,++Select:atom,++Module:atom)
% choose one entry in the list based on a heuristic
% this is a deterministic selection
% a special case for input order to speed up the selection in that case
%
:-mode delete(-,+,-,++,++,++).
delete(H,List,T,_Arg,input_order, _Module):-
	!, List = [H|T].
delete(X,List,R,Arg,Select, Module):-
	List = [H|T],
	find_criteria(H,Arg,Select,Crit, Module),
	( var(Crit) ->
	    X=H, R=T	% we can't do any better!
	;
	    find_best_and_rest(T,List,Crit,X,R,Arg,Select, Module)
	).


% find_best_and_rest(
%	+List:list,		the unscanned tail
%	+BestSoFar:list,	the tail starting with the current best
%	?Crit: variable, number or crit(Crit,Crit),
%	-Best, -Rest_best:list,	the result
%	++Arg:integer,++Select:atom,++Module:atom)
%
:- mode find_best_and_rest(+,+,?,-,-,++,++,++).
find_best_and_rest([], BestSoFar, _OldCrit, BestVar, Rest, _Arg, _Select, _Module) :- !,
	BestSoFar = [BestVar|Rest].
find_best_and_rest(List, BestSoFar, CritOld, BestVar, Rest, Arg, Select, Module) :-
	List = [Var|Vars],
	find_criteria(Var, Arg, Select, CritNew, Module),
	( CritNew @>= CritOld ->	% no better than the old one, continue
	    find_best_and_rest(Vars, BestSoFar, CritOld, BestVar, Rest, Arg, Select, Module)
	; nonvar(CritNew) ->		% found a better one, continue
	    % copy the chunk between old and new best
	    copy_until_elem(BestSoFar, Var, Rest, Rest0),
	    find_best_and_rest(Vars, List, CritNew, BestVar, Rest0, Arg, Select, Module)
	;
	    % we can't do any better, stop
	    BestVar = Var,
	    % copy the chunk between old and new best, and append the unscanned rest
	    copy_until_elem(BestSoFar, Var, Rest, Vars)
	).


% find_criteria(?Term,++Arg:integer,++Select:atom,
%		-Crit:integer or crit(integer,integer),
%               ++Module:atom)
%
% find a heuristic value from a term
:-mode find_criteria(?,++,++,-,++).
find_criteria(Term,0,Select,Crit, Module):-
	!,
	find_value(Term,Select,Crit, Module).
find_criteria(Term,Arg,Select,Crit, Module):-
	arg(Arg,Term,X),
	find_value(X,Select,Crit, Module).

% find_value(?X:dvarint,++Select:atom,
%	     -Crit:integer or crit(integer,integer),
%            ++Module:atom)
%
% Find a heuristic value from a domain variable: the smaller, the better.
% Values will be compared using @<, so be aware of standard term ordering!
% If the Criterion remains uninstantiated, this indicates an optimal value,
% which will be picked without looking any further down the list.
:-mode find_value(?,++,-,++).
find_value(X,first_fail,Size, _Module):-
	!,
	( nonvar(X) ->
	    true	% pick constants first and commit
	;
	    get_domain_size(X,Size0),
	    ( integer(Size0) -> Size=Size0 ; Size=inf )	% 99 @< 'inf'
	).
find_value(X,anti_first_fail,Number, _Module):-
	!,
	get_domain_size(X,Size),				% can be 1.0Inf
	Number is -Size.				% -1.0Inf @< -99
find_value(X,smallest,Min, _Module):-
	!,
	get_min(X,Min).
find_value(X,largest,Number, _Module):-
	!,
	get_max(X,Max),
	Number is -Max.
find_value(X,occurrence,Number, _Module):-
	!,
	( nonvar(X) ->
	    true	% pick constants first and commit
	;
	    get_constraints_number(X,Nr), 
	    Number is -Nr
	).
find_value(X,max_regret,Number, _Module):-
	!,
	( nonvar(X) ->
	    true	% pick constants first and commit
	;
	    get_regret_lwb(X, Regret),
	    Number is -Regret
	).
find_value(X,max_regret_upb,Number, _Module):-
	!,
	( nonvar(X) ->
	    true	% pick constants first and commit
	;
	    get_regret_upb(X, Regret),
	    Number is -Regret
	).
find_value(X,max_weighted_degree, Number, _Module):-
        !,
	( nonvar(X) ->
	    true	% pick constants first and commit
	;
	    get_weighted_degree(X, AFC),
	    Number is -AFC
	).
find_value(X,most_constrained,Crit, Module):-
	!,
	( nonvar(X) ->
	    true	% pick constants first and commit
	;
	    Crit = crit(Size,Number),
	    find_value(X,first_fail,Size, Module),
	    find_value(X,occurrence,Number, Module)
	).
find_value(X,most_constrained_per_value,Number, _Module):-
        !,
	( nonvar(X) ->
	    true	% pick constants first and commit
	;
            get_weighted_degree(X, AFC),
            get_domain_size(X, Size),
            Number is fix(round(Size/AFC))
        ).
find_value(X,max_weighted_degree_per_value,Number, _Module) :-
        !,
	( nonvar(X) ->
	    true	% pick constants first and commit
	;
            get_weighted_degree(X, AFC),
            get_domain_size(X, Size),
            Number is fix(round(Size/AFC))
        ).
find_value(X,User_method,Value,Module):-
	Call =..[User_method,X,Value],
	once(Call)@Module.	% do not allow backtracking in user routine


% Copy list until first occurrence of K and return as difference list
:- mode copy_until_elem(+,?,?,?).
copy_until_elem([X|Xs], K, Ys, Ys0) :-
	( X==K ->
	    Ys = Ys0
	;
	    Ys = [X|Ys1],
	    copy_until_elem(Xs, K, Ys1, Ys0)
	).


/* search/6 maps to Gecode's branching and search engines. 
   As cloning is done before and after the use of the search engine,
   we cannot treat the search as a normal gfd event. Instead, the 
   event queue is marked, so that the space returned after the search
   will become an ancestor if further events are posted. 
*/   
:- export struct(gfd_stats(prop,fail,node,depth,mem)).
:- export struct(gfd_control(commit_distance,adaptive_distance)).

search(Vars, Pos, Select, Choice, Method, Option) :-
        collection_to_list(Vars, List),
        get_prob_handle(H),
        H = gfd_prob{space:gfd_space{handle:SpH}},
        atom(Select), var_selection(Select),
        atom(Choice), val_choice(Choice),
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
        translate_search_method(Method, TMethod),
        GArray =.. [[]|GList],
        process_options(Option, TieBreak, Stats, Stop, Timeout, Control),
        g_setup_search(SpH, GArray, Select, Choice, TMethod, TieBreak, 
                       Timeout, Stop, Control, EngH),
        new_space_handle(SP),
        setarg(space of gfd_prob, H, SP),
        do_search(SpH, EngH, H, Method, Stats).

process_options([], TieBreak, _Stats, Stop, Timeout, Control) ?- !,
        (var(TieBreak) -> TieBreak = none ; true),
        (var(Stop) ->  Stop = none ; true),
        (var(Timeout) -> Timeout = 0 ; true),
        (var(Control) -> Control = none ; true).
process_options([O|Os], TieBreak, Stats, Stop, Timeout, Control) :-
        process_option(O, TieBreak, Stats, Stop, Timeout, Control),
        process_options(Os, TieBreak, Stats, Stop, Timeout, Control).

process_option(tiebreak(TieBreak0), TieBreak, _, _, _, _) ?-
        atom(TieBreak), !,
        var_selection(TieBreak0),
        TieBreak0 = TieBreak.
process_option(stats(Stats0), _, Stats, _, _, _) ?- !,
        (nonvar(Stats0) ->  Stats0 = gfd_stats{} ; true),
        Stats = Stats0.
process_option(backtrack(BT), _, Stats, _, _, _) ?- 
        var(BT), !,
        Stats = gfd_stats{fail:BT}.
process_option(limits(Stop0), _, _, Stop, _, _) ?-
        nonvar(Stop0),
        Stop0 = gfd_stats{}, !,
        Stop = Stop0.
process_option(nodes(N), _, _, Stop, _, _) ?- % ic/fd/gfd_search compatibility
        integer(N), N >= 0, !,
        Stop = gfd_stats{node:N}.
process_option(timeout(T), _, _, _, Timeout, _) ?-
        number(T), T >= 0, !,
        convert_timeout_to_ms(T, Timeout).
process_option(control(Control0), _, _, _, _, Control) ?-
        nonvar(Control0), 
        Control0 = gfd_control{}, !,
        Control0 = Control.


convert_timeout_to_ms(T0, T) :-
        T is integer(round(T0*1000)).

:- mode var_selection(+).
var_selection(input_order).
var_selection(first_fail).
var_selection(anti_first_fail).
var_selection(occurrence).
var_selection(anti_occurrence).
var_selection(largest).
var_selection(smallest).
var_selection(largest_uwb).
var_selection(smallest_upb).
var_selection(most_constrained).
var_selection(most_constrained_per_value).
var_selection(least_constrained_per_value).
var_selection(max_regret).
var_selection(max_regret_lwb).
var_selection(min_regret_lwb).
var_selection(max_regret_upb).
var_selection(min_regret_upb).
var_selection(random).
% newer heuristics, added since gecode 3.1
var_selection(max_weighted_degree).
var_selection(min_weighted_degree).
var_selection(max_weighted_degree_per_value).
var_selection(min_weighted_degree_per_value).


:- mode val_choice(+).
val_choice(indomain).
val_choice(indomain_from_max).
val_choice(indomain_min).
val_choice(indomain_max).
val_choice(indomain_median).
val_choice(indomain_random).
val_choice(indomain_split).
val_choice(indomain_reverse_split).
val_choice(indomain_interval).
val_choice(indomain_interval_min).
val_choice(indomain_interval_max).

:- mode translate_search_method(+, -).
translate_search_method(complete, complete).
translate_search_method(lds(D), lds(D)) :- integer(D).
translate_search_method(bb_min(CV),  bb_min(CIdx)) :-
        get_gecode_attr(CV, _, gfd{idx:CIdx}).
translate_search_method(restart_min(CV),  restart_min(CIdx)) :-
        get_gecode_attr(CV, _, gfd{idx:CIdx}).

:- mode optimising_search_method(+).
optimising_search_method(bb_min(_)).
optimising_search_method(restart_min(_)).

mark_handle_for_ancestor_update(H) :-
        setarg(events of gfd_prob, H, update).

do_search1(SP, EngH, LastSpH, InstList, ChgList, Stats) :-
        g_do_search(SP, EngH, LastSpH, InstList, ChgList, Stats, Status),
        check_search_status(Status).

check_search_status(0) ?- !, fail.  % search succeeded
check_search_status(1) ?- !.        % search failed
check_search_status(2) ?- !, abort. % search aborted with problem
check_search_status(Status) :-
        Status > 2,                 % search aborted because limit(s) reached 
        report_search_limits(Status),
        printf(log_output, "Search not completed.%n", []),
        (Status /\ 1 =:= 1 -> true/*has sol*/ ; fail/*no sol*/).

% Status here must correspond to those defined in Cutoff's enum in gfd.hpp
report_search_limits(Status) :- 
        ( Status /\  4 =:= 4 ->  % 1<<2
            printf(log_output, "Node limit reached. ", [])
        ;
            true
        ),
        ( Status /\  8 =:= 8  ->  % 1<<3
            printf(log_output, "Failure limit reached. ", [])
        ;
            true
        ),
        ( Status /\ 16 =:= 16 ->  % 1<<4
            printf(log_output, "Time limit reached. ", [])
        ;
            true
        ),
        ( Status /\ 32 =:= 32 ->  % 1<<5
            printf(log_output, "Memory limit reached. ", [])
        ;
            true
        ).
        
do_search(LastSpH, EngH, H, Method, Stats) :-
        mark_handle_for_ancestor_update(H),
        H = gfd_prob{space:SP},
        repeat,
        (do_search1(SP, EngH, LastSpH, InstList, ChgList, Stats) -> % fails if no more solution
            % a clone is created automatically by gecode's search
            % make sure an ancestor will be created on new event
%            writeln(succ:g_do_search(SP, EngH, MethodCode, LastSpH, InstList)),
            % no other solution if optimising
            ( optimising_search_method(Method) -> !  ; true), 
            H = gfd_prob{vars:VArr},
            SP = gfd_space{handle:SpH},
            propagate_gecode_changes(SpH, VArr, InstList, ChgList),
            wake % need wake here -- search is not an event
        ;
%            writeln(failed:g_do_search(SP, EngH, MethodCode, LastSpH, InstList)),
            !, fail
        ).


%------------------------------------------------------------------------
% meta support

get_min(I, Lo) :- 
        integer(I), !,
        Lo = I.
get_min(V, Lo) :-
        var(V),
        get_prob_handle(H),
        restore_space_if_needed(H, SpH),
        ( get_gecode_attr(V, H, Attr) ->
            Attr = gfd{idx:Idx},
            g_get_var_lwb(SpH, Idx, Lo)
        ;
            create_and_add_default_gfdvar(V, H),
            gfd_get_default(interval_min, Lo)
        ).

get_max(I, Hi) :- 
        integer(I), !,
        Hi = I.
get_max(V, Hi) :-
        var(V),
        get_prob_handle(H),
        restore_space_if_needed(H, SpH),
        ( get_gecode_attr(V, H, Attr) ->
            Attr = gfd{idx:Idx},
            g_get_var_upb(SpH, Idx, Hi)
        ;
            create_and_add_default_gfdvar(V, H),
            gfd_get_default(interval_max, Hi)
        ).

get_bounds(I, Lo, Hi) :-
        integer(I), !,
        Lo = I,
        Hi = I.
get_bounds(V, Lo, Hi) :-
        var(V),
        ( gfd_get_var_bounds(V, Lo, Hi) ->
            true
        ;
            % not an existing gfd var, add it
            get_prob_handle(H),
            restore_space_if_needed(H, _SpH),
            create_and_add_default_gfdvar(V, H),
            gfd_default_interval(Lo, Hi)
        ).

get_integer_bounds(V, Lo, Hi) :- % ic compatibility
        get_bounds(V, Lo, Hi).

get_finite_integer_bounds(V, Lo, Hi) :- % ic compatibility
        get_bounds(V, Lo, Hi).

get_domain(I, Dom) :-
        integer(I), !,
        Dom = [I].
get_domain(_{gfd:Attr}, Dom) ?-
        nonvar(Attr),
        Attr = gfd{prob:H, idx:Idx}, !,
        restore_space_if_needed(H, SpH),
        g_get_var_domain(SpH, Idx, Dom).
get_domain(X, Dom) :-
        error(5, get_domain(X, Dom)).


get_domain_as_list(V, DomList) :-
        get_domain(V, Dom),
        translate_domain_to_list(Dom, DomList).

  translate_domain_to_list([], DomList) ?- !,
        DomList = [].
  translate_domain_to_list([X|Xs], DomList) ?-
        ( integer(X) ->
            DomList = [X|DomList0]
        ; X = Lo..Hi ->
            ( for(I, Lo, Hi), 
              fromto(DomList, [I|DomList1],DomList1, DomList0)
            do
                true
            )
        ;
            fail
        ),
        translate_domain_to_list(Xs, DomList0).

get_domain_size(I, Size) :-
        integer(I), !,
        Size = 1.
get_domain_size(_{gfd:Attr}, Size) ?- 
        nonvar(Attr),
        Attr = gfd{prob:H, idx:Idx}, !,
        restore_space_if_needed(H, SpH),
        g_get_var_domain_size(SpH, Idx, Size).
get_domain_size(X, Size) :-
        error(5, get_domain_size(X, Size)).


get_delta(I, Width) :-
        integer(I), !,
        Width = 0.
get_delta(_{gfd:Attr}, Width) ?-
        nonvar(Attr),
        Attr = gfd{prob:H, idx:Idx},
        restore_space_if_needed(H, SpH),
        g_get_var_domain_width(SpH, Idx, Width).

get_median(I, M) :-
        integer(I), !,
        M = I.
get_median(_{gfd:Attr}, Med) ?-
        nonvar(Attr),
        Attr = gfd{prob:H, idx:Idx},
        restore_space_if_needed(H, SpH),
        g_get_var_median(SpH, Idx, Med).

get_constraints_number(T, Size) :-
        nonvar(T), !,
        Size = 1.0Inf.
get_constraints_number(V, Size) :-
        free(V), !,
        Size = 0.
get_constraints_number(_{gfd:Attr}, Size) ?-
        ( nonvar(Attr) ->
            Attr = gfd{prob:H, idx:Idx},
            restore_space_if_needed(H, SpH),
            g_get_var_degree(SpH, Idx, Size)
        ;
            Size = 0
        ).

get_weighted_degree(_{gfd:Attr}, Count) ?-
        nonvar(Attr),
        Attr = gfd{prob:H, idx:Idx},
        restore_space_if_needed(H, SpH),
        g_get_var_afc(SpH, Idx, Count).

get_regret_lwb(_{gfd:Attr}, Count) ?-
        nonvar(Attr),
        Attr = gfd{prob:H, idx:Idx},
        restore_space_if_needed(H, SpH),
        g_get_var_regret_lwb(SpH, Idx, Count).

get_regret_upb(_{gfd:Attr}, Count) ?-
        nonvar(Attr),
        Attr = gfd{prob:H, idx:Idx},
        restore_space_if_needed(H, SpH),
        g_get_var_regret_upb(SpH, Idx, Count).

impose_min(I, Min) :-
        integer(I), !,
        Min =< I.
impose_min(_{gfd:Attr}, Min) ?- 
        integer(Min),
        nonvar(Attr),
        Attr = gfd{prob:H, idx:Idx,bool:BI},
        gfdvar(Idx,BI, GV),
        post_new_event_no_wake(post_rc(default,GV #>= Min), H).

impose_max(I, Max) :-
        integer(I), !,
        integer(Max),
        Max >= I.
impose_max(_{gfd:Attr}, Max) ?- 
        integer(Max),
        nonvar(Attr),
        Attr = gfd{prob:H, idx:Idx,bool:BI},
        gfdvar(Idx,BI, GV),
        post_new_event_no_wake(post_rc(default,GV #=< Max), H).

impose_bounds(I, Min, Max) :-
        integer(I), !,
        (integer(Min) -> true ; error(5, impose_bounds(I, Min, Max))),
        (integer(Max) -> true ; error(5, impose_bounds(I, Min, Max))),
        Max >= I, Min =< I.
impose_bounds(V, Min, Max) :-
        (integer(Min) -> true ; error(5, impose_bounds(I, Min, Max))),
        (integer(Max) -> true ; error(5, impose_bounds(I, Min, Max))),
        gfd_set_var_bounds(V, Min, Max), 
        wake.

exclude(I, Excl) ?-
        integer(I), !,
        I =\= Excl.
exclude(V{gfd:Attr}, I) ?- 
        nonvar(Attr),
        (integer(I) -> true ; error(5, exclude(V, I))),
        Attr = gfd{prob:H, idx:Idx,bool:BI},
        gfdvar(Idx,BI, GV),
        post_new_event_no_wake(post_var_val_reif(GV, I, 0), H).


exclude_range(V{gfd:Attr}, Lo, Hi) ?-
        nonvar(Attr), !,
        (integer(Hi) -> true ; error(5, exclude_range(V, Lo, Hi))),
        (integer(Lo) -> true ; error(5, exclude_range(V, Lo, Hi))),
        Attr = gfd{prob:H, idx:Idx,bool:BI},
        gfdvar(Idx,BI, GV),
        post_new_event_no_wake(post_var_interval_reif(GV, Lo, Hi, 0), H).
exclude_range(X, Lo, Hi) :-
	integer(X),
	integer(Lo),
	integer(Hi),
	!,
	\+ ( Lo =< X, X =< Hi).
exclude_range(X, Lo, Hi) :-
	error(6, exclude_range(X, Lo, Hi)).


is_solver_type(I) :- integer(I), !.
is_solver_type(_{gfd:Attr}) ?- 
        nonvar(Attr),
        Attr = gfd{}.

is_solver_var(_{gfd:Attr}) ?-
        nonvar(Attr),
        Attr = gfd{}.

is_exact_solver_var(V) :-
        is_solver_var(V).

integers(V) :-
        get_prob_handle(H),
        ( var(V) ->
            ec_to_gecode_var(V, H, _)
        ;
            collection_to_list(V, VList),
            ec_to_gecode_varlist(VList, H, _)
        ).

% note these don't need a space, so no need to update handle
gfd_maxint(X) :-
        g_get_gfd_maxint(X).

gfd_minint(X) :-
        g_get_gfd_minint(X).

msg(X, Y, Dom) :-
        get_prob_handle(H),
        ( ec_to_gecode_oldvar(X, H, GX),
          ec_to_gecode_oldvar(Y, H, GY) ->
            restore_space_if_needed(H, _),
            H = gfd_prob{nvars:N0},
            % The new domain must be added to a new gfd var
            % use Dom0 in case Dom is an exising Domain var
            new_gfdvar(Dom0, H, N0,N, _GDom0),
            setarg(nvars of gfd_prob, H, N),
            % we follow Gecode's recomputation style, the
            % union will be recomputated, rather than storing it 
            post_new_event(newvars_dom_union(GX,GY,N), H),
            Dom0 = Dom
        ;
            true
        ).

is_in_domain(Val, Var) :-
        integer(Val), !,
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


%----------------------------------------------------------------------
% ic compatibility
% ordered(+Relation, List) -- naive implementation
%----------------------------------------------------------------------

:- comment(ordered/2, [
    summary:"Constrains List to be ordered according to Relation",
    amode:ordered(++,+),
    args:[
	"Relation":"One of the atoms <, =<, >, >=, =",
	"List":"Collection of integers or domain variables"
    ],
    see_also:[lexico_le/2,ordered_sum/2,sorted/2,collection_to_list/2]
    ]).

ordered(Order, Xs) :- var(Xs), !,
	suspend(ordered(Order, Xs), 4, Xs->inst).
ordered(_, []) :- !.
ordered(Order, [X1|Xs]) :- !,
	ordered1(Order, X1, Xs).
ordered(Order, Xs) :-
	collection_to_list(Xs, List),
	ordered(Order, List).

ordered1(Order, X1, Xs) :- var(Xs), !,
	suspend(ordered(Order, [X1|Xs]), 4, Xs->inst).
ordered1(_Order, _X1, []).
ordered1(Order, X1, X2Xs) :-
	X2Xs = [X2|Xs],
	ordered(Order, X1, X2),
	ordered1(Order, X2, Xs).

    ordered( <, X1, X2) :- X1 #<  X2.
    ordered(=<, X1, X2) :- X1 #=< X2.
    ordered(> , X1, X2) :- X1 #>  X2.
    ordered(>=, X1, X2) :- X1 #>= X2.
    ordered(= , X1, X2) :- X1 #=  X2.



:- erase_module(gfd_gac),
   create_constraint_pool(gfd_gac, 0, [
      (#\=)/2 -> '#\\=_c'/3,
      (#=)/2 -> '#=_c'/3,
      (#<)/2 -> '#<_c'/3,
      (#>)/2 -> '#>_c'/3,
      (#>=)/2 -> '#>=_c'/3,
      (#=<)/2 -> '#=<_c'/3,
      (and)/2 -> and_c/3,
      (or)/2 -> or_c/3,
      (xor)/2 -> xor_c/3,
      neg/1 -> neg_c/2,
      '<=>'/2 -> '<=>_c'/3,
      '=>'/2 -> '=>_c'/3,
/* currently gecode does not support gac for reified expressions    
      (#\=)/3 -> '#\\=_reif_c'/4,
      (#=)/3 -> '#=_reif_c'/4,
      (#<)/3 -> '#<_reif_c'/4,
      (#>)/3 -> '#>_reif_c'/4,
      (#>=)/3 -> '#>=_reif_c'/4,
      (#=<)/3 -> '#=<_reif_c'/4,
      (and)/3 -> and_reif_c/4,
      (or)/3 -> or_reif_c/4,
      (xor)/3 -> xor_reif_c/4,
      neg/2 -> neg_c/3,
      '<=>'/3 -> '<=>_reif_c'/4,
      '=>'/3 -> '=>_reif_c'/4, */
      abs/2 -> abs_c/3,
      plus/3 -> plus_c/4,
      mult/3 -> mult_c/4,
      sqr/2 -> sqr_c/3,
      sqrt/2 -> sqrt_c/3,
      alldifferent/1 -> alldifferent_c/2,
      alldifferent_cst/2 -> alldifferent_cst_c/3,
      bool_channeling/3 -> bool_channeling_c/4,
      count/4 -> count_c/5,
      occurrences/3 -> occurrences_c/4,
      atmost/3 -> atmost_c/4,
      atleast/3 -> atleast_c/4,
      element/3 -> element_c/4,
      element_g/3 -> element_g_c/4,
      circuit/1 -> circuit_c/2,
      circuit/3 -> circuit_c/4,
      circuit/4 -> circuit_c/5,
      circuit_g/1 -> circuit_g_c/2,
      circuit_g/3 -> circuit_g_c/4,
      circuit_g/4 -> circuit_g_c/5,
      gcc/2 -> gcc_c/3,
      inverse/2 -> inverse_c/3,
      inverse_g/2 -> inverse_g_c/3,
      inverse/4 -> inverse_c/5,
      inverse_g/4 -> inverse_g_c/5,
      minlist/2 -> minlist_c/3,
      maxlist/2 -> maxlist_c/3,
      min/2 -> minlist_c/3,
      max/2 -> maxlist_c/3,
      sum/2 -> sum_c/3,
      sum/3 -> sum_c/4,
      scalar_product/4 -> scalar_product_c/5,
      sequence/4 -> sequence_c/5,
      sequence/5 -> sequence_c/6
   ]).

:- erase_module(gfd_bc),
   create_constraint_pool(gfd_bc, 0, [
      (#\=)/2 -> '#\\=_c'/3,
      (#=)/2 -> '#=_c'/3,
      (#<)/2 -> '#<_c'/3,
      (#>)/2 -> '#>_c'/3,
      (#>=)/2 -> '#>=_c'/3,
      (#=<)/2 -> '#=<_c'/3,
      (and)/2 -> and_c/3,
      (or)/2 -> or_c/3,
      (xor)/2 -> xor_c/3,
      neg/1 -> neg_c/2,
      '<=>'/2 -> '<=>_c'/3,
      '=>'/2 -> '=>_c'/3,
      (#\=)/3 -> '#\\=_reif_c'/4,
      (#=)/3 -> '#=_reif_c'/4,
      (#<)/3 -> '#<_reif_c'/4,
      (#>)/3 -> '#>_reif_c'/4,
      (#>=)/3 -> '#>=_reif_c'/4,
      (#=<)/3 -> '#=<_reif_c'/4,
      (and)/3 -> and_reif_c/4,
      (or)/3 -> or_reif_c/4,
      (xor)/3 -> xor_reif_c/4,
      neg/2 -> neg_c/3,
      '<=>'/3 -> '<=>_reif_c'/4,
      '=>'/3 -> '=>_reif_c'/4,
      abs/2 -> abs_c/3,
      plus/3 -> plus_c/4,
      mult/3 -> mult_c/4,
      divide/3 -> div_c/4,
      mod/3 -> mod_c/4,
      divmod/4 -> divmod_c/5,
      sqr/2 -> sqr_c/3,
      sqrt/2 -> sqrt_c/3,
      alldifferent/1 -> alldifferent_c/2,
      alldifferent_cst/2 -> alldifferent_cst_c/3,
      element/3 -> element_c/4,
      element_g/3 -> element_g_c/4,
      gcc/2 -> gcc_c/3,
      minlist/2 -> minlist_c/3,
      maxlist/2 -> maxlist_c/3,
      min/2 -> minlist_c/3,
      max/2 -> maxlist_c/3,
      sum/2 -> sum_c/3,
      sum/3 -> sum_c/4,
      sum/4 -> sum_reif_c/5,
      scalar_product/4 -> scalar_product_c/5,
      scalar_product/5 -> scalar_product_reif_c/6,
      sorted_g/3 -> sorted_g_c/4,
      sorted/2 -> sorted_c/3,
      sorted/3 -> sorted_c/4
  ]).

:- erase_module(gfd_vc),
   create_constraint_pool(gfd_vc, 0, [
      alldifferent/1 -> alldifferent_c/2,
      alldifferent_cst/2 -> alldifferent_cst_c/3,
      bool_channeling/3 -> bool_channeling_c/4,
      circuit/1 -> circuit_c/2,
      circuit/3 -> circuit_c/4,
      circuit/4 -> circuit_c/5,
      circuit_g/1 -> circuit_g_c/2,
      circuit_g/3 -> circuit_g_c/4,
      circuit_g/4 -> circuit_g_c/5,
      cumulative/4 -> cumulative_c/5,
      cumulatives/5 -> cumulatives_c/6,
      cumulatives_min/5 -> cumulatives_min_c/6,
      cumulatives_g/5 -> cumulatives_g_c/6,
      cumulatives_min_g/5 -> cumulatives_min_g_c/6,
      inverse/2 -> inverse_c/3,
      inverse_g/2 -> inverse_g_c/3,
      inverse/4 -> inverse_c/5,
      inverse_g/4 -> inverse_g_c/5,
      gcc/2 -> gcc_c/3
  ]).


:- comment(include, "gfd_comments.ecl").
