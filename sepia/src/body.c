/* BEGIN LICENSE BLOCK
 * Version: CMPL 1.1
 *
 * The contents of this file are subject to the Cisco-style Mozilla Public
 * License Version 1.1 (the "License"); you may not use this file except
 * in compliance with the License.  You may obtain a copy of the License
 * at www.eclipse-clp.org/license.
 * 
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.  See
 * the License for the specific language governing rights and limitations
 * under the License. 
 * 
 * The Original Code is  The ECLiPSe Constraint Logic Programming System. 
 * The Initial Developer of the Original Code is  Cisco Systems, Inc. 
 * Portions created by the Initial Developer are
 * Copyright (C) 1989-2006 Cisco Systems, Inc.  All Rights Reserved.
 * 
 * Contributor(s): 
 * 
 * END LICENSE BLOCK */

/*
 * VERSION	$Id: body.c,v 1.1 2006/09/23 01:55:52 snovello Exp $
 */

/*
 * IDENTIFICATION		body.c
 *
 * DESCRIPTION
 *
 * Here are the routines to generate the abstract instructions for the
 * arguments of body subgoals.
 *
 *
 * CONTENTS:
 *
 * AUTHOR	VERSION	 DATE	REASON
 * Micha Meier	1.0		created the file
 * Micha Meier	2.2	20.7.89	rewritten for the new compiler
 */

/*
 * INCLUDES:
 */

#include 	"config.h"
#include	"sepia.h"
#include	"types.h"
#include        "embed.h"
#include	"error.h"
#include	"mem.h"
#include	"io.h"
#include	"dict.h"
#include	"opcode.h"
#include	"compiler.h"
#include	"gencode.h"
#include 	"emu_export.h"
#include	"module.h"
#include	"database.h"
#include	"debug.h"

/*
 * DEFINES:
 */
#define Puts_Module(module) 	Puts_Constant(ModuleTag(module), module)
#define Put_Module(module, arg)	Put_Constant(ModuleTag(module), module,\
					(arg)+1);
#define MAX_INIT_GAP		4	/* maximum 0's gap in the init mask */

#define MetaAttributePred(wd)	\
	    ((wd) == d_insert_susp4_ || (wd) == d_insert_susp3_ || \
	    (wd) == d_add_attribute2_ || (wd) == d_add_attribute3_ || \
	    (wd) == d_replace_attribute2_ || (wd) == d_replace_attribute3_ || \
	    (wd) == d_get_attribute2_ || (wd) == d_get_attribute3_ || \
	    (wd) == d_get_attributes4_)

/*
 * EXTERNAL VARIABLE DECLARATIONS:
 */
extern vmcode	    *get_structure(),
		    *compiler_error();
extern void	    free_sources();
pword	    	    *copy_ground_structure(proc_desc *procedure, value v, type t);
extern pword	    woken_susp_;

/*
 * EXTERNAL VARIABLE DEFINITIONS:
 */
vmcode		    *put_arguments(pword *goal, cl_desc *clds, proc_desc *procedure, register vmcode *code, int last, dident module),
		    *put_det_arguments(pword *goal, cl_desc *clds, proc_desc *procedure, vmcode *code, dident module),
		    *put_structure(pword *str, cl_desc *clds, proc_desc *procedure, int *str_mode, register vmcode *code),
		    *ec_put(pword *arg, cl_desc *clds, proc_desc *procedure, register vmcode *code, long int i, int last);

void		    reset_args(arg_desc *gargs);

/*
 * STATIC VARIABLE DEFINITIONS:
 */

static vmcode	    *_puts(pword *arg, cl_desc *clds, proc_desc *procedure, int *str_mode, register vmcode *code);
/*
 * FUNCTION DEFINITIONS:
 */

/*
 * FUNCTION NAME:
 *
 * PARAMETERS:
 *
 * DESCRIPTION:
 *
 * Generate the Put instructions for the first regular goal in the body.
 * There are various optimization possibilities when moving the information
 * from the head to the body, as opposed to the instructions for subsequent
 * body subgoals.
 */
vmcode             *
put_first_arguments(cl_desc *clds, proc_desc *procedure, register vmcode *code, int last, dident module)
{
    arg_desc		*gargs = procedure->gargs;
    int			*head_pass = procedure->head_pass;
    arg_desc		*ards;
    var_desc		*tvv;
    struct var_ch	*dest;
    pword		*arg;
    int                 arity = clds->reg_arity;
    long                i = 1;
    long		shared_pos = 0;
    long		saved_pos = 0;
    long		loc;
    pword		var;		/* to have an arg for ec_put()	*/
    int			moved = 0;	/* did we do a move?		*/
    int			loop = 0;	/* second and further pass	*/
    long		init_mask;	/* mask for Initialize		*/
    long		init_pos;	/* mask bit			*/
    int			init_gap;	/* how many consecutive 0's	*/
    long		init_first = 0;	/* first to initialize		*/
    vmcode		*saved_code;
    vmcode		*init_code;

    if (arity < DidArity(procedure->did))
	arity = DidArity(procedure->did);
    /*
     * First finish the head unification and put the nonground arguments
     * of the first regular goal.
     */
    if (head_pass[VAR] || head_pass[STRUCTURE])
    for (;;)
    {
	if (head_pass[VAR])
	{
	    moved = 0;
	    do
	    {
		ards = gargs + i;
		if (!ArgDone(ards) && (arg = ards->target) && IsTvv(arg->tag))
		{
		    if (ards->contents && IsTvv(ards->contents->tag))
		    {
			tvv = Tvv(ards->contents->val);
			if (VarPermanent(tvv))
			{
			    if (!VarAllocated(tvv))
			    {
				Assert(IsA(VarSource(tvv)));
				Buf_Alloc(procedure->codebuf, code, L_GET);
				Get_VariableL(tvv, ArgNo(VarSource(tvv)));
				Set_Var_Allocated(tvv);
			    }
			    remove_shared(procedure, i);
			    Set_Arg_Free(ards);
			}
		    }
		    if (ArgFree(ards))
		    {
			tvv = Tvv(arg->val);
			loc = VarSource(tvv);
			Assert(loc);
			code = ec_put(arg, clds, procedure, code, i, last);
			free_sources(tvv, procedure);
			Set_Var_Source(tvv, ContArg(i));
			Set_Arg_Done(ards);
			ards->target = 0;
			moved++;
			if (--head_pass[VAR] == 0)
			    break;
			if (i < arity && (saved_pos == 0 || saved_pos > i))
			    saved_pos = i + 1;
			if (IsA(loc))
			    i = ArgNo(loc);
			else
			    i++;
		    }
		    else
		    {
			if (!shared_pos && ArgShared(ards))
			    shared_pos = i;
			i++;
		    }
		}
		else
		    i++;
	    } while (i <= arity);
	    if (saved_pos)
	    {
		i = saved_pos;
		saved_pos = 0;
		continue;
	    }
	}
	if (head_pass[VAR] && shared_pos)
	{
	    /* Make a move to a shared argument */
	    i = shared_pos;
	    shared_pos = 0;
	    Set_Arg_Free(gargs + i);
	    remove_shared(procedure, i);
	    continue;
	}
	else if (head_pass[STRUCTURE] && (!loop || moved))
	{
	    loop++;
	    ards = gargs;
	    moved = 0;
	    for (i = 1; i <= arity; i++)
	    {
		if (ArgOccupied(++ards) && (arg = ards->contents) &&
			(IsCompound(arg->tag) ||
			IsFirstMetaTvv(arg->val, arg->tag)))
		{
		    /* Not testing ArgDone as it might be KEEP	*/
		    if (OutputMode(Mode(i, procedure->mode)))
		    {
			code = get_structure(arg, clds, procedure,
			    code, i, 0L, OUTPUT, 0, ClauseMatching(clds));
			moved++;
			if (!--head_pass[STRUCTURE])
			    break;
		    }
		    /* else it is a structure which was already unified */
		}
		else if (!ArgDone(ards) && (arg = ards->target) &&
			(IsCompound(arg->tag) && !ArgBodyGround(ards) ||
			IsTvv(arg->tag) && IsMeta(Tvv(arg->val)->header.tag)))
		{
		    if (ards->contents && IsTvv(ards->contents->tag))
		    {
			tvv = Tvv(ards->contents->val);
			if (VarPermanent(tvv))
			{
			    if (!VarAllocated(tvv))
			    {
				Assert(IsA(VarSource(tvv)));
				Buf_Alloc(procedure->codebuf, code, L_GET);
				Get_VariableL(tvv, ArgNo(VarSource(tvv)));
				Set_Var_Allocated(tvv);
			    }
			    Set_Var_Perm_Source(tvv);
			    Set_Arg_Free(ards);
			}
		    }
		    if (ArgShared(ards))
			shared_pos = i;
		    else if (ArgFree(ards))
		    {
			code = ec_put(arg, clds, procedure, code, i, last);
			Set_Arg_Done(ards);
			ards->target = 0;
			moved++;
			if (!--head_pass[STRUCTURE])
			    break;
		    }
		}
	    }
	    if (moved)
	    {
		i = 1;
		continue;	/* try again to move a variable	*/
	    }
	    else if (shared_pos)
	    {
		Set_Arg_Free(gargs + shared_pos);
		remove_shared(procedure, shared_pos);
		shared_pos = 0;
		moved = 1;
		continue;	/* a goto would be better... */
	    }
	}
	if (!head_pass[VAR] && !head_pass[STRUCTURE])
	    break;		/* we have finished the difficult part	*/
	/* We have a conflict. Move one variable to a Ti */
	ards = gargs;
	for (i = 1; i <= arity; i++)
	{
	    if (!ArgDone(++ards) && (arg = ards->target) &&
		(IsTvv(arg->tag) ||
		    !head_pass[VAR] &&
		    IsCompound(arg->tag) && !ArgBodyGround(ards)))
	    {
		reg_desc		*regs = procedure->regs;

		Assert(ArgOccupied(ards));
		if (RegFree(regs, Rt1))
		{
		    Set_Reg_Occupied(regs, Rt1);
		    loc = Rt1;
		}
		else
		{
		    loc = ContTmp(++(clds->pushed));
		}
		Assert(IsTvv(ards->contents->tag));
		Set_Var_Source(Tvv(ards->contents->val), loc);
		Buf_Alloc(procedure->codebuf, code, L_GET);
		Get_VariableT(loc, clds->pushed, i);
		Set_Arg_Free(ards);
		moved++;
		break;
	    }
	}
	Assert(moved);
    }

    /*
     * Put the remaining permanent variables into Yi and fetch the
     * rest of the goal arguments.
     */
    ards = gargs;
    for (i = 1; i <= arity; i++)
    {
	if ((++ards)->contents && IsTvv(ards->contents->tag))
	{
	    tvv = Tvv(ards->contents->val);
	    if (VarPermanent(tvv) && !VarAllocated(tvv))
	    {
		Assert(IsA(VarSource(tvv)));
		Buf_Alloc(procedure->codebuf, code, L_GET);
		Get_VariableL(tvv, ArgNo(VarSource(tvv)));
		Set_Var_Allocated(tvv);
		Set_Var_Perm_Source(tvv);
	    }
	}
	if (!ArgDone(ards))
	{
	    if (ards->target)
	    {
		code = ec_put(ards->target, clds, procedure, code, i, last);
		ards->target = 0;
	    }
	}
    }
    if (module != D_UNKNOWN) {
	Buf_Alloc(procedure->codebuf, code, L_PUT);
	Put_Module(module, clds->reg_arity);
    }

    tvv = clds->variables;
    var.tag.all = TTVV;
    while (tvv)
    {
	if (VarDestination(tvv) && !VarBody(tvv))
	{
	    var.val.ptr = (pword *) tvv;
	    dest = &tvv->destination;
	    do
	    {
		Assert(IsA(dest->cont));
		code = ec_put(&var, clds, procedure, code, ArgNo(dest->cont),
			last);
		dest = dest->next;
	    } while (dest && dest->cont);
	    VarDestination(tvv) = 0;
	}
	tvv = tvv->next_tvv;
    }
    tvv = clds->variables;
    Buf_Alloc(procedure->codebuf, code, L_INIT+L_WORD);
    init_code = code;
    code += 3;		/* space for Initialize */
    while (tvv)
    {
	if (VarPermanent(tvv))
	{
	    if (!VarAllocated(tvv)) /* or !VarPrefix(tvv) ?? */
	    {
		init_gap = 0;
		if (!init_first)
		{
		    init_first = VarPermOff(tvv);
		    init_mask = 0;
		    init_pos = 1;
		    if (GlobalFlags & VARIABLE_NAMES)
		    {
			Buf_Alloc(procedure->codebuf, code, L_WORD);
			if (IsName(tvv->header.tag)) {
			    Store_d(tvv->header.tag.kernel);
			}
			else {
			    Store_d(TREF);
			}
		    }
		}
		else
		{
		    init_mask |= init_pos;
		    /* limit the mask to lower 32 bits so that compatible code
                       would be generated for 32 and 64 wordlengths
		    */
		    init_pos = (init_pos << 1) & (unsigned) 0xffffffff;
		    if (GlobalFlags & VARIABLE_NAMES)
		    {
			Buf_Alloc(procedure->codebuf, code, L_WORD);
			if (IsName(tvv->header.tag)) {
			    Store_d(tvv->header.tag.kernel);
			    Set_Var_Global(tvv)
			}
			else {
			    Store_d(TREF);
			}
		    }
		    if (init_pos == 0)	/* overflow */
		    {
			Buf_Alloc(procedure->codebuf, code, L_INIT);
			saved_code = code;
			code = init_code;
			Initialize_(init_first, init_mask);
			init_code = saved_code;
			code = init_code + 3;
			init_first = 0;
		    }
		}
		Assert(!VarSource(tvv));
		Assert(ClauseAllocated(clds));
		Set_Var_Perm_Source(tvv);
		Set_Var_Allocated(tvv);
		Set_Var_Uninit(tvv);	/* actually 'first occ' */
		Set_Var_Init(tvv);
		if (IsName(tvv->header.tag)) {
		    Set_Var_Global(tvv)
		}
	    }
	    else
	    {
		if (init_first)
		{
		    /* limit the mask to lower 32 bits so that compatible code
                        would be generated for 32 and 64 wordlengths
		    */
		    init_pos = (init_pos << 1) & (unsigned) 0xffffffff;
		    if (++init_gap > MAX_INIT_GAP || init_pos == 0)
		    {
			Buf_Alloc(procedure->codebuf, code, L_INIT);
			saved_code = code;
			code = init_code;
			Initialize_(init_first, init_mask);
			init_code = saved_code;
			code = init_code + 3;
			init_first = 0;
		    }
		}
		Set_Var_Perm_Source(tvv);
	    }
	    tvv->counter = 0;
	}
	tvv = tvv->next_tvv;
    }
    if (init_first)
    {
	saved_code = code;
	code = init_code;
	Initialize_(init_first, init_mask);
	code = saved_code;
    }
    else
	code = init_code;
    reset_args(gargs);
    return code;
}

/*
 * FUNCTION NAME:
 *
 * PARAMETERS:
 *
 * DESCRIPTION:
 */
/* generate the instructions for the subsequent subgoal's arguments */
vmcode             *
put_arguments(pword *goal, cl_desc *clds, proc_desc *procedure, register vmcode *code, int last, dident module)
{
    long                 i;
    type                tag;
    int                 arity = GoalArity(goal);
    arg_desc		*ards = procedure->gargs;


    /* first the compound arguments */
    for (i = 1; i <= arity; i++)
    {
	ards++;
	if ((tag.all = (goal + i)->tag.all, IsCompound(tag)) && !ArgDone(ards))
	{
	    code = ec_put(goal + i, clds, procedure, code, i, last);
	    Set_Arg_Done(ards)
	}
    }

    /* and now the rest */
    ards = procedure->gargs;
    for (i = 1; i <= arity; i++)
    {
	ards++;
	if ((tag.all = (goal + i)->tag.all, !IsCompound(tag)) && !ArgDone(ards))
	{
	    code = ec_put(goal + i, clds, procedure, code, i, last);
	    Set_Arg_Done(ards)
	}
    }
    if (module != D_UNKNOWN) {
	Buf_Alloc(procedure->codebuf, code, L_PUT);
	Put_Module(module, arity);
    }
    reset_args(procedure->gargs);
    return code;
}

/*
 * FUNCTION NAME:
 *
 * PARAMETERS:
 *
 * DESCRIPTION:
 *
 * Generate the instructions for the last goal which is regular and the state
 * is known to be deterministic. Not quite sure how this can
 * be used - put_unsafe_value does not have to test
 */
vmcode             *
put_det_arguments(pword *goal, cl_desc *clds, proc_desc *procedure, vmcode *code, dident module)
{
    return put_arguments(goal, clds, procedure, code, 1, module);
}

/*
 * FUNCTION NAME:	ec_escape()
 *
 * PARAMETERS:
 *
 * DESCRIPTION:
 *
 * Generate the code for simple subgoals. It might be a built-in procedure,
 * an external non-backtracking procedure or an in-line expanded predicate.
 * Depending of the type of the variable and argument, some actions
 * on occur check, waking or in delay clauses are necessary.
 * For some arguments this is decidable immediately (U_SIMPLE),
 * for others all arguments have to be processed first. The former
 * are done with the cl_vars flag, the latter with other cl_* flags.
 * Note that for predicates that unify more than one pair of arguments
 * (this currently occurs only in externals) the action is approximated.
 */
vmcode             *
ec_escape(pri *procindex, pword *goal, dident gval, cl_desc *clds, proc_desc *procedure, register vmcode *code, int where, int gflags)
{
    arg_desc		*gargs = procedure->gargs;
    long		call_port;
    int			maywake = (UnifType(procindex) >= U_SIMPLE);
    extern dident	d_insert_susp4_, d_insert_susp3_;
    extern dident	d_add_attribute2_, d_add_attribute3_;
    extern dident	d_get_attribute2_, d_get_attribute3_;
    extern dident	d_get_attributes4_;
    extern dident	d_replace_attribute2_, d_replace_attribute3_;
    extern vmcode	*get_constant();
    dident		module;
    var_desc		*tvv;
    int                 arity = GoalArity(goal);
    long		i;
    uint32		bind_mode = procindex->mode;
    int			u_type = UnifType(procindex);
    int			cl_first = 0;
    int			cl_nonref = 0;
    int			cl_bind = 0;
    int			cl_ground = 0;
    int			cl_uninit = 0;
    int			cl_vars = 0;
    int			bound_arg;
    int			bound_arg_no = 0;
    pword		*arg;
    int			str_mode;
    int			extrn = 0; /* used to test EXTERN flag, now obsolete */
    int			functor_ground = 0;
    int			ignored = 0;

    module = ToolProc(procindex) ? procedure->module : D_UNKNOWN;
    call_port = CALL_PORT |
		(GFirst(gflags) ? FIRST_CALL : 0) |
		(GLast(gflags) || GLastn(gflags) ? LAST_CALL : 0);

    /* first check for the expanded built-ins */
    if (procindex->module_ref == d_.kernel_sepia)
    {
	if (ProcedureExpand(procedure) && procindex->did == d_.unify)
	{
	    /*
	     * If it is X = * , it will be unified with Get_value
	     * unless there no Ai's to put them in or if one of the
	     * arguments is a metaterm (clash of get_meta and bind_c)
	     */
	    if ((int) clds->reg_arity + 3 < MAXARITY) {
		if (IsTvv(goal[1].tag)) {
		    if (!IsMeta(Tvv(goal[1].val)->header.tag) &&
			    !(IsTvv(goal[2].tag) &&
			    IsMeta(Tvv(goal[2].val)->header.tag)))
			goto _unify_;
		} else if (IsTvv(goal[2].tag) &&
			!IsMeta(Tvv(goal[2].val)->header.tag))
		    goto _unify_;
	    }
	}
	/* we expand only make_susp/3, because the fourth arg is ambiguous -
	  it can be either the lookup module for the procedure or the
	  caller module of the tool goal
	 */
	else if (procindex->did == d_.make_suspension &&
		IsStructure(goal[1].tag))
	{
	    pri		*proc;

	    proc = visible_procedure(goal[1].val.ptr->val.did,
		procedure->module, procedure->module_tag, PRI_CREATE|PRI_REFER);
	    if (proc) {
		Buf_Alloc(procedure->codebuf, code, L_WORD);
		Puts_Proc(proc)
		(clds->pushed)++;
		module = D_UNKNOWN;		/* no additional argument */
	    }
	}
	else if (MetaAttributePred(procindex->did))
	{
	    int		slot;

	    if (ToolProc(procindex)) {
		if (slot = meta_index(procedure->module)) {
		    Buf_Alloc(procedure->codebuf, code, L_WORD);
		    Puts_Integer(slot);
		    (clds->pushed)++;
		    module = D_UNKNOWN;		/* no additional argument */
		}
	    }
	    else if (IsAtom(goal[arity].tag)) {
		if (slot = meta_index(goal[arity].val.did)) {
		    if (ClauseTrail(clds))
		    {
			Trail_Pword(goal + arity);
		    }
		    goal[arity].tag.all = TINT;
		    goal[arity].val.nint = slot;
		}
	    }
	}
	else if (procindex->did == d_.arg && IsInteger(goal[1].tag) &&
		goal[1].val.nint == 1 &&
		ProcedureExpand(procedure))
	{
	    long	loc2, loc3;

	    if (IsTvv(goal[2].tag) && IsTvv(goal[3].tag)) {
		loc2 = VarSource(Tvv(goal[2].val));
		if (IsA(loc2) && ArgNo(loc2) == procedure->index &&
			clds->desc_tag.kernel == TagMask(TCOMP))
		{
		    tvv = Tvv(goal[3].val);
		    loc3 = VarSource(tvv);
		    if (IsA(loc3)) {
			Buf_Alloc(procedure->codebuf, code, L_GET+L_GET);
			Get_Structure_Arguments(ArgNo(loc2));
			Store_2d(Read_valueAM, Address(ArgNo(loc3)));
			return code;
		    }
		    else if (loc3 == 0 && !VarPermanent(tvv)) {
			Buf_Alloc(procedure->codebuf, code, L_GET+L_GET);
			Get_Structure_Arguments(ArgNo(loc2));
			loc3 = free_arg(tvv, clds, procedure);
			Store_2d(Read_variableAM, Address(loc3));
			Set_Var_Source(tvv, ContArg(loc3));
			return code;
		    }
		}
	    }
	}
    }
    if (module != D_UNKNOWN)	       /* add the module argument, must be
				          global like goal */
    {
	Buf_Alloc(procedure->codebuf, code, L_WORD);
	Puts_Module(module);
	(clds->pushed)++;
    }
    /* generate the instructions */
    for (i = arity; i; i--)
    {
	arg = goal + i;
	bound_arg = extrn ? NONVAR : ArgBinding(i, bind_mode);
	str_mode = 0;
	if (bound_arg == NONVAR)	/* unification with another arg */
	{
	    if (IsTvv(arg->tag)) {
		tvv = Tvv(arg->val);
		if (!VarSource(tvv))
		    cl_first++;
		else if (VarGround(tvv))
		    cl_ground++;
		else if (VarUninit(tvv)) {
		    cl_uninit++;
		    Reset_Var_Uninit(tvv)
		}
		else if (VarNoalias(tvv))
		    cl_nonref++;
		else
		    cl_bind++;
	    }
	    else if (!IsCompound(arg->tag))
		cl_ground++;
	    bound_arg_no++;
	} else if (bound_arg) {		/* grounding */
	    if (IsTvv(arg->tag)) {
		tvv = Tvv(arg->val);
		if (!VarSource(tvv) || VarGround(tvv))
		    ;
		else if (VarUninit(tvv)) {
		    if (cl_vars < CL_DCOND)
			cl_vars = CL_DCOND;
		}
		else if (cl_vars < CL_WAKE)
		    cl_vars = CL_WAKE;
		if (!GDisj(where)) {
		    Set_Var_Ground(tvv)
		}
	    }
	    else if (procindex->did == d_.functor && IsSimple(arg->tag))
		functor_ground++;
	}
	code = _puts(arg, clds, procedure, &str_mode, code);
	if (IsCompound(arg->tag)) {
	    if (bound_arg == NONVAR) {
		if (str_mode & NONVAR)
		    cl_bind++;
		else if (str_mode & OUTPUT)
		    cl_nonref++;
		else
		    cl_ground++;
	    } else if (bound_arg) {
		if (str_mode & NONVAR) {
		    if (cl_vars < CL_WAKE)
			cl_vars = CL_WAKE;
		}
		else if (str_mode & OUTPUT) {
		    if (cl_vars < CL_DCOND)
			cl_vars = CL_DCOND;
		}
	    }
	}
    }
    if (u_type == U_GLOBAL)
	cl_bind++;
    if (u_type <= U_GROUND) {	/* no unification */
	if (bound_arg_no) {	/* bidirectional preds */
	    if (cl_uninit || cl_first > 1) {
		Set_Clause_Vars(CL_DCOND, clds);
	    } else if (cl_first || cl_bind == 0 && cl_nonref == 0) {
		Set_Clause_Vars(0, clds);
	    } else {
		Set_Clause_Vars(CL_WAKE, clds);
	    }
	}
	else {
	    Set_Clause_Vars(cl_vars, clds);
	}
    }
    else if (procindex->did == d_.functor) {
	/* functor/3 must have a special combination */
	if (cl_first) {
	    Set_Clause_Vars(0, clds);
	} else if (cl_uninit) {
	    Set_Clause_Vars(CL_DCOND, clds);
	} else if (!cl_ground && functor_ground == 2) {	/* (T, f, 3) */
	    Set_Clause_Vars(CL_WAKE, clds);
	} else {
	    Set_Clause_Vars(cl_vars, clds);
	}
    } else if (cl_first) {
	Set_Clause_Vars(0, clds);
    } else if (cl_bind > 1) {
	Set_Clause_Vars(CL_OC, clds);
    } else if (cl_bind && (cl_nonref || cl_ground)) {
	Set_Clause_Vars(CL_WAKE, clds);
    } else if (bound_arg_no == 1 && u_type == U_UNIFY || extrn) {
	if (cl_bind) {
	    Set_Clause_Vars(CL_OC, clds);
	} else if (cl_nonref) {
	    Set_Clause_Vars(CL_WAKE, clds);
	} else if (cl_uninit) {
	    Set_Clause_Vars(CL_DCOND, clds);
	}
    } else if (cl_uninit || cl_nonref) {
	Set_Clause_Vars(CL_DCOND, clds);
    }

    Buf_Alloc(procedure->codebuf, code, L_DB+L_DPORT+L_OC+L_ESC+L_GET);
    switch (PriCodeType(procindex))
    {
    case BIPNO:
_unify_:
	/* in this case, directly _put the opcode */
	if (ProcedureExpand(procedure)
	   && procindex->did == d_.unify 
	   && procindex->module_ref == d_.kernel_sepia
	   )
	{
	    var_desc	*tvva;
	    long	loc;
	    long	move = 0;

	    arg = 0;
	    if (HeadState(where) && IsTvv(goal[1].tag) &&
		IsA(loc = VarSource(tvva = (var_desc *) goal[1].val.ptr)))
		arg = goal + 2;
	    else if (HeadState(where) && IsTvv(goal[2].tag) &&
		IsA(loc = VarSource(tvva = (var_desc *) goal[2].val.ptr)))
		arg = goal + 1;
	    else
	    {
		/* One of the two arguments must be moved into an Ai
		 * so that we can use a get* instruction
		 */
		if (IsTvv(goal[1].tag))
		{
		    arg = goal + 2;
		    tvva = (var_desc *) goal[1].val.ptr;
		    if (IsR(VarSource(tvva)) && IsTvv(goal[2].tag))
		    {
			arg = goal + 1;
			tvva = (var_desc *) goal[2].val.ptr;
		    }
		}
		else if (IsTvv(goal[2].tag))
		{
		    arg = goal + 1;
		    tvva = (var_desc *) goal[2].val.ptr;
		}
		if (arg)
		{
		    if (BodyState(where)) {
			Set_Arg_Free(gargs + 1);
		    }
		    move = free_arg(tvva, clds, procedure);
		}
	    }
	    /* tvva is the variable, and arg the other term */
	    if (!arg ||		/* both are nonvariables */
		IsMeta(tvva->header.tag) ||	/* one is a metaterm */
		IsTvv(arg->tag) && IsMeta(Tvv(arg->val)->header.tag))
	    {
		if (ProcedureDebug(procedure) && !InvisibleProc(procindex))
		{
		    Store_3d(Debug_esc, procindex, call_port);
		}
		if (ClauseVars(clds) == CL_OC) {
		    Occur_Check_Next(0)
		}
		Store_d(Escape);
		Store_d(procindex);
		if (ProcedureDebug(procedure) && !InvisibleProc(procindex))
		{
		    Store_3d(Debug_esc, procindex, EXIT_PORT);
		}
	    }
	    else if (!VarVoid(tvva) || IsMeta(tvva->header.tag))
	    {
		/*
		 * Expand the =/2 predicate
		 */
		int		delcl = ProcedureDelcl(procedure);
		long		index;

		i = ArgNo(loc);
#if 0
		if (ProcedureDebug(procedure) && !InvisibleProc(procindex))
		{
		    Store_3d(Debug_esc, procindex, call_port | NO_ARGS);
		}
#endif
		if (move)
		{
		    /* The variable must be first moved into an Ai */
		    loc = VarSource(tvva);
		    if (ArgNo(VarDestination(tvva)) == move)
		    {
			Set_Arg_Done(gargs + move);
			Add_Var_Source(tvva, ContArg(move));
			Remove_Cont(&(tvva->destination));
		    }
		    if (IsT(loc))
		    {
			Put_ValueT(NumberOf(loc), clds->pushed, move);
		    }
		    else if (VarAllocated(tvva))
		    {
			Put_ValueL(tvva, move);
			Reset_Var_Init(tvva);
		    }
		    else
		    {
			/* first occurrence of a temporary, use Put instead */
			Assert(!loc);
			code = ec_put(arg, clds, procedure, code, move, 0);
			Buf_Alloc(procedure->codebuf, code, L_GET);
			if (VarPermanent(tvva))
			{
			    Get_VariableL(tvva, move);
			    Set_Var_Perm_Source(tvva);
			    Set_Var_Allocated(tvva);
			}
			else if (!ArgDone(gargs + move))
			{
			    loc = ContTmp(++(clds->pushed));
			    Get_VariableT(loc, clds->pushed, move);
			    Set_Var_Source(tvva, loc);
			}
			maywake = 0;
			break;
		    }
		    i = move;
		}
		if (IsCompound(arg->tag) || IsFirstMetaTvv(arg->val, arg->tag))
		{
		    arg_desc		*ards = gargs + i;
		    long		save_state = ards->state;
		    int			mode;
		    int			wake;

		    if (move) {
			if (VarSource(tvva)) {
			    if (VarGround(tvva))
				mode = GROUND;
			    else if (!VarUninit(tvva))
				mode = ANY;
			    else {
				mode = OUTPUT;
				Reset_Var_Uninit(tvva)
			    }
			}
			else
			    mode = OUTPUT;
		    } else if (delcl)
			mode = NONVAR;
		    else
			mode = Mode(i, procedure->mode);
		    if (i == procedure->index && where == Head &&
			clds->indexed_entry == code - LABEL_SIZE)
			index = i;
		    else
			index = 0;
		    wake = ClauseWillwake(clds);
		    Reset_Clause_Willwake(clds);
		    code = get_structure(arg, clds, procedure, code, i, index,
				mode , 0, 0L);
		    /* if ards is set to free, we have to undo this */
		    if (ArgFree(ards))
			ards->state = save_state;
		    if (ClauseWillwake(clds)) {
			if (mode == ANY || mode == NONVAR) {
			    Set_Clause_Vars(CL_OC, clds)
			} else if (mode != OUTPUT && mode != GROUND) {
			    Set_Clause_Vars(CL_WAKE, clds)
			}
		    }
		    else if (mode == ANY) {
			Set_Clause_Vars(CL_WAKE, clds)
		    } else if (wake) {
			Set_Clause_Willwake(clds)
		    }
		    Buf_Alloc(procedure->codebuf, code, L_DB);
		}
		else if (IsTvv(arg->tag))	/* both vars */
		{
		    tvv = Tvv(arg->val);
		    if (VarVoid(tvv) && !IsMeta(tvv->header.tag) ||
			    (tvv == tvva))
			;
		    else if (loc = VarSource(tvv))
		    {

			if (VarOnlyGround(tvva) && VarOnlyGround(tvv)){
			    Set_Clause_Vars(0, clds);
			}
			else if (VarGround(tvva) && !VarNoalias(tvv) ||
				VarGround(tvv) && !VarNoalias(tvva)) {
			    Set_Clause_Vars(CL_WAKE, clds);
			}
			else if (VarNoalias(tvva) || VarNoalias(tvv)) {
			    Set_Clause_Vars(CL_DCOND, clds);
			}
			else {
			    Set_Clause_Vars(CL_OC, clds);
			    Occur_Check_Next(Mode(ArgNo(loc), procedure->mode))
			}
			if (IsA(loc))
			{
			    Get_ValueA(ArgNo(loc), i, delcl &&
				!OutputMode(Mode(ArgNo(loc), procedure->mode)));
			}
			else if (IsT(loc))
			{
			    Get_ValueT(NumberOf(loc), clds->pushed, i, delcl);
			}
			else if (VarAllocated(tvv))
			{
			    Get_ValueL(tvv, i, delcl);
			    Reset_Var_Init(tvv);
			}
			else
			    Assert(0);
			if (HeadState(where))
			{
			    if (MemoryA(i))
			    {
				Add_Var_Source(tvv, ContArg(i));
			    }
			    else /* put the register at the beginning */
			    {
				New_Pair(tvv->source.next, VarSource(tvv));
				VarSource(tvv) = ContArg(i);
			    }
			}
			Reset_Var_Uninit(tvv);
			Reset_Var_Uninit(tvva);
		    }
		    else if (VarPermanent(tvv))
		    {
			Assert(ClauseAllocated(clds));
			Get_VariableL(tvv, i);
			Set_Var_Allocated(tvv);
			Set_Var_Perm_Source(tvv);
			break;
		    }
		    else
		    {
			reg_desc		*regs = procedure->regs;

			if (!VarUnsafe(tvv) && RegFree(regs, Rt1))
			{
			    Set_Reg_Occupied(regs, Rt1);
			    loc = Rt1;
			}
			else
			{
			    loc = ContTmp(++(clds->pushed));
			}
			Set_Var_Source(tvv, loc);
			Get_VariableT(loc, clds->pushed, i);
		    }
		}
		else
		{
		    if (VarSource(tvva)) {
			if (VarUninit(tvva)) {
			    Set_Clause_Vars(CL_DCOND, clds)
			} else if (!VarGround(tvva)) {
			    Set_Clause_Vars(CL_WAKE, clds);
			}
		    }
		    if (i == procedure->index && where == Head &&
			clds->indexed_entry == code - LABEL_SIZE)
			index = i;
		    else
			index = 0;
		    code = get_constant(procedure, arg, i,
				move ? 0 : (delcl ? NONVAR :
				(int) Mode(i, procedure->mode)), code, 0);
		    if (index) {
			New_Label(clds->indexed_entry)
		    }
		}
	    } else
		ignored = 1;
	}
	else if (gval == d_.fail && procindex->module_ref == d_.kernel_sepia)
	{
	    if (ProcedureDebug(procedure) && !InvisibleProc(procindex))
	    {
		Store_3d(Debug_esc, procindex, call_port);
	    }
	    Store_d(Failure);
	    return code;
	}
	else
	{
	    if (ProcedureDebug(procedure) && !InvisibleProc(procindex))
	    {
		Store_3d(Debug_esc, procindex, call_port);
	    }
	    if (UnifType(procindex) > U_GROUND && ClauseVars(clds) == CL_OC) {
		Occur_Check_Next(0)
	    }
	    Store_d(Escape);
	    Store_d(procindex);
	    if (ProcedureDebug(procedure) && !InvisibleProc(procindex))
	    {
		Store_3d(Debug_esc, procindex, EXIT_PORT);
	    }
	}
	break;

#if 0
    case B_SAFE:
	if (ProcedureDebug(procedure) && !InvisibleProc(procindex))
	{
	    Store_3d(Debug_esc, procindex, call_port);
	}
	Store_d(Escapes);
	Store_d(procindex);
	break;

    case B_UNSAFE:
	if (ProcedureDebug(procedure) && !InvisibleProc(procindex))
	{
	    Store_3d(Debug_esc, procindex, call_port);
	}
	if (UnifType(procindex) > U_GROUND && ClauseVars(clds) == CL_OC) {
	    Occur_Check_Next(0)
	}
	Store_d(Escapeu);
	Store_d(procindex);
	break;
#endif

    default:
	p_fprintf(current_err_,
	    "(codegen): wrong flags (0x%x) ",
	    PriFlags(procindex));
	p_fprintf(current_err_,
	    "of the built-in %s/%d ",
	    DidName(gval),
	    DidArity(gval));
	p_fprintf(current_err_,
	    "when compiling %s/%dn",
	    DidName(procindex->did),
	    DidArity(procindex->did));
	ec_flush(current_err_);
	p_reset();
    }
    if (maywake && ClauseVars(clds) >= CL_WAKE) {
	Set_Clause_Maywake(clds);
    }
    if (!GLast(gflags) && !ignored)
    {
	Debug_Exit(ProcedureDebug(procedure))
    }
#ifdef CALLEE_POP
    clds->pushed -= DidArity(PriDid(procindex)) + (ToolProc(procindex) ? 1 : 0);
#endif
	
    return code;
}

/* find a free argument to put the variable in */
int
free_arg(var_desc *tvv, cl_desc *clds, proc_desc *procedure)
{
    arg_desc	*gargs = procedure->gargs;
    int		max;
    int		arg;

    if (IsA(VarDestination(tvv))) {
	arg = ArgNo(VarDestination(tvv));
	if (arg && ArgFree(gargs + arg))
	    return arg;
    }

    max = DidArity(procedure->did);

    if (max < (int) clds->reg_arity + 1)
	max = (int) clds->reg_arity + 1;
    for (arg = max; arg >= 1; arg--)
       if (ArgFree(gargs + arg))
	    break;
    return arg ? arg : DidArity(procedure->did) + 1;
}

/*
 * FUNCTION NAME:
 *
 * PARAMETERS:
 *
 * DESCRIPTION:
 *
 * Generate the 'put' instruction for one argument.
 */
vmcode      *
ec_put(pword *arg, cl_desc *clds, proc_desc *procedure, register vmcode *code, long int i, int last)
{
    var_desc		*tvv;
    long		loc;
    pword		*dest;
    int			str_mode;

    Buf_Alloc(procedure->codebuf, code, L_PUT);
    switch (TagType(arg->tag))
    {
    case TTVV:
	tvv = Tvv(arg->val);
	if (!VarSource(tvv))
	{
	    if (VarPermanent(tvv))
	    {
		Assert(ClauseAllocated(clds));
		Put_VariableL(tvv, i);
		Set_Var_Allocated(tvv);
		Set_Var_Perm_Source(tvv);
	    }
	    else
	    {
		Put_Variable(tvv, i);
		Set_Var_Source(tvv, ContArg(i));
		Set_Var_Global(tvv);
	    }
	    if (IsMeta(tvv->header.tag)) {
#if ALLOW_COMPILED_ATTRIBUTES
		Set_Var_Global(tvv);
		code = put_structure(arg, clds, procedure, &str_mode, code);
#else
#define ILLEGAL_ATTRIBUTE 165
		return (compiler_error(ILLEGAL_ATTRIBUTE, clds, procedure, code));
#endif
	    }
	}
	else	/* subsequent occurrence */
	{
	    loc = VarSource(tvv);
	    if (last && VarUnsafe(tvv) && VarPermanent(tvv))
	    {
		Assert(VarAllocated(tvv));
		Put_Unsafe_ValueL(tvv, i);
		Reset_Var_Init(tvv);
	    }
	    else if (IsT(loc) && VarUnsafe(tvv))
	    {
		Assert(!IsR(loc));
		Put_Unsafe_ValueT(NumberOf(loc), clds->pushed, i);
	    }
	    else
	    {
		if (IsY(loc))
		{
		    if (IsMeta(tvv->header.tag) && VarInit(tvv)) {
#if ALLOW_COMPILED_ATTRIBUTES
			Store_4d(Put_referenceAM, Address(i), Esize(2),
			    tvv->header.tag.kernel)
			Store_2d(Read_valueL, VarPermOff(tvv))
			Reset_Var_Init(tvv);
			code = put_structure(arg, clds, procedure, &str_mode,
				code);
#else
			return (compiler_error(ILLEGAL_ATTRIBUTE, clds, procedure, code));
#endif
		    } else {
			Put_ValueL(tvv, i)
			Reset_Var_Init(tvv);
		    }
		}
		else if (IsA(loc) && ArgNo(loc) != i) /* noop after X=Y, p(X) */
		{
		    Put_ValueA(ArgNo(loc), i);
		}
		else
		{
		    Put_ValueT(NumberOf(loc), clds->pushed, i);
		}
	    }
	    /* Set_Var_Source(tvv, ContArg(i)); (?) */
	}
	Reset_Var_Uninit(tvv);
	break;

    case TCOMP:
	if (IsProc(arg->val.ptr->tag))
	{
	    Put_Structure(PriDid((pri *) ((arg->val.ptr)->val.ptr)), i);
	}
	else
	{
	    Put_Structure(arg->val.ptr->val.did, i);
	}
	code = put_structure(arg, clds, procedure, &str_mode, code);
	break;

    case TLIST:
	Put_List(i);
	code = put_structure(arg, clds, procedure, &str_mode, code);
	break;

    case TDICT:
	Put_Atom(arg->val.nint, i);
	break;

    case TNIL:
	Put_Nil(i);
	break;

    case TINT:
	Put_Integer(arg->val.nint, i);
	break;

#ifdef TFLOAT
    /* want to resurrect the XXX_Float instructions for doubles in the future */
    case TFLOAT:
	Put_Float(arg->val.nint, i);
	break;
#endif

    case TSTRG:
	{
	    value v;
	    v.ptr = enter_string_n(StringStart(arg->val),
			StringLength(arg->val), DICT_CODE_REF);
	    Put_String(v.nint, i);
	}
	break;

    case TGRS:	/* ground structure */
	dest = copy_ground_structure(procedure, arg->val, tcomp);
	Put_Constant(TCOMP, dest, i);
	break;

    case TGRL:	/* ground list */
	dest = copy_ground_structure(procedure, arg->val, tlist);
	Put_Constant(TLIST, dest, i);
	break;

    case TSUSP:	/* suspension: compile a woken one */
	Put_Constant(TSUSP, &woken_susp_, i);
	break;

    default:
	if (TagType(arg->tag) < 0 || TagType(arg->tag) > NTYPES || !ISAtomic(arg->tag.kernel))
	{
	    return (compiler_error(WRONG_TYPE, clds, procedure, code));
	}
	else if (!IsSimple(arg->tag))
	{
	    dest = copy_ground_structure(procedure, arg->val, arg->tag);
	    Put_Constant(arg->tag.kernel, dest, i);
	}
	else
	{
	    Put_Constant(arg->tag.kernel, arg->val.nint, i);
	}
    }
    return code;
}

/*
 * FUNCTION NAME:
 *
 * PARAMETERS:
 *
 * DESCRIPTION:
 *
 * Generate the Puts_* instruction for 1 argument. The count of pushed
 * args is incremented in the caller, not in this function.
 */
static vmcode      *
_puts(pword *arg, cl_desc *clds, proc_desc *procedure, int *str_mode, register vmcode *code)
{
    type                tag;
    var_desc		*tvv;
    long		loc;
    pword		*dest;

    Buf_Alloc(procedure->codebuf, code, L_PUTS);
    tag.all = arg->tag.all;
    switch (TagType(tag))
    {
    case TTVV:
	tvv = Tvv(arg->val);
	loc = VarSource(tvv);
	if (VarVoid(tvv))
	{
	    Puts_Variable(tvv);
	}
	else if (!loc)			/* First occurrence */
	{
	    if (VarPermanent(tvv))
	    {
		Assert(ClauseAllocated(clds));
		Puts_VariableL(tvv);
		Set_Var_Allocated(tvv);
		Set_Var_Perm_Source(tvv);
	    }
	    else if (VarOccCompnd(tvv))
	    {
		Puts_VariableG(tvv, clds);
		Set_Var_Global(tvv);
		Reset_Var_Uninit(tvv);
	    }
	    else
	    {
		Puts_Variable(tvv);
		Set_Var_Source(tvv, ContTmp(clds->pushed + 1));
	    }
	}
	else	/* subsequent occurrence */
	{
	    if (VarGlobalAlloc(tvv))
	    {
		/* We need to globalize tvv - we use push_local_value,
		 * but we must set S; put_list can do that
		 */
		Buf_Alloc(procedure->codebuf, code, L_PUT+2*L_PUSH);
		Put_List(MAXARITY - 1);
		/* globalize the value */
		if (IsY(loc))
		{
		    Push_Local_ValueL(tvv);
		    Reset_Var_Init(tvv);
		}
		else if (IsA(loc))
		{
		    Push_Local_ValueA(ArgNo(loc));
		}
		else
		{
		    Push_Local_ValueT(NumberOf(loc), clds->pushed);
		}
		Set_Var_Global(tvv);
		Reset_Var_Global_Alloc(tvv);
		Push_Nil
	    }
	    if (IsY(loc))
	    {
		if (IsMeta(tvv->header.tag) && VarInit(tvv)) {
#if ALLOW_COMPILED_ATTRIBUTES
		    Store_3d(Puts_reference, Esize(2), tvv->header.tag.kernel)
		    Store_2d(Read_valueL, VarPermOff(tvv))
#else
		    return (compiler_error(ILLEGAL_ATTRIBUTE, clds, procedure, code));
#endif
		} else {
		    Puts_ValueL(tvv);
		}
		Reset_Var_Init(tvv);
	    }
	    else if (IsA(loc))
	    {
		Puts_ValueA(ArgNo(loc));
	    }
	    else if (IsG(loc))
	    {
		Puts_ValueG(NumberOf(loc), clds->s)
	    }
	    else
	    {
		Puts_ValueT(NumberOf(loc), clds->pushed);
	    }
	    if (--tvv->counter == 0)
		free_sources(tvv, procedure);
	}
	(clds->pushed)++;
	if (IsMeta(tvv->header.tag) && !VarMetaCst(tvv)) {
#if ALLOW_COMPILED_ATTRIBUTES
	    if (!loc) {
		Set_Var_Global(tvv)
	    }
	    code = put_structure(arg, clds, procedure, str_mode, code);
#else
	    return (compiler_error(ILLEGAL_ATTRIBUTE, clds, procedure, code));
#endif
	}
	break;

    case TCOMP:
	if (IsProc(arg->val.ptr->tag))
	{
	    Puts_Structure(PriDid((pri *) ((arg->val.ptr)->val.ptr)));
	}
	else
	{
	    Puts_Structure(arg->val.ptr->val.did);
	}
	(clds->pushed)++;
	code = put_structure(arg, clds, procedure, str_mode, code);
	break;

    case TLIST:
	Puts_List();
	(clds->pushed)++;
	code = put_structure(arg, clds, procedure, str_mode, code);
	break;

    case TDICT:
	Puts_Atom(arg->val.nint);
	(clds->pushed)++;
	break;

    case TNIL:
	Puts_Nil;
	(clds->pushed)++;
	break;

    case TINT:
	Puts_Integer(arg->val.nint);
	(clds->pushed)++;
	break;

#ifdef TFLOAT
    case TFLOAT:
	Puts_Float(arg->val.nint);
	(clds->pushed)++;
	break;
#endif

    case TSTRG:
	{
	    value v;
	    v.ptr = enter_string_n(StringStart(arg->val),
			StringLength(arg->val), DICT_CODE_REF);
	    Puts_String(v.nint);
	}
	(clds->pushed)++;
	break;

    case TGRS:	/* ground structure */
	dest = copy_ground_structure(procedure, arg->val, tcomp);
	Puts_Constant(TCOMP, dest);
	(clds->pushed)++;
	break;

    case TGRL:	/* ground list */
	dest = copy_ground_structure(procedure, arg->val, tlist);
	Puts_Constant(TLIST, dest);
	(clds->pushed)++;
	break;

    case TSUSP:	/* suspension: compile a woken one */
	Puts_Constant(TSUSP, &woken_susp_);
	(clds->pushed)++;
	break;

    default:
	if (TagType(arg->tag) < 0 || TagType(arg->tag) > NTYPES || !ISAtomic(arg->tag.kernel))
	{
	    return (compiler_error(WRONG_TYPE, clds, procedure, code));
	}
	else if (!IsSimple(tag))
	{
	    dest = copy_ground_structure(procedure, arg->val, tag);
	    Puts_Constant(tag.kernel, dest);
	    (clds->pushed)++;
	}
	else
	{
	    Puts_Constant(tag.kernel, arg->val.nint);
	    (clds->pushed)++;
	}

	Puts_Default(tag, arg->val.nint);
    }
    return code;
}

/* the size of the queue for body compound arguments */
#define QSIZE		50
#define Enqueue(a)	{*QE++ = a; if (QE >= queue + qsize) QE = queue;}
#define Dequeue(a)	{a = *QS++; if (QS >= queue + qsize) QS = queue;}

/*
 * FUNCTION NAME:
 *
 * PARAMETERS:
 *
 * DESCRIPTION:
 */
/* Generate the instructions for a compound subgoal argument. It's used both
 * for lists and structures and for simple and regular subgoals, that's why
 * the Put or Puts instruction is not generated here. */
vmcode      *
put_structure(pword *str,	/* pointer to the first argument */
	cl_desc *clds,
	proc_desc *procedure,
	int *str_mode,
	register vmcode *code)
{
    arg_desc		*gargs = procedure->gargs;
    reg_desc		*regs = procedure->regs;
    pword		*default_queue[QSIZE];
    pword		**queue = default_queue;
    pword		**QS;
    pword		**QE;	       /* start and end of queue */
    pword		*arg;
    pword		*arg0;
    pword		*last;
    long		loc;
    var_desc		*tvv;
    int			qsize = QSIZE;
    int			enqueue = 0;

    QS = QE = queue;
    arg0 = str->val.ptr;
    for (;;)
    {
	if (IsTvv(str->tag))
	{
	    if (IsMeta(Tvv(str->val)->header.tag)) {
		arg0 = Tvv(str->val)->header.val.ptr + 1;
		Set_Var_Meta_Cst(Tvv(str->val));
		last = arg0 + 1;
	    } else
		last = arg0;		/* is already pushed */
	}
	else if (IsStructure(str->tag))
	{
	    if (IsProc(arg0->tag))
		last = arg0 + GoalArity(arg0) + 1;
	    else
		last = arg0 + DidArity(arg0->val.did) + 1;
	    arg0++;
	}
	else			       /* a list */
	    last = arg0 + 2;
	for (; arg0 < last; arg0++)
	{
	    arg = arg0;
	    Buf_Alloc(procedure->codebuf, code, L_PUSH);
	    switch (TagType(arg->tag))
	    {
	    case TTVV:
		tvv = Tvv(arg->val);
		if (VarVoid(tvv))
		{
		    Push_Void(tvv);
		    break;
		}
		else if (loc = VarSource(tvv))   /* subsequent occurrence */
		{
		    if (VarUninit(tvv))
			*str_mode |= OUTPUT;
		    else if (!VarGround(tvv))
			*str_mode |= NONVAR;
		    if (VarInit(tvv)) {
			Reset_Var_Init(tvv)
			Push_Init_VariableL(tvv)
			Set_Var_Global(tvv);
		    }
		    else if (VarGlobal(tvv))
		    {
			if (IsA(loc))
			{
			    Push_ValueA(NumberOf(loc));
			}
			else if (IsY(loc))
			{
			    Push_ValueL(tvv);
			    Reset_Var_Init(tvv)
			}
			else if (IsG(loc))
			{
			    Push_ValueG(NumberOf(loc), clds->s);
			}
			else
			{
			    Push_ValueT(NumberOf(loc), clds->pushed);
			}
		    }
		    else
		    {
			if (IsA(loc))
			{
			    Push_Local_ValueA(NumberOf(loc));
			}
			else if (IsY(loc))
			{
			    Push_Local_ValueL(tvv);
			}
			else
			{
			    Push_Local_ValueT(NumberOf(loc), clds->pushed);
			    if (VarAllocTemp(tvv)) {
				Set_Var_Global(tvv);
			    }
			}
			Reset_Var_Global_Alloc(tvv);
		    }
		    if (--tvv->counter == 0)
			free_sources(tvv, procedure);
		    Reset_Var_Uninit(tvv);
		    break;
		}	/* first occurrence */
		else if (VarPermanent(tvv))
		{
		    Assert(ClauseAllocated(clds));
		    Push_VariableL(tvv);
		    Set_Var_Global(tvv);
		    Set_Var_Allocated(tvv);
		    Set_Var_Perm_Source(tvv);
		    Reset_Var_Uninit(tvv);
		    break;
		}	/* Assert(Regular(goal))	*/
		else if (VarOccCompnd(tvv))
		{
		    Push_VariableG(tvv, clds);
		    Set_Var_Global(tvv);
		    Reset_Var_Uninit(tvv);
		    break;
		}
		else if (loc = VarDestination(tvv))
		{
		    /*
		     * First occurrence of a temporary variable,
		     * try to find an Ai to put it in.
		     */
		    struct var_ch	    *ptr = &tvv->destination;
		    struct var_ch	    *save_ptr = 0;
		    arg_desc	    *ards;

		    Reset_Var_Uninit(tvv);
		    while(ptr)
		    {
			loc = ptr->cont;
			Assert(IsA(loc));
			ards = gargs + ArgNo(loc);
			if (clds->block == 1 && !ArgFree(ards))
			{
			    if (!save_ptr || ArgShared(ards))
				save_ptr = ptr;
			}
			else
			{
			    Set_Arg_Done(ards);
			    Buf_Alloc(procedure->codebuf, code, L_PUSH);
			    Push_VariableA(ArgNo(loc), tvv);
			    Set_Var_Global(tvv);
			    Remove_Cont(ptr);
			    Set_Var_Source(tvv, loc);
			    break;
			}
			ptr = ptr->next;
		    }
		    if (ptr)	/* we have found an argument */
			break;	
		    else if (save_ptr &&
			    ArgShared(ards = gargs + ArgNo(save_ptr->cont)))
		    {
			remove_shared(procedure, ArgNo(save_ptr->cont));
			Set_Arg_Done(ards);
			Buf_Alloc(procedure->codebuf, code, L_PUSH);
			Push_VariableA(ArgNo(save_ptr->cont), tvv);
			Set_Var_Global(tvv);
			Remove_Cont(save_ptr);
			Set_Var_Source(tvv, loc);
			break;
		    }
		    else if (save_ptr)
		    {
			/*
			 * Try to free a destination which is occupied.
			 */
			while(save_ptr)
			{
			    loc = save_ptr->cont;
			    ards = gargs + ArgNo(loc);
			    if (IsTvv(ards->contents->tag) && !IsMeta(Tvv(ards->contents->val)->header.tag))
			    {
				var_desc		*tvv1;

				tvv1 = Tvv(ards->contents->val);
				if (VarPermanent(tvv1))
				{
				    if (!VarAllocated(tvv1))
				    {
					Buf_Alloc(procedure->codebuf, code,
						L_GET+L_PUSH);
					Get_VariableL(tvv1, ArgNo(loc));
					Set_Var_Allocated(tvv1);
				    }
				    Set_Var_Perm_Source(tvv1);
				    Set_Arg_Done(ards);
				    Push_VariableA(ArgNo(loc), tvv);
				    Set_Var_Global(tvv);
				    Remove_Cont(save_ptr);
				    Set_Var_Source(tvv, loc);
				    break;
				}
			    }
			    save_ptr = save_ptr->next;
			}
		    }
		    if (save_ptr)
			break;	
		}
		/* we have to use a new temporary */
		if (RegFree(regs, Rt1))
		{
		    Set_Reg_Occupied(regs, Rt1);
		    loc = ContTmp(Rt1);
		    Set_Var_Source(tvv, ContRTmp(loc));
		}
		else
		{
		    loc = 0;
		    (clds->pushed)++;
		    Set_Var_Source(tvv, ContTmp(clds->pushed));
		}
		Push_VariableT(loc, tvv);
		Set_Var_Global(tvv);
		Reset_Var_Uninit(tvv);
		break;

	    case TCOMP:
		if (IsProc(arg->val.ptr->tag))
		{
		    Push_Structure(GoalArity(arg->val.ptr));
		}
		else
		{
		    Push_Structure(DidArity(arg->val.ptr->val.did));
		}
		enqueue = 1;
		break;

	    case TLIST:
		Push_List(arg->val);
		enqueue = 1;
		break;

	    case TDICT:
		Push_Atom(arg->val.nint);
		break;

	    case TNIL:
		Push_Nil;
		break;

	    case TINT:
		Push_Integer(arg->val.nint);
		break;

#ifdef TFLOAT
	    case TFLOAT:
		Push_Float(arg->val.nint);
		break;
#endif

	    case TSTRG:
		{
		    value v;
		    v.ptr = enter_string_n(StringStart(arg->val),
				StringLength(arg->val), DICT_CODE_REF);
		    Push_String(v.nint);
		}
		break;

	    case TGRS:	/* ground structure */
		{
		    pword *dest;
		    dest = copy_ground_structure(procedure, arg->val, tcomp);
		    Push_Constant(TCOMP, dest);
		}
		break;

	    case TGRL:	/* ground list */
		{
		    pword *dest;
		    dest = copy_ground_structure(procedure, arg->val, tlist);
		    Push_Constant(TLIST, dest);
		}
		break;

	    case TSUSP:	/* suspension: compile a woken one */
		Push_Constant(TSUSP, &woken_susp_);
		break;

	    default:
		if (TagType(arg->tag) < 0 || TagType(arg->tag) > NTYPES || !ISAtomic(arg->tag.kernel))
		{
		    return (compiler_error(WRONG_TYPE, clds, procedure, code));
		}
		else if (!IsSimple(arg->tag))
		{
		    pword *dest;
		    dest = copy_ground_structure(procedure, arg->val, arg->tag);
		    Push_Constant(arg->tag.kernel, dest);
		}
		else
		{
		    Push_Constant(arg->tag.kernel, arg->val.nint);
		}
	    }
	    clds->s++;
	    if (enqueue)
	    {
		Enqueue(arg);
		if (QS == QE)	/* queue overflow */
		{
		    pword	**old_queue = queue;
		    pword	**to, **end;

		    queue = to = (pword **)
			hg_alloc((qsize + qsize) * sizeof(pword *));
		    end = old_queue + qsize;
		    while (QS < end)
			*to++ = *QS++;
		    QS = old_queue;
		    while (QS < QE)
			*to++ = *QS++;
		    if (qsize != QSIZE)
			hg_free((generic_ptr) old_queue);
		    QS = queue;
		    QE = to;
		    qsize += qsize;
		}
		enqueue = 0;
	    }

	}
	if (QS == QE)
	    break;
	Dequeue(str);
	Buf_Alloc(procedure->codebuf, code, L_PUSH);
	if (IsTvv(str->tag))
	{
	    Push_Named_Variable(Tvv(str->val));
	    clds->s++;
	}
	else
	{
	    arg0 = str->val.ptr;
	    if (IsStructure(str->tag))
	    {
		if (IsProc(arg0->tag))
		{
		    Push_Functor(PriDid((pri *) ((arg0)->val.ptr)));
		}
		else
		{
		    Push_Functor(arg0->val.did);
		}
		clds->s++;
	    }
	}
    }
    return code;
}

/*
 * copy_ground_structure(procedure, v, t) - copy a prolog term from the stack
 *		to the code heap. A seprate code block with the structure
 *		mark is allocated and linked.
 */
pword *
copy_ground_structure(proc_desc *procedure, value v, type t)
{
    vmcode		*code;
#ifndef DONT_USE_GROUND_CONSTANT_TABLE
    pword tabled_constant;
    int res = ec_constant_table_enter(v, t, &tabled_constant);
    if (res == PSUCCEED)
    {
    	return tabled_constant.val.ptr;
    }
    else if (res != PFAIL)
    {
	p_fprintf(current_err_, "INTERNAL ERROR %d in copy_ground_structure(%d)\n", res, t.all);
	ec_flush(current_err_);
    }
#endif
    {
	code = AllocateCodeBlock(0L, ProcLink(procedure->start), 0L, (uword) TNIL,
	    GROUND_TERM, 0L);
	ProcLink(procedure->start) = (uword) ProcHeader(code);
	(void) create_heapterm(ProcStruct(code), v, t);
	return ((pword *) ProcStruct(code))->val.ptr;
    }
}

reclaim_ground_structure(vmcode *code_header)
{
    extern void free_heapterm();

    free_heapterm(ProcStruct(CodeStart(code_header)));
    hg_free((generic_ptr) code_header);
}

void
reset_args(arg_desc *gargs)
{
    int		i;

    for (i = 1; i <= GargsSize(gargs); i++) {
	gargs[i].contents = gargs[i].target = 0;
	Reset_Arg_State(gargs + i)
    }
}

