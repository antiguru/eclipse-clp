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
 * VERSION	$Id: pass3.c,v 1.1 2006/09/23 01:56:11 snovello Exp $
 */

/*
 * IDENTIFICATION		pass3.c
 *
 * DESCRIPTION
 *
 *	SEPIA COMPILER
 *
 * This file contains the main routine for the third compilation pass -
 * the clause code generation. It is based on a finite state machine
 * which recognizes four possible inputs - a regular goal, simple goal,
 * cut and end of the clause and another one which is used to expand
 * disjunctions and the like.
 * The working of the former one is described in
 *	M.Meier: Compilation into SEPIA Abstract Machine, IR-LP-13-04
 *
 * Handling of the cut in the expanded goals:
 *	- flag 'Nested' is true if we are in a goal in which cut does not 
 *	  cut through, i.e. once/1 and not/1 (call/1 is not expanded).
 *	- if Nested is true, 'nested_cut' denotes the cut variable
 *	  that is valid for this goal. If it is 0, it means that the
 *	  goals are simple and that the cut can be omitted
 *	- flag 'Cond' is true if we are in the left argument of an implication
 *	  ->/2, It is necessary to reject cuts in it, which would be
 *	  difficult to process - a cut there would have to be
 *	  followed by Savecut's for all active cut variables.
 *
 * CONTENTS:
 *
 * AUTHOR	VERSION	 DATE	REASON
 * Micha Meier	2.2	20.7.89	created the file
 */

/*
 * INCLUDES:
 */
#include	<stdio.h>
#include	"config.h"
#include	"sepia.h"
#include	"types.h"
#include        "embed.h"
#include	"mem.h"
#include	"error.h"
#include	"dict.h"
#include	"opcode.h"
#include	"compiler.h"
#include	"gencode.h"
#include 	"emu_export.h"
#include	"debug.h"
#include	"io.h"
#include	"database.h"

/*
 * DEFINES:
 */
/* state definitions		*/
#define	After			| 
#define LastGoal		Last | 

/* global states, their order is significant	*/
#define	OrLeft			000
#define	SimpleOrLeft		002
#define OrRight			004
#define SimpleOrRight		006
#define EndOr			010
#define Goal			012
#define And			014
#define CondRight		016
#define SimpleCondRight		020
#define IfRight			022
#define SLabel			024

#define Once			030

#define Not			034
#define SimpleNot		036

#define CutVar			042
#define End			044
#define EndOrLeft		046
#define EndSimpleOrLeft		050
#define Pushed			052
#define NotNot			054
#define Not2			056
#define SoftCutRight		060

#define NotLast			0
#define Last			000001

#define Push_(state, gflags, label, goal, cut)	\
	*s++ = (long) ((state) | (gflags));	\
	*s++ = (long) (label);			\
	*s++ = (long) (goal);			\
	*s++ = (long) (cut);			\
	if (s == stack_end)			\
	{					\
	    int	stack_size = s - procedure->stack;\
	    procedure->stack = (long *) hg_resize((generic_ptr) procedure->stack,\
		2 * stack_size * sizeof(long));	\
	    s = procedure->stack + stack_size;	\
	    stack_end = procedure->stack_end = s + stack_size;\
	}


#define Pop_(state, gflags, label, goal, cut)	\
	cut = *--s;				\
	goal = (pword *) *--s;			\
	label = (vmcode *) *--s;		\
	state = *--s;				\
	gflags = state & GFLAGS;		\
	state &= ~GFLAGS;

#define Set_G_First_Top \
	Set_G_First((*(s-4)))

/* a local cut which is not traced */
#define Cut_L(var, env_size, debug)					\
	    if (var > active_size || var <= 0)				\
	       _codegen_error(procedure, clause_number, "-> variable trimmed");\
	    active_size = _environment_size(clds);		\
	    Store_3d(CutL, Esize(var), Esize(env_size));			\
	    Debug_Port(SEMI_CUT_PORT, debug);
#define Savecut_L(var)		Store_2d(SavecutL, Esize(var))
#define Set_Bp(debug, gflags)	Debug_Port_For_Call(SEMI_PORT, debug, gflags);\
				Store_d(Set_bp)
#define New_Bp()		Store_d(New_bp)
#define Restore_Bp()		Store_d(Restore_bp)
#define Softcut_L(var)					\
	    if (var > active_size || var <= 0)				\
	       _codegen_error(procedure, clause_number, "-> variable trimmed");\
	    Store_2d(SoftcutL, Esize(var));
#define Dfid_Test(dfid)		Store_d(Dfid_test);
#define Dfid_TestL(dfid)	Store_2d(Dfid_testL, Esize(dfid));
#define Depth_(dfid)		if (dfid) {Store_2d(Depth, Esize(dfid));}

#define Branchs_(size)							\
	    {								\
		if (size)						\
		    {Store_2d(Branchs, Esize(size));}			\
		else							\
		{							\
		    Store_d(Branch);					\
		}							\
	    }

#define Debug_Simple_Fail_Port(debug)					\
	    if (debug) {						\
		Store_3d(Debug_esc, fail_proc_, FAIL_PORT)			\
	    }

/*
 * EXTERNAL VARIABLE DECLARATIONS:
 */
extern vmcode	    *ec_escape(pri *procindex, pword *goal, dident gval, cl_desc *clds, proc_desc *procedure, register vmcode *code, int where, int gflags),
		    *head_unification(cl_desc *clds, proc_desc *procedure, vmcode *code),
		    *output_mode(cl_desc *clds, proc_desc *procedure, vmcode *code, int where),
		    *get_structure(pword *structure, cl_desc *clds, proc_desc *procedure, vmcode *code, long int i, long int index, int mode, int to_unif, long int match),
		    *ec_put(pword *arg, cl_desc *clds, proc_desc *procedure, register vmcode *code, long int i, int last),
		    *put_first_arguments(cl_desc *clds, proc_desc *procedure, register vmcode *code, int last, dident module),
		    *put_arguments(pword *goal, cl_desc *clds, proc_desc *procedure, register vmcode *code, int last, dident module),
		    *put_det_arguments(pword *goal, cl_desc *clds, proc_desc *procedure, vmcode *code, dident module),
		    *compiler_error(int number, cl_desc *clds, proc_desc *procedure, vmcode *code);

extern pri	    *cut_to_proc_;
extern pri	    *fail_proc_;
extern dident       d_call_susp_,
		    d_untraced_call;


/*
 * EXTERNAL VARIABLE DEFINITIONS:
 */
void                free_sources(var_desc *tvv, proc_desc *procedure);

/*
 * STATIC VARIABLE DEFINITIONS:
 */
static int	_environment_size(cl_desc *clds);
static vmcode	*_cut(int env_size, int nested_cut, int gflags, int debug, vmcode *code);
static void	_codegen_error(proc_desc *procedure, int clause_number, char *error),
		_mark_initialized(int var, cl_desc *clds);

#ifdef DEBUG_COMP
static int	debug_gstates = 0;
static char	*_gstates[] = {
"OrLeft          ",
"SimpleOrLeft    ",
"OrRight         ",
"SimpleOrRight   ",
"EndOr           ",
"Goal            ",
"And             ",
"CondRight       ",
"SimpleCondRight ",
"IfRight         ",
"SLabel          ",
"EndLabel        ",
"Once            ",
"NestedOnce      ",
"Not             ",
"SimpleNot       ",
"Pop             ",
"CutVar          ",
"End             ",
"EndOrLeft       ",
"EndSimpleOrLeft ",
"Pushed          ",
"NotNot          ",
"Not2            ",
"SoftCutRight    "
};
#endif

/*
 * FUNCTION DEFINITIONS:
 */

/*
 * FUNCTION NAME:	gen_clause()
 *
 * PARAMETERS:
 *
 * DESCRIPTION:
 *
 * Compile a clause, given the clause descriptor. This is the core procedure
 * of the compiler, it accepts a Prolog term and consequently generates
 * the code in the 3rd pass.
 * The code for the in-line expanded predicates like ;/2, ->/2 etc.
 * is complicated by the cuts in the 1st argument of ->/2 and in not/1 and
 * once/1. For the former, the local cut has to use the global cut value,
 * for the latter it is vice versa.
 */
vmcode             *
gen_clause(cl_desc *clds, int clause_number, proc_desc *procedure, register vmcode *code)
{
    pword              *clause = clds->clause;
    int                 determinate = ClauseDet(clds) &&
			    !(PriFlags(procedure->proc) & PROC_PARALLEL);
    int                 cortn = 0;	/* was GlobalFlags & CORTN; */
    int                 debug;
    int                 expand;
    dident              module = procedure->module;
    int                 *head_pass = procedure->head_pass;
    long		*s = procedure->stack;
    long		*stack_end = procedure->stack_end;
    pword              *goal;
    pword              *p;
    var_desc		*tvv;	       /* aux pointer	      */
    dident              gval;	       /* of the goal */
    pri                *gproc;	       /* proc. table entry of the goal */
    vmcode		*label = 0;
    int                 arity = DidArity(procedure->did);
    int                 gstate;
    int                 state;
    int			itype;
    int                 cut;
    int                 willwake;
    pword              *save_tg = Gbl_Tg;
    int                 flag;	       /* to save a boolean value	 */
    int			ignore;		/* don't generate the call	*/
    vmcode		*ptr;
    int			goaln = 0;
    int			active_size = clds->envsize;
    int			cut_var = 0;
    int			cut_var_valid = G_CVALID;
    int			nested_cut = 0;	/* for once(..!..) etc.	*/
    int			gflags;		/* global flags for the upper machine */
    int			null = 0;
#ifdef DEBUG_COMP
    uint16		pass2_block = clds->pushed;
#endif
    long		arg4;
    long		simple_pushed;
    int			savecut = 0;	/* savecut postponed after Resume */
    int			dfid;

    if (!ProcedureDebug(procedure))
	debug = 0;
    else if (ProcedureSilent(procedure))
	debug = FSILENT;
    else
	debug = LOUD;
    expand = ProcedureExpand(procedure);
    clause_init(clds, procedure);
    cut = ClauseBodyCut(clds);
#ifdef DFID
    dfid = (GlobalFlags & DFID_COMPILE) ? (cut ? 2 : 1) : 0;
#else
    dfid = 0;
#endif
    if (arity > 0)
	code = head_unification(clds, procedure, code);
    Gc_TestA(clds, arity);
    Gc_TestA_Next(clds);
#if 1
    Buf_Alloc(procedure->codebuf, code, L_DPORT);
    Debug_Port(CLAUSE_NUMBER_PORT | (clause_number << 16), debug);
#endif
    willwake = ClauseWillwake(clds);
    flag = ClauseRegular(clds) && !ClauseAllocated(clds);
    if (GlobalFlags & SINGLETON_CHECK	/* check_singletons */
	&& !ProcedureNoWarn(procedure))	/* compiling source, not terms */
	check_singletons(clds, clause_number, "", procedure);

    if (flag || clds->global)
    {
	/*
	 * Make global temps permanent and try to merge the Allocate
	 * with a Move
	 */
	tvv = clds->variables;
	while (tvv)
	{
	    if (VarPermanent(tvv))
	    {
		if (flag && IsA(VarSource(tvv)))
		{
		    Buf_Alloc(procedure->codebuf, code, L_GET);
		    Get_VariableNL(clds->envsize, tvv, ArgNo(VarSource(tvv)));
		    Set_Var_Allocated(tvv);
		    Set_Clause_Allocated(clds);
		    flag = 0;
		}
	    }
	    else if (VarAllocTemp(tvv) && !VarVoid(tvv) &&
		VarSource(tvv) == 0)
	    {
		int	loc = VarDestination(tvv);

		if (VarGlobalAlloc(tvv) ||
			!(IsA(loc) && ArgFree(procedure->gargs + ArgNo(loc))))
		{
		    if (flag)	/* we must allocate before puts */
		    {
			Buf_Alloc(procedure->codebuf, code, L_CALL);
			Allocate_(clds->envsize);
			Set_Clause_Allocated(clds);
			flag = 0;
		    }
		    /* rT could be used as well, but this is rare anyway */
		    Buf_Alloc(procedure->codebuf, code, L_PUTS);
		    Puts_Variable(tvv);
		    Set_Var_Source(tvv, ContTmp(++(clds->pushed)));
		    Set_Var_Uninit(tvv);
		    /* counter? */
		}
	    }
	    tvv = tvv->next_tvv;
	}
    }
    if (flag)	/* still not allocated */
    {
	Buf_Alloc(procedure->codebuf, code, L_CALL);
	Allocate_(clds->envsize);
	Set_Clause_Allocated(clds);
    }

    if (ClauseBskip(clds))
	goal = clds->body;
    else if (IsRule(clause))
	goal = clause->val.ptr + 2;
    else
	goal = 0;
    gstate = Goal;
    state = Head;
    gflags = G_LAST | G_FIRST | G_START;
    Push_(End, gflags, 0, 0, 0);

    for (;;)
    {
#ifdef DEBUG_COMP
	if (debug_gstates)
	{
	    p_fprintf(current_err_, "\n%s%d\t%5o\t",
		_gstates[gstate >> 1],
		clds->block,
		gflags);
	    if (goal && IsStructure(goal->tag) && !IsProc(goal->val.ptr->tag))
	    {
		gval = goal->val.ptr->val.did;
		p_fprintf(current_err_, "%s/%d",
		    DidName(gval),
		    DidArity(gval));
		if (GoalMarked(goal->val.ptr))
		    p_fprintf(current_err_, "\tcut: %d %s",
			GoalCutVar(goal->val.ptr),
			GoalSimple(goal->val.ptr) ? "simple" : "");
	    }
	    ec_flush(current_err_);
	}
#endif
	/* LOOP through the body goals */
	clds->cflags &= ~(CL_VARS | CL_MAYWAKE);
	switch (gstate)
	{
	case OrRight:
	    Buf_Alloc(procedure->codebuf, code, L_TRY);
	    if (IsStructure(goal->tag) && !IsProc((p = goal->val.ptr)->tag))
		gval = p->val.did;
	    else
		gval = 0;
	    if (gval == d_.semicolon &&
		/* a fix for #573 */
		(cut_var || !GoalSimple(goal->val.ptr)))
	    {	/* 5 */
		Retry_Me_Inline(label, debug, active_size);
		determinate = 0;
		Push_(EndOrLeft, gflags, label, p + 2, 0);
		goal = p + 1;
		gstate = OrLeft;
		Set_G_First(gflags);
		Set_G_Lastn(gflags);
	    }
	    else if (!debug && Negation(gval))
	    {
		if (!GoalSimple(p))
		{
		    pword	*next_goal;

		    Retry_Me_Inline(label, debug, active_size);
		    if (IsStructure(p[1].tag) &&
			(next_goal = p[1].val.ptr,
			!IsProc(next_goal->tag)) &&
			Negation(next_goal->val.did))
		    {	/* 5c */
			gstate = Not2;
			Debug_For_Call(d_.not_not, d_.kernel_sepia,
			    CALL_PORT | NO_ARGS, debug, gflags);
			goal = p + 1;
		    }
		    else
		    {	/* 5a */
			Debug_For_Call(d_.not1, d_.kernel_sepia, CALL_PORT | NO_ARGS, debug, gflags);
			determinate = 0;
			Push_(Not, NotGFirst(gflags), label, 0, nested_cut);
			goal = p + 1;
			gstate = And;
			Reset_G_Last(gflags);
			Set_G_Nested(gflags);
			Set_G_First(gflags);
			if (IsStructure(goal->tag))
			    nested_cut = GoalCutVar(goal->val.ptr);
			else
			    nested_cut = 0;
		    }
		}
		else
		{   /* 5b */
		    Trust_Me_Inline(label, debug, active_size);
		    gstate = And;
		}
	    }
	    else
	    {	/* 11 */
		if (!GLast(gflags)) {
		    Push_(EndOr, gflags, 0, 0, 0);
		}
		Trust_Me_Inline(label, debug, active_size);
		gstate = And;
	    }
	    state = Regular;
	    break;

	case SimpleOrRight:
	    Buf_Alloc(procedure->codebuf, code, L_TRY);
	    if (IsStructure(goal->tag) &&
		!IsProc((p = goal->val.ptr)->tag) &&
		GoalSimple(p))
	    {	
		gval = p->val.did;
		if (gval == d_.semicolon)
		{	/* 3 */
		    Debug_Port(RETRY_PORT, debug);
		    Debug_Simple_Fail_Port(debug);
		    New_Bp();
		    Push_(EndSimpleOrLeft, gflags, code++, p + 2, 0);
		    gstate = SimpleOrLeft;
		}
		else
		{	/* 3a */
		    Debug_Port(TRUST_PORT, debug);	/* for ; */
		    Debug_Simple_Fail_Port(debug);
		    New_Bp();
		    Push_(SimpleNot, NotGFirst(gflags), code++, 0, 0);
		    Debug_For_Call(d_.not1, d_.kernel_sepia,
			CALL_PORT | NO_ARGS, debug, gflags);
		    Debug_Port_For_Call(SEMI_PORT, debug, gflags|G_LASTN|G_FIRST);
		    Reset_G_Last(gflags);
		    gstate = And;
		}
		goal = p + 1;
	    }
	    else
	    {	/* 4 */
		if (!GLast(gflags)) {
		    Push_(EndOr, gflags, 0, 0, 0);
		}
		Debug_Port(TRUST_PORT, debug);
		Debug_Simple_Fail_Port(debug);
		Restore_Bp();
		gstate = And;
	    }
	    break;

	case EndOrLeft:
	case EndSimpleOrLeft:
	    Buf_Alloc(procedure->codebuf, code, L_CUT+L_GC+L_RESS+L_BRA+L_LAB);
	    if (state == Goal_Cut)
	    {
		Cut_(active_size, debug, gflags, nested_cut)
		Gc_Test(clds, goaln);
		clds->pushed = 0;
	    }
	    if (!GLast(gflags))
	    {
		if (willwake) {
		    Space_Res(-clds->pushed, 0, null, willwake, active_size)
		    clds->pushed = 0;
		    willwake = 0;
		}
		Branchs_(-clds->pushed);
		Push_(SLabel, gflags, code++, 0, 0);
		clds->pushed = 0;
	    }
	    Label_(label);
	    if (gstate == EndSimpleOrLeft) {
		gstate = SimpleOrRight;
		clds->pushed = simple_pushed;
	    }
	    else {
		gstate = OrRight;
		clds->pushed = 0;
	    }
	    state = Regular;
	    break;

	case IfRight:	/* 13 */
	    if (!cut_var_valid)
	    {
		Buf_Alloc(procedure->codebuf, code, L_CUT);
		Cut_L(cut_var, active_size, debug);
		clds->pushed = 0;
		state = Regular;
	    }
	    cut_var_valid = GCValid(gflags);
	    gstate = And;
	    break;

	case CondRight:	/* 13a */
	    Buf_Alloc(procedure->codebuf, code, L_CUT);
	    Cut_L(cut_var, active_size, debug);
	    clds->pushed = 0;
	    cut_var_valid = G_CVALID;
	    state = Regular;
	    gstate = And;
	    break;

	case SoftCutRight: /* 13b */
	    Buf_Alloc(procedure->codebuf, code, L_CUT);
	    Softcut_L(cut_var);
	    gstate = And;
	    break;

	case SimpleCondRight:	/* 12 */
	    Buf_Alloc(procedure->codebuf, code, L_TRY);
	    Restore_Bp();
	    Push_(Pushed, gflags, 0, 0, clds->pushed);
	    Debug_Port(SEMI_CUT_PORT, debug);
	    gstate = And;
	    break;

	case Once:
	    Buf_Alloc(procedure->codebuf, code, L_CUT+L_DPORT);
	    nested_cut = arg4;
	    if (!cut_var_valid)
	    {
		Cut_L(cut_var, active_size, debug);
		clds->pushed = 0;
		state = Regular;
	    }
	    cut_var_valid = GCValid(gflags);
	    if (GLast(gflags))		/* goal is already 0 */
	    {
		gstate = And;
		break;
	    }
	    else {
		Debug_Exit(debug);
		goto _pop_;
	    }

	case Not:	/* 21 */
	    Buf_Alloc(procedure->codebuf, code, L_CUT+L_WORD+L_TRY+L_DPORT);
	    nested_cut = arg4;
	    Cut_L(cut_var, active_size, debug);
	    clds->pushed = 0;
	    willwake = 0;
	    Store_d(Failure);
	    Label_(label);
	    Trust_Me_Inline(label, debug, active_size);
	    cut_var_valid = GCValid(gflags);
	    state = Regular;
	    if (GLast(gflags))		/* goal is already 0 */
	    {
		gstate = And;
		break;
	    }
	    else {
		Debug_Exit(debug);
		goto _pop_;
	    }

	case NotNot:	/* 21a */
	    Buf_Alloc(procedure->codebuf, code, L_CUT+2*L_TRY+L_DPORT+2*L_CALL);
	    nested_cut = arg4;
	    Cut_L(cut_var, active_size, debug);
	    clds->pushed = 0;
	    willwake = 0;
	    Trust_Me_Inline(label, debug, active_size);
	    Branchs_(clds->pushed); 
	    ptr = code++;
	    Label_(label);
	    Trust_Me_Inline(label, debug, active_size);
	    Store_d(Failure);
	    Label_(ptr);
	    cut_var_valid = GCValid(gflags);
	    state = Regular;
	    if (GLast(gflags))		/* goal is already 0 */
	    {
		gstate = And;
		break;
	    }
	    else {
		Debug_Exit(debug);
		goto _pop_;
	    }

	case SimpleNot:
	    Buf_Alloc(procedure->codebuf, code, 2*L_TRY+L_DB+2*L_DPORT+L_CALL);
	    Restore_Bp();
	    Debug_Cut(d_.cut, d_.kernel_sepia, CUT_PORT, debug);
	    Store_d(Failure);
	    willwake = 0;
	    Label_(label);
	    Restore_Bp();
	    if (!GLast(gflags))
	    {
		Space_(-clds->pushed)
		clds->pushed = 0;
	    }
	    Debug_Port(TRUST_PORT, debug);
	    Debug_Simple_Fail_Port(debug);
	    state = Regular;
	    if (GLast(gflags))		/* goal is already 0 */
	    {
		gstate = And;
		break;
	    }
	    else {
		Debug_Exit(debug);
		goto _pop_;
	    }

	case SLabel:	/* 15 */
	    Buf_Alloc(procedure->codebuf, code, L_LAB);
	    Label_(label);
	    goto _pop_;

	case CutVar:
	    cut_var = arg4;
	    cut_var_valid &= GCValid(gflags);
	    goto _pop_;

	case Pushed:
	    simple_pushed = arg4;
	    goto _pop_;

	case EndOr:	/* 17 */
	    Buf_Alloc(procedure->codebuf, code, L_CUT+L_GC+L_RESS);
	    if (state == Goal_Cut)
	    {
		Cut_(active_size, debug, gflags, nested_cut)
		clds->pushed = 0;
		Gc_Test(clds, goaln);
	    }
	    if (!GLast(gflags))
	    {
		/*
		Space_Res(-clds->pushed, 0, null, willwake, active_size)
		willwake = 0;
		*/
		Space_(-clds->pushed)
		clds->pushed = 0;
	    }
	    state = Regular;
	    gstate = And;
	    goto _pop_;

	case End:
	    Assert(clds->gc_list == 0);
	    Gbl_Tg = save_tg;
#ifdef DEBUG_COMP
	    if (pass2_block != (uint16) (clds->block))
		_codegen_error(procedure, clause_number, "block mismatch");
#endif
	    return code;

	default:
	    if (goal && IsStructure(goal->tag) && !IsProc(goal->val.ptr->tag))
	    {
		/*
		 * In-line expanded goals:
		 * 		,/2 ;/2 ->/2 not/1 \+/1 fail_if/1 once/1
		 */
		goal = goal->val.ptr;
		gval = goal->val.did;
		Buf_Alloc(procedure->codebuf, code, L_SC+L_CUT+L_GC+L_NECK+L_DFID);
		if (GoalCutVar(goal))	/* contains the cut var */
		{
		    Push_(CutVar, (gflags & ~G_CVALID) | cut_var_valid, 0, 0, cut_var);
		    cut_var = GoalCutVar(goal);
		    if (willwake)
			savecut = 1;
		    else if (state == Goal_Cut || state == DetSimple) {
			cut_var = 1;
		    }
		    else
		    {
			Savecut_L(cut_var);
			_mark_initialized(cut_var, clds);
		    }
		    cut_var_valid = G_CVALID;
		}

		if (gflags & G_START) {
		    gflags &= ~G_START;
		    if (gval == d_matching_guard1) {
			goal++;
			break;
		    }
		}

		if (gval == d_.comma)
		{ /* 14 */
		    Push_(And, NotGFirst(gflags), 0, goal + 2, 0);
		    goal++;
		    Reset_G_Last(gflags);
		    gstate = And;
		    break;
		}
		else /* if (gval == d_.semicolon || Negation(gval) || cortn ) */
		{
		    if (state == Goal_Cut)
		    {
			Cut_(active_size, debug, NotGLast(gflags), nested_cut)
			Gc_Test(clds, goaln);
			clds->pushed = 0;
		    }
		    if (HeadState(state))
		    {
			Neck_(shallow && !determinate, cut, arity);
			cut = 0;
			if (dfid)
			{
			    if (ClauseRegular(clds)) {
				Dfid_TestL(dfid);
			    }
			    else if (ClauseRule(clds)) {
				Dfid_Test(dfid);
			    }
			    _mark_initialized(dfid, clds);
			}
			Output_Mode(clds, procedure, code,
			    WDisj(gflags, state), head_pass, willwake);
			code = put_first_arguments(clds, procedure, code,
			    NotLast, D_UNKNOWN);
		    }
		    if (willwake
			||
			!GoalSimple(goal)
			||
			gstate == Not2)
		    {
			Buf_Alloc(procedure->codebuf, code, L_RESS+L_SC);
			Space_Res(-clds->pushed, 0, null, willwake,
			    active_size)
			willwake = 0;
			clds->pushed = 0;
			if (savecut) {
			    Savecut_L(cut_var);
			    savecut = 0;
			}
		    }
		    state = Regular;
		}
		Buf_Alloc(procedure->codebuf, code, L_TRY+L_DB+L_DPORT);
		if (gval == d_.semicolon)
		{
		    if (!GDisj(gflags))
			clds->block++;
		    Set_G_Disj(gflags);
		    if (GoalSimple(goal))
		    {	/* 1 */
			Set_Bp(debug, gflags);
			Set_G_First(gflags);
			Set_G_Lastn(gflags);
			Push_(EndSimpleOrLeft, gflags, code++, goal + 2, 0);
			gstate = SimpleOrLeft;
		    }
		    else
		    {	/* 2 */
			Debug_Port_For_Call(SEMI_PORT, debug, gflags);
			Try_Me_Else(0, label, debug);
			Set_G_First(gflags);
			Set_G_Lastn(gflags);
			Push_(EndOrLeft, gflags, label, goal + 2, 0);
			gstate = OrLeft;
			cut_var_valid = 0;
			determinate = 0;
		    }
		    goal++;
		}
		else if (Negation(gval))
		{
		    pword		*next_goal = goal + 1;

		    if (!GDisj(gflags))
			clds->block++;
		    Set_G_Disj(gflags);
		    if (gstate == Not2)
		    {	/* 2c */
			Push_(NotNot, NotGFirst(gflags), label, 0, nested_cut);
			Reset_G_Last(gflags);
			Set_G_Nested(gflags);
			if (IsStructure((goal + 1)->tag))
			    nested_cut = GoalCutVar((goal + 1)->val.ptr);
			else
			    nested_cut = 0;
			determinate = 0;
			gstate = And;
			goal++;
			break;
		    }
		    if (IsStructure(next_goal->tag) &&
			    (next_goal = next_goal->val.ptr,
			    !IsProc(next_goal->tag)) &&
			    Negation(next_goal->val.did))
		    {	/* 2b */
			Debug_For_Call(d_.not_not, d_.kernel_sepia,
			    CALL_PORT | NO_ARGS, debug, gflags);
			Debug_Port_For_Call(SEMI_PORT, debug, gflags|G_LASTN|G_FIRST);
			Try_Me_Else(0, label, debug);
			gstate = Not2;
		    }
		    else if (!GoalSimple(goal))
		    {	/* 2a */
			Debug_For_Call(d_.not1, d_.kernel_sepia,
			    CALL_PORT | NO_ARGS, debug, gflags);
			Debug_Port_For_Call(SEMI_PORT, debug, gflags|G_LASTN|G_FIRST);
			Try_Me_Else(0, label, debug);
			Push_(Not, NotGFirst(gflags), label, 0, nested_cut);
			if (IsStructure((goal + 1)->tag))
			    nested_cut = GoalCutVar((goal + 1)->val.ptr);
			else
			    nested_cut = 0;
			gstate = And;
			determinate = 0;
			Reset_G_Last(gflags);
			Set_G_Nested(gflags);
		    }
		    else
		    {	/* 1a */
			Debug_For_Call(d_.not1, d_.kernel_sepia,
			    CALL_PORT | NO_ARGS, debug, gflags);
			Set_Bp(debug, gflags|G_LASTN|G_FIRST);
			Push_(SimpleNot, NotGFirst(gflags), code++, 0, 0);
			Reset_G_Last(gflags);
			gstate = And;
		    }
		    Set_G_First(gflags);
		    goal++;
		}
		else if (gval == d_.cond)
		{
		    if (gstate == SimpleOrLeft)
		    {	/* 8 */
			Push_(SimpleCondRight, NotGFirst(gflags), 0,
			    goal + 2, 0);
		    }
		    else if (gstate == OrLeft)
		    {	/* 7a */
			Push_(CondRight, NotGFirst(gflags), 0, goal + 2, 0);
		    }
		    else
		    {	/* 7 */
			Push_(IfRight, NotGFirst(gflags) & ~G_CVALID |
			    cut_var_valid, 0, goal + 2, 0);
			cut_var_valid = G_CVALID;
		    }
		    goal++;
		    Reset_G_Last(gflags);
		    gstate = And;
		    Set_G_Cond(gflags);
		}
		else if (gval == d_.softcut && gstate == OrLeft)
		{    /* 7b */
		    Push_(SoftCutRight, NotGFirst(gflags), 0, goal + 2, 0);
		    goal++;
		    Reset_G_Last(gflags);
		    gstate = And;
		    Set_G_Cond(gflags);
		}
		else if (gval == d_.once)
		{    /* 23 */
		    Push_(Once, NotGFirst(gflags) & ~G_CVALID | cut_var_valid,
			0, 0, nested_cut);
		    goal++;
		    cut_var_valid = G_CVALID;
		    gstate = And;
		    nested_cut = cut_var;
		    Debug_For_Call(d_.once, d_.kernel_sepia,
			CALL_PORT | NO_ARGS, debug, gflags);
		    Reset_G_Last(gflags);
		    Set_G_Nested(gflags);
		    Set_G_First(gflags);
		}
		else
		{
		    Assert(0);
		}
	    }
	    else
	    {
		Buf_Alloc(procedure->codebuf, code, L_GC+L_CUT+L_DFID+L_SC+L_CALL+L_RESS);
		if (!goal)
		    itype = NO_GOAL;
		else
		{
		    /* a normal goal	*/
		    if (IsStructure(goal->tag))
			goal = goal->val.ptr;
		    gproc = (pri *) goal->val.did;
		    gval = PriDid(gproc);
		    ignore = (expand && gval == d_.true0 &&
			gproc->module_ref == d_.kernel_sepia);
		    itype = TypeOf(gproc);
		    if (!GDisj(gflags) && state == Regular && itype != GOAL_CUT)
			clds->block++;
		    if (itype == REGULAR)
		    {
			cut_var_valid = 0;
		    }
		    clds->global = clds->s = 0;
		    goaln++;
#ifdef DEBUG_COMP
		    if (debug_gstates)
		    {
			p_fprintf(current_err_,
			    "%s/%d\t%d\t%d\t%5o\t",
			    DidName(gval),
			    DidArity(gval),
			    clds->block,
			    goaln,
			    gflags);
			ec_flush(current_err_);
		    }
#endif
		    if (state != Goal_Cut)
		    {
			Gc_Test(clds, goaln);
		    }
		}

		switch (GLast(gflags) | itype | state)
		{
		case SIMPLE After Head:
		    Escape_(gproc, goal, gval, clds, procedure, willwake,
			gflags, state);
		    state = ExtHead;
		    break;

		case SIMPLE After ExtHead:
		case SIMPLE After NeckCut:
		case SIMPLE After Simple:
		case SIMPLE After DetSimple:
		    Escape_(gproc, goal, gval, clds, procedure, willwake,
			gflags, state);
		    break;

		case SIMPLE After NeckCutOnly:
		    Escape_(gproc, goal, gval, clds, procedure, willwake,
			gflags, state);
		    state = NeckCut;
		    break;

		case SIMPLE After Goal_Cut:
		    Cut_(active_size, debug, NotGLast(gflags), nested_cut)
		    Gc_Test(clds, goaln);
		    clds->pushed = 0;
		    Escape_(gproc, goal, gval, clds, procedure, willwake,
			gflags, state);
		    state = DetSimple;
		    break;

		case SIMPLE After Regular:
		    Escape_(gproc, goal, gval, clds, procedure, willwake,
			gflags, state);
		    state = Simple;
		    break;

		case REGULAR After ExtHead:
		case REGULAR After Head:
		    Neck_(shallow && !determinate, cut, arity);
		case REGULAR After NeckCut:
		case REGULAR After NeckCutOnly:
		    if (dfid)
		    {
			if (ClauseRegular(clds)) {
			    Dfid_TestL(dfid);
			}
			else if (ClauseRule(clds)) {
			    Dfid_Test(dfid);
			}
			_mark_initialized(dfid, clds);
		    }
		    Output_Mode(clds, procedure, code, WDisj(gflags, state),
			head_pass, willwake);
		    code = put_first_arguments(clds, procedure, code, NotLast,
			ToolProc(gproc) ? module : D_UNKNOWN);
		    goto _call_;

		case REGULAR After Goal_Cut:
		    Cut_(active_size, debug, NotGLast(gflags), nested_cut)
		    Gc_Test(clds, goaln);
		    clds->pushed = 0;
		case REGULAR After Regular:
		case REGULAR After Simple:
		case REGULAR After DetSimple:
		    code = put_arguments(goal, clds, procedure, code, NotLast,
			ToolProc(gproc) ? module : D_UNKNOWN);
_call_:		    if (gproc == cut_to_proc_)
			willwake = 0;
		    Buf_Alloc(procedure->codebuf, code, 2*L_PUT+2*L_DFID+L_RESS+L_CALL);
		    if (cortn && willwake) {
			Depth_(dfid);
		    }
		    if (!ignore) {
			Debug_Call(gproc, CALL_PORT, debug, gflags)
		    }
		    active_size = _environment_size(clds);
		    Depth_(dfid);
		    state = Regular;

		    /* C A L L */
		    if (gproc->module_ref == d_.kernel_sepia) {
			if (PriDid(gproc) == d_.call ||
			    PriDid(gproc) == d_.call_body ||
			    PriDid(gproc) == d_untraced_call)
			{
			    Space_Res(-clds->pushed, 2, null, debug || willwake,
				active_size)
			    clds->pushed = 0;
			    willwake = 1;
			    Store_3d(MoveAMAM, Address(2), Address(3))
			    Store_2d(SavecutAM, Address(4))
			    Store_2d(Metacall, Esize(active_size))
			    Debug_Exit(debug)
			    break;
			}
			else if (PriDid(gproc) == d_call_susp_)
			{
			    Space_Res(-clds->pushed, 3, null, debug,
				active_size)
			    clds->pushed = 0;
			    willwake = 1;
			    Store_2d(Suspension_call, Esize(active_size))
			    Debug_Exit(debug)
			    break;
			}
			else if (expand && PriDid(gproc) == d_.true0)
			{
			    Space_Res(-clds->pushed, 0, null, willwake,
				active_size)
			    clds->pushed = 0;
			    willwake = 0;
			    if (GLast(gflags) || GLastn(gflags)) {
				Debug_Exit(debug)
			    }
			    break;
			}
		    }
		    Space_(-clds->pushed)
		    clds->pushed = 0;
		    if (DirectLink(gval, procedure, gproc)) {
			Call_(CallA, determinate)
			Set_Label(procedure->start)
			Store_d(Esize(active_size))
		    } else if (LoadedPermanent(gproc)) {
			Call_(CallA, determinate)
			Store_2d(PCodeA(gproc), Esize(active_size))
		    }
		    else {
			Call_(CallP, determinate)
			Store_2d(PCodeP(gproc), Esize(active_size))
		    }
		    willwake = 0;
		    break;

		case GOAL_CUT After Head:
		    if (GCond(gflags))
			return (compiler_error(IF_CUT, clds, procedure, code));
		    Neckcut_(shallow, determinate, cut, debug);
		    state = NeckCutOnly;
		    Set_G_First_Top;
		    break;

		case GOAL_CUT After ExtHead:
		    if (GCond(gflags))
			return (compiler_error(IF_CUT, clds, procedure, code));
		    Neckcut_(shallow, determinate, cut, debug);
		    state = NeckCut;
		    Set_G_First_Top;
		    break;

		case GOAL_CUT After NeckCut:
		case GOAL_CUT After NeckCutOnly:
		case GOAL_CUT After Goal_Cut:
		    Set_G_First_Top;
		    break;

		case GOAL_CUT After Simple:
		case GOAL_CUT After Regular:
		case GOAL_CUT After DetSimple:
		    if (GCond(gflags))
			return (compiler_error(IF_CUT, clds, procedure, code));
		    state = Goal_Cut;
		    if (!GDisj(gflags))
			clds->block++;
		    active_size = _environment_size(clds);
		    Set_G_First_Top;
		    break;

		/* LAST SUBGOAL */

		case LastGoal SIMPLE After Head:
		case LastGoal SIMPLE After ExtHead:
		    Escape_(gproc, goal, gval, clds, procedure, willwake,
			gflags, state);
		case LastGoal NO_GOAL After Head:
		case LastGoal NO_GOAL After ExtHead:
		case LastGoal NO_GOAL After NeckCut:
		case LastGoal NO_GOAL After NeckCutOnly:
		    Neck_(shallow && !determinate, cut, arity);
		    Output_Mode(clds, procedure, code, WDisj(gflags, state),
			head_pass, willwake);
		    code = put_first_arguments(clds, procedure, code,
			Last, D_UNKNOWN);
		    Buf_Alloc(procedure->codebuf, code, L_CALL+L_RESS);
		case LastGoal NO_GOAL After Simple:
		case LastGoal NO_GOAL After Regular:
		    if (ClauseRegular(clds))
		    {
			Exit_(cortn, willwake, debug);
		    }
		    else
		    {
			Space_(-clds->pushed);
			Ret_(procedure->did, module, determinate,
			    cortn, willwake, debug);
		    }
		    break;

		case LastGoal SIMPLE After NeckCutOnly:
		case LastGoal SIMPLE After NeckCut:
		    Escape_(gproc, goal, gval, clds, procedure, willwake,
			gflags, state);
		    goto _retd_;

		case LastGoal GOAL_CUT After Head:
		case LastGoal GOAL_CUT After ExtHead:
		    Neckcut_(shallow, determinate, cut, debug);
		case LastGoal GOAL_CUT After NeckCut:
		case LastGoal GOAL_CUT After NeckCutOnly:
		    if (GCond(gflags))
			return (compiler_error(IF_CUT, clds, procedure, code));
_retd_:
		    Output_Mode(clds, procedure, code, WDisj(gflags, state),
			head_pass, willwake);
		    code = put_first_arguments(clds, procedure, code,
			Last, D_UNKNOWN);
		    Buf_Alloc(procedure->codebuf, code, L_CALL);
		    if (ClauseRegular(clds))
		    {
			Exitd_(cortn, willwake, debug);
		    }
		    else
		    {
			Space_(-clds->pushed);
			Retd_(procedure->did, module, cortn,
			    willwake, debug);
		    }
		    break;

		case LastGoal GOAL_CUT After Simple:
		case LastGoal GOAL_CUT After Regular:
		    if (!GDisj(gflags))
			clds->block++;
		case LastGoal GOAL_CUT After Goal_Cut:
		    if (GCond(gflags))
			return (compiler_error(IF_CUT, clds, procedure, code));
		    Gc_Test(clds, goaln);
		    Exitc_(cortn, willwake, debug);
		    state = Regular;
		    break;

		case LastGoal SIMPLE After Simple:
		case LastGoal SIMPLE After Regular:
		    Escape_(gproc, goal, gval, clds, procedure, willwake,
			gflags, state);
		    Buf_Alloc(procedure->codebuf, code, L_CALL);
		    Exit_(cortn, willwake, debug);
		    break;

		case LastGoal SIMPLE After Goal_Cut:
		    Cut_(active_size, debug, NotGLast(gflags), nested_cut)
		    Gc_Test(clds, goaln);
		    clds->pushed = 0;
		    Escape_(gproc, goal, gval, clds, procedure, willwake,
			gflags, state);
		    Buf_Alloc(procedure->codebuf, code, L_CALL);
		    Exitd_(cortn, willwake, debug);
		    state = Regular;
		    break;

		case LastGoal SIMPLE After DetSimple:
		    Escape_(gproc, goal, gval, clds, procedure, willwake,
			gflags, state);
		    Buf_Alloc(procedure->codebuf, code, L_CALL);
		    Exitd_(cortn, willwake, debug);
		    break;

		case LastGoal GOAL_CUT After DetSimple:
		    if (GCond(gflags))
			return (compiler_error(IF_CUT, clds, procedure, code));
		    Exitd_(cortn, willwake, debug);
		    if (!GDisj(gflags))
			clds->block++;
		    break;

		case LastGoal REGULAR After Head:
		case LastGoal REGULAR After ExtHead:
		    Neck_(shallow && !determinate, cut, arity);
		    flag = determinate;
		    goto _jmp_;

		case LastGoal REGULAR After NeckCut:
		case LastGoal REGULAR After NeckCutOnly:
		    flag = 1;
_jmp_:
		    if (dfid)
		    {
			if (ClauseRegular(clds)) {
			    Dfid_TestL(dfid);
			}
			else if (ClauseRule(clds)) {
			    Dfid_Test(dfid);
			}
			_mark_initialized(dfid, clds);
		    }
		    Output_Mode(clds, procedure, code, WDisj(gflags, state),
			head_pass, willwake);
		    code = put_first_arguments(clds, procedure, code, Last,
			ToolProc(gproc) ? module : D_UNKNOWN);
		    Buf_Alloc(procedure->codebuf, code, L_CALL+L_RESS);
		    if (!ClauseRegular(clds))
		    {
			/* if waking, state may be nondeterministic */
			Jmps_(gproc, gval, procedure, flag, module,
			    debug, -clds->pushed, ToolProc(gproc),
			    cortn, willwake, dfid, ignore, gflags);
		    }
		    else
		    {
			/*
			Space_(-clds->pushed)
			*/
			if (willwake) {
			    Depth_(dfid);
			}
			Chain_(gproc, gval, module, cortn, willwake, debug,
			    ignore, gflags);
		    }
		    willwake = 0;
		    break;

		case LastGoal REGULAR After Simple:
		case LastGoal REGULAR After Regular:
		    code = put_arguments(goal, clds, procedure, code, Last,
			ToolProc(gproc) ? module : D_UNKNOWN);
		    Buf_Alloc(procedure->codebuf, code, 2*L_DFID+L_RESS+L_CALL);
		    if (willwake) {
			Depth_(dfid);
		    }
		    /*
		    Space_(-clds->pushed)
		    */
		    Depth_(dfid);
		    Chain_(gproc, gval, module, cortn, willwake, debug, ignore,
			gflags);
		    willwake = 0;
		    break;

		case LastGoal REGULAR After Goal_Cut:
		    /* Chainc cannot be generated because of Put_unsafe_value */
		    Cut_(active_size, debug, NotGLast(gflags), nested_cut)
		    Gc_Test(clds, goaln);
		    clds->pushed = 0;
		case LastGoal REGULAR After DetSimple:
		    code = put_det_arguments(goal, clds, procedure, code,
			ToolProc(gproc) ? module : D_UNKNOWN);
		    Buf_Alloc(procedure->codebuf, code, L_DFID+L_CALL);
		    Depth_(dfid);
		    Chaind_(gproc, gval, module, cortn, willwake, debug, ignore,
			gflags)
		    willwake = 0;
		    break;

		default:
		    p_fprintf(current_err_,
			"compiler internal error- goal wrong state: %d After %d\n",
			GLast(gflags) | itype, state);
		    ec_flush(current_err_);
		    p_reset();
		}
_pop_:
		Pop_(gstate, gflags, label, goal, arg4);
	    }
	}
    }
}

/*
 * FUNCTION NAME:	_cut()
 *
 * PARAMETERS:
 *
 * DESCRIPTION:
 *
 * Generate the code for a cut.
 */
static vmcode *
_cut(int env_size, int nested_cut, int gflags, int debug, vmcode *code)
{
    if (GNested(gflags))
    {
	if (nested_cut)
	{
	    Debug_For_Call(d_.cut, d_.kernel_sepia,
		CUT_PORT, debug, gflags);
	    Store_3d(CutL, Esize(nested_cut), Esize(env_size));
	}
    }
    else
    {
	Debug_For_Call(d_.cut, d_.kernel_sepia,
	    CUT_PORT, debug, gflags);
	Store_2d(Cut, Esize(env_size))
    }
    return code;
}

/* Mark the cut variable as initialized. We don't
 * have its reference, so we have to look up its
 * number.
 */
static void
_mark_initialized(int var, cl_desc *clds)
{
    var_desc	*tvv;

    for (tvv = clds->variables; tvv; tvv = tvv->next_tvv)
    {
	if (VarPermNo(tvv) == var && VarPermanent(tvv))
	{
	    Set_Var_Perm_Source(tvv);
	    Set_Var_Allocated(tvv);
	    break;
	}
    }
}

/*
 * FUNCTION NAME:	free_sources(tvv, procedure)
 *
 * PARAMETERS:		tvv -	the variable descriptor
 *			procedure - the procedure descriptor
 *
 * DESCRIPTION:
 *
 * After all operations with a variable are finished, all the places
 * where is occurs are released to re-use. There must be at least one
 * source defined.
 */
void
free_sources(var_desc *tvv, proc_desc *procedure)
{
    struct var_ch      *ptr = &tvv->source;
    arg_desc		*gargs = procedure->gargs;
    uword              source;

    do
    {
	source = ptr->cont;
	if (IsA(source))
	{
	    Set_Arg_Free(gargs + ArgNo(source));
	}
	else if (IsRT(source))
	{
	    Set_Reg_Free(procedure->regs, NumberOf(source));
	}
	ptr = ptr->next;
    } while (ptr);
}


/*
 * FUNCTION NAME:	_environment_size(clds)
 *
 * PARAMETERS:		clds	- clause descriptor
 *
 * DESCRIPTION:		This routine returns the number of permanent
 *			variables that are still in use in that block.
 *			It is easy to modify to return varaibles
 *			in use _after_ that block or the full environment size.
 */
static int
_environment_size(cl_desc *clds)
{
    var_desc	*tvv = clds->last_var;
    int		size = 0;

    /* return clds->envsize;	/* no trimming at all	*/
    while (tvv)
    {
	if (VarPermanent(tvv))
	{
	    if (tvv->blockn >= clds->block)	/* or '>' if full trimming */
	    {
		size = VarPermNo(tvv);
		break;
	    }
	}
	tvv = tvv->prev_tvv;
    }
    clds->last_var = tvv;
    return size;
}

/*
 * FUNCTION NAME:	clause_init(clds, procedure)
 *
 * PARAMETERS:		clds	  - clause descriptor
 *			procedure - procedure descriptor
 *
 * DESCRIPTION:		Initialization of some data before the compilation
 *			of a clause. This initialization must be done before
 *			the first clause and before each clause in sequence.
 */
clause_init(cl_desc *clds, proc_desc *procedure)
{
    clds->pushed = 0;
    clds->block = 1;
    Mark_Regs_Free(procedure->regs);
    if (ProcedureTrail(procedure)) {
	Set_Clause_Trail(clds);
    }
}

check_singletons(cl_desc *clds, int clause_number, char *where, proc_desc *procedure)
{
    var_desc		*tvv;
    int			singletons = 0;
    char		*var_name;
    arg_desc		*gargs = procedure->gargs;

    tvv = clds->variables;
    while (tvv)
    {
	if (VarVoid(tvv) && (IsName(tvv->header.tag) || IsMeta(tvv->header.tag)
		&& IsNamed(tvv->header.tag.kernel)))
	{
	    var_name = DidName(TagDid(tvv->header.tag.kernel));
	    if (*var_name != '_' &&
	       /* if we removed var(X), maybe we created a void variable */
	       !(	ClauseSpec(clds) &&
		    gargs[procedure->index].contents->val.ptr == (pword *) tvv
		))
	    {
		if (!singletons)
		{
		    p_fprintf(warning_output_,
			"*** Warning: Singleton variables in %sclause %d of %s/%d: %s",
			where,
			clause_number,
			DidName(procedure->did),
			DidArity(procedure->did),
			var_name);
		    singletons++;
		}
		else
		    p_fprintf(warning_output_, ", %s", var_name);
	    }
	}
	tvv = tvv->next_tvv;
    }
    if (singletons)
       (void) ec_newline(warning_output_);
}

static void
_codegen_error(proc_desc *procedure, int clause_number, char *error)
{
    p_fprintf(current_err_,
	"compiler error: clause %d of %s/%d - %s\n",
	clause_number,
	DidName(procedure->did),
	DidArity(procedure->did),
	error);
    ec_flush(current_err_);
}
