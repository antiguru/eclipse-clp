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
 * VERSION	$Id: compiler.h,v 1.1 2006/09/23 01:55:00 snovello Exp $
 */

/*
 * IDENTIFICATION		compiler.h
 *
 * DESCRIPTION
 *
 *	SEPIA COMPILER
 *
 * This file contains macro definitions that are common for the 2nd and 3rd
 * compilation pass.
 *
 *
 * CONTENTS:
 *
 *
 * REVISION HISTORY:
 *
 * AUTHOR	VERSION	 DATE	REASON
 * Micha Meier	2.2	20.7.89	created the file
 */

/*
 * DEFINES:
 */

/*
 * compiler options - flags in the compile_data structure
 */
#define FNODBG		0x001	/* no debugger instructions, no varnames */
#define FNOSTAT		0x002
#define FSYSTEM		0x004	/* system flag, no source */
#define FDUMPED		0x008
#define FDUMPING	0x010
#define FTERM		0x020
#define FSKIPPED	0x040	/* skipped */
#define FINTERFACE	0x080
#define FSILENT		0x100	/* no tracing */
#define LOUD		1
#define FEXPAND		0x200	/* in-line expansion */
#define FEOF		0x400	/* EOF already consumed */

#define OLD_DUMP_VERSION	4	/* last back-compatible dump */
#define NEW_DUMP_VERSION	5	/* new dump */
#define PVAL_NUMBER		5

#ifdef DEBUG_COMP
#define Assert(ex)	{if (!(ex)){(void) p_fprintf(current_err_, "Compiler internal error: file \"%s\":%d\n", __FILE__, __LINE__); p_reset();}}
#else
#define Assert(ex)
#endif

/* goal types */
#define SIMPLE          000
#define REGULAR         020
#define GOAL_CUT        040
#define NO_GOAL         060

/* state in the clause		*/
/* the order is significant	*/
#define	Head			00000
#define	ExtHead			00002
#define	NeckCut			00004
#define	NeckCutOnly		00006
#define	Simple			00010
#define	Regular			00012
#define	Goal_Cut		00014
#define	DetSimple		00016
#define HeadState(state)	((state) < Simple)
#define BodyState(state)	((state) >= Simple)

/* flags of the state */
#define GFLAGS			0177001
#define G_LAST			0000001
#define G_DISJ			0001000
#define G_NESTED		0002000
#define G_COND			0004000
#define G_CVALID		0010000
#define G_FIRST			0020000
#define G_LASTN			0040000
#define G_START			0100000
#define Reset_G_Last(f)		(f) &= ~(G_LAST|G_LASTN);
#define Set_G_Disj(f)		(f) |= G_DISJ;
#define Set_G_Nested(f)		(f) |= G_NESTED;
#define Set_G_Cond(f)		(f) |= G_COND;
#define GLast(f)		((f) & G_LAST)
#define GLastn(f)		((f) & G_LASTN)
#define NotGLast(f)		((f) & ~(G_LAST|G_LASTN))
#define GFirst(f)		((f) & G_FIRST)
#define NotGFirst(f)		((f) & ~G_FIRST)
#define Set_G_First(f)		(f) |= G_FIRST;
#define Set_G_Lastn(f)		(f) |= G_LASTN;
#define GDisj(f)		((f) & G_DISJ)
#define GNested(f)		((f) & G_NESTED)
#define GCond(f)		((f) & G_COND)
#define GCValid(f)		((f) & G_CVALID)
#define WDisj(f, c)		(GDisj(f) | (c))
#define ARG_DESC_SIZE	10L	/* default arity for gargs */
#define STACK_SIZE	20L	/* default stack size */

#define DYN_BUFFER_SIZE		1000	/* must be >= L_START */
#define STAT_BUFFER_SIZE	1000	/* must be >= L_START */

/* sizes to check for buffer end */
#define L_WORD		1				/* One arg */
#define L_DB		4				/* Debug */
#define L_DPORT		2				/* Debug_port */
#define L_LAB		3				/* Label */
#define L_BRA		2				/* Branch */
#define L_FAIL		1
#define L_OC		2
#define L_GC		3				/* Gc_test */
#define L_INIT		3				/* Initialize */
#define L_SC		2
#define L_RESS		4				/* Ress or Space */
#define L_DFID		2
#define L_NECK		L_SC

#define L_START		(PROC_PREFIX_SIZE+L_LAB+L_BRA+L_FAIL)
#define L_SOT		((L_LAB+L_TRY+1)*NTYPES+2+L_BRA+L_LAB)
#define L_SWITCH	((L_LAB+L_TRY+1)*3+2+L_BRA+L_LAB)
#define L_TRY		(6+L_DPORT+L_LAB)
#define L_GET		(4)
#define L_UNIFY		(8)		/* max of all R/W units-ModeTl+W_s */
#define L_PUT		(4)
#define L_PUTS		(3)
#define L_PUSH		(3)
#define L_CALL		(3+L_DB+L_GC)
#define L_ESC		(2)
#define L_CUT		(3+L_DB+L_DFID)

#define INDEX_TYPES		(NTYPES + 1)	/* incl TTVV */

#define ASSERTA			0
#define ASSERTZ			1

#define Resize_And_Copy_Gargs(gargs, arity, new_arity)		\
	    gargs = (arg_desc *) hg_resize((generic_ptr) gargs,	\
		    sizeof(arg_desc) * ((new_arity) + 11));\
	    GargsSize(gargs) = (new_arity) + 10;
#define GargsOverflow(gargs, arity)	(GargsSize(gargs) <= (arity))

#define Replace_Goal(old, neww, trail)				\
				if (trail)			\
				{				\
				    Trail_Pword(old);		\
				}				\
				old->val.ptr = neww;		\
				old->tag.kernel = TCOMP;

#define IsTvv(t)		((t).kernel == TTVV)
#define IsMetaTvv(v, t)		(IsTvv(t) && IsMeta(Tvv(v)->header.tag))
#define IsFirstMetaTvv(v, t)	(IsMetaTvv(v, t) && !VarMetaCst(Tvv(v)))
#define IsRule(clause)		(IsStructure(clause->tag)		\
				&&					\
				(clause->val.ptr->val.did == d_.rulech2))
#define MatchingBody(body)						\
		(IsStructure(body->tag)	&& (				\
/*		    body->val.ptr->val.did == d_matching_guard || */	\
		    body->val.ptr->val.did == d_matching_guard1))
	
#define MarkGoal(goal, val)	    (goal)->tag.kernel = TNIL | (val);
#define Set_Goal_Cut_Var(goal, var) MarkGoal(goal, (var) << 9)
#define Add_Goal_Cut_Var(goal, var) MarkGoal(goal, 0x100 | (var) << 9)
#define GoalMarked(goal)	    (IsNil((goal)->tag))
#define GoalCutVar(goal)	    (GoalMarked(goal) ? ((goal)->tag.kernel >> 9) : 0)
#define Set_Goal_Simple(goal)       MarkGoal(goal, 0x100)
#define GoalSimple(goal)	    (((goal)->tag.kernel & (TNIL | 0x100)) == (TNIL | 0x100))

#define TypeOf(proc)                                       	\
(                                                               \
        PriDid(proc) == d_.cut					\
	?                                                       \
	GOAL_CUT                                                \
	:                                                       \
	(                                                       \
	    (PriArgPassing(proc) == ARGSTACK)            		\
	    ?                                               	\
	    SIMPLE                                          	\
	    :                                               	\
	    REGULAR                                         	\
	)                                                       \
)

#define Negation(gval)	(gval == d_.naf || gval == d_.fail_if || gval == d_.not1)
#define Condition(gval)	(gval == d_.cond || gval == d_.softcut)

/* arity of a goal s.t. IsProc(goal->tag) */
#define GoalArity(goal)		DidArity(PriDid((pri *) ((goal)->val.did)))

/* The passes of the head unification */
#define HEAD			0
#define CST			1
#define VALUE			2
#define GROUND_STRUCTURE	3
#define NONGROUND_STRUCTURE	4
#define PREFIX			5
#define OUTPUT_NONVAR		6
#define VAR			7
#define STRUCTURE		8
#define BODY_SIMPLE		9
#ifdef OBJECTS
#define TYPED_VAR		10
#define HEAD_PASSES		11
#else
#define HEAD_PASSES		10
#endif

#define ALL_TYPES		(((TagMask(TTVV) << 1) - 1) & ~0xff)
#define VAR_TYPES		(TagMask(TTVV) | TagMask(TTMM))
#define NONVAR_TYPES		(ALL_TYPES & ~VAR_TYPES)
#define TTMM			TPTR
#define TagMask(typ)		(1 << (typ + 8))
#define Mark_Mask(mask, typ)	(mask) |= TagMask(typ);
#define MatchesMask(mask, typ)	((mask) & TagMask(typ))

/* A souvenir from the times where these two functions were different */
#define Puts_First_Arguments		puts_arguments
#define Puts_Next_Arguments		puts_arguments

/* MACROS TO ACCESS THE DESCRIPTORS */

/*
 **** PROCEDURE *****
 */
#define PR_SHALLOW			0x00001
#define PR_DELCL			0x00002
#define PR_MACRO			0x00004
#define PR_1INDEX			0x00008
#define PR_DEBUG			0x00010
#define PR_ERROR			0x00020
#define PR_FATAL			0x00040
#define PR_FAIL				0x00080
#define PR_SOT				0x00100
#define PR_SYSTEM			0x00200
#define PR_TRAIL			0x00400
#define PR_NOWARN			0x00800
#define PR_DET				0x01000
#define PR_DYNAMIC			0x02000
#define PR_MIN				0x04000
#define PR_MAX				0x08000
#define PR_TINDEX			0x10000 /* value index is not enough */
#define PR_VARTEST			0x20000 /* free or meta used directly */
#define PR_SILENT			0x40000
#define PR_EXPAND			0x80000

#define Set_Procedure_Delcl(prds)	(prds)->pflags |= PR_DELCL;
#define Reset_Procedure_Delcl(prds)	(prds)->pflags &= ~PR_DELCL;
#define Set_Procedure_Macro(prds)	(prds)->pflags |= PR_MACRO;
#define Set_Procedure_1Index(prds)	(prds)->pflags |= PR_1INDEX;
#define Set_Procedure_Debug(prds)	(prds)->pflags |= PR_DEBUG;
#define Set_Procedure_Error(prds)	(prds)->pflags |= PR_ERROR;
#define Set_Procedure_Fatal(prds)	(prds)->pflags |= PR_ERROR | PR_FATAL;
#define Set_Procedure_Fail(prds)	(prds)->pflags |= PR_FAIL;
#define Set_Procedure_SOT(prds)		(prds)->pflags |= PR_SOT;
#define Reset_Procedure_SOT(prds)	(prds)->pflags &= ~PR_SOT;
#define Set_Procedure_System(prds)	(prds)->pflags |= PR_SYSTEM;
#define Set_Procedure_Trail(prds)	(prds)->pflags |= PR_TRAIL;
#define Set_Procedure_No_Warn(prds)	(prds)->pflags |= PR_NOWARN;
#define Set_Procedure_Det(prds)		(prds)->pflags |= PR_DET;
#define Set_Procedure_Min(prds)		(prds)->pflags |= PR_MIN;
#define Set_Procedure_Max(prds)		(prds)->pflags |= PR_MAX;
#define Set_Procedure_TIndex(prds)	(prds)->pflags |= PR_TINDEX;
#define Set_Procedure_VarTest(prds)	(prds)->pflags |= PR_VARTEST;
#define Reset_Procedure_MinMax(prds)	(prds)->pflags &= ~(PR_MIN|PR_MAX);
#define ProcedureShallow(prds)		((prds)->pflags & PR_SHALLOW)
#define ProcedureDelcl(prds)		((prds)->pflags & PR_DELCL)
#define ProcedureMacro(prds)		((prds)->pflags & PR_MACRO)
#define Procedure1Index(prds)		((prds)->pflags & PR_1INDEX)
#define ProcedureDebug(prds)		((prds)->pflags & PR_DEBUG)
#define ProcedureError(prds)		((prds)->pflags & PR_ERROR)
#define ProcedureFatal(prds)		((prds)->pflags & PR_FATAL)
#define ProcedureFail(prds)		((prds)->pflags & PR_FAIL)
#define ProcedureSOT(prds)		((prds)->pflags & PR_SOT)
#define ProcedureSystem(prds)		((prds)->pflags & PR_SYSTEM)
#define ProcedureTrail(prds)		((prds)->pflags & PR_TRAIL)
#define ProcedureNoWarn(prds)		((prds)->pflags & PR_NOWARN)
#define ProcedureDet(prds)		((prds)->pflags & PR_DET)
#define ProcedureDynamic(prds)		((prds)->pflags & PR_DYNAMIC)
#define ProcedureMin(prds)		((prds)->pflags & PR_MIN)
#define ProcedureMax(prds)		((prds)->pflags & PR_MAX)
#define ProcedureMinMax(prds)		((prds)->pflags & (PR_MIN|PR_MAX))
#define ProcedureTIndex(prds)		((prds)->pflags & PR_TINDEX)
#define ProcedureVarTest(prds)		((prds)->pflags & PR_VARTEST)
#define Set_Procedure_Silent(prds)	(prds)->pflags |= PR_SILENT;
#define ProcedureSilent(prds)		((prds)->pflags & PR_SILENT)
#define Set_Procedure_Expand(prds)	(prds)->pflags |= PR_EXPAND;
#define ProcedureExpand(prds)		((prds)->pflags & PR_EXPAND)

/*
 **** CLAUSE *****
 */
#define CL_BODY_CUT			0x00000001
#define CL_REGULAR			0x00000002
#define CL_ALLOCATED			0x00000004
#define CL_TRAIL			0x00000008
#define CL_DET				0x00000010
#define CL_VARS				0x00000060
#define CL_MAYWAKE			0x00000080
#define CL_WILLWAKE			0x00000100
#define CL_AFTER_RETRY			0x00000200
#define CL_DISJ				0x00000400
#define CL_DYNAMIC			0x00000800
#define CL_RULE				0x00001000
#define CL_UNIF				0x00002000
#define CL_SPEC				0x00004000
#define CL_BUNIF			0x00008000
#define CL_TEST				0x00010000
#define CL_MATCHING			0x00020000
#define CL_BSKIP			0x00040000
#define CL_BOUND			0x00080000
#define CL_FIRSTCALL			0x00100000
#define CL_MIN				0x00200000
#define CL_MAX				0x00400000
#define CL_NECKCUT			0x00800000

#define CL_DCOND			0x00000020
#define CL_WAKE				0x00000040
#define CL_OC				CL_VARS

#define Set_Clause_Body_Cut(clds)	(clds)->cflags |= CL_BODY_CUT;
#define Reset_Clause_Body_Cut(clds)	(clds)->cflags &= ~CL_BODY_CUT;
#define Set_Clause_Regular(clds)	(clds)->cflags |= CL_REGULAR;
#define Set_Clause_Allocated(clds)	(clds)->cflags |= CL_ALLOCATED;
#define Set_Clause_Trail(clds)		(clds)->cflags |= CL_TRAIL;
#define Set_Clause_Det(clds)		(clds)->cflags |= CL_DET;
#define Reset_Clause_Det(clds)		(clds)->cflags &= ~CL_DET;
#define Set_Clause_Vars(val, clds)	\
			(clds)->cflags = ((clds)->cflags & ~CL_VARS) | (val);
#define Set_Clause_Rule(clds)		(clds)->cflags |= CL_RULE;
#define Set_Clause_Maywake(clds)	(clds)->cflags |= CL_MAYWAKE;
#define Set_Clause_Willwake(clds)	(clds)->cflags |= CL_WILLWAKE;
#define Reset_Clause_Willwake(clds)	(clds)->cflags &= ~CL_WILLWAKE;
#define Set_Clause_After_Retry(clds)	(clds)->cflags |= CL_AFTER_RETRY;
#define Set_Clause_Disj(clds)		(clds)->cflags |= CL_DISJ;
#define Set_Clause_Unif(clds)		(clds)->cflags |= CL_UNIF;
#define Set_Clause_Spec(clds)		(clds)->cflags |= CL_SPEC;
#define Set_Clause_Bunif(clds)		(clds)->cflags |= CL_BUNIF;
#define Set_Clause_Bskip(clds)		(clds)->cflags |= CL_BSKIP;
#define Set_Clause_Bound(clds)		(clds)->cflags |= CL_BOUND;
#define Reset_Clause_Bskip(clds)	(clds)->cflags &= ~CL_BSKIP;
#define Reset_Clause_Specs(clds)	\
    (clds)->cflags &= ~(CL_NECKCUT|CL_SPEC|CL_BSKIP|CL_TEST|CL_BUNIF|CL_BOUND);\
    (clds)->nomatch = 0;\
    (clds)->body = 0;
#define Set_Clause_Test(clds)		(clds)->cflags |= CL_TEST;
#define Set_Clause_Matching(clds)	(clds)->cflags |= CL_MATCHING;
#define Reset_Clause_Matching(clds)	(clds)->cflags &= ~CL_MATCHING;
#define Set_Clause_Firstcall(clds)	(clds)->cflags |= CL_FIRSTCALL;
#define Reset_Clause_Firstcall(clds)	(clds)->cflags &= ~CL_FIRSTCALL;
#define Set_Clause_Min(clds)		(clds)->cflags |= CL_MIN;
#define Set_Clause_Max(clds)		(clds)->cflags |= CL_MAX;
#define ClauseBodyCut(clds)		((clds)->cflags & CL_BODY_CUT)
#define ClauseRegular(clds)		((clds)->cflags & CL_REGULAR)
#define ClauseAllocated(clds)		((clds)->cflags & CL_ALLOCATED)
#define ClauseTrail(clds)		((clds)->cflags & CL_TRAIL)
#define ClauseDet(clds)			((clds)->cflags & CL_DET)
#define ClauseVars(clds)		((clds)->cflags & CL_VARS)
#define ClauseRule(clds)		((clds)->cflags & CL_RULE)
#define ClauseWillwake(clds)		((clds)->cflags & CL_WILLWAKE)
#define ClauseAfterRetry(clds)		((clds)->cflags & CL_AFTER_RETRY)
#define ClauseDynamic(clds)		((clds)->cflags & CL_DYNAMIC)
#define ClauseUnif(clds)		((clds)->cflags & CL_UNIF)
#define ClauseSpec(clds)		((clds)->cflags & CL_SPEC)
#define ClauseBunif(clds)		((clds)->cflags & CL_BUNIF)
#define ClauseBskip(clds)		((clds)->cflags & CL_BSKIP)
#define ClauseTest(clds)		((clds)->cflags & CL_TEST)
#define ClauseMatching(clds)		((clds)->cflags & CL_MATCHING)
#define ClauseBound(clds)		((clds)->cflags & CL_BOUND)
#define ClauseFirstcall(clds)		((clds)->cflags & CL_FIRSTCALL)
#define ClauseMin(clds)			((clds)->cflags & CL_MIN)
#define ClauseMax(clds)			((clds)->cflags & CL_MAX)
#define ClauseMinMax(clds)		((clds)->cflags & (CL_MIN | CL_MAX))
#define Set_Clause_Neckcut(clds)	(clds)->cflags |= CL_NECKCUT;
#define ClauseNeckcut(clds)		((clds)->cflags & CL_NECKCUT)

/*
 * Insert the clause descriptor at the beginning of the chain.
 * This macro can be executed only if there are some variables in the chain.
 */
#define Prepend_Tvv(tvv, clds)					\
	if ((tvv) != (clds)->variables)				\
	{							\
		/* first remove from the chain, it cannot be the first one */\
		(tvv)->prev_tvv->next_tvv = (tvv)->next_tvv;	\
		if ((tvv)->next_tvv)				\
			(tvv)->next_tvv->prev_tvv = (tvv)->prev_tvv;\
		/* put before the first item */			\
		(tvv)->next_tvv = (clds)->variables;		\
		(tvv)->prev_tvv = 0;				\
		(clds)->variables->prev_tvv = (tvv);		\
		/* and set the new beginning */			\
		(clds)->variables = (tvv);			\
	}

/*
 **** ARGUMENT *****
 */
#define ARG_STATE			0x0ff
#define ARG_FREE_STATE			0x0fe
#define HEAD_GROUND			0x100	/* ground structure flag */
#define BODY_GROUND			0x200
#define STATE_FREE			0x000
#define STATE_KEEP			0x001
#define STATE_OCCUPIED			0x002
#define STATE_UNIFY			0x004
#define STATE_SHARED			0x008
#define Set_Arg_State(arg, val)		\
		(arg)->state = (arg)->state & ~ARG_FREE_STATE | (int) (val);
#define Set_Arg_Head_Ground(arg)	(arg)->state |= HEAD_GROUND;
#define Set_Arg_Body_Ground(arg)	(arg)->state |= BODY_GROUND;
#define Set_Arg_Free(arg)		Set_Arg_State(arg, STATE_FREE)
#define Set_Arg_Occupied(arg)		Set_Arg_State(arg, STATE_OCCUPIED)
#define Set_Arg_Keep(arg)		(arg)->state |= STATE_KEEP;
#define Set_Arg_Unify(arg)		Set_Arg_State(arg, STATE_UNIFY)
#define Reset_Arg_Unify(arg)		(arg)->state &= ~STATE_UNIFY;
#define Set_Arg_Shared(arg)		Set_Arg_State(arg, STATE_SHARED)
#define Set_Arg_Done(arg)		Set_Arg_State(arg, STATE_KEEP)
#define Reset_Arg_State(arg)		(arg)->state &= ~ARG_STATE;

#define ArgHeadGround(arg)		((arg)->state & HEAD_GROUND)
#define ArgBodyGround(arg)		((arg)->state & BODY_GROUND)
#define ArgState(arg)			((arg)->state & ARG_STATE)
#define ArgUnify(arg)			((arg)->state & STATE_UNIFY)
#define ArgFree(arg)			(ArgState(arg) == STATE_FREE)
#define ArgOccupied(arg)		((arg)->state & STATE_OCCUPIED)
#define ArgShared(arg)			((arg)->state & STATE_SHARED)
#define ArgDone(arg)			((arg)->state & STATE_KEEP)

#define GargsSize(gargs)		(gargs[0].state)

/*
 **** HARDWARE REGISTER *****
 */
#define REG_FREE			0x1
#define RegAddr(regs, r)		((regs) + RegNo(r))
#define RegNo(r)			((r) & VAL_MASK)
#define Set_Reg_Free(regs, r)		RegAddr(regs, r)->flags |= REG_FREE;
#define Set_Reg_Occupied(regs, r)	RegAddr(regs, r)->flags &= ~REG_FREE;

#define RegFree(regs, r)		(RegAddr(regs, r)->flags & REG_FREE)

#define Rt1				(CONT_T | CONT_R | 1)
#if NREGTMP > 0
#define Mark_Regs_Free(regs)		Set_Reg_Free(regs, Rt1)
#else
#define Mark_Regs_Free(regs)		Set_Reg_Occupied(regs, Rt1)
#endif


/*
 **** VARIABLE *****
 */
#define VAR_VOID			0x0001
#define VAR_PERMANENT			0x0002
#define VAR_UNINIT			0x0004
#define VAR_GLOBAL1			0x0008	/* not a local ref    */
#define VAR_GLOBAL2			0x0010	/* 1st occ globalized */
#define VAR_OCCBA			0x0020
#define VAR_OCCC			0x0040
#define VAR_GROUND			0x0080
#define VAR_ALLOCATED			0x0100	/* already in Yi?     */
#define VAR_DISJ			0x0200	/* occurs in a disj   */
#define VAR_INIT			0x0400	/* initialized	      */
#define VAR_OCCSA			0x0800
#define VAR_ALLOCTMP			0x1000
#define VAR_BODY			0x2000	/* not in the 1st block */
#define VAR_OCCR			0x4000	/* first occ in regular */
#define VAR_METACST			0x8000	/* metaterm is constructed */

#define VarGoal(goal)			((uword) (goal))
#define Set_Var_Simple_Arg_Occ(tvv, no)  (tvv)->goaln = VarGoal(no);
#define VarSimpleArgOcc(tvv)		(VarGoal((var_desc *) (tvv)->goaln))

#define Set_Vflags(tvv, val)		(tvv)->vflags |= (val);
#define Reset_Vflags(tvv, val)		(tvv)->vflags &= ~(val);
#define Set_Var_Nonvoid(tvv)		Reset_Vflags(tvv, VAR_VOID);
#define Set_Var_Permanent(tvv)		Set_Vflags(tvv, VAR_PERMANENT)\
					(tvv)->vflags &= ~VAR_OCCC;
#define Set_Var_Global_Alloc(tvv)	Set_Vflags(tvv, VAR_GLOBAL2)
#define Reset_Var_Global_Alloc(tvv)	Reset_Vflags(tvv, VAR_GLOBAL2)
#define Set_Var_Uninit(tvv)		Set_Vflags(tvv, VAR_UNINIT)
#define Reset_Var_Uninit(tvv)		Reset_Vflags(tvv, VAR_UNINIT)
#define Set_Var_Ground(tvv)		Set_Vflags(tvv, VAR_GROUND)
#define Set_Var_Noalias(tvv)		Set_Vflags(tvv, VAR_GROUND | VAR_UNINIT)
#define Set_Var_Occ_Body_Arg(tvv)	Set_Vflags(tvv, VAR_OCCBA)
#define Set_Var_Occ_Compnd(tvv)		Set_Vflags(tvv, VAR_OCCC)
#define Reset_Var_Occ_Compnd(tvv)	Reset_Vflags(tvv, VAR_OCCC)
#define Set_Var_Occ_Simple_Arg(tvv)	Set_Vflags(tvv, VAR_OCCSA)
#define Reset_Var_Occ_Simple_Arg(tvv)	Reset_Vflags(tvv, VAR_OCCSA)
#define Set_Var_Allocated(tvv)		Set_Vflags(tvv, VAR_ALLOCATED)
#define Set_Var_Disj(tvv)		Set_Vflags(tvv, VAR_DISJ)
#define Set_Var_Perm_No(tvv, no)	Set_Var_Simple_Arg_Occ(tvv, no)
#define Set_Var_Global(tvv)		if (!VarDisj(tvv)) Set_Vflags(tvv, VAR_GLOBAL1)
#define Set_Var_Head_Global(tvv)	Set_Vflags(tvv, VAR_GLOBAL1)
#define Set_Var_Init(tvv)		Set_Vflags(tvv, VAR_INIT)
#define Reset_Var_Init(tvv)		Reset_Vflags(tvv, VAR_INIT)
#define Set_Var_Alloc_Temp(tvv)		Set_Vflags(tvv, VAR_ALLOCTMP)
#define Reset_Var_Alloc_Temp(tvv)	Reset_Vflags(tvv, VAR_ALLOCTMP)
#define Set_Var_Body(tvv)		Set_Vflags(tvv, VAR_BODY)
#define Set_Var_Occ_Regular(tvv)	Set_Vflags(tvv, VAR_OCCR)
#define Set_Var_Meta_Cst(tvv)		Set_Vflags(tvv, VAR_METACST)

#define Get_Vflags(tvv, val)		((tvv)->vflags & (val))
#define VarVoid(tvv)			Get_Vflags(tvv, VAR_VOID)
#define VarPermanent(tvv)		Get_Vflags(tvv, VAR_PERMANENT)
#define VarOccCompnd(tvv)		Get_Vflags(tvv, VAR_OCCC)
#define VarOccSimpleArg(tvv)		Get_Vflags(tvv, VAR_OCCSA)
#define VarBodyArg(tvv)			Get_Vflags(tvv, VAR_OCCBA)
#define VarUninit(tvv)			Get_Vflags(tvv, VAR_UNINIT)
#define VarGround(tvv)			Get_Vflags(tvv, VAR_GROUND)
#define VarOnlyGround(tvv)		(VarNoalias(tvv) == VAR_GROUND)
#define VarNoalias(tvv)			\
		    Get_Vflags(tvv, VAR_UNINIT | VAR_GROUND)
#define VarAllocated(tvv)		Get_Vflags(tvv, VAR_ALLOCATED)
#define VarSource(tvv)			((tvv)->source.cont)
#define VarVoid(tvv)			Get_Vflags(tvv, VAR_VOID)
#define VarGlobal(tvv)			Get_Vflags(tvv, VAR_GLOBAL1)
#define VarGlobalAlloc(tvv)		Get_Vflags(tvv, VAR_GLOBAL2)
#define VarDestination(tvv)		((tvv)->destination.cont)
#define VarPermNo(tvv)			VarSimpleArgOcc(tvv)
#define VarUnsafe(tvv)			\
		(Get_Vflags(tvv, VAR_OCCBA | VAR_GLOBAL1) == VAR_OCCBA)
#define VarDisj(tvv)			Get_Vflags(tvv, VAR_DISJ)
#define VarInit(tvv)			Get_Vflags(tvv, VAR_INIT)
#define VarAllocTemp(tvv)		Get_Vflags(tvv, VAR_ALLOCTMP)
#define VarBody(tvv)			Get_Vflags(tvv, VAR_BODY)
#define VarOccRegular(tvv)		Get_Vflags(tvv, VAR_OCCR)
#define VarMetaCst(tvv)			Get_Vflags(tvv, VAR_METACST)

/* these macros are used when the parameter is a value (in the 3rd pass) */
#define Tvv(val)			((var_desc *) ((val).ptr))

/* source and destination of a variable */
#define VAL_MASK			0x07ffffffL
#define CONT_G				0x08000000L
#define CONT_A				0x10000000L
#define CONT_T				0x20000000L
#define CONT_Y				0x40000000L
/* #define CONT_R			0x80000000L in gencode.h */
#define ContArg(i)			((i) | CONT_A)
#define ContTmp(i)			((i) | CONT_T)
#define ContRTmp(i)			((i) | CONT_T | CONT_R)
#define ContPerm(i)			((i) | CONT_Y)
#define ContGl(i)			((i) | CONT_G)
#define NumberOf(loc)			((long) ((loc) & VAL_MASK))
#define ArgNo(loc)			NumberOf(loc)
#define IsA(loc)			((loc) & CONT_A)
#define IsT(loc)			((loc) & CONT_T)
#define IsY(loc)			((loc) & CONT_Y)
#define IsR(loc)			((loc) & CONT_R)
#define IsRT(loc)			(IsT(loc) && IsR(loc))
#define IsG(loc)			((loc) & CONT_G)
#define Set_Var_Source(tvv, src)	(tvv)->source.cont = (src);\
					(tvv)->source.next = 0;
#define Set_Var_Destination(tvv, dest)	(tvv)->destination.cont = (dest);\
					(tvv)->destination.next = 0;
#define Set_Var_Perm_Source(tvv)	\
			Set_Var_Source(tvv, ContPerm(VarPermNo(tvv)))
#define Remove_Cont(ptr)				\
	if ((ptr)->next)				\
	{						\
	    (ptr)->cont = (ptr)->next->cont;		\
	    (ptr)->next = (ptr)->next->next;		\
	}						\
	else						\
	    (ptr)->cont = 0;



/* Push a new pair; link is supposed to be a structure element, and have
 * the size of a pword
 */
#define New_Pair(link, value)				\
	{						\
		struct var_ch	*v = (struct var_ch *) Gbl_Tg;\
		Gbl_Tg += (sizeof(struct var_ch) + sizeof(pword) - 1)/\
				sizeof(pword);\
		Check_Gc;				\
		v->cont = value;			\
		v->next = link;				\
		link = v;				\
	}

#define Add_Var_Source(tvv, src)			\
	if (!(tvv)->source.cont)			\
	{						\
		Set_Var_Source(tvv, src)		\
	}						\
	else						\
	{						\
		New_Pair((tvv)->source.next, src);\
	}

#define Add_Var_Destination(tvv, dest)			\
	if (!(tvv)->destination.cont)			\
	{						\
		Set_Var_Destination(tvv, dest)		\
	}						\
	else						\
	{						\
		New_Pair((tvv)->destination.next, dest);\
	}


/* auxiliary */
struct var_ch
{
    uword		cont;
    type		tag;
    struct var_ch	*next;
};

struct bl_size
{
    int			goal;
    type		tag;
    long		size;
    struct bl_size	*next;
};

struct pile
{
    struct cl_d	*first;
    struct cl_d	*last;
    struct cl_d *lastvar;	/* last clause with a matching type test */
    int		more_values;
    long	length;		/* number of all clauses */
    long	size;		/* number of different values */
    int		matched;
    int		not_matched;
};

struct compile_data {
    stream_id	nst;			/* stream if reading from one	*/
    pword	*term;			/* list element if a term	*/
    pword	*list;			/* result of macro expansion	*/
    pword	*next_clause;		/* first clause of the next procedure */
    long	line;			/* line number                  */
    long	next_line;		/* line number of next procedure */
    long	offset;			/* byte offset                  */
    long	next_offset;		/* byte offset of next procedure */
    long	flags;   
    pword	*clause;		/* parsed clause */
    int		macro;			/* was macro-expanded */
    int		dump_version;		/* of the .sd file		*/
    unsigned char pval[PVAL_NUMBER];	/* previous string value	*/
};

/*
 * TYPEDEFS:
 */
typedef long		m_item;

/*
 * The clause descriptor. It is used throughout the whole compilation.
 * Since it is on the global stack, it must be pword aligned.
 */
typedef struct cl_d
{
	pword		*clause;	/* the clause			*/
	type		desc_tag;	/* tag 				*/
	struct cl_d	*next;		/* next in sequence		*/
	type		tag;		/* of the indexed argument	*/
	value		val;		/* of the indexed argument	*/
	pword		*head;		/* clause head			*/
	pword		*body;		/* clause body if skipped	*/
	struct cl_d	*nextv;		/* next with the same type	*/
	vmcode		*entry;		/* entry address		*/
	vmcode		*indexed_entry;	/* entry when indexed		*/
	vmcode		*retry;		/* address of the indexing instr*/
	struct var_d	*variables;	/* variable chain		*/
	struct var_d	*last_var;	/* last active variable		*/
	struct bl_size	*gc_list;	/* list of GC sizes		*/
	long		block;		/* current block #		*/
	long		envsize;	/* size of the environment	*/
	long		cflags;		/* clause flags			*/
	long		extreme;	/* min or max for integer range */
	uint32		nomatch;	/* types which may not match	*/
	uint16		reg_arity;	/* arity of the first regular goal */
	uint16		global;		/* number of global temps & TG	*/
	short		pushed;		/* how many Ti's pushed		*/
	uint16		s;		/* increment of S		*/
} cl_desc;

/*
 * The argument descriptor. It keeps information from the second pass
 * up to the end of the code generation for the first clause block.
 * The first element of the descriptor array (offset 0) contains
 * the size of the array in the 'state' field.
 * It is also used for indexing, to keep the info about generated
 * switches on multiple arguments, then 'state' is the size of the switch
 * and label points to the argument of its Branch instruction.
 * 
 */
typedef struct
{
	pword		*contents;	/* what is in the argument	*/
	pword		*target;	/* what will go to the argument	*/
	long		state;		/* state and flags		*/
	vmcode		*addr;		/* pointer to the Branch argument*/
} arg_desc;

/*
 * The descriptor of a hardware register, currently used only for the
 * rT variable(s). It only remembers whether the register is free or not,
 * the actual contents of the register is not needed.
 */
typedef struct
{
	short		flags;
} reg_desc;

/*
 * The variable descriptor. It keeps variable information from the 2nd pass
 * up to the end of the compilation. The descriptor are double-linked;
 * one link is actually enough, the two links make it possible to sort
 * the variables in the order of appearance in the blocks, which can be used
 * to number the permanent variables.
 */
typedef struct var_d
{
	uint32		vflags;
	long		goaln;		/* simple goal no. and perm. no. */
	type		tag;		/* for GC			 */
	pword		header;		/* to save previous value	 */
	struct var_ch	source;		/* where the variable is located */
	struct var_ch	destination;	/* where it should go		 */
	struct var_d	*prev_tvv;
	struct var_d	*next_tvv;
	pword		*top_disj;	/* for savecut variables	 */
	long		blockn;		/* last block occurrence	 */
	long		counter;	/* occurrences in compnd and simple */
} var_desc;

/*
 * The procedure descriptor.
 */
typedef struct
{
	cl_desc		*clauses;	/* clause list			*/
	cl_desc		*delay_clauses;	/* delay clause list		*/
	arg_desc	*gargs;		/* argument descriptors		*/
	reg_desc	*regs;		/* register descriptors		*/
	int		*head_pass;	/* head unification classif	*/
	long		*stack;		/* to compile clause body	*/
	long		*stack_end;	/* its end			*/
	struct pile	types[INDEX_TYPES];/* lists with the same type	*/
	Buf_Declare(codebuf);

	pri		*proc;		/* proc table descriptor	*/
	int		arity;		/* procedure arity		*/
	dident		did;		/* procedure DID		*/
	dident		module;		/* DID of its home module	*/
	type		module_tag;	/* module's tag marked or not	*/
	vmcode		*start;		/* code start			*/
	vmcode		*var_path;	/* start of the variable path	*/
	long		clauseno;	/* number of clauses		*/
	cl_desc		*catchall;	/* last true var clause		*/
	long		index_size;	/* not used			*/
	long		size;		/* really occupied size		*/
	long		saved_size;	/* to be able to reset cl. code	*/
	int		index;		/* indexed argument		*/
	m_item		mode;		/* mode declaration		*/
	dident		fid;		/* DID of the file name		*/
	uword		lid;		/* line number			*/
	uword		cid;		/* compilation ID#		*/
	uword		bid;		/* byte offset in the file	*/
	int		error;		/* error number if error	*/
	long		pflags;		/* procedure flags		*/
	int		var_types;	/* types of variable clauses	*/
	int		spec_number;	/* number of type test clauses	*/
	int		var_number;	/* number of variable clauses	*/
	cl_desc		*first_trailing;/* trailing var block		*/
	int		ntypes;		/* number of different clause types */
	int		ctype;		/* type of last nonvar clause	*/
	long		min;		/* minimum for integer range	*/
	long		max;		/* maximum for integer range	*/
	int		not_indexed;	/* types that are not indexed   */
} proc_desc;


/*
 * EXTERNAL FUNCTION DECLARATIONS: 
 */

/*
 * EXTERNAL VARIABLE DECLARATIONS: 
 */
extern dident		d_matching_guard1,
			d_matching_guard;
