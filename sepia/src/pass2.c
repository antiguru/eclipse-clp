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
 * VERSION	$Id: pass2.c,v 1.2 2007/07/03 00:10:30 jschimpf Exp $
 */

/*
 * IDENTIFICATION		pass2.c
 *
 * DESCRIPTION	
 *
 *	SEPIA COMPILER
 *
 *   This file contains routines that concern the second pass of the
 * compilation. They scan the clause, build data structures used
 * by the code generation (3rd pass) and collect some additional
 * information about the clause - variable classification and clause
 * classification.
 *
 *   Several details that concern all the involved procedures:
 *	- all reference chains are replaced by direct references/values.
 *	  This is because usually there are no chains, except for
 *	  some weird asserted clauses and so the code generator does
 *	  not have to dereference again.
 *	- the free variables are bound to a variable descriptor structure
 *	  using a TVV flag, so that finding the corresponding descriptor is
 *	  a constant operation. During the pass, variables that occur
 *	  in a new block are inserted at the beginning of the variable
 *	  chain so that finally the list of variables is sorted
 *	  by the block appearance.
 *	- due to the previous two operations, no references at all occur
 *	  in the clause structure after the second pass, since references
 *	  to free variables are replaced by a direct clause descriptor address
 *	- goal transformation is done in the 2nd pass, e.g. X => call(X)
 *	  etc. These should eventually be moved to a preprocessor.
 *	- the goal functors are replaced by a pword with the tag TPROC
 *	  whose value is the procedure entry address; therefore the
 *	  procedure table lookup does not have to be repeated in the 3rd pass
 *	- all changes of the source clauses are trailed and undone
 *	  at the end of the clause compilation; this is necessary when
 *	  dynamic clauses are compiled in assert/1
 *	- all terms are represented by pword pointers, which makes it
 *	  possible to treat structures and atoms similarly. A structure
 *	  is represented by a pointer to its functor, so it is significant
 *	  here, which may not be the case elsewhere in the system.
 * At the end of the second pass, the following data for inline expansion
 * is generated:
 *	- Inline expanded goals do not have the TPROC tag, the DID is kept.
 *	- Simple if-then-else and simple not are marked as SimpleGoal.
 *	- Goals which (or whose children) need a cut variable have it
 *	  as GoalCutVar.
 *
 *
 * AUTHOR	VERSION	 DATE	REASON
 * Micha Meier	2.2	20.7.89	created the file
 */

 /*
  * INCLUDES:
  */
#include        "config.h"
#include	"sepia.h"		/* for Dereference_	*/
#include	"types.h"
#include        "embed.h"
#include	"mem.h"
#include	"dict.h"
#include	"emu_export.h"
#include	"module.h"
#include	"compiler.h"
#include	"database.h"		/* for DEFAULT_FLAGS	*/
#include	"error.h"
#include	"io.h"			/* for StreamNr		*/
#include	"property.h"		/* for TransfTermIn	*/

 /*
  * TYPEDEFS:
  */
/* the structure for the private pass2 data */
typedef struct pass2_d {
    long		goaln;		/* current goal number		*/
    long		top_goaln;	/* conjunction goal number	*/
    long		gc_goal;	/* goal# for gc_test instr.	*/
    long		gc_size;	/* size used for gc_test gen.	*/
    var_desc		*savecut_var;	/* Y1 for the normal cut	*/
    char		gtype;		/* type of the current goal	*/
    char		call;		/* size of one goal call	*/
    short		pflags;		/* flags			*/
    struct bl_size	**gc_list;	/* list of GC sizes		*/
    int			block;		/* block number			*/
    unsigned		reg:8;		/* contains a regular goal	*/
    unsigned		bind:8;		/* number of unified simple args*/
} pass2_desc;

 /*
  * DEFINES:
  */
#define P2_DISJ				0x0001
#define P2_LAST				0x0002
#define P2_ARROW			0x0004
#define P2_UNIF				0x0008
#define P2_NOT				0x0010
#define P2_UNINIT			0x0020
#define P2_GROUND			0x0040
#define P2_CUT				0x0080
#define P2_META				0x0100
#define P2_NONVOID			0x0200
#define W_DISJ				P2_DISJ
#define W_PUT				0x0002
#define W_PUTS				0x0004
#define W_PUSH				0x0008
#define W_BIND				0x0010
#define Set_Pass2_Disj(p2ds)		(p2ds)->pflags |= P2_DISJ;
#define Reset_Pass2_Disj(p2ds)		(p2ds)->pflags &= ~P2_DISJ;
#define Set_Pass2_Last(p2ds)		(p2ds)->pflags |= P2_LAST;
#define Reset_Pass2_Last(p2ds)		(p2ds)->pflags &= ~P2_LAST;
#define Set_Pass2_Arrow(p2ds)		(p2ds)->pflags |= P2_ARROW;
#define Reset_Pass2_Arrow(p2ds)		(p2ds)->pflags &= ~P2_ARROW;
#define Reset_Pass2_Unif(p2ds)		(p2ds)->pflags &= ~P2_UNIF;
#define Set_Pass2_Unif(p2ds)		(p2ds)->pflags |= P2_UNIF;
#define Reset_Pass2_Not(p2ds)		(p2ds)->pflags &= ~P2_NOT;
#define Exchange_Pass2_Not(p2ds)	(p2ds)->pflags ^= P2_NOT;
#define Set_Pass2_Not(p2ds)		(p2ds)->pflags |= P2_NOT;
#define Set_Pass2_Uninit(p2ds)		\
    (p2ds)->pflags = ((p2ds)->pflags & ~(P2_UNINIT | P2_GROUND)) | P2_UNINIT;
#define Reset_Pass2_Noalias(p2ds)	(p2ds)->pflags &= ~(P2_UNINIT | P2_GROUND);
#define Set_Pass2_Noalias(p2ds)		(p2ds)->pflags |= P2_UNINIT | P2_GROUND;
#define Set_Pass2_Ground(p2ds)		\
    (p2ds)->pflags = ((p2ds)->pflags & ~(P2_UNINIT | P2_GROUND)) | P2_GROUND;
#define Set_Pass2_Cut(p2ds)		(p2ds)->pflags |= P2_CUT;
#define Set_Pass2_Meta(p2ds)		(p2ds)->pflags |= P2_META;
#define Reset_Pass2_Meta(p2ds)		(p2ds)->pflags &= ~P2_META;
#define Set_Pass2_Nonvoid(p2ds)		(p2ds)->pflags |= P2_NONVOID;
#define Reset_Pass2_Nonvoid(p2ds)	(p2ds)->pflags &= ~P2_NONVOID;
#define Pass2Disj(p2ds)			((p2ds)->pflags & P2_DISJ)
#define Pass2Last(p2ds)			((p2ds)->pflags & P2_LAST)
#define Pass2Arrow(p2ds)		((p2ds)->pflags & P2_ARROW)
#define Pass2Unif(p2ds)			((p2ds)->pflags & P2_UNIF)
#define Pass2Not(p2ds)			((p2ds)->pflags & P2_NOT)
#define Pass2Uninit(p2ds)		\
			(((p2ds)->pflags & (P2_UNINIT|P2_GROUND)) == P2_UNINIT)
#define Pass2Ground(p2ds)		\
			(((p2ds)->pflags & (P2_UNINIT|P2_GROUND)) == P2_GROUND)
#define Pass2Noalias(p2ds)		((p2ds)->pflags & (P2_GROUND|P2_UNINIT))
#define Pass2Cut(p2ds)			((p2ds)->pflags & P2_CUT)
#define Pass2Meta(p2ds)			((p2ds)->pflags & P2_META)
#define Pass2Nonvoid(p2ds)		((p2ds)->pflags & P2_NONVOID)

#define New_Block(p2ds)			if (!Pass2Disj(p2ds)) (p2ds)->block++;

#define Size(gtype, block)	((gtype) == SIMPLE ?			       \
				    ((block) > 1 ? GC_MAX_PUTS : GC_MAX_FACT): \
				    ((block) > 1 ? GC_MAX_PUT : GC_MAX_HEAD))

/*
 * The following macro eliminates variable chains
 * as the compiler expects chains of maximum length 1!
 */
#define Remove_Ref_Chains(ref, trail)			\
    if (IsRef((ref)->tag))				\
    {							\
                pword	*de_ref;			\
        de_ref = ref;                 			\
        Dereference_(de_ref);              		\
        if (!IsRef(de_ref->tag) || ref->val.ptr != de_ref)\
	/* not a ref to a free var */			\
        {						\
            if(IsRef(de_ref->tag))			\
            {						\
                if (trail)				\
		    {Trail_Pointer(&(ref->val.ptr));}	\
                ref->val.ptr = de_ref;			\
            } else					\
            {						\
		if (trail)				\
		    {Trail_Pword(ref);}			\
                ref->tag.all = de_ref->tag.all;		\
                ref->val.all = de_ref->val.all;		\
            }						\
        }						\
    }

#define Make_Structure(did1, ptr)\
	(ptr) = Gbl_Tg;\
	Gbl_Tg += DidArity(did1) + 1;\
	Check_Gc;\
	(ptr)->val.did = (did1);\
	(ptr)->tag.kernel = TDICT;


/*
  The following is used to get a valid reference pri (in PRI)
  PRI is a *pri, F is the did of the functor, M the did of the module
  and T the module tag.
  it make use of the variable res of type int to get the error.
*/
#define Get_Reference_Procedure(PRI, F, P)				\
	{PRI = visible_procedure(F, (P)->module, (P)->module_tag, PRI_CREATE);	\
	if (!PRI)							\
	{								\
	    Get_Bip_Error(res);						\
	    Bip_Error(res);						\
	}								\
	else if (PRI != (P)->proc)					\
	{								\
	    Pri_Set_Reference(PRI);					\
	}}

 /*
  * EXTERNAL VARIABLE DECLARATIONS:
  */
extern void	reset_args(arg_desc *gargs);

 /*
  * EXTERNAL VARIABLE DEFINITIONS: 
  */
static pword	*_transform_goal(pword *goal);
extern pri	*softcut_proc_;

 /*
  * STATIC VARIABLE DEFINITIONS: 
  */
static int	_body_pass2(pword *body, cl_desc *clds, pass2_desc *p2ds, proc_desc *procedure, long int global_size),
		_goal_pass2(pword *goal, cl_desc *clds, pass2_desc *p2ds, proc_desc *procedure, var_desc *cut_var),
		_perm_alloc(cl_desc *clds),
		_compound_pass2(pword *term, cl_desc *clds, pass2_desc *p2ds, proc_desc *procedure, arg_desc *gargs, int *groundflg, int where),
		_check_args(pword *ptr1, pword *ptr2, int arity, int trail);
static long	_head_pass2(cl_desc *clds, pass2_desc *p2ds, proc_desc *procedure);
static void	_gc_test(pass2_desc *p2ds);
static var_desc
		*_create_tvv(pword *ref, cl_desc *clds, int block),
		*_make_cut_var(int block),
		*_make_localcut_var(int block, cl_desc *clds, pword *goal);
static void	_print_pass2_info(cl_desc *clds, proc_desc *procedure);

int		debug_pass2 = 0;

 /*
  * FUNCTION DEFINITIONS: 
  */

/*
 * FUNCTION NAME:
 *
 * PARAMETERS:
 *
 * DESCRIPTION:
 * Go through the clause, collecting the necessary information.
 * The input is the clause descriptor address (without any reference chains).
 * The routine returns zero on a successfull completion, otherwise
 * a negative error code which can be (and is) used to raise an exception.
 */
int
clause_pass2(cl_desc *clds, proc_desc *procedure)
{
    pass2_desc		p2ds;
    int			*head_pass = procedure->head_pass;
    pword		*head = clds->clause;	/* used also for the clause */
    pword		*body;
    int			n;
    long		global_size;		/* for GC		*/

    if (IsRef(head->tag))
        return ILLEGAL_HEAD;
    for (n = 0; n < HEAD_PASSES; n++)
	head_pass[n] = 0;
    reset_args(procedure->gargs);
    if (ProcedureTrail(procedure)) {
	Set_Clause_Trail(clds);
    }
    if (IsRule(head))
    {
        head = head->val.ptr + 1;
	if (ClauseBskip(clds))
	    body = clds->body;
	else
	    body = head + 1;
        Remove_Ref_Chains(head, ClauseTrail(clds));
	if (body) {
	    Remove_Ref_Chains(body, ClauseTrail(clds));
	    if (MatchingBody(body)) {
		Set_Clause_Matching(clds);   /* here because of dynamic procs */
	    }
	}
    }
    else
        body = 0;
    if (IsList(head->tag))		/* transform a list into ./2 */
    {
        Replace_Goal(head, _transform_goal(head), ClauseTrail(clds));
    }
    if (IsStructure(head->tag))
    {
        head = head->val.ptr;
	if (DidArity(head->val.did) >= MAXARITY)
	    return ILLEGAL_HEAD;
    }
    else if (!IsAtom(head->tag) && !IsNil(head->tag))
        return ILLEGAL_HEAD;
    clds->head = head;
    p2ds.pflags = P2_UNIF;
    p2ds.block = 1;
    p2ds.goaln = 0;
    p2ds.top_goaln = 1;
    global_size = _head_pass2(clds, &p2ds, procedure);

    n = _body_pass2(body, clds, &p2ds, procedure, global_size);
    if (n)
	return n;

    /* end up the pass with the housekeeping information */
    if (clds->envsize)
    {
        Set_Clause_Regular(clds);
    }
    clds->pushed = p2ds.block;	/* to check internal errors */

    /* sum up all the instructions that may cause failure */
    head_pass[HEAD] = head_pass[CST] + head_pass[VALUE] +
		head_pass[GROUND_STRUCTURE] + head_pass[NONGROUND_STRUCTURE];
#ifdef OBJECTS
    head_pass[HEAD] += head_pass[TYPED_VAR];
#endif
    if (debug_pass2)
        _print_pass2_info(clds, procedure);
    return 0;
}

/*
 * FUNCTION NAME:	_head_pass2(clds, procedure)
 *
 * PARAMETERS:		clds	- pointer to the clause descriptor
 *			procedure - pointer to the procedure descriptor
 *
 * DESCRIPTION:
 * Scan the clause head. This routine remembers the position of arguments
 * in the clause head so that various optimizations of data moves
 * can be performed.
 * The input term is expected to be dereferenced.
 * It returns the size of the head in instructions.
 */
static long
_head_pass2(cl_desc *clds, pass2_desc *p2ds, proc_desc *procedure)
{
    pword		*term = clds->head;
    arg_desc		*gargs = procedure->gargs;
    int			*head_pass = procedure->head_pass;
    m_item		mode_decl = procedure->mode;
    int			i;
    int			arity;
    pword		*arg;
    int			groundflg;
    arg_desc		*ards = gargs;
    var_desc		*tvv;
    long		size = 0;
    int			mode;		/* for a given argument	*/
    int			trail = ClauseTrail(clds);
    int			index = procedure->index;

    arity = (IsNil(term->tag) ? 0 : DidArity(term->val.did));
    for (i = 1; i <= arity; i++)
    {
        arg = ++term;
        Remove_Ref_Chains(arg, trail);
        ards++;
	ards->state = 0;
	ards->target = 0;
	mode = Mode(i, mode_decl);
        if (IsRef(arg->tag) && !IsMeta(arg->val.ptr->tag))/* first occurrence */
        {
            tvv = _create_tvv(arg, clds, 1);
	    if (IsName(tvv->header.tag))
		size++;
            ards->contents = arg;
            Set_Var_Source(tvv, ContArg(i));
	    if (GroundMode(mode)) {
		Set_Var_Ground(tvv);
	    }
	    else if (OutputMode(mode)) {
		Set_Var_Uninit(tvv);
	    }
	    else if (NoaliasMode(mode)) {
		Set_Var_Noalias(tvv);
	    }
            /*
             * The default is a void variable; for typed variables
             * or similar stuff which requires some actions even
             * for void variables, the argument must be set
             * 'occupied'
             */
	    Set_Arg_Free(ards);
        }
        else if (IsTvv(arg->tag))	/* subsequent */
        {
            tvv = (var_desc *) arg->val.ptr;
	    if (!OutputMode(mode) && !VarUninit(tvv))
		Reset_Pass2_Unif(p2ds);
            Set_Var_Nonvoid(tvv);
	    tvv->counter++;
            ards->contents = arg;
	    if (IsMeta(tvv->header.tag)) {
		if (VarSource(tvv) && tvv->source.next) {
		    head_pass[VALUE]++;
		    Set_Arg_Unify(ards);
		}
		Add_Var_Source(tvv, ContArg(i));
                Set_Arg_Occupied(ards);
	    }
            else if (VarSource(tvv))		/* occurs in head arg */
            {
                int	j = ArgNo(VarSource(tvv));

                Set_Arg_Unify(ards);
		Set_Arg_Occupied(gargs + j);
                if (OutputMode(mode))
                {
		    head_pass[OUTPUT_NONVAR]++;
                }
                else if (GroundMode(mode) || OutputMode(Mode(j, mode_decl)))
                {	/* take this one as source */
                    Set_Var_Source(tvv, ContArg(i));
		    Set_Arg_Unify(gargs + j);
		    Set_Arg_Occupied(ards);
		    if (OutputMode(Mode(j, mode_decl)))
			head_pass[OUTPUT_NONVAR]++;
		    else
			head_pass[VALUE]++;
		    if (GroundMode(mode)) {
			Set_Var_Ground(tvv);
		    }
		    else if (OutputMode(mode)) {
			Set_Var_Uninit(tvv);
		    }
		    else if (NoaliasMode(mode)) {
			Set_Var_Noalias(tvv);
		    }
                }
		else	/* none is output */
		    head_pass[VALUE]++;
            }
            else /* only in structures */
            {
		Set_Var_Source(tvv, ContArg(i));
                Set_Arg_Occupied(ards);
		if (GroundMode(mode)) {
		    Set_Var_Ground(tvv);
		}
		else if (OutputMode(mode)) {
		    Set_Var_Uninit(tvv);
		}
		else if (NoaliasMode(mode)) {
		    Set_Var_Noalias(tvv);
		}
            }
        }
        else if (IsStructure(arg->tag) || IsList(arg->tag) || IsRef(arg->tag))
        {
            ards->contents = arg;
            Set_Arg_Occupied(ards);
            groundflg = 1;
	    if (GroundMode(mode))
		Set_Pass2_Ground(p2ds)
	    else if (OutputMode(mode))
		Set_Pass2_Uninit(p2ds)
	    else if (NoaliasMode(mode))
		Set_Pass2_Noalias(p2ds)
	    else
		Reset_Pass2_Noalias(p2ds)
	    Reset_Pass2_Nonvoid(p2ds);
	    if (IsRef(arg->tag) && ClauseMatching(clds) && OutputMode(mode)) {
		Set_Mode(i, procedure->mode, ANY);
		mode = ANY;
	    }
            size += _compound_pass2(arg, clds, p2ds, procedure, gargs,
                    &groundflg, 0);
	    if (i != index && OutputMode(mode))
	    {
		if (groundflg)
		{
		    Set_Arg_Head_Ground(ards);
		    head_pass[OUTPUT_NONVAR]++;
		}
		else
		    head_pass[STRUCTURE]++;
	    }
            else 
	    {
		if (groundflg)
		{
		    Set_Arg_Head_Ground(ards);
		    head_pass[GROUND_STRUCTURE]++;
		}
		else
		    head_pass[NONGROUND_STRUCTURE]++;
		if (i != index) {
		    Reset_Pass2_Unif(p2ds)
		}
		else if (!OutputMode(mode)) {
		    if (Pass2Nonvoid(p2ds)) {
			if (IsTvv(arg->tag)) {
			    if (ClauseMatching(clds)) {
				Mark_Mask(clds->nomatch, TTMM)
			    }
			} else {
			    Mark_Mask(clds->nomatch, arg->tag.kernel)
			}
		    }
		}
	    }
	    if (IsTvv(arg->tag)) {
		Set_Var_Source(Tvv(arg->val), ContArg(i));
	    }
        }
        else /* constant */
        {
            ards->contents = arg;
            Set_Arg_Occupied(ards);
	    if (i != index && OutputMode(mode))
		head_pass[OUTPUT_NONVAR]++;
	    else
	    {
		head_pass[CST]++;
		if (i != index)
		    Reset_Pass2_Unif(p2ds);
	    }
	    if (IsAtom(arg->tag))
		{Set_Did_Stability(arg->val.did, DICT_CODE_REF)}
        }
    }
    /* clear arg(arity+1) as it might be used for =/2 expansion */
    (++ards)->state = 0;
    ards->target = 0;
    Reset_Pass2_Noalias(p2ds);
    return size;
}

/*
 * FUNCTION NAME:		_body_pass2
 *
 * PARAMETERS:
 *
 * DESCRIPTION:
 * Go through the clause body. The recognized grammar is
 *	clause ::= head | head ":-" body
 *	body ::= goal
 *	goal ::= goal "," goal | goal ";" goal |
 *		goal "->" goal |term
 *	term ::= Constant | Compound
 * The input body can contain reference chains. The function returns 0
 * or error code.
 */
static int
_body_pass2(pword *body, cl_desc *clds, pass2_desc *p2ds, proc_desc *procedure, long int global_size)
{
    int			res;
    var_desc		*dfid_tvv;
    dident		wd;

    p2ds->gc_goal = 0;
    p2ds->gc_size = global_size;
    p2ds->savecut_var = 0;
    p2ds->gtype = SIMPLE;
    p2ds->call = ProcedureDebug(procedure) ? 3 : 1;
    p2ds->gc_list = &(clds->gc_list);
    p2ds->pflags &= P2_UNIF;
    Set_Pass2_Last(p2ds);
    p2ds->reg = 0;
    if (body)
    {
	if (IsStructure(body->tag)) {
	    wd = body->val.ptr->val.did;    
	    if (wd == d_matching_guard1)
		body = body->val.ptr + 1;
	}
	res = _goal_pass2(body, clds, p2ds, procedure, (var_desc *)0);
	if (res < 0)
	    return res;
	/* check if the clause is determinate wrt. indexing and cut */
	if (Pass2Unif(p2ds) && (Pass2Cut(p2ds) || ClauseNeckcut(clds))) {
	    Set_Clause_Unif(clds)	/* if the indexed argument is a	  */
	}				/* variable, it succeeds and cuts */
    }

    if (p2ds->block > 1)
    {
        Set_Clause_Regular(clds);
#ifdef DFID
	if (GlobalFlags & DFID_COMPILE) {
	    dfid_tvv = _make_localcut_var(p2ds->block, clds, (pword *) 0);
	    Set_Var_Permanent(dfid_tvv);
	}
#endif
    }
    /* now insert/create the variable for the cut handling,
     * it is Y1 and so we don't want it inside the chain
     */
    if (ClauseBodyCut(clds))
    {
        p2ds->savecut_var = _make_cut_var(p2ds->block);
	p2ds->savecut_var->next_tvv = clds->variables;
	p2ds->savecut_var->prev_tvv = 0;
	if (clds->variables)
	    clds->variables->prev_tvv = p2ds->savecut_var;
	clds->variables = p2ds->savecut_var;
    }

    p2ds->gc_size += _perm_alloc(clds);
    if (p2ds->gc_size > Size(p2ds->gtype, p2ds->block))
    {
	*(p2ds->gc_list) = (struct bl_size *) Gbl_Tg;
	Gbl_Tg = (pword *) (*(p2ds->gc_list) + 1);
	Check_Gc;
	(*(p2ds->gc_list))->tag.kernel = TBLSIZE;
	(*(p2ds->gc_list))->size = p2ds->gc_size;
	(*(p2ds->gc_list))->goal = p2ds->gc_goal;
	(*(p2ds->gc_list))->next = 0;
    }
    return 0;
}

/*
 * FUNCTION NAME:
 *
 * PARAMETERS:
 *
 * DESCRIPTION:
 * Go through a term which occurs at the goal level. The control structures
 * with functors ; , -> are handled separately in order to discover
 * a cut inside, which may require a permanent variable.
 * The input term may not be dereferenced. The flags are restored so that
 * they are not changed by this routine.
 * It returns the error code or 0.
 */
static int
_goal_pass2(pword *goal, cl_desc *clds, pass2_desc *p2ds, proc_desc *procedure, var_desc *cut_var)
{
    pri			*procindex;
    arg_desc		*gargs;
    arg_desc		*ards;
    var_desc		*tvv;
    int			*head_pass = procedure->head_pass;
    int			i;
    int			arity;
    pword		*arg;
    pword		*saved_goal;
    int			groundflg;
    int			gcsize = 0;
    int			comp_size;
    int			trail = ClauseTrail(clds);
    int			clause_arity;
    int			saved_pflags;
    int			res;
    int			old_type;
    int			saved_reg;
    type		ptag;
    int			transformed = 0;
    dident		wd;
    int			expanded = 0;

_start_:	/* TRO */
    Remove_Ref_Chains(goal, trail);
    if (IsRef(goal->tag))		/* first occurrence, as goal */
	return VARIABLE_GOAL;
    else if (IsTvv(goal->tag) || IsList(goal->tag))
    {
	Replace_Goal(goal, _transform_goal(goal), trail);
	goal = goal->val.ptr;
	ptag = goal->tag;
    }
    else if (IsStructure(goal->tag))
    {	/* the tag might be TNIL!! */

	saved_goal = goal;
	goal = goal->val.ptr;
	if (GoalMarked(goal))
	    ptag.kernel = TDICT;
	else
	   ptag = goal->tag;
	if (IsAtom(ptag) && DidArity(goal->val.did) >= MAXARITY)
	    return ILLEGAL_GOAL;
    }
    else
	ptag = goal->tag;

    if (IsAtom(ptag))
    {
	wd = goal->val.did;
	Get_Reference_Procedure(procindex, wd, procedure);
	arity = DidArity(wd);
    }
    else if (IsProc(ptag))
    {
	procindex = (pri *) goal->val.ptr;
	wd = PriDid(procindex);
	arity = DidArity(wd);
    }
    else if (IsNil(ptag))
    {
	Get_Reference_Procedure(procindex, d_.nil, procedure);
	wd = d_.nil;
	arity = 0;
    }
    else
	return ILLEGAL_GOAL;


    if (procindex->module_ref == d_.kernel_sepia)
    {
	if (Pass2Arrow(p2ds) && !Condition(procindex->did)) {
	    cut_var = 0;
	    Reset_Pass2_Arrow(p2ds);
	}
	else if (Pass2Not(p2ds) && !(Negation(procindex->did))) {
	    Reset_Pass2_Not(p2ds)
	}
	saved_pflags = p2ds->pflags;
	if (procindex->did == d_.comma)
	{
	    Reset_Pass2_Last(p2ds);	
	    p2ds->bind = 2;
	    saved_reg = p2ds->reg;
	    p2ds->reg = 0;
	    i = ClauseBodyCut(clds);
	    if ((res = _goal_pass2(goal + 1, clds, p2ds, procedure, cut_var)) < 0)
		return res;
	    if (p2ds->reg || !i && ClauseBodyCut(clds))
		cut_var = 0;
	    else
		p2ds->reg = saved_reg;
	    p2ds->pflags = saved_pflags | Pass2Cut(p2ds);
	    p2ds->top_goaln++;
	    goal += 2;
	    goto _start_;
	}
	else if (procindex->did == d_.semicolon)
	{
	    pword		*pw;

	    Set_Clause_Disj(clds);
	    New_Block(p2ds);
	    Set_Pass2_Disj(p2ds);
	    Set_Pass2_Arrow(p2ds);
	    if (!cut_var)
		cut_var = _make_localcut_var(p2ds->block, clds, goal);

	    pw = goal + 1;
	    res = _goal_pass2(pw, clds, p2ds, procedure, cut_var);
	    if (res < 0)
		return res;
	    if (IsStructure(pw->tag) && pw->val.ptr->val.did == d_.cond)
	    {
/* simple not/1 and once/1 is also a simple condition */
		if (p2ds->bind < 2 && !(p2ds->reg))
		{
		    if (trail)
		    {
			Trail_Pword(goal);
		    }
		    Set_Goal_Simple(goal);
		}
	    }
	    p2ds->reg = 1;

	    _gc_test(p2ds);
	    Set_Pass2_Disj(p2ds);
	    res = _goal_pass2(goal + 2, clds, p2ds, procedure, cut_var);
	    if (res < 0)
		return res;
	    p2ds->gtype = REGULAR;
	    p2ds->pflags = saved_pflags;
	    return 0;
	}
	else if (Pass2Arrow(p2ds) && Condition(procindex->did))
	{
	    int		saved_bind;

	    Reset_Pass2_Arrow(p2ds);
	    Reset_Pass2_Last(p2ds);
	    p2ds->reg = 0;
	    p2ds->bind = 0;
	    if ((res = _goal_pass2(goal + 1, clds, p2ds,
		    procedure, (var_desc *)0)) < 0)
		return res;
	    if (procindex->did == d_.softcut)
	    {
		if (!(p2ds->reg || p2ds->bind >= 2)) {
		    if (trail) {
			Trail_Pword(goal);
		    }
		    goal->val.did = d_.cond;
		    procindex = softcut_proc_;
		}
		else {
		    cut_var = _make_localcut_var(p2ds->block, clds, goal);
		    Set_Var_Permanent(cut_var);
		    cut_var = 0;
		}
	    }
	    if (procindex->did == d_.cond && (p2ds->bind >= 2 || p2ds->reg))
	    {
		Set_Var_Permanent(cut_var);
		cut_var->blockn = p2ds->block;
		Prepend_Tvv(cut_var, clds);
	    }
	    saved_reg = p2ds->reg;
	    saved_bind = p2ds->bind;
	    if ((res = _goal_pass2(goal + 2, clds, p2ds,
		    procedure, cut_var)) < 0)
		return res;
	    p2ds->reg = saved_reg;
	    p2ds->bind = saved_bind;
	    p2ds->pflags = saved_pflags;
	    Reset_Pass2_Arrow(p2ds);
	    return 0;
	}
	else if (procindex->did == d_.cond)
	{
/*	    if (GlobalFlags & CORTN) */
		p2ds->gtype = REGULAR;
	    if (!cut_var)
		cut_var = _make_localcut_var(p2ds->block, clds, goal);
	    p2ds->bind = 2;
	    Reset_Pass2_Last(p2ds);
	    saved_reg = p2ds->reg;
	    p2ds->reg = 0;
	    if ((res = _goal_pass2(goal + 1, clds, p2ds,
		    procedure, cut_var)) < 0)
		return res;
	    if (p2ds->reg)
	    {
		Set_Var_Permanent(cut_var);
		cut_var->blockn = p2ds->block;
		Prepend_Tvv(cut_var, clds);
		p2ds->gtype = REGULAR;
	    }
	    else
		p2ds->reg = saved_reg;
	    p2ds->pflags = saved_pflags;
	    return _goal_pass2(goal + 2, clds, p2ds, procedure, cut_var);
	}
	else if (Negation(procindex->did))
	{
	    var_desc	*nested_cut = 0;
	    int		body_cut = ClauseBodyCut(clds);
	    int		saved_bind;
	    int		saved_not;

	    Reset_Clause_Body_Cut(clds);
	    Set_Clause_Disj(clds);
	    New_Block(p2ds);
	    Set_Pass2_Disj(p2ds);
	    saved_not = Pass2Not(p2ds);
	    Exchange_Pass2_Not(p2ds);
	    Reset_Pass2_Last(p2ds);
	    p2ds->reg = 0;
	    saved_bind = p2ds->bind;
	    p2ds->bind = 0;

	    if (!cut_var) {
		cut_var = _make_localcut_var(p2ds->block, clds, goal);
	    }
	    nested_cut = _make_localcut_var(p2ds->block, clds, (pword *) 0);
	    _gc_test(p2ds);
	    if ((res = _goal_pass2(goal + 1, clds, p2ds, procedure, nested_cut)) < 0)
		return res;
	    if (!saved_not && IsStructure(goal[1].tag) &&
		!IsProc(goal[1].val.ptr->tag) &&
		Negation(goal[1].val.ptr->val.did))
	    {
		if (cut_var->top_disj == goal)
		    ;
	    }
	    else if (!saved_not && p2ds->bind < 2 && !(p2ds->reg))
	    {
		if (trail) {
		    Trail_Pword(goal);
		}
		Set_Goal_Simple(goal);
	    }
	    else {
		cut_var->blockn = p2ds->block;
		Prepend_Tvv(cut_var, clds);
		Set_Var_Permanent(cut_var);
	    }
	    goal++;
	    if (ClauseBodyCut(clds))
	    {
		if (IsStructure(goal->tag)) {
		    nested_cut->top_disj = goal->val.ptr;
		    nested_cut->blockn = p2ds->block;
		    Prepend_Tvv(nested_cut, clds);
		    Set_Var_Permanent(nested_cut);
		}
		if (!body_cut) {
		    Reset_Clause_Body_Cut(clds);
		}
	    }
	    else if (nested_cut && VarPermanent(nested_cut))
		nested_cut->top_disj = goal->val.ptr;
	    if (body_cut) {
		Set_Clause_Body_Cut(clds);
	    }
	    p2ds->reg = 1;
	    p2ds->pflags = saved_pflags;
	    p2ds->bind += saved_bind;
	    p2ds->gtype = REGULAR;
	    return 0;
	}
	else if (procindex->did == d_.once)
	{
	    int	body_cut = ClauseBodyCut(clds);

/*	    if (GlobalFlags & CORTN) */
		p2ds->gtype = REGULAR;
	    Reset_Clause_Body_Cut(clds);
	    if (!cut_var)
		cut_var = _make_localcut_var(p2ds->block, clds, goal);
	    Reset_Pass2_Last(p2ds);
	    saved_reg = p2ds->reg;
	    p2ds->reg = 0;
	    if ((res = _goal_pass2(goal + 1, clds, p2ds, procedure, cut_var)) < 0)
		return res;
	    if (p2ds->reg)
	    {
		cut_var->blockn = p2ds->block;
		Prepend_Tvv(cut_var, clds);
		Set_Var_Permanent(cut_var);
		p2ds->gtype = REGULAR;
	    }
	    else
		p2ds->reg = saved_reg;
	    if (ClauseBodyCut(clds))
	    {
		cut_var->blockn = p2ds->block;
		Prepend_Tvv(cut_var, clds);
		Set_Var_Permanent(cut_var);
		if (!body_cut)
		{
		    Reset_Clause_Body_Cut(clds);
		}
	    }
	    else if (body_cut)
	    {
		Set_Clause_Body_Cut(clds);
	    }
	    p2ds->pflags = saved_pflags;
	    return 0;
	}
	else if (procindex->did == d_.call_explicit)
	{
	    dident		module;
	    arg = goal + 2;
	    Dereference_(arg);
	    if (IsAtom(arg->tag) || IsNil(arg->tag))
	    {
		module = IsNil(arg->tag) ? d_.nil : arg->val.did;
		arg = goal + 1;
		Dereference_(arg);
		if (IsStructure(arg->tag))
		    arg = arg->val.ptr;
		if (IsAtom(arg->tag) || IsNil(arg->tag))
		{
		    if (trail)
		    {
			Trail_Pword(saved_goal);
		    }
		    saved_goal->val.ptr = arg;
		    goal = arg;
		    procindex = qualified_procedure(IsNil(goal->tag) ? d_.nil : goal->val.did,
		    	module, procedure->module, procedure->module_tag);
		    arity = DidArity(IsNil(goal->tag) ? d_.nil : goal->val.did);
		}
	    }
	}
	else if (procindex->did == d_.colon)
	{
	    dident		module;
	    arg = goal + 1;
	    Dereference_(arg);
	    if (IsAtom(arg->tag) || IsNil(arg->tag))
	    {
		module = IsNil(arg->tag) ? d_.nil : arg->val.did;
		arg = goal + 2;
		Dereference_(arg);
		if (IsStructure(arg->tag))
		    arg = arg->val.ptr;
		if (IsAtom(arg->tag) || IsNil(arg->tag))
		{
		    if (trail)
		    {
			Trail_Pword(saved_goal);
		    }
		    saved_goal->val.ptr = arg;
		    goal = arg;
		    procindex = qualified_procedure(IsNil(goal->tag) ? d_.nil : goal->val.did,
		    	module, procedure->module, procedure->module_tag);
		    arity = DidArity(IsNil(goal->tag) ? d_.nil : goal->val.did);
		}
	    }
	}
	else if (procindex->did == d_.unify) {
	    /* if expanded, nothing is Puts'ed and so no global temps
	       are needed */
	    if (ProcedureExpand(procedure)
		    /* should be also: && clds->reg_arity + 3 < MAXARITY*/ )
		expanded = 1;
	}
    }
    Reset_Pass2_Arrow(p2ds);

    old_type = p2ds->gtype;
    p2ds->gtype = TypeOf(procindex);
    p2ds->goaln++;

    if (old_type == REGULAR)
    {
	/* block boundary */
	_gc_test(p2ds);
	New_Block(p2ds);
    }

    /* now replace the functor by <TPROC, procindex> */
    if (!IsProc(goal->tag))
    {
	if (trail)
	{
	    Trail_Pword(goal);
	}
	goal->tag.all = TPROC;
	goal->val.nint = (long) procindex;
    }

    /* now set gargs; previous _goal_pass2 calls might have changed it */
    gargs = procedure->gargs;
    if (p2ds->gtype == GOAL_CUT)
    {
    /*
     ******** CUT *********
     */
	arity = 1;			/* only for the code size */
	if (p2ds->block > 1)
	{
	    Set_Clause_Body_Cut(clds);
	    if (old_type == SIMPLE)
	    {
		New_Block(p2ds);
	    }
	}
	else {
	    p2ds->gtype = SIMPLE;	/* a neckcut */
	}
	if (p2ds->goaln == 1) {
	    Set_Pass2_Cut(p2ds);
	}
    }
    else if (p2ds->gtype == REGULAR)
    {
    /*
     ******** A REGULAR GOAL *********
     */
	clause_arity = DidArity(procedure->did);
	p2ds->reg = 1;
	if (p2ds->block == 1)
	{
	    clds->reg_arity = arity;
	    Set_Clause_Rule(clds);
	}
	if (arity > clause_arity)
	{
	    if (GargsOverflow(gargs, arity))
	    {
		Resize_And_Copy_Gargs(gargs, clause_arity, arity);
		procedure->gargs = gargs;
	    }
	    for (i = clause_arity + 1; i <= arity; i++)
	    {
		gargs[i].state = 0;
		gargs[i].contents = 0;
	    }
	}
        /*
         * first scan only the structures
         */
        for (i = 1; i <= arity; i++)
        {
            arg = goal + i;
            Remove_Ref_Chains(arg, trail);
            if (IsStructure(arg->tag) || IsList(arg->tag) ||
		    IsRef(arg->tag) && IsMeta(arg->val.ptr->tag))
            {
                ards = gargs + i;
                if (p2ds->block == 1)		/* first regular */
                {
		    ards->target = arg;
		    /* check equal arguments */
		    if (ards->contents == 0)
                        ;
                    else if (ards->contents == arg)
                    {
                        Set_Arg_Keep(ards);
                    }
		    else if (IsStructure(arg->tag))
		    {
			if (IsStructure(ards->contents->tag) &&
			    arg->val.ptr->val.did
			    ==
			    ards->contents->val.ptr->val.did &&
			    _check_args(arg->val.ptr + 1,
					ards->contents->val.ptr + 1,
					(int) DidArity(arg->val.ptr->val.did),
					trail))
			{
			    Set_Arg_Keep(ards);
			}
		    }
		    else if (IsList(arg->tag))
		    {
			if (IsList(ards->contents->tag) &&
			    _check_args(arg->val.ptr, ards->contents->val.ptr,
					2, trail))
			{
			    Set_Arg_Keep(ards);
			}
		    }
                }
		if (p2ds->block > 1 || !ArgDone(ards))
		{
		    groundflg = 1;
		    comp_size = _compound_pass2(arg, clds, p2ds, procedure,
				gargs, &groundflg, W_PUT | Pass2Disj(p2ds));
		    if (groundflg)
		    {
			if (p2ds->block == 1)
			{
			    Set_Arg_Body_Ground(ards);
			    head_pass[BODY_SIMPLE]++;
			}
			if (!ClauseDynamic(clds) && !IsProc(arg->val.ptr->tag))
			{
			    /* a body ground structure, mark it with a tag */
			    if (trail)
			    {
				Trail_Word(arg, 1,  TRAILED_WORD32);
			    }
			    arg->tag.kernel = (IsList(arg->tag)) ? TGRL : TGRS;
			}
			else
			    gcsize += comp_size;
		    }
		    else
		    {
			gcsize += comp_size;
			if (p2ds->block == 1)
			    head_pass[STRUCTURE]++;
		    }
		    if (IsTvv(arg->tag)) {
			tvv = Tvv(arg->val);
			if (p2ds->block > 1) {
			    Add_Var_Destination(tvv, ContArg(i));
			    Set_Var_Body(tvv)
			}
			Reset_Var_Occ_Compnd(tvv)
		    }
		}
		if (IsTvv(arg->tag)) {
		    if (Pass2Disj(p2ds)) {
			Set_Var_Disj(Tvv(arg->val))
		    }
		    arg->tag.all = TEND;	/* mark we've just done it */
		}
            }
        }
        /*
         * then the constants and variables
         */
	ards = gargs;
        for (i = 1; i <= arity; i++)
        {
            arg = ++goal;
	    Remove_Ref_Chains(arg, trail);
	    ards++;
            if (IsRef(arg->tag))		/* first occurrence */
            {
                tvv = _create_tvv(arg, clds, p2ds->block);
		if (IsName(tvv->header.tag))
		{
		    gcsize++;
		}
                if (!Pass2Last(p2ds)) {
                    Set_Var_Permanent(tvv);
		    Set_Var_Occ_Body_Arg(tvv);
		}
		/*
		 * A destination could be set even for temporary variables
		 * occurring outside the first block, e.g. ..., p(X, f(X)), ..
		 * (if they occur in a structure, it is done there)
		 */
                if (p2ds->block == 1)
                {
                    Set_Var_Destination(tvv, ContArg(i));
		    ards->target = 0;
		    head_pass[BODY_SIMPLE]++;
                }
		else if (Pass2Disj(p2ds)) {
		    Set_Var_Disj(tvv)
		}
		Set_Var_Occ_Regular(tvv);
            }
            else if (IsTvv(arg->tag))	/* subsequent */
            {
                tvv = (var_desc *) arg->val.ptr;
                Set_Var_Nonvoid(tvv);
		tvv->counter++;
                if (p2ds->block != tvv->blockn)
                {
                    Set_Var_Permanent(tvv);
                    tvv->blockn = p2ds->block;
		    /* If it occurs only once in head arg, mark occupied */
		    if (VarSource(tvv) &&
			ArgFree(gargs + ArgNo(VarSource(tvv))))
		    {
			Set_Arg_Occupied(gargs + ArgNo(VarSource(tvv)));
			gargs[ArgNo(VarSource(tvv))].contents = arg;
		    }
                }
		else if (Pass2Disj(p2ds)) {
		    Set_Var_Permanent(tvv);
		}
		if (!VarPermanent(tvv))
		{
		    if (VarOccCompnd(tvv)) {
			Reset_Var_Occ_Compnd(tvv)
			if (p2ds->block > 1 || i > procedure->arity) {
			    Reset_Var_Alloc_Temp(tvv)
			}
			else if (!VarOccRegular(tvv)) {
			    Set_Var_Alloc_Temp(tvv)
			}
		    }
		    if (p2ds->block > 1) {
			Add_Var_Destination(tvv, ContArg(i));
			Set_Var_Body(tvv)
		    }
		}
		Prepend_Tvv(tvv, clds);
                if (p2ds->block == 1)
		{
		    if (VarSource(tvv))
		    {
			if (/* already there */
			    tvv->source.cont ==	ContArg(i) ||
			    /* will be after unif */
			    ards->contents &&
			    IsTvv(ards->contents->tag) &&
			    ards->contents->val.ptr == arg->val.ptr)
			{
			    Set_Arg_Keep(ards);
			    ards->target = arg;		/* not necessary? */
			}
			else
			{
			    ards->target = arg;
			    head_pass[VAR]++;
			}
			Set_Arg_Occupied(gargs + ArgNo(VarSource(tvv)));
		    }
		    else /* only in simple or structs */
		    {
			Add_Var_Destination(tvv, ContArg(i));
			ards->target = 0;
			head_pass[BODY_SIMPLE]++;
		    }
		}
            }
	    else if (arg->tag.all == TEND)
		arg->tag.all = TTVV;
            else if (!IsCompound(arg->tag))
	    {
		if (p2ds->block == 1)		/* first regular */
		{
		    register pword	*cont = ards->contents;

		    if (cont && SameType(cont->tag, arg->tag) &&
			(cont->val.nint == arg->val.nint ||
			IsNil(cont->tag) ||
			IsString(cont->tag) &&
			    !compare_strings(arg->val, cont->val)))
		    {
			Set_Arg_Keep(ards);
			ards->target = arg;		/* not necessary? */
		    }
		    else
		    {
			ards->target = arg;
			head_pass[BODY_SIMPLE]++;
		    }
		}
		if (IsAtom(arg->tag))
		    {Set_Did_Stability(arg->val.did, DICT_CODE_REF)}
	    }
        }
    }
    else
    {
    /*
     ******** A SIMPLE GOAL *********
     */
        /*
         * the arguments are scanned backwards, like for the generation
         */
        for (goal += arity; arity; arity--)
        {
            arg = goal--;
            Remove_Ref_Chains(arg, trail);
            if (IsRef(arg->tag) && !IsMeta(arg->val.ptr->tag))
            {
                tvv = _create_tvv(arg, clds, p2ds->block);
		if (IsName(tvv->header.tag))
		{
		    gcsize++;
		}
		if ((i = ArgBinding(arity, procindex->mode)) != CONSTANT &&
			i != GROUND)
		{
		    Set_Var_Occ_Body_Arg(tvv);
		}
#ifdef CALLEE_POP
		Set_Var_Occ_Compnd(tvv);
#endif
		Set_Var_Occ_Simple_Arg(tvv);
		tvv->goaln = p2ds->goaln;
		tvv->counter = 1;
		if (UnifType(procindex) > U_SIMPLE &&
		    ArgBinding(arity, procindex->mode))
		    p2ds->bind++;
            }
            else if (IsTvv(arg->tag))
            {
                tvv = (var_desc *) arg->val.ptr;
                Set_Var_Nonvoid(tvv);
		tvv->counter++;
                if (p2ds->block != tvv->blockn)
                {
                    Set_Var_Permanent(tvv);
                    tvv->blockn = p2ds->block;
                }
                else if (Pass2Disj(p2ds)) {
                    Set_Var_Permanent(tvv);
		}
		else if (tvv->goaln != p2ds->goaln)
		{
		    if (VarOccCompnd(tvv)) {
			Set_Var_Alloc_Temp(tvv)
		    } else {
			Set_Var_Occ_Simple_Arg(tvv);
		    }
		}
		else if (!VarOccCompnd(tvv)) {
		    Set_Var_Occ_Simple_Arg(tvv);
		}
		tvv->goaln = p2ds->goaln;
		Set_Var_Simple_Arg_Occ(tvv, p2ds->goaln)
		Prepend_Tvv(tvv, clds);
		/* If it occurs only once in head arg, mark occupied */
		if (VarSource(tvv) &&
		    ArgFree(gargs + ArgNo(VarSource(tvv))))
		{
		    Set_Arg_Occupied(gargs + ArgNo(VarSource(tvv)));
		    gargs[ArgNo(VarSource(tvv))].contents = arg;
		}
		if (UnifType(procindex) > U_SIMPLE &&
		    ArgBinding(arity, procindex->mode) &&
		    !VarNoalias(tvv))
		    p2ds->bind++;
                /*
		 * Here I should actually check the type
                 * of the predicate - ==/2, @</2 etc.
		 * However, correct '-' mode requires no shared variables,
		 * so it is not used.
                if (p2ds->block == 1)
                {
                    Set_Var_Unify_Now(tvv);
                }
                 */
            }
            else if (IsStructure(arg->tag) || IsList(arg->tag) ||
		    IsRef(arg->tag))
	    {
		register pword		*pw;
		int			where = Pass2Disj(p2ds);

		if (expanded && arity == 2) {
		    pw = goal;
		    Dereference_(pw);
		    if (!IsRef(pw->tag) && !IsTvv(pw->tag) ||
			    IsRef(arg->tag))
			expanded = 0;
		}
		groundflg = 1;
		if (!expanded)
		    where |= W_PUTS;
		if (UnifType(procindex) > U_SIMPLE &&
		    ArgBinding(arity, procindex->mode))
		    where |= W_BIND;
                comp_size = _compound_pass2(arg, clds, p2ds, procedure, gargs,
		    &groundflg, where);
		if (  groundflg 
		   && !ClauseDynamic(clds) 
		   && (  procindex->did != d_.unify 
		      || procindex->module_ref != d_.kernel_sepia
		      )
		   && !IsProc(arg->val.ptr->tag)
		   )
		{
		    /* a body ground structure, mark it with a tag */
		    if (trail)
		    {
			Trail_Word(arg, 1,  TRAILED_WORD32);
		    }
		    arg->tag.kernel = (IsList(arg->tag)) ? TGRL : TGRS;
		}
		else
		    gcsize += comp_size;
	    }
	    else if (IsAtom(arg->tag))
		{Set_Did_Stability(arg->val.did, DICT_CODE_REF)}
        }
    }
    p2ds->gc_size += gcsize;

    return 0;
}

/*
 * FUNCTION NAME:
 *
 * PARAMETERS:
 *
 * DESCRIPTION:
 * Process the arguments of a compound term. It is called only on real
 * compound terms (after processing conjectures), and so only the
 * variable info is collected, together with the ground/nonground flag.
 * It is used for all the compound terms in the head, prefix and body.
 * The input must be dereferenced (otherwise it might not be a compound
 * term, anyway).
 * It returns the number of arguments in the whole structure (comp_size).
 */
#define IN_META		1
#define TOP_ATTRIBUTE	2
static int
_compound_pass2(pword *term,
	cl_desc *clds,
	pass2_desc *p2ds,
	proc_desc *procedure,
	arg_desc *gargs,
	int *groundflg,
	int where)		/* in a disjunction?		*/
{
    pword		*arg;
    var_desc		*tvv;
    int			arity;
    int			size;
    int			trail = ClauseTrail(clds);
    int			block = p2ds->block;
    int			in_meta = 0;

    if (IsStructure(term->tag))
    {
	term = term->val.ptr;
	if (IsProc(term->tag))
	    arity = GoalArity(term);
	else {
	    arity = DidArity(term->val.did);
	    Set_Did_Stability(term->val.did, DICT_CODE_REF);
	}
	term++;
    }
    else if (IsList(term->tag))
    {
        term = term->val.ptr;
        arity = 2;
    }
    else if (IsRef(term->tag)) {
	in_meta = IN_META;
	arity = 2;
    }
    else
    {
	Assert(0);
    }
    size = arity + 1;
    while (arity--)
    {
        arg = term++;
        Remove_Ref_Chains(arg, trail);
        if (IsRef(arg->tag) && (!IsMeta(arg->val.ptr->tag) ||
		in_meta == IN_META))
        {
            *groundflg = 0;
            tvv = _create_tvv(arg, clds, block);
	    if (IsName(tvv->header.tag))
		size++;
            if (block == 1)
	    {
	    }
	    else if (where & W_DISJ) {
		if (!Pass2Last(p2ds)) {
		    Set_Var_Permanent(tvv)
		    Set_Var_Occ_Body_Arg(tvv)
		}
		Set_Var_Disj(tvv)
	    }
	    if (Pass2Ground(p2ds)) {
		Set_Var_Ground(tvv);
	    }
	    else if (Pass2Uninit(p2ds)) {
		Set_Var_Uninit(tvv);
	    }
	    else if (Pass2Noalias(p2ds)) {
		Set_Var_Noalias(tvv);
	    }
	    if (where & (W_PUT|W_PUTS)) {
		Set_Var_Occ_Compnd(tvv);
		if (where & W_PUT) {
		    Set_Var_Occ_Regular(tvv);
		}
	    }
	    tvv->goaln = p2ds->goaln;
	    if (in_meta == IN_META) {
		in_meta = TOP_ATTRIBUTE;
		term = tvv->header.val.ptr + 1;
	    }
	    else
		in_meta = 0;
	    if (where & W_BIND)
		p2ds->bind++;
        }
        else if (IsTvv(arg->tag))
        {
            *groundflg = 0;
	    in_meta = 0;
            tvv = (var_desc *) arg->val.ptr;
	    if (p2ds->goaln == 0 && !VarUninit(tvv)) {
		Set_Pass2_Nonvoid(p2ds);
	    }
            Set_Var_Nonvoid(tvv);
	    tvv->counter++;
	    if (VarSource(tvv) &&
		ArgFree(gargs + ArgNo(VarSource(tvv))))
	    {
		Set_Arg_Occupied(gargs + ArgNo(VarSource(tvv)));
	    }
            if (where & W_PUTS && p2ds->goaln == tvv->goaln &&
		VarOccSimpleArg(tvv))
	    {
		Set_Var_Global_Alloc(tvv);
		Set_Var_Alloc_Temp(tvv)
            }
            if (block != tvv->blockn)
            {
                Set_Var_Permanent(tvv);
                tvv->blockn = block;
            }
	    else if (where & W_DISJ) {
		Set_Var_Permanent(tvv);
	    }
	    else if (VarOccCompnd(tvv) && p2ds->goaln != tvv->goaln)
	    {
		Set_Var_Alloc_Temp(tvv)
	    }
	    Reset_Var_Occ_Simple_Arg(tvv)
	    Prepend_Tvv(tvv, clds);
            if (where & W_PUTS && block == 1)
            {
		/* If the first occurrence is in an output argument, we
		 * need this (could be optimized).
		 */
		if (VarUninit(tvv) && (!VarSource(tvv) ||
		    OutputMode(Mode(ArgNo(VarSource(tvv)), procedure->mode))))
		{
		    Set_Var_Alloc_Temp(tvv);
		}
            }
	    if (where & W_BIND)
		p2ds->bind++;
        }
	else
	{
	    if (ProcedureDelcl(procedure))
		size++;			/* Read_test_var */
	    if (p2ds->goaln == 0) {
		if (in_meta != TOP_ATTRIBUTE) {
		    Set_Pass2_Nonvoid(p2ds);
		}
	    }
	    in_meta = 0;
	    if (IsStructure(arg->tag))
	    {
		size += _compound_pass2(arg, clds, p2ds, procedure, gargs,
			groundflg, where | W_PUSH);
	    }
	    else if (IsList(arg->tag) ||
		IsRef(arg->tag) && IsMeta(arg->val.ptr->tag))
	    {
		if (arity)
		    size += _compound_pass2(arg, clds, p2ds, procedure, gargs,
			    groundflg, where | W_PUSH);
		else	/* tail-recursive call */
		{
		    arity = 2;
		    size += 2;
		    if (IsRef(arg->tag)) {
			in_meta = IN_META;
			term = arg;
		    }
		    else
			term = arg->val.ptr;
		}
	    }
	    else {
		if (IsAtom(arg->tag))
		    {Set_Did_Stability(arg->val.did, DICT_CODE_REF)}
		if (arity && where & W_BIND)
		    p2ds->bind++;
	    }
	}
    } /* end while */
    return size;
}

/*
 * FUNCTION NAME:	_create_tvv(ref, clds, block)
 *
 * PARAMETERS:		ref	- a pointer to the variable
 *			clds	- the clause descriptor
 *			block	- the block number of the first occurrence
 *			trail	- do we have to trail changes?
 *
 * DESCRIPTION:
 * Create the variable descriptor for the given free variable. The variable
 * is bound to it using the tag TTVV, and all changes are trailed.
 * The previous contents of the variable is stored in the header of the
 * variable descriptor, so this is how the type of the variable can be queried.
 */
static var_desc *
_create_tvv(pword *ref, cl_desc *clds, int block)
{
    var_desc			*tvv = (var_desc *) Gbl_Tg;
    pword			*ptr;
    dident			wd;

    Gbl_Tg += (sizeof(var_desc) + sizeof(pword) - 1)/ 
            sizeof(pword);	/* reserve space & keep alignment */
    Check_Gc;
    if ((ptr = ref->val.ptr) != ref /* && !IsName(ref->tag) */)
	/* pointer to a free variable	*/
    {
	if (ClauseTrail(clds))
	{
	    Trail_Pword(ref);	
	}
	ref->val.ptr = (pword *) tvv;	/* bind the variable to the descr */
	ref->tag.all = TTVV;
	ref = ptr;		/* and continue with the proper free var */
    }
    if (ClauseTrail(clds))
    {
	Trail_Tag(ref);	
    }
    tvv->tag.kernel = TVARDESC;
    tvv->header.val.ptr = ref->val.ptr;
    if (GlobalFlags & VARIABLE_NAMES && IsNamed(ref->tag.kernel))
    {
	tvv->header.tag.all = ref->tag.all;
	wd = TagDid(ref->tag.kernel);
	Set_Did_Stability(wd, DICT_CODE_REF);
    } else if (IsMeta(ref->tag))
	tvv->header.tag.kernel = RefTag(TMETA);
    else
	tvv->header.tag.kernel = TREF;
    ref->val.ptr = (pword *) tvv;	/* bind the variable to the descr */
    ref->tag.all = TTVV;
    tvv->source.cont = tvv->destination.cont = 0;	/* initialize */
    tvv->vflags = VAR_VOID;
    tvv->blockn = block;
    tvv->next_tvv = clds->variables;
    tvv->prev_tvv = 0;
    tvv->top_disj = 0;
    tvv->counter = 0;
    if (clds->variables)
        clds->variables->prev_tvv = tvv;
    clds->variables = tvv;
    return tvv;
}

/*
 * FUNCTION NAME:	_perm_alloc(clds)
 *
 * PARAMETERS:		clds	- clause descriptor pointer
 *
 * DESCRIPTION:
 * The allocation of the permanent variables. Some variables are already
 * flagged as permanent - those occurring in at least two blocks
 * and those occurring only in regular goal arguments (and not in
 * the last goal). The list is sorted so that the variables occurring in the
 * higher blocks are before those which occur only in lower blocks.
 * Global temporary variables, i.e. the nonvoid temporaries that occur first
 * in a compound argument of a simple goal, are as well classified
 * as permanent except for unit clauses.
 * It returns the number of variables that can be globalized in the last
 * goal (for the GC test).
 */
static int
_perm_alloc(cl_desc *clds)
{
    var_desc		*tvv = clds->variables;	/* aux pointer		      */
    var_desc		*last = tvv;
    pword		*disj;
    int			size = 0;		/* number of permanent vars   */
    int			temp = 0;		/* number of global temps     */
    int			global = 0;		/* number of pushed vars      */

    while (tvv)
    {
	if (VarVoid(tvv)) {
	    Reset_Var_Occ_Compnd(tvv)
	}
        else if (VarAllocTemp(tvv))
        {
	    if (ClauseRegular(clds))
	    {
		/* Global nonvoid temporaries are made permanent. */
		Set_Var_Permanent(tvv);
	    } else if (IsMeta(tvv->header.tag))
	    {
		Set_Clause_Regular(clds);
		Set_Var_Permanent(tvv);
	    }
	    else
	    {
		Set_Var_Perm_No(tvv, ++temp);
	    }
	    Reset_Var_Occ_Compnd(tvv)
        }
        if (VarPermanent(tvv))
        {
            Set_Var_Perm_No(tvv, ++size);
	    if (disj = tvv->top_disj)	/* a variable for ->	*/
	    {
		if (GoalSimple(disj)) {
		    Add_Goal_Cut_Var(disj, size);
		}
		else {
		    if (ClauseTrail(clds)) {
			Trail_Pword(disj);
		    }
		    Set_Goal_Cut_Var(disj, size);
		}
	    }
	    if (VarUnsafe(tvv))
		global++;
        }
	else if (VarBodyArg(tvv))
	    global++;
	last = tvv;
        tvv = tvv->next_tvv;
    }
    clds->envsize = size;
    clds->global = temp;
    clds->last_var = last;
    return global;
}


/*
 * FUNCTION NAME:
 *
 * PARAMETERS:
 *
 * DESCRIPTION:
 * Replace the original goal by another one, which is more straightforward
 * to compile. Currently the transformations are:
 *		X		==>		call(X)
 *		[H|T]		==>		'.'(H, T)
 *		call_super(G)	==>		call_super(G, Method, Label)
 *			(for objects only)
 * Input is the goal reference (dereferenced), output is the new goal reference.
 * Since the transformations occur at different places, different information
 * is necessary.
 * The transformed goal is assumed to be always a regular procedure.
 * If the input is a variable, it must have the TTVV tag.
 */
/*ARGSUSED*/
static pword *
_transform_goal(pword *goal)
{
    pword		*p;

    if (IsTvv(goal->tag))
    {
	p = Gbl_Tg;
	Gbl_Tg += 2;
	Check_Gc;
	p[0].tag.all = TDICT;
	p[0].val.did = d_.call;
	p[1].tag.all = goal->tag.all;
	p[1].val.ptr = goal->val.ptr;
	return p;
    }
    else if (IsList(goal->tag))
    {
	p = Gbl_Tg;
	Gbl_Tg += 3;
	Check_Gc;
	goal = goal->val.ptr;
	p[0].tag.all = TDICT;
	p[0].val.did = d_.list;
	p[1].tag.all = goal[0].tag.all;
	p[1].val.ptr = goal[0].val.ptr;
	p[2].tag.all = goal[1].tag.all;
	p[2].val.ptr = goal[1].val.ptr;
	return p;
    }
    return 0;
}

/*
 * FUNCTION NAME:
 *
 * PARAMETERS:
 *
 * DESCRIPTION:
 * Create the descriptor for the variable which is used for the handling
 * of the cut.
 */
static var_desc *
_make_cut_var(int block)
{
    var_desc		*tvv = (var_desc *) Gbl_Tg;
    Gbl_Tg += (sizeof(var_desc) + sizeof(pword) - 1)/ 
            sizeof(pword);	/* reserve space & keep alignment */
    Check_Gc;
    tvv->tag.kernel = TVARDESC;
    tvv->header.val.ptr = (pword *) tvv;
    tvv->header.tag.all = TREF;
    tvv->vflags = VAR_PERMANENT | VAR_ALLOCATED;
    tvv->blockn = block;
    tvv->counter = 1;
    tvv->top_disj = 0;
    Set_Var_Source(tvv, ContPerm(1));
    Set_Var_Destination(tvv, 0);
    return tvv;
}

/*
 * FUNCTION NAME:		_make_localcut_var()
 *
 * PARAMETERS:
 *
 * DESCRIPTION:
 * Create the descriptor for the variable which is used for the handling
 * of the if-then-else constructs. The variable is allocated at the
 * topmost disjunction and shared for all descendants whenever possible.
 * This means that generally too many of these variables are allocated,
 * however other alternatives were even more complicated.
 */
static var_desc *
_make_localcut_var(int block, cl_desc *clds, pword *goal)
{
    var_desc		*tvv = (var_desc *) Gbl_Tg;

    Gbl_Tg += (sizeof(var_desc) + sizeof(pword) - 1)/ 
            sizeof(pword);	/* reserve space & keep alignment */
    Check_Gc;
    tvv->tag.kernel = TVARDESC;
    tvv->header.val.ptr = (pword *) tvv;
    tvv->header.tag.all = TREF;
    /* changed to PERM if it will be used	*/
    tvv->vflags = 0;
    tvv->blockn = block;
    tvv->counter = 1;
    tvv->top_disj = goal;
    tvv->source.cont = tvv->destination.cont = 0;	/* initialize */
    tvv->next_tvv = clds->variables;
    tvv->prev_tvv = 0;
    if (clds->variables)
        clds->variables->prev_tvv = tvv;
    clds->variables = tvv;
    return tvv;
}


/*
 * FUNCTION NAME:	_check_args(ptr1, ptr2, arity)
 *
 * PARAMETERS:		ptr1 -	pointer to an argument block
 *			ptr2 - pointer to another argument block
 *			arity -	arity of the structure
 *
 * DESCRIPTION:		This procedure checks whether a compound term
 *			which occurs in the same argument in the head
 *			and in the first regular subgoal has identical
 *			arguments. This a compromise between full
 *			structure re-using, which is too expensive,
 *			and overhead of constructing a structure
 *			which already exists. The arguments are not
 *			scanned recursively.
 */
static int
_check_args(pword *ptr1, pword *ptr2, int arity, int trail)
{
    int			i;
    pword		*arg1;
    pword		*arg2;

    for (i = arity; i; i--)
    {
	arg1 = ptr1++;
	arg2 = ptr2++;
	Remove_Ref_Chains(arg1, trail);
	Remove_Ref_Chains(arg2, trail);
	if (arg1->tag.kernel != arg2->tag.kernel ||
		arg1->val.all != arg2->val.all &&
		!IsNil(arg1->tag))
	    return 0;
    }
    if (GlobalFlags & SINGLETON_CHECK)
    {
	/* We must set variables as nonvoid, otherwise we'd get a warning
	   about singletons from the compiler */
	for (i = 1; i <= arity; i++)
	{
	    arg1 = --ptr1;
	    if (IsTvv(arg1->tag))
		Set_Var_Nonvoid(Tvv(arg1->val));
	}
    }
    return 1;
}


/*
 * FUNCTION NAME:	_print_cont_chain(ptr)
 *
 * PARAMETERS:		ptr -	pointer to a chain of values
 *
 * DESCRIPTION:		An auxiliary function for debugging which outputs
 *			the values stored in the source or destination
 *			chain of a variable descriptor.
 */
static void
_print_cont_chain(struct var_ch *ptr)
{
    while (ptr)
    {
        p_fprintf(current_output_, "%x ", ptr->cont);
        ptr = ptr->next;
    }
    (void) ec_newline(current_output_);
}

/*
 * FUNCTION NAME:	_print_pass2_info(clds, procedure)
 *
 * PARAMETERS:		clds		- clause descriptor
 *			procedure	- procedure descriptor
 *
 * DESCRIPTION:		This is an auxiliary procedure which outputs
 *			all the information collected in the second pass.
 */
static void
_print_pass2_info(cl_desc *clds, proc_desc *procedure)
{
    arg_desc		*gargs = procedure->gargs;
    int			*head_pass = procedure->head_pass;
    var_desc		*var;
    struct bl_size	*bl;
    int			i;
    int			arity = DidArity(procedure->did);
    int			max_arg;
    value		v1, v2;

    v1.ptr = (pword *) StreamNr(current_output_);
    v2.did = d_.default_module;
    p_fprintf(current_output_,
	    "Clause %s/%d:\n\tenv_size = %d\n\tflags =\t   %x\n",
	    DidName(procedure->did),
	    arity,
	    clds->envsize,
	    clds->cflags);


    var = clds->variables;
    if (var)
	p_fprintf(current_output_, "Variables:");
    while (var)
    {
	p_fprintf(current_output_,
	    "\n\ttvv =     0x%x\n\tno =      %d\n\tblockn =  %d\n\tcounter = %d\n\tflags =   0x%x\n",
	    var,
	    VarPermNo(var),
	    var->blockn,
	    var->counter,
	    var->vflags);
	if (IsNamed(var->header.tag.kernel))
	    p_fprintf(current_output_, "\tname =    %s\n", DidName(TagDid(var->header.tag.kernel)));
	if (var->source.cont)
	{
	    p_fprintf(current_output_, "\tsource =  ");
	    _print_cont_chain(&var->source);
	}
	if (var->destination.cont)
	{
	    p_fprintf(current_output_,
	    "\tdestination = ");
	    _print_cont_chain(&var->destination);
	}
	var = var->next_tvv;
    }

    max_arg = arity > (int) clds->reg_arity ? arity : clds->reg_arity;
    p_fprintf(current_output_, "Arguments:");
    for(i = 1; i <= max_arg; i++)
    {
	p_fprintf(current_output_,
	    "\n\t%d. state = 0x%x",
	    i,
	    (++gargs)->state);

	if (gargs->contents)
	{
	    p_fprintf(current_output_, "\n\t   contents = ");
	    (void) p_writeq3(v1, tint,
		gargs->contents->val, gargs->contents->tag, v2, tdict);
	}
	if (gargs->target)
	{
	    p_fprintf(current_output_,
		"\n\t   target =   ");
	    (void) p_writeq3(v1, tint, gargs->target->val, gargs->target->tag,
		v2, tdict);
	}
    }
    p_fprintf(current_output_, "\nPasses:\n");
    p_fprintf(current_output_,
		"\tCONSTANT\t\t%d\n",
		head_pass[CST]);
#ifdef OBJECTS
    p_fprintf(current_output_
		"\tTYPED VARS\t\t%d\n",
		head_pass[TYPED_VAR]);
#endif
    p_fprintf(current_output_,
		"\tVALUE\t\t\t%d\n",
		head_pass[VALUE]);
    p_fprintf(current_output_,
		"\tGROUND_STRUCTURE\t%d\n",
		head_pass[GROUND_STRUCTURE]);
    p_fprintf(current_output_,
		"\tNONGROUND_STRUCTURE\t%d\n",
		head_pass[NONGROUND_STRUCTURE]);
    p_fprintf(current_output_,
		"\tPREFIX\t\t\t%d\n",
		head_pass[PREFIX]);
    p_fprintf(current_output_,
		"\tOUTPUT_NONVAR\t\t%d\n",
		head_pass[OUTPUT_NONVAR]);
    p_fprintf(current_output_,
		"\tVARS\t\t\t%d\n",
		head_pass[VAR]);
    p_fprintf(current_output_,
		"\tSTRUCTURE\t\t%d\n",
		head_pass[STRUCTURE]);
    p_fprintf(current_output_,
		"\tBODY_SIMPLE\t\t%d\n",
		head_pass[BODY_SIMPLE]);
    if (clds->gc_list)
	p_fprintf(current_output_, "\nGC sizes:\n");
    bl = clds->gc_list;
    while (bl)
    {
	p_fprintf(current_output_,
	    "\tgoal =     %d\tsize =      %d\n",
	    bl->goal, bl->size);
	bl = bl->next;
    }
    ec_flush(current_output_);
}

void
pass2_init(int flags)
{
}

/*
 * FUNCTION NAME:	_gc_test(p2ds)
 *
 * PARAMETERS:		p2ds	- pass2 descriptor
 *
 * DESCRIPTION:		This procedure checks whether the GC_test
 *			instruction will be necessary and if so, creates
 *			the corresponding data structure.
 */
static void
_gc_test(pass2_desc *p2ds)
{
    if (p2ds->gc_size > GC_MAX_PUT)
    {
	*p2ds->gc_list = (struct bl_size *) Gbl_Tg;
	Gbl_Tg = (pword *) (*p2ds->gc_list + 1);
	Check_Gc;
	(*p2ds->gc_list)->tag.kernel = TBLSIZE;
	(*p2ds->gc_list)->size = p2ds->gc_size;
	(*p2ds->gc_list)->goal = p2ds->gc_goal;
	(*p2ds->gc_list)->next = 0;
	p2ds->gc_list = &(*p2ds->gc_list)->next;
    }
    p2ds->gc_size = 0;
    p2ds->gc_goal = p2ds->goaln;
}

