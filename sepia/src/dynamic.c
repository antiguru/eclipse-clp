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
 * VERSION	$Id: dynamic.c,v 1.2 2007/09/04 16:28:48 jschimpf Exp $
 */

/*
 * IDENTIFICATION		dynamic.c
 *
 * DESCRIPTION	
 *
 * CONTENTS:
 *
 * AUTHOR	VERSION	 DATE	REASON
 * Dominique
 * Micha Meier	2.2	20.7.89	updated for the new compiler
 * periklis             26.9.89 Major revision for the logical update semantics
 *
 */

 /*
  * INCLUDES:
  */


#include	"config.h"
#include	"sepia.h"
#include	"types.h"
#include	"embed.h"
#include	"mem.h"
#include 	"error.h"
#include	"dict.h"
#include 	"emu_export.h"
#include    	"io.h"
#include	"opcode.h"
#include	"compiler.h"
#include	"gencode.h"
#include	"debug.h"
#include	"dynamic.h"
#include	"database.h"
#include	"module.h"

#ifdef OLD_DYNAMIC

 /*
  * TYPEDEFS:
  */

 /*
  * EXTERNAL VARIABLE DECLARATIONS:
  */
#ifdef PRINTAM
extern void	print_procedure(dident wdid, vmcode *code);
#endif
extern int	debug_pass3;

extern void	reclaim_procedure(vmcode *code),
		copy_dynamic_clause(proc_desc *procedure),
		delete_proc_from_chain(pri *p, proc_duet **chain);
extern dident	get_did(proc_desc *procedure, pword *clause);


 /*
  * EXTERNAL VARIABLE DEFINITIONS: 
  */

 /*
  * STATIC VARIABLE DEFINITIONS: 
  */
static void	_reset_gc_fields(vmcode *code);

 /*
  * FUNCTION DEFINITIONS: 
  */

/*
 * FUNCTION NAME:
 *
 * PARAMETERS:
 *
 * DESCRIPTION:
 */
/*
 * This file contains the main routines of compilation of dynamic 
 * Prolog procedures and the dynamic database garbage collector.
 */

extern vmcode   *gen_clause(cl_desc *clds, int clause_number, proc_desc *procedure, register vmcode *code);

int		compile_dynamic(proc_desc *procedure, dident module);
static	int	_gc_dynamic_procs(void),
		_alive(uword birth, uword death, uword *call_clock_list, uword call_clock_list_top),
		_retract_next_clause_of(vmcode *code_start);
static void	_garbage_collect_proc(vmcode *code_start, uword abolish_clock);

/*
 * FUNCTION NAME:	compile_clause(clause, proc)
 *
 * PARAMETERS:		clause	- pointer to the clause to be asserted
 *			proc	- procedure table entry
 *			codea	- pointer to the asserted clause code
 *			codes	- pointer to the source clause code
 *			
 *
 * DESCRIPTION:
 * Compiles the given clause twice, once as the dynamic clause and second
 * time as the source clause in the form
 *	clause(Head, Body, CodeARef)
 * where Body is 'true' if the asserted clause is a fact and CodeARef is
 * the start of the asserted clause code (codea).
 * It does the necessary dynamic memory allocation, returning in code[as]
 * the start of the two pieces of code.
 * The structure of both compiled and source clauses is :
 *	DYNAMIC_INSTR_SIZE words free (for the dynamic instruction), followed by
 *	the actual code
 * The beginning of the code is the dynamic instruction.
 * If the body is not valid, an error is returned to be used by the
 * calling procedure.
 */
#define Clean_And_Exit(code)		res = code; goto _free_memory_;
int
compile_clause(pword *clause, pri *proc, vmcode **codea, vmcode **codes, proc_desc *procedure)
{
    reg_desc		r_desc[NREG_DFLT];
    int			h_pass[HEAD_PASSES];

    cl_desc  		clds;
    vmcode		*code;
    pword		**save_tt = TT;
    pword		*save_tg = Gbl_Tg;
    pword		source_clause[4];
    dident		module = PriModule(proc);
    int			state;
    int			arity = DidArity(PriDid(proc));
    int			res = PSUCCEED;

    /* Initialize the procedure descriptor */
    Buf_Create(procedure->codebuf, DYN_BUFFER_SIZE);
    procedure->clauses = &clds;
    procedure->pflags |= PR_DYNAMIC;
    procedure->gargs = (arg_desc *) hg_alloc(sizeof(arg_desc) * (arity + 10));
    procedure->stack = (long *) hg_alloc(4 * sizeof(long) * (int) STACK_SIZE);
    procedure->stack_end = procedure->stack + 4 * STACK_SIZE;
    GargsSize(procedure->gargs) = arity + 9;
    procedure->regs = r_desc;
    procedure->head_pass = h_pass;

    procedure->did = PriDid(proc);
    procedure->module = module;
    procedure->clauseno = 1;
    procedure->index = 0;
    procedure->mode = PriMode(proc);
    procedure->size = -PROC_PREFIX_SIZE;

    /*
     * Compile the dynamic procedure
     */
    if (DebugProc(proc))
    {
	Set_Procedure_Debug(procedure);
    }

    clds.clause = clause;
    clds.variables = 0;
    clds.cflags = CL_TRAIL | CL_AFTER_RETRY | CL_DYNAMIC;
    clds.reg_arity = 0;
    clds.gc_list = 0;
    (void) get_did(procedure, clause);

    state = clause_pass2(&clds, procedure);
    if (state < 0) {
	Clean_And_Exit(state)
    }
    code = BufWriteZ(procedure->codebuf);
    Buf_Alloc(procedure->codebuf, code, L_START);
    *code = 0;
    code = CodeStart(code);
    ProcLid(code) = DYNAMIC_PROC;
    New_Label(clds.entry);			/* clause beginning */
    code = gen_clause(&clds, procedure->ctype, procedure, code);
    Buf_Alloc(procedure->codebuf, code, L_WORD);
    Store_d(Code_end);

#ifdef PRINTAM
    if (debug_pass3)
	print_procedure(procedure->did, procedure->start);
#endif
    Buf_Flush(procedure->codebuf, code);
#ifdef ALLOC
    p_fprintf(current_output_,
	      "\n%s/%d: dynamic used %d\n",
	      DidName(procedure->did),
	      DidArity(procedure->did),
	      BufPos(procedure->codebuf, code));
#endif
    procedure->size += BufPos(procedure->codebuf, code);
    copy_dynamic_clause(procedure);
    *codea = procedure->start - DYNAMIC_INSTR_SIZE;

    Untrail_Variables(save_tt);

    /*
     * Compile the source procedure
     */
    if (IsRule(clause) &&
	!(IsAtom(clause->val.ptr[2].tag) &&
	clause->val.ptr[2].val.did == d_.true0))
    {
	source_clause[0].tag.kernel = TDICT;
	source_clause[0].val.did = d_.clause;
	source_clause[1] = clause->val.ptr[1];
	source_clause[2] = clause->val.ptr[2];
	source_clause[3].tag.kernel = TINT;
	source_clause[3].val.nint = (long) *codea;

	clds.clause = source_clause;
	clds.variables = 0;
	clds.cflags = CL_TRAIL | CL_AFTER_RETRY | CL_DYNAMIC;
	clds.reg_arity = 0;
	clds.gc_list = 0;

	Buf_Reinit(procedure->codebuf);
	code = BufWriteZ(procedure->codebuf);
	procedure->did = d_.clause;
	procedure->pflags &= ~PR_DEBUG;
	procedure->mode = 0;
	procedure->size = -PROC_PREFIX_SIZE;
	Set_Procedure_No_Warn(procedure);	/* never warning for source */

	state = clause_pass2(&clds, procedure);
	if (state < 0) {
	    Clean_And_Exit(state)
	}
	code = BufWriteZ(procedure->codebuf);
	Buf_Alloc(procedure->codebuf, code, L_START);
	*code = 0;
	code = CodeStart(code);
	ProcLid(code) = DYNAMIC_PROC;
	New_Label(clds.entry);
	code = gen_clause(&clds, 0, procedure, code);
	Buf_Alloc(procedure->codebuf, code, L_WORD);
	Store_d(Code_end);

	Buf_Flush(procedure->codebuf, code);
#ifdef ALLOC
    p_fprintf(current_output_,
	      "\n%s/%d: source used  %d\n",
	      DidName(procedure->did),
	      DidArity(procedure->did),
	      BufPos(procedure->codebuf, code));
#endif
	procedure->size += BufPos(procedure->codebuf, code);
	copy_dynamic_clause(procedure);
	*codes = procedure->start - DYNAMIC_INSTR_SIZE;

	Untrail_Variables(save_tt);
    } else {					/* a fact */
	code = *codes = (vmcode *) hg_alloc((int)
		(FACT_SOURCE_SIZE + DYNAMIC_INSTR_SIZE) * sizeof(vmcode));
	procedure->start = code = code + DYNAMIC_INSTR_SIZE;
	Store_Arg_2(Out_get_integerAM, Out_get_integerAR, 3, *codea)
	Store_Arg_2(Get_atomAM, Get_atomAR, 2, d_.true0)
	if (arity) {
	    Store_2(Meta_jmpA, *codea + DYNAMIC_INSTR_SIZE)
	} else {
	    Store_i(Ret);
	}
	Store_i(Code_end);
    }

#ifdef PRINTAM
    if (debug_pass3)
	print_procedure(procedure->did, procedure->start);
#endif

_free_memory_:
    Buf_Destroy(procedure->codebuf);
    hg_free((generic_ptr) procedure->gargs);
    hg_free((generic_ptr) procedure->stack);

    Gbl_Tg = save_tg;
    return res;
}
#endif

/*
	PROCEDURE COMPILATION OF DYNAMIC CLAUSES
	compile_dynamic(procedure) compile a set of clauses of a
	dynamic procedure. It is called from compile/1.
	The whole procedure has been read already from a file.
	The procedure is known as dynamic hence is at least initialized.
	It is accessed at the address 'procedure' on the global stack.
	Each clause is compiled and linked to the previous one, using assert.
*/
int
compile_dynamic(proc_desc *procedure, dident module)
{
    pword	*clause;
    value	v;
    int		result;
    proc_desc	proc;	/* could be optimized to use the same */
    
    v.did = module;
    proc.ctype = 1;	/* use for clause numbers */
    proc.proc = procedure->proc;	/* to test flags */
    while (procedure->clauses)
    {
#ifdef OLD_DYNAMIC
	proc.pflags = procedure->pflags & PR_NOWARN;
	clause = (procedure->clauses->clause);
	if ((result = assert_clause(clause->val, clause->tag,
			v, procedure->module_tag, &proc, ASSERTZ)) == PSUCCEED)
#else
	pword *tr = TG;
	dident d_assert_ = in_dict("assert_", 2);
	proc.pflags = procedure->pflags & PR_NOWARN;
	clause = (procedure->clauses->clause);
	Push_Struct_Frame(d_assert_);
	tr[1] = *clause;
	tr[2].val.did = module;
	tr[2].tag.all = procedure->module_tag.all;
	if ((result = do_trafo(tr)) == PSUCCEED)
#endif
	{
	    procedure->clauses = procedure->clauses->next;
	    procedure->size += proc.size;
	}
	else
	{
	    return result;
	}
	proc.ctype++;
    }
    Succeed_;
} /* compile_dynamic */

#ifdef OLD_DYNAMIC

/******************************************************************************/
/*		The Dynamic Database Garbage Collector			      */
/******************************************************************************/

/*
Is called when the size of the 'retracted' code exceeds a set value.
It uses the gc_field of the source clauses of dynamic predicates, which is
normally zero.
Garbage collection is done in two passes : mark and deallocate.
In the mark phase, the control stack is scaned and for every
choice point to a dynamic clause, (compiled or source) the gc field of the
source clause is set to the value of the call clock stored in the choice point.
The kind of clause is determined by the m.s.b. of the arity field of the dynamic
instruction, which is set when the instruction belongs to a source clause.
If the gc field is already set (because another choice point for that clause has
already been encountered) then it is set to the special value
MULTIPLE_CH_POINTS, which signifies to the deallocation phase that more than
one choice points exist for this clause.
At the deallocation phase all dynamic procedures are scanned
(including the abolished ones, whose clauses are stored in a linked list
starting at the global variable abolished_dynamic_procedures).
A pass is made through all the clauses of every procedure and a list is built
of the call clocks encountered so far and stored in the gc_field.
At the same time, if a clause has been retracted and no call clock in the list
is contained in the clause's life interval (Birth, Death], the clause can be
safely removed from the chain and its space freed.
Otherwise, the gc_field is reset to zero for the next garbage collection.
A limitation of the algorithm is that, if more that one choice points exist for
the same (dead) clause, with different call clocks, deallocation can not be
done at this phase and will be done at the next garbage collection.
*/
int
p_garbage_collect_dyn_db(void)
{
	/* Mark phase : go through all the choice points */
	control_ptr	b_aux;
	vmcode		*code_start;

	b_aux.top = B.top - 1;	/* control stack starts at B_ORIG */
	while (B_ORIG < b_aux.args )
	{
		if (IsInterruptFrame(b_aux.top) ||
				IsRecursionFrame(b_aux.top))
		{
		    b_aux.args = b_aux.top->frame.args - SAFE_B_AREA;
		    b_aux.top--;
		    continue;
		}
		code_start = (vmcode *) b_aux.top->backtrack;
		if (SameCode(DynamicInst(code_start), Retry_me_dynamic))
		/* note that this should never be Try_me_dynamic, which would
		   mean a choice point to the first clause of a procedure */
		{
			/*we have a choice point to a retracted dynamic clause*/
			if ((long) Arity(code_start) >= 0)
			/* true if the first bit is zero */
			{
				/* its the compiled clause, so we make 
				   code_start the source clause start */
				code_start= (vmcode *)StartOfSource(code_start);
			}
			if (GcField(code_start) > 0)
			{
				/* there's already a choice point for this one*/
				GcField(code_start) = MULTIPLE_CH_POINTS;
			}
			else
			{
				/* set the gc field to the value of the
				   call clock stored in the choice point
				   (the last argument in the choicepoint) */
				GcField(code_start) =
					(b_aux.args - 1)->val.nint;
			}
		}
		b_aux.any_frame = b_aux.top->frame;
		b_aux.top--;
	}
	_gc_dynamic_procs();
	Succeed_;
} /* p_garbage_collect_dyn_db */




/*
  call _garbage_collect_proc() for all dynamic procedures.
*/
static int
_gc_dynamic_procs(void)
{
    proc_duet	*p_duet;
    uword	saved_killed_size = DynKilledCodeSize;
 
    p_duet = DynamicProcedures;
    while (p_duet)
    {
	_garbage_collect_proc(PriCode(p_duet->desc), NOTYET);
	p_duet = p_duet->next;
    }    
    p_duet = AbolishedDynProcedures;
    while (p_duet)
    {
	vmcode	*code = (vmcode *) (p_duet->desc);
	vmcode	*ass_code = StartOfAss(code);

	_garbage_collect_proc(ass_code, Death(ass_code));
	p_duet = p_duet->next;
	/* Check if only one clause left */
	if (NextAlternative(code) == FAIL)
	{
	    a_mutex_lock(&ProcChainLock);
	    delete_proc_from_chain((pri *) code,
		&AbolishedDynProcedures);
	    a_mutex_unlock(&ProcChainLock);
	    reclaim_procedure(ProcHeader(code));
	}
    }    
    /* DynKilledCodeSize is not incremented by abolish, and so we
       now have garbage in it because it is always decremented
       in _retract_next_clause_of(). So we restore it now */
    DynKilledCodeSize = saved_killed_size;
} /* _gc_dynamic_procs */





/* 
Given the start of the code of a dynamic procedure removes all unnecessary
clauses but the first one.  It expects that the gc fields of the dynamic
instructions of the source clause will have been set to :
0 if there is no choice point for that source clause (or its compiled clause),
MULTIPLE_CH_POINTS if there were more than one choice points or
any other value, if there was exactly one choice point.  Furthermore, this
must be the value of the call clock stored in that choice point.
Since every clause is a candidate for choice points for all previous clauses,
as the clauses are scanned, a list of call clocks encountered so far is created
and a clause is considered dead only if it has been retracted and there is no
call clock in the call clock list for which the clause would be living.
*/
static void
_garbage_collect_proc(vmcode *code_start, uword abolish_clock)
{
    uword	call_clock_list[CALL_CLOCK_LIST_SIZE],
		call_clock_list_top = 0,
		call_clock,
		source_start;
    uword	death;
    vmcode	*next_clause;

    if (!SameCode(DynamicInst(code_start), Try_me_dynamic))
    {
	p_fprintf(current_err_,
	  "dynamic.c/_garbage_collect_proc error: procedure is not dynamic\n");
	ec_flush(current_err_);
    }
    source_start = StartOfSource(code_start);
    while ((next_clause = (vmcode *) NextAlternative(code_start)) != FAIL)
    {
	death = Death(next_clause);
	if (death == NOTYET)
	    death = abolish_clock;
	if (death == NOTYET &&  (GcField(StartOfSource(next_clause))==0))
	{	/* the clause is not yet retracted
		   and there is no choice point to it */
	    code_start = next_clause;
	}
	else
	{
	    call_clock = GcField(StartOfSource(next_clause));
	    GcField(StartOfSource(next_clause)) = 0;
	    if (call_clock == MULTIPLE_CH_POINTS)
	    {
		/*there's nothing more we can do for this proc*/
		_reset_gc_fields(next_clause);
		return;
	    }
	    /* insert in chp list the call clock */
	    if (call_clock_list_top >= CALL_CLOCK_LIST_SIZE)
	    {
		/* we are stuck for this proc */
		_reset_gc_fields(next_clause);
		return;
	    }
	    if (call_clock > 0)
	    {	/* there was a choice point */
		call_clock_list[call_clock_list_top++] = call_clock;
	    }
	    if (_alive(Birth(next_clause), death,
		       call_clock_list, call_clock_list_top))
	    {
		code_start = next_clause;
	    }
	    else
	    {
		_retract_next_clause_of(code_start);
	    }
	}
    }
    /* NextAlternative(code_start) = FAIL */
    StartOfLastClause(source_start) = (vmcode) code_start;
} /* _garbage_collect_proc */


/*
Given the birth and death stamps of a clause, returns true if it is alive
with respect to the global clock and the list of call clocks stored in choice
points (call_clock_list) that point to this or previous clauses of the same
procedure.
*/
static int
_alive(uword birth, uword death, uword *call_clock_list, uword call_clock_list_top)
{
    if (death == NOTYET)
	return 1;
    else
    {
	while (call_clock_list_top > 0)
	{
	    if ((birth < call_clock_list[call_clock_list_top -1 ])
		&&  (call_clock_list[call_clock_list_top -1 ] <= death))
	    {
		return 1;
	    }
	    call_clock_list_top--;
	}
	return 0;
    }
}


/*
Given the start of the code of a clause of a dynamic procedure, actually
retracts the following clause by setting the NextAlternative fields of the
clause and its corresponding source clause to the NextAlternative of their
NextAlternatives.  In addtion the space occupied by the clause is deallocated
and the global variable DynKilledCodeSize is decremented accordingly.
*/
static int
_retract_next_clause_of(vmcode *code_start)
{
    vmcode	*rtrctd_clause_start;

    rtrctd_clause_start = (vmcode *) NextAlternative(code_start);
    NextAlternative(code_start) =
	NextAlternative(NextAlternative(code_start));
    NextAlternative(StartOfSource(code_start)) =
	NextAlternative(NextAlternative(StartOfSource(code_start)));
    DynKilledCodeSize -=
	((HEADER *) rtrctd_clause_start - 1)->s.size
	+ ((HEADER *) StartOfSource(rtrctd_clause_start) - 1)->s.size;
    hg_free((generic_ptr) StartOfSource(rtrctd_clause_start));
    hg_free((generic_ptr) rtrctd_clause_start);
}

static void
_reset_gc_fields(vmcode *code)
{
    while (code != FAIL)
    {
	GcField(StartOfSource(code)) = 0;
	code = (vmcode *) NextAlternative(code);
    }
}

#else

int
p_garbage_collect_dyn_db(void)
{
    Succeed_;
}

#endif
