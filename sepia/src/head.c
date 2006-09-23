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
 * VERSION	$Id: head.c,v 1.1 2006/09/23 01:56:04 snovello Exp $
 */

/*
 * IDENTIFICATION		head.c
 *
 * DESCRIPTION			Compilation of the clause head.
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
#include	"config.h"
#include	"sepia.h"
#include	"types.h"
#include        "embed.h"
#include	"error.h"
#include	"mem.h"
#include 	"dict.h"
#include	"opcode.h"
#include	"compiler.h"
#include	"gencode.h"
#include 	"debug.h"
#include 	"emu_export.h"
#include 	"database.h"		/* for CodeStart only */

 /*
  * DEFINES:
  */
#define Read_Test_Var(match)	if (match) Store_d(Read_test_var);
#define Write_Label(mode, match, lab)	if (match || GroundMode(mode)) \
				    {Store_d(FailCode)}\
				else\
				    {Set_Label(lab)}
/* Overwrite the FailCode */
#define Label_Single(addr)	Label_(addr);

#define Arg(addr)	((pword *) addr - g_emu_.emu_args)

 /*
  * TYPEDEFS:
  */

 /*
  * EXTERNAL VARIABLE DECLARATIONS:
  */
extern vmcode	*compiler_error(int number, cl_desc *clds, proc_desc *procedure, vmcode *code),
		*put_structure(pword *str, cl_desc *clds, proc_desc *procedure, int *str_mode, register vmcode *code);
extern pword	*copy_ground_structure(proc_desc *procedure, value v, type t);
extern void	free_sources(var_desc *tvv, proc_desc *procedure);
extern void	exit(int);
extern pword	woken_susp_;

 /*
  * EXTERNAL VARIABLE DEFINITIONS: 
  */
vmcode  	*get_structure(pword *structure, cl_desc *clds, proc_desc *procedure, vmcode *code, long int i, long int index, int mode, int to_unif, long int match),
		*get_constant(proc_desc *procedure, pword *arg, long int arg_no, int mode, vmcode *code, int wflag);

 /*
  * STATIC VARIABLE DEFINITIONS: 
  */
static int	_check_void_level(proc_desc *procedure, vmcode *wptr, vmcode **pptr, vmcode **cptr),
		_void_rest(pword *next, int count);

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
 * The compilation of the head unification of a Prolog clause.
 * The information collected in the pass2 is used to generate
 * the unification instructions.
 */
vmcode		*
head_unification(cl_desc *clds, proc_desc *procedure, vmcode *code)
{
    arg_desc		*gargs = procedure->gargs;
    int			*head_pass = procedure->head_pass;
    m_item		mode_decl = procedure->mode;
    long		index = procedure->index;
    arg_desc		*ards;
    pword		*arg;
    var_desc		*tvv;
    int			arity = DidArity(procedure->did);
    long		i;
    type		tag;
    value		val;
    int			wake;
    int			mode;		/* of the current argument */
    int			part_mode;	/* auxiliary, to get successive modes */

    /*
     * First unify the indexed argument if it has an indexable type.
     * Otherwise, especially when the indexed argument is a variable,
     * it is processed with the others.
     */
    if (index > 0)
    {
	Buf_Alloc(procedure->codebuf, code, L_GET+L_LAB);
	mode = Mode(index, mode_decl);
	if (ClauseMatching(clds))
	    mode = OutputMode(mode) ? NONVAR : mode | NONVAR;
	ards = gargs + index;
	arg = ards->contents;
	val.all = arg->val.all;
	tag.all = arg->tag.all;
	switch (TagType(tag))
	{
	case TTVV:
	    tvv = Tvv(val);
	    if (!(ClauseMatching(clds) && IsMeta(tvv->header.tag) &&
		    !VarMetaCst(tvv))) {
		New_Label(clds->indexed_entry);   /* not indexed */
		break;
	    }
	    if (ArgUnify(ards)) {	/* changed order */
		Set_Arg_Unify(gargs + ArgNo(tvv->source.next->cont));
		tvv->source.next = 0;
		Reset_Arg_Unify(ards);
	    }
	    /* fall into */
	case TCOMP:
	case TLIST:
	    if (!GroundMode(mode))
	    {
		Gc_TestA(clds, arity);
	    }
	    wake = ClauseWillwake(clds);
	    /* clds->indexed_entry is set in the following function */
	    code = get_structure(arg, clds, procedure,
		code, index, index, mode,
		head_pass[HEAD] - 1, ClauseMatching(clds));
	    if (!InputMode(mode))
	    {
		Set_Clause_Willwake(clds);	/* even if no substructures */
	    }
	    else if (GroundMode(mode) && !wake)
	    {
		Reset_Clause_Willwake(clds);
	    }
	    if (ArgHeadGround(ards))
		head_pass[GROUND_STRUCTURE]--;
	    else
		head_pass[NONGROUND_STRUCTURE]--;
	    break;

	case TDICT:
	    Get_Atom(val.did, index, mode);
	    New_Label(clds->indexed_entry);
	    Set_Arg_Free(ards);
	    if (!InputMode(mode))
		Set_Clause_Willwake(clds);
	    head_pass[CST]--;
	    break;

	case TNIL:
	    Get_Nil(index, mode);
	    New_Label(clds->indexed_entry);
	    Set_Arg_Free(ards);
	    if (!InputMode(mode))
		Set_Clause_Willwake(clds);
	    head_pass[CST]--;
	    break;

	case TINT:
	    Get_Integer(val.nint, index, mode);
	    New_Label(clds->indexed_entry);
	    Set_Arg_Free(ards);
	    if (!InputMode(mode))
		Set_Clause_Willwake(clds);
	    head_pass[CST]--;
	    break;

	case TPROC:			/* if head is shared with the body */
	default:
	    New_Label(clds->indexed_entry);   /* not indexed */
	}
	if (ArgFree(ards))
	    head_pass[HEAD]--;
    }

    /*
     * The core of the unification algorithm. In separate passes
     * through the head arguments it emits the code for the various
     * argument classes, if they exist.
     */
    /*
     * Constants not in output mode
     */
    if (head_pass[CST])
    {
	ards = gargs;
	part_mode = mode_decl;
	for (i = 1; i <= arity; i++)
	{
	    ards++;
	    Next_Mode(part_mode, mode);
	    if (OutputMode(mode) || !ArgOccupied(ards))
		continue;
	    if (ClauseMatching(clds))
		mode |= NONVAR;
	    if ((tag.all = ards->contents->tag.all, !IsCompound(tag)) &&
		!IsTvv(tag))
	    {
		code = get_constant(procedure, ards->contents, i, mode, code, 0);
		Set_Arg_Free(ards);
		if (!InputMode(mode))
		    Set_Clause_Willwake(clds);
		if (!--head_pass[CST])
		    break;
	    }
	}
    }

    /*
     * Multiple head argument variable occurrences which are not in output
     * mode or which appear in special (currently any) goals in the prefix.
     */
    if (head_pass[VALUE])
    {
	ards = gargs;
	part_mode = mode_decl;
	Set_Clause_Willwake(clds);
	for (i = 1; i <= arity; i++)
	{
	    Next_Mode(part_mode, mode);
	    if (ArgUnify(++ards))
	    {
		/*
		 * It is a variable with multiple head occurrences.
		 * We unify it here only if we have to.
		 */
		Assert(ards->contents);
		Assert(IsTvv(ards->contents->tag));
		tvv = Tvv(ards->contents->val);
		if (!OutputMode(mode))
		{
		    uword		loc = VarSource(tvv);

		    Assert(IsA(loc));
		    Buf_Alloc(procedure->codebuf, code, L_GET+L_OC);
		    if (!NoaliasMode(mode)) {
			Occur_Check_Next(Mode(ArgNo(loc), mode_decl))
		    }
		    Get_ValueA(ArgNo(loc), i, ClauseMatching(clds) &&
			!OutputMode(Mode(ArgNo(loc), mode_decl)));
		    if (--tvv->counter ||
			VarPermanent(tvv) && !VarAllocated(tvv))
		    {
			Add_Var_Source(tvv, ContArg(i));
			Set_Arg_Shared(ards);
			Set_Arg_Shared(gargs + ArgNo(loc));
		    }
		    else
		    {
			free_sources(tvv, procedure);
			Set_Arg_Free(ards);
		    }
		    if (!--head_pass[VALUE])
			break;
		}
	    }
	}
    }

    /*
     * Ground head structures not in output mode.
     */
    if (head_pass[GROUND_STRUCTURE])
    {
	Gc_TestA(clds, arity);
	Gc_TestA_Next(clds);
	ards = gargs;
	part_mode = mode_decl;
	for (i = 1; i <= arity; i++)
	{
	    ards++;
	    Next_Mode(part_mode, mode);
	    if (ArgHeadGround(ards) && ArgOccupied(ards) && !OutputMode(mode))
	    {
		wake = ClauseWillwake(clds);
		if (ClauseMatching(clds))
		    mode |= NONVAR;
		code = get_structure(ards->contents, clds, procedure,
		    code, i, 0L, mode, 0, ClauseMatching(clds));
		if (!InputMode(mode))
		{
		    Set_Clause_Willwake(clds);
		}
		else if (GroundMode(mode) && !wake)
		{
		    Reset_Clause_Willwake(clds);
		}
		if (!--head_pass[GROUND_STRUCTURE])
		    break;
	    }
	}
    }

    /*
     * Nonground head structures, not in output mode.
     */
    if (head_pass[NONGROUND_STRUCTURE])
    {
	Gc_TestA(clds, arity);
	Gc_TestA_Next(clds);
	ards = gargs;
	part_mode = mode_decl;
	for (i = 1; i <= arity; i++)
	{
	    ards++;
	    Next_Mode(part_mode, mode);
	    arg = ards->contents;
	    if (ArgOccupied(ards) && !OutputMode(mode) &&
		((tag.all = arg->tag.all, IsCompound(tag)) ||
		IsFirstMetaTvv(arg->val, tag)))
	    {
		wake = ClauseWillwake(clds);
		if (ClauseMatching(clds))
		    mode |= NONVAR;
		code = get_structure(ards->contents, clds, procedure,
		    code, i, 0L, mode, --head_pass[NONGROUND_STRUCTURE],
		    ClauseMatching(clds));
		if (!InputMode(mode))
		{
		    Set_Clause_Willwake(clds);
		}
		else if (GroundMode(mode) && !wake)
		{
		    Reset_Clause_Willwake(clds);
		}
		if (!head_pass[NONGROUND_STRUCTURE])
		    break;
	    }
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
 * This routine generates the instruction sequence for a head constant.
 * The mode is set to ANY for output arguments because otherwise
 * the tag would not be tested and a wrong call could cause problems.
 * wflag is intended to be used inside the write sequence, if needed.
 */
/*ARGSUSED*/
vmcode *
get_constant(proc_desc *procedure, pword *arg, long int arg_no, int mode, vmcode *code, int wflag)
{
    pword		*dest;

    if (OutputMode(mode))
	mode = ANY;
    Buf_Alloc(procedure->codebuf, code, L_GET);
    switch (TagType(arg->tag))
    {
    case TDICT:
	Get_Atom(arg->val.nint, arg_no, mode);
	break;

    case TNIL:
	Get_Nil(arg_no, mode);
	break;

    case TINT:
	Get_Integer(arg->val.nint, arg_no, mode);
	break;

#ifdef TFLOAT
    case TFLOAT:
	Get_Float(arg->val.nint, arg_no, mode);
	break;
#endif

    case TSTRG:
	{
	    value v;
	    v.ptr = enter_string_n(StringStart(arg->val),
			StringLength(arg->val), DICT_CODE_REF);
	    Get_String(v.nint, arg_no, mode);
	}
	break;

    case TCOMP:
	dest = copy_ground_structure(procedure, arg->val, tcomp);
	Get_Constant(TCOMP, dest, arg_no, mode);
	break;

    case TLIST:
	dest = copy_ground_structure(procedure, arg->val, tlist);
	Get_Constant(TLIST, dest, arg_no, mode);
	break;

    case TSUSP:
	Get_Constant(TSUSP, &woken_susp_, arg_no, mode);
	break;

    default:
	if (!IsSimple(arg->tag))
	{
	    dest = copy_ground_structure(procedure, arg->val, arg->tag);
	    Get_Constant(arg->tag.kernel, dest, arg_no, mode);
	}
	else
	    Get_Constant(arg->tag.kernel, arg->val.nint, arg_no, mode);
    }
    return code;
}

#define STRSIZE		100

#define FIRST		0x0000
#define NEXT		0x1000
#define LAST		0x2000
#define ALONE		0x3000

#define Push_(ptr, state, count, temp)		\
	*s++ = (long) (ptr);		\
	*s++ = (state) | (count);	\
	*s++ = (temp);
#define Pop_(ptr, state, count, temp)		\
	temp = *--s;			\
	state = *--s;			\
	ptr = (pword *) *--s;		\
	count = state & 0xFFF;		\
	state &= 0xF000;

/*
 * FUNCTION NAME:
 *
 * PARAMETERS:
 *
 * DESCRIPTION:
 *
 * This routine generates the instruction sequence for a head compound term.
 * It makes a depth-first traversal through the structure using a private
 * stack and generates the 'write mode' sequence. Then it makes a pass
 * through this sequence, generates the 'read mode' sequence and fills in the
 * appropriate labels.
 * If the term contains compound subterms, the current address is stored
 * into a temporary which is then used for further compound arguments
 * of this term, i.e. for the whole level. Deeper levels use own temporaries.
 */
vmcode  *
get_structure(pword *structure,
	cl_desc *clds,
	proc_desc *procedure,
	vmcode *code,
	long int i,
	long int index,
	int mode,
	int to_unif,		/* is there still something to unify?	*/
	long int match)
{
    arg_desc		*gargs = procedure->gargs;
    reg_desc		*regs = procedure->regs;
    int			deep = !ProcedureShallow(procedure) || ClauseDet(clds);
    var_desc		*tvv;		/* variable descriptor		      */
    long		stack[3 * STRSIZE];/* the stack for depth-first pass  */
    long		*s = stack;	/* its stack pointer		      */
    vmcode		*save_label;	/* to fill in the label of 'Get'      */
    vmcode		*start_read;
    pword		*next;		/* right-hand brother of a compnd arg */
    vmcode		*wptr;		/* pointer to the write sequence      */
    vmcode		*pptr;		/* aux pointer to the write sequence  */
    int			state;		/* the current state of the automaton */
    register int	temp;		/* the number of the used Ti	      */
    type		tag;		/* tag of the argument		      */
    value		val;		/* value of the argument	      */
    int			pushed = clds->pushed;
    int			count;		/* the number of remaining arguments  */
    long		loc;		/* the source or dest of a variable   */
    pword		*arg = structure->val.ptr;
    dident		wd;
    int			read_seq = !OutputMode(mode);
    int			in_meta;
    int			meta_attribute = 0;

    Buf_Alloc(procedure->codebuf, code, L_GET+2);
    if (IsList(structure->tag))
    {
	Push_(arg, FIRST, 2, 0);
	Get_List(i, mode);
	in_meta = 0;
    }
    else if (IsStructure(structure->tag))
    {
	if (IsProc(arg->tag))
	{
	    Push_(arg + 1, FIRST, GoalArity(arg), 0);
	    wd = PriDid((pri *)(arg->val.ptr));
	}
	else
	{
	    Push_(arg + 1, FIRST, DidArity(arg->val.did), 0);
	    wd = arg->val.did;
	}
	Get_Structure(wd, i, mode);
	in_meta = 0;
    }
    else if (IsFirstMetaTvv(structure->val, structure->tag)) {
	Push_(Tvv(structure->val)->header.val.ptr, FIRST, 2, 0);
	if (match) {
	    Store_2d(In_get_metaAM, Address(i))
	    read_seq = 1;
	    meta_attribute = 1;
	} else {
#if ALLOW_COMPILED_ATTRIBUTES
	    Store_3d(Get_metaAM, Address(i), Tvv(structure->val)->header.tag.kernel)
	    read_seq = 0;
#else
	    return (compiler_error(ILLEGAL_HEAD, clds,procedure, code));
#endif
	}
	Set_Var_Global(Tvv(structure->val));
	Set_Var_Meta_Cst(Tvv(structure->val));
	in_meta = 1;
    }
    if (gargs[i].contents && IsTvv(gargs[i].contents->tag)) {
	tvv = Tvv(gargs[i].contents->val);
	if (--tvv->counter == 0 && tvv->source.cont)
	    free_sources(tvv, procedure);
    } else {
	Set_Arg_Free(gargs + i);
    }
    /* surprizingly, this does not yield much better code 
    if (OutputMode(mode))
	return put_structure(structure, clds, procedure, &state, code);
     */
    if (read_seq)
	{Forward_Label(save_label)}	/* there will be a read mode label */

    wptr = BufReadW(procedure->codebuf, code);

    while (s > stack)
    {
	Pop_(next, state, count, temp);
	do
	{
	    Buf_Alloc(procedure->codebuf, code, L_UNIFY);
	    tag.all = next->tag.all;
	    val.all = (next++)->val.all;
	    if (--count == 0)
	    {
		if (state == NEXT)
		{
		    Mode2_(temp, pushed, mode, FailCode);
		}
		state = LAST;
		if (!MemoryT(temp))
		{
		    Set_Reg_Free(regs, temp);
		    temp = 0;	/* no temp in use */
		}
	    }
	    if (IsCompound(tag) || IsFirstMetaTvv(val, tag))
	    {
		switch (state)
		{
		case FIRST:
		    if (GroundMode(mode) && _void_rest(next, count))
			break;
		    if (RegFree(regs, Rt1))
		    {
			temp = Rt1;
			Set_Reg_Occupied(regs, Rt1);
		    }
		    else
		    {
			if (!ClauseAllocated(clds) && ClauseRegular(clds))
			{
			    Allocate_(clds->envsize);
			    Set_Clause_Allocated(clds);
			}
			temp = ++pushed;
		    }
		    First_(temp);
		    break;

		case NEXT:
		    Next2_(temp, pushed, FailCode);
		    break;

		case ALONE:
		    Next1_(temp, pushed);
		    break;
		}
		if (state != LAST)	/* count != 0 */
		{
		    Push_(next, NEXT, count, temp);
		    temp = 0;
		}
		if (IsStructure(tag))
		{
		    if (IsProc(val.ptr->tag))
		    {
			Write_Structure(meta_attribute, PriDid((pri *)(val.ptr->val.ptr)));
			count = GoalArity(val.ptr);
		    }
		    else
		    {
			Write_Structure(meta_attribute, val.ptr->val.did);
			count = DidArity(val.ptr->val.did);
		    }
		    next = val.ptr + 1;
		    if (meta_attribute)
			meta_attribute = 0;
		    else {
			New_Label(pptr);	/* dummy arguments */
		    }
		}
		else if (IsList(tag))
		{
		    Write_List(val);
		    count = 2;
		    next = val.ptr;
		    New_Label(pptr);	/* dummy arguments */
		}
		else {
		    Store_2d(Write_meta, Tvv(val)->header.tag.kernel);
		    Set_Var_Global(Tvv(val));
		    Set_Var_Meta_Cst(Tvv(val));
		    count = 2;
		    next = Tvv(val)->header.val.ptr;
		    in_meta = 1;
		    if (match)
			meta_attribute = 1;
		    New_Label(pptr);	/* dummy arguments */
		}
		state = FIRST;
	    }
	    else	       /* simple term */
	    {
		int		arity = DidArity(procedure->did);

		if (state == NEXT)
		{
		    Mode2_(temp, pushed, mode, FailCode);
		    state = ALONE;
		    if (!MemoryT(temp) && count > 1)
		    {
			/* try to release the register */
			pword	*ptr = next + count - 1;
			while (ptr >= next)
			{
			    if (IsCompound(ptr->tag))
				break;
			    ptr--;
			}
			if (ptr < next)
			{
			    Set_Reg_Free(regs, temp);
			    temp = 0;	/* no temp in use */
			}
		    }
		}
		switch (TagType(tag))
		{
		case TTVV:
		    tvv = Tvv(val);
		    if (VarVoid(tvv))
		    {
			Write_Void(tvv);
			break;
		    }
		    loc = VarSource(tvv);
		    if (loc == ContArg(i)) {
			if (tvv->source.next)
			    loc = tvv->source.next->cont;
			else if (VarPermanent(tvv))
			    loc = 0;
			else if (in_meta) {
			    Write_Void(tvv);
			    break;
			}
		    }
		    if (loc)
		    /* subsequent occurrence */
		    {
			if (!VarNoalias(tvv)) {
			    Occur_Check_Next(mode)
			}
			if (VarInit(tvv)) {
			    Reset_Var_Init(tvv)
			    Store_2d(Push_init_variableL, VarPermOff(tvv));
			    Set_Var_Global(tvv);
			}
			else if (VarGlobal(tvv))
			{
			    if (IsA(loc))
			    {
				Write_ValueA(NumberOf(loc));
			    }
			    else if (IsY(loc))
			    {
				Write_ValueL(tvv);
			    }
			    else
			    {
				Write_ValueT(NumberOf(loc), pushed);
			    }
			    in_meta = 0;
			}
			else
			{
			    if (IsA(loc))
			    {
				Write_Local_ValueA(NumberOf(loc));
			    }
			    else if (IsY(loc))
			    {
				Write_Local_ValueL(tvv);
			    }
			    else
			    {
				/*
				 * This instruction could store the global
				 * reference in Ti so that next accesses
				 * can use Write_value instead of
				 * Write_local_value. For Yi it would need
				 * trailing, for Ai it would prevent shallow b.
				 */
				Write_Local_ValueT(NumberOf(loc), pushed);
			    }
			}
			if (--tvv->counter == 0)
			    free_sources(tvv, procedure);
			if (!OutputMode(mode)) {
			    Reset_Var_Uninit(tvv);
			}
			break;
		    }
		    /* first occurrence */
		    if (!OutputMode(mode)) {
			Reset_Var_Uninit(tvv);
		    }
		    if (VarPermanent(tvv))
		    {
			if (!ClauseAllocated(clds))
			{
			    Write_VariableNL(clds->envsize, tvv);
			    Set_Clause_Allocated(clds);
			}
			else
			{
			    Write_VariableL(tvv);
			}
			Set_Var_Head_Global(tvv);
			Set_Var_Allocated(tvv);
			Set_Var_Perm_Source(tvv);
			break;
		    }
		    else if (VarDestination(tvv) &&
			(deep || arity < (int) clds->reg_arity))
		    {
			/*
			 * first occurrence of a temporary variable,
			 * try to find an Ai to put it in. This will
			 * be the famous register optimization.
			 */
			struct var_ch	    *ptr = &tvv->destination;
			struct var_ch	    *save_ptr = 0;
			arg_desc	    *ards;

			while(ptr)
			{
			    loc = ptr->cont;
			    Assert(IsA(loc));
			    if (deep || ArgNo(loc) > arity)
			    {
				if (!ArgFree(ards = gargs + ArgNo(loc)))
				{
				    if (!save_ptr || ArgShared(ards))
					save_ptr = ptr;
				}
				else
				{
				    Set_Arg_Done(ards);
				    Write_VariableA(ArgNo(loc), tvv);
				    Set_Var_Head_Global(tvv);
				    Remove_Cont(ptr);
				    Set_Var_Source(tvv, loc);
				    break;
				}
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
			    Write_VariableA(ArgNo(save_ptr->cont), tvv);
			    Set_Var_Head_Global(tvv);
			    Remove_Cont(save_ptr);
			    Set_Var_Source(tvv, loc);
			    break;
			}
			else if (save_ptr && to_unif == 0 &&
				(s == stack && count == 0) ||
				OutputMode(mode))
			{
			    /*
			     * Try to free a destination which is occupied.
			     * The condition could be released, e.g. count > 0
			     * if all following args are variables etc.
			     */
			    while(save_ptr)
			    {
				loc = save_ptr->cont;
				if (deep || ArgNo(loc) > arity)
				{
				    ards = gargs + ArgNo(loc);
				    if (IsTvv(ards->contents->tag))
				    {
					var_desc		*tvv1;

					tvv1 = Tvv(ards->contents->val);
					if (VarPermanent(tvv1))
					{
					    if (!VarAllocated(tvv1))
					    {
						if (!ClauseAllocated(clds))
						{
						    Get_VariableNL(clds->envsize,
							tvv1, ArgNo(loc));
						    Set_Clause_Allocated(clds);
						} else {
						    Get_VariableL(tvv1,
								    ArgNo(loc));
						}
						Set_Var_Allocated(tvv1);
					    }
					    remove_shared(procedure, ArgNo(loc));
					    Set_Arg_Free(ards);
					}
					else if (ArgUnify(ards))
					{
					    if (!VarNoalias(tvv1)) {
						Occur_Check_Next(Mode(
						   ArgNo(loc), procedure->mode))
					    }
					    Get_ValueA(ArgNo(VarSource(tvv1)),
						ArgNo(loc), match && 
						!OutputMode(Mode(ArgNo(VarSource(tvv1)), procedure->mode)) && !OutputMode(Mode(ArgNo(loc), procedure->mode)));
					    Set_Arg_Free(ards);
					}
				    }
				    /**** this could be done, but macros for
				     * Store_W must be defined (a lot)
				     * NOW NOT ANY LONGER !
				    else if (!IsCompound(ards->contents->tag))
				    {
					code = get_constant(procedure, ards->contents, ArgNo(loc), 
					    Mode(ArgNo(loc), clds->mode), 1);
					Set_Arg_Free(ards);
					if (!InputMode(Mode(ArgNo(loc),
						clds->mode)))
					    Set_Clause_Willwake(clds);
					Set_Arg_Free(ards);
				    }
				    *****/
				    if (ArgFree(ards))
				    {
					Set_Arg_Done(ards);
					Write_VariableA(ArgNo(loc), tvv);
					Set_Var_Head_Global(tvv);
					Remove_Cont(save_ptr);
					Set_Var_Source(tvv, loc);
					break;
				    }
				}
				save_ptr = save_ptr->next;
			    }
			    if (save_ptr)
				break;	
			}
		    }
		    /* We have to use a temporary variable */
		    if (RegFree(regs, Rt1))
		    {
			Set_Reg_Occupied(regs, Rt1);
			loc = Rt1;
			Set_Var_Source(tvv, ContRTmp(loc));
		    }
		    else
		    {
			loc = 0;
			if (ClauseRegular(clds) &&
				!ClauseAllocated(clds))
			{
			    Allocate_(clds->envsize);
			    Set_Clause_Allocated(clds);
			}
			pushed++;
			Set_Var_Source(tvv,ContTmp(pushed));
		    }
		    if (count > 0 && IsTvv(next->tag) &&
			VarSource(Tvv(next->val)) == 0 &&
			IsA(VarDestination(Tvv(next->val))))
		    {
			/*
			 * It is a	write_variableT
			 *		write_variableA
			 * sequence.
			 Assert(0);
			 */
		    }
		    Write_VariableT(loc, tvv);
		    Set_Var_Head_Global(tvv);
		    break;

		case TDICT:
		    Write_Atom(val.nint);
		    break;

		case TNIL:
		    Write_Nil;
		    break;

		case TINT:
		    Write_Integer(val.nint);
		    break;

#ifdef TFLOAT
		case TFLOAT:
		    Write_Float(val.nint);
		    break;
#endif

		case TSTRG:
		    {
			value v;
			v.ptr = enter_string_n(StringStart(val),
				    StringLength(val), DICT_CODE_REF);
			Write_String(v.nint);
		    }
		    break;

		case TSUSP:
		    Write_Constant(TSUSP, &woken_susp_);
		    break;

		 default:
		    if(IsCompound(tag)) {
		      return (compiler_error(WRONG_TYPE, clds,procedure, code));
		    } else if (!IsSimple(tag)) {
		      pword *dest = copy_ground_structure(procedure, val, tag);
		      Write_Constant(tag.kernel, dest);
		    } else {
		      Write_Constant(tag.kernel,val.nint);
		    }
		}
	    }
	}
	while (count);
    }
    clds->pushed = pushed;

    if (!read_seq)
	return code;
    Buf_Alloc(procedure->codebuf, code, L_BRA+L_LAB+L_LAB+L_UNIFY);
    Store_2d(Branch, 0);

    /* read sequence */

    if (i == index)
    {
	New_Label(clds->indexed_entry);
	Gc_TestA(clds, DidArity(procedure->did));
	Gc_TestA_Next(clds);
	if (ClauseAfterRetry(clds))
	{
	    if (IsStructure(structure->tag))
	    {
		Get_Structure_Arguments(i);
	    }
	    else
	    {
		Get_List_Arguments(i);
	    }
	}
    }
    Label_(save_label);		/* set the 'Get' label */
    start_read = code;
    for (;;) {
	Buf_Alloc(procedure->codebuf, code, L_UNIFY);
	Buf_Check(procedure->codebuf, wptr);
	switch (temp = *wptr++)
	{
	case Branch:	       /* end of the sequence (no nested branches) */
	    if (code == start_read && i == index) {	/* no read sequence */
		code = clds->indexed_entry - 2;
		procedure->size += 2*LABEL_SIZE;
		New_Label(clds->indexed_entry);
		Add_Label(save_label, code - LABEL_SIZE);
	    }
	    else {
		Label_(wptr);
	    }
	    return code;

	case ModeTMlab:
	    if (_check_void_level(procedure, wptr + 2, &pptr, &code)) {
		Label_Single(wptr + 1);
		wptr = pptr;
		break;
	    }
	    Store_2d(ModeTM, *wptr++);
	    Label_Single(wptr);
	    wptr++;
	    break;

	case First:	       /* first */
	    Set_Clause_Willwake(clds);
	    if (*wptr == Write_structure)
	    {
		wptr++;
		Store_2d(Read_structure, *wptr);
		Write_Label(mode, match, wptr + 1);
	    }
	    else if (*wptr == Write_list) {
		Store_d(Read_list);
		Write_Label(mode, match, wptr + 1);
	    }
	    else {
		wptr++;
		if (match) {
		    Store_d(Match_meta)
		} else {
		    Store_2d(Read_meta, *wptr)
		    Set_Label(wptr + 1)
		}
	    }
	    wptr += 1 + LABEL_SIZE;
	    break;

	case NextTMlab:       /* next */
	    wptr++;
	    Label_Single(wptr);
	    if (*(wptr + 1) == Write_structure)
	    {
		Store_3d(Read_next_structureTM, *(wptr + 2), *(wptr - 1));
		wptr++;
		Write_Label(mode, match, wptr + 2);
	    }
	    else if (*(wptr + 1) == Write_list)
	    {
		Store_2d(Read_next_listTM, *(wptr - 1));
		Write_Label(mode, match, wptr + 2);
	    }
	    else {
		if (match) {
		    Store_2d(Match_next_metaTM, *(wptr - 1));
		} else {
		    Store_3d(Read_next_metaTM, *(wptr - 1), *(wptr + 2));
		    Set_Label(wptr + 3);
		}
		wptr++;
	    }
	    wptr += LABEL_SIZE + 2;
	    break;

	case NextTM:	       /* alone */
	    if (*(wptr + 1) == Write_structure)
	    {
		Store_3d(Read_structureTM, *(wptr + 2), *wptr);
		wptr++;
		Write_Label(mode, match, wptr + 2);
	    }
	    else if (*(wptr + 1) == Write_list)
	    {
		Store_2d(Read_listTM, *wptr);
		Write_Label(mode, match, wptr + 2);
	    }
	    else {
		if (match) {
		    Store_2d(Match_metaTM, *wptr)
		} else {
		    Store_3d(Read_metaTM, *wptr, *(wptr + 2));
		    Set_Label(wptr + 3);
		}
		wptr++;
	    }
	    wptr += LABEL_SIZE + 2;
	    break;

	case Label:
	    wptr += LABEL_SIZE - 1;
	    break;

	case Write_structure: /* last */
	    Set_Clause_Willwake(clds);
	    Store_2d(Read_last_structure, *wptr);
	    Write_Label(mode, match, wptr + 1);
	    wptr += LABEL_SIZE + 1;
	    break;

	case Write_list:      /* last */
	    Set_Clause_Willwake(clds);
	    Store_d(Read_last_list);
	    Write_Label(mode, match, wptr);
	    wptr += LABEL_SIZE;
	    break;

	case Write_meta:      /* last */
	    if (match) {
		Store_d(Match_last_meta)
	    } else {
		Set_Clause_Willwake(clds);
		Store_2d(Read_last_meta, *wptr);
		Set_Label(wptr + 1);
	    }
	    wptr += LABEL_SIZE + 1;
	    break;

	case Write_void:
	    /*
	     * If there are no further non-void arguments in this level,
	     * skip it.
	     */
	    if (_check_void_level(procedure, wptr, &wptr, &code))
		break;
	    Store_d(temp + 1);
	    break;

	case Write_named_void:
	    if (_check_void_level(procedure, wptr + 1, &wptr, &code))
		break;
	    Store_d(Read_void);
	    wptr++;
	    break;

	case Read_variable:
	case Write_variable:
	    Store_d(Read_variable);
	    break;

	case Write_named_variable:
	    Store_d(Read_variable);
	    wptr++;
	    break;

	case Read_variableNL:
	case Write_variableNL:
	    Store_3d(Read_variableNL, *wptr++, *wptr++);
	    break;

	case Write_named_variableNL:
	    Store_3d(Read_variableNL, *wptr++, *wptr++);
	    wptr++;
	    break;

	case Write_variableL:
	case Write_variableAM:
	    Store_2d(temp + 1, *wptr++);
	    break;

	case Write_named_variableL:
	    Store_2d(Read_variableL, *wptr++);
	    wptr++;
	    break;

	case Write_named_variableAM:
	    Store_2d(Read_variableAM, *wptr++);
	    wptr++;
	    break;

	case Write_valueL:
	case Write_valueTM:
	    if (match)
	    {
		Store_d(temp + 2);
	    }
	    else
	    {
		Set_Clause_Willwake(clds);
		Store_d(temp + 1);
	    }
	    Store_d(*wptr++);
	    break;

	case Write_valueAM:
	    if (match && !OutputMode(Mode(Arg(*wptr), procedure->mode)))
	    {
		Store_d(temp + 2);
	    }
	    else
	    {
		Set_Clause_Willwake(clds);
		Store_d(temp + 1);
	    }
	    Store_d(*wptr++);
	    break;

	case Write_local_valueL:
	case Write_local_valueTM:
	    if (match)
	    {
		Store_d(temp - 1);
	    }
	    else
	    {
		Set_Clause_Willwake(clds);
		Store_d(temp - 2);
	    }
	    Store_d(*wptr++);
	    break;

	case Write_local_valueAM:
	    if (match && !OutputMode(Mode(Arg(*wptr), procedure->mode)))
	    {
		Store_d(temp - 1);
	    }
	    else
	    {
		Set_Clause_Willwake(clds);
		Store_d(temp - 2);
	    }
	    Store_d(*wptr++);
	    break;

	case Push_init_variableL:
	    Store_2d(Read_valueL, *wptr++);
	    break;

	case Write_constant:  
	    Set_Clause_Willwake(clds);
	    Read_Test_Var(match);
	    Store_3d(Read_constant, *wptr++, *wptr++);
	    break;

	case Write_did:
	case Write_integer:
	case Write_float:
	case Write_string:
	    Set_Clause_Willwake(clds);
	    Read_Test_Var(match);
	    Store_2d(temp + 1, *wptr++);
	    break;

	case Write_nil:
	    Set_Clause_Willwake(clds);
	    Read_Test_Var(match);
	    Store_d(Read_nil);
	    break;

	case Allocate:
	case Get_valueAMAR:
	case MoveARL:
	case Read_attribute:
	case Read_referenceL:
	case Read_referenceAM:
	    Store_2d(temp, *wptr++);
	    break;

	case Read_valueL:
	case Read_valueAM:
	case Read_valueTM:
	    if (match) {
		Store_2d(temp + 1, *wptr++);
	    } else {
		Store_2d(temp, *wptr++);
	    }
	    break;

	case Get_variableNAML:
	    Store_4d(temp, *wptr++, *wptr++, *wptr++);
	    break;

	case Get_valueAMAM:
	case MoveAML:
	case Read_referenceNL:
	    Store_3d(temp, *wptr++, *wptr++);
	    break;

	case Read_void:
	    if (_check_void_level(procedure, wptr, &wptr, &code))
		break;
	    Store_d(temp);
	    break;

	case Occur_check_next:
	case Read_reference:
	    Store_d(temp);
	    break;

	Read_Default(wptr-1);
	}
    }
}

/*
 * FUNCTION NAME:	remove_shared(procedure, arg_no)
 *
 * PARAMETERS:		procedure - proedure descriptor
 *			arg_no -   the argument number
 *
 * DESCRIPTION:
 *
 *	When an Ai is used to store a temporary variable, and it currently
 *	holds another variable which is also available in other sources,
 *	this routine updates the arguments to corespond to the
 *	resulting state, i.e. Ai contains the new variable.
 */
remove_shared(proc_desc *procedure, long int arg_no)
{
    arg_desc		*gargs = procedure->gargs;
    var_desc		*tvv = Tvv((gargs + arg_no)->contents->val);
    struct var_ch	*ptr = &tvv->source;
    int			i = 0;
    uword		arg_cont = ContArg(arg_no);

    while (ptr)
    {
	if (ptr->cont == arg_cont)
	{
	    Remove_Cont(ptr)
	}
	else
	    i++;
	ptr = ptr->next;
    }
    if (i < 2)		/* it was only two	*/
    {
	if (!VarSource(tvv))
	{
	    Assert(VarPermanent(tvv) && VarAllocated(tvv));
	    Set_Var_Perm_Source(tvv);
	}
	else if (IsA(VarSource(tvv)))
	{
	    Set_Arg_Occupied(gargs + ArgNo(VarSource(tvv)));
	}
	else if (IsRT(VarSource(tvv)))
	{
	    Set_Reg_Occupied(procedure->regs, NumberOf(VarSource(tvv)));
	}
    }
}

/*
 * FUNCTION NAME:
 *
 * PARAMETERS:
 *
 * DESCRIPTION:
 *
 * 	This routine generates the unification instructions for the
 *	output mode arguments which normally can wait until after the neck.
 *	The 'where' argument might be needed to distinguish between
 *	extended head and prefix.
 */
/*ARGSUSED*/
vmcode *
output_mode(cl_desc *clds, proc_desc *procedure, vmcode *code, int where)
{
    m_item		mode_decl = procedure->mode;
    arg_desc		*gargs = procedure->gargs;
    int			*head_pass = procedure->head_pass;
    int			arity = DidArity(procedure->did);
    int                 debug = ProcedureDebug(procedure);
    long		i;
    arg_desc		*ards;
    int			mode;
    int			present = 0;

    for (i = 1, ards = gargs + 1; i <= arity; i++, ards++)
    {
	Next_Mode(mode_decl, mode);
	if (OutputMode(mode) && (ArgOccupied(ards) || ArgUnify(ards)))
	{
	    /*
	     * This solution seems rather suspect, but I didn't find a better
	     * one. The unification of output mode arguments is done
	     * between an EXIT and a CALL and so the trailing is
	     * not forced. We force it by wrapping it into an invisible call
	     */
	    if (!present) {
		Buf_Alloc(procedure->codebuf, code, L_DB);
		Debug_(d_.dummy_call, d_.kernel_sepia, CALL_PORT, debug);
		present++;
	    }
	    switch (TagType(ards->contents->tag))
	    {
	    case TTVV:
		if (ArgUnify(ards))
		{
		    var_desc	*tvv = Tvv(ards->contents->val);
		    uword		loc = VarSource(tvv);

		    Buf_Alloc(procedure->codebuf, code, L_GET);
		    if (IsA(loc))
		    {
			Get_ValueA(ArgNo(loc), i, 0);
			Set_Arg_Shared(gargs + ArgNo(loc));
		    }
		    else if (IsT(loc))
		    {
			Get_ValueT(NumberOf(loc), i, clds->pushed, 0);
		    }
		    else if (VarAllocated(tvv))
		    {
			Get_ValueL(tvv, i, 0);
		    }
		    else
			Assert(0);
		    if (MemoryA(i))
		    {
			Add_Var_Source(tvv, ContArg(i));
		    }
		    else /* put the register at the beginning */
		    {
			New_Pair(tvv->source.next, VarSource(tvv));
			VarSource(tvv) = ContArg(i);
		    }
		    Set_Arg_Shared(ards);
		    head_pass[OUTPUT_NONVAR]--;
		}
		break;

	    case TCOMP:
	    case TLIST:
		if (!ArgHeadGround(ards))
		    break;
		/* else fall through */
	    default:	/* constant or ground compound */
		if (ClauseMatching(clds)) {
		    Store_d(Failure)
		} else
		    code = get_constant(procedure, ards->contents, i, OUTPUT,
			code, 0);
		Set_Arg_Free(ards);
		head_pass[OUTPUT_NONVAR]--;
	    }
	    Set_Clause_Willwake(clds);
	}
    }
    Buf_Alloc(procedure->codebuf, code, L_DPORT);
    Debug_Exit(debug && present);
    return code;
}


/*
 * FUNCTION NAME:		_check_void_level(procedure, wptr, pptr, cptr)
 *
 * PARAMETERS:			wptr -	the pointer where to start checking
 *					the code of the Write sequence
 *				pptr -	pointer to the wptr pointer
 *					used to generate the Read sequence
 *				cptr -	the current code pointer &, used to
 *					update the ModeTMlab instruction
 *
 * DESCRIPTION:
 *
 *		This routine is used while generating the Read sequence
 *		for a head structure. It checks whether some instructions
 *		in the current level are void. If not, it returns 0,
 *		otherwise it returns nonzero and sets *pptr to point to the next
 *		nonvoid instruction.
 */
static int
_check_void_level(proc_desc *procedure, vmcode *wptr, vmcode **pptr, vmcode **cptr)
{
    vmcode	*code;
    int		skipped = 0;	/* did we change the wptr? */
    vmcode	*save_w = wptr;

    for(;;) {
	Buf_Check(procedure->codebuf, wptr);
	switch (*wptr)
	{
	case Branch:	/* The whole level can be omitted */
	    *pptr = wptr;
	    goto _restore_level_;

	case Write_named_void:
	    wptr++;
	case Read_void:
	case Write_void:
	    wptr++;
	    break;

	case Read_attribute:
	    wptr = wptr + 2 + *(wptr + 1);
	    break;

	case ModeTMlab:
	    skipped = 1;
	    *pptr = wptr;
	    wptr += 3;
	    break;

	default:
	    if (!skipped) {
		Buf_Set_Read(procedure->codebuf, save_w);
		return 0;
	    }
	    else
		goto _restore_level_;
	}
    }

_restore_level_:
    /*
     * Now fill in the labels in the skipped write sequence
     */
    Buf_Set_Read(procedure->codebuf, save_w);
    wptr = save_w;
    Buf_Check(procedure->codebuf, wptr);
    code = *cptr;
    while (wptr != *pptr) {
	switch (*wptr)
	{
	case Read_void:
	case Write_void:
	    wptr++;
	    break;

	case Read_attribute:
	    wptr = wptr + 2 + *(wptr + 1);
	    break;

	case Write_named_void:
	    wptr += 2;
	    break;

	case ModeTMlab:
	    wptr += 2;
	    Buf_Alloc(procedure->codebuf, code, L_LAB);
	    Label_(wptr);
	    wptr++;
	    break;
	}
	Buf_Check(procedure->codebuf, wptr);
    }
    *cptr = code;
    Buf_Set_Read(procedure->codebuf, *pptr);
    return 1;
}

static int
_void_rest(pword *next, int count)
{
    while (count--)
    {
	if (!IsTvv(next->tag) || !VarVoid(Tvv(next->val)))
	    return 0;
	next++;
    }
    return 1;
}
