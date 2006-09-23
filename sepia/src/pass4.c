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
 * VERSION	$Id: pass4.c,v 1.1 2006/09/23 01:56:12 snovello Exp $
 */

/*
 * IDENTIFICATION		pass4.c
 *
 * DESCRIPTION	
 *
 *	SEPIA COMPILER
 *
 * This file contains the routines to handle the compiled intermediate code.
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
#include	"dict.h"
#include	"io.h"
#include	"opcode.h"
#include	"compiler.h"
#include	"database.h"
#include	"gencode.h"

/*
 * DEFINES:
 */
#define Do_Opcode(rptr, wptr)		\
				    if (!SkippingToLabel()) \
					*wptr++ = (vmcode) Op_Value(*rptr++);\
				    else\
					rptr++;
#define Do_Word(rptr, wptr)		\
				    if (!SkippingToLabel()) \
					*wptr++ = (vmcode) *rptr++;\
				    else\
					rptr++;
#define Do_Direct_Opcode(op, wptr)		\
				    if (!SkippingToLabel()) \
					*wptr++ = (vmcode) Op_Value(op);
#define Do_Direct_Word(wd, wptr)		\
				    if (!SkippingToLabel()) \
					*wptr++ = (vmcode) (wd);
#define AddrChain(rptr)			(*((vmcode *) (rptr) + 1))
#define NewPosition(rptr)		(*((vmcode *) (rptr) + 2))
#define Do_Label_Skip(rptr, wptr, skip)		\
				    if (!SkippingToLabel()) {\
					_do_label((vmcode **) rptr, wptr, procedure, skip);\
					Buf_Set_Read(procedure->codebuf, rptr);\
					wptr++;\
				    }\
				    rptr++;
#define Do_Label(rptr, wptr)		Do_Label_Skip(rptr, wptr, 1)
#define Do_Par_Label(rptr, wptr)		\
			    _do_label((vmcode **) rptr, wptr, procedure, 1);\
			    wptr--;
#define Do_Address(rptr, wptr, adr)	if (*rptr == (vmcode) (adr)) \
					    {Do_Word(rptr, wptr)}\
					else\
					    {Do_Label(rptr, wptr)}
#define Do_Hash_Address(rptr, wptr, adr) if (*rptr == (vmcode) (adr)) \
					    {Do_Word(rptr, wptr)}\
					else {\
					    _do_label((vmcode **) rptr, wptr, procedure, 1);\
					    rptr++;\
					    wptr++;\
					}
#define Do_Table_Address(rptr, wptr, adr)	\
				if (*rptr == (vmcode) (adr)) \
				    {Do_Word(rptr, wptr)}\
				else {\
				    Assert(** (vmcode **) rptr == Label)\
				    if (!SkippingToLabel()) {\
					if (NewPosition(*rptr))\
					    *wptr++ = NewPosition(*rptr);\
					else {\
					    *wptr = AddrChain(*rptr);\
					    AddrChain(*rptr) = (vmcode) wptr++;\
					}\
				    }\
				    rptr++;\
				}
#define Link_Labels(rptr, wptr)						\
		{							\
		    register vmcode	*s, *ptr;			\
		    ptr = (vmcode *) AddrChain(rptr);			\
		    while (s = ptr) {					\
			ptr = (vmcode *) *ptr;				\
			*s = (vmcode) (wptr);				\
		    }							\
		    NewPosition(rptr) = (vmcode) (wptr);		\
		    rptr += LABEL_SIZE;	/* don't copy the pseudoinstruction */\
		}
#define Skip_To_Label()			labsearch = 1;
#define SkippingToLabel()		labsearch
#define ProcSize(ptr)			((word) ProcFid(ptr))
#define IsCall(c)			((c) >= CallA && (c) <= JmpdP)

#define RefailCode			par_fail_code_

/*
 * EXTERNAL VARIABLE DECLARATIONS:
 */
extern void	reclaim_procedure(vmcode *code);
extern vmcode	par_fail_code_[];

/*
 * EXTERNAL VARIABLE DEFINITIONS: 
 */

/*
 * STATIC VARIABLE DEFINITIONS: 
 */
static void	_copy_code(proc_desc *procedure, register vmcode *wptr),
		_do_label(vmcode **rptr, vmcode *wptr, proc_desc *procedure, int skip);

static vmcode	*_do_parallel(vmcode *rptr, vmcode *wptr, proc_desc *procedure);

void
copy_procedure(proc_desc *procedure)
{
    vmcode	*code;

    Allocate_Procedure(procedure->size, procedure->bid, procedure->fid,
	procedure->lid, procedure->cid, procedure->did);
    _copy_code(procedure, code);
}

void
copy_dynamic_clause(proc_desc *procedure)
{
    vmcode      *code;

    code = (vmcode *) hg_alloc((int)(procedure->size + DYNAMIC_INSTR_SIZE) *
	sizeof(vmcode));
    code += DYNAMIC_INSTR_SIZE;
    _copy_code(procedure, code);
}

static void
_copy_code(proc_desc *procedure, register vmcode *wptr)
{
    register vmcode	*rptr;
    vmcode		*next;
    vmcode		inst;
    vmcode		*addr;
    vmcode		*old_code = 0;
    long		i;
    int			labsearch = 0;
    int			first = 0;

    rptr = BufReadA(procedure->codebuf);
    procedure->start = wptr;
    do
    {
	next = (vmcode *) *(rptr);
	switch (BlockType(rptr))
	{
	case GROUND_TERM:
	    /* keep it in the block chain */
	    *rptr = ProcLink(procedure->start);
	    ProcLink(procedure->start) = (vmcode) rptr;
	    break;

	case HASH_TABLE:
	    *rptr = (vmcode) old_code;
	    old_code = rptr;
	    rptr = CodeStart(rptr);
	    i = ProcSize(rptr);
	    labsearch = 0;
	    *wptr++ = Op_Value(Comment);
	    addr = wptr++;
#if (SIZEOF_CHAR_P == 8)
	    if ((uword) wptr & 0xf)
#else
	    if ((uword) wptr & 7)
#endif
		wptr++;			/* hash table must be double aligned */
	    if (*rptr == MAX_U_WORD) {	/* seq table */
		*wptr++ = *rptr++;
		*wptr++ = *rptr++;
		i--;
	    }
	    Link_Labels(rptr, wptr);
	    while (i--) {
		Do_Word(rptr, wptr)
		Do_Hash_Address(rptr, wptr, FailCode)
	    }
	    *addr = (vmcode) (wptr-addr-1); /* offset from end of comment instr */
	    break;

	default:	/* a code block */
	    rptr = CodeStart(rptr);
	    Buf_Set_Read(procedure->codebuf, rptr);
	    while ((inst = *rptr) != Code_end)
	    {
		switch (inst)
		{
			/* 0 arguments */
		case Deallocate:
		case Dfid_test:
		case Explicit_jmp:
		case Inst_Error:
		case Match_meta:
		case Match_last_meta:
		case Meta_jmp:
		case Occur_check_next:
		case Push_list:
		case Push_nil:
		case Push_variable:
		case Push_void:
		case Puts_list:
		case Puts_nil:
		case Puts_variable:
		case Read_nil:
		case Read_reference:
		case Read_test_var:
		case Read_variable:
		case Restore_bp:
		case Savecut:
		case Suspension_jmp:
		case Throw:
		case Debug_exit:
		case Wake:
		case Write_list:
		case Write_nil:
		case Write_variable:
		case Write_void:
		    Do_Opcode(rptr, wptr);
		    break;

		case Neckcut:
		    rptr++;
		    if (PriFlags(procedure->proc) & PROC_PARALLEL) {
			Do_Direct_Opcode(Neckcut_par, wptr)
		    } else {
			Do_Direct_Opcode(Neckcut, wptr)
		    }
		    break;

		case First:
		    first = SkippingToLabel() ? 0 : 1;
		    Do_Opcode(rptr, wptr);
		    break;

		case Exit:
		case Exitc:
		case Exitd:
		case Failure:
		case Ret:
		case Retd:
		case Retn:
		    Do_Opcode(rptr, wptr);
		    Skip_To_Label();
		    break;

			/* 1 argument */
		case Allocate:
		case Cut:
		case Cut_single:
		case Depth:
		case Dfid_testL:
		case ExtCall:
		case Escape:
		case Gc_test:
		case Get_list_argumentsAM:
		case Get_nilAM:
		case Get_structure_argumentsAM:
		case In_get_nilAM:
		case Match_metaTM:
		case Match_next_metaTM:
		case Metacall:
		case ModeTM:
		case MoveAM:
		case NextTM:
		case Out_get_listAM:
		case Out_get_nilAM:
		case Push_float:
		case Push_init_variableL:
		case Push_integer:
		case Push_local_valueAM:
		case Push_local_valueL:
		case Push_local_valueTM:
		case Push_reference:
		case Push_self_reference:
		case Push_string:
		case Push_structure:
		case Push_valueAM:
		case Push_valueG:
		case Push_valueL:
		case Push_valueTM:
		case Push_variableAM:
		case Push_variableL:
		case Push_void_reference:
		case Put_listAM:
		case Put_nilAM:
		case Put_variableAM:
		case Puts_atom:
		case Puts_float:
		case Puts_integer:
		case Puts_proc:
		case Puts_string:
		case Puts_structure:
		case Puts_valueAM:
		case Puts_valueG:
		case Puts_valueL:
		case Puts_valueTM:
		case Puts_variableAM:
		case Puts_variableL:
		case Read_atom:
		case Read_float:
		case Read_integer:
		case Read_matched_valueAM:
		case Read_matched_valueL:
		case Read_matched_valueTM:
		case Read_referenceAM:
		case Read_referenceL:
		case Read_string:
		case Read_valueAM:
		case Read_valueL:
		case Read_valueTM:
		case Read_variableAM:
		case Read_variableL:
		case SavecutAM:
		case SoftcutL:
		case Space:
		case Suspension_call:
		case Wake_init:
		case Write_did:
		case Write_float:
		case Write_integer:
		case Write_local_valueAM:
		case Write_local_valueL:
		case Write_local_valueTM:
		case Write_meta:
		case Write_named_variable:
		case Write_named_void:
		case Write_string:
		case Write_structure:
		case Write_valueAM:
		case Write_valueL:
		case Write_valueTM:
		case Write_variableAM:
		case Write_variableL:
		case Fail_clause:
		    Do_Opcode(rptr, wptr);
		    Do_Word(rptr, wptr);
		    break;

		case ChainP:
		case ChaincP:
		case ChaindP:
		case JmpP:
		case JmpdP:
		case Undefined:
		    Do_Opcode(rptr, wptr);
		    Do_Word(rptr, wptr);
		    Skip_To_Label();
		    break;

			/* 2 arguments */
		case CallP:
		case CallfP:
		case CutL:
		case Debug_call:
		case Debug_esc:
		case Fastcall:
		case Gc_testA:
		case Get_atomAM:
		case Get_floatAM:
		case Get_integerAM:
		case Get_matched_valueAMAM:
		case Get_matched_valueAML:
		case Get_matched_valueAMTM:
		case Get_metaAM:
		case Get_stringAM:
		case Get_valueAMAM:
		case Get_valueAML:
		case Get_valueAMTM:
		case In_get_atomAM:
		case In_get_floatAM:
		case In_get_integerAM:
		case In_get_stringAM:
		case Initialize:
		case MoveAMAM:
		case MoveAML:
		case MoveLAM:
		case MoveTMAM:
		case Out_get_atomAM:
		case Out_get_floatAM:
		case Out_get_integerAM:
		case Out_get_stringAM:
		case Out_get_structureAM:
		case Push_constant:
		case Push_init_referenceL:
		case Push_referenceAM:
		case Push_referenceL:
		case Put_atomAM:
		case Put_floatAM:
		case Put_integerAM:
		case Put_stringAM:
		case Put_structureAM:
		case Put_unsafe_valueAML:
		case Put_unsafe_valueAMTM:
		case Put_variableAML:
		case Puts_constant:
		case Puts_reference:
		case Read_constant:
		case Read_referenceNL:
		case Read_variableNL:
		case Res:
		case Write_constant:
		case Write_named_variableAM:
		case Write_named_variableL:
		case Write_variableNL:
		    Do_Opcode(rptr, wptr);
		    Do_Word(rptr, wptr);
		    Do_Word(rptr, wptr);
		    break;

			/* 3 arguments */
		case Get_constantAM:
		case Get_variableNAML:
		case In_get_constantAM:
		case Out_get_constantAM:
		case Put_constantAM:
		case Put_referenceAM:
		case Puts_referenceL:
		case Ress:
		case Write_named_variableNL:
		    Do_Opcode(rptr, wptr);
		    Do_Word(rptr, wptr);
		    Do_Word(rptr, wptr);
		    Do_Word(rptr, wptr);
		    break;

			/* 4 arguments */
		case Put_referenceAML:
		    Do_Opcode(rptr, wptr);
		    Do_Word(rptr, wptr);
		    Do_Word(rptr, wptr);
		    Do_Word(rptr, wptr);
		    Do_Word(rptr, wptr);
		    break;

			/* variable number of arguments */
		case Initialize_named:
		{
		    unsigned long	init_mask;
		    int			was_name = 0;

		    Do_Opcode(rptr, wptr);
		    Do_Word(rptr, wptr);	/* Offset */
		    init_mask = *rptr;
		    Do_Word(rptr, wptr);	/* Mask */
		    Buf_Check(procedure->codebuf, rptr);
		    addr = wptr;
		    if (*rptr != TREF)
			was_name = 1;
		    Do_Word(rptr, wptr);	/* Tag1 */
		    while (init_mask)
		    {
			Buf_Check(procedure->codebuf, rptr);
			if (init_mask & 1) {
			    if (*rptr != TREF)
				was_name = 1;
			    Do_Word(rptr, wptr);
			}
			init_mask >>= 1;
		    }
		    if (!was_name && !SkippingToLabel()) {  /* all non-named */
			*(addr - 3) = Op_Value(Initialize);
			wptr = addr;
		    }
		    break;
		}

		case New_bp:
		case Set_bp:
		    Do_Opcode(rptr, wptr);
		    Do_Label(rptr, wptr);
		    break;

		case ChainA:
		case ChaincA:
		case ChaindA:
		case JmpA:
		case JmpdA:
		    Do_Opcode(rptr, wptr);
		    Do_Label_Skip(rptr, wptr, 0);
		    Skip_To_Label();
		    break;

		case Read_last_list:
		case Read_list:
		    Do_Opcode(rptr, wptr);
		    Do_Address(rptr, wptr, FailCode);
		    break;

		case Get_listAM:
		case GuardL:
		    Do_Opcode(rptr, wptr);
		    Do_Word(rptr, wptr);
		    Do_Label(rptr, wptr);
		    break;

		case ModeTMlab:
		case NextTMlab:
		    Do_Opcode(rptr, wptr);
		    Do_Word(rptr, wptr);
		    Do_Address(rptr, wptr, FailCode);
		    if (!first) {
			Skip_To_Label();
		    }
		    break;

		case Branchs:
		case In_get_listAM:
		case In_get_metaAM:
		case Read_meta:
		case Read_last_meta:
		    Do_Opcode(rptr, wptr);
		    Do_Word(rptr, wptr);
		    Do_Label(rptr, wptr);
		    Skip_To_Label();
		    break;

		case JmpdAs:
		    Do_Opcode(rptr, wptr);
		    Do_Word(rptr, wptr);
		    Do_Label_Skip(rptr, wptr, 0);
		    Skip_To_Label();
		    break;

		case Read_last_structure:
		case Read_listTM:
		case Read_next_listTM:
		case Read_structure:
		    Do_Opcode(rptr, wptr);
		    Do_Word(rptr, wptr);
		    Do_Address(rptr, wptr, FailCode);
		    break;

		case Retry_me_inline:
		    Do_Opcode(rptr, wptr);
		    Do_Word(rptr, wptr);
		    Do_Label(rptr, wptr);
		    Do_Word(rptr, wptr);
		    break;

		case Get_structureAM:
		    Do_Opcode(rptr, wptr);
		    Do_Word(rptr, wptr);
		    Do_Word(rptr, wptr);
		    Do_Label(rptr, wptr);
		    break;

		case In_get_structureAM:
		case Read_metaTM:
		case Read_next_metaTM:
		    Do_Opcode(rptr, wptr);
		    Do_Word(rptr, wptr);
		    Do_Word(rptr, wptr);
		    Do_Label(rptr, wptr);
		    Skip_To_Label();
		    break;

		case Read_next_structureTM:
		case Read_structureTM:
		    Do_Opcode(rptr, wptr);
		    Do_Word(rptr, wptr);
		    Do_Word(rptr, wptr);
		    Do_Address(rptr, wptr, FailCode);
		    break;

		case Meta_jmpA:
		    Do_Opcode(rptr, wptr);
		    Do_Label(rptr, wptr);
		    Skip_To_Label();
		    break;

		case CallA:
		case CallfA:
		    Do_Opcode(rptr, wptr);
		    Do_Label_Skip(rptr, wptr, 0);
		    Do_Word(rptr, wptr);
		    break;

		case List_switchAM:
		    Do_Opcode(rptr, wptr);
		    Do_Word(rptr, wptr);
		    Do_Address(rptr, wptr, FailCode);
		    Do_Address(rptr, wptr, FailCode);
		    Do_Address(rptr, wptr, FailCode);
		    break;

		case Try:
		case Try_me_else:
		    if (PriFlags(procedure->proc) & PROC_PARALLEL) {
			wptr = _do_parallel(rptr, wptr, procedure);
			rptr += 4;
			Buf_Set_Read(procedure->codebuf, rptr);
			Skip_To_Label();
		    } else {
			Do_Opcode(rptr, wptr);
			Do_Word(rptr, wptr);
			Do_Word(rptr, wptr);
			Do_Label(rptr, wptr);
		    }
		    break;

		case Trylab:
		    if (PriFlags(procedure->proc) & PROC_PARALLEL) {
			wptr = _do_parallel(rptr, wptr, procedure);
			rptr += 5;
			Buf_Set_Read(procedure->codebuf, rptr);
		    } else {
			Do_Opcode(rptr, wptr);
			Do_Word(rptr, wptr);
			Do_Word(rptr, wptr);
			Do_Label(rptr, wptr);
			Do_Label(rptr, wptr);
		    }
		    Skip_To_Label();
		    break;

		case Retry:
		case Retry_me_else:
		    Do_Opcode(rptr, wptr);
		    Do_Word(rptr, wptr);
		    Do_Label(rptr, wptr);
		    break;

		case Retrylab:
		    Do_Opcode(rptr, wptr);
		    Do_Word(rptr, wptr);
		    Do_Label(rptr, wptr);
		    Do_Label(rptr, wptr);
		    Skip_To_Label()
		    break;

		case Trust_me_inline:
		    Do_Opcode(rptr, wptr);
		    Do_Word(rptr, wptr);
		    Do_Word(rptr, wptr);
		    break;

		case Trust_me:
		    Do_Opcode(rptr, wptr);
		    Do_Word(rptr, wptr);
		    break;

		case Trust:
		    Do_Opcode(rptr, wptr);
		    Do_Word(rptr, wptr);
		    Do_Label(rptr, wptr);
		    Skip_To_Label();
		    break;

		case Try_parallel:
		    Do_Opcode(rptr, wptr);
		    Do_Word(rptr, wptr);
		    Do_Word(rptr, wptr);
		    Do_Table_Address(rptr, wptr, 0);
		    break;

		case Retry_seq:
		case Try_clause:
		    Do_Opcode(rptr, wptr);
		    Do_Table_Address(rptr, wptr, 0);
		    break;

		case Atom_switchAM:
		case Functor_switchAM:
		case Integer_switchAM:
		    Do_Opcode(rptr, wptr);
		    Do_Word(rptr, wptr);
		    Do_Table_Address(rptr, wptr, 0);
		    Do_Word(rptr, wptr);
		    Do_Address(rptr, wptr, FailCode);
		    break;
			
		case Integer_range_switchAM:
		    Do_Opcode(rptr, wptr);
		    Do_Word(rptr, wptr);
		    Do_Table_Address(rptr, wptr, 0);
		    Do_Word(rptr, wptr);
		    Do_Address(rptr, wptr, FailCode);
		    Do_Address(rptr, wptr, FailCode);
		    break;

		case Switch_on_typeAM:
		    Do_Opcode(rptr, wptr);
		    Do_Word(rptr, wptr);
		    {
			for (i = 0; i < NTYPES; i++)
			{
			    Do_Address(rptr, wptr, FailCode);
			}
		    }
		    break;

		/* should not happen */
		case Retry_me_dynamic:
		case Try_me_dynamic:
		case External:
		case External0:
		case External1:
		case External2:
		case External3:
		    Assert(0);
		    break;

		/* pseudoinstructions and special cases */
		case Nop:
		    rptr++;
		    break;

		case Reserve:
		    i = *rptr;
		    rptr += i;
		    wptr += i;
		    break;

		case Label:
		    if ((long) AddrChain(rptr) == -1)
			rptr += LABEL_SIZE;
		    else {
			labsearch = 0;
			Link_Labels(rptr, wptr)
		    }
		    break;

		case Comment:		/* skip the code */
		    /* arg. is words to skip *after* the comment instr */
		    /* rptr is pointing at op-code, so +2 for instr */
		    rptr = (vmcode *) (rptr + (*(rptr+1) + 2));
		    Buf_Set_Read(procedure->codebuf, rptr);
		    break;

		case Branch:
		    if (SkippingToLabel())
			rptr += 2;
		    else if (*(rptr + 1) == (vmcode) (rptr + 2)) {
			if (NewPosition(rptr + 2)) {
			    /* we have an absolute branch to a non-copied code*/
			    Do_Opcode(rptr, wptr);
			    Do_Label(rptr, wptr);
			    rptr += LABEL_SIZE;
			} else
			    rptr += 2;	/* ignore branch to next word */
		    }
		    else if (*(rptr + 1) == (vmcode) (rptr + 2 + LABEL_SIZE) &&
			*(rptr + 2) == Label)
			rptr += 2;	/* ignore a branch over a label */
		    else {
			addr = rptr;
			do {
			    switch (inst = *addr) {
			    case Label:
				addr += LABEL_SIZE;
				break;

			    case Branch:
				addr = (vmcode *) (*(addr + 1));
				Buf_Set_Read(procedure->codebuf, addr);
				break;

			    case Exit:
			    case Exitc:
			    case Exitd:
			    case Failure:
			    case Ret:
			    case Retd:
			    case Retn:
				Do_Opcode(addr, wptr);
				rptr += 2;
				break;

			    case ChainP:
			    case ChaincP:
			    case ChaindP:
			    case JmpP:
			    case JmpdP:
				Do_Opcode(addr, wptr);
				Do_Word(addr, wptr);
				rptr += 2;
				break;

			    case JmpA:
			    case JmpdA:
				Do_Opcode(addr, wptr);
				Do_Label(addr, wptr);
				rptr += 2;
				break;

			    case Branchs:
			    case JmpdAs:
				Do_Opcode(addr, wptr);
				Do_Word(addr, wptr);
				Do_Label(addr, wptr);
				rptr += 2;
				break;

			    default:
				Do_Opcode(rptr, wptr);
				Do_Label(rptr, wptr);
				break;
			    }
			    Buf_Check(procedure->codebuf, addr);
			} while (inst == Label || inst == Branch);
			Buf_Set_Read(procedure->codebuf, rptr);
			Skip_To_Label();
		    }
		    break;

		case SavecutL:		/* erase SavecutL - CutL */
		    addr = rptr;
		    rptr += 2;
		    Buf_Check(procedure->codebuf, rptr);
		    if (*rptr == CutL && *(rptr + 1) == *(addr + 1))
			rptr += 3;
		    else {
			Do_Opcode(addr, wptr);
			Do_Word(addr, wptr);
		    }
		    break;

		case Read_attribute:	/* count Read_void's	*/
		    addr = rptr;
		    rptr += 2;
		    for (i = 1; *rptr == Read_void; i++) {
			rptr++;
			Buf_Check(procedure->codebuf, rptr);
		    }
		    Do_Direct_Opcode(Read_attribute, wptr)
		    Do_Direct_Word(Esize(i), wptr)
		    break;

		case Read_void:		/* check for Read_voidN */
		    rptr++;
		    for (i = 0; *rptr == Read_void; i++) {
			rptr++;
			Buf_Check(procedure->codebuf, rptr);
		    }
		    if (i) {
			Do_Direct_Opcode(Read_voidN, wptr)
			Do_Direct_Word(Esize(i + 1), wptr)
		    }
		    else {
			Do_Direct_Opcode(Read_void, wptr)
		    }
		    break;

		default:
		    Assert(0);
		}
		Buf_Check(procedure->codebuf, rptr);
	    }
	}
    } while (rptr = next);
    *wptr++ = Code_end;		/* must not be Op_value! */
    if (wptr > procedure->start + procedure->size) {
	p_fprintf(current_output_,
		  "\n%s/%d: allocated %d, used %d\n",
		  DidName(procedure->did),
		  DidArity(procedure->did),
		  procedure->size,
		  wptr - procedure->start);
	ec_flush(current_output_);
	p_reset();
    }
    else			/* fill the extra space with Code_end */
    {
	while (wptr < procedure->start + procedure->size)
	    *wptr++ = Code_end;
    }
    if (old_code)
	reclaim_procedure(old_code);
}

static void
_do_label(vmcode **rptr,
	vmcode *wptr,
	proc_desc *procedure,
	int skip)		/* dereference Branch instructions */
{
    register vmcode	*s = *rptr;
    register vmcode	*r = s;

    Assert(* (vmcode *) s == Label)
    Buf_Set_Read(procedure->codebuf, s);
    for(;;)
    {
	if (*s == Label) {
	    s += LABEL_SIZE;
	    Buf_Check(procedure->codebuf, s);
	}
	else if (skip && *s == Branch) {
	    r = s = (vmcode *) *(s + 1);
	    Buf_Set_Read(procedure->codebuf, s);
	}
	else
	    break;
    }
    if (NewPosition(r))
	*wptr = NewPosition(r);
    else {
	*wptr = AddrChain(r) == -1 ? 0 : AddrChain(r);
	AddrChain(r) = (vmcode) wptr;
    }
}

static vmcode *
_do_parallel(vmcode *rptr, vmcode *wptr, proc_desc *procedure)
{
    vmcode	*addr = rptr;
    vmcode	*table;
    vmcode	*tptr;
    long	nalt = 0;
    int		stop = 0;
    int		labsearch = 0;
    long	arity;

    /* first of all, find out the number of alternatives */
    do {
	switch (*rptr)
	{
	case Try_me_else:
	    nalt++;
	    arity = (long) *(rptr + 2);
	    rptr = (vmcode *) *(rptr + 3);
	    Buf_Set_Read(procedure->codebuf, rptr);
	    break;

	case Try:
	    nalt++;
	    arity = (long) *(rptr + 2);
	    rptr += 4;
	    Buf_Check(procedure->codebuf, rptr);
	    break;

	case Trylab:
	    nalt++;
	    arity = (long) *(rptr + 2);
	    rptr = (vmcode *) *(rptr + 4);
	    Buf_Set_Read(procedure->codebuf, rptr);
	    break;

	case Retry_me_else:
	    nalt++;
	    rptr = (vmcode *) *(rptr + 2);
	    Buf_Set_Read(procedure->codebuf, rptr);
	    break;

	case Retry:
	    nalt++;
	    rptr += 3;
	    Buf_Check(procedure->codebuf, rptr);
	    break;

	case Retrylab:
	    nalt++;
	    rptr = (vmcode *) *(rptr + 3);
	    Buf_Set_Read(procedure->codebuf, rptr);
	    break;

	case Label:
	    AddrChain(rptr) = -1;	/* we'll ignore Retry and Trust */
	    rptr += LABEL_SIZE;
	    Buf_Check(procedure->codebuf, rptr);
	    break;

	case Trust:
	case Trust_me:
	    nalt++;
	    stop++;
	    break;

	default:
	    Assert(0);
	}
    } while (!stop);

    table = (vmcode *) AllocateCodeBlock((nalt + 1) * sizeof(vmcode *),
	ProcLink(procedure->start), 0L, (vmcode) nalt, PARALLEL_TABLE, 0L);
    ProcLink(procedure->start) = (vmcode)(ProcHeader(table));

    rptr = addr;
    Buf_Set_Read(procedure->codebuf, rptr);
    tptr = table + nalt;

    Do_Direct_Opcode(Try_parallel, wptr)
    Do_Direct_Word(nalt, wptr)
    Do_Direct_Word(arity, wptr)
    Do_Direct_Word(table, wptr)

    Do_Direct_Opcode(Retry_seq, wptr)
    Do_Direct_Word(table, wptr)

/*    Do_Direct_Opcode(Retry_par, wptr) */
	Do_Direct_Opcode(Fail_clause, wptr)
	Do_Direct_Word(2, wptr)
	Do_Direct_Opcode(Try_clause, wptr)
    Do_Direct_Word(table, wptr)

    stop = 0;
    do {
	switch (*rptr)
	{
	case Try_me_else:
	    addr = rptr + 4;
	    Buf_Check(procedure->codebuf, addr);
	    Do_Par_Label(&addr, tptr)
	    rptr = (vmcode *) *(rptr + 3);
	    Buf_Set_Read(procedure->codebuf, rptr);
	    break;

	case Try:
	    addr = rptr + 3;
	    Do_Par_Label(addr, tptr);
	    Buf_Set_Read(procedure->codebuf, rptr);	/* possibly changed */
	    rptr += 4;
	    Buf_Check(procedure->codebuf, rptr);
	    break;

	case Trylab:
	    addr = rptr + 3;
	    Do_Par_Label(addr, tptr);
	    rptr = (vmcode *) *(rptr + 4);
	    Buf_Set_Read(procedure->codebuf, rptr);
	    break;

	case Retry_me_else:
	    addr = rptr + 3;
	    Buf_Check(procedure->codebuf, addr);
	    Do_Par_Label(&addr, tptr)
	    rptr = (vmcode *) *(rptr + 2);
	    Buf_Set_Read(procedure->codebuf, rptr);
	    break;

	case Retry:
	    addr = rptr + 2;
	    Do_Par_Label(addr, tptr);
	    Buf_Set_Read(procedure->codebuf, rptr);	/* possibly changed */
	    rptr += 3;
	    Buf_Check(procedure->codebuf, rptr);
	    break;

	case Retrylab:
	    addr = rptr + 2;
	    Do_Par_Label(addr, tptr);
	    rptr = (vmcode *) *(rptr + 3);
	    Buf_Set_Read(procedure->codebuf, rptr);
	    break;

	case Label:
	    rptr += LABEL_SIZE;
	    Buf_Check(procedure->codebuf, rptr);
	    break;

	case Trust_me:
	    stop++;
	    addr = rptr + 2;
	    Buf_Check(procedure->codebuf, addr);
	    Do_Par_Label(&addr, tptr)
	    break;

	case Trust:
	    stop++;
	    addr = rptr + 2;
	    Do_Par_Label(addr, tptr);
	    break;
	}
    } while (!stop);
    *tptr = (vmcode) RefailCode;
    return wptr;
}
