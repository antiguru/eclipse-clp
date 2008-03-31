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
 * SEPIA C SOURCE MODULE
 *
 * VERSION	$Id: emu_util.c,v 1.5 2008/03/31 14:48:57 jschimpf Exp $
 */

/*
 * IDENTIFICATION		emu_util.c
 *
 */

#include "config.h"

#ifdef AS_EMU
#include <sys/time.h>
#include <sys/resource.h>
#endif

#include "sepia.h"
#include "types.h"
#include "debug.h"
#include "embed.h"
#include "error.h"
#include "mem.h"
#include "opcode.h"
#include "dict.h"
#include "module.h"
#include "emu_export.h"
#include "io.h"
#include "sav_res.h"

extern int 	p_exit(value v, type t);		/* to stop in a clean way */
extern int 	ec_init_postponed(void);

pri	*identical_proc_,
        *not_identical_proc_,
        *inequality_proc_,
        *not_ident_list_proc_,
        *minus_proc_,
        *add_proc_,
        *sub_proc_,
        *mul_proc_,
        *quot_proc_,
        *div_proc_,
        *rem_proc_,
        *fdiv_proc_,
        *mod_proc_,
        *and_proc_,
        *or_proc_,
        *xor_proc_,
        *bitnot_proc_,
        *lt_proc_,
        *le_proc_,
        *gt_proc_,
        *ge_proc_,
        *eq_proc_,
        *ne_proc_,
	*lt_proc3_,
	*le_proc3_,
	*gt_proc3_,
	*ge_proc3_,
	*eq_proc3_,
	*ne_proc3_,
        *arg_proc_,
        *make_suspension_proc_,
	*cut_to_stamp_proc_,
	*fail_proc_;

fail_data_t	fail_trace_[MAX_FAILTRACE];

#ifdef AS_EMU

pword	*bmax_;	/* to define the Gc and overflow checks for the assembler */
pword	*spmax_; /* not for overflow checks, just to know if an address
		  * is in the local stack
		  */

#endif /* AS_EMU */

/* fraction of global_trail size to take as default gc-interval */
#define GC_INTERVAL_FRACTION	32

/* minimal default gc-interval */
#define MIN_GC_INTERVAL	(64*1024)

/*
 * allocate_stacks()
 *
 * allocate Prolog stacks with the given sizes and initialize
 * the pointers to their borders
 */


allocate_stacks(void)
{
    extern void alloc_stack_pairs(int nstacks, char **names, uword *bytes, struct stack_struct **descr);
    static char *names[4] = { "global","trail","control","local" };
    uword sizes[4];
    struct stack_struct *stacks[4];

    stacks[0] = &g_emu_.global_trail[0];
    stacks[1] = &g_emu_.global_trail[1];
    stacks[2] = &g_emu_.control_local[0];
    stacks[3] = &g_emu_.control_local[1];

    sizes[0] = ec_options.globalsize;
    sizes[1] = 0;
    sizes[2] = ec_options.localsize;
    sizes[3] = 0;

    TG_SEG =
    	( ec_options.globalsize/GC_INTERVAL_FRACTION > MIN_GC_INTERVAL ?
	  ec_options.globalsize/GC_INTERVAL_FRACTION : MIN_GC_INTERVAL ) /sizeof(pword);

    alloc_stack_pairs( 4, names, sizes, stacks);

#ifdef AS_EMU

    /* differences with the assembler emulator:
     * - g_emu_.sporigin is set in main() to point into the C stack
     * - B is checked against bmax_ in overflow checks (there is always
     *   room left for one frame of the biggest size (invocation frame))
     */

    bmax_ = (pword *) ((char *) g_emu_.blimit - MAXARITY * sizeof(pword)
            - sizeof(struct invocation_frame));

#if defined(RLIMIT_STACK)
    {
	struct rlimit rlp;
	getrlimit(RLIMIT_STACK, &rlp);

	spmax_ = g_emu_.sporigin - rlp.rlim_cur/sizeof(pword);
    }
#else /* don't know how to find the stack size in SYS_V */
    spmax_ = g_emu_.sporigin - 0x1000000;	/* 16MB */
#endif

#endif /* AS_EMU */

}

/*
 * p_print_stacks()
 * prints out the memory layout of the stacks
 */
int
p_print_stacks(void)
{
    struct stack_struct *stacks[4];
    struct stack_struct *s;
    int i;

    stacks[0] = &g_emu_.global_trail[0];
    stacks[1] = &g_emu_.global_trail[1];
    stacks[2] = &g_emu_.control_local[0];
    stacks[3] = &g_emu_.control_local[1];

    p_fprintf(current_err_,"Name\t\tStart\t\tEnd\t\tPeak\n");
    for(i=0 ; i<4 ; i++)
    {
	s = stacks[i];
	p_fprintf(current_err_,"%s\t\t0x%08x\t0x%08x\t0x%08x\n",
		s->name,s->start,s->end,s->peak);
    }
    ec_flush(current_err_);
    Succeed_;
}


/*
 * Initialize global variables
 * Caution: pushes stuff on global stack
 */
void
ec_init_globvars(void)
{
    pword  *p;
    int	i;

    g_emu_.global_variable = TG;
    Push_Struct_Frame(in_dict("gv",GLOBAL_VARS_NO));
    for (i = 0; i < GLOBAL_VARS_NO; i++)
    {
	Make_Integer(&GLOBVAR[i], 0);
    }
#ifdef DFID
    p = TG;				/* DFID vars */
    TG += 4;
    for (i = 0; i < 4; i++) {
	GLOBVAR[i+1].tag.kernel = TREF;
	GLOBVAR[i+1].val.ptr = p + i;
	p[i].tag.kernel = TINT;
    }
    p[0].val.nint = p[3].val.nint = 0;
    p[1].val.nint = p[2].val.nint = 1000000;
#endif
}


/*
 * (re)initialize the abstract machine status on booting or after reset
 * We need to initialize those registers that might not be initialised
 * on emulator entry (save_vm_status), or that need to have a sensible
 * previous value.
 */

void
emu_init(int flags, int vm_options)
{
    int		i;
#ifdef lint
    value v1;
    uword *find_word();

    v1.all = 0;
    (void) schedule_cut_fail_action(emu_init, v1, tint);
    (void) lastpp(0);
    (void) find_word((uword) 0);
    (void) check_global();
#endif /* lint */

    if (flags & INIT_PRIVATE)
	allocate_stacks();

    /* the stack pointers */
    TG = GCTG = GB = (pword *) g_emu_.global_trail[0].start;
    TT = (pword **) g_emu_.global_trail[1].start;
    if (!trim_global_trail(TG_SEG))		/* sets TG_LIM and TT_LIM */
	ec_panic(MEMORY_P, "emu_init()");

    B.args = PB = PPB = (pword *) g_emu_.control_local[0].start;
#ifndef AS_EMU
    E = SP = EB = (pword *) g_emu_.control_local[1].start;
#endif
    if (!trim_control_local())		/* sets b_limit and sp_limit */
	ec_panic(MEMORY_P, "emu_init()");

    /* some other registers */
    DE = MU = LD = LCA = OCB = TCS = TO = TG_SL = TG_SLS = (pword *) 0;
    FO = PO = (char *) 0;
    PP = (vmcode *) 0;
    WP = LOAD = NTRY = 0;

    /* Push a witness that is older than any choicepoint's witness.
     * It must be the first pword on the global stack!!!
     * (this is assumed by the Init_Stamp() macro)
     */
    Push_Witness;			/* a stamp older than any other */

    Make_Struct(&TAGGED_WL, (pword*)0);	/* WL */
    Make_Ref(&WP_STAMP, (pword*)0);	/* Make_Stamp(&WP_STAMP) */
    Make_Var(&POSTED);			/* difference list of posted goals */
    POSTED_LAST = POSTED;
    PARSENV = NULL;
    Set_Bip_Error(0);

    for(i = 0; i <= MAXARITY; i++)
    {
	A[i].val.all = 0;
	A[i].tag.kernel = TEND;
    }

    g_emu_.nesting_level = 0;
    g_emu_.it_buf = (jmp_buf *) NULL; /* overwritten in emulc() */
    VM_FLAGS = vm_options;
    EVENT_FLAGS = 0;

    ec_init_dynamic_event_queue();

    Make_Integer(&TAGGED_TD, 0);
    FTRACE = fail_trace_;
    FCULPRIT = -1;
    DBG_PRI = (pri *) 0;

    if (!ec_options.parallel_worker)
	LEAF = 0;

    ec_init_globvars();
    ec_init_postponed();

    TracerInit;
}


/*
 * Finalize the engine
 */

void
ec_emu_fini()
{
    extern void dealloc_stack_pairs(struct stack_struct *, struct stack_struct *);
    dealloc_stack_pairs(g_emu_.global_trail, g_emu_.control_local);
}


static int
_equal_value(pword *pw1, pword *pw2)
{
    return pw1 == pw2;
}

static int
_equal_handle(pword *pw1, pword *pw2)
{
    return 
	ExternalClass(pw1) == ExternalClass(pw2)	/* same type */
	&&
	(
	    ExternalData(pw1) == ExternalData(pw2)	/* same value */
	||
	    ExternalClass(pw1)->equal			/* has comp fct */
	    &&
	    ExternalClass(pw1)->equal(ExternalData(pw1), ExternalData(pw2))
	);
}

static int
_compare_dummy(value v1, value v2)
{
    return -1;
}

static int
_arith_compare_dummy(value v1, value v2, int *res)
{
    *res = -1;
    return PERROR;
}

/*ARGSUSED*/
static int
_compare_pointers(value v1, value v2)
{
    return v1.ptr - v2.ptr;
}

/*ARGSUSED*/
static int
_arith_compare_pointers(value v1, value v2, int *res)
{
    *res = v1.ptr - v2.ptr;
    Succeed_;
}

/*ARGSUSED*/
static int
_lex_error(char* s, pword* result, int base)
{
    return BAD_NUMERIC_CONSTANT;
}

/*
 * Bips coded in the emulator
 *
 * to add a new one: add a new call to built_in after the last with flags
 * BIPNO, add the case in the emulator in the instruction Escape and
 * the BIopcode in opcode.h. Also add the name in names.h
 */

void
bip_emu_init(int flags)
{
  pri		*proc;
  int		i;

  if (flags & INIT_PRIVATE)
  {
    int o = 1;

    for (i=0; i <= NTYPES; i++)
    {
	tag_desc[i].equal = _equal_value;
	tag_desc[i].compare = _compare_dummy;
	tag_desc[i].arith_compare = _arith_compare_dummy;
	tag_desc[i].from_string = _lex_error;
	tag_desc[i].string_size = 0;
	tag_desc[i].to_string = 0;
	tag_desc[i].order = 0;
    }

    tag_desc[THANDLE].equal = _equal_handle;
    tag_desc[TSUSP].compare = _compare_pointers;
    tag_desc[TSUSP].arith_compare = _arith_compare_pointers;

    tag_desc[TIVL].order = o++;	/* this determines the type order in @> etc */
    tag_desc[TDBL].order = o++;
    tag_desc[TRAT].order = o++;
    tag_desc[TINT].order =
    tag_desc[TBIG].order = o++;
    tag_desc[TSTRG].order = o++;
    tag_desc[TNIL].order =
    tag_desc[TDICT].order = o++;
    tag_desc[TLIST].order =
    tag_desc[TCOMP].order = o++;
    for (i=0; i <= NTYPES; i++)
    {
	if (!tag_desc[i].order)
	    tag_desc[i].order = o++;
    }

  }

  if (flags & INIT_SHARED)
  {
    /*
     * Initialise the emulator-builtins. Default flag settings:
     * DEBUG_SK: set them all to skipped so they can be handled more efficiently
     * in the Debug_esc instruction (no need to raise an event).
     * PROC_DEMON: set only for predicates that can be re-delayed -
     * their code is responsible for calling Kill_DE before succeeding!
     */
#ifndef NEW_BIP_CONVENTION
    proc = local_built_in(
	in_dict("sys_return", 1),		(int (*) ()) BIExit,
	BIPNO);
	PriFlags(proc) |= DEBUG_SK;
    proc = exported_built_in(
	in_dict("get_cut", 1),			(int (*) ()) BIGetCut,
	BIPNO|U_SIMPLE);
	PriFlags(proc) |= DEBUG_SK;
    cut_to_stamp_proc_ =
    proc = exported_built_in(
	in_dict("cut_to_stamp", 2),		(int (*) ()) BICutToStamp,
	BIPNO);
	PriFlags(proc) |= DEBUG_SK;
    fail_proc_ =
    proc = built_in(d_.fail,			(int (*) ()) BIFail,
	BIPNO);
	PriFlags(proc) |= DEBUG_SK;
    proc = built_in(d_.free1,			(int (*) ()) BIFree,
	BIPNO);
	PriFlags(proc) |= DEBUG_SK;
    proc = built_in(d_.is_suspension,		(int (*) ()) BIIsSuspension,
	BIPNO);
	PriFlags(proc) |= DEBUG_SK;
    proc = built_in(d_.is_event,		(int (*) ()) BIIsEvent,
	BIPNO);
	PriFlags(proc) |= DEBUG_SK;
    proc = built_in(d_.is_handle,		(int (*) ()) BIIsHandle,
	BIPNO);
	PriFlags(proc) |= DEBUG_SK;
    proc = built_in(d_.var,			(int (*) ()) BIVar,
	BIPNO);
	PriFlags(proc) |= DEBUG_SK;
    proc = built_in(d_.nonvar,			(int (*) ()) BINonVar,
	BIPNO);
	PriFlags(proc) |= DEBUG_SK;
    proc = built_in(d_.meta,			(int (*) ()) BIMeta,
	BIPNO);
	PriFlags(proc) |= DEBUG_SK;
    proc = built_in(d_.atom,			(int (*) ()) BIAtom,
	BIPNO);
	PriFlags(proc) |= DEBUG_SK;
    proc = built_in(d_.integer,			(int (*) ()) BIInteger,
	BIPNO);
	PriFlags(proc) |= DEBUG_SK;
    proc = built_in(d_.rational1,		(int (*) ()) BIRational,
	BIPNO);
	PriFlags(proc) |= DEBUG_SK;
    proc = built_in(d_.real,			(int (*) ()) BIReal,
	BIPNO);
	PriFlags(proc) |= DEBUG_SK;
    proc = built_in(d_.float1,			(int (*) ()) BIFloat,
	BIPNO);
	PriFlags(proc) |= DEBUG_SK;
    proc = built_in(d_.breal,			(int (*) ()) BIBreal,
	BIPNO);
	PriFlags(proc) |= DEBUG_SK;
    proc = built_in(d_.string,			(int (*) ()) BIString,
	BIPNO);
	PriFlags(proc) |= DEBUG_SK;
    proc = built_in(d_.number,			(int (*) ()) BINumber,
	BIPNO);
	PriFlags(proc) |= DEBUG_SK;
    proc = built_in(d_.atomic,			(int (*) ()) BIAtomic,
	BIPNO);
	PriFlags(proc) |= DEBUG_SK;
    proc = built_in(d_.compound,		(int (*) ()) BICompound,
	BIPNO);
	PriFlags(proc) |= DEBUG_SK;
    proc = built_in(d_.is_list,			(int (*) ()) BIIsList,
	BIPNO);
	PriFlags(proc) |= DEBUG_SK;
    identical_proc_ =
    proc = built_in(
	d_.identical,				(int (*) ()) BIIdentical,
	BIPNO);
	PriFlags(proc) |= DEBUG_SK;
    not_identical_proc_ =
    proc = built_in(
	d_.not_identical,			(int (*) ()) BINotIdentical,
	BIPNO);
	PriFlags(proc) |= DEBUG_SK;
    not_ident_list_proc_ = 
    proc = exported_built_in(
	in_dict("\\==",3),			(int (*) ()) BINotIdentList,
	BIPNO);
	PriMode(proc) = BoundArg(3, NONVAR);
	PriFlags(proc) |= DEBUG_SK;
    inequality_proc_ =
    proc = built_in(
	d_.diff_reg,				(int (*) ()) BIInequality,
	BIPNO|PROC_DEMON);
	PriFlags(proc) |= DEBUG_SK;
    proc = built_in(d_.unify,				(int (*) ()) BIUnify,
	BIPNO|U_UNIFY);
	PriMode(proc) = BoundArg(1, NONVAR) | BoundArg(2, NONVAR);
	PriFlags(proc) |= DEBUG_SK;
    proc = exported_built_in(
	in_dict("set_bip_error",1),		(int (*) ()) BISetBipError,
	BIPNO);
	PriFlags(proc) |= DEBUG_SK;
    proc = exported_built_in(
	in_dict("get_bip_error",1),		(int (*) ()) BIGetBipError,
	BIPNO|U_SIMPLE);
	PriFlags(proc) |= DEBUG_SK;
#ifdef SAVEDSTATES
    (void) built_in(in_dict("save", 1),	(int (*) ()) BISave,	BIPNO);
    (void) built_in(in_dict("restore", 1),	(int (*) ()) BIRestore,	BIPNO);
#endif
    proc = local_built_in(in_dict("cont_debug",0),	(int (*) ()) BIContDebug,
	BIPNO);
	PriFlags(proc) |= DEBUG_SK;

    minus_proc_ =
    proc = built_in(in_dict("-",2), (int (*) ()) BIMinus,BIPNO|U_SIMPLE);
	PriMode(proc) = BoundArg(2, CONSTANT);
	PriFlags(proc) |= DEBUG_SK;
    add_proc_ =
    proc = built_in(in_dict("+",3), (int (*) ()) BIAdd,	BIPNO|U_SIMPLE|PROC_DEMON);
	PriMode(proc) = BoundArg(3, CONSTANT);
	PriFlags(proc) |= DEBUG_SK;
    sub_proc_ =
    proc = built_in(in_dict("-",3), (int (*) ())BISub, 	BIPNO|U_SIMPLE|PROC_DEMON);
	PriMode(proc) = BoundArg(3, CONSTANT);
	PriFlags(proc) |= DEBUG_SK;
    mul_proc_ =
    proc = built_in(in_dict("*",3), (int (*) ())BIMul, 	BIPNO|U_SIMPLE|PROC_DEMON);
	PriMode(proc) = BoundArg(3, CONSTANT);
	PriFlags(proc) |= DEBUG_SK;
    quot_proc_ =
    proc = built_in(in_dict("/",3), (int (*) ())BIQuot,	BIPNO|U_SIMPLE|PROC_DEMON);
	PriMode(proc) = BoundArg(3, CONSTANT);
	PriFlags(proc) |= DEBUG_SK;
    div_proc_ =
    proc = built_in(in_dict("//",3), (int (*) ())BIDiv,	BIPNO|U_SIMPLE|PROC_DEMON);
	PriMode(proc) = BoundArg(3, CONSTANT);
	PriFlags(proc) |= DEBUG_SK;
    rem_proc_ =
    proc = built_in(in_dict("rem",3), (int (*) ())BIRem,BIPNO|U_SIMPLE|PROC_DEMON);
	PriMode(proc) = BoundArg(3, CONSTANT);
	PriFlags(proc) |= DEBUG_SK;
    fdiv_proc_ = 
    proc = built_in(in_dict("div",3), (int (*) ())BIFloorDiv,BIPNO|U_SIMPLE|PROC_DEMON);
	PriMode(proc) = BoundArg(3, CONSTANT);
	PriFlags(proc) |= DEBUG_SK;
    mod_proc_ =
    proc = built_in(in_dict("mod",3), (int (*) ())BIFloorRem,BIPNO|U_SIMPLE|PROC_DEMON);
	PriMode(proc) = BoundArg(3, CONSTANT);
	PriFlags(proc) |= DEBUG_SK;
    and_proc_ =
    proc = built_in(in_dict("/\\",3), (int (*) ())BIAnd,BIPNO|U_SIMPLE|PROC_DEMON);
	PriMode(proc) = BoundArg(3, CONSTANT);
	PriFlags(proc) |= DEBUG_SK;
    or_proc_ =
    proc = built_in(in_dict("\\/",3), (int (*) ())BIOr,	BIPNO|U_SIMPLE|PROC_DEMON);
	PriMode(proc) = BoundArg(3, CONSTANT);
	PriFlags(proc) |= DEBUG_SK;
    xor_proc_ =
    proc = built_in(in_dict("xor",3), (int (*) ())BIXor,BIPNO|U_SIMPLE|PROC_DEMON);
	PriMode(proc) = BoundArg(3, CONSTANT);
	PriFlags(proc) |= DEBUG_SK;
    bitnot_proc_ =
    proc = built_in(in_dict("\\",2), (int (*) ())BIBitnot,BIPNO|U_SIMPLE);
	PriMode(proc) = BoundArg(2, CONSTANT);
	PriFlags(proc) |= DEBUG_SK;

    lt_proc_ =
    proc = built_in(d_.inf,		(int (*) ())BILt,	BIPNO|PROC_DEMON);
	PriFlags(proc) |= TOOL|DEBUG_SK;
    le_proc_ =
    proc = built_in(d_.infq,		(int (*) ())BILe,	BIPNO|PROC_DEMON);
	PriFlags(proc) |= TOOL|DEBUG_SK;
    gt_proc_ =
    proc = built_in(d_.sup,		(int (*) ())BIGt,	BIPNO|PROC_DEMON);
	PriFlags(proc) |= TOOL|DEBUG_SK;
    ge_proc_ = 
    proc = built_in(d_.supq,		(int (*) ())BIGe,	BIPNO|PROC_DEMON);
	PriFlags(proc) |= TOOL|DEBUG_SK;
    eq_proc_ =
    proc = built_in(d_.equal,		(int (*) ())BIEq,	BIPNO|PROC_DEMON);
	PriFlags(proc) |= TOOL|DEBUG_SK;
    ne_proc_ =
    proc = built_in(d_.not_equal, 	(int (*) ())BINe,	BIPNO|PROC_DEMON);
	PriFlags(proc) |= TOOL|DEBUG_SK;

    arg_proc_ =
    proc = built_in(
	in_dict("arg",3),			(int (*) ())BIArg,
	BIPNO|U_UNIFY|PROC_DEMON);
	PriMode(proc) = BoundArg(2, NONVAR) | BoundArg(3, NONVAR);
	PriFlags(proc) |= DEBUG_SK;

    make_suspension_proc_ =
    proc = exported_built_in(
	in_dict("make_suspension",4),		(int (*) ())BIMakeSuspension,
	BIPNO|U_UNIFY);
	PriMode(proc) =	BoundArg(3, NONVAR);
	PriFlags(proc) |= DEBUG_INVISIBLE;
#endif
  }
  else if (flags & INIT_PRIVATE) /* just init the private pri pointers */
  {
#ifdef NEW_BIP_CONVENTION
    type tm;
    tm.kernel = ModuleTag(d_.kernel_sepia);
#define KernelProc(d) local_procedure(d, d_.kernel_sepia, tm, 0)

    cut_to_stamp_proc_ = KernelProc(in_dict("cut_to_stamp", 2));
    fail_proc_ = KernelProc(d_.fail);
    identical_proc_ = KernelProc(d_.identical);
    not_identical_proc_ = KernelProc(d_.not_identical);
    not_ident_list_proc_ = KernelProc(in_dict("\\==",3));
    inequality_proc_ = KernelProc(d_.diff_reg);
    minus_proc_ = KernelProc(in_dict("-",2));
    add_proc_ = KernelProc(in_dict("+",3));
    sub_proc_ = KernelProc(in_dict("-",3));
    mul_proc_ = KernelProc(in_dict("*",3));
    quot_proc_ = KernelProc(in_dict("/",3));
    div_proc_ = KernelProc(in_dict("//",3));
    rem_proc_ = KernelProc(in_dict("rem",3));
    fdiv_proc_ = KernelProc(in_dict("div",3));
    mod_proc_ = KernelProc(in_dict("mod",3));
    and_proc_ = KernelProc(in_dict("/\\",3));
    or_proc_ = KernelProc(in_dict("\\/",3));
    xor_proc_ = KernelProc(in_dict("xor",3));
    bitnot_proc_ = KernelProc(in_dict("\\",3));
    lt_proc_ = KernelProc(d_.inf);
    le_proc_ = KernelProc(d_.infq);
    gt_proc_ = KernelProc(d_.sup);
    ge_proc_ = KernelProc(d_.supq);
    eq_proc_ = KernelProc(d_.equal);
    ne_proc_ = KernelProc(d_.not_equal);
    arg_proc_ = KernelProc(in_dict("arg",3));
    make_suspension_proc_ = KernelProc(in_dict("make_suspension",4));
#endif
  }
}


/*
 * Initialize the read-only table opaddr[]
 * It holds the addresses of abstract instructions in the emulator
 * This is only needed for threaded code versions
 * With gcc we use a different scheme and ignore POSTPRO. 
 */

#if defined(THREADED) && !defined(POSTPRO)
vmcode	op_addr[NUMBER_OP];
#endif

void
opaddr_init(void)
{
#ifdef THREADED
#if defined(__GNUC__) || defined(_WIN32)
    op_addr[0] = 0;
    (void) ec_emulate();	/* will init op_addr[] */
    if (op_addr[Retry] == op_addr[Retry_inline]
     || op_addr[Trust] == op_addr[Trust_inline])
     {
	ec_panic("Instructions not distinguishable - C compiler too clever", "opaddr_init()");
     }
#else
#ifdef POSTPRO
#ifdef mc68000
    int i, j;

    for (i=0,j=0; i<NUMBER_OP; i++)
    {
	/*
	 * For compilers that generate switch tables with relative offsets,
	 * we have to compute the op_addr[] array from this switch table
	 * (otherwise the switch table can be used directly as op_addr[])
	 * If the -J option is used in cc, opswitch_table[] has to be long int!
	 */
	extern short opswitch_table[]; /* opt_sun3.sh inserts this label */

	op_addr[i] = (long) opswitch_table[i] + (long) opswitch_table;
    }
#endif
#endif
#endif
#endif /*THREADED*/
}


#if defined(PRINTAM) || defined(LASTPP)

/*
 * lastpp(n) - a tool for debugging the emulator
 * prints the n most recently executed abstract instructions
 * can be called from dbx etc.
 */

lastpp(int n)
{
    extern vmcode *ec_backtrace[];
    extern int bt_index, bt_max;
    extern vmcode *print_am(register vmcode *code, vmcode **label, int *res, int option);
    int i;
    vmcode	*dummy_l = NULL;
    int		dummy_r;

    if (n >= bt_max) i = bt_index;
    else i = (bt_index + bt_max - n) % bt_max;
    do {
	(void) print_am(ec_backtrace[i], &dummy_l, &dummy_r, 2 /*PROCLAB*/);
	i = (i+1) % bt_max;
    } while (i != bt_index);
}

#endif /* PRINTAM */

#if defined(PRINTAM)

uword *
find_word(uword w)	/* scan Prolog data areas for a particular uword */
{
    uword *p;
    for(p = g_emu_.global_trail[0].start; p < g_emu_.global_trail[0].end; p++)
	if (*p == w) p_fprintf(current_err_, "global 0x%x\n", p);
    for(p = g_emu_.global_trail[1].end; p < g_emu_.global_trail[1].start; p++)
	if (*p == w) p_fprintf(current_err_, "trail 0x%x\n", p);
    for(p = g_emu_.control_local[0].start; p < g_emu_.control_local[0].end; p++)
	if (*p == w) p_fprintf(current_err_, "control 0x%x\n", p);
    for(p = g_emu_.control_local[1].end; p < g_emu_.control_local[1].start; p++)
	if (*p == w) p_fprintf(current_err_, "local 0x%x\n", p);
    for(p = (uword *) &g_emu_.emu_args[0];
				p < (uword *) &g_emu_.emu_args[MAXARITY]; p++)
	if (*p == w) p_fprintf(current_err_, "arg 0x%x\n", p);
    ec_flush(current_err_);
}

void
print_chp(pword *b, int n)	/* print the n topmost choicepoints (0 = all) */
{
    extern vmcode par_fail_code_[];
    control_ptr fp;
    fp.args = b ? b : B.args;
    do
    {
	p_fprintf(current_err_, "0x%x --- ", fp.args);
	if (BPrev(fp.args) == (pword *) (fp.top - 1))
	{
	    p_fprintf(current_err_, "if-then-else:\n");
	}
	else
	{
	    if (IsInterruptFrame(BTop(fp.args)))
	    {
		p_fprintf(current_err_, "interrupt:\n");
		n=1;
	    }
	    else if (IsRecursionFrame(BTop(fp.args)))
	    {
		p_fprintf(current_err_, "invocation:\n");
		n=1;
		p_fprintf(current_err_,
			"    ppb=0x%x alt=%d node={0x%x,0x%x,0x%x}\n",
			BPar(fp.args)->ppb, BPar(fp.args)->alt,
			BPar(fp.args)->node.site, BPar(fp.args)->node.edge,
			BPar(fp.args)->node.knot);
	    }
	    else if (IsExceptionFrame(BTop(fp.args)))
		p_fprintf(current_err_, "exception:\n");
	    else if (IsCatchFrame(BTop(fp.args)))
		p_fprintf(current_err_, "catch:\n");
	    else if (IsGcFrame(BTop(fp.args)))
		p_fprintf(current_err_, "gc-dummy:\n");
	    else if (IsRetryMeInlineFrame(BTop(fp.args))
		    || IsTrustMeInlineFrame(BTop(fp.args))
		    || IsRetryInlineFrame(BTop(fp.args))
		    || IsTrustInlineFrame(BTop(fp.args)))
		p_fprintf(current_err_, "inline:\n");
	    else if (IsUnpubParFrame(BTop(fp.args)))
	    {
		p_fprintf(current_err_, "parallel unpublished:\n");
		p_fprintf(current_err_,
			"    ppb=0x%x alt=%d node={0x%x,0x%x,0x%x}\n",
			BPar(fp.args)->ppb, BPar(fp.args)->alt,
			BPar(fp.args)->node.site, BPar(fp.args)->node.edge,
			BPar(fp.args)->node.knot);
	    }
	    else if (IsPubParFrame(BTop(fp.args)))
	    {
		p_fprintf(current_err_, "parallel published:\n");
		p_fprintf(current_err_,
			"    ppb=0x%x alt=%d node={0x%x,0x%x,0x%x}\n",
			BPar(fp.args)->ppb, BPar(fp.args)->alt,
			BPar(fp.args)->node.site, BPar(fp.args)->node.edge,
			BPar(fp.args)->node.knot);
	    }
	    else if (BBp(fp.args) == par_fail_code_)
	    {
		p_fprintf(current_err_, "parallel dead:\n");
		p_fprintf(current_err_,
			"    ppb=0x%x alt=%d node={0x%x,0x%x,0x%x}\n",
			BPar(fp.args)->ppb, BPar(fp.args)->alt,
			BPar(fp.args)->node.site, BPar(fp.args)->node.edge,
			BPar(fp.args)->node.knot);
	    }
	    else
	    {
		p_fprintf(current_err_, "sequential:\n");
	    }

	    p_fprintf(current_err_,
			"    sp=0x%x tg=0x%x tt=0x%x e=0x%x ld=0x%x\n",
			BChp(fp.args)->sp, BChp(fp.args)->tg,
			BChp(fp.args)->tt, BChp(fp.args)->e,
			BChp(fp.args)->ld);
	}
	fp.args = BPrev(fp.args);
    }
    while (--n);
    ec_flush(current_err_);
}

#endif /* PRINTAM */
