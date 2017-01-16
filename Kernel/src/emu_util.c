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
 * VERSION	$Id: emu_util.c,v 1.17 2017/01/16 19:04:18 jschimpf Exp $
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
#include "ec_io.h"
#include "os_support.h"

#if 0
#define DbgPrintf(s,...) p_fprintf(current_err_,s,__VA_ARGS__);ec_flush(current_err_);
#else
#define DbgPrintf(s,...)
#endif


#ifdef AS_EMU

pword	*bmax_;	/* to define the Gc and overflow checks for the assembler */
pword	*spmax_; /* not for overflow checks, just to know if an address
		  * is in the local stack
		  */

#endif /* AS_EMU */

/* fraction of global_trail size to take as default gc-interval */
#define GC_INTERVAL_FRACTION	64

/* minimal default gc-interval */
#define MIN_GC_INTERVAL	(64*1024)

/*
 * allocate_stacks()
 *
 * allocate Prolog stacks with the given sizes and initialize
 * the pointers to their borders
 */

static void
allocate_stacks(ec_eng_t *ec_eng)
{
    extern void alloc_stack_pairs(int nstacks, char **names, uword *bytes, struct stack_struct **descr);
    static char *names[4] = { "global","trail","control","local" };
    uword sizes[4];
    struct stack_struct *stacks[4];

    stacks[0] = &ec_eng->global_trail[0];
    stacks[1] = &ec_eng->global_trail[1];
    stacks[2] = &ec_eng->control_local[0];
    stacks[3] = &ec_eng->control_local[1];

    sizes[0] = ec_eng->options.globalsize;
    sizes[1] = 0;
    sizes[2] = ec_eng->options.localsize;
    sizes[3] = 0;

    TG_SEG =
    	( ec_eng->options.globalsize/GC_INTERVAL_FRACTION > MIN_GC_INTERVAL ?
	  ec_eng->options.globalsize/GC_INTERVAL_FRACTION : MIN_GC_INTERVAL ) /sizeof(pword);

    alloc_stack_pairs( 4, names, sizes, stacks);

#ifdef AS_EMU

    /* differences with the assembler emulator:
     * - ec_eng->sporigin is set in main() to point into the C stack
     * - B is checked against bmax_ in overflow checks (there is always
     *   room left for one frame of the biggest size (invocation frame))
     */

    bmax_ = (pword *) ((char *) ec_eng->blimit - NARGREGS * sizeof(pword)
            - sizeof(struct invocation_frame));

#if defined(RLIMIT_STACK)
    {
	struct rlimit rlp;
	getrlimit(RLIMIT_STACK, &rlp);

	spmax_ = ec_eng->sporigin - rlp.rlim_cur/sizeof(pword);
    }
#else /* don't know how to find the stack size in SYS_V */
    spmax_ = ec_eng->sporigin - 0x1000000;	/* 16MB */
#endif

#endif /* AS_EMU */

}

/**
 * Prints out the memory layout of the stacks.
 */
int
p_print_stacks(ec_eng_t *ec_eng)
{
    struct stack_struct *stacks[4];
    struct stack_struct *s;
    int i;

    stacks[0] = &ec_eng->global_trail[0];
    stacks[1] = &ec_eng->global_trail[1];
    stacks[2] = &ec_eng->control_local[0];
    stacks[3] = &ec_eng->control_local[1];

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
ec_init_globvars(ec_eng_t *ec_eng)
{
#ifdef DFID
    pword  *p;
    int	i;

    ec_eng->global_variable = TG;
    Push_Struct_Frame(in_dict("gv",GLOBAL_VARS_NO));
    for (i = 0; i < GLOBAL_VARS_NO; i++)
    {
	Make_Integer(&GLOBVAR[i], 0);
    }
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

    ec_eng->references = NULL;
}


/*
 * (re)initialize the abstract machine status on booting or after reset
 * We need to initialize those registers that might not be initialised
 * on emulator entry (save_vm_status), or that need to have a sensible
 * previous value.
 */

void
emu_init(ec_eng_t *parent_eng, ec_eng_t *ec_eng)
{
    int		i;
#ifdef lint
    value v1;
    uword *find_word();

    v1.all = 0;
    (void) lastpp(0);
    (void) find_word((uword) 0);
    (void) check_global();
#endif /* lint */

    ec_eng->ref_ctr = 1;
    ec_eng->needs_dgc_marking = 0;
    mt_mutex_init_recursive(&ec_eng->lock);
    ec_cond_init(&ec_eng->cond);
    allocate_stacks(ec_eng);

    if (parent_eng)
	ec_eng->frand_state = parent_eng->frand_state;	/* inherit */
    else
	ec_frand_init(&ec_eng->frand_state);

    if (ec_eng->options.default_module)
	ec_eng->default_module = in_dict(ec_eng->options.default_module, 0);
    else if (parent_eng) 
	ec_eng->default_module = parent_eng->default_module;
    else
	ec_eng->default_module = d_.default_module;

    /* the stack pointers */
    TG = GCTG = GB = (pword *) ec_eng->global_trail[0].start;
    TT = (pword **) ec_eng->global_trail[1].start;
    if (!trim_global_trail(ec_eng, TG_SEG))		/* sets TG_LIM and TT_LIM */
	ec_panic(MEMORY_P, "emu_init()");

    B.args = PB = PPB = (pword *) ec_eng->control_local[0].start;
#ifndef AS_EMU
    E = SP = EB = (pword *) ec_eng->control_local[1].start;
#endif
    if (!trim_control_local(ec_eng))		/* sets b_limit and sp_limit */
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
    Make_Atom(&POSTED, d_.true0);	/* init posted goals with true */
    PARSENV = NULL;
    Set_Bip_Error(0);
    ec_eng->last_os_error = ec_eng->last_os_errgrp = 0;

    for(i = 0; i < NARGREGS; i++)
    {
	A[i].val.all = 0;
	A[i].tag.kernel = TEND;
    }

    ec_eng->nesting_level = 0;
    ec_eng->it_buf = NULL; /* overwritten in emulc() */
    ec_eng->run_thread = NULL; /* overwritten in emulc() */
    ec_eng->own_thread = NULL;
    ec_eng->owner_thread = ec_thread_self();
    ec_eng->paused = 0;
    ec_eng->storage = NULL;
    ec_eng->report_to = NULL;
    VM_FLAGS = ec_eng->options.vm_options;
    EVENT_FLAGS = 0;

    ec_init_dynamic_event_queue(ec_eng);

    Init_Cleanup();

    Make_Integer(&TAGGED_TD, 0);
    FTRACE = NULL;	/* lazily allocated in debug_reset */
    FCULPRIT = -1;
    DBG_PRI = (pri *) 0;

    if (!ec_options.parallel_worker)
	LEAF = 0;

    ec_init_globvars(ec_eng);
    ec_init_postponed(ec_eng);

    Make_Ref(&ec_eng->allrefs.var,NULL);
    ec_eng->allrefs.next = & ec_eng->allrefs ;
    ec_eng->allrefs.prev = & ec_eng->allrefs ;

    ec_eng->next = ec_eng->prev = NULL;

    TracerInit;

#ifdef PRINTAM
    /* WAM level debugging support */
    ec_eng->stop_address = NULL;
    for(i=0; i<MAX_BACKTRACE; i++)
	ec_eng->backtrace[i] = NULL;
    ec_eng->bt_index = 0;
#endif
}


/*
 * Finalize the engine
 * Expected state on entry:
 *   - we have a strong reference
 *   - we own the engine
 *   - engine is exited
 *   - engine is in chain
 * After:
 *   - no longer in chain
 *   - engine dead (no stacks)
 */

void
ec_emu_fini(ec_eng_t *ec_eng)
{
    void *engine_thread = NULL;

    extern void dealloc_stack_pairs(struct stack_struct *, struct stack_struct *);

    assert(!EngIsDead(ec_eng));
    assert(EngIsOurs(ec_eng));
    assert(!EngIsPaused(ec_eng));

    DbgPrintf("Finalizing engine %x\n", ec_eng);

    /* release any objects/locks the engine may still be holding */
    /* TODO: move this to exit? */
    Do_Cleanup();
    Fini_Cleanup();

    mt_mutex_lock(&ec_eng->lock);	/* prevent new requests being posted */

    /* Handle remaining requests, such as DGC, but ignore exit/throw requests. */
    (void) ecl_housekeeping(ec_eng, 2);

    /* shut down the engine's thread, if any (unless it is ourselves) */
    if (ec_eng->own_thread  &&  ec_eng->own_thread != ec_eng->owner_thread) {
	engine_thread = ec_eng->own_thread;
	ec_eng->own_thread = NULL;	/* indicates finalization request */
	ec_cond_signal(&ec_eng->cond, 0);
	/* ec_thread_join(engine_thread);	done below for concurrency */
    }

    ec_eng->tg = NULL;			/* indicates engine is now dead */
    ec_eng->owner_thread = NULL;	/* final relinquish of ownership */

    mt_mutex_unlock(&ec_eng->lock);	/* lock no longer necessary */

    /* Now the engine is flagged as dead.  It also has no ownership, but
     * since it is dead it cannot be acquired by anyone else.  Other threads
     * can still do read-only accesses to A[1..2] and locking/unlocking,
     * but none of this interferes with the cleanup below.  We still own
     * a strong reference here, and the final destruction operations in
     * ecl_free_engine() will only happen once the last reference disappears.
     */

#ifdef UNCHAIN_ENGINES_WHEN_DEAD
    /* unchain engine, unless it is the header */
    if (ec_eng != eng_list_header) {
	mt_mutex_lock(&EngineListLock);
	ec_eng->next->prev = ec_eng->prev;
	ec_eng->prev->next = ec_eng->next;
	ec_eng->prev = ec_eng->next = ec_eng;
	mt_mutex_unlock(&EngineListLock);

    }
#endif
    dealloc_stack_pairs(ec_eng->global_trail, ec_eng->control_local);

    /* make sure other attached memory is gone */
    destroy_parser_env(ec_eng);
    ec_fini_dynamic_event_queue(ec_eng);	/* after disabling requests */
    assert(!ec_eng->references);
    assert(ec_eng->allrefs.next == &ec_eng->allrefs
    	&& ec_eng->allrefs.prev == &ec_eng->allrefs);
    if (FTRACE) {
	hg_free_size(FTRACE, MAX_FAILTRACE * sizeof(fail_data_t));
	FTRACE = NULL;
    }
    if (ec_eng->report_to) {
	t_ext_ptr report_to = ec_eng->report_to;
	ec_eng->report_to = NULL;
	heap_rec_header_tid.free(report_to);
    }
    if (engine_thread)
	ec_thread_join(engine_thread);	/* make sure it's gone and does not reference ec_eng anymore */

    EngLogMsg(ec_eng, "exited", 0);
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
_compare_handle(value v1, value v2)
{
    /* TODO: comparing the addresses of class descriptors is not ideal,
     * as they may differ between executables.  Better compare some ID.
     */
    int diff = (int)(ExternalClass(v1.ptr) - ExternalClass(v2.ptr));
    if (!diff)
	diff = (int)(ExternalData(v1.ptr) - ExternalData(v2.ptr));
    return diff;
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
_lex_error(ec_eng_t *ec_eng, char* s, pword* result, int base)
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
    tag_desc[THANDLE].compare = _compare_handle;
    tag_desc[TSUSP].compare = _compare_pointers;

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
}


/*
 * Initialize the read-only table opaddr[]
 * It holds the addresses of abstract instructions in the emulator
 * This is only needed for threaded code versions
 * With gcc we use a different scheme and ignore POSTPRO. 
 */

#if defined(THREADED) && defined(HAVE_COMPUTED_GOTO)
vmcode	op_addr[NUMBER_OP];
#endif

void
opaddr_init(void)
{
#ifdef THREADED
#if defined(HAVE_COMPUTED_GOTO)
    op_addr[0] = 0;
    (void) ec_emulate(NULL);	/* to init op_addr[] */
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

lastpp(ec_eng_t *ec_eng, int n)
{
    int i;
    if (n >= MAX_BACKTRACE) i = ec_eng->bt_index;
    else i = (ec_eng->bt_index + MAX_BACKTRACE - n) % MAX_BACKTRACE;
    do {
	print_instr(ec_eng->backtrace[i], 2 /*PROCLAB*/);
	i = (i+1) % MAX_BACKTRACE;
    } while (i != ec_eng->bt_index);
}

#endif /* PRINTAM */

#if defined(PRINTAM)

uword *
find_word(ec_eng_t *ec_eng, uword w)	/* scan Prolog data areas for a particular uword */
{
    uword *p;
    for(p = ec_eng->global_trail[0].start; p < ec_eng->global_trail[0].end; p++)
	if (*p == w) p_fprintf(current_err_, "global 0x%x\n", p);
    for(p = ec_eng->global_trail[1].end; p < ec_eng->global_trail[1].start; p++)
	if (*p == w) p_fprintf(current_err_, "trail 0x%x\n", p);
    for(p = ec_eng->control_local[0].start; p < ec_eng->control_local[0].end; p++)
	if (*p == w) p_fprintf(current_err_, "control 0x%x\n", p);
    for(p = ec_eng->control_local[1].end; p < ec_eng->control_local[1].start; p++)
	if (*p == w) p_fprintf(current_err_, "local 0x%x\n", p);
    for(p = (uword *) &ec_eng->a[0]; p < (uword *) &ec_eng->a[NARGREGS]; p++)
	if (*p == w) p_fprintf(current_err_, "arg 0x%x\n", p);
    ec_flush(current_err_);
}

void
print_chp(ec_eng_t *ec_eng, pword *b, int n)	/* print the n topmost choicepoints (0 = all) */
{
    extern vmcode par_fail_code_[];
    control_ptr fp;
    fp.args = b ? b : B.args;
    do
    {
	if (fp.args <= B_ORIG) {
	    p_fprintf(current_err_, "0x%x --- bottom of control stack\n", fp.args);
	    break;
	}
	p_fprintf(current_err_, "0x%x --- ", fp.args);
	if (BPrev(fp.args) == (pword *) (fp.top - 1))
	{
	    p_fprintf(current_err_, "if-then-else:\n");
	}
	else
	{
	    int arity = (pword*)BTop(fp.args) - (pword*)(BChp(fp.args)+1);

	    if (IsNestingFrame(BTop(fp.args)))
	    {
		p_fprintf(current_err_, "recursion:\n");
		p_fprintf(current_err_,
			"    ppb=0x%x alt=%d node={0x%x,0x%x,0x%x}\n",
			BPar(fp.args)->ppb, BPar(fp.args)->alt,
			BPar(fp.args)->node.site, BPar(fp.args)->node.edge,
			BPar(fp.args)->node.knot);
		arity = (pword*)BTop(fp.args) - (pword*)(BInvoc(fp.args)+1);
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
		arity = (pword*)BTop(fp.args) - (pword*)(BInvoc(fp.args)+1);
	    }
	    else if (IsExceptionFrame(BTop(fp.args)))
	    {
		p_fprintf(current_err_, "exception:\n");
		arity = (pword*)BTop(fp.args) - (pword*)(BException(fp.args)+1);
	    }
	    else if (IsCatchFrame(BTop(fp.args)))
		p_fprintf(current_err_, "catch:\n");
	    else if (IsGcFrame(BTop(fp.args)))
		p_fprintf(current_err_, "gc-dummy:\n");
	    else if (IsRetryMeInlineFrame(BTop(fp.args))
		    || IsTrustMeInlineFrame(BTop(fp.args))
		    || IsRetryInlineFrame(BTop(fp.args))
		    || IsTrustInlineFrame(BTop(fp.args)))
		p_fprintf(current_err_, "inline(0x%lx):\n", BBp(fp.args));
	    else if (IsUnpubParFrame(BTop(fp.args)))
	    {
		p_fprintf(current_err_, "parallel unpublished:\n");
		p_fprintf(current_err_,
			"    ppb=0x%x alt=%d node={0x%x,0x%x,0x%x}\n",
			BPar(fp.args)->ppb, BPar(fp.args)->alt,
			BPar(fp.args)->node.site, BPar(fp.args)->node.edge,
			BPar(fp.args)->node.knot);
		arity = (pword*)BTop(fp.args) - (pword*)(BPar(fp.args)+1);
	    }
	    else if (IsPubParFrame(BTop(fp.args)))
	    {
		p_fprintf(current_err_, "parallel published:\n");
		p_fprintf(current_err_,
			"    ppb=0x%x alt=%d node={0x%x,0x%x,0x%x}\n",
			BPar(fp.args)->ppb, BPar(fp.args)->alt,
			BPar(fp.args)->node.site, BPar(fp.args)->node.edge,
			BPar(fp.args)->node.knot);
		arity = (pword*)BTop(fp.args) - (pword*)(BPar(fp.args)+1);
	    }
	    else if (BBp(fp.args) == par_fail_code_)
	    {
		p_fprintf(current_err_, "parallel dead:\n");
		p_fprintf(current_err_,
			"    ppb=0x%x alt=%d node={0x%x,0x%x,0x%x}\n",
			BPar(fp.args)->ppb, BPar(fp.args)->alt,
			BPar(fp.args)->node.site, BPar(fp.args)->node.edge,
			BPar(fp.args)->node.knot);
		arity = (pword*)BTop(fp.args) - (pword*)(BPar(fp.args)+1);
	    }
	    else
	    {
		p_fprintf(current_err_, "sequential(0x%lx):\n", BBp(fp.args));
	    }

	    p_fprintf(current_err_,
			"    sp=0x%x tg=0x%x tt=0x%x e=0x%x ld=0x%x /%d\n",
			BChp(fp.args)->sp, BChp(fp.args)->tg,
			BChp(fp.args)->tt, BChp(fp.args)->e,
			BChp(fp.args)->ld, arity);
	}
	fp.args = BPrev(fp.args);
    }
    while (--n);
    ec_flush(current_err_);
}


static _print_code_address(stream_id nst, vmcode *code)
{
    extern pri *ec_code_procedure(vmcode *code);
    pri *pd = ec_code_procedure(code);
    if (pd) {
        p_fprintf(nst,"%s/%d+%d",
                DidName(PriDid(pd)), DidArity(PriDid(pd)), code - PriCode(pd));
    } else {
        p_fprintf(nst,"<proc unknown>");
    }
}


/*
 * Print all choicepoints and all environment chains.
 * If execution is currently inside emulator, pass e and sp as parameters!
 */

void
print_control(ec_eng_t *ec_eng, pword *e, pword *sp)
{
    control_ptr		fp;
    pword               *b, *env;
    int                 after_call;
    char                *kind;

    if (!e) e = E;      /* use the exported values, if none given */
    if (!sp) sp = SP;

    p_fprintf(current_err_, "current\n");
    p_fprintf(current_err_, "  rtrnto 0x%lx ?-> 0x%lx ", SP, *(vmcode**)SP);
    _print_code_address(current_err_, *(vmcode**)SP);
    ec_newline(current_err_);
    for(env = E; env < SP_ORIG; env = *(pword**)env)
    {
        vmcode **cpp = ((vmcode**)env)+1;
        p_fprintf(current_err_, "  exitto 0x%lx -> 0x%lx ", cpp, *cpp);
        _print_code_address(current_err_, *cpp);
        ec_newline(current_err_);
    }

    for(b=B.args;;b=fp.args)
    {
        ec_newline(current_err_);
        fp.args = BPrev(b);

	if (IsNestingFrame(BTop(b)))
	{
            p_fprintf(current_err_, "recursion");
            break;
	}
	if (IsRecursionFrame(BTop(b)))
	{
            p_fprintf(current_err_, "invoc");
            break;
	}
	else if (IsCatchFrame(BTop(b)))
        {
            kind = "catch"; after_call = 1;
        }
	else if (IsExceptionFrame(BTop(b)))
        {
            kind = "exception"; after_call = 0;
        }
	else if (IsRetryMeInlineFrame(BTop(b)))
	{
            kind = "inline"; after_call = 0;
	}
	else if (IsTrustMeInlineFrame(BTop(b)))
	{
            kind = "inline"; after_call = 0;
	}
	else if (IsRetryInlineFrame(BTop(b)))
	{
            kind = "inline"; after_call = 0;
	}
	else if (IsTrustInlineFrame(BTop(b)))
	{
            kind = "inline"; after_call = 0;
	}
	else /* if (IsChoicePoint(BTop(b))) */
	{
            kind = "clause"; after_call = 1;
	}

        p_fprintf(current_err_, "%s 0x%lx -> 0x%lx ", kind, b, BBp(b));
        _print_code_address(current_err_, BBp(b));
        ec_newline(current_err_);

        if (after_call)
        {
            p_fprintf(current_err_, "  rtrnto 0x%lx -> 0x%lx ", SP, *(vmcode**)SP);
            _print_code_address(current_err_, *(vmcode**)SP);
            ec_newline(current_err_);
        }
        for(env = fp.chp->e; env < SP_ORIG; env = *(pword**)env)
        {
            vmcode **cpp = ((vmcode**)env)+1;
            p_fprintf(current_err_, "  exitto 0x%lx -> 0x%lx ", cpp, *cpp);
            _print_code_address(current_err_, *cpp);
            ec_newline(current_err_);
        }
    }
    ec_newline(current_err_);
}


#endif /* PRINTAM */
