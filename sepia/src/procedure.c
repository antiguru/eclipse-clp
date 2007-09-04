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
 * VERSION	$Id: procedure.c,v 1.2 2007/09/04 16:57:34 jschimpf Exp $
 */

/*
 * IDENTIFICATION		procedure.c
 *
 * DESCRIPTION	
 *
 *	SEPIA COMPILER
 *
 * This file contains the main routines of Prolog procedure compilation.
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
#include	<fcntl.h>
#include	"sepia.h"
#include	"types.h"
#include        "embed.h"
#include 	"error.h"
#include	"mem.h"
#include	"dict.h"
#include	"emu_export.h"
#include	"property.h"
#include	"opcode.h"
#include	"compiler.h"
#include	"gencode.h"
#include	"io.h"
#include	"database.h"
#include	"module.h"
#include	"read.h"
#include	"os_support.h"


/*
 * DEFINES:
 */
#define Eof(pw)			(IsAtom(pw->tag) && pw->val.did == d_.eof)
#define Query(did)		((did == d_.rulech1 || did == d_.goalch))

/* Macro expansion flags */
#define FOUND_MACRO		1
#define GOAL_MACRO		2

#define Transformed(cd)	((cd)->macro & FOUND_MACRO)


/*
 * EXTERNAL VARIABLE DECLARATIONS:
 */

#ifdef OLD_DYNAMIC
extern vmcode	*init_dynamic(pri *pd);
#endif
extern int	compile_dynamic(proc_desc *procedure, dident module);
extern void	copy_procedure(proc_desc *procedure);

extern pword	*move_term(pword *pw, pword *dest);
extern void	remove_procedure(pri*);


 /*
  * EXTERNAL VARIABLE DEFINITIONS: 
  */

pword		*g_alldynamic_;

int		debug_pass3 = 0;	/* to debug the compiler */

dident
		d_module2,
		d_module3,
		d_module_interface,
		d_begin_module,
		d_begin_module2,
		d_create_module3_,
		d_call_susp_,
		d_erase_module_,
		d_eclipse_language_,
		d_insert_susp4_,
		d_insert_susp3_,
		d_add_attribute2_,
		d_add_attribute3_,
		d_get_attribute2_,
		d_get_attribute3_,
		d_get_attributes4_,
		d_replace_attribute2_,
		d_replace_attribute3_,
		d_matching_guard1,
		d_matching_guard,
		d_noskip,
		d_expand,
		d_noexpand,
		d_record_interface,
		d_silent_debug,
		d_tr_clause,
		d_untraced_call;

pword		woken_susp_;

int             ec_compile(stream_id nst, long int flags, dident module, type tmod);
vmcode		*compile_section(proc_desc *procedure);
vmcode         *compiler_error(int number, cl_desc *clds, proc_desc *procedure, vmcode *code);
int      	clause_error(int number, pword *clause, proc_desc *procedure);
dident   	get_did(proc_desc *procedure, pword *clause);
pword		*make_proc_id(dident wdid);

#ifdef PRINTAM
void		print_procedure(dident wdid, vmcode *code);
#endif
pword		*g_index_range;

 /*
  * STATIC VARIABLE DEFINITIONS: 
  */
static void	_file_query(pword *clause, proc_desc *procedure, struct compile_data *cd, dident module, type mtag);
static pword	_eof_pw;
static int	_args_error(int number, pword *arg1, pword *arg2, pword *arg3, proc_desc *procedure),
		_read_procedure(proc_desc *procedure, struct compile_data *cd, pword *g_base, uword dist);

#ifdef lint
#define calloc(a, b)	0
#endif

 /*
  * FUNCTION DEFINITIONS: 
  */


/*
 * Check whether the next len characters in stream nst match header[].
 * If yes, skip them, otherwise don't advance the stream pointer.
 */

static int
_skip_header_if_present(stream_id nst, char *header, int len)
{
    int i, res;

    if (IsTty(nst))
    {
	/* Don't expect headers on a tty. This fixes bug 473 (having
	 * to type CTRL-D 3 times to get out of the [user] prompt)  */
    	return PFAIL;
    }
    for (i=0; i<len; ++i)
    {
	res = ec_getch(nst);
	if (res < 0  ||  (char) res != header[i])
	{
	    /* header doesn't match: unget everything */
	    while(i-- >= 0)
	    	ec_ungetch(nst);
	    return PFAIL;
	}
    }
    return PSUCCEED;
}


/*
 * Source files may start with a UTF-8 Byte-Order-Mark, which we want to skip
 */

#define UTF8_BOM_LENGTH	3
static char utf8_bom[UTF8_BOM_LENGTH] = {'\357','\273','\277'};


/*
 * Current eco file version. This must correspond to
 * the number in dump_header/1 in the file io.pl.
 */
#define ECO_CURRENT_VERSION	0x16

#define MAGIC_LEN 3
static char eco_magic[MAGIC_LEN] = {'\354','\034','\051'};

static int
_read_eco_header(stream_id nst)
{
    int i, res;
    /*
     * temporarily limit buffering to the header size because
     * we may have to switch to SSCRAMBLE mode for the rest!
     */
    int bufsize = StreamSize(nst);
    StreamSize(nst) = MAGIC_LEN+1;

    /* check for eco header and skip if present */
    res = _skip_header_if_present(nst, eco_magic, MAGIC_LEN);
    StreamSize(nst) = bufsize;
    if (res != PSUCCEED) 
    	return PFAIL;

    /* next byte indicates the eco version */
    res = ec_getch(nst);
    if (res < 0)
    	return res;
    if (res != ECO_CURRENT_VERSION)
	return BAD_DUMP_VERSION;

    StreamMode(nst) |= SSCRAMBLE;
    StreamRand(nst) = 73540 ^ 0x9bc33c86;

    /* read the rest of the header */
    for(i=0; i<8; ++i)
    	res = ec_getch(nst);
    return res < 0 ? res : PSUCCEED;
}


/*
 * FUNCTION NAME:	ec_compile(file, flags, module, tmod)
 *
 * PARAMETERS:		file	- DID of the file name, full pathname
 *			flags	- compilation flags
 *			module	- home module of the procedure
 *			tmod	- tag of the module argument (access checking)
 *
 * DESCRIPTION:
 */
int ec_compile(stream_id nst, long int flags, dident module, type tmod)
{
    dident		file;
    reg_desc		r_desc[NREG_DFLT];
    int			h_pass[HEAD_PASSES];
    pword		arg1;
    pword		arg2;
    pword		arg3;

    proc_desc		procedure;
    struct compile_data	cd;
    long		t_start;
    double		t_exec;
    dident		wdid;	       /* compiled procedure's DID */
    pri			*procindex;     /* proc table entry */
    int			debug;
    pword		*saveTG = Gbl_Tg;	/* to pop the procedures */
    pword		**saveTT = Gbl_Tt;	/* to pop the trail */
    int			status;
    long		code_size = 0;
    vmcode		*code;
    int			err;
    uword		cid;
    dident		fid;
    pword		*save_depth;
    uint32		affected_flags, new_flags;
    long 		len;

    if (flags & FTERM) {
	cd.nst = 0;
	cd.term = (pword *) nst;
    } else {
	file = StreamName(nst);
	cd.nst = nst;
	cd.term = 0;
    }
    cd.flags = flags;
    cd.list = 0;
    cd.next_clause = 0;
    cd.next_offset = 0;
    cd.dump_version = 0;
    cd.macro = 0;
#ifdef DFID
    save_depth = DfidDepth;
#endif
    if (!(GlobalFlags & DBGCOMP))
	cd.flags |= FNODBG;
    if (GlobalFlags & GOALEXPAND)
	cd.flags |= FEXPAND;

    if (!(cd.flags & FNOSTAT))
    {
	/* Time statistics */
	t_start = user_time();
    }

    if (cd.nst) {
	/* find out whether we are reading a source file or an eco file */
	err = _read_eco_header(nst);
	switch (err)
	{
	case PSUCCEED:
	    cd.flags |= FDUMPED;
	    break;
	case PFAIL:
	    /* it is a source file, allow a leading UTF-8 BOM */
	    _skip_header_if_present(nst, utf8_bom, UTF8_BOM_LENGTH);
	    break;
	default:
	    arg1.tag.kernel = TDICT;
	    arg1.val.did = file;
	    arg2.tag.kernel = TNIL;
	    arg3.tag.kernel = TNIL;
	    (void) _args_error(err, &arg1, &arg2, &arg3, &procedure);
	    return PFAIL;
	}
    }

    /* Initialize the procedure descriptor */
    Buf_Create(procedure.codebuf, STAT_BUFFER_SIZE);
    procedure.gargs = (arg_desc *) hg_alloc(
	sizeof(arg_desc) * ((int)ARG_DESC_SIZE + 1));
    GargsSize(procedure.gargs) = ARG_DESC_SIZE;
    procedure.stack = (long *) hg_alloc(4 * sizeof(long) * (int)STACK_SIZE);
    procedure.stack_end = procedure.stack + 4 * STACK_SIZE;
    procedure.regs = r_desc;
    procedure.head_pass = h_pass;
    procedure.module = module;
    procedure.module_tag = tmod;
    procedure.cid = CompileId++;
    procedure.pflags = 0;

    for(;;)
    {
	procedure.pflags = 0;
	procedure.size = -PROC_PREFIX_SIZE;
	procedure.start = 0;
	cd.offset = cd.next_offset;
	Buf_Reinit(procedure.codebuf);
	procedure.clauseno = _read_procedure(
					&procedure,
					&cd,
					saveTG,
					((pword *) Gbl_Tt) - saveTG);
	if (procedure.clauseno <= 0)
	{
	    err = procedure.clauseno;
	    break;
	}
#ifdef PRINTAM
	if (cd.flags & FTERM)
#else
	if (cd.flags & (FSYSTEM | FTERM))
#endif
	{
	    procedure.lid = procedure.bid = 0;
	    procedure.fid = D_UNKNOWN;
	    if (cd.flags & FTERM) {
		Set_Procedure_Trail(&procedure);
		Set_Procedure_No_Warn(&procedure);
	    }
	}
	else
	{
	    if (file == d_.user || cd.flags & FDUMPED)
		procedure.lid = procedure.bid = 0;
	    else
	    {
		procedure.lid = cd.line;
		procedure.bid = cd.offset;
	    }
	    procedure.fid = file;
	    Set_Did_Stability(file, DICT_CODE_REF);
	}

	/* Handle the flags which may have changed in last query */
	debug = (cd.flags & FNODBG) ? 0 : GlobalFlags & DBGCOMP;
	if (cd.flags & FSILENT) {
	    Set_Procedure_Silent(&procedure);
	}
	if (cd.flags & FEXPAND) {
	    Set_Procedure_Expand(&procedure);
	}
	module = procedure.module;
	if (debug)
	    Set_Procedure_Debug(&procedure);
	if (cd.flags & FSYSTEM) {
	    Set_Procedure_System(&procedure);
	}

	/* get the descriptor, and check it is not an import one */
	wdid = procedure.did;

	procindex = procedure.proc =
	    local_procedure(procedure.did, module, procedure.module_tag, PRI_CREATE);
	if (!procindex)
	{
	    Get_Bip_Error(err);
	    (void) clause_error(err, make_proc_id(procedure.did),
				&procedure);
	    Set_Procedure_Error(&procedure);
	    continue;
	}

	affected_flags = EXTERN|TOOL|SYSTEM|DEBUG_DB
		| (cd.flags & FSKIPPED ? DEBUG_SK : 0);
	new_flags = (cd.flags & FSYSTEM ? SYSTEM : 0)
		| (cd.flags & FSKIPPED ? DEBUG_SK : 0)
		| (debug ? DEBUG_DB : 0);

	err = pri_compatible_flags(procindex, affected_flags|CODETYPE, new_flags|VMCODE);
	if (err != PSUCCEED)
	{
	    (void) clause_error(err, make_proc_id(procedure.did), &procedure);
	    Set_Procedure_Error(&procedure);
	    continue;
	}

	/* allocate new gargs if necessary */
	if (GargsOverflow(procedure.gargs, procedure.arity))
	{
	    hg_free((generic_ptr) procedure.gargs);
	    procedure.gargs = (arg_desc *) hg_alloc(
		sizeof(arg_desc) * ((int)(procedure.arity) + 11));
	    GargsSize(procedure.gargs) = procedure.arity + 10;
	}

#ifdef OLD_DYNAMIC
	/* dynamic procedures */
	if (g_alldynamic_->val.nint && !(cd.flags & FDUMPED))
	{
	    /* make a dynamic procedure out of it */
	    if (!(procindex->flags & CODE_DEFINED))
	    {
		a_mutex_lock(&ProcedureLock);
		remove_procedure(procindex);
		procindex->flags |= PROC_DYNAMIC | CODE_DEFINED;
		PriCode(procindex) = init_dynamic(procindex);
		/* add system and/or debug flag if necessary */
		if (cd.flags & FSYSTEM)
		    procindex->flags |= SYSTEM;
		if (cd.flags & FSKIPPED)
		    procindex->flags |= DEBUG_SK;
		if (debug)
		    procindex->flags |= DEBUG_DB;
		else
		    procindex->flags &= ~DEBUG_DB;
		a_mutex_unlock(&ProcedureLock);
	    }
	    else if (!DynamicProc(procindex))
	    {
		(void) clause_error(ALREADY_DEFINED,
				    make_proc_id(procedure.did), &procedure);
		Set_Procedure_Error(&procedure);
		continue;
	    }
	}
#endif

	/* check for consecutive clauses */
	code = PriCode(procindex);
	if (!DynamicProc(procindex) && code)
	{
	    cid = ProcCid(code);
	    fid = ProcFid(code);
	    if (cid == procedure.cid)
	    {
		if (clause_error(CONSECUTIVE, make_proc_id(wdid), &procedure)
			!= PSUCCEED)
		{
		    Set_Procedure_Error(&procedure);
		    continue;
		}
	    }
	    else if (fid != procedure.fid &&
		fid != D_UNKNOWN &&
		procedure.fid != D_UNKNOWN ||
		PriFlags(procindex) & EXTERN)
	    {
		arg2.tag = tdict;
		arg2.val.did = PriFlags(procindex) & EXTERN ? d_.external :
				fid;
		arg3.tag = tdict;
		arg3.val.did = procedure.fid;
		if (_args_error(MULTIFILE, make_proc_id(wdid), &arg2, &arg3,
			&procedure) != PSUCCEED)
		{
		    Set_Procedure_Error(&procedure);
		    continue;
		}
	    }
	}

	procedure.mode = PriMode(procindex);

	/* The procedure may have changed to dynamic by the previous events */
	if (DynamicProc(procindex))
	{
	    status = compile_dynamic(&procedure, module);
	    if (status != PSUCCEED)
	    {
		(void) clause_error(status, make_proc_id(wdid), &procedure);
		Set_Procedure_Error(&procedure);
	    }
	    procedure.clauses = 0;
	}
	if (procedure.clauses)
	{

	    procedure.mode = PriMode(procindex);
	    code = compile_section(&procedure);
            if (!code) continue; /* error has occured; do not generate code */
	    Buf_Flush(procedure.codebuf, code);
	    procedure.size += BufPos(procedure.codebuf, code);
	    copy_procedure(&procedure);

	    /* Now replace the procedure in the procedure table */
	    {
		pri_code_t pricode;
		pri_change_flags(procindex, affected_flags, new_flags);
		pricode.vmc = procedure.start;
		pri_define_code(procindex, VMCODE, pricode);
	    }
	}

#ifdef PRINTAM
	if (debug_pass3)
	    print_procedure(procedure.did, procedure.start);
#endif
	code_size += procedure.size;
    }					/* end of procedure loop */

    Buf_Destroy(procedure.codebuf);
    hg_free((generic_ptr) procedure.gargs);
    hg_free((generic_ptr) procedure.stack);

    if (!(cd.flags & FTERM) && !ProcedureFail(&procedure))
    {
	(void) clause_error(CODE_UNIT_LOADED, &_eof_pw, &procedure);
    }

    if (!(cd.flags & FNOSTAT) && !ProcedureFail(&procedure))
    {
	/* print time & space statistics */
	t_exec = 1.0 * (user_time() - t_start) / clock_hz;
	if (cd.flags & FTERM)
	    arg1.val.did = d_.term;
	else
	    arg1.val.did = file;
	arg1.tag.kernel = TDICT;
	arg2.tag.kernel = TINT;
	arg2.val.nint = code_size * sizeof(uword);
	Make_Double(&arg3, t_exec);
	(void) _args_error(COMPILED_FILE, &arg1, &arg2, &arg3, &procedure);
    }

    /*
     * Note by Joachim 2002-11-08:  the following two lines are not
     * ok.  popping the trail will cause any side-effect untrails to
     * be lost.  However, removing the two lines also doesn't work,
     * probably because some improper data is left on the global
     * stack, which confuses the gc.
     */
    Gbl_Tg = saveTG;
    Gbl_Tt = saveTT;	/* should be safe (?) */
#ifdef DFID
    DfidDepth = save_depth;
#endif
    if (ProcedureFail(&procedure))
	return PFAIL;
    else if (ProcedureError(&procedure) || ProcedureFatal(&procedure))
    {
	if (err == BAD_DUMP_VERSION)
	{
	    arg1.tag.kernel = TDICT;
	    arg1.val.did = file;
	    arg2.tag.kernel = TNIL;
	    arg3.tag.kernel = TNIL;
	    (void) _args_error(BAD_DUMP_VERSION, &arg1, &arg2, &arg3, &procedure);
	}
	return err;
    }
    else
	return PSUCCEED;
}

void
compiler_init(int flags)
{
    value	vval;

    if (flags & INIT_SHARED)
    {
	CompileId = 0;
    }

    d_call_susp_ = in_dict("call_suspension", 1);
    d_untraced_call = in_dict("untraced_call", 2);
    d_insert_susp4_ = in_dict("insert_suspension", 4);
    d_insert_susp3_ = in_dict("insert_suspension", 3);
    d_add_attribute2_ = in_dict("add_attribute", 2);
    d_add_attribute3_ = in_dict("add_attribute", 3);
    d_get_attribute2_ = in_dict("get_attribute", 2);
    d_get_attribute3_ = in_dict("get_attribute", 3);
    d_get_attributes4_ = in_dict("get_attributes", 4);
    d_replace_attribute2_ = in_dict("replace_attribute", 2);
    d_replace_attribute3_ = in_dict("replace_attribute", 3);
    d_module2 = in_dict("module", 2);
    d_module3 = in_dict("module", 3);
    d_module_interface = in_dict("module_interface", 1);
    d_begin_module = in_dict("begin_module", 1);
    d_begin_module2 = in_dict("begin_module", 2);
    d_record_interface = in_dict("record_interface", 2);
    d_matching_guard1 = in_dict("-?->", 1);
    d_matching_guard = in_dict("-?->", 2);
    d_noskip = in_dict("noskip", 0);
    d_expand = in_dict("expand", 0);
    d_noexpand = in_dict("noexpand", 0);
    d_silent_debug = in_dict("silent_debug", 0);
    d_tr_clause = in_dict("tr_clause", 3);
    d_erase_module_ = in_dict("erase_module", 1);
    d_create_module3_ = in_dict("create_module", 3);
    d_eclipse_language_ = in_dict("eclipse_language", 0);

    vval.nint = 0;
    g_alldynamic_ =
	init_kernel_var(flags, in_dict("alldynamic", 0), vval, tint);
    g_index_range =
	init_kernel_var(flags, in_dict("index_range", 0), vval, tint);

    /*
     * The eof atom
     */
    _eof_pw.tag.kernel = TDICT;
    _eof_pw.val.did = d_.eof;

    /*
     * A suspension which is marked as dead. Any suspension that occurs 
     * in a compiled clause is compiled into a TSUSP pointer to this one.
     */
    Init_Susp_Dead(&woken_susp_);
}

/* Clause and goal transformation.
 * Returns PFAIL if no transformation was possible
 * Returns PSUCCEED if transformation was done
 * Otherwise error code
 */
int
trafo_clause(value cval, type ctag, int macro, dident module, type mtag, pword **list)
{
    dident		wdid;
    pword		*tr;
    long		tt;
    int			res = PFAIL;
    int			clres = PFAIL;
    int			goalres = PFAIL;
    extern pword	*trafo_term(dident tr_did, int flags, dident mv, type mt, int *tr_flags);

    if (!(GlobalFlags & MACROEXP))
	return PFAIL;
    tt = ctag.kernel;
    if (IsStructure(ctag)) {
	wdid = cval.ptr->val.did;
	if (wdid == d_.rulech2) {
		pword		*p = cval.ptr + 1;

	    Dereference_(p);
	    if (IsStructure(p->tag))
		wdid = p->val.ptr->val.did;
	    else if (IsAtom(p->tag)) {
		wdid = p->val.did;
		tt = TDICT;
	    }
	    else if (IsNil(p->tag)) {
		wdid = d_.nil;
		tt = TNIL;
	    }
	    else if (IsList(p->tag)) {
		wdid = d_.list;
		tt = TLIST;
	    }
	    else
		return PFAIL;
	}
    }
    else if (IsAtom(ctag))
	wdid = cval.did;
    else if (IsNil(ctag))
	wdid = d_.nil;
    else if (IsList(ctag))
	wdid = d_.list;
    else
	return PFAIL;
    if (DidMacro(wdid) || DidMacro(wdid = TransfDid(tt)))
    {
	int	tr_flags;

	tr = trafo_term(wdid, TR_CLAUSE, module, mtag, &tr_flags);
	if (tr) {
	    TransfTermIn(tr)->val.all = cval.all;
	    TransfTermIn(tr)->tag.all = ctag.all;
	    res = do_trafo(tr);
	    if (res == PSUCCEED) {
		*list = TransfTermOut(tr);
		clres = PSUCCEED;
	    }
	    else if (res < 0) {
		return res;
	    }
	}
    }
    if (macro & GOAL_MACRO)
    {
	/* create a goal:
	 *	tr_clause(In,Out,CurModule)
	 */
	tr = Gbl_Tg;
	Gbl_Tg += 4;
	Check_Gc;
	tr->tag.all		= TDICT;
	tr->val.did		= d_tr_clause;
	if (res == PSUCCEED)
	    *(tr + 1) = **list;
	else {
	    (tr+1)->tag.kernel = ctag.kernel;
	    (tr+1)->val.ptr	= cval.ptr;
	}
	(tr+2)->tag.kernel	= TREF;
	(tr+2)->val.ptr	= (tr+2);
	(tr+3)->tag.all	= mtag.all;
	(tr+3)->val.did	= module;
	goalres = do_trafo(tr);
	if (goalres == PSUCCEED) {
	    *list = tr + 2;
	}
	else if (goalres < 0) {
	    return goalres;
	}
    }
/* The clause trafo always succeeds, so we cannot check a fixpoint
    res = clres;
    while (res == PSUCCEED)
	res = trafo_clause((*list)->val, (*list)->tag, macro, module, mtag, list);
    if (res < 0)
	return res;
    else
*/
	return clres == PSUCCEED ? clres : goalres;
}


/*
	This one is called only by the compiler
*/

int
ec_pread(struct compile_data *cd, int varnames_wanted, pword **clause, pword **varlist, dident module, type module_tag)
{
    int		status, has_macro;
    stream_id	nst = cd->nst;
    value	vm;

    cd->line = StreamLine(nst);

    vm.did = module;
    *clause = Gbl_Tg++;		/* result must be on global stack */
    status = ec_read_term(nst,
    		(varnames_wanted ? VARNAMES_PLEASE : 0),
		*clause, 0, &has_macro, vm, module_tag);

    if (has_macro)
	cd->macro = FOUND_MACRO|GOAL_MACRO;

    if (status == PEOF)
    {
	Make_Atom(*clause, d_.eof);
	status = PSUCCEED;
    }
    else if (status <= ERR_PARSE)
    {
	proc_desc	procedure;
	pword		culprit[3];

	if (status == ENDOFFILE)
	    cd->flags |= FEOF;
	culprit[0].tag.kernel = TCOMP;
	culprit[0].val.ptr = culprit + 1;
	culprit[1].tag.kernel = TDICT;
	culprit[1].val.did = d_.compile_stream;
	culprit[2].tag.kernel = TINT;
	culprit[2].val.nint = StreamNr(nst);
	procedure.module = module;
	procedure.module_tag = module_tag;
	(void) clause_error(status, culprit, &procedure);
    }
    return (status);
}


static dident
_read_clause(proc_desc *procedure, struct compile_data *cd)
{
    pword		*local_tg = Gbl_Tg;
    int			code;
    dident		wdid;
    int			varnames_wanted;
    pword		*clause;
    pword		*list;
    pword		*tr_clause;

    varnames_wanted = !(cd->flags & FNODBG) && (GlobalFlags & VARIABLE_NAMES);

    for (;;)
    {
	if (list = cd->list) {		/* rest from a previous macro */
	    clause = list++;
	    Dereference_(clause);
	    Dereference_(list);
	    if (IsList(list->tag))
		cd->list = list->val.ptr;
	    else
		cd->list = 0;
	    cd->macro |= FOUND_MACRO;
	}
	else
	{
	    if (cd->flags & FTERM)		/* compile_term/1, 2	*/
	    {
		list = cd->term;
		if (!list)
		    clause = &_eof_pw;
		else
		{
		    clause = list++;
		    Dereference_(list);
		    if (IsList(list->tag))
			cd->term = list->val.ptr;
		    else
			cd->term = 0;
		    cd->macro |= FOUND_MACRO;
		}
	    }
	    else if (cd->flags & FDUMPED)	/* read a dumped term */
	    {
		long n, nread;
		char *s;

		cd->macro = GOAL_MACRO;		/* we don't know */
		clause = &_eof_pw;
		s = ec_getstring(cd->nst, 4L, &nread);
		if (s  &&  nread == 4L)
		{
		    n = (unsigned char) *s++ << 24;
		    n |= (unsigned char) *s++ << 16;
		    n |= (unsigned char) *s++ << 8;
		    n |= (unsigned char) *s;
		    s = ec_getstring(cd->nst, n, &nread);
		    if (n > nread)
			s = 0;
		    if (s)
			clause = dbformat_to_term(s, procedure->module,
					procedure->module_tag);
		}
	    }
	    else	/* call the parser */
	    {
		pword	*varlist = NULL;

		if (cd->flags & FEOF) {
		    cd->flags &= ~FEOF;
		    clause = TG++;
		    Check_Gc;
		    clause->val.did = d_.eof;
		    clause->tag.kernel = TDICT;
		} else {
		    code = ec_pread(cd, varnames_wanted, &clause, &varlist,
				procedure->module, procedure->module_tag);
		    if (code != PSUCCEED)
			clause = 0;
		}
	    }
	    /* clause transformation */
	    if (!clause)
		continue;
	    Dereference_(clause);
	    if (!(cd->flags & FEXPAND))
		cd->macro &= ~GOAL_MACRO;
	    code = trafo_clause(clause->val, clause->tag, cd->macro,
			procedure->module, procedure->module_tag, &tr_clause);
	    if (code == PSUCCEED) {
		if (IsList(tr_clause->tag)) {
		    cd->list = tr_clause->val.ptr;
		    clause = 0;
		}
		else if (IsNil(tr_clause->tag)) {
		    clause = 0;
		} else {
		    clause = tr_clause;
		    Dereference_(clause);
		}
		cd->macro = FOUND_MACRO;
	    }
	    else if (code < 0)
	    {
		(void) clause_error(code, clause, procedure);
		clause = 0;
	    }
	}
	if (clause)
	{
	    if ((wdid = get_did(procedure, clause)) == D_UNKNOWN)
	    {
		(void) clause_error(ILLEGAL_HEAD, clause, procedure);
		Gbl_Tg = local_tg;
	    }
	    else
		break;
	}
    }
    cd->clause = clause;
    return wdid;
}

/*
 * Read a procedure. If the space occupied by the terms on the global stack
 * exceeds dist, erase everything and copy the next clause to the
 * previous top - g_base. A true Prolog list of clauses is created; it is
 * not possible to do this in Prolog, then it would be impossible to
 * just pop the clauses, GC would have to be used instead.
 */
static int
_read_procedure(proc_desc *procedure, struct compile_data *cd, pword *g_base, uword dist)
{
    pword		*clause;
    cl_desc		*descriptor;
    dident		wdid;
    dident		did1;
    int			clauseno = 0;
    int			line;
    long		offset;
    int			macro = 0;	/* was a clause transformed?	*/

    procedure->clauses = procedure->delay_clauses = 0;

    if (cd->next_clause == 0)	       /* starting from scratch */
    {
	wdid = _read_clause(procedure, cd);
	if (Transformed(cd))
	   macro = 1;
	clause = cd->clause;
	Dereference_(clause);
	line = cd->line;
    }
    else
    {
	clause = cd->next_clause;
	wdid = get_did(procedure, clause);
	macro = ProcedureMacro(procedure);
	line = cd->next_line;
    }
    if (Eof(clause))
	return clauseno;
    while (Query(wdid))
    {
	pword		*save_depth;
#ifdef DFID
	save_depth = DfidDepth;
#endif
	_file_query(clause, procedure, cd,
		procedure->module, procedure->module_tag);
#ifdef DFID
	DfidDepth = save_depth;
#endif
	if (!cd->list)
	    Gbl_Tg = g_base;       /* pop the query if not from a macro */
	if (cd->nst) {
	    cd->offset = StreamOffset(cd->nst) + StreamPtr(cd->nst) - StreamBuf(cd->nst);
	}
	wdid = _read_clause (procedure, cd);
	clause = cd->clause;
	Dereference_(clause);
	macro = Transformed(cd);
	if (Eof(clause))
	    return clauseno;
    }

    /* we have the first clause of the next procedure */
    /* CAUTION: be sure address computations don't overflow! */
    if (clause-g_base > dist/2 && !(cd->flags & FTERM) && !cd->list)
    {
	Gbl_Tg = move_term(clause, g_base);	/* copy clause down the stack */
	clause = g_base;
    }
    procedure->clauses = (cl_desc *) Gbl_Tg;

    while (1)
    {
	clauseno++;
	if (cd->nst) {
	    offset = StreamOffset(cd->nst) + StreamPtr(cd->nst) - StreamBuf(cd->nst);
	}
	/* push a clause descriptor on the global stack */
	/* make sure the stack stays pword-aligned! */
	descriptor = (cl_desc *) Gbl_Tg;
	Gbl_Tg += (sizeof(cl_desc) - 1)/sizeof(pword) + 1;
	Check_Gc;
	descriptor->desc_tag.kernel = TREF;
	descriptor->clause = clause;
	descriptor->tag.kernel = TTVV;		/* default for no index */
	descriptor->nextv = 0;
	descriptor->variables = 0;
	descriptor->gc_list = 0;
	descriptor->reg_arity = descriptor->cflags = 0;
	descriptor->nomatch = 0;
	descriptor->entry = descriptor->retry = 0;
	/* read the next clause */
	did1 = _read_clause(procedure, cd);
	clause = cd->clause;
	Dereference_(clause);
	if (Eof(clause) || did1 != wdid)
	    break;
	descriptor->next = (cl_desc *) Gbl_Tg;
	if (Transformed(cd))
	   macro = 1;
    }
    descriptor->next = 0;
    procedure->did = wdid;
    procedure->arity = DidArity(wdid);
    cd->next_clause = clause;	       /* save the following clause */
    cd->next_line = cd->line;
    cd->line = line;
    cd->next_offset = offset;
    if (Transformed(cd)) {
	Set_Procedure_Macro(procedure);
    }
    cd->macro = macro;
    return (procedure->clauseno = clauseno);
}

/*
 * Return the did of the clause head given a pointer to the clause.
 * If the clause is a delay clause, the procedure is marked as such.
 */
/*ARGSUSED*/
dident
get_did(proc_desc *procedure, pword *clause)
{
    type            tag;

    Dereference_(clause);
    if (IsRule(clause)) {
	clause = clause->val.ptr + 1;
	Dereference_(clause);
    }
    tag.all = clause->tag.all;
    if (IsStructure(tag))
	return clause->val.ptr->val.did;
    else if (IsAtom(tag))
	return clause->val.did;
    else if (IsList(tag))
	return d_.list;
    else if (IsNil(tag))
	return d_.nil;
    else
	return D_UNKNOWN;	/* bad */
}

#ifdef PRINTAM
void
print_procedure(dident wdid, vmcode *code)
{
	extern int	als(long int addr);

	p_fprintf(current_output_, "\n%s/", DidName(wdid));
	p_fprintf(current_output_, "%d:\n", DidArity(wdid));

	(void) als((long) code);
	ec_flush(current_output_);
}
#endif

static void
_file_query(pword *clause, proc_desc *procedure, struct compile_data *cd, dident module, type mtag)
{
    pword	*query = clause->val.ptr + 1;
    pword	*pw = query;
    pri		*proc;
    int		result;
    int		interface = 0;
    value	v, v1;
    type	t;
    dident	wd;

    if (IsAtom(query->tag))
	wd = query->val.did;
    else if (IsStructure(query->tag))
	wd = query->val.ptr->val.did;
    else if (IsList(query->tag))
	wd = d_.list;

    if (wd == d_.pragma)
    {
	proc = visible_procedure(wd, module, mtag, 0);
	if (!proc) {
	    Get_Bip_Error(result);	/* reset */
	    pw = query->val.ptr + 1;
	    Dereference_(pw);
	    if (IsAtom(pw->tag))
		wd = pw->val.did;
	    else if (IsStructure(pw->tag)) {
		pw = pw->val.ptr;
		wd = pw++->val.did;
	    } else
		wd = 0;

	    if (wd == d_.nodebug)
		cd->flags |= FNODBG;
	    else if (wd == d_.debug)
		cd->flags &= ~(FNODBG|FSILENT);
	    else if (wd == d_silent_debug) {
		cd->flags &= ~FNODBG;
		cd->flags |= FSILENT;
	    }
	    else if (wd == d_.system)
		cd->flags |= FSYSTEM;
	    else if (wd == d_.skip)
		cd->flags |= FSKIPPED;
	    else if (wd == d_noskip)
		cd->flags &= ~FSKIPPED;
	    else if (wd == d_expand)
		cd->flags |= FEXPAND;
	    else if (wd == d_noexpand)
		cd->flags &= ~FEXPAND;
	    else
		(void) clause_error(BAD_PRAGMA, query, procedure);
	    return;
	}
    }
    if (wd == d_.system)
    {
	/* the :- system directive, set the flags */
	cd->flags |= FSYSTEM|FSKIPPED|FNODBG|FEXPAND;
    }
    else if (wd == d_.system_debug)
    {
	cd->flags |= FSYSTEM;
	cd->flags &= ~(FNODBG|FSKIPPED);
    }
    else
    {
	pword	*next_pw, *exports_pw = 0, *language_pw = 0;
	int	new_module = 0;

	Dereference_(pw);
	if (IsStructure(pw->tag) &&
	    pw->val.ptr->val.did == d_.module1)
	{
	    pw = pw->val.ptr;
	    next_pw = 0;
	}
	else if (IsStructure(pw->tag) &&
	    pw->val.ptr->val.did == d_module_interface)
	{
	    pw = pw->val.ptr;
	    next_pw = 0;
	    interface = 1;
	}
	else if (IsStructure(pw->tag) &&
	    pw->val.ptr->val.did == d_module2)
	{
	    pw = pw->val.ptr;
	    next_pw = 0;
	    exports_pw = pw+2;
	    interface = 1;
	}
	else if (IsStructure(pw->tag) &&
	    pw->val.ptr->val.did == d_module3)
	{
	    pw = pw->val.ptr;
	    next_pw = 0;
	    exports_pw = pw+2;
	    language_pw = pw+3;
	    interface = 1;
	}
	else if (IsStructure(pw->tag) &&
	    pw->val.ptr->val.did == d_begin_module)
	{
	    pw = pw->val.ptr;
	    next_pw = 0;
	    interface = 2;
	}
	else if (IsStructure(pw->tag) &&
	    pw->val.ptr->val.did == d_.comma &&
	    IsStructure(pw->val.ptr[1].tag) &&
	    pw->val.ptr[1].val.ptr->val.did == d_.module1)
	{
	    next_pw = pw->val.ptr + 2;
	    pw = pw->val.ptr[1].val.ptr;
	}
	else
	{
	    next_pw = pw;
	    pw = 0;
	}
	if (pw)
	{
	    pw++;
	    Dereference_(pw);
	    /* construct the goal on the global stack because we must
	       add the module argument.					*/
	    v1.ptr = Gbl_Tg;
	    Push_Struct_Frame(interface == 2 ?
				d_begin_module2 : d_.module_directive)
	    v1.ptr[1].tag.kernel = pw->tag.kernel;
	    v1.ptr[1].val.did = pw->val.did;
	    v1.ptr[2].tag.kernel = mtag.kernel;
	    v1.ptr[2].val.did = module;
	    if (exports_pw)
		v1.ptr[3] = *exports_pw;
	    else
		v1.ptr[3].tag.kernel = TNIL;
	    if (language_pw)
		v1.ptr[4] = *language_pw;
	    else {
		v1.ptr[4].val.did = in_dict("eclipse_language",0);
		v1.ptr[4].tag.kernel = ModuleTag(d_.kernel_sepia);
	    }
	    v.did = d_.kernel_sepia;
	    t.kernel = ModuleTag(d_.kernel_sepia);
	    result = query_emulc(v1, tcomp, v, t);
	    if (result == PSUCCEED)
	    {
		procedure->module = pw->val.did;
		procedure->module_tag.kernel = ModuleTag(pw->val.did);
		new_module = 1;
	    }
	}
	if (next_pw)
	{
	    if (new_module) {
		v.did = procedure->module;
		mtag = procedure->module_tag;
	    }
	    else
		v.did = module;
	    if (cd->flags & FINTERFACE) {
		v1.ptr = Gbl_Tg;
		Push_Struct_Frame(d_record_interface)
		v1.ptr[1].tag.kernel = next_pw->tag.kernel;
		v1.ptr[1].val.did = next_pw->val.did;
		v1.ptr[2].tag.kernel = mtag.kernel;
		v1.ptr[2].val.did = v.did;
		v.did = d_.kernel_sepia;
		mtag.all = ModuleTag(d_.kernel_sepia);
		result = query_emulc(v1, tcomp, v, mtag);
	    } else
		result = query_emulc(next_pw->val, next_pw->tag,
			v, mtag);
	}
	if (result != PSUCCEED)
	    (void) clause_error(QUERY_FAILED, query, procedure);
	if (interface == 1)
	    cd->flags |= FINTERFACE;
	else if (interface == 2)
	    cd->flags &= ~FINTERFACE;
	/* if we had a global directive, set the flags */
	if (!(GlobalFlags & DBGCOMP))
	    cd->flags |= FNODBG;
	if (GlobalFlags & GOALEXPAND)
	    cd->flags |= FEXPAND;
    }
}

#if !defined(AS_EMU) && defined(PRINTAM)
int
print_ancestors(uword *e)
{
	vmcode		*cp;
	pri            *proc;

	while (e < (uword *) SP_ORIG && e > (uword *) SP)
	{
		cp = *((vmcode **)e + 1);
		p_fprintf(current_err_, "E = 0x%x, ", e);
		p_fprintf(current_err_, "CP = 0x%x", cp);
		if (SameCode(Fastcall, *(cp - 2)))
			p_fprintf(current_err_, ", Fastcall");
		else if (SameCode(Metacall, *(cp - 2)))
			p_fprintf(current_err_, ", Metacall");
		else if (SameCode(*(cp - 3), CallfP) ||
			SameCode(*(cp - 3), CallP))
		{
			proc = (pri *) *(cp - 2);
			p_fprintf(current_err_,
				  ", did = %s/%d",
				  DidName(proc->did),
				  DidArity(proc->did));
		}
		e = (uword *) * e;
		(void) ec_newline(current_err_);
	}
	ec_flush(current_err_);
	Succeed_;
}
#endif


/*
 * Raise an exception during the compilation of a clause. The appropriate
 * error handler is called. Then, if it returns, the code for the
 * bad clause is replaced by 'Failure' in order to maintain consistency
 * with the remaining code & indexing. Note that this procedure can
 * return from gen_clause(), but if used in deeper nested function calls,
 * you must assure that it exits all the parents up to gen_clause(),
 * or you don't mind if some junk code is generated afterwards.
 */
vmcode         *
compiler_error(int number, cl_desc *clds, proc_desc *procedure, vmcode *code)
{
    (void) clause_error(number, clds->clause, procedure);/* return code ? */
    if (procedure->clauseno == 1 && !procedure->delay_clauses &&
	!ProcedureDynamic(procedure)) {
	/* single clause - the procedure will be still undefined */
	code = procedure->start + LABEL_SIZE;
	procedure->size = -PROC_PREFIX_SIZE - LABEL_SIZE;
	Buf_Set_Read(procedure->codebuf, code);
	code = BufWriteR(procedure->codebuf, code);
	Buf_Alloc(procedure->codebuf, code, L_CALL);
	Store_2d(Undefined, procedure->proc);
    } else {
#if 1
	/* Joachim 15.11.00: reinstated the original code. The fix caused
	 * error 0868, and things seem to work ok even without it now.
	 */
	if (clds->entry) {
	    code = clds->entry;
	    procedure->size = procedure->saved_size;
	    Buf_Set_Read(procedure->codebuf, code);
	    code = BufWriteR(procedure->codebuf, code);
	    New_Label(clds->entry);
	}
	Buf_Alloc(procedure->codebuf, code, L_WORD+L_LAB);
	clds->indexed_entry = clds->entry;
	Store_d(Failure);
#else
    /*
     * Kish 98.7.16 -- this procedure does not generate correct code. Changed
     * the second case to return 0 in code which cause code generation for this
     * procedure to be skipped.
     */
      code = 0;
#endif
    }
    return code;
}

/*
 * Call the error handler for the specified error number. The argument
 * is the offending clause. Calls syserror(ErrNo, Clause, Module).
 */
int
clause_error(int number, pword *clause, proc_desc *procedure)
{
    value		v1, v2;
    type		t2;
    int			status;

    v1.ptr = Gbl_Tg;
    Push_Struct_Frame(d_.syserror)
    v1.ptr[1].tag.kernel = TINT;
    v1.ptr[1].val.nint = -number;
    v1.ptr[2].val.ptr = clause->val.ptr;
    v1.ptr[2].tag.all = clause->tag.all;
    v1.ptr[3].tag = procedure->module_tag;
    v1.ptr[3].val.did = procedure->module;
    v1.ptr[4] = v1.ptr[3];
    v2.did = d_.kernel_sepia;
    t2.kernel = ModuleTag(d_.kernel_sepia);
    status = query_emulc(v1, tcomp, v2, t2);
    if (status == PFAIL)
    {
	Set_Procedure_Fail(procedure);
    }
    return status;
}

pword *
make_proc_id(dident wdid)
{
	pword *p = Gbl_Tg;

	Gbl_Tg += 4;
	Check_Gc;
	p[0].tag.kernel = TCOMP;
	p[0].val.ptr = p + 1;
	p[1].tag.kernel = TDICT;
	p[1].val.did = d_.quotient;
	p[2].tag.kernel = TDICT;
	p[2].val.did = add_dict(wdid, 0);
	p[3].tag.kernel = TINT;
	p[3].val.nint = DidArity(wdid);
	return p;
}

/*
 * Raise an exception with culprit (arg1, arg2, arg3) to handle
 * events that need more information.
 */
static int
_args_error(int number, pword *arg1, pword *arg2, pword *arg3, proc_desc *procedure)
{
	pword *p = Gbl_Tg;

	Gbl_Tg += 7;
	Check_Gc;
	p[0].tag.kernel = TCOMP;
	p[0].val.ptr = p + 1;
	p[1].tag.kernel = TDICT;
	p[1].val.did = d_.comma;
	p[2] = *arg1;
	p[3].tag.kernel = TCOMP;
	p[3].val.ptr = p + 4;
	p[4].tag.kernel = TDICT;
	p[4].val.did = d_.comma;
	p[5] = *arg2;
	p[6] = *arg3;
	return clause_error(number, p, procedure);
}


/***********************************************************************
 * Load an .eco file
 *
 * An .eco file contains only directives
 * Only module directives are treated specially here
 ***********************************************************************/

int
ec_load_eco_from_stream(stream_id nst, int options, value vmod, type tmod)
{
    int res;
    pword *clause, *query;
    pword query_pw;
    value vquery;
    int encoded = 0;

    extern p_read3(value vs, type ts, value v, type t, value vm, type tm);

    /* we are expecting an eco-encoded file, but we allow text as well */
    res = _read_eco_header(nst);
    encoded = (res == PSUCCEED);

    for(;;)
    {
	int recreate_module = 0;
	pword exports_pw, language_pw;
	pword *module_pw = 0;

	if (encoded)			/* encoded dbformat */
	{
	    int n;
	    long nread;

	    char *s = ec_getstring(nst, 4L, &nread);
	    if (!(s))
		return nread;	/* error code */
	    if (nread < 4L)
	    	return (nread == 0L) ? PSUCCEED : UNEXPECTED_EOF;

	    n = (unsigned char) *s++ << 24;
	    n |= (unsigned char) *s++ << 16;
	    n |= (unsigned char) *s++ << 8;
	    n |= (unsigned char) *s;
	    s = ec_getstring(nst, n, &nread);
	    if (!(s))
		return nread;	/* error code */
	    if (nread < n)
		return UNEXPECTED_EOF;

	    clause = dbformat_to_term(s, vmod.did, tmod);
	    if (!clause)
		return NOT_DUMP_FILE;

	}
	else				/* text format, call the parser */
	{
	    value vs, vclause;
	    clause = TG++;
	    Check_Gc;
	    Make_Var(clause);
	    vs.nint = StreamNr(nst);
	    vclause.ptr = clause;
	    res = p_read3(vs, tint, vclause, vclause.ptr->tag, vmod, tmod);
	    if (res != PSUCCEED)
		return PEOF ? PSUCCEED : NOT_DUMP_FILE;
	}

	if (!Query(get_did((proc_desc*) 0, clause)))
	    return NOT_DUMP_FILE;

	query = clause->val.ptr + 1;
	if (IsStructure(query->tag))	/* look for special directives */
	{
	    pword *pw = query;
	    Dereference_(pw);
	    if (IsStructure(pw->tag) &&
		pw->val.ptr->val.did == d_.module1)
	    {
		recreate_module = 1;
		module_pw = &pw->val.ptr[1];
		Make_Nil(&exports_pw);
		Make_Atom(&language_pw, d_eclipse_language_);
	    }
	    if (IsStructure(pw->tag) &&
		pw->val.ptr->val.did == d_module_interface)
	    {
		recreate_module = 1;
		module_pw = &pw->val.ptr[1];
		Make_Nil(&exports_pw);
		Make_Atom(&language_pw, d_eclipse_language_);
	    }
	    else if (IsStructure(pw->tag) &&
		pw->val.ptr->val.did == d_module2)
	    {
		recreate_module = 1;
		module_pw = &pw->val.ptr[1];
		exports_pw = pw->val.ptr[2];
		Make_Atom(&language_pw, d_eclipse_language_);
	    }
	    else if (IsStructure(pw->tag) &&
		pw->val.ptr->val.did == d_module3)
	    {
		recreate_module = 1;
		module_pw = &pw->val.ptr[1];
		exports_pw = pw->val.ptr[2];
		language_pw = pw->val.ptr[3];
	    }
	    else if (IsStructure(pw->tag) &&
		pw->val.ptr->val.did == d_begin_module)
	    {
		module_pw = &pw->val.ptr[1];
		query = &query_pw;	/* don't execute anything */
		Make_Atom(query, d_.true0);
	    }
	}

	if (recreate_module)		/* build translated module query */
	{
	    pword *pw;
	    query = &query_pw;
	    Make_Struct(query, TG);
	    Push_Struct_Frame(d_.comma);
	    Make_Struct(&query->val.ptr[1], TG);
	    pw = TG;
	    Push_Struct_Frame(d_erase_module_);
	    pw[1] = *module_pw;
	    Make_Struct(&query->val.ptr[2], TG);
	    pw = TG;
	    Push_Struct_Frame(d_create_module3_);
	    pw[1] = *module_pw;
	    pw[2] = exports_pw;
	    pw[3] = language_pw;
	}
					/* execute the query/directive */
	res = query_emulc(query->val, query->tag, vmod, tmod);

	if (res != PSUCCEED)
	    return QUERY_FAILED;

	if (module_pw)			/* change to new context module */
	{
	    Dereference_(module_pw);
	    vmod.all = module_pw->val.all;
	    tmod.all = module_pw->tag.all;
	}
    }
    return PSUCCEED;
}

