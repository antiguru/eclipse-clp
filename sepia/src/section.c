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
 * VERSION	$Id: section.c,v 1.1 2006/09/23 01:56:18 snovello Exp $
 */

/*
 * IDENTIFICATION		section.c
 *
 * DESCRIPTION	
 *
 *	SEPIA COMPILER
 *
 * This file contains the routines to compile the indexing information
 * of a Prolog procedure.
 * The algorithms and the background can be found in
 *
 *	M.Meier: Analysis of Prolog Procedures for Indexing Purposes
 *		in Proc. FGCS'88 Tokyo
 * and
 *	M.Meier: Indexing in Sepia, IR-LP-13-05
 *
 *
 * CONTENTS:
 *
 * AUTHOR	VERSION	 DATE	REASON
 * Micha Meier	1.0		created the file
 * Micha Meier	2.2	20.7.89	updated for the new compiler
 * Micha Meier	2.3	23.1.90	removed distinction of sections and procedures
 */

 /*
  * INCLUDES:
  */
#include 	"config.h"
#include	"sepia.h"
#include	"types.h"
#include        "embed.h"
#include 	"error.h"
#include	"mem.h"
#include	"dict.h"
#include	"emu_export.h"
#include	"opcode.h"
#include	"compiler.h"
#include	"gencode.h"
#include	"io.h"
#include	"database.h"
#include	"debug.h"

/*
 * DEFINES:
 */
#define KEY_SIZE		8
#define	RADIX			(2 << KEY_SIZE - 1)
#define PASSES_NO		((8*SIZEOF_WORD - 1 + KEY_SIZE)/KEY_SIZE)
#define PAR_SIZE		/* 8 */	10

#define TSTAT			THANDLE
#define VarType(t)		((t) >= TTMM)	/* relies on value of VV&MM! */
#define VarMask(t)		(!((t) & ~(TagMask(TTVV) | TagMask(TTMM))))
#define CompTest(wd)		((wd) == d_.unify || (wd) == d_.identical || (wd) == d_.not_unify || (wd) == d_.not_identical)
#define ArithTest(wd)		((wd) == d_.inf || (wd) == d_.infq || (wd) == d_.sup || (wd) == d_.supq)

/* Note by Joachim: I have introduced the following macro to get the same
 * set of tags that were there originally, but I'm not at all sure what
 * this set really represents (I have guessed the name for the macro).
 * Maybe it should be defined as (IsAtomic(t) && !HashIndex(t))
 */
#define NoSwitchType(t)		( t == TSTRG || t == TBIG || t == TIVL || \
				  t == TRAT  || t == TDBL )

#define HashIndex(tag)		((tag) == TDICT || (tag) == TINT || (tag) == TCOMP)
#define BIN_SEARCH		1
#define RANGE_SEARCH		2

/* Which fraction of clauses must have guard tests to be taken into account */
#define SPEC_FACTOR		3
/* How many guards prevent further indexes */
#define GUARD_FACTOR		0.5
/* Maximum length of a seq. path, absolute and as fraction of total */
#define SEQ_FACTOR		2
#define MAX_SEQ_PATH_LENGTH	3
/* How many variable clauses invalidate an index */
#define VAR_FACTOR		0.75

#define MatchesTag(clds, mask)	(clds->desc_tag.kernel & (mask))
#define Next_Matching(cl, mask) \
		while (cl && ClauseSpec(cl) && !MatchesTag(cl, mask))\
		    cl = cl->nextv;
#define Next_Matching_Sum(cl, mask, types, next_field, entry_field) \
		for (;;) {\
		    if (!cl || !ClauseSpec(cl))\
			break;\
		    else if (MatchesTag(cl, mask)) {\
			types->matched &= cl->desc_tag.kernel & ~0xff;\
			break;}\
		    else {\
			types->not_matched |= cl->desc_tag.kernel & ~0xff;\
			cl = cl->next_field;}\
		}
#define Next_Matching_Val(procedure, cl, t, cval, types) \
	for (;;) {\
	    Next_Matching_Sum(cl, TagMask(t), types, nextv, indexed_entry)\
	    if (t == TINT && ProcedureMinMax(procedure) && (cl) && ClauseMinMax(cl)) {\
		if (ClauseMin(cl) && cval <= (cl)->extreme ||\
		    ClauseMax(cl) && cval > (cl)->extreme)\
		    break;\
		cl = cl->nextv;\
	    } else\
		break;\
	}

#define Next_Matching_Var(cl) \
	Next_Matching_Sum(cl, TagMask(TTVV) & matching_types, (procedure->types + TTVV), next, clause)
#define Next_Matching_VM(cl, mask, types) \
	Next_Matching_Sum(cl, mask, types, next, indexed_entry)
#define Next_Val(cl, cval, cont)	\
		for (;;) {\
		    if (!cl || cl->val.nint != cval) {\
			*cont = cl;\
			cl = 0;\
			break;\
		    } else if (cl->indexed_entry)\
			break;\
		    cl = cl->nextv;\
		}

#define For_Tags_In_Mask(mask, typ, i)	\
		for (i = ((mask) & ALL_TYPES) >> 8, (typ) = 0; i; i>>=1, typ++)\
		    if (i & 1)

#define INPUT_INDEX		0x1
#define Entry(c, t)		((t) == TTVV || (t) == TTMM && (c)->tag.kernel != TTMM ? (c)->entry : (c)->indexed_entry)

/* complex guard tests, the order is significant */
#define ALL			6
#define START			4
#define SOME			2
#define NONE			1	/* implies setting body */
#define SET_BODY		1

/*
 * EXTERNAL VARIABLE DECLARATIONS:
 */
extern vmcode	*gen_clause(cl_desc *clds, int clause_number, proc_desc *procedure, register vmcode *code);
extern vmcode	*compiler_error(int number, cl_desc *clds, proc_desc *procedure, vmcode *code);

/*
 * EXTERNAL VARIABLE DEFINITIONS: 
 */
extern pword	*g_index_range;

/*
 * STATIC VARIABLE DEFINITIONS: 
 */
static void	_sort(struct pile *list, cl_desc **top, cl_desc **botm),
		_set_head(cl_desc *clds),
		_var_blocks(cl_desc *clds, proc_desc *procedure),
		_reset_types(struct pile *types),
		_count_list(struct pile *list);
static int	_select_index(proc_desc *procedure, int from, int to),
		_test_goal(proc_desc *procedure, cl_desc *clds, pword *goal, int index, int top, int cm, int *outi, int *mask, int *umask, int topb),
		_test_guard(proc_desc *procedure, int start_index, int max_index),
		_better_index(int first, int second, proc_desc *procedure),
		_check_head(proc_desc *procedure, pword *head, pword *var, int *bound),
		_analyse_clauses(proc_desc *procedure, int switch_no);
static vmcode	*_indexed_paths(proc_desc *procedure, register vmcode *code, vmcode *switch_end),
		*_switch(proc_desc *procedure, int ntypes, int ctype, long int size, register vmcode *code),
		*_hashed_path(int ttype, proc_desc *procedure, register vmcode *code, vmcode *address, vmcode *default_code),
		*_seq_path(int ttype, cl_desc *clds, proc_desc *procedure, long int cval, register vmcode *code, vmcode *address, cl_desc **contcl),
		*_default_path(proc_desc *procedure, register vmcode *code, vmcode **address),
		*_index_next_arguments(proc_desc *procedure, int next_index, int max_index, register vmcode *code);

struct table_entry {
	uword		arg;
	vmcode		*code;
};


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

vmcode *
compile_section(proc_desc *procedure)
{
    cl_desc		*clds = procedure->clauses;
    cl_desc		*nextvarcl;
    struct pile		*types = procedure->types;
    register vmcode	*code;
    pword		**save_tt = TT;
    int			index = 0;
    int			next_index = 0;
    int			debug = ProcedureDebug(procedure);
    vmcode		dummy = 0;	/* to be able to point somewhere */
    vmcode		*label = &dummy;
    vmcode		*switch_end;
    vmcode		*last_switch;
    vmcode		*index_link;
    int			i;
    int			j;
    int			arity = procedure->arity;
    register int	tag;
    int			state = 1;
    int			start_index = 1;
    int			max_index = arity;
    int			matched_var_types = 0;
    int			matching_types;	/* which types can still match */
    int			selected_types;
    int			matches_varcl;
    int			index_mode;

    _set_head(procedure->clauses);
    procedure->var_path = 0;
    procedure->not_indexed = TagMask(TTVV);
    if (IsList(g_index_range->tag))
    {
	start_index = g_index_range->val.ptr[0].val.nint;
	max_index = g_index_range->val.ptr[1].val.nint;
	if (start_index < 1)
	    start_index = 1;
	if (max_index > arity)
	    max_index = arity;
    }
    if (procedure->clauseno > 1 && arity > 0 && procedure->clauses) {
	index = _test_guard(procedure, start_index, max_index);
	next_index = _select_index(procedure, start_index, max_index);
	if ((procedure->index = index) && _analyse_clauses(procedure, 0))
	{
	    if (next_index && next_index == index)
		next_index = _select_index(procedure, index + 1, max_index);
	}
	else
	{
	    if (index)
		for (clds = procedure->clauses; clds; clds = clds->next) {
		    Reset_Clause_Specs(clds);
		}
	    start_index = next_index + 1;
	    while (index = next_index)
	    {
		next_index = _select_index(procedure, start_index, max_index);
		if (next_index &&
		    _better_index(index, next_index, procedure) &&
		    (InputMode(Mode(next_index, procedure->mode)) ||
		    !InputMode(Mode(index, procedure->mode))))
		{
		    i = next_index;
		    next_index = index;
		    index = i;
		    start_index = index + 1;
		}
		else
		    start_index = next_index + 1;
		procedure->index = index;
		if (_analyse_clauses(procedure, 0))
		    break;
	    }
	}
    }

    if (!index)		/* no one found */
    {
	procedure->index = 0;
	_reset_types(types);
	index_mode = 0;
    } else
	index_mode = Mode(index, procedure->mode);
    if (InputMode(index_mode)) {
	matching_types = NONVAR_TYPES;
	nextvarcl = 0;
    } else {
	matching_types = OutputMode(index_mode) ? TagMask(TTVV) : ALL_TYPES;
	nextvarcl = procedure->clauses;
	Next_Matching_Var(nextvarcl);
    }

    code = BufWriteZ(procedure->codebuf);
    Buf_Alloc(procedure->codebuf, code, L_START);
    *code = 0;
    code = CodeStart(code);
    ProcLid(code) = DEFAULT_LINE;
    New_Label(procedure->start);

    /* branch if another index first */
    Buf_Alloc(procedure->codebuf, code, L_BRA+L_LAB);
    Store_d(Branch);
    Forward_Label(index_link);
    Label_(code - 1);

    if (next_index &&
	    procedure->spec_number >= GUARD_FACTOR * procedure->clauseno) {
	next_index = 0;
#ifdef TUNE_COMP
	p_fprintf(log_output_,
		"%s/%d: guards prevent further indexing (%d out of %d)\n",
		DidName(procedure->did),
		procedure->arity,
		procedure->spec_number,
		procedure->clauseno);
    }
    else if (next_index && procedure->spec_number) {
	p_fprintf(log_output_,
		"%s/%d: GUARD_FACTOR not reached (%d out of %d)\n",
		DidName(procedure->did),
		procedure->arity,
		procedure->spec_number,
		procedure->clauseno);
#endif
    }

    /* Now generate the SWITCH instruction */
    if (index)
    {
	code = _switch(procedure, procedure->ntypes, procedure->ctype,
		types[procedure->ctype].size, code);
	New_Label(switch_end);

	if (!next_index) {
	    Set_Procedure_1Index(procedure);
	    Set_Procedure_Det(procedure);
	}
	Buf_Alloc(procedure->codebuf, code, L_BRA+L_LAB);
	if (!nextvarcl) {
	    last_switch = code;
	    Store_d(Failure);
	}
	else {
	    New_Label(procedure->var_path);
	    Branch_(last_switch);
	    label = last_switch;
	    *last_switch = (vmcode) code;
	}
	types[TTVV].matched = matching_types;
	types[TTVV].not_matched = 0;
    }

    /* Generate the code for clauses */

    for (i = 1, j = 1, clds = procedure->clauses; clds; i++, clds = clds->next)
    {
	tag = TagType(clds->tag);
	if (ClauseSpec(clds))
	    selected_types = clds->desc_tag.kernel;
	else if (tag == TTVV)
	    selected_types = ALL_TYPES;
	else
	    selected_types = TagMask(tag) | TagMask(TTVV) | TagMask(TTMM);
	selected_types &= matching_types;
	if (!selected_types) {	/* a redundant clause */
#ifndef TUNE_COMP
	  /* we want to know if kernel is strange */
	  if (procedure->module == d_.kernel_sepia)
#endif
	    {
		p_fprintf(warning_output_,
		    "%s/%d: clause %d is redundant\n",
		    DidName(procedure->did),
		    procedure->arity,
		    i);
	    }
	    Set_Clause_Spec(clds);
	    clds->desc_tag.kernel = TCLDESC;
	    clds->indexed_entry = 0;
	    continue;
	}
	clds->desc_tag.kernel = selected_types;

	if (clds == nextvarcl) {
	    matches_varcl = 1;
	    nextvarcl = nextvarcl->next;
	    Next_Matching_Var(nextvarcl);
	} else
	    matches_varcl = 0;

	/* set determinacy state and the flag 'not first to execute' for the
	 * following clause */
	if (tag == TTVV) {
	    if (ClauseSpec(clds))
		matched_var_types |= clds->desc_tag.kernel;
	    else
		matched_var_types = ALL_TYPES;
	}
	else if (clds->nextv && clds->val.did == clds->nextv->val.did) {
	    Set_Clause_After_Retry(clds->nextv);
	}
	if (MatchesMask(matched_var_types, tag) ||
		(tag == TLIST || tag == TTMM) && ProcedureSOT(procedure))
	    Set_Clause_After_Retry(clds);

	if (index && !(selected_types & VAR_TYPES)) {
	    Set_Mode(index, procedure->mode, index_mode | NONVAR);
	    if (!(selected_types & (TagMask(TLIST) | TagMask(TCOMP)))) {
		Set_Mode(index, procedure->mode, GROUND);
	    }
	} else if (index && selected_types & TagMask(TTVV) &&
		!(selected_types & NONVAR_TYPES)) {
	    Set_Mode(index, procedure->mode, OUTPUT);
	} else {
	    Set_Mode(index, procedure->mode, index_mode);
	}
	if (/* selected_types & clds->nomatch || */
	    !ClauseSpec(clds) || ClauseTest(clds) ||
	    ClauseMinMax(clds) && !ProcedureMinMax(procedure) ||
	    (!ProcedureDet(procedure) ||
		next_index &&
		clds->desc_tag.kernel & VAR_TYPES &&
		clds->desc_tag.kernel & NONVAR_TYPES))
	{
	    Reset_Clause_Bskip(clds);
	}
	state = clause_pass2(clds, procedure);
	if (state < 0) {
	    code = compiler_error(state, clds, procedure, code);
            /*continue; does not work */
	    return 0;
	}

	/* check if this is the last clause that matches for selected args */
	if (!clds->next) {
	    Set_Clause_Det(clds)
	}
	else if (ClauseUnif(clds) && !(clds->nomatch & selected_types))
	{
	    Set_Clause_Det(clds)
	    Set_Procedure_Det(procedure)
	    if (next_index && InputMode(index_mode)) {
		next_index = 0;
		Set_Procedure_1Index(procedure);
	    }
	    if ((VarType(tag) && !ClauseMinMax(clds)) ||
		    tag == TNIL || tag == TLIST)
		matching_types &= ~selected_types;
	}
	else if ((!matches_varcl || !nextvarcl) &&
	    ( NoSwitchType(tag) || VarType(tag) ||
	    !clds->nextv || clds->val.did != clds->nextv->val.did) &&
	    procedure->catchall < clds)
	{
	    register int	a, t, m;

	    m = selected_types;
	    if (ClauseUnif(clds))
		m &= clds->nomatch;
	    For_Tags_In_Mask(m, t, a) {
		if (types[t].lastvar > clds || types[t].last > clds &&
		    !HashIndex(tag))
		    break;
	    }
	    if (!a) {
		Set_Clause_Det(clds)
		Set_Procedure_Det(procedure)
		if (next_index && (nextvarcl || InputMode(index_mode))) {
		    next_index = 0;
		    Set_Procedure_1Index(procedure);
#ifdef TUNE_COMP
		    p_fprintf(log_output_,
			    "%s/%d: further indexing prevented\n",
			    DidName(procedure->did),
			    procedure->arity);
#endif
		}
	    }
	}
	/* Even if the clause is not the last one for all selected types,
	   it might be one at least for some */
	if (clds->nomatch && ClauseUnif(clds))
	    matching_types &= ~(selected_types & ~clds->nomatch);
	else if (ClauseUnif(clds))
	    matching_types &= ~(selected_types & VAR_TYPES);
	if (ParallelProc(procedure->proc) && !ClauseDet(clds)) {
	    Set_Clause_After_Retry(clds);
	}

	/* make the variable path */
	Buf_Alloc(procedure->codebuf, code, L_TRY+L_LAB);
	if (matches_varcl)
	{
	    if (ClauseUnif(clds)) {
		if (!(clds->nomatch & selected_types & VAR_TYPES))
		    nextvarcl = 0;
		if (!ClauseDet(clds)) {
		    /* type is not enough to succeed */
		    if (tag != TLIST)
			types[TTVV].not_matched |= (selected_types &
			    ~TagMask(TNIL) & ~TagMask(TTVV) & ~TagMask(TTMM));
		    else if (clds->nomatch & TagMask(TLIST))
			Mark_Mask(types[TTVV].not_matched, TLIST)
		}
		else
		    types[TTVV].not_matched |= ~selected_types;
	    }
	    if (ClauseBskip(clds))
		types[TTVV].not_matched |= ~selected_types;
	    if (nextvarcl) {
		if (j == 1) {
		    Label_(label)
		    if (PriFlags(procedure->proc) & PROC_PARALLEL) {
#ifdef ORACLE
#define L_PAR 5
			Buf_Alloc(procedure->codebuf, code, L_PAR+L_TRY+L_LAB);
			Store_2d(Try_parallel,arity)
			Store_2d(Fail_clause,0)
			Store_d(Try_clause)
#endif
			procedure->size += PAR_SIZE;
		    }
		    /* Try_parallel needs no debug instruction */
		    Try_Me_Else(arity, label,
			PriFlags(procedure->proc) & PROC_PARALLEL ? 0 : debug);
		}
		else {
		    Label_(label)
		    Retry_Me_Else(label, debug);
		}
		j++;
	    }
	    else {
		Label_(label)
		if (j != 1) {
		    Trust_Me(label, debug);
		}
		matching_types &= ~TagMask(TTVV);
	    }
	    types[TTVV].matched &= selected_types;
	}
	else
	    types[TTVV].not_matched |= selected_types;

	procedure->saved_size = procedure->size;
	New_Label(clds->entry);			/* clause beginning */
	code = gen_clause(clds, i, procedure, code);
	Untrail_Variables(save_tt);
    }

    /* And finally generate the indexed paths */
    if (index)
	code = _indexed_paths(procedure, code, switch_end);

    if (next_index)
    {
	arg_desc	*args = procedure->gargs + 1;

	/* to avoid new indexing if we swapped index and next_index */
	Set_Mode(index, procedure->mode, OUTPUT);
	args[0].addr = index_link;
	args[1].addr = last_switch;
	i = (int) ((i - 1.0) / (j ? j : 1));
	args[1].state = types[TSTAT].size + procedure->spec_number +
		(i > 1 ? i : 0) + (j <= 1 ? 1 : 0);
    	code = _index_next_arguments(procedure, next_index, max_index, code);
    }

    if (procedure->clauses) {
	Buf_Alloc(procedure->codebuf, code, L_WORD);
	Store_d(Code_end);
    }
#ifdef ALLOC
    p_fprintf(current_output_,
	      "\n%s/%d: used %d\n",
	      DidName(procedure->did), arity, BufPos(procedure.codebuf, code));
#endif

    return code;
}

/*
 * FUNCTION NAME:	_set_head(clds)
 *
 * PARAMETERS:		clds - clause descriptor of the first clause
 *
 * DESCRIPTION:		Find the clause head and store its address into
 *			the clause descriptor. If it is an atom, this value
 *			has no meaning, since it is used only for indexing
 *			on the main argument (the first found). If there are
 *			some references, it does not work.
 */
static void
_set_head(cl_desc *clds)
{
    pword	*clause;
    pword	*h;
    pword	*b;

    while (clds)
    {
	clause = clds->clause;
	Dereference_(clause);
	b = 0;
	if (IsList(clause->tag))
	    h = clause->val.ptr - 1;
	else if (IsRule(clause))
	{
	    h = clause->val.ptr + 1;
	    b = clause->val.ptr + 2;
	    Dereference_(h);
	    Dereference_(b);
	    if (IsList(h->tag))
		h = h->val.ptr - 1;
	    else
		h = h->val.ptr;
	    if (MatchingBody(b)) {
		Set_Clause_Matching(clds);
	    }
	}
	else
	    h = clause->val.ptr;
	clds->head = h;
	clds->body = b;
	clds = clds->next;
    }
}

/*
 * Test whether the body contains some information which can be used
 * for the indexing.
 * It looks for the following tests as first subgoals:
 *	var/1, atom/1, atomic/1, number/1, integer/1, real/1, string/1,
 *	compound/1, nonvar/1, nonground/1, but they are used only
 * if there are enough such clauses in the procedure.
 * The predicates =/2 and ==/2 are used for indexing, too.
 */
static int
_test_guard(proc_desc *procedure, int start_index, int max_index)
{
    cl_desc		*clds;
    pword		*p;
    int			index = 0;
    int			var_types = 0;
    int			mask;
    int			um;
    int			i;
    int			specno = 0;
    int			unifno = 0;
    int			common_mask = ALL_TYPES;
    int			dif_mask = ALL_TYPES;
    int			res;

    if (start_index <= max_index)
	for (clds = procedure->clauses; clds; clds = clds->next)
	{
	    clds->tag.kernel = TTVV;
	    p = clds->clause->val.ptr;
	    Dereference_(p);
	    if (p->val.did != d_.rulech2)
		continue;	       /* no body */
	    um = mask = 0;
	    clds->body = (pword *) 0;
	    res = _test_goal(procedure, clds, p + 2, index, 1, 0, &i,
		&mask, &um, 1);

	    if (res == NONE || index && i != index ||
		i < start_index || i > max_index)
	    {
		Reset_Clause_Specs(clds);
		continue;
	    }
	    else if (!index)
		index = i;
	    if (res == ALL) {
		Set_Clause_Bskip(clds)
	    }
	    else if (res & SET_BODY) {
		Reset_Clause_Bskip(clds)
	    }
	    if (mask != ALL_TYPES)
		var_types |= mask;
	    common_mask &= mask;
	    dif_mask &= ~mask;
	    clds->desc_tag.kernel |= mask;
	    Set_Clause_Spec(clds);
	    if (ClauseBunif(clds))
		unifno++;
	    else
		specno++;
	    if (clds->tag.kernel == TTVV &&
		mask & ~(TagMask(TLIST) | TagMask(TNIL) | TagMask(TSTRG) |
			TagMask(TBIG) | TagMask(TDBL) | TagMask(TRAT) |
			TagMask(TIVL)) &&
		!ClauseMinMax(clds)) {
		Set_Procedure_TIndex(procedure)
	    }
	}
    if (unifno || specno &&
	(specno != procedure->clauseno ||
	(dif_mask | common_mask) != ALL_TYPES ||
	ProcedureMinMax(procedure)))
    {
	procedure->var_types = var_types;
	procedure->spec_number = specno;
	Set_Procedure_Det(procedure)
	return index;
    }
    else if (unifno || specno)
    {
	for (clds = procedure->clauses; clds; clds = clds->next) {
	    Reset_Clause_Specs(clds);
	}
#ifdef TUNE_COMP
	p_fprintf(log_output_,
		"%s/%d: SPEC_FACTOR not reached (%d out of %d)\n",
		DidName(procedure->did),
		procedure->arity,
		specno,
		procedure->clauseno);
#endif
    }
    procedure->var_types = 0;
    procedure->spec_number = 0;
    return 0;
}

/* Try to recognize type tests at the beginning of the clause body.
 * Goals ,/2, ;/2 and negations are transparent.
 */
static int
_test_goal(proc_desc *procedure, cl_desc *clds, pword *goal, int index, int top, int cm, int *outi, int *mask, int *umask, int topb)
{
    dident		wdid;
    pword		*var;
    pri			*proc;
    int			i;
    int			j;
    int			bound;
    int			m1 = 0;
    int			m2 = 0;
    int			um;
    int			neg = 0;
    int			res1;
    int			res2;

    Dereference_(goal);
    if (IsStructure(goal->tag)) {
	goal = goal->val.ptr;
	wdid = goal++->val.did;
    } else if (IsAtom(goal->tag))
	wdid = goal->val.did;
    else
	return NONE;
    proc = visible_procedure(wdid, procedure->module,
	    procedure->module_tag, 0);
    if (!proc) {
	Get_Bip_Error(i);	/* clear error */
	if (topb && wdid == d_matching_guard1) {
	    return _test_goal(procedure, clds, goal, index, top,
		cm, outi, mask, umask, 0);
	} else {
	    return NONE;
	}
    } else if (proc->module_ref != d_.kernel_sepia) {
	return NONE;
    }
    if (wdid == d_.comma || wdid == d_.cond) {
	res1 =
	    _test_goal(procedure, clds, goal++, index, top, cm, &i, &m1,
		umask, 0);
	if (res1 == NONE)
	    return NONE;
	else if (res1 == SOME) {
	    *outi = i;
	    *mask = m1;
	    return SOME|SET_BODY;
	}
	Dereference_(goal);
	res2 = _test_goal(procedure, clds, goal, i, top, m1, &j, &m2, umask, 0);
	if (res2 == NONE)
	    *mask = m1;
	else if (ClauseBound(clds))
	    *mask = (m1 & m2) ? m1 & (m2 | VAR_TYPES) : 0;
	else
	    *mask = m1 & m2;
	if (top && res2 & SET_BODY) {
	    clds->body = goal;
	    Set_Clause_Bskip(clds)
	}
	*outi = i;
	if (res1 == ALL && res2 == ALL)
	    return ALL;
	else if (res2 < START)
	    return res1 < ALL ? SOME|SET_BODY : SOME;
	else
	    return res1 < ALL ? START|SET_BODY : START;
    }
    else if (wdid == d_.semicolon) {
	res1 = _test_goal(procedure, clds, goal, index, 0, cm, &i, &m1, umask, 0);
	if (res1 == NONE)
	    return NONE;
	res2 = _test_goal(procedure, clds, goal + 1, i, 0, cm, &j, &m2, umask, 0);
	if (res2 == NONE)
	    return NONE;
	*mask = m1 | m2;
	*outi = i;
	if (res1 == ALL && res2 == ALL)
	   return ALL;
	else if (res1 >= START && res2 >= START)
	    return START | SET_BODY;
	else
	    return SOME | SET_BODY;
    }
    else if (Negation(wdid)) {
	um = 0;
	res1 = _test_goal(procedure, clds, goal, index, 0, cm, &i, &m1, &um, 0);
	if (res1 == NONE)
	    return NONE;
	else if (res1 < START)
	    um = ALL_TYPES;
	*mask = (~m1 | um) & ALL_TYPES;
	*umask |= um;
	clds->nomatch |= um;
	*outi = i;
	return res1 == ALL ? ALL : res1 | SET_BODY;
    }
    else if (CompTest(wdid))
    {
	if (IsRef(goal->tag))
	    var = goal++->val.ptr;
	else if (IsRef((++goal)->tag))
	    var= (goal--)->val.ptr;
	else
	    return NONE;
    }
    else if (top && MatchesMask(cm, TINT) &&
	ArithTest(wdid) &&
	!(cm & ~(TagMask(TINT) | TagMask(TBIG) | TagMask(TTVV) | TagMask(TTMM)))
	)
    {
	if (IsRef(goal->tag))
	    var = goal++->val.ptr;
	else if (IsRef((goal + 1)->tag)) {
	    var= (goal + 1)->val.ptr;
	    if (wdid == d_.inf)
		wdid = d_.sup;
	    else if (wdid == d_.infq)
		wdid = d_.supq;
	    else if (wdid == d_.sup)
		wdid = d_.inf;
	    else
		wdid = d_.infq;
	}
	else
	    return NONE;
	/* should dereference */
	if (!IsInteger(goal->tag) || goal->val.nint < 0)
	    return NONE;
    }
    else if (DidArity(wdid) == 1) {
	if (!IsRef(goal->tag))
	    return NONE;
	var = goal->val.ptr;		       /* the argument of the test */
    }
    else if (!top && wdid == d_.true0) {
	*outi = index;
	*mask = ALL_TYPES;
	return ALL;
    }
    else if (top && wdid == d_.cut) {
	Set_Clause_Neckcut(clds);
	return NONE;
    }
    else
	return NONE;
    Dereference_(var);
    i = _check_head(procedure, clds->head, var, &bound);
    if (!i || index && i != index)
	return NONE;
    res1 = ALL;
    if (wdid == d_.var) {
	Mark_Mask(*mask, TTVV);
	Mark_Mask(*mask, TTMM);
    }
    else if (wdid == d_.meta) {
	Mark_Mask(*mask, TTMM);
	Set_Procedure_VarTest(procedure)
    }
    else if (wdid == d_.free1) {
	Mark_Mask(*mask, TTVV);
	Set_Procedure_VarTest(procedure)
    }
    else if (wdid == d_.atom) {
	Mark_Mask(*mask, TDICT);
	Mark_Mask(*mask, TNIL);
    }
    else if (wdid == d_.integer) {
	Mark_Mask(*mask, TINT);
	Mark_Mask(*mask, TBIG);
    }
    else if (wdid == d_.bignum) {
	Mark_Mask(*mask, TBIG);
    }
    else if (wdid == d_.rational1) {
	Mark_Mask(*mask, TRAT);
    }
    else if (wdid == d_.atomic) {
	Mark_Mask(*mask, TSTRG);
	Mark_Mask(*mask, TBIG);
	Mark_Mask(*mask, TDBL);
	Mark_Mask(*mask, TRAT);
	Mark_Mask(*mask, TIVL);
	Mark_Mask(*mask, TSUSP);
	Mark_Mask(*mask, THANDLE);
	Mark_Mask(*mask, TNIL);
	Mark_Mask(*mask, TINT);
	Mark_Mask(*mask, TDICT);
	Mark_Mask(*mask, TDBREF);
    }
    else if (wdid == d_.nonvar) {
	Mark_Mask(*mask, TLIST);
	Mark_Mask(*mask, TCOMP);
	Mark_Mask(*mask, TSTRG);
	Mark_Mask(*mask, TBIG);
	Mark_Mask(*mask, TDBL);
	Mark_Mask(*mask, TRAT);
	Mark_Mask(*mask, TIVL);
	Mark_Mask(*mask, TSUSP);
	Mark_Mask(*mask, THANDLE);
	Mark_Mask(*mask, TNIL);
	Mark_Mask(*mask, TINT);
	Mark_Mask(*mask, TDICT);
	Mark_Mask(*mask, TDBREF);
    }
    else if (wdid == d_.number) {
	Mark_Mask(*mask, TINT);
	Mark_Mask(*mask, TBIG);
	Mark_Mask(*mask, TDBL);
	Mark_Mask(*mask, TRAT);
	Mark_Mask(*mask, TIVL);
    }
    else if (wdid == d_.real) {
	Mark_Mask(*mask, TDBL);
	Mark_Mask(*mask, TIVL);
    }
    else if (wdid == d_.float1) {
	Mark_Mask(*mask, TDBL);
    }
    else if (wdid == d_.double1) {
	Mark_Mask(*mask, TDBL);
    }
    else if (wdid == d_.breal) {
	Mark_Mask(*mask, TIVL);
    }
    else if (wdid == d_.compound) {
	Mark_Mask(*mask, TCOMP);
	Mark_Mask(*mask, TLIST);
    }
    else if (wdid == d_.is_list) {
	Mark_Mask(*mask, TNIL);
	Mark_Mask(*mask, TLIST);
	Mark_Mask(clds->nomatch, TLIST)
	Mark_Mask(*umask, TLIST)
	res1 = START | SET_BODY;
    }
    else if (wdid == d_.nonground) {
	Mark_Mask(*mask, TTVV);
	Mark_Mask(*mask, TTMM);
	Mark_Mask(*mask, TLIST);
	Mark_Mask(*mask, TCOMP);
	Mark_Mask(clds->nomatch, TLIST)
	Mark_Mask(clds->nomatch, TCOMP)
	Mark_Mask(*umask, TCOMP)
	Mark_Mask(*umask, TLIST)
	res1 = START | SET_BODY;
    }
    else if (wdid == d_.ground) {
	Mark_Mask(*mask, TSTRG);
	Mark_Mask(*mask, TBIG);
	Mark_Mask(*mask, TDBL);
	Mark_Mask(*mask, TRAT);
	Mark_Mask(*mask, TIVL);
	Mark_Mask(*mask, TSUSP);
	Mark_Mask(*mask, THANDLE);
	Mark_Mask(*mask, TNIL);
	Mark_Mask(*mask, TINT);
	Mark_Mask(*mask, TDICT);
	Mark_Mask(*mask, TDBREF);

	Mark_Mask(*mask, TLIST);
	Mark_Mask(*mask, TCOMP);
	Mark_Mask(clds->nomatch, TLIST)
	Mark_Mask(clds->nomatch, TCOMP)
	Mark_Mask(*umask, TCOMP)
	Mark_Mask(*umask, TLIST)
	res1 = START | SET_BODY;
    }
    else if (wdid == d_.string) {
	Mark_Mask(*mask, TSTRG);
    }
/* We can't do the following because is_suspension/1 also checks for non-dead
    else if (wdid == d_.is_suspension) {
	Mark_Mask(*mask, TSUSP);
    }
*/
    else if (wdid == d_.is_handle) {
	Mark_Mask(*mask, THANDLE);
    }
    else if (wdid == d_.unify || wdid == d_.identical)
    {
	Dereference_(goal);
	if (!IsRef(goal->tag) && !(cm && VarMask(cm)))
	{
	    if (!cm && ClauseMatching(clds))
		/* matching clauses usually shift assignments to the body,
		   and the matching argument should be the indexed one */
		return NONE;
	    Mark_Mask(*mask, TagType(goal->tag));
	    if (!IsNil(goal->tag)) {	/* nomatch for lists is yet unknown */
		Mark_Mask(*umask, TagType(goal->tag));
	    }
	    if (top) {
		clds->tag.all = goal->tag.all;
		if (IsCompound(goal->tag)) {
		    /* should be only if nonvoid */
		    Mark_Mask(clds->nomatch, TagType(goal->tag))
		}
		if (wdid == d_.unify) {
		    /* unless the type already matches, match a variable */
		    if (!MatchesMask(cm, TagType(goal->tag))) {
			Mark_Mask(*mask, TTVV)
			Mark_Mask(*mask, TTMM)
			Set_Clause_Bound(clds)
		    }
		}
		if (IsStructure(goal->tag))
		    goal = goal->val.ptr;
		if (IsInteger(goal->tag) || IsAtom(goal->tag)) {
		    if (ClauseBunif(clds) && clds->val.did != goal->val.did)
		    {
			*mask = 0;		/* fail */
			return ALL;
		    }
		    clds->val.all = goal->val.all;
		    if (!(*mask & VAR_TYPES || clds->nomatch))
			res1 = ALL;	/* we know it succeeds and is a noop */
		    else
			res1 = START | SET_BODY;
		}
		else {
		    clds->val.nint = goal->tag.kernel;	/* not indexable */
		    res1 = START | SET_BODY;
		}
		Set_Clause_Bunif(clds)
	    }
	    else {
		if (wdid == d_.unify) {
		    Mark_Mask(*mask, TTVV)
		    Mark_Mask(*mask, TTMM)
		}
		Mark_Mask(clds->nomatch, TagType(goal->tag));
		res1 = START | SET_BODY;
	    }
	} else
	    return NONE;
    }
    else if (wdid == d_.not_unify)
    {
	Dereference_(goal);
	if (IsNil(goal->tag))
	    *mask = ~(VAR_TYPES | TagMask(TNIL)) & ALL_TYPES;
	else if (!IsRef(goal->tag)) {
	    Mark_Mask(clds->nomatch, TagType(goal->tag));
	    *mask = ~VAR_TYPES;
	    neg = 1;
	    res1 = START;
	}
	else
	    return NONE;
    }
    else if (wdid == d_.not_identical)
    {
	Dereference_(goal);
	if (IsNil(goal->tag))
	    *mask = ~TagMask(TNIL) & ALL_TYPES;
	else
	    return NONE;
    }
    else if (wdid == d_.infq || wdid == d_.inf)
    {
	if (ProcedureMin(procedure))
	    return NONE;
	if (wdid == d_.infq)
	    procedure->min = goal->val.nint;
	else
	    procedure->min = goal->val.nint - 1;
	if (ProcedureMax(procedure) && procedure->min > procedure->max)
	    return NONE;
	clds->extreme = procedure->min;
	Set_Clause_Min(clds);
	Set_Procedure_Min(procedure);
	Mark_Mask(*mask, TINT);
    }
    else if (wdid == d_.supq || wdid == d_.sup)
    {
	if (ProcedureMax(procedure))
	    return NONE;
	if (wdid == d_.sup)
	    procedure->max = goal->val.nint;
	else
	    procedure->max = goal->val.nint - 1;
	if (ProcedureMin(procedure) && procedure->min > procedure->max)
	    return NONE;
	clds->extreme = procedure->max;
	Set_Clause_Max(clds);
	Set_Procedure_Max(procedure);
	Mark_Mask(*mask, TINT);
    }
    else
	return NONE;
    if (bound) {
	if (!neg) {
	    Mark_Mask(*mask, TTVV);
	    Mark_Mask(*mask, TTMM);
	    Mark_Mask(*umask, TTVV);
	    Mark_Mask(*umask, TTMM);
	    Mark_Mask(clds->nomatch, TTVV);
	    Mark_Mask(clds->nomatch, TTMM);
	}
	res1 = START | SET_BODY;
    }
    *outi = i;
    return res1;
}

/*
 * A very silly check that all arguments in the head are different variables
 * and search for an argument which is a given variable.
 * Output mode arguments are not taken into account, because they cannot
 * influence the result of the unification (actually they could when
 * e.g. ?- p(X, X) is matched with p(a, b) but then p(-, -) is not a correct
 * mode specification).
 * bound is 1 iff the indexed var can be bound during the unification of other
 * arguments
 * nonvars is 1 iff the head contains nonvariables in non-output positions
 *		or multiple occurrences of variables
 */
static int
_check_head(proc_desc *procedure, pword *head, pword *var, int *bound)
{
    m_item		mode_decl = procedure->mode;
    int			i, index;
    int			output_index = 0;
    register pword	*var1;

    index = *bound = 0;
    for (i = 1; i <= procedure->arity; i++)
    {
	if (!OutputMode(Mode(i, mode_decl)))
	{
	    if (IsCompound(head[i].tag)) {
		if (ec_occurs(var->val, var->tag, head[i].val, head[i].tag))
		    *bound = 1;
	    }
	    else if (IsRef(head[i].tag)) {
		var1 = head[i].val.ptr;
		Dereference_(var1);
		if (var == var1) {
		    if (index)
			*bound = 1;
		    else
			index = i;
		}
	    }
	}
	else if (IsRef(head[i].tag) && var == head[i].val.ptr)
	    output_index = i;
    }
    return index ? index : output_index;
}

/*
 * Select the right argument for indexing. If there is a special
 * test on some argument (currently only var(X), !) then select this
 * one, otherwise find an argument position with at least one
 * nonvariable.
 */
static int
_select_index(proc_desc *procedure, int from, int to)
{
    cl_desc		*clause;
    pword		*arg;
    m_item		mode_decl = procedure->mode;
    type		tag;
    int			i;

    for (i = from; i <= to; i++)   /* loop through arguments */
    {
	if (OutputMode(Mode(i, mode_decl)))	/* omit (-) mode */
	    continue;
	/* check if some of the arguments is a nonvariable */
	for (clause = procedure->clauses; clause; clause = clause->next)
	{
	    arg = clause->head + i;
	    Dereference_(arg);
	    tag.all = arg->tag.all;
	    if (IsRef(tag)) {
		if (IsMeta(tag) && ClauseMatching(clause))
		    return i;
	    } else if (IsTvv(tag)) {
		if (ClauseMatching(clause) && IsMeta(Tvv(arg->val)->header.tag))
		    return i;
	    } else
		return i;
	}
    }
    return 0;		/* none found			*/
}

/*
 * FUNCTION NAME:
 *
 * PARAMETERS:
 *
 * DESCRIPTION:		This function uses a simple heuristic to find out
 *			which one of the two arguments is better suitable
 *			for indexing. The argument whose value is sooner
 *			repeated  is rejected. Returns 0 if the first
 *			is chosen, else 1.
 */
static int
_better_index(int first, int second, proc_desc *procedure)
{
    cl_desc		*clds = procedure->clauses;
    pword		*head = clds->head;
    dident		val1;
    dident		val2;

    if (ClauseSpec(clds)) {
	if (IsRef(head[second].tag))
	    return 0;
    } else if (IsRef(head[first].tag)) {
	if (!IsRef(head[second].tag))
	    return 1;
    } else if (IsRef(head[second].tag))
	return 0;
    val1 = IsCompound(head[first].tag) ? head[first].val.ptr->val.did :
		head[first].val.did;
    val2 = IsCompound(head[second].tag) ? head[second].val.ptr->val.did :
		head[second].val.did;

    clds = clds->next;
    while (clds)
    {
	head = clds->head;
	if (IsRef(head[second].tag))
	    return 0;
	else if (!IsCompound(head[second].tag)) {
	    if (val2 == head[second].val.did)
		return 0;
	} else if (head[second].val.ptr->val.did == val2)
	    return 0;
	if (ClauseSpec(clds))
	    ;
	else if (IsRef(head[first].tag))
	    return 1;
	else if (!IsCompound(head[first].tag)) {
	    if (val1 == head[first].val.did)
		return 1;
	} else if (head[first].val.ptr->val.did == val1)
	    return 1;
	clds = clds->next;
    }
    return 0;
}

static int
_analyse_clauses(proc_desc *procedure,
	int switch_no)			/* 0 if the main index	*/
{
    cl_desc		*clds = procedure->clauses;
    struct pile		*types = procedure->types;
    value   		lastval[INDEX_TYPES];
    int			ctype = INDEX_TYPES;
    int			ntypes = 0;
    register int	i;
    register int	t;

    _var_blocks(clds, procedure);
    _reset_types(types);
    procedure->catchall = 0;
    /* make the lists of clauses with the same type */
    for (; clds; clds = clds->next)
    {
	if (ClauseSpec(clds) && !(clds->desc_tag.kernel & ALL_TYPES))
	    /* an ignored clause */
	    continue;
	t = TagType(clds->tag);
	if (types[t].first)
	{
	    types[t].last->nextv = clds;
	    types[t].last = clds;
	    if (lastval[t].did != clds->val.did)
		types[t].more_values = 1;
	}
	else		/* the first item */
	{
	    types[t].first = types[t].last = clds;
	    lastval[t] = clds->val;
	    types[t].more_values = 0;
	    if (t != TTVV)
	    {
		ctype = t;
		ntypes++;
	    }
	}
	if (ClauseMatching(clds) && t != TTVV) {
	    Set_Clause_Spec(clds);
	    Mark_Mask(clds->desc_tag.kernel, t);
	}
	if (NoSwitchType(t)) {
	    Mark_Mask(clds->nomatch, t);
	}
	if (ClauseSpec(clds)) {
	    register int	j;

	    For_Tags_In_Mask(clds->desc_tag.kernel, i, j) {
		if (i)
		    types[i].lastvar = clds;
	    }
	}
	else if (t == TTVV) {
	    types[TSTAT].length++;
	    procedure->catchall = clds;
	}
	clds->nextv = 0;
    }

    /* []'s might have different values */
    types[TNIL].more_values = 0;
    /* treat list and nil like one type */
    if (ntypes == 2 && types[TLIST].first && types[TNIL].first)
    {
	ntypes = 1;
	ctype = TLIST;
    }
    if (ntypes == 0 &&
	(procedure->var_types & ~(TagMask(TTVV) | TagMask(TTMM))) ==
	    TagMask(TINT) &&
	ProcedureMinMax(procedure))
    {
	ntypes = 1;
	ctype = TINT;
    }
    else if (ntypes == 1 && procedure->var_types) {
	/* even if there is only one explicit type, we might be better
	   off with a general switch, if there are enough implicit types */

	if (procedure->var_types & (TagMask(TSTRG) | TagMask(TINT) |
		TagMask(TDICT)) &&
	    procedure->var_types & TagMask(TTVV) &&
	    procedure->first_trailing &&
	    (!ClauseSpec(procedure->first_trailing) ||
		types[TCOMP].first ||
		procedure->var_types & TagMask(TCOMP)) &&
	    procedure->var_types & NONVAR_TYPES & ~TagMask(ctype))
	    ntypes = 2;
    }

    /* Now we have the lists of nonvariable clauses of the same type and a
     * flag saying whether there are some different values there. For
     * indexable items, we sort the lists. */

    if
	(
	 types[TDICT].more_values
	 ||
	 types[TINT].more_values
	 ||
	 types[TCOMP].more_values
	)
    {
	cl_desc **top, **botm;

	top = (cl_desc **) hg_alloc_size(RADIX * sizeof(cl_desc *));
	botm = (cl_desc **) hg_alloc_size(RADIX * sizeof(cl_desc *));
	for (i = 0; i < RADIX; i++)	/* must be initialised */
	    top[i] = 0;

	if (types[TDICT].more_values)
	    _sort(&types[TDICT], top, botm);
	if (types[TINT].more_values)
	    _sort(&types[TINT], top, botm);
	if (types[TCOMP].more_values)
	    _sort(&types[TCOMP], top, botm);
	hg_free_size((generic_ptr) top, RADIX * sizeof(cl_desc *));
	hg_free_size((generic_ptr) botm, RADIX * sizeof(cl_desc *));
    }
    /* Count the lists */
    for (i = 0; i < INDEX_TYPES; i++) {
	if (types[i].first && i != TSTAT && i != TTVV)
	{
	    _count_list(&types[i]);
	    types[TSTAT].size += types[i].size;
	}
    }

    /* if not enough nonvariable clauses, don't index on it */
    if (types[TSTAT].length > VAR_FACTOR * procedure->clauseno ||
	    (switch_no && types[TSTAT].size + procedure->spec_number <= 1 &&
	    types[TSTAT].length == 0))
	return 0;

    if (ProcedureMinMax(procedure) && !ProcedureMax(procedure)) {
	if (types[TINT].last)
	    procedure->max = types[TINT].last->val.nint;
	else if (procedure->min < MAX_S_WORD)
	    procedure->max = procedure->min + 1;
	else
	    procedure->max = procedure->min;
    }
    procedure->ntypes = ntypes;
    procedure->ctype = ctype;
    return 1;
}

/*
 * Return the type of the variable blocks and moves the indexed argument
 * into the clause descriptor.
 */
static void
_var_blocks(cl_desc *clds, proc_desc *procedure)
{
    int			index = procedure->index;
    int			var = 1;
    register type	tag;
    pword		*arg;
    long		min = procedure->min;
    long		max = procedure->max;

    procedure->first_trailing = 0;
    for (; clds; clds = clds->next)
    {
	arg = clds->head + index;
	Dereference_(arg);
	tag.all = arg->tag.all;
	if (IsRef(tag) || IsTvv(tag))
	{
	    if (ClauseBunif(clds))	/* already set */
		var = 0;
	    else {
		if (ClauseMatching(clds) &&
			(IsMeta(tag) ||
			IsTvv(tag) && IsMeta(Tvv(arg->val)->header.tag)))
		{
		    clds->tag.all = TTMM;
		    clds->val.all = TTMM;
		    Set_Procedure_VarTest(procedure)
		}
		else
		{
		    clds->tag.all = TTVV;
		    clds->val.all = arg->val.all;
		}
		if (!(var++))
		    procedure->first_trailing = clds;
	    }
	}
	else
	{
	    var = 0;
	    if (IsStructure(tag))
		clds->val.did = arg->val.ptr->val.did;
	    else if (IsInteger(tag)) {
		clds->val.all = arg->val.all;
		if (clds->val.nint <= min && ProcedureMin(procedure)) {
		    Reset_Procedure_MinMax(procedure)
		}
		else if (clds->val.nint > max && ProcedureMax(procedure)) {
		    Reset_Procedure_MinMax(procedure)
		}
	    }
	    else if (IsAtom(tag))
		clds->val.all = arg->val.all;
	    else
		clds->val.nint = tag.kernel;	/* not indexable */
	    clds->tag.all = Tag(tag.all);
	}
    }
    if (!var)
	procedure->first_trailing = 0;
}

static void
_reset_types(struct pile *types)
{
    register int	i;

    for (i = 0; i < INDEX_TYPES; i++)
    {
	types[i].last = types[i].first = types[i].lastvar = 0;
	types[i].more_values = types[i].matched = types[i].not_matched = 0;
	types[i].size = types[i].length = 0;
    }
}

static void
_sort(struct pile *list,
	cl_desc **top,		/* array must be zeroed and is returned zeroed! */
	cl_desc **botm)
{
    cl_desc *start = list->first;
    register cl_desc *ptr;
    register cl_desc **tail;
    long  key;
    int     pass;
    int     i;

    for (pass = 0; pass < PASSES_NO; pass++)
    {
	for (ptr = start; ptr; ptr = ptr->nextv)
	{
	    key = (ptr->val.nint >> (pass * KEY_SIZE)) & (RADIX - 1);

	    if (top[key])
	    {
		top[key]->nextv = ptr;
		top[key] = ptr;
	    }
	    else		/* first item in the queue */
		top[key] = botm[key] = ptr;
	}

	tail = &start;			/* concat the sublists */
	/* the last pass starts in the middle and wraps around,
	 * in order to get a signed comparison
	 */
	for(i = (pass < PASSES_NO-1) ? 0 : RADIX/2;;)
	{
	    if (top[i])
	    {
		*tail = botm[i];
		tail = &top[i]->nextv;
		top[i] = 0;
	    }
	    ++i;
	    if (pass < PASSES_NO-1)
	    {
		if (i == RADIX)
		    break;
	    }
	    else	/* last pass */
	    {
		if (i == RADIX/2)
		    break;
		else if (i == RADIX)	/* wrap */
		    i = 0;
	    }
	}
	*tail = 0;
    }
    list->first = start;
}


/*
 * Get the number of different items in the list (size) and the number
 * of all items in it (length).
 */
static void
_count_list(struct pile *list)
{
    cl_desc *ptr;
    int     size;
    int     length;
    dident  val;

    ptr = list->first->nextv;
    val = list->first->val.did;
    for (length = size = 1; ptr; ptr = ptr->nextv, length++)
	if (val != ptr->val.did)
	{
	    size++;
	    val = ptr->val.did;
	}
    list->length = length;
    list->size = size;
}

static vmcode *
_indexed_paths(proc_desc *procedure, register vmcode *code, vmcode *switch_end)
{
    struct pile		*types = procedure->types;
    cl_desc		*clds;			/* dummy */
    vmcode		*default_code;
    vmcode		*default_int_code;
    vmcode		*switch_code;
    int			ctype;
    int			i;

    code = _default_path(procedure,
			 code,
			 &default_code);
    if (ProcedureMinMax(procedure))
    {
	if (ProcedureMin(procedure) && ProcedureMax(procedure) &&
	       procedure->min == procedure->max)
	   default_int_code = FailCode;
	else
	{
	    code = _seq_path(TINT, (cl_desc *) 0,
			     procedure,
			     procedure->max,
			     code,
			     (vmcode *) &default_int_code,
			     &clds);
	}
    }
    else
	default_int_code = default_code;
    if (procedure->ntypes == 1 && !ProcedureSOT(procedure))
    {
	switch (ctype = procedure->ctype)
	{
	case TLIST:
	    code = _seq_path(TLIST, types[TLIST].first,
			     procedure,
			     (long) TLIST,
			     code,
			     switch_end - 3,
			     &clds);
	    if (!types[TNIL].first)	/* only list */
	    {
		Add_Label(switch_end - 2, default_code)
		break;
	    }
	    /* else continue to */
	case TNIL:
	    code = _seq_path(TNIL, types[TNIL].first,
			     procedure,
			     (long) TNIL,
			     code,
			     switch_end - 2,
			     &clds);
	    if (ctype != TLIST)	{	/* only nil */
		Add_Label(switch_end - 3, default_code)
	    }
	    break;

	case TCOMP:
	case TINT:
	case TDICT:
	    code = _hashed_path(ctype,
				procedure,
				code,
				switch_end - 2,
				default_int_code);
	    break;
	}
	Add_Label(switch_end - 1, default_code)
	if (ProcedureMinMax(procedure)) {
	    Add_Label(switch_end - 2, default_int_code)
	}
    }
    else if (ProcedureSOT(procedure))
	for (i = 0; i < NTYPES; i++)
	{
	    if (types[i].first &&
		(types[i].length > 1 || procedure->types[TTVV].first) &&
		HashIndex(i) ||
		i == TINT && ProcedureMinMax(procedure))
	    {	/* value-indexable items */
		vmcode		*default_value_code;

		Buf_Alloc(procedure->codebuf, code, L_LAB);
		Label_(switch_end - NTYPES + i);
		code = switch_code =
		    _switch(procedure, 1, i, types[i].size, code);
		if (i == TINT && ProcedureMinMax(procedure))
		{
		   *(switch_code - 1) = (vmcode) FailCode;
		   Add_Label(switch_code - 2, default_int_code)
		   default_value_code = default_int_code;
		}
		else if (procedure->var_types & TagMask(i))
		{
		    code = _seq_path(i, (cl_desc *) 0,
				     procedure,
				     procedure->max,
				     code,
				     switch_code - 1,
				     &clds);
		    default_value_code = (vmcode *) *(switch_code - 1);
		}
		else {
		    Add_Label(switch_code - 1, default_code)
		    default_value_code = default_code;
		}
		code = _hashed_path(i,
				    procedure,
				    code,
				    switch_code - 2,
				    default_value_code);
	    }
	    else if ((types[i].first || procedure->var_types & TagMask(i))
		&& i != TTVV)
	    {	/* type-indexable items */

		if (procedure->var_path && types[TTVV].matched & TagMask(i) &&
			!(types[TTVV].not_matched & TagMask(i)) &&
			    (!types[i].first || /* don't miss indexed_entry */
			    i != TNIL && i != TLIST && i != TTMM))
		{
		    Add_Label(switch_end - NTYPES + i, procedure->var_path)
		    procedure->not_indexed |= TagMask(i);
		}
		else
		{
		    int		j = i;

		    if (!types[i].first)
			for (j = 0; j < i; j++)
			    if (types[j].matched & TagMask(i) &&
				    !(types[j].not_matched & TagMask(i)))
				break;
		    if (j < i) {
			Copy_Label(switch_end - NTYPES + i,
				switch_end - NTYPES + j)
			types[i].matched = 0;
		    }
		    else {
			clds = types[i].first;
			if (HashIndex(i) && clds) /* single-value */
			    clds->indexed_entry = clds->entry;
			code = _seq_path(i, clds,
					 procedure,
					 clds ? clds->val.nint : 0,
					 code,
					 switch_end - NTYPES + i,
					 &clds);
		    }
		}
	    }
	    else if (i == TTMM && !(types[TTVV].not_matched & TagMask(TTMM))) {
		/* share the var path */
		Add_Label(switch_end - NTYPES + TTMM, switch_end)
		if (procedure->var_path)
		    procedure->not_indexed |= TagMask(TTMM);
	    }
	    else {
		Add_Label(switch_end - NTYPES + i, default_code)
		types[i].matched = 0;
	    }
	}
    return code;
}

static vmcode *
_switch(proc_desc *procedure,
	int ntypes,
	int ctype,
	long int size,		/* number of different values */
	register vmcode *code)
{
    int		index = procedure->index;

    if (ntypes == 1 &&
	(procedure->spec_number == 0 ||
	procedure->spec_number == 1 && VarMask(procedure->var_types) ||
	ProcedureSOT(procedure) ||
	ctype == TINT && ProcedureMinMax(procedure) &&
	(procedure->var_types & ~VAR_TYPES) == TagMask(TINT) &&
	!ProcedureVarTest(procedure)
	))
    {
	Buf_Alloc(procedure->codebuf, code, L_SWITCH);
	switch (ctype)
	{
	case TLIST:
	    List_Switch(index);
	    return code;

	case TNIL:
	    List_Switch(index);
	    *(code - 3) = (vmcode) FailCode;
	    return code;

	case TCOMP:
	    Functor_Switch(index)
	    return code;

	case TINT:
	    if (ProcedureMinMax(procedure)) {
		Store_2d(Integer_range_switchAM, Address(index))
		code += 4;
	    }
	    else
	    {
		Integer_Switch(index);
	    }
	    return code;

	case TDICT:
	    Atom_Switch(index);
	    return code;
	}
    }

    Buf_Alloc(procedure->codebuf, code, L_SOT);
    Switch_On_Type(index);
    Set_Procedure_SOT(procedure);
    return code;
}

/* Make a path which is used as default in the switch instructions.
 * If there is a SOT, it takes only variables without guard tests,
 * otherwise it takes all variable clauses.
 */
static vmcode *
_default_path(proc_desc *procedure,
	register vmcode *code,
	vmcode **address)	/* the address of a C code variable */
{
    int    		debug = ProcedureDebug(procedure);
    cl_desc		*clds;
    cl_desc		*nextcl;
    int			mask;

    clds = procedure->types[TTVV].first;
    mask = ((ProcedureSOT(procedure) && procedure->var_types) ||
	    ProcedureMinMax(procedure)) ?
		NONVAR_TYPES & ~procedure->var_types :
		NONVAR_TYPES;
    Next_Matching(clds, mask);
    if (clds) {
	nextcl = clds->nextv;
	Next_Matching(nextcl, mask);
    }
    Buf_Alloc(procedure->codebuf, code, L_TRY);
    if (!clds)					/* no variable clauses */
    {
	*address = FailCode;
	return code;
    }
    else if (!nextcl || ClauseDet(clds))	/* only one matching */
    {
	*address = clds->entry;
	/* if not the first, we might need the Trust */
	if (clds != procedure->clauses)
	{
	    clds->retry = 0;
	}
	return code;
    }

    New_Label(*address)
    if (PriFlags(procedure->proc) & PROC_PARALLEL) {
#ifdef ORACLE
	Buf_Alloc(procedure->codebuf, code, L_PAR+L_TRY);
	Store_2d(Try_parallel,procedure->arity)
	Store_2d(Fail_clause,0)
	Store_d(Try_clause)
#endif
	procedure->size += PAR_SIZE;
    }
    Try_(procedure->arity, clds->entry,
		PriFlags(procedure->proc) & PROC_PARALLEL ? 0 : debug);
    clds->retry = 0;
    for (;;)
    {
	clds = nextcl;
	nextcl = nextcl->nextv;
	Next_Matching(nextcl, mask);
	if (!nextcl || ClauseDet(clds))
	    break;
	Buf_Alloc(procedure->codebuf, code, L_TRY);
	New_Label(clds->retry)
	Retry_(clds->entry, debug);
    }
    Buf_Alloc(procedure->codebuf, code, L_TRY);
    New_Label(clds->retry)
    Trust_(clds->entry, debug);
    return code;
}

/* Generate a path through nonvariable and variable clauses */
static vmcode *
_seq_path(int ttype,
	cl_desc *clds,
	proc_desc *procedure,
	long int cval,
	register vmcode *code,
	vmcode *address,	/* points to an instruction's argument */
	cl_desc **contcl)
{
    int    		arity = procedure->arity;
    int    		debug = ProcedureDebug(procedure);
    struct pile		*types = procedure->types + ttype;
    cl_desc		*varcl = procedure->types[TTVV].first;
    cl_desc		*nextcl;
    cl_desc		*nextvarcl;
    cl_desc		*cl;
    int			length = 0;
    vmcode		*save_code;
    long		save_size;
    int			share_default;
    int			clauses = 0;

    save_code = BufReadW(procedure->codebuf, code);
    save_size = procedure->size;
    if (VarType(ttype)) {
	varcl = 0;
	clds = procedure->clauses;
	types->matched = ALL_TYPES;
	types->not_matched = ~TagMask(ttype);
	Next_Matching_VM(clds, TagMask(ttype), types);
	if (clds) {
	    nextcl = clds->next;
	    Next_Matching_VM(nextcl, TagMask(ttype), types);
	} else
	    nextcl = 0;
    }
    else
    {
	Next_Val(clds, cval, contcl);
	types->matched = NONVAR_TYPES;
	types->not_matched = clds ? ~TagMask(ttype) : 0;
	Next_Matching_Val(procedure, varcl, ttype, cval, types)
	if (clds) {
	    nextcl = clds->nextv;
	    Next_Val(nextcl, cval, contcl);
	} else
	    nextcl = 0;
    }
    if (varcl) {
	nextvarcl = varcl->nextv;
	Next_Matching_Val(procedure, nextvarcl, ttype, cval, types)
    } else
	nextvarcl = 0;

    /* This sequence links all pure var clauses and those with matching
     * type and value tests. Thus it can share the default sequence either if
     * there is no SOT and no not matching types, or SOT and no matching
     * types. */
    share_default = !ProcedureSOT(procedure) &&
		    !(procedure->var_types & (ALL_TYPES ^ TagMask(ttype))) ||
	ProcedureSOT(procedure) && !(procedure->var_types & TagMask(ttype));

    for (;;)
    {
	cl = (clds && clds < varcl || !varcl) ? clds : varcl;
	if (!cl) {			/* no clauses */
	    *address = (vmcode) FailCode;
	    *contcl = 0;
	    return code;
	}
	Buf_Alloc(procedure->codebuf, code, L_TRY);
	if (ClauseDet(cl) || (!nextcl && !nextvarcl && !(clds && varcl)) ||
	    ClauseUnif(cl) && ClauseSpec(cl) &&
		!(cl->nomatch & TagMask(ttype)))
	{				/* last clause */
	    if (nextcl) {
		while (nextcl && nextcl->val.nint == cval)
		    nextcl = nextcl->nextv;
		*contcl = nextcl;
	    }
	    if (!clauses) {
		Add_Label(address,  Entry(cl, ttype));
		return code;
	    } else {
		Trust_(Entry(cl, ttype), debug)
		break;
	    }
	}
	else if (share_default && cl == clds && !nextcl && varcl && varcl->retry)
	{				/* last nonvar & share the var path */
	    *contcl = clds->nextv;
	    if (!clauses) {
		Label_(address)
		if (PriFlags(procedure->proc) & PROC_PARALLEL) {
#ifdef ORACLE
		    Buf_Alloc(procedure->codebuf, code, L_PAR+L_TRY);
		    Store_2d(Try_parallel,arity)
		    Store_2d(Fail_clause,0)
		    Store_d(Try_clause)
#endif
		    procedure->size += PAR_SIZE;
		}
		Try_Else(arity, Entry(clds, ttype), varcl->retry,
			PriFlags(procedure->proc) & PROC_PARALLEL ? 0 : debug)
		return code;
	    } else {
		Retry_Else(Entry(clds, ttype), varcl->retry, debug)
		break;
	    }
	}
	else
	{				/* not last */
	    if (!clauses) {
		Label_(address)
		if (PriFlags(procedure->proc) & PROC_PARALLEL) {
#ifdef ORACLE
		    Buf_Alloc(procedure->codebuf, code, L_PAR+L_TRY);
		    Store_2d(Try_parallel,arity)
		    Store_2d(Fail_clause,0)
		    Store_d(Try_clause)
#endif
		    procedure->size += PAR_SIZE;
		}
		Try_(arity, Entry(cl, ttype),
			PriFlags(procedure->proc) & PROC_PARALLEL ? 0 : debug);
	    } else {
		Retry_(Entry(cl, ttype), debug);
	    }
	    if (cl == clds) {
		clds = nextcl;
		if (clds) {
		    nextcl = clds->nextv;
		    if (VarType(ttype)) {
			Next_Matching_VM(nextcl, TagMask(ttype), types)
		    } else {
			Next_Val(nextcl, cval, contcl)
		    }
		}
	    }
	    else {
		varcl = nextvarcl;
		if (varcl) {
		    nextvarcl = varcl->nextv;
		    Next_Matching_Val(procedure, nextvarcl, ttype, cval, types)
		}
	    }
	}
	length++;
	clauses++;
    }
    if (++length > MAX_SEQ_PATH_LENGTH &&
	SEQ_FACTOR * length > procedure->clauseno &&
	procedure->var_path &&
	!(procedure->types[TTVV].not_matched & TagMask(ttype)) &&
	/* must not have matched other types */
	!(procedure->types[TTVV].matched & ~(TagMask(ttype))))
    {
	/* the path is too long, drop it and take all clauses instead */
	Add_Label(address, procedure->var_path)
	procedure->not_indexed |= TagMask(ttype);
	code = BufWriteR(procedure->codebuf, save_code);
	procedure->size = save_size;
/*
#ifdef TUNE_COMP
	p_fprintf(warning_output_,
	    "%s/%d: SEQ_FACTOR exceeded (%d out of %d)\n",
	    DidName(procedure->did),
	    procedure->arity,
	    length,
	    procedure->clauseno);
#endif
*/
    }
    return code;
}

/*
 * Generate the search table. Currently the method is to generate
 * a sorted table and to apply either sequential or binary search to it.
 * Due to the efficiency of the binary search, the table must
 * be PWORD ALIGNED !!!
 */
static vmcode *
_hashed_path(int ttype,
	proc_desc *procedure,
	register vmcode *code,
	vmcode *address,	/* points to an instruction's argument */
	vmcode *default_code)	/* points to an instruction */
{
    cl_desc		*clds = procedure->types[ttype].first;
    long		size = procedure->types[ttype].size;
    struct table_entry	*table;
    int			search;
    vmcode		*save_code;
    long		mm;

    if (ttype == TINT && ProcedureMinMax(procedure)) {
	search = RANGE_SEARCH;
	size += 2;
	address--;
    }
    else
	search = BIN_SEARCH;
    table = (struct table_entry *) (AllocateCodeBlock(
	size * sizeof(struct table_entry)/sizeof(vmcode) + LABEL_SIZE,
	ProcLink(procedure->start), 0L, (uword) size, HASH_TABLE, 0L));
    ProcLink(procedure->start) = (uword)(ProcHeader(table));

    if (search == BIN_SEARCH)
        *address-- = size;
    else if (search == RANGE_SEARCH)
	*address-- = size - 2;
    save_code = code;
    code = (vmcode *) table;
    Label_(address);
    table = (struct table_entry *) code;
    code = save_code;
    /* increment size; add 1 for alignment and 2 for Comment, + Label */
    procedure->size += size * sizeof(struct table_entry)/sizeof(vmcode) + 
	3 + LABEL_SIZE;

    if (search == RANGE_SEARCH) {
	cl_desc		*dummy;

	if (ProcedureMin(procedure)) {
	    table->arg = procedure->min+1;
	    mm = procedure->min;	
	} else {
	    table->arg = mm = MIN_S_WORD;
	}
	code = _seq_path(ttype, (cl_desc *) 0, procedure, mm, code,
		(vmcode *) &table->code, &dummy);
	table++;
	mm = ProcedureMax(procedure) ? procedure->max : MAX_S_WORD;
	table->arg = mm;
	code = _seq_path(ttype, (cl_desc *) 0, procedure, MAX_S_WORD, code,
		(vmcode *) &table->code, &dummy);
	table++;
    }

    while (clds)
    {
	table->arg = clds->val.nint;
	code = _seq_path(ttype, clds, procedure, clds->val.nint, code, (vmcode *) &table->code,
		&clds);
	table++;
    }

    return code;
}

static vmcode *
_index_next_arguments(proc_desc *procedure, int next_index, int max_index, register vmcode *code)
{
    cl_desc		*clds;
    vmcode		*last_switch;
    vmcode		*start;
    vmcode		*switch_end;
    vmcode		*save_code;
    long		switch_amount;
    arg_desc		*args = procedure->gargs + 1;
    int			switch_no = 1;
    /* If we've optimized away a cut, we must start with the main index */
    int			sort_start = ProcedureDet(procedure) ? 2 : 1;
    int			i;
    int			j;

    /* reset data which is used only for the first switch */
    for (clds = procedure->clauses; clds; clds = clds->next)
    {
	clds->indexed_entry = clds->entry;
	if (ClauseSpec(clds) && !VarMask(clds->desc_tag.kernel & ALL_TYPES)) {
	    Reset_Clause_Det(clds);
	}
	if (ClauseSpec(clds) && ProcedureDet(procedure) &&
		!(clds->desc_tag.kernel & procedure->not_indexed))
	{
	    clds->desc_tag.kernel = TCLDESC;
	    clds->nomatch = 0;
	}
	else {
	    clds->desc_tag.kernel = TCLDESC;
	    Reset_Clause_Specs(clds);
	}
    }
    Reset_Procedure_MinMax(procedure);
    procedure->var_types = 0;
    procedure->spec_number = 0;
    procedure->var_path = 0;

    do
    {
	procedure->index = next_index;
	next_index = _select_index(procedure, next_index + 1, max_index);
	if (!_analyse_clauses(procedure, switch_no))
	    continue;

	switch_amount = procedure->types[TSTAT].size;
	if (InputMode(Mode(procedure->index, procedure->mode)))
	    switch_amount += procedure->clauseno + 1;

	if (switch_amount <= args[switch_no].state &&
		args[switch_no].state > procedure->clauseno + 1)
	    /* it is not better than any previous input mode argument */
	    continue;

	Buf_Alloc(procedure->codebuf, code, L_START);
	New_Label(start);

	/* Now generate the SWITCH instruction */
	Reset_Procedure_SOT(procedure);
	code = _switch(procedure, procedure->ntypes, procedure->ctype,
		procedure->types[procedure->ctype].size, code);
	New_Label(switch_end);

	/* handle input mode */
	if (InputMode(Mode(procedure->index, procedure->mode)))
	{
	    Store_d(Failure);
	    last_switch = 0;
	} else {
	    Branch_(last_switch);
	}
	save_code = _indexed_paths(procedure, code, switch_end);

	/* find the place to put this indexed argument */
	for (i = sort_start; i <= switch_no; i++)
	    if (args[i].state < switch_amount)
		break;
	if (*args[i - 1].addr == Failure) {	/* will never be taken */
	    code = save_code;
	    continue;
	}
	if (last_switch) {	/* move following switches one higher */
	    Copy_Label(last_switch, args[i - 1].addr)
	    for (j = ++switch_no; j > i; j--)
	    {
		args[j].addr = args[j - 1].addr;
		args[j].state = args[j - 1].state;
	    }
	    args[i].addr = last_switch;
	} else {		/* cancel all following switches */
	    for (j = switch_no; j >= i; j--)
	    {
		code = (vmcode *) *args[j - 1].addr;
		/* this would work for complete dead code elimination	*/
		/* New_Label(args[j].addr);				*/
		Comment_(args[j].addr + 1)
	    }
	    switch_no = i;
	    args[i].addr = save_code - 1;
	}
	Add_Label(args[i - 1].addr, start)
	args[i].state = switch_amount;
	code = save_code;
    }
    while (next_index);
    if (switch_no > procedure->clauseno)
    {
	vmcode		clause_code;

	i = procedure->clauseno;
	clause_code = *args[switch_no].addr;
	for (j = switch_no; j > i; j--)
	{
	    code = (vmcode *) *args[j - 1].addr;
	    Comment_(args[j].addr + 1)
	}
	*args[i].addr = clause_code;
	code = save_code;
    }
    return code;
}

vmcode *
allocate_code_block(long int size, uword link, uword bid, uword fid, uword btype, uword cid)
{
    vmcode	*code;

    code = (vmcode *) hg_alloc(((int)size + PROC_PREFIX_SIZE) * sizeof(vmcode));
    Make_Prefix(link, size, bid, fid, btype, cid)
    return code;
}

#ifdef PRINTAM

static void
_print_types(proc_desc *procedure)
{
    int 	      i;
    printf(" T	   first      last     lastvar	mv len s  matched not_matched\n");
    for (i=0; i < INDEX_TYPES; i++)
      printf("%2i: 0x%08x 0x%08x 0x%08x%2d%3d%3d %8x %8x\n",
	  i,
	  procedure->types[i].first,
	  procedure->types[i].last,
	  procedure->types[i].lastvar,
	  procedure->types[i].more_values,
	  procedure->types[i].length,
	  procedure->types[i].size,
	  procedure->types[i].matched,
	  procedure->types[i].not_matched);

}

static void
_print_cldesc(proc_desc *procedure)
{
    cl_desc	      *clds;

    printf("	addr   desc_tag tag cflags   nomatch\n");
    for (clds = procedure->clauses; clds; clds = clds->next)
      printf("0x%08x 0x%06x %2d 0x%06x 0x%06x\n",
	  clds,
	  clds->desc_tag.all & ALL_TYPES,
	  clds->tag.all,
	  clds->cflags,
	  clds->nomatch & ALL_TYPES);
}

#endif
