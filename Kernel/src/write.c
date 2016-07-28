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
 * VERSION	$Id: write.c,v 1.19 2016/07/28 03:34:36 jschimpf Exp $
 */

/*
 * IDENTIFICATION		write.c
 *
 * DESCRIPTION:		SEPIA terminal input/output routines
 *				by Dominique Henry de Villeneuve
 *
 * CONTENTS: 		write/1,2
 *			writeq/1,2
 *			write_canonical/1,2
 *			print/1,2
 *			printf_/4
 *			display/1
 *			depth/1
 *
 */

/*
 * INCLUDES:
 */

#include	"config.h"
#include	<math.h>
#include	<stdio.h>
#include	"sepia.h"
#include	"types.h"
#include	"embed.h"
#include	"mem.h"
#include	"error.h"
#include	"dict.h"
#include	"lex.h"
#include 	"ec_io.h"
#include	"emu_export.h"
#include	"module.h"
#include	"property.h"
#include	"read.h"	/* for transformation functions */
#include	"os_support.h"

#ifdef STDC_HEADERS
#include <stdlib.h>
#endif

#if STDC_HEADERS || HAVE_STRING_H
#  include <string.h>
#else
#  include <strings.h>
#  define strchr index
#endif

#ifdef HAVE_CTYPE_H
#include <ctype.h>
#endif


/* 
 * DEFINES
 */

#define 	ATOM		0
#define		OPERATOR	1

#define WRITE_OPTIONS_WRITE	(OUT_DOLLAR_VAR)
#define WRITE_OPTIONS_PRINT	(OUT_DOLLAR_VAR|PRINT_CALL)
#define WRITE_OPTIONS_DISPLAY	(CANONICAL|DOTLIST)
#define WRITE_OPTIONS_WRITEQ	(QUOTED|FULLDEPTH|VAR_NUMBERS|STD_ATTR|NO_MACROS|OUT_DOLLAR_VAR)
#define WRITE_OPTIONS_CANON	(QUOTED|FULLDEPTH|VAR_NUMBERS|STD_ATTR|NO_MACROS|CANONICAL|DOTLIST)

#define UseDepth(id)		(!((id) & FULLDEPTH))

#define MacrosAllowed(idwrite)	(!((idwrite) & NO_MACROS))
#define GoalMacro(idwrite)	(idwrite & WRITE_GOAL ? TR_GOAL : \
				    (idwrite & WRITE_CLAUSE ? TR_CLAUSE : 0))

#define Handle_Type_Macro(t)						\
	if (MacrosAllowed(idwrite) && DidMacro(TransfDid(t))) {		\
	    pword *tr_res = _write_trafo(TransfDid(t),			\
				GoalMacro(idwrite), &idwrite,		\
				val, tag, module, mod_tag, ec_eng);	\
	    if (tr_res) {						\
		val.all = tr_res->val.all;				\
		tag.all = tr_res->tag.all;				\
		goto _pwrite_;	/* print the transformed term */	\
	    }								\
	}



/*
 * FUNCTION DECLARATIONS:
 */

static int 
		_get_mode_mask(char *string, int *clr_mask, int *mask),
		_merge_output_modes(int mask, int remove, int add),
		_handle_string_size(value v, type t, int quoted_or_base),
		_handle_to_string(value v, type t, char *buf, int quoted_or_base),
		_num_string_size(value v, type t, int quoted),
		_int_to_string(value v, type t, char *buf, int quoted_or_base),
		_float_to_string(value v, type t, char *buf, int precise),
		_float_to_string_opt(value v, type t, char *buf, int precise, int options),
		_printf_asterisk(word asterisk, pword **list, type arg_type, stream_id nst, char *par),
		_print_var(int idwrite, value v, type t, stream_id str, int depth, dident module, type mod_tag, syntax_desc *sd, ec_eng_t *),
		_pwrite1(int idwrite, stream_id out, value val, type tag, int maxprec, int depth, dident module, type mod_tag, syntax_desc *sd, int flags, ec_eng_t *ec_eng),
		_is_proper_list(pword *list),
		_write_args_from_list(int idwrite, stream_id out, pword *list, int depth, dident module, type mod_tag, syntax_desc *sd, int flags, ec_eng_t *ec_eng),
		_write_quoted(int idwrite, stream_id out, char *name, word len, char quotechar, syntax_desc *sd, int depth),
		_write_infix(int idwrite, stream_id out, dident d, int flags, dident module, type mod_tag, syntax_desc *sd, pword *right, int depth),
		_write_atom(int idwrite, stream_id out, dident d, int what, int flag, dident module, type mod_tag, syntax_desc *sd, int depth),
		_write_string(int idwrite, stream_id out, char *start, word length, int depth),
		_portray_term(int idwrite, stream_id out, value val, type tag, dident module, type mod_tag, ec_eng_t*);

static void	_output_mode_string(char *s, int mask);

static pword	*_write_trafo(dident d, int flags, int *idwrite, value val, type tag, dident module, type mod_tag, ec_eng_t *ec_eng);


/*
 * STATIC VARIABLE DEFINITIONS: 
 */

static dident		d_dollar_var,
			d_portray1,
			d_portray2,
			d_print_attributes,
                        d_var_name,
                        d_vname2;

static char	output_mode_chars[OUTPUT_MODES+1] = "OD.QvVPKmGMTCN_IUFL";

static int	output_mode_mask = QUOTED | PRINT_CALL | ATTRIBUTE;



/*
 * FUNCTION DEFINITIONS: 
 */

/*
 * visible_d_procedure() is the same as visible_procedure() except that
 * it only returns something if there is a CODE_DEFINED (callable)
 * procedure. It also does not set global_bip_error.
*/
static pri *
visible_d_procedure(dident functor, dident module, type module_tag)
{
    int err;
    pri *pd = visible_procedure(functor, module, module_tag, 0, &err);
    return pd && (PriFlags(pd) & CODE_DEFINED) ? pd : 0;
}


#define	Write_Infix(ww, s, d, flags, mod, mt, sd, arg, narg)		\
	status = _write_infix(ww, s, d, flags, mod, mt, sd, narg, depth);\
	if (status < 0)							\
	   return(status);

#define	Write_Postfix(ww, s, d, flags, mod, mt, sd)			\
	if((status = ec_outfc( s, ' ')) < 0 || 				\
	(status = _write_atom(ww, s, d, OPERATOR, flags, mod, mt, sd, depth)) < 0)	\
	return(status);

#define	Write_Prefix(ww, s, d, flags, mod, mt, sd)			\
	if((status = _write_atom(ww, s, d, OPERATOR, flags, mod, mt, sd, depth)) < 0 || \
	(status = ec_outfc( s, ' ')) < 0) return(status);

#define	Write_Atom(ww, s, d, what, flags, mod, mt, sd)			\
    if((status = _write_atom(ww, s, d, what, flags, mod, mt, sd, depth)) < 0)	\
	return(status);

#define Pwrite(ww, s, v, t, mp, d, mod, mt, sd, flags) 			\
    if((status = _pwrite1(ww, s, v, t, mp, d, mod, mt, sd, flags, ec_eng)) < 0)	\
	return(status);
	
#define Write_Char(s,c) if ((status = ec_outfc(s,c)) < 0) return(status);

#define Write_Str(s,str,l) if ((status = ec_outf(s,str,l)) < 0) return(status);

#define Write_Comma(s) \
	Write_Char(s, ','); \
	if (!(idwrite & WRITE_COMPACT)) { Write_Char(s, ' '); }

#define Next_Element(element, list, Return)			\
	{							\
	    if (list)						\
	    {							\
		element = list++;				\
		Dereference_(list)				\
		Dereference_(element)				\
		if (IsNil(list->tag))				\
		    list = 0;					\
		else if (!IsList(list->tag)) {			\
		    Return(TYPE_ERROR);				\
		}						\
		else {						\
		    list = list->val.ptr;			\
		}						\
	    }							\
	    else {						\
		Return(BAD_ARGUMENT_LIST);			\
	    }							\
	}

#define Get_Counter(start,ptr,c)				\
	c = 0;							\
	ptr = start;						\
	while (*(ptr) >= '0' && *(ptr) <= '9')			\
	    c = c * 10 + *(ptr)++ - '0';


#define MAXPREC		((sd->options & LIMIT_ARG_PRECEDENCE) ? 999 : 1200)


/*
	write_(Term, Module)
 	writes the Prolog term (tag,val) to the current output stream.
 	The term is written according to the current operator
 	declarations and spaces are inserted to separate operators
 	where necessary.
	Functors, atoms and strings are not quoted.
*/
static int
p_write(value val, type tag, value vm, type tm, ec_eng_t *ec_eng)
{
    int		res;
    Check_Module(tm, vm);
    Lock_Stream(current_output_);
    res = ec_pwrite(ec_eng, 0, WRITE_OPTIONS_WRITE, current_output_, val, tag, 1200, 0, vm.did, tm);
    Unlock_Stream(current_output_);
    return res;
}

/*
	writeq_(Term, Module)
	The Prolog term is written to the current output stream
	according to the current operator declarations.
	Functors, atoms and strings are quoted.
*/
static int
p_writeq(value val, type tag, value vm, type tm, ec_eng_t *ec_eng)
{
    int		res;
    Check_Module(tm, vm);
    Lock_Stream(current_output_);
    if (IsAtom(tag) && val.did == d_.eocl)
	res = ec_outf(current_output_, "'.'", 3);
    else
	res = ec_pwrite(ec_eng, 0, WRITE_OPTIONS_WRITEQ,
		    current_output_, val, tag, 1200, 0, vm.did, tm);
    Unlock_Stream(current_output_);
    return res;
}


/*
	writeq_(Stream, Term, Module)
*/
static int
p_writeq3(value vals, type tags, value val, type tag, value vm, type tm, ec_eng_t *ec_eng)
{
    int		res;
    stream_id	out;

    Get_Locked_Stream(vals, tags, SWRITE, out);
    Check_Module(tm, vm);
    if (IsAtom(tag) && val.did == d_.eocl)
     	res = ec_outf(out, "'.'", 3);
    else
	res = ec_pwrite(ec_eng, 0, WRITE_OPTIONS_WRITEQ,
		    out, val, tag, 1200, 0, vm.did, tm);
    return res;
}

/*
	write_canonical_(Term, Module)
*/
static int
p_write_canonical(value val, type tag, value vm, type tm, ec_eng_t *ec_eng)
{
    int		res;
    Check_Module(tm, vm);
    Lock_Stream(current_output_);
    if (IsAtom(tag) && val.did == d_.eocl)
	res = ec_outf(current_output_, "'.'", 3);
    else
	res = ec_pwrite(ec_eng, 0, WRITE_OPTIONS_CANON,
		    current_output_, val, tag, 1200, 0, vm.did, tm);
    Unlock_Stream(current_output_);
    return res;
}

/*
	write_canonical_(Stream, Term, Module)
*/
static int
p_write_canonical3(value vals, type tags, value val, type tag, value vm, type tm, ec_eng_t *ec_eng)
{
    stream_id	out;

    Get_Locked_Stream(vals, tags, SWRITE, out);
    Check_Module(tm, vm);
    if (IsAtom(tag) && val.did == d_.eocl)
        return ec_outf(out, "'.'", 3);
    else
	return ec_pwrite(ec_eng, 0, WRITE_OPTIONS_CANON,
		    out, val, tag, 1200, 0, vm.did, tm);
}

/*
 	write_(Stream, Term, Module)
 	writes the Prolog term (tag,val) to the specified output stream.
 	The term is written according to the current operator
 	declarations and spaces are inserted to separate operators
 	where necessary.
*/
static int
p_write3(value vals, type tags, value val, type tag, value vm, type tm, ec_eng_t *ec_eng)
{
    int		res;
    stream_id out;

    Get_Locked_Stream(vals, tags, SWRITE, out);
    Check_Module(tm, vm);
    return ec_pwrite(ec_eng, 0, WRITE_OPTIONS_WRITE, out, val, tag, 1200, 0, vm.did, tm);
}


/* 
 * writeln is in C because we want it atomic and the correct flushing
 * behaviour (like nl)
 */
static int
p_writeln(value vals, type tags, value val, type tag, value vm, type tm, ec_eng_t *ec_eng)
{
    int		res;
    stream_id	out;

    Get_Locked_Stream(vals, tags, SWRITE, out);
    Check_Module(tm, vm);
    res = ec_pwrite(ec_eng, 0, WRITE_OPTIONS_WRITE, out, val, tag, 1200, 0, vm.did, tm);
    if (res == PSUCCEED)
	res = ec_newline(out);
    return res;
}

/*
 	print_(Term, Module)
 	writes the Prolog term (tag,val) using portray/1,2 if it exists.
 	The term is written according to the current operator
 	declarations and spaces are inserted to separate operators
 	where necessary.
*/
static int
p_print(value val, type tag, value vm, type tm, ec_eng_t *ec_eng)
{
    int		res;

    Check_Module(tm, vm);
    Lock_Stream(current_output_);
    res = ec_pwrite(ec_eng, 0, WRITE_OPTIONS_PRINT, current_output_, val, tag, 1200, 0, vm.did, tm);
    Unlock_Stream(current_output_);
    return res;
}

/*
 	print_(Stream, Term, Module)
 	writes the Prolog term (tag,val) to the specified output stream,
	possibly using portray/1,2 to output it.
 	The term is written according to the current operator
 	declarations and spaces are inserted to separate operators
 	where necessary.
*/
static int
p_print3(value vals, type tags, value val, type tag, value vm, type tm, ec_eng_t *ec_eng)
{
    stream_id	out;

    Get_Locked_Stream(vals, tags, SWRITE, out);
    Check_Module(tm, vm);
    return ec_pwrite(ec_eng, 0, WRITE_OPTIONS_PRINT, out, val, tag, 1200, 0, vm.did, tm);
}


/*
 *	display(Stream, Term)
 *	The output is written (even for the operators) in functional form.
 *	Functors, atoms and strings are not quoted.
*/
static int
p_display(value vs, type ts, value val, type tag, ec_eng_t *ec_eng)
{
    stream_id	out;

    Get_Locked_Stream(vs, ts, SWRITE, out);
    /* the module tag is not meaningful here				*/
    return ec_pwrite(ec_eng, 0, WRITE_OPTIONS_DISPLAY,
		    out, val, tag, 1200, 0, d_.dummy_module, tdict);
}


/* auxiliary for ec_pwrite(): terminate term with fullstop and/or newline */

static int
_terminate_term(stream_id nst, int options, syntax_desc *sd)
{
    int status;
    if (options & TERM_FULLSTOP)
    {
	/* write a space if last character was a symbol */
	if (Symbol(sd->char_class[(unsigned char)StreamLastWritten(nst)]))
	{
	    Write_Char(nst, ' ');
	}
	Write_Char(nst, '.');
	if (options & TERM_NEWLINE)
	    return ec_newline(nst);	/* maybe YIELD_ON_FLUSH_REQ */
	else
	    return ec_outfc(nst, ' ');
    }
    else if (options & TERM_NEWLINE)
    {
	return ec_newline(nst);		/* maybe YIELD_ON_FLUSH_REQ */
    }
    return PSUCCEED;
}


/*
 * ec_pwrite() - write a Prolog term
 *
 * When writing any meta variables are marked (tag is modified) these marks
 * are trailed. This function is simply a wrapper round prwite1() which
 * does initialisation and finalisation, while pwrite() is recursive.
 */
int
ec_pwrite(ec_eng_t *ec_eng, int mode_clr, int mode_set, stream_id out, value val, type tag, int maxprec, int depth, dident module, type mod_tag)
{
    pword			**old_tt = TT, *old_tg = TG, *old_ld = LD;
    syntax_desc *		sd = ModuleSyntax(module);
    int				idwrite;
    int				result;

    /* Catch null stream here because some code within _pwrite1()
     * assumes the presence of a stream buffer! */
    if ((StreamMode(out) & STYPE) == SNULL)
	return PSUCCEED;
    	
    if (!IsTextStream(out))
	return STREAM_MODE;

    /*
     * Merge the stream's default output mode settings with the modes
     * for this particular call
     */
    idwrite = _merge_output_modes(StreamOutputMode(out), mode_clr, mode_set);

    /*
     * For backward compatibility, map obsolete syntax options to output modes
     */
    if (sd->options & DOLLAR_VAR)
    	idwrite |= OUT_DOLLAR_VAR;
    /* not fully compatible:
    if (sd->options & DENSE_OUTPUT)
    	idwrite |= WRITE_COMPACT;
    */

    /*
     * If 0, inherit print depth from stream or from global setting
     * (if the FULLDEPTH flag is set, this is irrelevant)
     */
    if (depth == 0)
    {
	depth = StreamPrintDepth(out);
	if (depth == 0)
	    depth = PrintDepth;
    }

    /*
     * If the module is locked we cannot call any print handlers
     * or look up the visible operators.
     * In principle, we should also not see the locked module's
     * syntax, but that may be unnecessarily restrictive.
     */
    if (UnauthorizedAccess(module, mod_tag))
    	idwrite = idwrite & ~(ATTRIBUTE|PORTRAY2|PORTRAY1|PRINT_CALL)
			|NO_MACROS|CANONICAL;

    /*
     * If needed, do the expensive procedure lookups for portray/1,2
     * here and set PORTRAY2 and PORTRAY1 flags accordingly.
     */
    if (idwrite & PRINT_CALL)
    {
	if (visible_d_procedure(d_portray2, module, mod_tag))
	    idwrite |= PORTRAY2;
	if (visible_d_procedure(d_portray1, module, mod_tag))
	    idwrite |= PORTRAY1;
    }

    result = _pwrite1(idwrite, out, val, tag, maxprec, depth,
			module, mod_tag, sd, ARGLAST, ec_eng);
    
    /* terminate the term, if requested */
    if (result == PSUCCEED)
	result = _terminate_term(out, idwrite, sd);

    /*
     * Pop stuff that may have been left by write macros and
     * untrail all marking that has been done during printing.
     */
    Untrail_Variables(old_tt); TG = old_tg; LD = old_ld;
    return result;

}


static int
_is_signed_number(value v, type t)
{
    pword sign;
    int res = tag_desc[TagType(t)].arith_op[ARITH_SGN](NULL, v, &sign);
    /* res can be ARITH_EXCEPTION for zero-spanning breals! */
    if (res != PSUCCEED) return 1;
    if (sign.val.nint < 0) return 1;
    if (sign.val.nint > 0) return 0;

    /* deal with negative zeros */
    switch (TagType(t))
    {
    case TDBL:
	return PedanticZeroLess(Dbl(v),0.0);
    case TIVL:
	return PedanticZeroLess(IvlLwb(v.ptr),0.0);
    }
    return 0;
}


/*
 * _pwrite1() - write a Prolog term
 *
 * idwrite: flags for the different write options (see ec_io.h)
 *	CANONICAL	ignore operators
 *	FULLDEPTH	ignore depth
 *	DOTLIST		write lists in dot notation
 *	QUOTED		print quotes when needed
 *	VAR_NUMBERS	print var number only
 *	VAR_NAMENUM	print var name (if available) and number
 *	VAR_ANON	print var as _
 *	PRINT_CALL	print was called, use portray
 *	PORTRAY_VAR	call portray even for variables
 *	WRITE_GOAL	print with goal output macros
 *	ATTRIBUTE	print attributes of metaterms in user format
 *	STD_ATTR	print attributes of metaterms in standard format
 *	NO_MACROS	don't apply write macros
 *	PORTRAY2	a portray/2 predicate exists
 *	PORTRAY1	a portray/1 predicate exists
 *	VARTERM		print variables as '_'(...)
 * flags: further context information for writeq
 *	ARGOP		immediate argument of any operator
 *	ARGYF		immediate argument of YF or YFX operator
 *	ARGLAST		last term, i.e. a delimiter follows
 *	ARGLIST		inside a bracketed list, used to handle
 *			bars that occur as atoms or operators
 *	ARGTERM		inside a structure argument, used to handle
 *			commas that are not argument separators
 *	ARGSIGN		term _textually_ follows a -/1 or +/1
 * maxprec: the maximum precedence that may be printed without brackets
 */

#define UnsignedNumberNeedsBrackets \
    ((idwrite & QUOTED) && (flags & ARGSIGN))

static int
_pwrite1(int idwrite, stream_id out, value val, type tag, int maxprec, int depth, dident module, type mod_tag, syntax_desc *sd, int flags, ec_eng_t *ec_eng)
{
    pword	*arg;
    int	status, arity;
    dident	d;
    opi			d_opi_desc;
    int			res;

_pwrite_:
    if (UseDepth(idwrite) && depth <= 0)
	return (ec_outf(out, "...", 3));

    if (IsRef(tag))
	if ((idwrite & (PORTRAY2|PORTRAY1))
		&& (idwrite & PORTRAY_VAR || IsMeta(tag))
		&& _portray_term(idwrite, out, val, tag, module, mod_tag, ec_eng))
	    return PSUCCEED;
	else
	{
	    return _print_var(idwrite, val.ptr->val, val.ptr->tag, out, depth,
					module, mod_tag, sd, ec_eng);
	}
    else if ((idwrite & (PORTRAY2|PORTRAY1))
    		&& _portray_term(idwrite, out, val, tag, module, mod_tag, ec_eng))
	return PSUCCEED;

    switch (TagType(tag))
    {
    case TDICT:
	Handle_Type_Macro(TDICT)
	if (MacrosAllowed(idwrite) && DidMacro(val.did))
	{
	    pword *narg;
	    if ((narg = _write_trafo(val.did, GoalMacro(idwrite),
				&idwrite, val, tag, module, mod_tag, ec_eng)))
	    {
		val.all = narg->val.all;
		tag.all = narg->tag.all;
		idwrite &= ~(WRITE_GOAL|WRITE_CLAUSE);
		goto _pwrite_;		/* print the transformed term */
	    }
	}
	return _write_atom(idwrite,out,val.did,ATOM,flags,module,mod_tag, sd, depth);

    case TINT:
	Handle_Type_Macro(TINT)
	if (UnsignedNumberNeedsBrackets && (val.nint >= 0))
	    return (p_fprintf(out, "(%" W_MOD "d)", val.nint));
	else
	    return (p_fprintf(out, "%" W_MOD "d", val.nint));

    case TDBL:
	Handle_Type_Macro(TDBL)
	{
	    char fbuf[32];
	    int size = _float_to_string_opt(val, tag, fbuf, idwrite & QUOTED, sd->options);
	    if (UnsignedNumberNeedsBrackets && fbuf[0] != '-')
	    {
		if ((status = ec_outfc(out, '(')) < 0 ||
		    (status = ec_outf(out, fbuf, size)) < 0 ||
		    (status = ec_outfc(out, ')')) < 0)
			return status;
		return status;
	    }
	    else
		return ec_outf(out, fbuf, size);
	}

    case TSTRG:
	Handle_Type_Macro(TSTRG)
	return  (idwrite & QUOTED)  ?
		_write_quoted(idwrite, out, StringStart(val), StringLength(val),
					(char) sd->current_sq_char, sd, depth) :
		_write_string(idwrite, out, StringStart(val),
				StringLength(val), depth);

    case TNIL:
	Handle_Type_Macro(TDICT)
	return (ec_outf(out, "[]", 2));

    case TEXTERN:	/* shouldn't occur */
        return p_fprintf(out, "EXTERN_%" W_MOD "x", val.nint);

    case TPTR:
        return p_fprintf(out, "PTR_%" W_MOD "x", val.ptr);

    case TSUSP:
	Handle_Type_Macro(TSUSP)
	if (!val.ptr)
	    return p_fprintf(out, "'SUSP-0-dead'");
	res = SuspDebugInvoc(val.ptr);
        status = p_fprintf(out, "'SUSP-%s%d-%s'",
		res ? "" : "_", res ? res : val.ptr - TG_ORIG, 
		SuspDead(val.ptr) ? "dead" : SuspScheduled(val.ptr) ? "sched" : "susp");
	if (status < 0)
	    return status;
#if 0
	if (SuspDead(val.ptr) || !(idwrite & QUOTED))
	    return PSUCCEED;
	arg = &val.ptr[SUSP_GOAL];	/* print: (Goal,Module) */
	arity = 2;
	goto _write_args_;		/* (arg,arity) */
#else
	return PSUCCEED;
#endif

    case THANDLE:
	Handle_Type_Macro(THANDLE)
	if (ExternalClass(val.ptr)->to_string && ExternalData(val.ptr))
	{
	    int bufsize = 1 + (ExternalClass(val.ptr)->string_size)(ExternalData(val.ptr), idwrite&QUOTED?1:0);
	    New_Array(char, buf, bufsize);
	    int len = (ExternalClass(val.ptr)->to_string)(ExternalData(val.ptr), buf, idwrite&QUOTED?1:0);
	    status = ec_outf(out, buf, len);
	    Delete_Array(char, buf, bufsize);
	    return status;
	}
	else
	{
	    return p_fprintf(out, "'HANDLE'(16'%08x)", ExternalData(val.ptr));
	}

    case TPROC:		/* an atom goal in the compiler */
	return _write_atom(idwrite, out, PriDid((pri *) (val.ptr)),
		ATOM,flags,module,mod_tag, sd, depth);

    case TCOMP:
    case TGRS:		/* a ground structure in the compiler */
	if (val.ptr == 0) {	/* e.g. default WL */
	    return p_fprintf(out, "BAD_TERM_0x%" W_MOD "x_0x%" W_MOD "x", val.all, tag.all);
	}
	Handle_Type_Macro(TCOMP)
	if (SameTypeC(val.ptr->tag, TPROC))
	{
	    /* We are inside the compiler, change TPROC to TDICT */
	    d = PriDid((pri *) (val.ptr->val.ptr));
	}
	else
	    d = val.ptr->val.did;	/* did of the functor */
	arg = (val.ptr) + 1;
_write_structure_:			/* (d, arg) */
	arity = DidArity(d);
	if (d == d_dollar_var && (idwrite & OUT_DOLLAR_VAR)) /* '$VAR'/1 */
	{
	    pword *narg = arg;
	    Dereference_(narg);
	    if (IsInteger(narg->tag) && narg->val.nint >= 0) {
		if ((status = ec_outfc(out, 'A' + (char)(narg->val.nint % 26))) < 0)
		    return (status);
		if (narg->val.nint / 26)
		    return p_fprintf(out, "%" W_MOD "d", narg->val.nint / 26);
		return PSUCCEED;
	    } else if (!(sd->options & ISO_RESTRICTIONS)) {
		switch (TagType(narg->tag)) {
		case TSTRG:
		    return ec_outf(out, StringStart(narg->val),
					(int) StringLength(narg->val));
		case TDICT:
		    return ec_outf(out, DidName(narg->val.did),
					(int) DidLength(narg->val.did));
		case TNIL:
		    return ec_outf(out, "[]", 2);
		}
	    }
	    /* else print the structure normally */
	}
	if (!(idwrite & CANONICAL))
	{
	    dident hd = d;
	    if (d == d_.rulech2) {
		pword		*p = val.ptr + 1;
		Dereference_(p);
		if (IsAtom(p->tag))
		    hd = p->val.did;
		else if (IsStructure(p->tag))
		    hd = p->val.ptr->val.did;
	    }
	    if (MacrosAllowed(idwrite) && DidMacro(hd))	/* output macros */
	    {
		pword *narg;
		if ((narg = _write_trafo(hd, GoalMacro(idwrite),
				    &idwrite, val, tag, module, mod_tag, ec_eng)))
		{
		    val.all = narg->val.all;
		    tag.all = narg->tag.all;
		    idwrite &= ~(WRITE_GOAL|WRITE_CLAUSE);
		    goto _pwrite_;	/* print the transformed term */
		}
	    }
	    idwrite &= ~(WRITE_GOAL|WRITE_CLAUSE);

	    /*
	     * Check for all the functors that can have special syntax
	     */
	    if (d == d_.nilcurbr1)	/* special case {}/1 */
	    {
		if ((status = ec_outfc(out, '{')) < 0)
		    return (status);
		Dereference_(arg);
		status = _pwrite1(idwrite, out, arg->val, arg->tag, MAXPREC, 
				 depth-1, module, mod_tag, sd, 0, ec_eng);
		if (status < 0 || (status = ec_outfc(out, '}')) < 0)
		    return (status);
		return (PSUCCEED);
	    }
 	    else if (d == d_.subscript  &&  !(sd->options & NO_ARRAY_SUBSCRIPTS	))
 	    {
		pword *arg1 = arg;
 		pword *arg2 = arg + 1;
 		Dereference_(arg1);
 		Dereference_(arg2);
 		if (IsList(arg2->tag) && (IsStructure(arg1->tag) ||
 		    IsRef(arg1->tag) && !IsMeta(arg1->tag) ||
 		    IsAtom(arg1->tag) && (sd->options & ATOM_SUBSCRIPTS)))
 		{
 		    Pwrite(idwrite|CANONICAL, out, arg1->val, arg1->tag, MAXPREC,
 			     depth, module, mod_tag, sd, flags);
 		    Pwrite(idwrite, out, arg2->val, arg2->tag, MAXPREC,
 			     depth, module, mod_tag, sd, flags);
 		    return (PSUCCEED);
 		}
 	    }
	    else if (d == d_.with_attributes2  &&  !(sd->options & NO_ATTRIBUTES))
	    {
		pword *arg1 = arg;
 		pword *arg2 = arg + 1;
 		Dereference_(arg1);
 		Dereference_(arg2);
		if ((IsRef(arg1->tag) && !IsMeta(arg1->tag)) && _is_proper_list(arg2))
		{
		    Pwrite(idwrite, out, arg1->val, arg1->tag, MAXPREC, 
			     depth, module, mod_tag, sd, ARGTERM | ARGLAST);
		    Write_Char(out, '{');
		    status = _write_args_from_list(idwrite, out, arg2, depth, module, mod_tag, sd, flags, ec_eng);
		    if (status < 0) return status;
		    Write_Char(out, '}');
		    return (PSUCCEED);
		}
	    }
	    else if (d == d_.apply2  &&  (sd->options & VAR_FUNCTOR_IS_APPLY))
	    {
		pword *arg1 = arg;
 		pword *arg2 = arg + 1;
 		Dereference_(arg1);
 		Dereference_(arg2);
		if ((IsRef(arg1->tag) && !IsMeta(arg1->tag)) && _is_proper_list(arg2))
		{
		    Pwrite(idwrite, out, arg1->val, arg1->tag, MAXPREC, 
			     depth, module, mod_tag, sd, ARGTERM | ARGLAST);
		    Write_Char(out, '(');
		    status = _write_args_from_list(idwrite, out, arg2, depth, module, mod_tag, sd, flags, ec_eng);
		    if (status < 0) return status;
		    Write_Char(out, ')');
		    return (PSUCCEED);
		}
	    }
	    else if (d == d_.with2  &&  !(sd->options & NO_CURLY_ARGUMENTS))
	    {
		pword *arg1 = arg;
 		pword *arg2 = arg + 1;
 		Dereference_(arg1);
 		Dereference_(arg2);
		if (IsAtom(arg1->tag) && (IsNil(arg2->tag) || _is_proper_list(arg2)))
		{
		    Write_Atom(idwrite, out, arg1->val.did, ATOM, flags & ARGLIST, module, mod_tag, sd);
		    Write_Char(out, '{');
		    status = _write_args_from_list(idwrite, out, arg2, depth, module, mod_tag, sd, flags, ec_eng);
		    if (status < 0) return status;
		    Write_Char(out, '}');
		    return (PSUCCEED);
		}
	    }

	    /*
	     * Check whether the functor is an operator
	     */
	    if (OpiPreced(d_opi_desc = visible_op(d, module, mod_tag, &res)))
	    {			/* val is an operator */
		int		prec;
		int		openpar = 0;
		word		assoc;
		opi		post_infix;
		pword		*narg;

		prec = OpiPreced(d_opi_desc);
		assoc = OpiAssoc(d_opi_desc);
	    	narg = arg + 1;
		if (IsPostfixAss(assoc))
		{
		    dident		atom = add_dict(d, 0);
		    post_infix = visible_infix_op(atom, module, mod_tag, &res);
		}
		else {
		    Set_Opi_Invalid(post_infix);
		}
		if (  prec > maxprec 
		    || d == d_.comma && (flags & ARGTERM)
		    || flags & ARGYF && prec == maxprec &&
			(assoc == FY || assoc == XFY || assoc == FXY)
		    || OpiPreced(post_infix) && !(flags & ARGLAST)
		   )
		{
		    flags = flags  & ~(ARGTERM | ARGLIST | ARGSIGN) | ARGLAST;
		    openpar = 1;
		    Write_Char(out, '(');
		}
		Dereference_(arg);
		if (arity == 1)
		{
		    switch (assoc)
		    {
		    case FX:
			prec -= 1;
		    case FY:
			if ( !(sd->options & BLANK_AFTER_SIGN) && (
			     d == d_.minus1 ||
                             d == d_.plus1 && !(sd->options & PLUS_IS_NO_SIGN)))
			{
			    /* ignore operators to avoid confusion
			     * with signed numbers	*/
			    Write_Atom(idwrite, out, d, ATOM, flags & ARGLIST,
					 module, mod_tag, sd);
			    Write_Char(out, '(');
			    Pwrite(idwrite, out, arg->val, arg->tag,
				    MAXPREC, depth - 1, module, mod_tag,
				    sd, ARGTERM | ARGLAST);
			    Write_Char(out, ')');
			}
			else
			{
			    Write_Prefix(idwrite, out, d, flags & ARGLIST,
					 module, mod_tag, sd);
			    Pwrite(idwrite, out, arg->val, arg->tag,
				    prec, depth - 1, module, mod_tag,
				    sd, flags & (ARGTERM | ARGLIST | ARGLAST)
				    | ARGOP |
				    ( sd->options & BLANK_AFTER_SIGN && (
					d == d_.minus1 ||
					d == d_.plus1 && !(sd->options & PLUS_IS_NO_SIGN))
				    ? ARGSIGN : 0 ));
			}
			break;

		    case YF:
			Pwrite(idwrite, out, arg->val, arg->tag,
				prec, depth - 1, module, mod_tag, sd,
				flags & ~ARGLAST & (ARGTERM | ARGLIST | ARGSIGN)
				| ARGYF | ARGOP);
			Write_Postfix(idwrite, out, d, flags & ARGLIST,
				      module, mod_tag, sd);
			break;

		    case XF:
			Pwrite(idwrite, out, arg->val, arg->tag,
				prec - 1, depth - 1, module, mod_tag, sd,
				flags & ~ARGLAST & (ARGTERM | ARGLIST | ARGSIGN)
				| ARGOP);
 			Write_Postfix(idwrite, out, d, flags & ARGLIST,
				      module, mod_tag, sd);
			break;
 		    }
		}
		else	/* arity = 2 */
		{
		    Dereference_(narg);
		    switch (assoc)
		    {
		    case XFX:
		    case XFY:
		    case YFX:
			Pwrite(idwrite, out, arg->val, arg->tag,
				assoc == YFX ? prec : prec - 1,
				depth - 1, module, mod_tag, sd,
				flags & ~ARGLAST & (ARGTERM | ARGLIST | ARGSIGN)
				| ARGOP | (assoc==YFX?ARGYF:0));
			Write_Infix(idwrite, out, d, flags & ARGLIST,
				    module, mod_tag, sd, arg, narg);
			Pwrite(idwrite, out, narg->val, narg->tag,
				assoc == XFY ? prec : prec - 1,
				depth - 1, module, mod_tag, sd,
				flags & (ARGTERM | ARGLIST | ARGLAST)
				| ARGOP);
			break;

		    case FXX:
		    case FXY:
			Write_Prefix(idwrite, out, d, flags & ARGLIST,
				     module, mod_tag, sd);
			Pwrite(idwrite, out, arg->val, arg->tag,
				prec - 1, depth - 1, module, mod_tag, sd,
				flags & ~ARGLAST & (ARGTERM | ARGLIST)
				| ARGOP |
				    ( sd->options & BLANK_AFTER_SIGN && (
					d == d_.minus ||
					d == d_.plus && !(sd->options & PLUS_IS_NO_SIGN))
				    ? ARGSIGN : 0 ));
			Write_Char(out, ' ');
			Pwrite(idwrite, out, narg->val, narg->tag,
				assoc == FXY ? prec : prec - 1,
				depth - 1, module, mod_tag, sd,
				flags & (ARGTERM | ARGLIST | ARGLAST)
				| ARGOP);
			break;
		    }
		}
		if (openpar)
		{
		    Write_Char(out, ')');
		}
		return (PSUCCEED);
	    }
	    /* else do as for a normal functor */
	}

	/* normal functor or we ignore operators */

	Write_Atom(idwrite, out, d, ATOM, flags & ARGLIST, module, mod_tag, sd);

_write_args_:				/* (arg,arity) */
	Write_Char(out, '(');
	if (UseDepth(idwrite) && depth <= 1)
	{
	    /* abbreviate even more: only one ... for all arguments */
	    if ((status = ec_outf(out, "...", 3)) < 0)
	    	return status;
	}
	else if (arity > 0)		/* should always be true */
	{
	    for(;;)
	    {
		pword *narg = arg + 1;
		Dereference_(arg);
		Pwrite(idwrite, out, arg->val, arg->tag, MAXPREC, 
				 depth-1, module, mod_tag, sd, ARGTERM | ARGLAST);
		if (--arity == 0)
		    break;
		Write_Comma(out);
		arg = narg;
	    }
	}
	Write_Char(out, ')');
	break;

    case TLIST:
    case TGRL:		/* a ground list in the compiler */
	Handle_Type_Macro(TCOMP)
	if (idwrite & DOTLIST)
	{
	    d = d_.list;		/* write list in ./2 notation */
	    arg = val.ptr;
	    goto _write_structure_;
	}
	else				/* write list in [ ] notation */
	{
	    pword *tail;
	    if (MacrosAllowed(idwrite) && DidMacro(d_.list))	/* output macros */
	    {
		pword *narg;
		if ((narg = _write_trafo(d_.list, GoalMacro(idwrite),
				    &idwrite, val, tag, module, mod_tag, ec_eng)))
		{
		    val.all = narg->val.all;
		    tag.all = narg->tag.all;
		    idwrite &= ~(WRITE_GOAL|WRITE_CLAUSE);
		    goto _pwrite_;	/* print the transformed term */
		}
	    }
	    idwrite &= ~(WRITE_GOAL|WRITE_CLAUSE);

	    if ((status = ec_outfc(out, '[')) < 0)
		return (status);
	    arg = val.ptr;
	    tail = arg + 1;
	    Dereference_(arg)
	    status = _pwrite1(idwrite, out, arg->val, arg->tag, MAXPREC, 
		     --depth, module, mod_tag, sd, ARGTERM | ARGLIST | ARGLAST, ec_eng);
	    if (status < 0)
		return (status);
	    while (!(UseDepth(idwrite) && depth <= 0))
	    {
		Dereference_(tail);
		switch (TagType(tail->tag))
		{
		case TNIL:
		    break;
		case TLIST:
		    Write_Comma(out);
		    tail = tail->val.ptr;
		    arg = tail++;
		    Dereference_(arg);
		    status = _pwrite1(idwrite, out, arg->val, arg->tag, MAXPREC, 
				    --depth, module, mod_tag, sd,
				    ARGTERM | ARGLIST | ARGLAST, ec_eng);
		    if (status < 0)
			return (status);
		    continue;
		default:
		    if ((status = ec_outfc(out, '|')) < 0)
			return (status);
		    status = _pwrite1(idwrite, out, tail->val, tail->tag, 
				    MAXPREC, --depth, module, mod_tag,
				    sd, ARGTERM | ARGLIST | ARGLAST, ec_eng);
		    if (status < 0)
			return (status);
		    break;
		}
		break;
	    }
	    return (ec_outfc(out, ']'));
	}


/***** EXTENSION SLOT WRITE *****/

    default:
	if (TagType(tag) >= 0 && TagType(tag) <= NTYPES)
	{
	    Handle_Type_Macro(TagType(tag))

	    if (tag_desc[TagType(tag)].numeric
	    	&& UnsignedNumberNeedsBrackets
		&& !_is_signed_number(val, tag))
	    {
		if ((status = ec_outfc(out, '(')) < 0 ||
		    (status = tag_desc[TagType(tag)].write(idwrite & QUOTED, out, val, tag)) < 0 ||
		    (status = ec_outfc(out, ')')) < 0)
			return status;
		return status;
	    }
	    return tag_desc[TagType(tag)].write(idwrite & QUOTED, out, val, tag);
	}
	else
	    p_fprintf(out, "BAD_TERM_0x%" W_MOD "x_0x%" W_MOD "x", val.all, tag.all);
	Succeed_
    }
    return (PSUCCEED);
}


static int
_is_proper_list(pword *list)
{
    if (!IsList(list->tag))
    	return 0;
    for(;;)
    {
	list = list->val.ptr + 1;
	Dereference_(list);
	if (!IsList(list->tag))
	    return IsNil(list->tag);
    }
}



/* CAUTION: this function assumes that list is a proper list! */
static int
_write_args_from_list(int idwrite, stream_id out, pword *list, int depth, dident module, type mod_tag, syntax_desc *sd, int flags, ec_eng_t *ec_eng)
{
    pword *arg;
    int status;
    if (IsNil(list->tag))
	return PSUCCEED;
    if (UseDepth(idwrite) && depth <= 1)
    {
	/* abbreviate even more: only one ... for all arguments */
	Write_Str(out, "...", 3);
    }
    for(;;)
    {
	list = list->val.ptr;
	arg = list++;
	Dereference_(arg);
	Pwrite(idwrite, out, arg->val, arg->tag, MAXPREC, 
		 depth-1, module, mod_tag, sd, ARGTERM | ARGLAST);
	Dereference_(list);
	if (IsList(list->tag))
	{
	    Write_Comma(out);
	    continue;
	}
	return PSUCCEED;
    }
}


static pword *
_write_trafo(dident d, int flags, int *idwrite, value val, type tag, dident module, type mod_tag, ec_eng_t *ec_eng)
{
    int macroflags;
    pword *result, *tr_goal;
    pword	*pw;

    if (d == D_UNKNOWN) {	/* meta attribute */
	pw = TG;
	TG += 3;
	Check_Gc;
	pw[0].val.did = d_print_attributes;
	pw[0].tag.kernel = TDICT;
	pw[1].val.all = val.all;
	pw[1].tag.kernel = tag.kernel;
	pw[2].tag.kernel = TREF;
	pw[2].val.ptr = pw + 2;
	tr_goal = pw;
	macroflags = 0;
	result = pw + 2;
    } else {
	tr_goal = trafo_term(ec_eng, d, TR_WRITE|TR_TOP|flags, module, mod_tag, &macroflags);
	if (tr_goal)
	{
	    TransfTermIn(tr_goal)->val.all = val.all;
	    TransfTermIn(tr_goal)->tag.kernel = tag.kernel;
	    result = TransfTermOut(tr_goal);
	} else
	    return (pword *) 0;
    }

    if (do_trafo(ec_eng, tr_goal) == PSUCCEED)
    {
	Dereference_(result);
	/* to avoid looping, check if something was actually transformed */
	if (result->val.all != val.all || result->tag.all != tag.all) {
	    if (macroflags & TR_PROTECT)
		*idwrite |= NO_MACROS;
	    return result;
	}
    }
    return (pword *) 0;
}


/*
 * Call portray/1,2 on a specified term. Returns 1 iff the call succeeded.
 */
static int
_portray_term(int idwrite, stream_id out, value val, type tag, dident module, type mod_tag, ec_eng_t *ec_eng)
{
    value		v1, v2;
    int			status = PFAIL;
    pword		goal[3];

    v1.ptr = goal;
    v2.did = module;
    if (idwrite & PORTRAY2)
    {
	Make_Atom(&goal[0], d_portray2);
	goal[1] = StreamHandle(out);
	goal[2].tag = tag;
	goal[2].val = val;
	status = sub_emulc_opt(ec_eng, v1, tcomp, v2, mod_tag, GOAL_NOTNOT);
	if (status == PSUCCEED) return 1;
	/* else try portray/1 */
    }
    if (idwrite & PORTRAY1)
    {
	/* compatibility hack for portray/1: temporarily redirect output */
	stream_id saved_output = stream_tid.copy(current_output_);
	if (set_stream(d_.output, stream_tid.copy(out)) != PSUCCEED)
	    return 0;
	Make_Atom(&goal[0], d_portray1);
	goal[1].tag = tag;
	goal[1].val = val;
	status = sub_emulc_opt(ec_eng, v1, tcomp, v2, mod_tag, GOAL_NOTNOT);
	(void) set_stream(d_.output, saved_output);
    }
    return (status == PSUCCEED) ? 1 : 0;
}

/*
 * Try to avoid space printing around some frequent infix operators.
 * Except for comma, make it symmetric.
 *
 * TODO: This should be done very differently. Rather than trying to
 * look ahead to the right hand side argument, we should remember the
 * last character of the operator and lazily insert a space if necessary
 * when we are about to print the first character of the next item.
 */
static int
_write_infix(int idwrite, stream_id out, dident d, int flags, dident module, type mod_tag, syntax_desc *sd, pword *right, int depth)
{
    int		status;
    int		spaces = 0;

    if ((sd->options & DENSE_OUTPUT || idwrite & WRITE_COMPACT) &&  d != d_.comma)
    {
	int last_left, first_right;
	int first = sd->char_class[*DidName(d)];
	int last = sd->char_class[*(DidName(d) + DidLength(d) - 1)];
	last_left = sd->char_class[(unsigned char)StreamLastWritten(out)];
	if (IsNumber(right->tag))
	{
	    if (_is_signed_number(right->val, right->tag))
		first_right = sd->char_class['-'];
	    else
		first_right = N;
	}
	else if (IsAtom(right->tag))
	    first_right = sd->char_class[*(DidName(right->val.did))];
	else
	    first_right = -1;

	if (last_left == first || Alphanum(last_left) && Alphanum(first) ||
	    last == first_right || Alphanum(last) && Alphanum(first_right) ||
	    (!IsNumber(right->tag) && !IsAtom(right->tag) && !IsList(right->tag)))
	{
	    spaces = 1;
	}
    }
    else
    {
	spaces = 1;
    }
    if (spaces && d != d_.comma)
	if ((status = ec_outfc(out, ' ')) < 0)
	    return status;
    if ((status = _write_atom(idwrite, out, d, OPERATOR, flags,
						    module, mod_tag, sd, depth)) < 0)
	return status;
    if (spaces && (d != d_.comma || !(idwrite & WRITE_COMPACT)))
	if ((status = ec_outfc(out, ' ')) < 0)
	    return(status);
    return 0;
}


#define STRING_PLUS	10
/*ARGSUSED*/
static int
_write_string(int idwrite, stream_id out, char *start, word length, int depth)
{
/* It is not obvious what is the best way to avoid long strings
    if (UseDepth(idwrite) && depth > 0 &&
	    length > PrintDepth - depth + STRING_PLUS) {
	length = PrintDepth - depth + STRING_PLUS;
	Write_Str(out, start, (int) length);
	return (ec_outf(out, "...", 3));
    } else
 */
	return ec_outf(out, start, (int) length);
}

/* module argument is meaningful only when ARGOP is set in flag &&
   QUOTED is set in idwrite						*/
static int
_write_atom(int idwrite, stream_id out, dident d, int what, int flag, dident module, type mod_tag, syntax_desc *sd, int depth)
{
    int	    status;
    word    length = DidLength(d);
    char    *name = DidName(d);

    if (DidArity(d) < 0)
    {
	return ec_outfs(out, DidArity(d) == UNUSED_DID_ARITY ?
			    "ILLEGAL_FREED_FUNCTOR" : "ILLEGAL_FUNCTOR");
    }

    if (idwrite & QUOTED)
    {
	dident  d0 = check_did(d, 0);
	int nq = ec_need_quotes(d, sd);

	if (nq == QIDENTIFIER ||
	    nq == COMMA && (what != OPERATOR) ||
	    nq == BAR && (flag & ARGLIST
                || ( (what == OPERATOR && d == d_.bar)
                   ? sd->options & BAR_IS_SEMICOLON
                   : sd->options & BAR_IS_NO_ATOM )) ||
	    nq == EOCL && (what == OPERATOR || (flag & ARGOP)))
	{
		if ((flag & ARGOP)
		    && is_visible_op(d0, module, mod_tag))
		{
			if ( ((status = ec_outfc(out, '(')) < 0)
			   || ((status = _write_quoted(idwrite, out, name, length,
					(char) sd->current_aq_char, sd, depth)) < 0)
			   || ((status = ec_outfc(out, ')')) <0 ))
			{
				return (status);
			}
			else
			{
				return(PSUCCEED);
			}
		}
		else
		{
		    return _write_quoted(idwrite, out, name, length,
					(char) sd->current_aq_char, sd, depth);
		}
	}

	if ((flag & ARGOP)
	    && is_visible_op(d0, module, mod_tag))
	{
		if (((status = ec_outfc(out, '(')) <0)
		    || ((status = ec_outf(out, name, (int) length)) < 0)
		    || ((status = ec_outfc(out, ')')) < 0))
		{
			return(status);
		}
		else
		{
			return(PSUCCEED);
		}
	}
	else
	{
	    return(ec_outf(out, name, (int) length));
	}
   }
   else
   {
	if (!strcmp(name, "|") && (flag &ARGLIST))
	{
	    return _write_quoted(idwrite, out, name, length,
			(char)sd->current_aq_char, sd, depth);
	}
	else
	{
	    return _write_string(idwrite, out, name, length, depth);
	}
   }

}


/*
 *	write a quoted atom or string
 *
 *	If an escape character (usually backslash) is defined,
 *	non printable characters are printed as <escape> <letter>
 *	or (if no special notation exists) as <escape> <octal>.
 *	Moreover, the escape character itself and the current quote
 *	are escaped.
 *	If no escape character is defined, only the current quote is
 *	treated in a special way (doubled) to achieve Cprolog compatibility.
 */
/*ARGSUSED*/
static int
_write_quoted(int idwrite, stream_id out, char *name, word len, char quotechar, syntax_desc *sd, int depth)
{
    int			status;
    int			cut;
    char		c;

/* It is not obvious what is the best way to avoid long strings
    if (UseDepth(idwrite) && depth > 0 && len > PrintDepth - depth + STRING_PLUS) {
	len = PrintDepth - depth + STRING_PLUS;
	cut = 1;
    } else
*/
	cut = 0;
    if ((status = ec_outfc(out, quotechar)))	/* write the left quote		*/
	return status;

    if (sd->current_escape >= 0)	/* there is an escape character */
    {
	while (len-- > 0)
	{
	    switch(c = *name++)
	    {
	    case 0007:
		c = 'a'; break;
	    case 0013:
		c = 'v'; break;
	    case '\b':
		c = 'b'; break;
	    case '\t':
		if (idwrite & DONT_QUOTE_NL)
		{
		    if ((status = ec_outfc(out, c)))
			return status;
		    continue;
		}
		c = 't'; break;
	    case '\n':
		if (idwrite & DONT_QUOTE_NL)
		{
		    if ((status = ec_outfc(out, c)))
			return status;
		    continue;
		}
		c = 'n';
		break;
	    case '\r':
		c = 'r'; break;
	    case '\f':
		c = 'f'; break;
	    default:
		if (c == (char) sd->current_escape  ||  c == quotechar)
		    break;
		else if(c < 32  ||  c >= 127)	/* write escaped octal	*/
		{
		    if ((status = ec_outfc(out, sd->current_escape)))
			return status;
		    if (sd->options & ISO_ESCAPES)
		    {
			if ((status = p_fprintf(out, "%o", c & 0xff)))
			    return status;
			if ((status = ec_outfc(out, sd->current_escape)))
			    return status;
		    }
		    else
			if ((status = p_fprintf(out, "%03o", c & 0xff)))
			    return status;
		}
		else			/* normal printable character	*/
		    if ((status = ec_outfc(out, c)))
			return status;
		continue;
	    }
	    				/* write escaped char	*/
	    if ((status = ec_outfc(out, sd->current_escape)))
		return status;
	    if ((status = ec_outfc(out, c)))
		return status;
	}
    }
    else				/* we have no escape character */
    {
	while (len-- > 0)
	{
	    c = *name++;
	    if (c == quotechar)		/* double an internal quote	*/
		if ((status = ec_outfc(out, c)))
		     return status;
	    if ((status = ec_outfc(out, c)))
		return status;
	}
    }
    if (cut) {
	Write_Str(out, "...", 3);
    }

    return ec_outfc(out, quotechar);	/* write the right quote	*/
}

/*
 * Print the variable.
 * The number is the distance in pwords from the stack origin.
 * The stack is pword-aligned.
 */
static int
_print_var(int idwrite, value v, type t, stream_id str, int depth, dident module, type mod_tag, syntax_desc *sd, ec_eng_t *ec_eng)
{
    int name_printed = 0;
    int slot;

    if (idwrite & VARTERM)
	(void) ec_outf(str, "'_'(\"", 5);

    if (idwrite & VAR_ANON)
    {
	(void) ec_outfc(str, (char) sd->current_ul_char);
    }
    else if (EclGblFlags & STRIP_VARIABLES) /* in the tests, all vars are the same */
    {
	if (IsMeta(t))
	    (void) ec_outf(str, "_m", 2);
	else
	    (void) ec_outf(str, "_g", 2);
	return PSUCCEED;
    }
    else
    {
	/* ISO requires _xxx names */
	if (!(idwrite & VAR_NUMBERS) && !(sd->options & ISO_RESTRICTIONS))
	{
	    switch (TagType(t))
	    {
	    case TMETA:
		if ((slot = meta_index(d_var_name)))
		{
		    pword *t1, *t2;

		    t1 = (v.ptr + 1)->val.ptr + slot;
		    Dereference_(t1);
		    if (IsStructure(t1->tag))
		    {
			t1 = t1->val.ptr;
			if ((t1++)->val.did == d_vname2)
			{/* vname(basename, number) as in var_name.ecl */
			    t2 = t1 + 1;
			    Dereference_(t1);
			    Dereference_(t2);
			    if (IsString(t1->tag) && IsInteger(t2->tag)) 
			    {
				p_fprintf(str, "%s#%" W_MOD "d", StringStart(t1->val), t2->val.nint);
				name_printed = 2; 
			    }
			}
		    }
		}

	    case TNAME:		/* all the named variable types */
	    case TUNIV:
		if (IsNamed(t.kernel) && (name_printed != 2))
		{
		    p_fprintf(str, "%s", DidName(TagDid(t.kernel)));
		    name_printed = 1;
		}
	    }
	}

	if ((idwrite & (VAR_NUMBERS|VAR_NAMENUM) && name_printed != 2)
		|| !name_printed)
	{
	    (void) ec_outfc(str, (char) sd->current_ul_char);
	    switch (TagType(t))
	    {
	    case TVAR_TAG:
		if (B_ORIG < v.ptr && v.ptr <= SP_ORIG) /* local */
		    p_fprintf(str, "l%" W_MOD "d", SP_ORIG - v.ptr);
		else
	    case TNAME:
		if (TG_ORIG <= v.ptr && v.ptr < B_ORIG)	/* global */
		    p_fprintf(str, "%" W_MOD "d", v.ptr - TG_ORIG);
		else			/* heap */
		    p_fprintf(str, "h%" W_MOD "d", v.ptr - B_ORIG);
		break;

	    case TUNIV:
		p_fprintf(str, "%" W_MOD "d", v.ptr - TG_ORIG);
		break;

	    case TMETA:
		p_fprintf(str, "%" W_MOD "d", v.ptr - TG_ORIG);
		break;

	    default:
		p_fprintf(str, "BAD_VAR_0x%" W_MOD "x_0x%" W_MOD "x", v.all, t.all);
		break;
	    }
	}
    }

    /* if it's a non marked metavariable write the metaterm */
    if (IsMeta(t) && (idwrite & (STD_ATTR | ATTRIBUTE)) && !(t.kernel & HIDE_ATTR))
    {
	/* important to mark before printing meta term or
	 * could not write circular metaterms.
	 * mark by changing type to normal variable so that other occurrences
	 * will be printed normally
	 */
	Trail_Tag(v.ptr);

	if (idwrite & STD_ATTR) {
	    pword *pw, *r;
	    pword pw_out;
            (v.ptr)->tag.kernel  |= HIDE_ATTR;
	    (void) ec_outfc(str,'{');
	    pw = MetaTerm(v.ptr);
	    Dereference_(pw);
	    r = TG;
	    TG += ATTR_IO_TERM_SIZE;
	    Check_Gc;
	    TG = transf_meta_out(ec_eng, pw->val, pw->tag, r,
	    	(idwrite & CANONICAL ? D_UNKNOWN : module), &pw_out);
	    (void) _pwrite1(idwrite, str, pw_out.val, pw_out.tag, 1200, depth,
						module, mod_tag, sd, ARGLAST, ec_eng);
	    (void) ec_outfc(str,'}');
	} else {
	    pword *r = _write_trafo(D_UNKNOWN /*META*/, 0,
				&idwrite, v, t, module, mod_tag, ec_eng);
	    (v.ptr)->tag.kernel  |= HIDE_ATTR;
	    if (r) {
		(void) _pwrite1(idwrite, str, r->val, r->tag, 1200, depth,
			    module, mod_tag, sd, ARGLAST, ec_eng);
	    }
	}
    }

    if (idwrite & VARTERM)
	(void) ec_outf(str, "\")", 2);

    return PSUCCEED;
}


/*
 * Convert a float to a Prolog-readable representation.
 * The caller has to provide a large enough buffer.
 * The length of the printed representation is returned.
 * If the precise-flag is set, we make sure that reading back the
 * number will give exactly the same float as before.
 */

static int
_float_to_string(value v, type t, char *buf, int precise)
{
    return _float_to_string_opt(v, t, buf, precise, 0);
}


static int
_float_to_string_opt(value v, type t, char *buf, int precise, int syntax_options)
{
    char aux[32];
    char *s;
    char *bufp = buf;
    int dot_seen = 0;
    int is_nan = 0;
    double f = Dbl(v);

    if (!GoodFloat(f))
    {
	ieee_double nan;
	is_nan = 1;
	nan.as_dbl = f;
	/* change exponent to 0 (+3ff bias), to enable printing as number */
	nan.as_struct.mant1 = (nan.as_struct.mant1 & 0x800FFFFF)|0x3FF00000;
	f = nan.as_dbl;
    }
    if (!finite(f))
    {
	s = f < 0 ? "-1.0Inf" : "1.0Inf";
    }
    else if (f == 0.0)		/* not all sprintf's deal properly with -0.0 */
    {
	s = (1.0/f < 0.0 /* && precise */) ? "-0.0" : "0.0";
    }
    else
    {
	if (IsDouble(t))
	{
	    (void) sprintf(aux, "%.15g", f);  /* try with precise digits only */
	    if (precise && f != atof(aux))
		(void) sprintf(aux, "%.17g", f);/* not exact enough, use more */
	}
	else
	{
	    (void) sprintf(aux, "%.6g", f);   /* try with precise digits only */
	    if (precise && (float) f != (float) atof(aux))
		(void) sprintf(aux, "%.9g", f); /* not exact enough, use more */
	}
	s = aux;
	if (*s == '-')
	    *bufp++ = *s++;		/* copy sign */
	if (*s == '.')
	    *bufp = '0';		/* insert 0 in front of . */
	for (;;)
	{
	    switch (*s)
	    {
	    case 'e':
	    case 'E':
		if (!dot_seen && (syntax_options & FLOAT_NEEDS_POINT))
                {
		    *bufp++ = '.';	/* insert .0 */
		    *bufp++ = '0';
                }
		dot_seen = 1;
		*bufp++ = *s++;
		if (*s == '+' || *s == '-')	/* copy sign if any */
		    *bufp++ = *s++;
		while (*s == '0')	/* remove leading zeros in exponent */
		    ++s;
		if (! *s)		/* but don't lose them all */
		    *bufp++ = '0';
		continue;
	    case '.':
		dot_seen = 1;
		break;
	    case 0:
		if (!dot_seen)
		{
		    *bufp++ = '.';	/* insert .0 */
		    *bufp++ = '0';
		}
		*bufp++ = 0;
		goto _return_;
	    }
	    *bufp++ = *s++;
	}
	/* NOTREACHED */
    }
    while ((*bufp++ = *s++)) {}		/* copy the rest of sprintf result */
_return_:
    if (is_nan) {
	s = "NaN";
	--bufp;
	while ((*bufp++ = *s++)) {}
    }
    return (bufp - buf) - 1;
}

/*ARGSUSED*/
static int
_num_string_size(value v, type t, int quoted)
{
    /* enough space for an integer in base 2 + sign */
    return 8*SIZEOF_WORD + 1;
}

/*ARGSUSED*/
static int
_int_to_string(value v, type t, char *buf, int quoted_or_base)
{
    int base = quoted_or_base < 2 ? 10 : quoted_or_base;
    word number = v.nint;
    word aux = number;
    int	len, pos = 0;
    value vv;

    do	/* count digits */
    {
	++pos;
	aux /= base;
    } while(aux);

    if (number < 0)
    {
        len = pos+1;
	buf[0] = '-';
	buf[len] = '\0';
	if (number == MIN_S_WORD)    /* special case -2^(wordsize-1) */
	{
	  int ch = (number-base) % base;
	  buf[pos--] = (ch < 10) ? ch + '0' : ch + 'a' - 10;
	  number = -(number/base);
        } else
	  number = -number;
    } else
    {
        len = pos;
	buf[pos--] = '\0';
    }
    do
    {
	int ch = number % base;
	buf[pos--] = (ch < 10) ? ch + '0' : ch + 'a' - 10;
	number /= base;
    } while(number);

    return len;
}


static int
_handle_string_size(value v, type t, int quoted_or_base)
{
    if (ExternalClass(v.ptr)->string_size && ExternalData(v.ptr))
	return (ExternalClass(v.ptr)->string_size)(ExternalData(v.ptr), quoted_or_base);
    else
	return 0;
}

static int
_handle_to_string(value v, type t, char *buf, int quoted_or_base)
{
    if (ExternalClass(v.ptr)->to_string && ExternalData(v.ptr))
	return (ExternalClass(v.ptr)->to_string)(ExternalData(v.ptr), buf, quoted_or_base);
    else
	return 0;
}


/*
 *
 * printf_(+Stream, +Format, +List, +Module, 0'%, -ErrFormat, -ErrList, -Res)
 *
 * ErrFormat and ErrList return the remaining data
 * when there was an error (Res != 0)
 */

#define FMT_LEFTALIGN	1
#define FMT_SIGNSPACE	2
#define FMT_SIGNPLUS	4
#define FMT_ZEROFILL	8
#define FMT_UPCASE	16

/*
 * Auxiliary: Print a formatted integer (any type)
 * - flags:     FMT_{LEFTALIGN|SIGNSPACE|SIGNPLUS|ZEROFILL|UPCASE}
 * - radix:     2..36
 * - width:     -1 (default) or 0..N (minimum total number of characters)
 * - precision: -1 (default) or 0..N (number of digits printed)
 */

static int
_printf_integer(stream_id nst, int flags, int radix, word width, word precision, value v, type t)
{
    int res, sign;
    int i, print_len, pad, zero_fill;
    int bufsize = 1 + tag_desc[TagType(t)].string_size(v, t, radix);
    New_Array(char, buf, bufsize);
    char *digits = buf;
    int len = tag_desc[TagType(t)].to_string(v, t, buf, radix);

    if (*digits == '-') {
	sign = '-';
	digits++;
	len--;
    } else {
	sign = (flags & FMT_SIGNPLUS) ? '+' : (flags & FMT_SIGNSPACE) ? ' ': 0;
    }

    if (width < 0) width = 0;
    if (flags & FMT_LEFTALIGN) {
	zero_fill = (precision > len) ? precision-len : 0;
	pad = width - (sign?1:0) - zero_fill - len;
    } else {
	if (precision >= 0) {
	    zero_fill = (precision > len) ? precision-len : 0;
	    pad = width - (sign?1:0) - zero_fill - len;
	    while (pad-- > 0)
		if ((res = ec_outfc(nst, ' ')) < 0) return res;
	} else if (flags & FMT_ZEROFILL) {
	    zero_fill = width - (sign?1:0) - len;
	    pad = 0;
	} else {
	    zero_fill = 0;
	    pad = width - (sign?1:0) - len;
	    while (pad-- > 0)
		if ((res = ec_outfc(nst, ' ')) < 0) return res;
	}
    }
    if (sign) {
	if ((res = ec_outfc(nst, sign)) < 0) return res;
    }
    while (zero_fill-- > 0) {
	if ((res = ec_outfc(nst, '0')) < 0) return res;
    }
    if (flags & FMT_UPCASE) {
	for (res=0,i=0; res==0 && i<len; ++i)
	    res = ec_outfc(nst, toupper(digits[i]));
    } else {
	res = ec_outf(nst, digits, len);
    }
    if (res < 0) return res;
    if (flags & FMT_LEFTALIGN) {
	while (pad-- > 0)
	    if ((res = ec_outfc(nst, ' ')) < 0) return res;
    }
    Delete_Array(char, buf, bufsize);
    return PSUCCEED;
}


/*
 * Auxiliary: Print a formatted string or atom
 * - flags:     FMT_{LEFTALIGN|UPCASE}
 * - width:     -1 (default) or 0..N (minimum total number of characters)
 * - precision: -1 (default) or 0..N (number of characters printed)
 */

static int
_printf_string(stream_id nst, int flags, word width, word precision, value v, type t)
{
    word len, pad;
    char *s;
    int res;

    if (IsString(t)) {
	len = (int) StringLength(v);
	s = StringStart(v);
    } else if (IsAtom(t)) {
	len = (int) DidLength(v.did);
	s = DidName(v.did);
    } else if (IsNil(t)) {
	len = (int) DidLength(d_.nil);
	s = DidName(d_.nil);
    } else if (IsRef(t)) {
	return INSTANTIATION_FAULT;
    } else {
	return TYPE_ERROR;
    }
    if (precision < 0 || len < precision)
	precision = len;
    pad = 0;
    if (width < 0)
	width = 0;
    else if (width > precision)
	pad = width - precision;

    if (pad && !(flags & FMT_LEFTALIGN)) {
	for (; width > precision; --width)
	    if ((res = ec_outfc(nst, ' ')) < 0) return res;
    }
    if (flags & FMT_UPCASE) {
	for (res=0, len=precision; res==0 && len--; ++s)
	    if ((res = ec_outfc(nst, toupper(*s))) < 0) return res;
    } else {
	if ((res = ec_outf(nst, s, precision)) < 0) return res;
    }
    if (pad && (flags & FMT_LEFTALIGN)) {
	for (; width > precision; --width)
	    if ((res = ec_outfc(nst, ' ')) < 0) return res;
    }
    return PSUCCEED;
}


/*
 * CAUTION: p_printf5() uses a special error return mechanism in order to
 * deal better with errors that occur halfway through the format string.
 * It always succeeds and returns:
 *	the return/error code in verr/terr
 *	the remaining format string in vse/tse
 *	the remaining argument list in vle/tle
 * Bip_Error() is therefore temporarily redefined during p_printf5()
 * and changed back later!!!
 */

#undef Bip_Error
#define Bip_Error(N) Printf_Error(N)
#define Printf_Error(N) { res = N; goto _return_res_; }

static int
p_printf5(value vs, type ts, value strval, type strtag, value lval, type ltag, value vm, type tm, value vfc, type tfc, value vse, type tse, value vle, type tle, value verr, type terr, ec_eng_t *ec_eng)
{
    char 	formstrt = vfc.nint;
    char 	*format, *par;
    int 	success_code = PSUCCEED;
    int 	res;
    stream_id 	nst;
    word 	format_len;
    pword	my_list[2];
    pword	*old_tg = TG;
    pword	*list;
    pword	*elem;
    char	*last_format = NULL;
    pword	*last_list;

    Get_Locked_Stream(vs, ts, SWRITE, nst);
    if (IsString(strtag)) {
	format = StringStart(strval);
	format_len = StringLength(strval);
    } else if (IsAtom(strtag)) {
	format = DidName(strval.did);
	format_len = DidLength(strval.did);
    } else if (IsNil(strtag)) {
	format = DidName(d_.nil);
	format_len = DidLength(d_.nil);
    } else if (IsRef(strtag)) {
	Bip_Error(INSTANTIATION_FAULT)
    } else {
	Bip_Error(TYPE_ERROR)
    }
    Check_Module(tm, vm);
    Check_Integer(tfc);

    if ((StreamMode(nst) & STYPE) == SNULL)
	goto _return_succ_;

    if (IsNil(ltag))
	list = 0;
    else if (!IsList(ltag))
    {
	my_list[0].tag = ltag;
	my_list[0].val = lval;
	my_list[1].tag.kernel = TNIL;
	list = &my_list[0];
    }
    else
	list = lval.ptr;

    /* reserve a buffer big enough for copying the whole format string */
    Push_Buffer(format_len+1);
    par = (char*) BufferStart(old_tg);
    par[0] = '%';	/* here we build up the format string for C printf */

    last_list = list;
    last_format = format;

    for (; *format; last_format = ++format, last_list = list)
    {
	if (*format != formstrt)
        {
	    if ((res = ec_outfc(nst, *format)) < 0)
	        goto _return_res_;
	}
        else                    /* within control sequence */
	{       
	    int flags = 0;	/* flags characters in "-+0 #" */
	    int asterisk = 0;	/* number of '*' in format string */
	    int ells = 0;	/* number of 'l' */
	    int mods = 0;	/* number of modifiers */
	    word width = -1;	/* indicates default */
	    word precision = -1; /* indicates default */
	    int	radix;
	    int ch;		/* last character parsed */
	    int c;
	    char *modifiers;	/* points behind last number */
	    char *cpar = &par[0]; /* copy destination */

#define NextCh ch = *(++cpar) = *(++format)

	    /* Now parse [flags][width][.precision][modifiers]<fmt> */

	    /* flags */
	    modifiers = cpar+1;
	    for(NextCh;;NextCh)
	    {
		switch(ch) {
		case '-': flags |= FMT_LEFTALIGN; continue;
		case '+': flags |= FMT_SIGNPLUS;  continue;
		case '0': flags |= FMT_ZEROFILL;  modifiers=cpar+1; continue;
		case ' ': flags |= FMT_SIGNSPACE; continue;
		case '#': continue;
		}
		break;
	    }

	    /* width */
	    if (ch == '*') {
		++asterisk;
		Next_Element(elem, list, Printf_Error)
		Check_Integer(elem->tag)
		width = elem->val.nint;
		NextCh;
		modifiers = cpar;
	    } else if ('0' <= ch && ch <= '9') {
		width = 0;
		for(; '0'<=ch && ch<='9'; NextCh)
		    width = 10*width + (ch - '0');
		modifiers = cpar;
	    } else if (flags & FMT_ZEROFILL) {
		width = 0;
		flags &= ~FMT_ZEROFILL;
	    }

	    /* precision */
	    if (ch == '.') {
		precision = 0;	/* consider precision as given */
		NextCh;
		if (ch == '*') {
		    ++asterisk;
		    Next_Element(elem, list, Printf_Error)
		    Check_Integer(elem->tag)
		    precision = (int) elem->val.nint;
		    NextCh;
		    modifiers = cpar;
		} else if ('0' <= ch && ch <= '9') {
		    for(; '0'<=ch && ch<='9'; NextCh)
			precision = 10*precision + (ch - '0');
		    modifiers = cpar;
		}
	    }

	    /* modifiers */
	    for(;;NextCh) {
		switch(ch) {
		case 'm':
		case 'v':
		case 'C':
		case 'D':
		case 'F':
		case 'G':
		case 'I':
		case 'K':
		case 'L':
		case 'M':
		case 'N':
		case 'O':
		case 'P':
		case 'Q':
		case 'T':
		case 'U':
		case 'V':
		case '_':
		case '-':	/* caution: also the leftalign char */
		case '.':	/* caution: also the precision intro! */
		    mods++;
		    continue;

		case 'l' :
		    ells++;
		    continue;
		}
		break;
	    }

	    /* actual format character */
	    if (ch == formstrt) {
		if (cpar != &par[1]) {
		    /* something between two %'s */
		    Printf_Error(BAD_FORMAT_STRING);
		} else if ((res = ec_outfc(nst, formstrt)) < 0) {
		    goto _return_res_;
		}
	    } else {
		switch (ch)
		{
/* 
 * free : hjyz BHJSYZ (must be disjoint from flags and modifiers above)
 */
		case 'd' :        /* signed integers  */
		    radix = 10;
		_printf_integer_:
		    /* for compatibility, accept one 'l', but ignore it */
		    if (mods || ells > 1) {
			Printf_Error(BAD_FORMAT_STRING);
		    }
		    Next_Element(elem, list, Printf_Error)
		    Check_Integer_Or_Bignum(elem->tag);
		    res = _printf_integer(nst, flags, radix, width, precision,
		    			elem->val, elem->tag);
		    if (res < 0) goto _return_res_;
		    break;

		case 'R':		/* radix printing */
		    flags |= FMT_UPCASE;
		    /*fall through*/
		case 'r':		/* radix printing */
		    if (precision >= 0) {	/* <width>.<radix> */
			radix = precision;
			precision = -1;
		    } else if (width >= 0) {	/* <radix> */
			radix = width;
			width = -1;
		    } else {			/* default */
			radix = 8;
		    }
		    if (radix < 2 || radix > 'z' - 'a' + 11) {
			Printf_Error(BAD_FORMAT_STRING)
		    }
		    goto _printf_integer_;

		case 'o' :
		case 'u' :
		case 'x' :
		case 'X' :
		    /* allow one 'l' */
		    if (mods || ells > 1) {
			Printf_Error(BAD_FORMAT_STRING);
		    }
		    Next_Element(elem, list, Printf_Error)
		    Check_Integer(elem->tag);
		    *(++cpar) = '\0';
		    switch (asterisk) {
			case 0: res = p_fprintf(nst, par, elem->val.nint); break;
			case 1: res = p_fprintf(nst, par, width, elem->val.nint); break;
			case 2: res = p_fprintf(nst, par, width, precision, elem->val.nint); break;
			default: res = BAD_FORMAT_STRING;
		    }
		    if (res < 0) goto _return_res_;
		    break;

		case 'f' :        /*  floating numbers  */
		case 'e' :
		case 'E' :
		case 'g' :
		    if (mods) {
			Printf_Error(BAD_FORMAT_STRING);
		    }
		    *(++cpar) = '\0';
		    Next_Element(elem, list, Printf_Error)
		    Check_Float(elem->tag);
		    switch (asterisk) {
			case 0: res = p_fprintf(nst, par, Dbl(elem->val)); break;
			case 1: res = p_fprintf(nst, par, width, Dbl(elem->val)); break;
			case 2: res = p_fprintf(nst, par, width, precision, Dbl(elem->val)); break;
			default: res = BAD_FORMAT_STRING;
		    }
		    if (res < 0) goto _return_res_;
		    break;

		case 'n' :		/* newline */
		case 't' :		/* tab */
		case 'c' :		/* single char */
		    if (flags || precision >= 0 || mods) {
			Printf_Error(BAD_FORMAT_STRING)
		    }
		    if (width<0) width = 1;
		    c = '\t';
		    switch (ch)
		    {
		    case 'c':
			Next_Element(elem, list, Printf_Error)
			Check_Integer(elem->tag)
			c = elem->val.nint;
			/*fall through*/
		    case 't':
			while(width--)
			{
			    if ((res = ec_outfc(nst, c) < 0))
				goto _return_res_;
			}
			break;
		    case 'n':
			while (width--)
			{
			    if ((res = ec_newline(nst)) < 0) {
				if (res == YIELD_ON_FLUSH_REQ)
				    success_code = res;
				else
				    goto _return_res_;
			    }
			}
			break;
		    }
		    break;

		case 'A' :	/* write atom or string, upper case */
		    flags |= FMT_UPCASE;
		    /*fall through*/
		case 'a' :	/* write atom or string */
		case 's' :	/* string or atom */
		    if ((flags & ~(FMT_UPCASE|FMT_LEFTALIGN)) || mods) {
			Printf_Error(BAD_FORMAT_STRING)
		    }
		    Next_Element(elem, list, Printf_Error)
		    res = _printf_string(nst, flags, width, precision, elem->val, elem->tag);
		    if (res < 0) goto _return_res_;
		    break;

                case 'w' :        /* 'write' term (ignore stream defaults) */
                case 'W' :        /* 'write' term (use stream defaults) */
		{
		    int mask_clr, mask_set;

		    if (precision > 0 || ells > 0) {
			Printf_Error(BAD_FORMAT_STRING)
		    }
		    if (width<0) width = 0;
		    *cpar = '\0';
		    res = _get_mode_mask(modifiers, &mask_clr, &mask_set);
		    if (res != PSUCCEED) goto _return_res_;

		    if (ch == 'w')
		    	mask_clr = StreamOutputMode(nst);

		    Next_Element(elem, list, Printf_Error)
		    res = ec_pwrite(ec_eng, mask_clr, mask_set, nst, elem->val, elem->tag,
			1200, width, vm.did, tm);
		    if (res < 0) goto _return_res_;
		    break;
		}

                case 'p' :              /* 'print' term  */
		    if (cpar != &par[1])
		    {
			 Printf_Error(BAD_FORMAT_STRING)
                    }
		    Next_Element(elem, list, Printf_Error)
		    res = ec_pwrite(ec_eng, 0, WRITE_OPTIONS_PRINT, nst,
			    elem->val, elem->tag, 1200, 0, vm.did, tm);
		    if (res < 0) goto _return_res_;
		    break;

                case 'q' :              /* 'writeq' term  */
		    if (cpar != &par[1])
		    {
			 Printf_Error(BAD_FORMAT_STRING)
                    }
		    Next_Element(elem, list, Printf_Error)
		    res = ec_pwrite(ec_eng, 0, WRITE_OPTIONS_WRITEQ, nst,
			    elem->val, elem->tag, 1200, 0, vm.did, tm);
		    if (res < 0) goto _return_res_;
		    break;

                case 'k' :              /* 'display' term  */
		    if (cpar != &par[1])
		    {
			 Printf_Error(BAD_FORMAT_STRING)
                    }
		    Next_Element(elem, list, Printf_Error)
		    res = ec_pwrite(ec_eng, 0, WRITE_OPTIONS_DISPLAY, nst,
			    elem->val, elem->tag, 1200, 0, vm.did, tm);
		    if (res < 0) goto _return_res_;
		    break;

                case 'i' :              /* skip term */
		    if (flags || precision >= 0 || mods) {
			Printf_Error(BAD_FORMAT_STRING)
		    }
		    if (width<0) width = 1;
		    while (width--) {
			Next_Element(elem, list, Printf_Error)
		    }
		    break;

		case 'b':		/* flush buffer */
		    if (cpar != &par[1])
		    {
			 Printf_Error(BAD_FORMAT_STRING)
                    }
		    if ((res = ec_flush(nst)) < 0) {
			if (res == YIELD_ON_FLUSH_REQ)
			    success_code = res;
			else
			    goto _return_res_;
		    }
		    break;

		default:
		    Printf_Error(BAD_FORMAT_STRING);
		    break;
                } 
            }
        }
    }
    if (list) {
	Printf_Error(BAD_ARGUMENT_LIST)
    }
    TG = old_tg;	/* pop buffer */
_return_succ_:
    Return_Unify_Integer(verr, terr, success_code)

_return_res_:
    {
	value	fv;
	Prepare_Requests;

	TG = old_tg;	/* pop buffer */
	if (last_format)
	{
	    /* compute the "remaining" format string and list */
	    Cstring_To_Prolog(last_format, fv);
	    Request_Unify_String(vse, tse, fv.ptr);
	    if (last_list == 0) {
		Request_Unify_Nil(vle, tle);
	    } else if (last_list == &my_list[0]) {
		Request_Unify_Pw(vle, tle, my_list[0].val, my_list[0].tag);
	    } else {
		Request_Unify_List(vle, tle, last_list);
	    }
	}
	else
	{
	    Request_Unify_Pw(vse, tse, strval, strtag);
	    Request_Unify_Pw(vle, tle, lval, ltag);
	}
	/* override the error generated by Check_<type> macros */
	if (res == ARITH_TYPE_ERROR) res = TYPE_ERROR;
	Request_Unify_Integer(verr, terr, -res)
	Return_Unify;
    }
}

/* define Bip_Error() back to Bip_Error_Fail() */
#undef Bip_Error
#define Bip_Error(N) Bip_Error_Fail(N)


/*
 * get/set output_mode_mask (as integer)
 */
static int
p_output_mode_mask(value v, type t, ec_eng_t *ec_eng)
{
    if (IsRef(t)) {
	Return_Unify_Integer(v, t, output_mode_mask);
    } else {
	Check_Integer(t);
	if (v.nint & WRITE_GOAL) {	/* must not be set */
	    Bip_Error(RANGE_ERROR)
	}
	output_mode_mask = v.nint;
	Succeed_;
    }
}

/*
 * get/set output_mode_mask (as string)
 */
static int
p_output_mode(value val, type tag, ec_eng_t *ec_eng)
{
    if (IsRef(tag))
    {
	value	sv;
	char	s[OUTPUT_MODES+1];

	_output_mode_string(s, output_mode_mask);
	Cstring_To_Prolog(s, sv);
	Return_Unify_String(val, tag, sv.ptr);
    }
    else
    {
	char	*new_output_mode;
	int	mask, mask_clr;
	int	res;

	Get_Name(val, tag, new_output_mode);
	if ((res = _get_mode_mask(new_output_mode, &mask_clr, &mask)) != PSUCCEED) {
	    Bip_Error(res)
	}
	if (mask_clr) {			/* not supported here */
	    Bip_Error(RANGE_ERROR)
	}
	if (mask & WRITE_GOAL) {	/* must not be set */
	    Bip_Error(RANGE_ERROR)
	}
	output_mode_mask = mask;
	Succeed_;
    }
}

static void
_output_mode_string(char *s, int mask)
{
    int		i = 0, j;

    for (j=0; j<OUTPUT_MODES; j++)
    {
	if (mask & 1<<j)
	    s[i++] = output_mode_chars[j];
    }
    s[i] = '\0';
}


/*
 * _get_mode_mask() to decode a printf %w format string:
 *
 *	characters must be those in output_mode_chars[]
 *	options can be negated by prefixing a - sign
 *	returns one bit mask with bits to clear, and one with bits to set
 * 
 */

#define MoreThanOneBitSet(n) ((n) & ((n)-1))	/* cute 2's complement trick */

static int
_get_mode_mask(char *string, int *clr_mask, int *mask)
{
    char	c;
    char	*p;
    int		negative = 0;
    int		bit;

    *mask = *clr_mask = 0;
    for (; (c = *string); ++string)
    {
	if (c == '-')
	{
	    negative = 1;
	    continue;
	}
	if ((p = strchr(output_mode_chars, c)))
	    bit = 1 << (p - output_mode_chars);
	else
	    return(RANGE_ERROR);
	if (negative)
	{
	    negative = 0;
	    *clr_mask |= bit;
	}
	else
	{
	    *mask |= bit;
	}
    }

    /* Don't allow setting more than one of the mutually exclusive options */
    if (MoreThanOneBitSet(*mask & (VAR_NUMBERS|VAR_ANON|VAR_NAMENUM))
     || MoreThanOneBitSet(*mask & (STD_ATTR|ATTRIBUTE)))
    {
	return BAD_FORMAT_STRING;
    }
    return PSUCCEED;
}

static int
_merge_output_modes(int mask, int remove, int add)
{
    mask &= ~remove;
    /* if any of the one-of-several-bits options is added, clear bits first */
    if (add & (VAR_NUMBERS|VAR_ANON|VAR_NAMENUM))
    	mask &= ~(VAR_NUMBERS|VAR_ANON|VAR_NAMENUM);
    if (add & (STD_ATTR|ATTRIBUTE))
    	mask &= ~(STD_ATTR|ATTRIBUTE);
    return mask | add;
}


/* A Function to be used in the debugger */
void
writeq_term(uword val, uword tag)
{
    value	v;
    type	t;
    value	vm;

    v.all = val;
    t.kernel = tag;
    vm.did = d_.dummy_module;

    (void) p_writeq(v, t, vm, tdict, default_eng);
    ec_flush(current_output_);
    (void) ec_newline(current_output_);
}


/*
 * write_term(+Stream, +Term, +ClrOptions, +SetOptions, +Depth, +Precedence, +Module)
 *
 * Depth=0	use stream's/global default setting
 */
static int
p_write_term(value vs, type ts, value val, type tag, value vcm, type tcm,
	value vsm, type tsm, value vdepth, type tdepth,
	value vprec, type tprec, value vm, type tm, ec_eng_t *ec_eng)
{
    stream_id	out;

    Get_Locked_Stream(vs, ts, SWRITE, out);
    Check_Integer(tcm);
    Check_Integer(tsm);
    Check_Integer(tdepth);
    Check_Integer(tprec);
    if (vprec.nint < 0 || 1200 < vprec.nint) { Bip_Error(RANGE_ERROR); }
    Check_Module(tm, vm);
    return ec_pwrite(ec_eng, vcm.nint, vsm.nint, out, val, tag, vprec.nint, vdepth.nint, vm.did, tm);
}

/* CAUTION: Bip_Error() is redefined to Bip_Error_Fail() ! */



void
write_init(int flags)
{
    d_portray1 = in_dict("portray", 1);
    d_portray2 = in_dict("portray", 2);
    d_dollar_var = in_dict("$VAR", 1);
    d_print_attributes = in_dict("print_attributes", 2);
    d_var_name = in_dict("var_name", 0);
    d_vname2 = in_dict("vname", 2);

    tag_desc[TINT].string_size = _num_string_size;
    tag_desc[TINT].to_string = _int_to_string;
    tag_desc[TDBL].string_size = _num_string_size;
    tag_desc[TDBL].to_string = _float_to_string;
    tag_desc[THANDLE].string_size = _handle_string_size;
    tag_desc[THANDLE].to_string = _handle_to_string;

    if (!(flags & INIT_SHARED))
	return;
    
    PrintDepth = 20;

    (void) exported_built_in(in_dict("write_", 2), p_write, B_SAFE);
    (void) exported_built_in(in_dict("writeq_", 2), p_writeq, B_SAFE);
    (void) exported_built_in(in_dict("print_", 2), p_print, B_SAFE);
    (void) exported_built_in(in_dict("write_canonical_", 2), p_write_canonical, B_SAFE);
    (void) exported_built_in(in_dict("print_", 3), p_print3, B_SAFE);
    (void) exported_built_in(in_dict("printf_", 8), p_printf5, B_SAFE);
    (void) exported_built_in(in_dict("write_", 3), p_write3, B_SAFE);
    (void) local_built_in(in_dict("writeln_body", 3), p_writeln, B_SAFE);
    (void) exported_built_in(in_dict("writeq_", 3), p_writeq3, B_SAFE);
    (void) exported_built_in(in_dict("write_canonical_", 3), p_write_canonical3, B_SAFE);
    (void) exported_built_in(in_dict("write_term", 7), p_write_term, B_SAFE);
    (void) built_in(in_dict("display", 2), p_display, B_SAFE);
    (void) local_built_in(in_dict("output_mode", 1), p_output_mode, B_UNSAFE|U_SIMPLE);
    (void) local_built_in(in_dict("output_mode_mask", 1), p_output_mode_mask, B_UNSAFE|U_SIMPLE);
}

/* Add all new code in front of the initialization function! */
