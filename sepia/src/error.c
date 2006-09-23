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
 * VERSION	$Id: error.c,v 1.1 2006/09/23 01:56:01 snovello Exp $
 */

/*
 * IDENTIFICATION		error.c
 *
 */

/*
 *
 * Errors in SEPIA. This file contains the error messages generated by SEPIA.
 *
 * Author: Micha Meier
 *
 */

#include "config.h"
#include "sepia.h"
#include "types.h"
#include "embed.h"
#include "error.h"
#include "mem.h"
#include "dict.h"
#include "emu_export.h"
#include "io.h"
#include "os_support.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif

char *	ec_error_message[MAX_ERRORS] = {
/* 0 - SUCCEED */
0,
		/*
		 * Argument Types and Values
		 ***************************
		 */
/* Argument Types */
/* 1 */
"general error",				/* PERROR		*/
"term of an unknown type",			/* UNIFY_OVNI		*/
0,
"instantiation fault",				/* INSTANTIATION_FAULT	*/
"type error",					/* TYPE_ERROR		*/
"out of range",					/* RANGE_ERROR	 	*/
"string contains unexpected characters",	/* BAD_FORMAT_STRING	*/
"bad argument list",				/* BAD_ARGUMENT_LIST	*/
0,
/* -10 */
0,						/* was META_META_UNIFY	*/
"unify event",					/* META_TERM_UNIFY	*/
0,						/* was META_TERM	*/
0,						/* was READ_INT_OVERFLOW */
0,						/* was READ_FLOAT_OVERFLOW */
"creating parallel choice point",		/* CREATE_PAR_CHP	*/
"failing to parallel choice point",		/* FAIL_TO_PAR_CHP	*/
"recomputation failed",				/* RECOMP_FAILED	*/
0,
0,
	/* Arithmetic */
/* -20 */
"arithmetic exception",				/* ARITH_EXCEPTION	*/
"undefined arithmetic expression",		/* UNDEF_ARITH		*/
0,						/* was ARITH_FAIL	*/
"comparison trap",				/* COMPARE_TRAP		*/
"number expected",				/* ARITH_TYPE_ERROR	*/
"integer overflow",				/* INTEGER_OVERFLOW	*/
0,
0,
0,
0,
/* Environment */
/* -30 */
"trying to write a read-only flag",		/* READONLY_FLAG	*/
"arity limit exceeded",				/* ARITY_LIMIT		*/
"no handler for event",				/* EVENT_IGNORED	*/
"event queue overflow",				/* EVENT_QUEUE_FULL	*/
0,
0,
0,
0,
0,
0,
		/*
		 * Data and Memory Areas, Predicates, Modules, Operators
		 *******************************************************
		 */
/* Records, Arrays, Operators */
/* -40 */
"stale object handle",				/* STALE_HANDLE		*/
"array or global variable does not exist",	/* NOGLOBAL		*/
"redefining an existing array",			/* ARRAY_EXISTS		*/
"multiple definition postfix/infix",		/* MULT_POST_INF	*/
"record already exists",			/* LOCAL_REC		*/
"record does not exist",			/* NO_LOCAL_REC		*/
0,
0,
0,
0,
/* Memory, Heaps, Dictionary */
/* -50 */
"trying to modify a read-only ground term",	/* GROUND_CONST_MODIFY	*/
0,
0,
0,
0,
0,
0,
0,
0,
0,
	/* Predicates, Procedure Table, operators */
/* -60 */
"referring to an undefined procedure",		/* NOENTRY		*/
"inconsistent tool redefinition",		/* TOOL_REDEF		*/
"inconsistent procedure redefinition",		/* INCONSISTENCY	*/
"procedure not dynamic",			/* NOT_DYNAMIC		*/
"procedure already dynamic",
"procedure already defined",
"trying to modify a system predicate",		/* REDEF_SYS		*/
"procedure is not yet loaded",			/* NOT_LOADED		*/
"calling an undefined procedure",		/* CALLING_UNDEFINED	*/
"autoload event",				/* CALLING_AUTOLOAD	*/
/* -70 */
"accessing an undefined dynamic procedure",	/* ACCESSING_UNDEF_DYN_PROC */
"procedure already parallel",			/* ALREADY_PARALLEL	*/
"accessing an undefined operator",		/* UNDEF_OPERATOR	*/
"redefining an existing operator",		/* REDEF_OPERATOR	*/
"hiding an existing global operator",		/* HIDING_OPERATOR	*/
"referring to a deprecated predicate",		/* 			*/
"predicate declared but not defined",
"predicate used but not declared or defined",
"calling a procedure with a reserved name",
0,
/* Modules */
/* -80 */
"not a module",					/* MODULENAME		*/
"module/1 can appear only as a directive",	/* DIRECTIVE		*/
"trying to access a locked module",		/* LOCKED		*/
"creating a new module",			/*                      */
"referring to non-exported predicate",		/* MISSING_EXPORT_WARN	*/
"referring to non-existing module",		/* NO_LOOKUP_MODULE_WARN */
"lookup module does not exist",			/* NO_LOOKUP_MODULE	*/
"attempt to redefine an existing local item",
"attempt to redefine an existing exported item",
"attempt to redefine an already imported item",
/* -90 */
"procedure is already reexported",		/* REEXPORT_EXISTS	*/
"not a tool procedure",				/* NO_TOOL		*/
"trying to redefine an existing local procedure",	/* LOCAL_EXISTS		*/
"trying to redefine an existing exported procedure",	/* EXPORT_EXISTS	*/
"trying to redefine an existing imported procedure",	/* IMPORT_EXISTS	*/
0,						/* was GLOBAL_EXISTS	*/
"ambiguous import",				/* IMPORT_CLASH_RESOLVE	*/
"module already exists",			/* MODULE_EXISTS 	*/
"key not correct",				/* WRONG_UNLOCK_STRING	*/
"unresolved ambiguous import",			/* IMPORT_CLASH		*/
/* Spare */
/* -100 */
"accessing a procedure defined in another module",/* ACCESSING_NON_LOCAL*/
"trying to erase a module from itself",		/* SELF_DESTRUCTION	*/
0,
0,
0,
0,
0,
0,
0,
0,
		/*
		 * Run-Time System, Compilation, Execution, Top-Level
		 ****************************************************
		 */
/* Syntax */
/* -110 */
"syntax error: ",				/* was AMBIGUITY	*/
"syntax error: list tail ended improperly",	/* PUNCTUATION		*/
"syntax error: illegal character in a quoted token",	/* ILL_QUOTED	*/
"syntax error: unexpected comma",		/* UNEXCOMMA		*/
"syntax error: unexpected token",		/* UNEXPECTED		*/
"syntax error: unexpected end of file",		/* ENDOFFILE		*/
"syntax error: numeric constant out of range",	/* BAD_NUMERIC_CONSTANT	*/
"syntax error: bracket necessary",		/* BRACKET		*/
"syntax error: unexpected fullstop",		/* ENDOFCLAUSE		*/
"syntax error: postfix/infix operator expected",/* POSTINF		*/
/* -120 */
"syntax error: wrong solo char",		/* SOLOCH		*/
"syntax error: space between functor and open bracket",	 /* BLANK	*/
"syntax error: variable with multiple attributes",	/* MULTI_META	*/
"illegal iteration specifier in do-loop",
"syntax error : prefix operator followed by infix operator",	/* PREFINF */
"syntax error : unexpected closing bracket",		/* UNCLOSBR	*/
"syntax error : grammar rule head is not valid",	/* GRAMMAR_HEAD */
"syntax error : grammar rule body is not valid",	/* GRAMMAR_BODY */
"syntax error : in source transformation",		/* TRANS_ERROR */
"syntax error: source transformation floundered",	/* QUERY_FLOUNDERED */
/* Compilation */
/* -130 */
"syntax error: illegal head",			/* ILLEGAL_HEAD		*/
"syntax error: illegal goal",			/* ILLEGAL_GOAL		*/
"syntax error: term of an unknown type",	/* WRONG_TYPE		*/
"loading the library ",				/* LIBRARY		*/
"procedure clauses are not consecutive",	/* CONSECUTIVE		*/
"trying to redefine a protected procedure",	/* PROTECT_REDEF	*/
"trying to redefine a built-in predicate",	/* BUILT_IN_REDEF	*/
"trying to redefine a procedure with another type",/* INCONSISTENT_REDEF */
"singleton local variable in do-loop",
"compiled or dumped file message",		/* COMPILED_FILE	*/
/* Execution */
/* -140 */
"undefined instruction",			/* UNDEFINED		*/
"unimplemented functionality",			/* UNIMPLEMENTED	*/
"built-in predicate not available on this system",/* NOT_AVAILABLE	*/
"compiled query failed",			/* QUERY_FAILED		*/
"a cut is not allowed in a condition",		/* IF_CUT		*/
"procedure being redefined in another file",	/* MULTIFILE		*/
"start of compilation",
"compilation aborted",
"bad pragma",					/* BAD_PRAGMA		*/
"code unit loaded",				/* CODE_UNIT_LOADED	*/
/* Top Level Loop Extension Hooks */
/* -150 */
"start of eclipse execution",
"eclipse restart",
"end of eclipse execution",
"toplevel: print prompt",
"toplevel: start of query execution",
"toplevel: print values",
"toplevel: print answer",
"error exit",
"toplevel: entering break level",
"toplevel: leaving break level",
/* Spare */
/* -160 */
"global macro transformation already exists",	/* GLOBAL_TR_EXISTS	*/
"macro transformation already defined in this module",	/* TR_IN_MOD	*/
"no macro transformation defined in this module",	/* NO_TR	*/
"illegal attempt to remove the last member of a character class",	/* ONE_SQ_AQ */
"toplevel: print banner",
"can't compile an attributed variable (use add_attribute/2,3)",	/* ILLEGAL_META */
"file successfully processed",
"initialization/finalization goal failed or aborted",
0,
0,
		/*
		 * I/O, Operating System, External Interface
		 *******************************************
		 */
/* Operating System, Files */
/* -170 */
"system interface error",			/* SYS_ERROR		*/
"File does not exist : ",			/* NOFILE		*/
"File is not open :",				/* FILE_NOT_OPEN	*/
"library not found",				/* NOLIB		*/
"child process terminated due to signal",
"child process stopped",
"message passing error",			/* MPS_ERROR		*/
"shared library not found",			/* NO_SHARED_LIB	*/
0,
0,
/* -180 */
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
/* I/O, Streams */
/* -190 */
"end of file reached",				/* PEOF			*/
"output error",					/* OUT_ERROR		*/
"illegal stream mode",				/* STREAM_MODE		*/
"illegal stream specification",			/* STREAM_SPEC		*/
"too many symbolic names of a stream",		/* TOO_MANY_NAMES	*/
"yield on flush",				/* YIELD_ON_FLUSH_REQ	*/
"trying to modify a system stream",		/* SYSTEM_STREAM	*/
"use 'input' or 'output' instead of 'user'",	/* INCORRECT_USER	*/
"reading past the file end",			/* READ_PAST_EOF	*/
0,
/* -200 */
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
/* Externals */
/* -210 */
"Remember() not inside a backtracking predicate",/* REMEMBER		*/
"External function does not exist",		/* NOCODE		*/
"External function returned invalid code",	/* ILLEGAL_RETURN	*/
"Error in external function",			/* EC_EXTERNAL_ERROR	*/
"Licensing problem",				/* EC_LICENSE_ERROR	*/
0,
0,
0,
0,
0,
/* IC specific */
/* -220 */ 
"Unrepresentable domain exclusion on IC variable",	/* IC_EXCLUSION_ERROR */
0, 
0,
0, 
0, 
0, 
0, 
0, 
0, 
0,
		/*
		 * Advanced Features, Extensions, Debugging
		 ******************************************
		 */
/* Events */
/* -230 */
"uncaught exception",
"default help/0 message",
0,
0,
0,
0,
0,
0,
0,
0,
/* -240 */
0,
0,
0,
0,
0,
0,
0,
0,
0,
"debugger new suspensions event",
/* Debugging */
/* -250 */ 
"debugger init event", 
"debugger builtin fail event",
"debugger port event",
"debugger call event",
"debugger exit event",
"debugger redo event",
"debugger delay event",
"debugger wake event",
"debugger builtin call event",
"debugger builtin exit event",
/* Save and restore */
/* -260 */
"unexpected end of file",			/* UNEXPECTED_EOF	*/
"invalid saved state",				/* INVALID_SS		*/
"can not allocate required space",		/* CANT_ALLOCATE	*/
"can not save or restore from another break level than level 0",/*WRONG_LEVEL*/
"not an eclipse object file ",			/* NOT_DUMP_FILE	*/
"bad eclipse object file version ",		/* BAD_DUMP_VERSION	*/
0,
"predicate not implemented in this version",	/* NOT_IMPLEMENTED	*/
"predicate not supported in parallel session",	/* NOT_IN_PARALLEL	*/
0,
/* Coroutining */
/* -270 */
"undefined variable attribute",			/* UNDEF_ATTR		*/
"bad format of the variable attribute",		/* ATTR_FORMAT		*/
"delay clause may cause indefinite delay",
"delayed goals left",				/* LEFT_DELAYED_GOAL	*/
"stack of woken lists empty",	 		/* BAD_RESTORE_WL	*/
0,					 	/* was DELAY_SIMPLE	*/
0,
0,
0, 
0,
/* -280 */
"Found a solution with cost ", 			/* min_max message	*/
0, 
0,
0, 
0, 
0, 
0, 
0, 
0, 
0,
/* -290 */
0, 
0, 
0,
0, 
0, 
0, 
0, 
0, 
0, 
0,
/* MegaLog events */
/* -300 */
"Bad attribute specification",
"Bad data given for attribute",
"Database not open",
"Relation does not exist",
"Bad condition tree",
"Bad projection list",
"Relation already exists",
"Cannot create relation",
"Illegal output relation",
"Internal error",
/* -310 */
"Cannot read and write a relation at the same time\nA tuple retrieval can be closed by a cut afterwards\n",
"Cannot close DB, there are still open relations\nInvoke \"bang_free_cursor/0\" to close tuple retrieval\n",
"Updates only apply to new state",
"Temporary relation has no old state",
"Schema modification requires single user system",
"Operation illegal inside transaction",
"Operation illegal outside of transaction",
"Transaction abort due to deadlock",  
/* IMPORTANT NOTE : The handler must do exit_block(abort_transaction) silent */
"Out of global stack",
"Out of local stack (term buffer space)",
/* -320 */ 
"Too many deadlocks in sequence -- abort transaction",
"Bad attribute in projection list",
"Bad comparison in where-expression",
"Read/Write error on bang.log",
0,
0,
0,
0,
"Database operation not possible inside interrupt handler",
"MegaLog related parameter dropped -- use set_flag/2 instead",
/* -330 */
"KEGI general error", 			/* KEGI */
"KEGI system error", 			/* KEGI */
"SEDUCE error",				/* SEDUCE */
"window event", 			/* X event */
0, 
0, 
0, 
0, 
0, 
0,
		/*
		 * User-Defined Events
		 *********************
		 */
/* -340 = USER_ERROR */
0, 
0, 
0,
0, 
0, 
0, 
0, 
0, 
0, 
0,
/* -350 */
0, 
0, 
0,
0, 
0, 
0, 
0, 
0, 
0, 
0,
/* -360 */
0, 
0, 
0,
0, 
0, 
0, 
0, 
0, 
0, 
0,
/* -370 = MAX_ERRORS */
};


/*
 * 	error_id(+Number, ?Message)
 *
 *		Returns the appropriate error message. Fails if the
 *		message string is empty or out of range, so that it
 *		can be used to check whether the given error exists.
 */
static int
p_error_id(value valn, type tagn, value vale, type tage)
{
    Error_If_Ref(tagn);
    Check_Output_String(tage);
    if (IsInteger(tagn))
    {
	if
	(
		valn.nint < 1
		||
		valn.nint >= MAX_ERRORS
		||
		!ErrorMessage[valn.nint]
	)
	{
		Fail_;
	}
	{
	    value v;
	    Cstring_To_Prolog(ErrorMessage[valn.nint], v);
	    Return_Unify_String(vale, tage, v.ptr);
	}
    }
    else if (IsAtom(tagn))
    {
	Return_Unify_String(vale, tage, DidString(valn.did));
    }
    else
    {
	Bip_Error(TYPE_ERROR);
    }
}

char * Winapi
ec_error_string(int n)
{
    n = -n;
    if (n < 1 || n >= MAX_ERRORS)
    	return ErrorMessage[n];
    return (char *) 0;
}

static int
p_get_last_errno(value v, type t)
{
    Return_Unify_Integer(v, t, ec_os_errno_);
}

static int
p_set_last_errno(value v, type t)
{
    Check_Integer(t);
    ec_os_errno_ = v.nint;
    ec_os_errgrp_ = ERRNO_UNIX;
    Succeed_;
}

static int
p_max_error(value val1, type tag1)
{
	Check_Output_Integer(tag1);
	Return_Unify_Integer(val1, tag1, MAX_ERRORS - 1);
}


static int
p_errno_id(value eval, type etag, value sval, type stag)
{
    pword	pw;
    char	buf[1024];

    Check_Integer(etag);
    Check_Output_String(stag);

    Make_String(&pw, ec_os_err_string(eval.nint, ec_os_errgrp_, buf, 1024));
    Return_Unify_Pw(sval, stag, pw.val, pw.tag);
}

static int
p_errno_id1(value sval, type stag)
{
    pword	pw;
    char	buf[1024];

    Check_Output_String(stag);

    Make_String(&pw, ec_os_err_string(ec_os_errno_, ec_os_errgrp_, buf, 1024));
    Return_Unify_Pw(sval, stag, pw.val, pw.tag);
}

void
error_init(int flags)
{
    if (flags & INIT_SHARED)
    {
	int i;
	/*
	 * Due to the user entries, part of the message array is
	 * mutable and must be in shared memory.
	 */
	ErrorMessage = (char **) hg_alloc_size(sizeof(char *) * MAX_ERRORS);
	for(i=0; i<MAX_ERRORS; i++)
	{
	    ErrorMessage[i] = ec_error_message[i];
	}
	(void) built_in(in_dict("error_id", 2), p_error_id, B_UNSAFE|U_SIMPLE);
	(void) local_built_in(in_dict("max_error", 1), p_max_error, B_UNSAFE|U_SIMPLE);
	(void) local_built_in(in_dict("set_last_errno", 1), p_set_last_errno, B_SAFE);
	(void) local_built_in(in_dict("get_last_errno", 1), p_get_last_errno, B_UNSAFE|U_SIMPLE);
	(void) built_in(in_dict("errno_id", 1), 	p_errno_id1,	B_UNSAFE|U_SIMPLE);
	(void) built_in(in_dict("errno_id", 2), 	p_errno_id,	B_UNSAFE|U_SIMPLE);
    }
}


void
default_panic(const char *what, const char *where)
{
#define MAX_PANIC_BUF 200
    char buf[MAX_PANIC_BUF];
    if (strlen(what) + 4 + strlen(where) < MAX_PANIC_BUF)
	strcat(strcat(strcpy(buf, what), " in "), where);
    else
	strncpy(buf, what, MAX_PANIC_BUF-1)[MAX_PANIC_BUF] = 0;
    ec_bad_exit(buf);
}

void
ec_panic(const char *what, const char *where)
{
    (*ec_options.user_panic)(what,where);
}
