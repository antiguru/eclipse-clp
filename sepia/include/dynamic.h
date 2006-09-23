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
 * SEPIA INCLUDE FILE
 *
 * VERSION	$Id: dynamic.h,v 1.1 2006/09/23 01:55:01 snovello Exp $
 *
 */

/* NOTE:	Starting point: dynamic.h 21.1
 */

/*
 * IDENTIFICATION		dynamic.h
 *
 * DESCRIPTION	
 *	
 *
 * CONTENTS:
 * 
 * AUTHOR       VERSION  DATE   REASON 
 * periklis             26.9.89 Major revision for the logical update semantics  * Dominique 
 *
 */

/******************************************************************************/
/*		Additions by periklis starting from 14/7/89		      */
/*		Needed for the logical update semantics			      */
/******************************************************************************/

/* given a PriCode, get the start of the asserted clause */
#define StartOfAss(vref)		(vref)

/* Given the start of the code of an asserted clause,
 * gives the start of the code of the corresponding compiled source clause.
 *	vmcode	*vref;
 */
#define StartOfSource(vref)		AuxField(vref)
/* ditto, if it points to the procedure start */
#define StartOfProcSource(vref)		StartOfSource(StartOfAss(vref))

/* given procedure descriptor, set start of the asserted clause */
#define Set_Start_Of_Ass(proc, code)	PriCode(proc) = code;


/* Given the start of the code of a compiled source clause, gives
 * the start of the code of the last asserted clause of the same procedure.
 *      vmcode   *vsrc_ref;
 */
#define StartOfLastClause(vsrc_ref)	AuxField(vsrc_ref)


/* Given the start of the code of a dynamic clause (asserted or source),
 * initialises its dynamic instruction (first instruction in the code) to
 * [Try|Retry]_me_dynamic DynGlobalClock, NOTYET, next, arity
 * Birth is taken to be the value of DynGlobalClock.
 * Death is assumed to be NOTYET.
 * next should be the beginning of the code of the next clause of the procedure
 * or FAIL if there is no next clause.
 * arity should be the arity of the procedure.
 *      vmcode  *vref;
 *	vmcode	inst_op_code,
 *		next,
 *		arity;
 */
#define Init_Dyn_Inst(vref, inst_op_code, next, arity, aux)	\
	DynamicInst(vref)	= Op_Value(inst_op_code);\
	Birth(vref)		= DynGlobalClock;	\
	Death(vref)		= NOTYET;		\
	NextAlternative(vref)	= (next);		\
	Arity(vref)		= (arity);		\
	AuxField(vref)		= (vmcode) (aux);


/* The value of the 'next' field of the [Try|Retry]_me_dynamic instruction of
 * the last clause of a dynamic procedure.
 */
#define FAIL	0


/* Increases the value of DynGlobalClock.
 * Should be done after every assertion or retraction
 * of a clause of a dynamic procedure.
 */
#define Advance_Global_Clock	DynGlobalClock++


/* The value of the death tag of a dynamic clause which has not yet been
 * retracted. Changed to the current value of DynGlobalClock
 * by kill_pair on retraction.
 */
#define NOTYET			MAX_U_WORD


/* True iff start_of_code is the start of the code of the (once only)
 * allocated pair of clauses (asserted and corresponding source) which is used
 * for all dynamic procedures which have no clauses.
 *	vmcode  *start_of_code;
 */
#define IsFailPair(start_of_code) (((vmcode *) StartOfAss(start_of_code)) == (fail_pair_))


/* Given the start of the code of an asserted clause,
 * gives the opcode of its dynamic instruction.
 *      vmcode	*vref; 
 */
#define DynamicInst(vref)	(* ((vmcode *) vref))
 

/* Given the start of the code of an asserted clause, gives its birth stamp.
 *      vmcode	*vref; 
 */
#define Birth(vref)		(* (((vmcode *) vref) + 1))
 

/* Given the start of the code of an asserted clause, gives its death stamp.
 *      vmcode	*vref; 
 */
#define Death(vref)		(* (((vmcode *) vref) + 2))


/* Given the start of the code of an asserted clause,
 * yields TRUE iff the clause is dead
 */
#define Dead(vref, clock) \
    (Birth(vref) >= (clock) || Death(vref) < (clock))


/*Given the start of the code of an asserted clause, gives the next alternative.
 *      vmcode	*vref; 
 */
#define NextAlternative(vref)	(* (((vmcode *) vref) + 3))

/* Given the start of the code of an asserted clause, gives the arity of the 
 * procedure to which it belongs (stored as the last argument of the  
 * [Try|Retry]_me_dynamic instruction. 
 *      vmcode  *vref; 
 */
#define Arity(vref) 		(* (((vmcode *) vref) + 4))

#define AuxField(vsrc_ref)	(* (((vmcode *) vsrc_ref) + 5))

/* The gc_field of the source clauses of dynamic predicates is set to it during 
 * the marking phase of the garbage collection to signify that there were more 
 * than one choice points for this clause with different call clocks. 
 */
#define MULTIPLE_CH_POINTS	MAX_U_WORD

/* Given the start of the code of a compiled source clause, gives
 * the value stored in the garbage collector field.  This is normally zero.
 *      vmcode   *vsrc_ref;
 */
#define GcField(vsrc_ref)	AuxField(vsrc_ref)

/* The most significant bit of the arity field of every dynamic instruction is
 * set when the instruction belongs to a source clause.
 * This is used by the garbage collector to distinguish between source and
 * compiled clauses.  Therefore the true arity is got by anding it with
 * the Src_Clause_Arity_Mask. In addition, the arity of source clauses is 3.
 */
#define SRC_CLAUSE_ARITY_MASK    MAX_S_WORD
#define SRC_CLAUSE_ARITY        (SIGN_BIT | 3)
