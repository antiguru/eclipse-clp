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
 * VERSION	$Id: sav_res.h,v 1.1 2006/09/23 01:55:04 snovello Exp $
 */

/*
 * IDENTIFICATION		sav_res.h
 */

#define PROG_AND_DATA	0	/* non reallocable saved state that
				   saved the program the stacks and
				   the abstract machine registers    */
#define PROG_ONLY	1	/* non reallocable saved state that
				   save the program state only	     */
#define REALLOCABLE	2	/* reallocable saved state that only
				   saves the heap (completely located
				   before the prolog stacks)	     */
#define NO_SAVED_STATE	3	/* not a saved state		     */
#define SAVED_CORE	4	/* a saved core without saved state  */
