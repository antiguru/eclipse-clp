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
 * Copyright (C) 1997-2006 Cisco Systems, Inc.  All Rights Reserved.
 * 
 * Contributor(s): 
 * 
 * END LICENSE BLOCK */

/*
 * ECLiPSe LIBRARY MODULE
 *
 * $Id: eg_cc_yield.cc,v 1.1 2006/09/23 01:55:54 snovello Exp $
 *
 *
 * IDENTIFICATION:	cc_yield.c
 *
 * AUTHOR:		Joachim Schimpf
 * AUTHOR:		Stefano Novello
 *
 * CONTENTS:		name/arity
 *
 * DESCRIPTION:
 *	Example of minimal main using external embed interface.
 */

#include	"eclipseclass.h"
#include	<iostream.h>


main(int argc,char ** argv)
{
    ec_init();
    EC_ref X_or_Cut;
    int res;
    long x;

    post_goal("between(1,99,1,X), writeln(X),
    		(X>5 ->
		    yield(X, Cont),
		    ( Cont == stop -> exit_block(abort) ; true )
		;
		    true
		)");
    res = EC_resume(X_or_Cut);

    for (;;)
    {
	switch (res)
	{
	    case EC_succeed:
		cout << "succeeded\n";
		post_goal("fail");
		res = EC_resume(X_or_Cut);
		break;

	    case EC_fail:
		cout << "failed\n";
		goto _stop_;

	    case EC_throw:
		cout << "aborted\n";
		post_goal("writeln(new_goal_after_abort)");
		res = EC_resume(X_or_Cut);
		break;

	    case 3:	/* case EC_yield: */
		cout << "yielded\n";
		if (EC_succeed == EC_word(X_or_Cut).is_long(&x) && x>6)
		    res = EC_resume(EC_atom("stop"), X_or_Cut);
		else
		    res = EC_resume(EC_atom("cont"), X_or_Cut);
		break;

	    default:
		cout << "bad return code\n";
		break;
	}
    }

_stop_:
    ec_cleanup(0);
    exit(0);
}

