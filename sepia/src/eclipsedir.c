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
 * ECLiPSe C SOURCE MODULE
 *
 * $Id: eclipsedir.c,v 1.1 2006/09/23 01:55:54 snovello Exp $
 *
 * Note that this file is used by different executables
 * (eclipse, worker manager)
 *
 */

#include "config.h"
#include "os_support.h"

#ifdef STDC_HEADERS
#include	<stdlib.h>
#endif


extern char	*whereami();

static char	*eclipsehome_;

char *
eclipsehome(void)
{
    if (!eclipsehome_)
    {
	eclipsehome_ = whereami();
	if (!eclipsehome_)
	{
	    char buf[MAX_PATH_LEN];
	    int size=MAX_PATH_LEN;
	    if (ec_env_lookup("ECLIPSEDIR", buf, &size))
	    {
		char buf1[MAX_PATH_LEN];
		(void) canonical_filename(buf, buf1);
		eclipsehome_ = strcpy((char*) malloc(strlen(buf1)+1), buf1);
	    }
	    else
	    {
#ifdef _WIN32
		eclipsehome_ = "//C/Eclipse";
#else
		eclipsehome_ = "/usr/local/eclipse";
#endif
	    }
	}
    }
    return eclipsehome_;
}

