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
 *
 * Simplified standalone main() for Windows
 *
 */

#include	<windows.h>
#include 	"eclipse.h"
#include	"os_support.h"


#include <stdio.h>
#include <errno.h>
#include <setjmp.h>

#ifdef STDC_HEADERS
#include <stdlib.h>
#include <string.h>
#endif

#ifndef ACCESS_IN_UNISTD
#include <sys/file.h>
#endif

#ifdef STRTOL_UNDEF
extern long strtol();
#endif

#define KB			1024
#define MIN_LOCAL		100*KB
#define MIN_GLOBAL		210*KB
#define MIN_PRIVATE		210*KB
#define MIN_SHARED		210*KB

#define MAX_MEMORY	(SIGN_BIT/8*15)	/* 15/16 of the address space */

/*
 * LOCAL declarations
 */

static void main_panic(char *, char *);
static void usage(char *);


/*
 * GLOBAL variable definitions
 */

/*
 * To avoid infinite loops when the memory is corrupted, this flag
 * is set when SEPIA did not manage it to print the prompt and start user query
 * after a reset. 0 means ok, 1 is after a reset and before a query.
 */
static int	memory_corrupted = 0;

jmp_buf         reset;

static uword
sizearg(char *arg)
{
    int last = strlen(arg) -1;	
    uword multiple = 0;
    uword size;

    switch(arg[last])
    {
    case 'k':
    case 'K':
	multiple = KB;
	arg[last] = '\000';
	break;
    case 'm':
    case 'M':
	multiple = KB * KB;
	arg[last] = '\000';
	break;
    case 'g':
    case 'G':
	multiple = KB * KB * KB;
	arg[last] = '\000';
	break;
    case '0': case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8': case '9':
	multiple = KB;
	break;
    default:
    	usage(arg);
    }
    size = (uword) atol(arg);
    return (size > MAX_MEMORY/multiple) ? 0 : size * multiple;
}

main(int argc, char **argv)
{
    char *	initfile = (char *) 0;
    char *	eclipsedir = (char *) 0;
    int		c, new_argc, err;
    int		init_flags = INIT_SHARED|INIT_PRIVATE|INIT_ENGINE|INIT_PROCESS;
    char *	session, * nsrv_hostname;
    unsigned    nsrv_port_number;
    uword	size;

    /*
     * If stdio is not a tty, get rid of the console window. This is not ideal
     * since the window flashes up briefly, but no better solution yet.
     * (The correct way would be not to build eclipse.exe as a "console
     * application" and have a WinMain() instead of main(). But then we have
     * to do all the setup of stdin/out/err, argc/argv, environment etc
     * ourselves)
     */
    if (!isatty(_fileno(stdin))
     && !isatty(_fileno(stdout))
     && !isatty(_fileno(stderr)))
    {
	FreeConsole();
    }
	
    /*
     * collect information from the command line
     * remove some internally used arguments from the command line
     */
    for (c = new_argc = 1; c < argc; )
    {
	if (argv[c][0] == '-' && argv[c][2] == 0)	/* single char opt */
	{
	    switch (argv[c][1])
	    {
	    case 'a':			/* -a <worker> <session> 
                                              <nsrv_hostname> <nsrv_port_no> */
		if (++c + 4 > argc) usage(argv[c-1]);
		ec_set_option_int(EC_OPTION_PARALLEL_WORKER, atoi(argv[c++]));	
		session = argv[c++];
		nsrv_hostname = argv[c++];
		nsrv_port_number = atoi(argv[c++]);
		break;

	    case 'c':				/* -c <shared_map_file> */
		if (++c + 1 > argc) usage(argv[c-1]);
		ec_set_option_ptr(EC_OPTION_MAPFILE, argv[c++]);
		ec_set_option_int(EC_OPTION_ALLOCATION, ALLOC_FIXED);
		init_flags &= ~INIT_SHARED;
		break;

	    case 'm':				/* -m <shared_map_file> */
		if (++c + 1 > argc) usage(argv[c-1]);
		ec_set_option_ptr(EC_OPTION_MAPFILE, argv[c++]);
		ec_set_option_int(EC_OPTION_ALLOCATION, ALLOC_FIXED);
		break;

	    case 'b':				/* -b <bootfile> */
		argv[new_argc++] = argv[c];		/* shift */
		if (++c + 1 > argc) usage(argv[c-1]);
		argv[new_argc++] = argv[c++];		/* shift */
		break;

	    case 'e':				/* -e <goal> */
		argv[new_argc++] = argv[c];		/* shift */
		if (++c + 1 > argc) usage(argv[c-1]);
		argv[new_argc++] = argv[c++];		/* shift */
		break;

	    case 'g':				/* -g <size> */
		argv[new_argc++] = argv[c];		/* shift */
		if (++c + 1 > argc) usage(argv[c-1]);
		argv[new_argc++] = argv[c];		/* shift */
		size = sizearg(argv[c++]);
		ec_set_option_int(EC_OPTION_GLOBALSIZE, size);
		if (size < MIN_GLOBAL) {
		    fprintf(stderr,"Global stack size out of range\n");
		    exit(-1);
		}
		break;

	    case 'd':				/* -d <n> */
		/* delay worker startup by <n> seconds */
		if (++c + 1 > argc) usage(argv[c-1]);
		Sleep(1000 * atoi(argv[c++]));
		break;

	    case 'D':				/* -D <eclipsedir> */
		if (++c + 1 > argc) usage(argv[c-1]);
		eclipsedir = argv[c++];
		break;

#ifdef PRINTAM
	    case 'i':		/* to boot something else than kernel.xyz */
		if (++c + 1 > argc) usage(argv[c-1]);
		initfile = argv[c++];
		break;
#endif
	    case 'l':				/* -l <size> */
		argv[new_argc++] = argv[c];		/* shift */
		if (++c + 1 > argc) usage(argv[c-1]);
		argv[new_argc++] = argv[c];		/* shift */
		size = sizearg(argv[c++]);
		ec_set_option_int(EC_OPTION_LOCALSIZE, size);
		if (size < MIN_LOCAL) {
		    fprintf(stderr,"Local stack size out of range\n");
		    exit(-1);
		}
		break;

	    case 'h':				/* -h <size> */
		argv[new_argc++] = argv[c];		/* shift */
		if (++c + 1 > argc) usage(argv[c-1]);
		argv[new_argc++] = argv[c];		/* shift */
		size = sizearg(argv[c++]);
		ec_set_option_int(EC_OPTION_PRIVATESIZE, size);
		if (size < MIN_PRIVATE) {
		    fprintf(stderr,"Private heap size out of range\n");
		    exit(-1);
		}
		break;

	    case 's':				/* -s <size> */
		argv[new_argc++] = argv[c];		/* shift */
		if (++c + 1 > argc) usage(argv[c-1]);
		argv[new_argc++] = argv[c];		/* shift */
		size = sizearg(argv[c++]);
		ec_set_option_int(EC_OPTION_SHAREDSIZE, size);
		if (size < MIN_SHARED) {
		    fprintf(stderr,"Shared heap size out of range\n");
		    exit(-1);
		}
		break;

	    case 'o':				/* enable oracles */
		c += 1;
		/* vm_options = ORACLES_ENABLED; */
		break;

	    case '-':				/* -- give the rest to Prolog */
		for (; c < argc; )
		    argv[new_argc++] = argv[c++];
		break;

	    default:				/* skip unknown */
		argv[new_argc++] = argv[c++];
		break;
	    }
	}
	else if (!strcmp(argv[c], "-debug_level"))
	{
	    if (++c + 1 > argc) usage(argv[c-1]);
	    ec_set_option_int(EC_OPTION_DEBUG_LEVEL, atoi(argv[c++]));
	}
	else /* ignore/shift bad arguments unless there is a -- option */
	{
	    argv[new_argc++] = argv[c++];
	}
    }

    /*----------------------------------------------------------------
     * Entry point after longjmp(reset)
     *----------------------------------------------------------------*/

    switch (setjmp(reset))
    {
    case 0:		/* raw boot or -r from above */
	break;
    case 3:		/* restore program state */
    case 2:
	init_flags = REINIT_SHARED|INIT_ENGINE|INIT_PRIVATE;
	break;
    case 4:		/* restore execution state */
	init_flags = REINIT_SHARED|INIT_PRIVATE;
	break;
    case 1:		/* reset after fatal error */
    default:
	init_flags = INIT_ENGINE;
	switch (memory_corrupted++)
	{
	    case 0:
		break;

	    case 1:
		/* try to print a message */
		memory_corrupted = 2;
		fprintf(stderr,"\n*** SEPIA Fatal error: memory corrupted\n");
		/* fall to */
	    case 2:
		/* we couldn't even print the message */
		exit(-1);
	}
	break;
    }
    
    /*
     * set up our own panic function which longjumps back to reset
     */
    ec_set_option_ptr(EC_OPTION_PANIC, main_panic);

    ec_set_option_int(EC_OPTION_INIT, init_flags);
    ec_set_option_int(EC_OPTION_ARGC, new_argc);
    ec_set_option_ptr(EC_OPTION_ARGV, argv);
    if (eclipsedir)
	ec_set_option_ptr(EC_OPTION_ECLIPSEDIR, eclipsedir);

    ec_init();
    ec_post_goal(ec_term(ec_did(":",2), ec_atom(ec_did("sepia_kernel",0)),
	    ec_atom(ec_did("standalone_toplevel",0))));
    do {
	err = ec_resume();
    } while (err == PYIELD);
    ec_cleanup();
    return err;
}


/*
 * Print the warning about wrong usage and bad_exit.
 * The argument is the bad option string.
 */
/*ARGSUSED*/
static void
usage(char *opt)
{
    fprintf(stderr,"Bad option: %s\n",opt);
    fprintf(stderr,"Usage:\n");
    fprintf(stderr,"-b <file>       compile or load a boot file\n");
    fprintf(stderr,"-e <goal>       prolog goal to execute\n");
    fprintf(stderr,"-g <kbytes>     global+trail stack size\n");
    fprintf(stderr,"-l <kbytes>     local+control stack size\n");
    fprintf(stderr,"-h <kbytes>     private heap size\n");
    fprintf(stderr,"-s <kbytes>     shared heap size\n");
    fprintf(stderr,"-d <seconds>    delayed startup\n");
    fprintf(stderr,"-D <dir>        installation directory\n");
    fprintf(stderr,"--              end of ECLiPSe options\n");
    fprintf(stderr,"Parallel system only:\n");
    fprintf(stderr,"-w <num>        number of parallel workers\n");
    fprintf(stderr,"-wmi            popup worker manager interface\n");
    fprintf(stderr,"-wv             verbose worker startup\n");
    fprintf(stderr,"-wx <exec>      use specified worker executable\n");
    fprintf(stderr,"Reserved:\n");
    fprintf(stderr,"-a <><><><>\n");
    fprintf(stderr,"-c <>\n");
    fprintf(stderr,"-m <>\n");
    fprintf(stderr,"-o \n");
    fprintf(stderr,"-r <>\n");
    exit(-1);
}

static void
main_panic(char *what, char *where)
{
    fprintf(stderr, "\n*** ECLiPSe fatal error: %s",what);

    if (where)
        fprintf(stderr, " in %s",where);

    fprintf(stderr, "\n");
    fflush(stderr);

    longjmp(reset, 1);
}

