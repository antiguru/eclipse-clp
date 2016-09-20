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
 * The Original Code is  Engine Manipulation Operations for ECLiPSe CLP
 * The Initial Developer of the Original Code is  Coninfer Ltd.
 * Portions created by the Initial Developer are
 * Copyright (C) 2015-2016 Coninfer Ltd
 * Parts of this file have been factored out of embed.c, these are
 * Copyright (C) 1997-2014 Cisco Systems, Inc.  All Rights Reserved.
 * 
 * Contributor(s):	Joachim Schimpf, Coninfer Ltd 
 * 
 * END LICENSE BLOCK */

/** @file
 * @version	$Id: engines.c,v 1.4 2016/09/20 22:26:35 jschimpf Exp $
 *
 */


#include 	"config.h"
#include        "sepia.h"
#include 	"types.h"
#include 	"error.h"
#include 	"mem.h"
#include 	"dict.h"
#include	"module.h"
#include	"emu_export.h"
#include	"embed.h"
#include	"os_support.h"


#ifdef STDC_HEADERS
#include <stdarg.h>
#include <string.h>
#include <limits.h>
#else
#include <varargs.h>
extern char *	strcat();
extern char *	strcpy();
#endif

#if 0
#define DbgPrintf(s,...) p_fprintf(current_err_,s,__VA_ARGS__);ec_flush(current_err_);
#else
#define DbgPrintf(s,...)
#endif


static int _poll_requests(ec_eng_t *ec_eng, int allow_exit);

/*
 * Global state
 */

#ifdef _WIN32
static void *resume_thread = NULL;
#endif


/*----------------------------------------------------------------------
 * Initialising an embedded Eclipse
 *----------------------------------------------------------------------*/

/**
 * (Re)initialize a small auxiliary engine.
 * Assume ec_eng is uninitialised or exited.
 */

int
ecl_init_aux(t_eclipse_options *opts, ec_eng_t *ec_eng, int extra_flags)
{
    int res;

    if (!opts)
	opts = &ec_options;	/* default to global options */

    ec_eng->options = *opts;
    ecl_set_option_long(&ec_eng->options, EC_OPTION_GLOBALSIZE, 20*1024*1024);
    ecl_set_option_long(&ec_eng->options, EC_OPTION_LOCALSIZE, 1*1024*1024);
    ecl_set_option_ptr(&ec_eng->options, EC_OPTION_DEFAULT_MODULE, "sepia_kernel");
    ec_eng->options.init_flags |= extra_flags;

    res = ecl_engine_init(NULL, ec_eng);
    ec_eng->vm_flags |= ENG_HIDDEN;	/* hide in current_engines/1 */
    return res;
}


/**
 * Initialize the static Prolog engines, and perform Prolog-level boot.
 */

int
ecl_engines_init(t_eclipse_options *opts, ec_eng_t **eng)
{
    char *	initfile = (char *) 0;
    char	filename_buf[MAX_PATH_LEN];
    int		res;
    ec_eng_t	*ec_eng;

    /* Create a small auxiliary engine, used for startup and shutdown */
    res = ecl_init_aux(opts, &ec_.m_aux, INIT_NO_MAIN);
    if (res != PSUCCEED) return res;

    /* boot the system using this auxiliary engine */
    initfile = strcat(strcpy(filename_buf, ec_eclipse_home), "/lib/kernel.eco");
    if (ec_access(initfile, R_OK) < 0)
    {
	initfile = strcat(strcpy(filename_buf, ec_eclipse_home), "/lib/kernel.pl");
	if (ec_access(initfile, R_OK) < 0)
	{
	    ec_panic("Aborting: Can't find boot file! Please check either\na) your program's setting for eclipsedir in ec_set_option(), or\nb) your setting for ECLIPSEDIR environment variable.\n","ec_init()");
	}
    }	    
    res = eclipse_boot(&ec_.m_aux, initfile);
    if (res != PSUCCEED) return res;

    /* start a main loop in the aux engine and finish startup initialization */
    res = eclipse_main(&ec_.m_aux, 1);
    if (res != PYIELD) return PFAIL;
    ecl_relinquish_engine(&ec_.m_aux);

    /* Create a small signal engine and start its main loop */
    res = ecl_init_aux(opts, &ec_.m_sig, 0);
    if (res != PSUCCEED) return res;
    ecl_relinquish_engine(&ec_.m_sig);	/* will be acquired by signal thread */

    /* Create a small timer engine and start its main loop */
    res = ecl_init_aux(opts, &ec_.m_timer, INIT_ASYNC);
    if (res != PSUCCEED) return res;
    res = ecl_resume_async1(&ec_.m_timer, ec_atom(ec_did("after_loop",0)), ec_nil());
    if (res != PSUCCEED) return res;
    ecl_free_engine(&ec_.m_timer, 0);	/* live while thread lives */

    /* If requested, create a working engine and start its main loop */
    if (eng) {
	ec_eng = &ec_.m;
	ec_eng->options = *opts;
	res = ecl_engine_init(NULL, ec_eng);
	if (res != PSUCCEED) return res;
	/* Set up an RPC event handler on this engine */
	res = ecl_resume2(ec_eng, ecl_term(ec_eng, ec_did(":",2),
				    ec_atom(ec_did("sepia_kernel",0)),
				    ec_atom(ec_did("rpc_server_init",0))), NULL);
	if (res != PSUCCEED) return res;
	*eng = ec_eng;
    }
    return PSUCCEED;
}


void
ec_embed_fini(void)
{
#ifdef _WIN32
    if (resume_thread)
    {
	(void) ec_thread_terminate(resume_thread, 3000/*ms timeout*/);
	resume_thread = NULL;
    }
#endif
    hp_free(ec_eclipse_home);
    ec_eclipse_home = 0;
}


/*----------------------------------------------------------------------
 * Making engines
 *----------------------------------------------------------------------*/

void *
_engine_run_thread(ec_eng_t *ec_eng)
{
    /* assert(ec_eng->own_thread == ec_thread_self());	can't test: race! */

    /* thread-local initializations */
    ec_thread_reinstall_handlers(NULL);

    /* resume the engine whenever we get the ownership */
    ec_mutex_lock(&ec_eng->lock);
    for(;;)
    {
	int res;

	/* wait for ownership */
	while (!EngIsOurs(ec_eng)) {
	    res = ec_cond_wait(&ec_eng->cond, &ec_eng->lock, -1);
	    assert(!res);
	}
	ec_mutex_unlock(&ec_eng->lock);
	assert(!EngIsDead(ec_eng));
	DbgPrintf("Async engine 0x%x starts running\n", ec_eng);

	/* we now have a strong reference and engine ownership */
	res = resume_emulc(ec_eng);

	/* If engine was detached, destroy the engine and return
	 * PEXITED(<actual return status as integer>)
	 */
	if ((res != PEXITED) && (ec_eng->options.vm_options & ENG_DETACHED)) {
	    ecl_engine_exit(ec_eng, A[1].val.nint);	/* now PEXITED */
	    res = PEXITED;
	}

	DbgPrintf("Async release 0x%x\n", ec_eng);

	if (res == PEXITED) {
	    ec_mutex_lock(&ec_eng->lock);
	    ec_eng->owner_thread = NULL;	/* release */
	    if (!ecl_free_engine(ec_eng, 1)) {	/* drop strong reference */
		ec_cond_signal(&ec_eng->cond, 1);	/* wake all joiners */
		ec_mutex_unlock(&ec_eng->lock);
	    }
	    break;				/* end thread */
	}
	
	ec_mutex_lock(&ec_eng->lock);
	ec_eng->paused = PauseType(YIELD_ARITY, PAUSE_EXITABLE_VIA_JOIN);
	_poll_requests(ec_eng, 1);

	ec_eng->owner_thread = NULL;	/* release */

	DbgPrintf("Async free 0x%x\n", ec_eng);
	if (ecl_free_engine(ec_eng, 1/*islocked*/))	/* after releasing */
	    break;			/* engine deallocated, end thread */

	/* We have freed our strong reference, but as long as we hold
	 * the lock, the engine cannot be acquired for exiting/destruction
	 * by another thread, even if the ref count goes to zero.
	 * So lock and cond are still usable.
	 */
	DbgPrintf("Async go to sleep 0x%x\n", ec_eng);
	ec_cond_signal(&ec_eng->cond, 1);	/* wake all joiners */
    }
    ec_thread_detach(ec_thread_self());
    EngLogMsg(ec_eng, "thread terminated", 0);
    return NULL;
}


/*
 * The lock, cond and owner fields of ec_eng must be initialized already!
 */

static int
_engine_thread_create(ec_eng_t *ec_eng)
{
    int err = ec_thread_create(&ec_eng->own_thread,
		(void*(*)(void*)) _engine_run_thread,
		(void*) ec_eng);
    if (err) {
	Set_Sys_Errno(err, ERRNO_OS);
	return SYS_ERROR;
    }
    EngLogMsg(ec_eng, "thread created", 0);
    return PSUCCEED;
}



/**
 * Initialize an engine whose descriptor is already allocated.
 * - options are set in the descriptor's options
 * - parent_eng only used to inherit settings (may be NULL)
 * - returns an acquired engine
 * TODO: merge with emu_init()
 */

int
ecl_engine_init(ec_eng_t *parent_eng, ec_eng_t *new_eng)
{
    int res;

    emu_init(parent_eng, new_eng);

    /* link it into the global list of engines */
    mt_mutex_lock(&EngineListLock);
    new_eng->needs_dgc_marking = 0;	/* under lock, see gc_dictionary() */
    if (new_eng == eng_chain_header) {
	assert(eng_chain_header->next == NULL); /* must be first engine inserted */
	eng_chain_header->prev = eng_chain_header->next = eng_chain_header;
    } else {
	new_eng->next = eng_chain_header;
	new_eng->prev = eng_chain_header->prev;
	new_eng->prev->next = new_eng;
	eng_chain_header->prev = new_eng;
    }
    mt_mutex_unlock(&EngineListLock);

    /* Start a thread if necessary */
    if (new_eng->options.init_flags & INIT_ASYNC) {
	res = _engine_thread_create(new_eng);
	if (res != PSUCCEED) {
	    res = ENGINE_NOT_UP;
	    goto _return_error_;
	}
    } 

    /* start the main loop */
    if (!(new_eng->options.init_flags & INIT_NO_MAIN)) {
	if (eclipse_main(new_eng, 0) != PYIELD) {
	    res = ENGINE_NOT_UP;
	    goto _return_error_;
	}
    }
    return PSUCCEED;

_return_error_:
    ec_emu_fini(new_eng);
    return res;
}



/**
 * Create a new engine
 * @param opts engine options, may be NULL (for using global options)
 * @param parent_eng only used to inherit settings (may be NULL)
 * @param eng returns an acquired engine
 * @return
 *	- PSUCCEED
 *	- RANGE_ERROR		options out of range (e.g. stack sizes)
 *	- ENGINE_NOT_UP		could not create/start engine
 *	- SYS_ERROR		OS error during creation
 */


int Winapi
ecl_engine_create(t_eclipse_options *opts, ec_eng_t *parent_eng, ec_eng_t **eng)
{
    int res;
    ec_eng_t *new_eng;

    /* check whether engine creation is forbidden */
    if (ShutdownInProgress)
    	return ENGINE_NOT_UP;

    /* check minimum stack sizes */
    if (opts->localsize < ENG_MIN_LOCAL*1024
     || opts->globalsize < ENG_MIN_GLOBAL*1024) {
	return RANGE_ERROR;
    }

    new_eng = (ec_eng_t *) hg_alloc_size(sizeof(ec_eng_t));
    new_eng->options = *opts;
    res = ecl_engine_init(parent_eng, new_eng);
    if (res != PSUCCEED) {
	hg_free(new_eng);
	return res;
    }

    EngLogMsg(new_eng, "created", 0);
    *eng = new_eng;
    return PSUCCEED;
}


/*----------------------------------------------------------------------
 * Engine ownership
 *----------------------------------------------------------------------*/

/**
 * Acquire ownership of an engine for the calling thread.
 *
 * Any engine manipulation (constructing terms in it, resuming it, etc)
 * requires engine ownership.
 * This is using locking, so only one thread can grab the engine.
 * An engine remains owned by a thread until it is released via
 * ecl_relinquish_engine().
 * @return
 *	- PSUCCEED	engine was successfully acquired
 *	- PFAIL		engine was already owned (don't relinquish!)
 *	- ENGINE_BUSY	engine owned by other thread (not acquired)
 *	- ENGINE_DEAD	engine already dead (not acquired)
 * 
 * A newly acquired engine is no longer 'paused'.
 */
int Winapi
ecl_acquire_engine(ec_eng_t *ec_eng)
{
    int res;
    mt_mutex_lock(&ec_eng->lock);
    if (EngIsDead(ec_eng)) {
	assert(A[1].val.nint == PEXITED);
	res = ENGINE_DEAD;
    } else if (!ec_eng->owner_thread) {
	ec_eng->owner_thread = ec_thread_self();
	ec_eng->paused = 0;
        res = PSUCCEED;
    } else if (ec_eng->owner_thread == ec_thread_self()) {
        res = PFAIL;
    } else {
	res = ENGINE_BUSY;
    }
    mt_mutex_unlock(&ec_eng->lock);
    return res;
}


/**
 * Give up the right to modify the engine from the current thread.
 *
 * We handle requests that might be left over from the previous phase,
 * and then leave the engine in a free and 'paused' (or dead) state.
 */

void Winapi
ecl_relinquish_engine(ec_eng_t *ec_eng)
{
    assert(EngIsOurs(ec_eng));
    assert(!EngIsPaused(ec_eng));

    mt_mutex_lock(&ec_eng->lock);
    if (!EngIsDead(ec_eng)) {
	/* Before relinquishing ownership, we have to check for pending requests */
	if (FakedOverflow) {
	    if (ecl_housekeeping(ec_eng, YIELD_ARITY, 1) == PEXITED) {
		assert(EngIsDead(ec_eng));
		ec_eng->owner_thread = NULL;
		mt_mutex_unlock(&ec_eng->lock);
		return;
	    }
	}
	ec_eng->paused = PauseType(YIELD_ARITY, PAUSE_NOT_EXITABLE);
    }
    ec_eng->owner_thread = NULL;
    mt_mutex_unlock(&ec_eng->lock);
}


/*----------------------------------------------------------------------
 * Reference counting
 *----------------------------------------------------------------------*/

/*
 * Lose a (strong) reference to an engine.
 * Return 1 if engine must be considered deallocated.
 * The pointer must not be used after calling this function!
 */

int
ecl_free_engine(ec_eng_t *ec_eng, int locked)	/* ec_eng != NULL */
{
    int rem = atomic_add(&ec_eng->ref_ctr, -1);
    if (rem > 0) {
	EngLogMsg(ec_eng, "refs %d->%d", rem+1, rem);
	return 0;
    }

    /* We now have exclusive access to the engine, although it may have
     * a weak reference from the global chain (but as the reference count
     * is now 0, it cannot be resurrected via the weak reference any longer).
     */

    if (locked)
	mt_mutex_unlock(&ec_eng->lock);

    switch (ecl_acquire_engine(ec_eng)) {

	case ENGINE_DEAD:
#ifdef UNCHAIN_ENGINES_WHEN_DEAD
	    /* engine is dead, this implies it is not in the global
	     * chain either, so it can be destroyed completely
	     */
#else
	    /* unchain, if not already done in ec_emu_fini() */
	    mt_mutex_lock(&EngineListLock);
	    ec_eng->next->prev = ec_eng->prev;
	    ec_eng->prev->next = ec_eng->next;
	    mt_mutex_unlock(&EngineListLock);
#endif
	    ec_cond_destroy(&ec_eng->cond);
	    mt_mutex_destroy(&ec_eng->lock);
	    /* free the structure, unless it is one of the static engines */
	    if ((void*)ec_eng < (void*)&ec_  ||  (void*)(&ec_+1) <= (void*)ec_eng) {
		hg_free_size(ec_eng, sizeof(ec_eng_t));
	    }
	    EngLogMsg(ec_eng, "destroyed", 0);
	    return 1;

	case PFAIL:		/* already owned by us */
	    /* should have released before losing the reference! */
	    p_fprintf(warning_output_, "\necl_free_engine(): losing last reference while owning engine %0x%x", ec_eng);
	    ec_flush(warning_output_);
	    /* fall through */
	case PSUCCEED:		/* successfully acquired */
	    EngLogMsg(ec_eng, "forcing exit", 0);
	    ec_eng->ref_ctr = 1;	/* resurrect for exiting */
	    /* now further resurrections are possible again, but the
	     * engine is already acquired and will certainly exit
	     */
	    if (ec_eng->own_thread && ec_eng->own_thread != ec_thread_self()) {
		/* all the rest done by the engine's own thread! */
		ecl_exit_async(ec_eng, 0);
	    } else {
		ecl_engine_exit(ec_eng, 0);	/* now PEXITED */
		ecl_relinquish_engine(ec_eng);
	    }
	    return ecl_free_engine(ec_eng, 0);

	case ENGINE_BUSY:	/* could not acquire */
	    p_fprintf(current_err_, "\necl_free_engine(): can't aquire engine %0x%x for exiting", ec_eng);
	    ec_flush(current_err_);
	    return 1;

	default:
	    assert(0);
    }
}

/**
 * Get another strong reference (we must hold one already),
 * i.e. increment the engine's reference count.
 * The engine can be in any state.
 */

ec_eng_t *
ecl_copy_engine(ec_eng_t *ec_eng)	/* ec_eng != NULL */
{
    int rem = atomic_add(&ec_eng->ref_ctr, 1);
    EngLogMsg(ec_eng, "refs %d->%d", rem-1, rem);
    assert(rem>1);	/* caller wasn't actually holding a strong reference */
    return ec_eng;
}

/**
 * Get a strong reference to an engine, if possible.
 * Use this when you currently only have a weak reference.
 * NULL return means the engine is about to exit and disappear
 * (although it may still be live at this moment).
 * Even with a non-NULL return, the engine can already be dead,
 * or already acquired for exiting.  I.e. there is no guarantee that the
 * engine is a usable one, this must be ascertained by trying to 'acquire'.
 */

ec_eng_t *
ecl_resurrect_engine(ec_eng_t *ec_eng)
{
    int old;
    for(;;) {
	old = atomic_load(&ec_eng->ref_ctr);
	if (old == 0)
	    return NULL;	/* can't increment from 0 */
	if (compare_and_swap(&ec_eng->ref_ctr, old, old+1))
	    break;
    }
    EngLogMsg(ec_eng, "refs %d->%d", old, old+1);
    return ec_eng;
}


/*----------------------------------------------------------------------
 * Posting goals
 *----------------------------------------------------------------------*/

/* goal must already be on ec_eng (not checked!) */
int Winapi
ecl_post_goal(ec_eng_t *ec_eng, const pword goal)
{
    pword conj;

    assert(EngIsOurs(ec_eng));

    if (ec_eng->nesting_level > 1)
	ec_panic("can't post goal to nested engine","ecl_post_goal()");

    /*TODO: mutual exclusion*/
    if (IsAtom(POSTED.tag) && POSTED.val.did == d_.true0)
	return ecl_assign(ec_eng, &POSTED, goal.val, goal.tag);

    Make_Struct(&conj, TG);
    Push_Struct_Frame(d_.comma);
    conj.val.ptr[1] = POSTED;
    conj.val.ptr[2] = goal;
    return ecl_assign(ec_eng, &POSTED, conj.val, conj.tag);
}

static pword
_get_posted_goals(ec_eng_t *ec_eng)
{
    pword empty;
    pword posted = POSTED;
    Make_Atom(&empty, d_.true0);
    ecl_assign(ec_eng, &POSTED, empty.val, empty.tag);
    return posted;
}

int Winapi
ecl_post_string(ec_eng_t *ec_eng, const char *callstring)
{
    return ecl_post_goal(ec_eng, ecl_term(ec_eng, ec_.d.colon,
	ec_atom(ec_.d.kernel_sepia),
	ecl_term(ec_eng, ec_did("exec_string",2), ecl_string(ec_eng, callstring), ecl_newvar(ec_eng))));
}

int Winapi
ecl_post_exdr(ec_eng_t *ec_eng, int length, const char *exdr_string)
{
    return ecl_post_goal(ec_eng, ecl_term(ec_eng, ec_.d.colon,
	ec_atom(ec_.d.kernel_sepia),
    	ecl_term(ec_eng, ec_did("exec_exdr",1), ecl_length_string(ec_eng, length, exdr_string))));
}

int Winapi
ecl_exec_string(
	ec_eng_t *ec_eng,
    	char *callstring,
	ec_ref varsref)		/* NULL is allowed */
{
    int		res;
    pword	vars;
    dident exec_string_2 =  enter_dict("exec_string",2);
    
    vars = ecl_newvar(ec_eng);
    if (varsref)
	ec_ref_set(varsref, vars);
    res = ecl_post_goal(ec_eng, ecl_term(ec_eng, ec_.d.colon,
	ec_atom(ec_.d.kernel_sepia),
	ecl_term(ec_eng, exec_string_2, ecl_string(ec_eng, callstring), vars)));
    Return_If_Not_Success(res);
    return ecl_resume1(ec_eng, 0);
}


/*----------------------------------------------------------------------
 * Resuming engine execution
 *----------------------------------------------------------------------*/

#define Check_Resumable(eng) \
    if (EngIsDead(eng)) return ENGINE_DEAD; \
    if (EngIsFree(eng)) return ENGINE_NOT_OWNED;\
    if (!EngIsOurs(eng)) return PRUNNING;\
    assert(!EngIsPaused(eng));


int Winapi
ecl_set_context_module(ec_eng_t *ec_eng, dident module)
{
    if (module != d_.nil)
    	return RANGE_ERROR;
    ec_eng->default_module = module;
    return PSUCCEED;
}


/*
 * DEPRECATED: resume with posted goals
 */
int Winapi
ecl_resume(ec_eng_t *ec_eng)
{
    return ecl_resume1(ec_eng, 0);
}

/*
 * DEPRECATED: resume with posted goals
 */
int Winapi
ecl_resume1(ec_eng_t *ec_eng, ec_ref chp)
{
    return ecl_resume2(ec_eng, _get_posted_goals(ec_eng), chp);
}

/*
 * DEPRECATED: resume with posted goals
 */

int Winapi
ecl_resume2(ec_eng_t *ec_eng, const pword from_c, ec_ref to_c)
{
    int res;

    Check_Resumable(ec_eng);

    A[1] = from_c;
    Make_Module_Atom(&A[2], ec_eng->default_module);
    res = resume_emulc(ec_eng);

    if (to_c)
	ec_ref_set_safe(to_c,A[2]);

    return res;
}


/**
 * This is a shorthand for ecl_resume2() in the case that we are resuming
 * with a goal and module.
 * @param module can be [], in which case the engine's default module is used.
 * @param to_c can be NULL
 * @param options
 *    - GOAL_CALL:	invoke `Goal.`
 *    - GOAL_CUT:	invoke `Goal, !.`
 *    - GOAL_NOTNOT:	invoke `\+ \+ Goal.`
 *    - GOAL_CUTFAIL:	invoke `Goal, !, fail.`
 */

int Winapi
ecl_resume_goal(ec_eng_t *ec_eng, const pword goal, pword module,
		ec_ref to_c, int option)
{
    int res;

    Check_Resumable(ec_eng);
    assert(A[1].val.nint == PSUCCEED || A[1].val.nint == PFAIL || A[1].val.nint == PTHROW);

    switch(option)
    {
	default:
	case GOAL_CALL:
	    A[1] = goal;
	    break;
	case GOAL_CUT:
	    A[1] = ecl_term(ec_eng, d_.comma, goal, ec_atom(d_.cut));
	    break;
	case GOAL_NOTNOT:
	    A[1] = ecl_term(ec_eng, d_.naf, ecl_term(ec_eng, d_.naf, goal));
	    break;
	case GOAL_CUTFAIL:
	    A[1] = ecl_term(ec_eng, d_.comma, goal, ecl_term(ec_eng,
				    d_.comma, ec_atom(d_.cut), ec_atom(d_.fail)));
	    break;
    }
    if (IsNil(module.tag)) {
	Make_Module_Atom(&A[2], ec_eng->default_module);
    } else {
	A[2] = module;
    }

    res = resume_emulc(ec_eng);

    if (to_c)
	ec_ref_set_safe(to_c, A[2]);

    return res;
}


int Winapi
ecl_resume_long(ec_eng_t *ec_eng, long int *to_c)
{
    int res;
    pword * pw;

    Check_Resumable(ec_eng);

    A[1] = _get_posted_goals(ec_eng);
    Make_Module_Atom(&A[2], ec_eng->default_module);

    res = resume_emulc(ec_eng);

    if (to_c) {
	pw = &A[2];
	Dereference_(pw)
	*to_c = IsInteger(pw->tag) ? pw->val.nint : 0;
    }
    return res;
}



int Winapi
ec_running(void)
{
#ifdef _WIN32
    int res;
    if (resume_thread  &&  !ec_thread_stopped(resume_thread, &res))
	return 1;
#endif
    return 0;
}


/*
 * Copies the term from from_eng to ec_eng, and resumes ec_eng.
 * Both engines must be owned.
 */

int
ecl_copy_resume(ec_eng_t *from_eng, ec_eng_t *ec_eng, const pword term, const pword module)
{
    int res;

    Check_Resumable(ec_eng);
    if (!EngIsOurs(from_eng)) return ENGINE_NOT_OWNED;
    if (from_eng == ec_eng) return ENGINE_BUSY;	/* cannot resume myself */

    /* copy resume argument to A[1] */
    res = ec_copy_term_across(from_eng, ec_eng, term.val, term.tag, &A[1], 1);
    if (res != PSUCCEED) {
	return res;
    }
    if (IsNil(module.tag)) {
	Make_Module_Atom(&A[2], ec_eng->default_module);
    } else {
	A[2] = module;
    }

    ec_eng->parent_engine = from_eng;
    res = resume_emulc(ec_eng);
    ec_eng->parent_engine = NULL;
    return res;
}


int Winapi
ecl_running(ec_eng_t *ec_eng)
{
    return !EngIsFree(ec_eng);
}



/**
 * Resume an engine in its own thread.
 * This creates an extra reference and transfers engine ownership to the thread.
 * @return PSUCCEED, PRUNNING, ENGINE_NOT_ASYNC, SYS_ERROR
 */

int Winapi
ecl_resume_async(ec_eng_t *ec_eng)
{
    return ecl_resume_async1(ec_eng, _get_posted_goals(ec_eng), ec_nil());
}


int Winapi
ecl_resume_async1(ec_eng_t *ec_eng, const pword from_c, const pword module)
{
    Check_Resumable(ec_eng);

    /* engine must have its own thread, create if necessary */
    if (!ec_eng->own_thread) {
	int res = _engine_thread_create(ec_eng);
	if (res != PSUCCEED) return res;
    }

    A[1] = from_c;
    if (IsNil(module.tag)) {
	Make_Module_Atom(&A[2], ec_eng->default_module);
    } else {
	A[2] = module;
    }

    /* pass ownership to the engine's own thread */
    ecl_copy_engine(ec_eng);	/* extra reference to pass to the thread */
    mt_mutex_lock(&ec_eng->lock);
    ec_eng->owner_thread = ec_eng->own_thread;
    ec_cond_signal(&ec_eng->cond, 0);
    mt_mutex_unlock(&ec_eng->lock);

    return PSUCCEED;
}


/*
 * Copies the term from from_eng to ec_eng, and resumes ec_eng.
 */

int
ecl_copy_resume_async(ec_eng_t *from_eng, ec_eng_t *ec_eng, const pword term, const pword module)
{
    int res;

    Check_Resumable(ec_eng);
    if (!EngIsOurs(from_eng)) return ENGINE_NOT_OWNED;
    if (from_eng == ec_eng) return ENGINE_BUSY;	/* cannot resume myself */

    /* engine must have its own thread, create if necessary */
    if (!ec_eng->own_thread) {
	int res = _engine_thread_create(ec_eng);
	if (res != PSUCCEED) return res;
    }

    /* copy resume argument to A[1] */
    res = ec_copy_term_across(from_eng, ec_eng, term.val, term.tag, &A[1], 1);
    if (res < 0) return res;
    if (IsNil(module.tag)) {
	Make_Module_Atom(&A[2], ec_eng->default_module);
    } else {
	A[2] = module;
    }

    /* pass ownership to the engine's own thread */
    ecl_copy_engine(ec_eng);	/* extra reference to pass to the thread */
    mt_mutex_lock(&ec_eng->lock);
    ec_eng->owner_thread = ec_eng->own_thread;
    ec_cond_signal(&ec_eng->cond, 0);
    mt_mutex_unlock(&ec_eng->lock);
    return PSUCCEED;
}


int
ecl_exit_async(ec_eng_t *ec_eng, int exit_code)
{
    pword goal;
    assert(EngIsOurs(ec_eng));
    Make_Struct(&goal, TG);
    Push_Struct_Frame(in_dict("exit",1));
    Make_Integer(&goal.val.ptr[1], (word) exit_code);
    return ecl_resume_async1(ec_eng, goal, ec_nil());
}


/*----------------------------------------------------------------------
 * Wait/join an asynchronous engine
 *----------------------------------------------------------------------*/

/**
 * Wait until an asynchronous engine stops, then acquire it
 * (ready for result retrieval).
 *
 * @param timeout	in milliseconds, -1 means no timeout
 * @return
 *	- PSUCCEED	engine stopped and acquired for result retrieval
 *	- ENGINE_NOT_ASYNC	engine not joinable (self or not async running)
 *	- PRUNNING	timeout, engine still running
 *	- SYS_ERROR	error from ec_cond_wait()
 */
int Winapi
ecl_join_acquire(ec_eng_t *ec_eng, int timeout)
{
    int res = PSUCCEED;

    if (EngIsOurs(ec_eng))
	return ENGINE_NOT_ASYNC;	/* can't wait for myself (shortcut) */

    ec_mutex_lock(&ec_eng->lock);

    while (!EngIsFree(ec_eng)) {
	/* we only allow waiting for engines that are running on their
	 * own thread, because only they will signal when finished.
	 */
	if (ec_eng->owner_thread != ec_eng->own_thread) {
	    res = ENGINE_NOT_ASYNC;	/* cannot be waited for */
	    goto _unlock_return_;
	}
	res = ec_cond_wait(&ec_eng->cond, &ec_eng->lock, timeout);
	if (res < 0) {
	    res = PRUNNING;		/* timeout */
	    goto _unlock_return_;
	}
	if (res > 0) {
	    res = SYS_ERROR;
	    goto _unlock_return_;
	}
    }

    ec_eng->owner_thread = ec_thread_self();	/* acquire */
    ec_eng->paused = 0;
    res = PSUCCEED;

_unlock_return_:
    ec_mutex_unlock(&ec_eng->lock);
    return res;
}


/*
 * Wait until an asynchronous engine stops, acquire it,
 * return its status in res, returned term (if any) in to_c.
 *
 * timeout	in milliseconds, -1 means no timeout
 * Returns:
 *	PSUCCEED..PFLUSHIO	engine status (=A[1],  and A[2])
 *	ENGINE_NOT_ASYNC engine not joinable (self or not async running)
 *	PRUNNING	timeout, engine still running
 *	SYS_ERROR	error from ec_cond_wait()
 */
int Winapi
ecl_wait_resume_status_long(ec_eng_t *ec_eng, long int *to_c, int timeout)
{
    int res;
    pword *pw;

    res = ecl_join_acquire(ec_eng, timeout);
    if (res != PSUCCEED) return res;

    /* engine is now acquired */

    if (to_c) {
	pw = &A[2];
	Dereference_(pw)
	*to_c = IsInteger(pw->tag) ? pw->val.nint : 0;
    }

    /* get the actual status code from the engine */
    assert(IsInteger(A[1].tag));
    res = A[1].val.nint;
    assert(PSUCCEED <= res && res <= PFLUSHIO);
    return res;
}


int Winapi
ecl_resume_status_long(ec_eng_t *ec_eng, long int *to_c)
{
    return ecl_wait_resume_status_long(ec_eng, to_c, 0);
}


int Winapi
ecl_resume_status(ec_eng_t *ec_eng)
{
    long dummy;
    return ecl_resume_status_long(ec_eng, &dummy);
}



/*----------------------------------------------------------------------
 * Resuming Eclipse without continuing
 * just create an opportunity for event handling
 * Return values:
 *	PRUNNING
 *		engine not yet ready (previous resume_async)
 *	PFLUSHIO,PWAITIO
 *		nested request from within handler
 *	PSUCCEED
 *		handler finished
 *	PFAIL,PTHROW
 *		should never occur (prevented by yield/3)
 *	PYIELD
 *		programmer error (yield/2 in handler)
 *
 * This is for backward compatibility, but PFAIL/PTHROW/PYIELD
 * can now occur if that's what the handler did.
 *----------------------------------------------------------------------*/

/*
 * This should be the same as resume(true), except that the previous
 * engine status is preserved when the handler(s) succeed.
 */
int Winapi
ecl_handle_events(ec_eng_t *ec_eng)
{
    pword *pw;

    Check_Resumable(ec_eng);
    Make_Nil(&A[1])		/* don't care */
    Make_Nil(&A[2]);		/* nil module indicates no goal */
    return resume_emulc(ec_eng);
}


int Winapi
ecl_handle_events_long(ec_eng_t *ec_eng, long int *to_c)
{
    int res = ecl_handle_events(ec_eng);
    if (to_c)
    {
	pword * pw = &A[2];
	Dereference_(pw)
	*to_c = IsInteger(pw->tag) ? pw->val.nint : 0;
    }
    return res;
}


/*----------------------------------------------------------------------
 * Requests to engines
 *----------------------------------------------------------------------*/

/**
 * Send a simple request to the engine.
 * The type of request is encoded as a bitmask (same as for EVENT_FLAGS).
 * Depending on the engine state, the request is either
 * - handled immediately (when paused)
 * - flagged up in the engine by setting the corresponding bit
 *   in EVENT_FLAGS together with a FakedOverflow
 * - ignored (when the engine is already dead)
 *
 * @param request
 *	- DICT_GC_REQUEST	request dictionary marking
 *	- TEST_REQUEST		for testing only
 * @return
 *	- PSUCCEED		request was handled directly
 *	- PRUNNING		request posted
 *	- PEXITED		engine was (or has just been) exited
 */
int
ecl_request(ec_eng_t *ec_eng, int request)
{
    int res;

    mt_mutex_lock(&ec_eng->lock);
    if (EngIsDead(ec_eng))
    {
	DbgPrintf("Ignoring request(%d) - engine dead!\n", request);
    	res = PEXITED;
    }
    else if (EngIsPaused(ec_eng))
    {
	/* paused (including free): can't change under lock */
	DbgPrintf("Handling request(%d) directly!\n", request);
	switch(request) {
	    case DICT_GC_REQUEST:
		ecl_mark_engine(ec_eng, EngPauseArity(ec_eng));
		break;
	    case TEST_REQUEST:
		p_fprintf(log_output_, "Handling test_request directly (arity=%d)!\n",  EngPauseArity(ec_eng));
		ec_flush(log_output_);
		break;
	    default:
		p_fprintf(current_err_, "Unrecognized engine request: %x\n", request);
		ec_flush(current_err_);
		break;
	}
	res = PSUCCEED;
    }
    else
    {
	/* unpaused: can go to paused (followed by poll) */
	DbgPrintf("Making request(%d)!\n", request);
	/* Note: no guarantee that the engine will handle the request! */
	atomic_or(&EVENT_FLAGS, request);
	Fake_Overflow;
	res = PRUNNING;
    }
    mt_mutex_unlock(&ec_eng->lock);
    return res;
}


/**
 * Exit the given engine immediately if possible, otherwise
 * set its EXIT_REQUEST flag.
 * @param exit_code	integer returned as engine exit code
 * @return
 *	PEXITED		if engine has already exited
 *	PRUNNING	if an EXIT_REQUEST has been submitted
 */

int Winapi
ecl_request_exit(ec_eng_t *ec_eng, int exit_code)
{
    int res;

    mt_mutex_lock(&ec_eng->lock);
    if (EngIsDead(ec_eng)) {
	DbgPrintf("ecl_request_exit(%x): already dead\n", ec_eng);
	assert(A[1].val.nint == PEXITED);
	res = PEXITED;

    } else if (EngIsFree(ec_eng)) {
	if (ec_eng->own_thread) {
	    DbgPrintf("ecl_request_exit(%x): exit via own thread\n", ec_eng);
	    ec_eng->owner_thread = ec_thread_self();	/* acquire */
	    ec_eng->paused = 0;
	    mt_mutex_unlock(&ec_eng->lock);
	    res = ecl_exit_async(ec_eng, exit_code);
	    res = (res==PSUCCEED) ? PEXITED : res;

	} else {
	    DbgPrintf("ecl_request_exit(%x): acquiring and exiting\n", ec_eng);
	    ec_eng->owner_thread = ec_thread_self();	/* acquire */
	    ec_eng->paused = 0;
	    ecl_engine_exit(ec_eng, exit_code);	/* now PEXITED */
	    res = PEXITED;
	}

    } else if (EngPauseExitable(ec_eng)) {
	if (ec_eng->owner_thread == ec_thread_self()) {
	    /* shouldn't happen */
	    DbgPrintf("ecl_request_exit(%x): exiting from pause\n", ec_eng);
	    ecl_engine_exit(ec_eng, exit_code);	/* now PEXITED */
	    res = PEXITED;
	} else if (ec_eng->owner_thread == ec_eng->own_thread) {
	    DbgPrintf("ecl_request_exit(%x): cancel/acquire/exiting from pause\n", ec_eng);
	    ec_thread_cancel_and_join(ec_eng->owner_thread);
	    ec_eng->owner_thread = ec_eng->own_thread = ec_thread_self();
	    ec_eng->paused = 0;
	    ecl_engine_exit(ec_eng, exit_code);	/* now PEXITED */
	    res = PEXITED;
	} else {
	    /* owned by random thread, just post */
	    DbgPrintf("ecl_request_exit(%x): posting request\n", ec_eng);
	    /* Note: no guarantee that the engine will handle the request! */
	    ec_eng->requested_exit_code = exit_code;
	    atomic_or(&EVENT_FLAGS, EXIT_REQUEST);
	    Fake_Overflow;
	    res = PRUNNING;
	}

    } else {
	DbgPrintf("ecl_request_exit(%x): posting request\n", ec_eng);
	/* Note: no guarantee that the engine will handle the request! */
	ec_eng->requested_exit_code = exit_code;
	atomic_or(&EVENT_FLAGS, EXIT_REQUEST);
	Fake_Overflow;
	res = PRUNNING;
    }
    mt_mutex_unlock(&ec_eng->lock);
    return res;
}


static int
_poll_requests(ec_eng_t *ec_eng, int allow_exit)
{
    if (FakedOverflow) {
	/* return PSUCCEED or PEXITED */
	return ecl_housekeeping(ec_eng, YIELD_ARITY, allow_exit);
    }
    return PSUCCEED;
}


/**
 * Pause an engine, i.e. indicate that it is idle and in a clean state.
 * On a paused engine, certain operations (like dictionary marking) can
 * be performed by a thread other than the engine's owner (under lock).
 * Pausing can only be initiated by the current owner.
 * @param arity the number of valid engine argument registers
 * @param exitability whether the engine may be exited during the pause
 * CAUTION: if exitability is given, the engine may be dead after return.
 */
void
ecl_pause_engine(ec_eng_t *ec_eng, int arity, int exitability)
{
    assert(EngIsOurs(ec_eng));
    assert(!EngIsDead(ec_eng));
    assert(!EngIsPaused(ec_eng));
    /* can be joinable or not */
    /* engine owned by us */

    mt_mutex_lock(&ec_eng->lock);
    ec_eng->paused = PauseType(arity, exitability);
    /* From now on, new requests are handled directly (as soon as unlocked) */
    /* Before continuing, we have to check for pending requests */
    _poll_requests(ec_eng, exitability);
    mt_mutex_unlock(&ec_eng->lock);
}

/**
 * Take the engine out of Paused-state.
 * Can only be called by the current owner.
 * We expect the engine to be Paused.
 * We have to lock to disable request handling before
 * the phase is switched, because
 * we can't switch during request handling
 */
void
ecl_unpause_engine(ec_eng_t *ec_eng)
{
    assert(EngIsOurs(ec_eng));
    /* can be dead */
    /* can be joinable or not */
    /* engine owned by us */

    /* lock because housekeeping may be ongoing */
    mt_mutex_lock(&ec_eng->lock);
    if (!EngIsDead(ec_eng))
    {
	assert(EngIsPaused(ec_eng));
	ec_eng->paused = 0;
    }
    mt_mutex_unlock(&ec_eng->lock);
}

