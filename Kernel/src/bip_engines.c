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
 * The Original Code is  Multi-Engine built-ins for ECLiPSe CLP
 * The Initial Developer of the Original Code is  Coninfer Ltd.
 * Portions created by the Initial Developer are
 * Copyright (C) 2014 Coninfer Ltd
 * 
 * Contributor(s):	Joachim Schimpf, Coninfer Ltd 
 * 
 * END LICENSE BLOCK */

/*
 * VERSION	$Id: bip_engines.c,v 1.3 2016/08/04 09:09:04 jschimpf Exp $
 */

/****************************************************************************
 *
 *	Built-in Predicates: Engine handling
 *
 *****************************************************************************/

#include	"config.h"
#include        "sepia.h"
#include        "types.h"
#include        "embed.h"
#include        "mem.h"
#include        "error.h"
#include        "ec_io.h"
#include	"dict.h"
#include	"emu_export.h"
#include	"module.h"
#include        "property.h"
#include        "os_support.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef STDC_HEADERS
#include <stdlib.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <assert.h>
#include <stdio.h>

#if 0
#define DbgPrintf(s,...) p_fprintf(current_err_,s, __VA_ARGS__);ec_flush(current_err_);
#else
#define DbgPrintf(s,...)
#endif

static dident
	d_thread_,
	d_thread1_,
	d_clone_,
	d_clone1_,
	d_detached_,
	d_detached1_,
	d_engine_,
	d_exception1_,
	d_exited1_,
	d_flushio1_,
	d_references1_,
	d_running_,
	d_paused_,
	d_status1_,
	d_verbose_,
	d_waitio1_,
	d_yielded1_;


/*----------------------------------------------------------------------
 * Engines and Threads
 *----------------------------------------------------------------------*/

/*
 * Engine object handles
 */


void
_free_engine(ec_eng_t *ec_eng)	/* ec_eng != NULL */
{
    ecl_free_engine(ec_eng, 0);
}


static int
_tostr_engine(ec_eng_t *ec_eng, char *buf, int quoted)
{
#define STRSZ_ENGINE 30
#if 0
    sprintf(buf, "'ENGINE'(16'%x)", (int)((word)ec_eng));
#else
    sprintf(buf, "'ENGINE'(16'%08x)", (int)(EngPrintId(ec_eng)));
#endif
    return strlen(buf);
}


static int
_strsz_engine(ec_eng_t *ec_eng, int quoted)
{
    return STRSZ_ENGINE;
}

static dident
_kind_engine(void)
{
    return d_engine_;
}


static int
_lock_engine(ec_eng_t *obj)
{
    return mt_mutex_lock(&obj->lock);
}

static int
_trylock_engine(ec_eng_t *obj)
{
    return mt_mutex_trylock(&obj->lock);
}

static int
_unlock_engine(ec_eng_t *obj)
{
    return mt_mutex_unlock(&obj->lock);
}



/* CLASS DESCRIPTOR (method table) */

t_ext_type engine_tid = {
    (void (*)(t_ext_ptr)) _free_engine,	/* free */
    (t_ext_ptr (*)(t_ext_ptr)) ecl_copy_engine,	/* copy */
    0,						/* mark */
    (int (*)(t_ext_ptr,int)) _strsz_engine,	/* strsz */
    (int (*)(t_ext_ptr,char*,int)) _tostr_engine,	/* tostr */
    0,	/* equal */
    0,	/* remote_copy */
    0,	/* get */
    0,	/* set */
    _kind_engine,				/* kind */
    (int (*)(t_ext_ptr)) _lock_engine,		/* lock */
    (int (*)(t_ext_ptr)) _trylock_engine,	/* trylock */
    (int (*)(t_ext_ptr)) _unlock_engine		/* unlock */
};


/*----------------------------------------------------------------------*/

/*
 * Option list to t_eclipse_options structure
 */

static int
_options_from_list(value v, type t, t_eclipse_options *eng_opts)
{
    Check_List(t);
    if (IsList(t)) {
	pword *car = v.ptr;
	while (1) {
	    pword *cdr = car+1;
	    Dereference_(car);
	    Dereference_(cdr);
	    if (IsAtom(car->tag)) {
		if (car->val.did == d_thread_)
		    eng_opts->init_flags |= INIT_ASYNC;
		else if (car->val.did == d_clone_)
		    eng_opts->init_flags |= INIT_CLONE;
		else if (car->val.did == d_detached_)
		    eng_opts->vm_options |= ENG_DETACHED;
		else if (car->val.did == d_verbose_)
		    eng_opts->vm_options |= ENG_VERBOSE;
		else
		    return RANGE_ERROR;
	    } else {
		pword *arg;
		dident d;
		Check_Structure(car->tag);
		d = car->val.ptr->val.did;
		arg = car->val.ptr + 1;
		Dereference_(arg);

		if (d == d_.local) {
		    Check_Integer(arg->tag);
		    eng_opts->localsize = (uword) arg->val.nint * 1024;

		} else if (d == d_.global) {
		    Check_Integer(arg->tag);
		    eng_opts->globalsize = (uword) arg->val.nint * 1024;

		} else if (d == d_thread1_) {
		    Check_Atom(arg->tag);
		    if (arg->val.did = d_.true0)
			eng_opts->init_flags |= INIT_ASYNC;
		    else if (arg->val.did = d_.false0)
			eng_opts->init_flags &= ~INIT_ASYNC;
		    else return RANGE_ERROR;

		} else if (d == d_clone1_) {
		    Check_Atom(arg->tag);
		    if (arg->val.did = d_.true0)
			eng_opts->init_flags |= INIT_CLONE;
		    else if (arg->val.did = d_.false0)
			eng_opts->init_flags &= ~INIT_CLONE;
		    else return RANGE_ERROR;

		} else if (d == d_detached1_) {
		    Check_Atom(arg->tag);
		    if (arg->val.did = d_.true0)
			eng_opts->vm_options |= ENG_DETACHED;
		    else if (arg->val.did = d_.false0)
			eng_opts->vm_options &= ~ENG_DETACHED;
		    else return RANGE_ERROR;

		} else {
		    return RANGE_ERROR;
		}
	    }
	    if (IsList(cdr->tag))
	        car = cdr->val.ptr;
	    else if (IsNil(cdr->tag))
	        break;
	    else
	        return IsRef(cdr->tag) ? INSTANTIATION_FAULT : TYPE_ERROR;
	}
    }
    return PSUCCEED;
}


/*
 * engine_create(-Engine, +Options)
 */

static int
p_engine_create(value v, type t, value vopt, type topt, ec_eng_t *ec_eng)
{
    int res;
    t_eclipse_options opts = ec_eng->options;	/* inherit */
    ec_eng_t *new_eng;
    pword handle;

    /* decode the options argument */
    res = _options_from_list(vopt, topt, &opts);
    Return_If_Not_Success(res);

    /* create and return engine */
    res = ecl_engine_create(&opts, ec_eng, &new_eng);
    Return_If_Not_Success(res);
    ecl_relinquish_engine(new_eng);
    handle = ecl_handle(ec_eng, &engine_tid, (t_ext_ptr) new_eng);
    Return_Unify_Pw(v, t, handle.val, handle.tag);
}


/*
 * Get the status of engine eng
 * The engine must either be owned or free and locked.
 *
 * This can be called with phase==ENG_LOADING, but only when the
 * caller knows that A[1..2] contain a valid status (set status_known=1).
 */

static int
_encode_result(ec_eng_t *ec_eng, ec_eng_t *eng, int status_known, pword *pw)
{
    int res;
    pword *parg;

#if 0
    if (!status_known) {
	if (!EngIsFree(eng) && !EngIsOurs(eng))
	{
	    Make_Atom(pw, EngIsPaused(eng)? d_paused_ : d_running_);
		return PRUNNING;
	}
    }
#endif

    /* get the actual status code from the Prolog level */
    parg = &eng->emu_args[1];
    Dereference_(parg)
    res = IsInteger(parg->tag) ? parg->val.nint : TYPE_ERROR;

    switch(res) {
    case PSUCCEED:
	Make_Atom(pw, d_.true0);
	break;
    case PFAIL:
	Make_Atom(pw, d_.false0);
	break;
    case PTHROW:
	Make_Struct(pw, TG);
	Push_Struct_Frame(d_exception1_);
	goto _get_arg1_;
    case PEXITED:
_exited_:
	Make_Struct(pw, TG);
	Push_Struct_Frame(d_exited1_);
	goto _get_arg1_;
    case PYIELD:
	Make_Struct(pw, TG);
	Push_Struct_Frame(d_yielded1_);
	goto _get_arg1_;
    case PFLUSHIO:
	Make_Struct(pw, TG);
	Push_Struct_Frame(d_flushio1_);
	goto _get_arg1_;
    case PWAITIO:
	Make_Struct(pw, TG);
	Push_Struct_Frame(d_waitio1_);
_get_arg1_:
	if (IsSimple(eng->emu_args[2].tag))
	    pw->val.ptr[1] = eng->emu_args[2];
	else if (PSUCCEED !=
	    ec_copy_term_across(eng, ec_eng, eng->emu_args[2].val, eng->emu_args[2].tag, &pw->val.ptr[1], 0))
	{
	    Make_Atom(&pw->val.ptr[1], d_.question);
	}
	break;
    default:
	assert(res < 0);
	break;

    }
    return res;
}


/*
 * engine_resume(+Engine, +Term, -Status)@Module
 */

static int
p_engine_resume3(value v, type t, value vin, type tin, value vs, type ts, value vm, type tm, ec_eng_t *ec_eng)
{
    int res;
    pword term, module, result;
    ec_eng_t *eng;
    Get_Typed_Object(v, t, &engine_tid, eng);

    term.val = vin;
    term.tag = tin;
    module.val = vm;
    module.tag = tm;
    res = ecl_acquire_engine(eng);
    Return_If_Error(res==PFAIL?ENGINE_BUSY:res);
    res = ecl_copy_resume(ec_eng, eng, term, module);
    res = _encode_result(ec_eng, eng, 1, &result);
    ecl_relinquish_engine(eng);	/* if not dead */
    Return_If_Error(res)
    Return_Unify_Pw(vs, ts, result.val, result.tag);
}


#if 0
/*
 * engine_handle_events(+Engine, -Status)
 * Might not be needed because the difference from
 * engine_resume(Engine, true, Status) is only that the previous
 * engine status is preserved -- iff all the event handlers succeed.
 * I.e. in this case Status is the previous status, instead of 'true'.
 */

static int
p_engine_handle_events(value v, type t, value vs, type ts, ec_eng_t *ec_eng)
{
    int res;
    pword result;
    ec_eng_t *eng;
    Get_Typed_Object(v, t, &engine_tid, eng);

    res = ecl_acquire_engine(eng);
    Return_If_Error(res==PFAIL?ENGINE_BUSY:res);
    res = ecl_handle_events(ec_eng);
    Return_If_Error(res);
    res = _encode_result(ec_eng, eng, 1, &result);
    if (res != PEXITED)
	ecl_relinquish_engine(eng);
    Return_If_Error(res)
    Return_Unify_Pw(vs, ts, result.val, result.tag);
}
#endif


/*
 * engine_resume_thread(+Engine, +Term)@Module
 */

static int
p_engine_resume_thread(value v, type t, value vin, type tin, value vm, type tm, ec_eng_t *ec_eng)
{
    int res;
    pword term, module, result;
    ec_eng_t *eng;
    Get_Typed_Object(v, t, &engine_tid, eng);

    term.val = vin;
    term.tag = tin;
    module.val = vm;
    module.tag = tm;
    res = ecl_acquire_engine(eng);
    Return_If_Error(res==PFAIL?ENGINE_BUSY:res);
    res = ecl_copy_resume_async(ec_eng, eng, term, module);
    if (res != PSUCCEED)
	ecl_relinquish_engine(ec_eng);
    /* else engine has been handed over to its own thread */
    return res;
}


/*
 * engine_status(+Engine, -Status)
 * This works without acquiring the engine.
 */

static int
_engine_status(ec_eng_t *ec_eng, ec_eng_t *eng, pword *result)
{
    int res = PSUCCEED;
    mt_mutex_lock(&eng->lock);
    if (EngIsFree(eng) || EngIsDead(eng)) {
	res = _encode_result(ec_eng, eng, 1, result);
    } else {
	Make_Atom(result, EngIsPaused(eng)? d_paused_ : d_running_);
    }
    mt_mutex_unlock(&eng->lock);
    return res;
}

static int
p_engine_status(value v, type t, value vs, type ts, ec_eng_t *ec_eng)
{
    int res;
    pword result;
    ec_eng_t *eng;

    Get_Typed_Object(v, t, &engine_tid, eng);
    res = _engine_status(ec_eng, eng, &result);
    Return_If_Error(res)
    Return_Unify_Pw(vs, ts, result.val, result.tag);
}


/**
 * engine_join(+Engine, +TimeoutInSeconds, -Status)
 */

static int
p_engine_join(value v, type t, value vto, type tto, value vs, type ts, ec_eng_t *ec_eng)
{
    int res;
    pword result;
    ec_eng_t *eng;
    word timeout_ms; 

    if (IsInteger(tto))
	timeout_ms = 1000 * vto.nint;
    else if (IsDouble(tto))
	timeout_ms = (word) (1000.0 * Dbl(vto));
    else if (IsAtom(tto) && vto.did == d_.block)
	timeout_ms = -1;
    else {
	Error_If_Ref(tto);
	Bip_Error(TYPE_ERROR);
    }
    if (timeout_ms < 0 && !IsAtom(tto)) {
	Bip_Error(RANGE_ERROR);
    }

    Get_Typed_Object(v, t, &engine_tid, eng);
    res = ecl_join_acquire(eng, timeout_ms);
    if (res != PSUCCEED) {
	if (res == PRUNNING) {
	    Fail_;	/* for timeout */
	}
	Bip_Error(res);
    }
    assert(EngIsOurs(eng));
    res = _encode_result(ec_eng, eng, 1, &result);
    if (res != PEXITED)
	ecl_relinquish_engine(eng);
    Return_If_Error(res)
    Return_Unify_Pw(vs, ts, result.val, result.tag);
}


/**
 * engine_self(-Engine) for get_flag(engine, -Engine).
 * Not so useful, because this engine is not in a yielded state.
 * TODO: there is no need to make a full copy the engine reference here,
 * as it is a weak reference (this handle should not keep the engine alive).
 */

static int
p_engine_self(value v, type t, ec_eng_t *ec_eng)
{
    pword handle = ecl_handle(ec_eng, &engine_tid, (t_ext_ptr) ecl_copy_engine(ec_eng));
    Return_Unify_Pw(v, t, handle.val, handle.tag);
}


/*
 * engine_post_event(+Engine, +Event)
 */

static int
p_engine_post_event(value v, type t, value vevent, type tevent, ec_eng_t *ec_eng)
{
    int res;
    pword event;
    ec_eng_t *eng;
    event.val = vevent;
    event.tag = tevent;
    Get_Typed_Object(v, t, &engine_tid, eng);

    mt_mutex_lock(&eng->lock);
    if (IsAtom(tevent) && vevent.did == d_.abort)
	/* will also work for looping unifications etc, via Poll_Interrupts() */
	res = ecl_request_throw(eng, event);
    else
	res = ecl_post_event(eng, event);
    mt_mutex_unlock(&eng->lock);
    Return_If_Error(res);
    Succeed_;
}



static int
p_engine_request(value v, type t, value vcode, type tcode, ec_eng_t *ec_eng)
{
    ec_eng_t *eng;
    Check_Integer(tcode);
    Get_Typed_Object(v, t, &engine_tid, eng);
    (void) ecl_request(eng, (int)vcode.nint);
    Succeed_;
}


static int
p_engine_exit(value v, type t, value vcode, type tcode, ec_eng_t *ec_eng)
{
    int res;
    ec_eng_t *eng;
    Check_Integer(tcode);
    Get_Typed_Object(v, t, &engine_tid, eng);
    res = ecl_request_exit(eng, (int)vcode.nint);
    assert(res==PRUNNING || res==PEXITED);
    Succeed_;
}


static int
p_broadcast_exit(value v, type t, ec_eng_t *ec_eng)
{
    ec_eng_t *eng;
    Check_Integer(t);

    /* send the request to all engines */
    mt_mutex_lock(&shared_data->engine_list_lock);
    shared_data->shutdown_in_progress = 1;
    eng = eng_chain_header;
    do {
	ec_eng_t *next = eng->next;
	if (eng == ec_eng) {
	    DbgPrintf("Ignoring self-exit request in engine %x!\n", ec_eng);
	} else if (!(eng->vm_flags & ENG_HIDDEN)) {
	    /* Caution: this may unlink eng from the global list! */
	    ec_eng_t *eng_copy = ecl_resurrect_engine(eng);
	    (void) ecl_request_exit(eng_copy, (int)v.nint);
	    ecl_free_engine(eng_copy, 0);
	}
	eng = next;
    } while(eng != eng_chain_header);
    mt_mutex_unlock(&shared_data->engine_list_lock);
    Succeed_;
}


/**
 * current_engines(-Engines)
 * Return a list of engines that currently have strong references.
 * This may include engines that are already exited and dying/dead.
 * Hidden (internally used) engines are not returned.
 */

static int
p_current_engines(value v, type t, ec_eng_t *ec_eng)
{
    pword result;
    pword *pw = &result;
    ec_eng_t *eng = eng_chain_header;

    mt_mutex_lock(&shared_data->engine_list_lock);
    do {
#undef SHOW_HIDDEN_ENGINES
#ifndef SHOW_HIDDEN_ENGINES
	if (!(eng->vm_flags & ENG_HIDDEN))
#endif
	{
	    ec_eng_t *eng_copy = ecl_resurrect_engine(eng);
	    if (eng_copy)
	    {
		Make_List(pw, TG);
		pw = TG;
		Push_List_Frame();
		*pw++ = ecl_handle(ec_eng, &engine_tid, (t_ext_ptr) eng_copy);
	    }
	}
	eng = eng->next;
    } while(eng != eng_chain_header);
    Make_Nil(pw);
    mt_mutex_unlock(&shared_data->engine_list_lock);
    Return_Unify_Pw(v, t, result.val, result.tag);
}


/*
 * engine_properties(+Engine,-Properties)
 */

static int
p_engine_properties(value v, type t, value vprops, type tprops, ec_eng_t *ec_eng)
{
    int res;
    ec_eng_t *eng;
    pword *old_tg = TG;
    pword result;
    pword *plist = &result;
    pword *pw;

    Get_Typed_Object(v, t, &engine_tid, eng);

#define Make_Elem(p,wdid) \
	Make_List(plist, TG); \
	plist = TG; Push_List_Frame(); \
	Make_Struct(plist, TG); plist++; \
	p = TG; Push_Struct_Frame(wdid)

    Make_Elem(pw, d_status1_);
    res = _engine_status(ec_eng, eng, &pw[1]);
    if (res < 0) { TG = old_tg; Bip_Error(res); }

    Make_Elem(pw, d_references1_);
    Make_Integer(&pw[1], eng->ref_ctr);

    Make_Elem(pw, d_thread1_);
    Make_Atom(&pw[1], eng->own_thread ? d_.true0 : d_.false0);

    Make_Elem(pw, d_detached1_);
    Make_Atom(&pw[1], eng->options.vm_options & ENG_DETACHED ? d_.true0 : d_.false0);

    Make_Elem(pw, d_.local);
    Make_Integer(&pw[1], eng->options.localsize/1024);

    Make_Elem(pw, d_.global);
    Make_Integer(&pw[1], eng->options.globalsize/1024);

    Make_Nil(plist);
    Return_Unify_Pw(vprops, tprops, result.val, result.tag);
}


/*----------------------------------------------------------------------
 * Prolog-level locking
 *----------------------------------------------------------------------*/

/*
 * ecl_trail_undo(ec_eng, _handle_unlock, v.ptr, NULL, NULL, 0, 0);
 * The trailed item is the duplicated stack anchor of the locked object.
 * If it has already been untrailed, it is stale, and we do nothing here.
 */

static void
_handle_unlock(pword *panchor, word *pdata, int size, int undo_context, ec_eng_t *ec_eng)
{
    assert(panchor != NULL);
    assert(IsTag(panchor[0].tag.kernel, TEXTERN));
    assert(IsTag(panchor[1].tag.kernel, TPTR));
    if (ExternalData(panchor)) {
	DbgPrintf("Untrail, unlocking\n", ExternalData(panchor));
	if (!ExternalClass(panchor)->unlock(ExternalData(panchor))) {
	    assert(0);
	}
    } else {
	DbgPrintf("Untrail, stale, no unlocking\n", 0);
    }
}


static int
p_handle_lock_trailed(value v, type t, value vflag, type tflag, ec_eng_t *ec_eng)
{
    pword result;

    Check_Type(t, THANDLE);
    if (!ExternalClass(v.ptr)->lock) {
	Bip_Error(UNIMPLEMENTED);
    }
    DbgPrintf("Locking 0x%x\n", ExternalData(v.ptr));
    if (!ExternalClass(v.ptr)->lock(ExternalData(v.ptr))) {
	Bip_Error(SYS_ERROR);
    }
    result = ecl_handle(ec_eng, ExternalClass(v.ptr),
		    ExternalClass(v.ptr)->copy(ExternalData(v.ptr)));
    ecl_trail_undo(ec_eng, _handle_unlock, result.val.ptr, NULL, NULL, 0, 0);
    Return_Unify_Pw(vflag, tflag, result.val, result.tag);
}


static int
p_handle_unlock_free(value v, type t, ec_eng_t *ec_eng)
{
    int res;

    Check_Type(t, THANDLE);
    if (!ExternalClass(v.ptr)->unlock) {
	Bip_Error(UNIMPLEMENTED);
    }
    if (ExternalData((v).ptr)) {
	/* handle still valid: unlock */
	DbgPrintf("Unlocking 0x%x\n", ExternalData(v.ptr));
	if (!ExternalClass(v.ptr)->unlock(ExternalData(v.ptr))) {
	    Bip_Error(SYS_ERROR);
	}
	/* free the handle, indicating that we must not unlock again */
	return p_handle_free(v, t, ec_eng);
    } else {
	DbgPrintf("Not unlocking, stale\n", 0);
	Succeed_;
    }
}


#if 0
static int
p_handle_lock(value v, type t, ec_eng_t *ec_eng)
{
    Check_Type(t, THANDLE);
    if (!ExternalClass(v.ptr)->lock) {
	Bip_Error(UNIMPLEMENTED);
    }
    if (!ExternalClass(v.ptr)->lock(ExternalData(v.ptr))) {
	Bip_Error(SYS_ERROR);
    }
    Succeed_;
}

static int
p_handle_trylock(value v, type t, ec_eng_t *ec_eng)
{
    int res;
    Check_Type(t, THANDLE);
    if (!ExternalClass(v.ptr)->trylock) {
	Bip_Error(UNIMPLEMENTED);
    }
    res = ExternalClass(v.ptr)->trylock(ExternalData(v.ptr));
    if (!res) {
	Bip_Error(SYS_ERROR);
    }
    Succeed_If(res>0);
}

static int
p_handle_unlock(value v, type t, ec_eng_t *ec_eng)
{
    Check_Type(t, THANDLE);
    if (!ExternalClass(v.ptr)->unlock) {
	Bip_Error(UNIMPLEMENTED);
    }
    if (!ExternalClass(v.ptr)->unlock(ExternalData(v.ptr))) {
	Bip_Error(SYS_ERROR);
    }
    Succeed_;
}
#endif


static int
p_handle_proceed(value v, type t, value vall, type tall, ec_eng_t *ec_eng)
{
    int err, all;

    Check_Type(t, THANDLE);
    if (!ExternalClass(v.ptr)->signal) { Bip_Error(UNIMPLEMENTED); }
    Check_Atom(tall);
    if (vall.did==d_.all) all=1;
    else if (vall.did==d_.one) all=0;
    else { Bip_Error(RANGE_ERROR); }

    err = ExternalClass(v.ptr)->signal(ExternalData(v.ptr), all);
    if (err) {
	Set_Sys_Errno(err,ERRNO_OS);
	Bip_Error(SYS_ERROR);
    }
    Succeed_;
}

static int
p_handle_wait(value v, type t, value vtimeout, type ttimeout, ec_eng_t *ec_eng)
{
    int err, timeout_ms;

    Check_Type(t, THANDLE);
    if (!ExternalClass(v.ptr)->wait) { Bip_Error(UNIMPLEMENTED); }
    if (IsAtom(ttimeout) && vtimeout.did == d_.block) {
    	timeout_ms = -1;
    } else {
	Get_Milliseconds(vtimeout, ttimeout, timeout_ms);
	if (timeout_ms < 0) { Bip_Error(RANGE_ERROR); }
    }

    ecl_pause_engine(ec_eng, 2L, PAUSE_EXITABLE_VIA_LONGJMP);
    err = ExternalClass(v.ptr)->wait(ExternalData(v.ptr), timeout_ms);
    ecl_unpause_engine(ec_eng);
    if (err > 0) {
	Set_Sys_Errno(err,ERRNO_OS);
	Bip_Error(SYS_ERROR);
    }
    Succeed_If(err==0);		/* fail if timeout */
}



static int
p_is_handle(value v, type t, value vk, type tk, ec_eng_t *ec_eng)
{
    dident kind;
    if (!IsTag(t.kernel, THANDLE)) {
	Fail_;	/* like is_handle/1 type test, fail even for variables */
    }
    if (!ExternalClass(v.ptr)->kind)
    	kind = d_.question;
    else
    	kind = ExternalClass(v.ptr)->kind();
    Return_Unify_Atom(vk, tk, kind);
}


/*----------------------------------------------------------------------
 * Initialisation
 *----------------------------------------------------------------------*/

void
bip_engines_init(int flags)
{
    d_thread_ = in_dict("thread",0);
    d_thread1_ = in_dict("thread",1);
    d_clone_ = in_dict("clone",0);
    d_clone1_ = in_dict("clone",1);
    d_detached_ = in_dict("detached",0);
    d_detached1_ = in_dict("detached",1);
    d_engine_ = in_dict("engine",0);
    d_exception1_ = in_dict("exception",1);
    d_exited1_ = in_dict("exited",1);
    d_flushio1_ = in_dict("flushio",1);
    d_references1_ = in_dict("references",1);
    d_paused_ = in_dict("paused",0);
    d_running_ = in_dict("running",0);
    d_status1_ = in_dict("status",1);
    d_verbose_ = in_dict("verbose",0);
    d_waitio1_ = in_dict("waitio",1);
    d_yielded1_ = in_dict("yielded",1);

    if (flags & INIT_SHARED)
    {
	(void) built_in(in_dict("engine_create", 2), p_engine_create, B_SAFE);
	(void) built_in(in_dict("engine_resume_", 4), p_engine_resume3, B_SAFE);
#if 0
	(void) built_in(in_dict("engine_handle_events", 2), p_engine_handle_events, B_SAFE);
#endif
	(void) built_in(in_dict("engine_resume_thread_", 3), p_engine_resume_thread, B_SAFE);
	(void) built_in(in_dict("engine_join", 3), p_engine_join, B_SAFE);
	(void) built_in(in_dict("engine_status", 2), p_engine_status, B_SAFE);
	(void) built_in(in_dict("engine_self", 1), p_engine_self, B_SAFE);
	(void) built_in(in_dict("engine_post_event", 2), p_engine_post_event, B_SAFE);
	(void) built_in(in_dict("engine_properties", 2), p_engine_properties, B_SAFE);
	(void) built_in(in_dict("current_engines", 1), p_current_engines, B_SAFE);

	(void) built_in(in_dict("broadcast_exit", 1), p_broadcast_exit, B_SAFE);
	(void) built_in(in_dict("engine_exit", 2), p_engine_exit, B_SAFE);
	(void) built_in(in_dict("engine_request", 2), p_engine_request, B_SAFE);

	(void) built_in(in_dict("is_handle", 2), p_is_handle, B_SAFE);
#if 0
	(void) built_in(in_dict("handle_lock", 1), p_handle_lock, B_SAFE);
	(void) built_in(in_dict("handle_trylock", 1), p_handle_trylock, B_SAFE);
	(void) built_in(in_dict("handle_unlock", 1), p_handle_unlock, B_SAFE);
#endif
	(void) built_in(in_dict("handle_lock_trailed", 2), p_handle_lock_trailed, B_SAFE);
	(void) built_in(in_dict("handle_unlock_free", 1), p_handle_unlock_free, B_SAFE);
	(void) built_in(in_dict("handle_proceed", 2), p_handle_proceed, B_SAFE);
	(void) built_in(in_dict("handle_wait", 2), p_handle_wait, B_SAFE);
	(void) built_in(in_dict("handle_abolish", 1), p_handle_free, B_SAFE);
    }
}

/* Add all new code in front of the initialization function! */
