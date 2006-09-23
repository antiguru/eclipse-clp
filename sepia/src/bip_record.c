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
 * VERSION	$Id: bip_record.c,v 1.1 2006/09/23 01:55:50 snovello Exp $
 */

/* ********************************************************************
 *
 *	Sepia built-ins for the indexed database
 *
 *
 ******************************************************************** */


#include	"config.h"
#include        "sepia.h"
#include        "types.h"
#include        "embed.h"
#include        "mem.h"
#include        "error.h"
#include        "io.h"
#include	"debug.h"
#include	"dict.h"
#include	"database.h"
#include	"emu_export.h"
#include	"property.h"
#include	"module.h"

#include <stdio.h>	/* for sprintf() */

#define 	RECORDA		0
#define		RECORDZ		1
#define		REF_WANTED	2

#define Check_Output_Db_Ref(item)	Check_Output_Type(item, TDBREF)


#define NewListCell()		((pword *) hg_alloc_size(2*sizeof(pword)))

#ifdef DEBUG_RECORD
#define Free_List_Cell(p)	hg_free_size((generic_ptr)(p), 2*sizeof(pword));\
				p_fprintf(current_output_,"free %x\n", p);
#define Save_List_Cell(p)	(p)->val.ptr = ErasedDbRefs;\
				ErasedDbRefs = (p);\
				p_fprintf(current_output_,"save %x\n", p);
#define Debug_NewListCell(p)	p_fprintf(current_output_,"new %x\n", p);
#else /* DEBUG_RECORD */
#define Free_List_Cell(p)	hg_free_size((generic_ptr)(p), 2*sizeof(pword));
#define Save_List_Cell(p)	(p)->val.ptr = ErasedDbRefs;\
				ErasedDbRefs = (p);
#define Debug_NewListCell(p)
#endif /* DEBUG_RECORD */

#define IsErased(tag)	SameTypeC(tag, TEND)

/* this flag is kept in the tag of the first pword of the difference list
 * cell. If set it indicates that there are database references to all
 * or some of the records in the corrsponding key. In that case we do
 * not free the list cells, but move them to ErasedDbRefs.
 */
#define		DB_REFERENCED	0x100

/* The global (shared) list ErasedDbRefs holds 4 word cells that belong to
 * record items that have been erased with erase/1. They are kept because
 * they may still be referenced by bloody database references!
 * They are linked by their first words, the second is a TEND tag,
 * then follows a pword that holds the old link to the next recorded item.
 * This memory is not reclaimed so far since it would involve an
 * exhaustive scan for db-references in the whole system.
 * free_erased_db_refs/0 does it anyway.
 */

static int
#ifdef ANONYMOUS_RECORDS
		p_rec_create(),
		p_rec_enter_a(),
		p_rec_enter_z(),
		p_rec_list(),
		p_rec_all(),
		p_rec_term(),
		p_rec_erase(),
#endif
		p_erase_record(value vkey, type tkey, value vdref, type tdref, value vmod, type tmod),
		p_erase(value vdref, type tdref),
		p_erase_all_body(value vkey, type tkey, value vmod, type tmod),
		p_free_erased_db_refs(void),
		p_is_record_(value vkey, type tkey, value vv, type tv, value ve, type te, value vmod, type tmod),
		p_recorda_body(value vkey, type tkey, value vvalue, type tvalue, value vmod, type tmod),
		p_recordz_body(value vkey, type tkey, value vvalue, type tvalue, value vmod, type tmod),
		p_recorda3_body(value vkey, type tkey, value vvalue, type tvalue, value vdref, type tdref, value vmod, type tmod),
		p_recordz3_body(value vkey, type tkey, value vvalue, type tvalue, value vdref, type tdref, value vmod, type tmod),
		p_referenced_record(value vdref, type tdref, value vinst, type tinst),
		p_recorded_list_body(value vkey, type tkey, value vlist, type tlst, value vmod, type tmod),
		p_local_record_body(value vkey, type tkey, value vmod, type tmod),
		p_global_record_body(value vkey, type tkey, value vmod, type tmod),
		p_abolish_record_body(value vkey, type tkey, value vmod, type tmod),
		p_valid_key(value v, type t),
		p_get_recordlist(value vkey, type tkey, value vlist, type tlst, value vflag, type tflag, value vmod, type tmod),
		p_get_record(value vlist, type tlst, value vvalue, type tvalue, value vdref, type tdref);

static dident	d_visible_;
pword		*p_meta_attribute_;


void
bip_record_init(int flags)
{
    pri		*pd;
    type	t;
    value	v1, v2;
    int		res;

    d_visible_ = in_dict("visible", 0);
    tdbref.kernel = TDBREF;

    if (flags & INIT_SHARED)
    {
	ErasedDbRefs = (pword *) 0;

	exported_built_in(in_dict("recorded_list_body", 3),
			p_recorded_list_body, B_UNSAFE|U_FRESH)
	    -> mode = BoundArg(2, NONVAR);
#ifdef ANONYMOUS_RECORDS
	(void) exported_built_in(in_dict("rec_create", 1), p_rec_create, B_SAFE|U_SIMPLE);
	(void) exported_built_in(in_dict("rec_enter_a", 2), p_rec_enter_a, B_SAFE|U_NONE);
	(void) exported_built_in(in_dict("rec_enter_z", 2), p_rec_enter_z, B_SAFE|U_NONE);
	(void) exported_built_in(in_dict("rec_all", 2), p_rec_all, B_SAFE|U_UNIFY);
	(void) exported_built_in(in_dict("rec_list", 2), p_rec_list, B_SAFE|U_UNIFY);
	(void) exported_built_in(in_dict("rec_term", 2), p_rec_term, B_SAFE|U_UNIFY);
	(void) exported_built_in(in_dict("rec_erase", 1), p_rec_erase, B_SAFE|U_NONE);
	(void) exported_built_in(in_dict("rec_abolish", 1), p_handle_free, B_SAFE|U_NONE);
#endif

	(void) local_built_in(in_dict("valid_key", 1), p_valid_key, B_SAFE|U_SIMPLE);
	(void) local_built_in(in_dict("erase_record", 3),p_erase_record, B_UNSAFE);
	(void) exported_built_in(in_dict("erase_all_body", 2),
				 p_erase_all_body, B_UNSAFE);
	(void) exported_built_in(in_dict("is_record_", 4),
				 p_is_record_, B_UNSAFE);
	(void) exported_built_in(in_dict("recorda_body", 3),
				 p_recorda_body, B_UNSAFE);
	pd = exported_built_in(in_dict("recordz_body", 3),
			       p_recordz_body, B_UNSAFE);
	(void) exported_built_in(in_dict("recorda_body", 4),
				 p_recorda3_body, B_UNSAFE);
	(void) exported_built_in(in_dict("recordz_body", 4),
				 p_recordz3_body, B_UNSAFE);
	(void) built_in(in_dict("erase", 1), p_erase, B_UNSAFE);
	built_in(in_dict("referenced_record", 2),
				    p_referenced_record, B_UNSAFE|U_FRESH)
	    -> mode = BoundArg(2, NONVAR);
	pd = local_built_in(in_dict("get_recordlist", 4),
			    p_get_recordlist, B_UNSAFE|U_FRESH);
	pd->mode = BoundArg(2, NONVAR);
	(void) b_built_in(in_dict("get_record", 3), p_get_record, d_.kernel_sepia);
	(void) exported_built_in(in_dict("local_record_body", 2),
				 p_local_record_body, B_UNSAFE);
	(void) local_built_in(in_dict("global_record_body", 2),
				 p_global_record_body, B_UNSAFE);
	(void) exported_built_in(in_dict("abolish_record_body", 2),
				 p_abolish_record_body, B_UNSAFE);
	(void) exported_built_in(in_dict("free_erased_db_refs", 0),
				 p_free_erased_db_refs, B_SAFE);
    }

    t.kernel = ModuleTag(d_.kernel_sepia);
    v1.did = in_dict("meta_attribute", 0);
    v2.did = d_.kernel_sepia;
    if (flags & INIT_SHARED)
    {
	(void) p_local_record_body(v1, tdict, v2, t);
    }
    p_meta_attribute_ = get_modular_property(v1.did,
			IDB_PROP, v2.did, t, LOCAL_PROP, &res);
}


/*
	is_record_(Key, Visibility, Empty, Module)
	checks whether an atom is a key on which recorded terms have been
	(and still are) stored.
*/
static int
p_is_record_(value vkey, type tkey, value vv, type tv, value ve, type te, value vmod, type tmod)
{
   pword	*prop;
   dident	key_did;
   int		vis;
   int		err;

   Get_Key_Did(key_did,vkey,tkey);
   Check_Atom(tv);
   if (vv.did == d_.local0)
	vis = LOCAL_PROP;
    else if (vv.did == d_visible_)
	vis = VISIBLE_PROP;
    else {
	Bip_Error(RANGE_ERROR)
    }

   a_mutex_lock(&PropertyLock);

    prop = get_modular_property(key_did, IDB_PROP, vmod.did, tmod, vis, &err);

   if (prop)
   {
       if (IsList(prop->tag))
       {
	   if (!IsList((prop -> val.ptr) -> tag)) {  /* empty */
	       a_mutex_unlock(&PropertyLock);
	       Return_Unify_Nil(ve, te);
	    } else {
	       a_mutex_unlock(&PropertyLock);
	       Succeed_;
	    }
       }
   }
   else if (err != PERROR)
   {
       a_mutex_unlock(&PropertyLock);
       Bip_Error(err);
   }
   a_mutex_unlock(&PropertyLock);
   Fail_;
}
  

/*

	do_record(flag, val_did, vvalue, tvalue, vmod, tmod)
	auxiliary function for recorda_body/3, recordz_body/3.

	record given value under val_did
	flag is RECORDA or RECORDZ

	Record values are organized as a prolog list.
	In order to speed up adding an element to the end of this list,
	we also update a pointer to the last pair.

	[[]|[]]         no entries

	[[d]|[a,b,c,d]] entries a,b,c,d

	It makes the concatenation on the heap of a new value with the
	previous one. It has been defined for the indexed database.
	It is called with the key as first argument, the value to record as
	second argument and a boolean (0/1) whether the value is added at
	the head (recorda) or at the tail (record).

	This uses the first argument as a global variable whose value is
	the list of recorded items:
		array	-->	head->val.ptr
				TLIST = head->tag
				tail
		head    -->	first element of the list
	
*/

static int
do_record(int flag, dident val_did, value vvalue, type tvalue, value vdref, type tdref, value vmod, type tmod)
{
    register pword	*prop, *p, *first;
    int			err;

    /* 
     * Make the new list cell and copy the term.
     * This is still interruptable.
     */
    p = NewListCell();
    Debug_NewListCell(p);
    if ((err = create_heapterm(p,vvalue,tvalue)) != PSUCCEED)
    {
	Free_List_Cell(p);
	Bip_Error(err);
    }

    /* 
     * Link the new cell p into the record property list.
     * This must be atomic.
     */
    a_mutex_lock(&PropertyLock);
    prop = GetPointerToRecord(val_did, vmod.did, tmod, &err);

    if (prop == (pword *) NULL)	/* no entries yet */
    {
	if (err == PERROR)
	    err = NO_LOCAL_REC;
	goto _do_record_err_;
    } 
    else if (IsList(prop -> tag)) 
    {
	prop = prop -> val.ptr;
    } 
    else 
    {
	err = STALE_HANDLE;
	goto _do_record_err_;
    }

    /* remove the directly erased items at the list beginning.
     * We link them using the value part of the TEND pword.
     * Important: The second pword is not affected!
     */
    first = prop + 1;
    while (!IsNil(first->tag) && IsErased(first->val.ptr->tag))
    {
	first = first->val.ptr;
	Save_List_Cell(first);
	first += 1;
    }
    /* Now link the new cell p */
    if (IsNil(first->tag))	 /* no entries ([[]|[]]) */
    {
	 Make_List(prop,p);
	 Make_List(prop + 1, p);
	 Make_Nil(p + 1);
    }
    else if (flag & RECORDZ)
    {
	Make_List((prop -> val.ptr) + 1, p);
	prop -> val.ptr = p;
	Make_Nil(p + 1);
    }
    else /* RECORDA */
    {
	*(p + 1) = *(prop + 1);
	(prop + 1) -> val.ptr = p;
    }

    if (flag & REF_WANTED)
    {
	value	pval;

	prop->tag.kernel |= DB_REFERENCED;	/* mark as referenced */
	pval.ptr = p;
	a_mutex_unlock(&PropertyLock);
	Return_Unify_Pw(vdref, tdref, pval, tdbref);
    }
    else
    {
	a_mutex_unlock(&PropertyLock);
	Succeed_;
    }

_do_record_err_:		/* (p,err) */
    free_heapterm(p);
    Free_List_Cell(p);
    a_mutex_unlock(&PropertyLock);
    Bip_Error(err);
}

static int
p_recorda_body(value vkey, type tkey, value vvalue, type tvalue, value vmod, type tmod)
{
    dident key_did;

    Get_Key_Did(key_did,vkey,tkey)
    return do_record(RECORDA, key_did, vvalue, tvalue, vvalue, tvalue,
		     vmod, tmod);
}

static int
p_recordz_body(value vkey, type tkey, value vvalue, type tvalue, value vmod, type tmod)
{
    dident key_did;

    Get_Key_Did(key_did,vkey,tkey)
    return do_record(RECORDZ, key_did, vvalue, tvalue, vvalue, tvalue,
		     vmod, tmod);
}

static int
p_recorda3_body(value vkey, type tkey, value vvalue, type tvalue, value vdref, type tdref, value vmod, type tmod)
{
    dident key_did;

    Check_Output_Db_Ref(tdref);
    Get_Key_Did(key_did,vkey,tkey)
    return do_record(RECORDA|REF_WANTED, key_did, vvalue, tvalue, vdref, tdref,
		     vmod, tmod);
}

static int
p_recordz3_body(value vkey, type tkey, value vvalue, type tvalue, value vdref, type tdref, value vmod, type tmod)
{
    dident key_did;

    Check_Output_Db_Ref(tdref);
    Get_Key_Did(key_did,vkey,tkey)
    return do_record(RECORDZ|REF_WANTED, key_did, vvalue, tvalue, vdref, tdref,
		     vmod, tmod);
}


/*
 * Support function for the dictionary garbage collector
 * Mark the DIDs that occur in the given recorded list.
 */

void
mark_dids_from_record(pword *prop)
{
    if (IsList(prop->tag))
    {
	register pword *aux;
	for (aux = prop->val.ptr + 1; IsList(aux->tag); aux = aux->val.ptr + 1)
	{
	    mark_dids_from_heapterm(aux->val.ptr);
	}
    }
}


/*
 * void erase_all_records(prop) - auxiliary function
 *
 * erase all recorded terms related to the given property word
 * This function is used by erase_all_body/2 and erase_module/1
 *
 * Warning: must be called with disabled interruptions.
 */

void
erase_all_records(pword *prop)
{
    if (!IsList(prop->tag))
	return;
    erase_records(prop);
    Free_List_Cell(prop->val.ptr);
    return;
}


/*
 * The same as above, but the difference list head cell
 * is kept and set to [[]|[]]
 */

void
erase_records(pword *prop)
{
    register pword *el, *next;

    if (!IsList(prop->tag))
	return;

    el = prop->val.ptr;
    next = el + 1;
    if (IsList(next->tag))
    {
	register long referenced = el->tag.kernel & DB_REFERENCED;
	next->tag.kernel = TNIL;	/* replace with [[]|[]]		*/
	el->tag.kernel = TNIL;

	next = next->val.ptr;
	el = next++;

	while (IsList(next->tag))
	{
	    next = next->val.ptr;
	    free_heapterm(el);	/* free the proper record	*/
	    if (referenced)		/* free the list cell		*/
	    {
		el->tag.kernel = TEND;
		Save_List_Cell(el);
	    }
	    else
		{ Free_List_Cell(el) }
	    el = next++;
	}
	free_heapterm(el);
	if (referenced)
	{
	    el->tag.kernel = TEND;
	    Save_List_Cell(el);
	}
	else
	    { Free_List_Cell(el) }
    }
    /* else no record to erase */
    return;
}


/*
	erase_all_body/2
	removes all the recorded terms associated to a key. 
	but keep the [[]|[]] list element for future use.
*/
static int
p_erase_all_body(value vkey, type tkey, value vmod, type tmod)
{
    pword	*prop;
    dident	key_did;
    int		err;

    Get_Key_Did(key_did,vkey,tkey);

    a_mutex_lock(&PropertyLock);

    prop = GetPointerToRecord(key_did, vmod.did, tmod, &err);
    if (prop)
	erase_records(prop);
    else if (err != PERROR)
    {
	a_mutex_unlock(&PropertyLock);
	Bip_Error(err);
    }
    a_mutex_unlock(&PropertyLock);
    Succeed_;
}
    

/*
 * 	get_recordlist(+Key, -List, ?Flag, +Module) - auxiliary predicate
 * 
 * 	gets the list of values associated to a key and binds the first
 *	record element to the List argument - this is a heap pointer!
 * 	The Flag argument tells us if the heap reference is going to be
 *	given to the outside (recorded/3 - Flag=Ref) or if it is only
 *	used internally (erase/2 - Flag=[]). In the first case we mark
 *	the whole key as being DB_REFERENCED.
 */
/*ARGSUSED*/
static int
p_get_recordlist(value vkey, type tkey, value vlist, type tlst, value vflag, type tflag, value vmod, type tmod)
{
    register pword	*prop, *el;
    dident		key_did;
    int			err;

    Check_Ref(tlst);
    Get_Key_Did(key_did,vkey,tkey);

    a_mutex_lock(&PropertyLock);

    if ((prop = GetPointerToRecord(key_did, vmod.did, tmod, &err)))
    {
	if (IsList(prop->tag))
	{
	    prop = prop->val.ptr;
	    if (IsList((prop+1)->tag))
	    {
		el = (prop+1)->val.ptr;
		if (IsErased(el->tag))		/* skip erased records */
		{
		    do				/* unlink erased items */
		    {
			Save_List_Cell(el);
		    	if (IsNil((el+1)->tag))
			{
			    prop->tag.kernel = TNIL;
			    (prop+1)->tag.kernel = TNIL;
			    a_mutex_unlock(&PropertyLock);
			    Fail_;		/* only erased items */
			}
			el = (el+1)->val.ptr;
		    } while (IsErased(el->tag));
		    (prop+1)->val.ptr = el;
		}
		/* el points to non-erased item */
		if (IsRef(tflag))
		    prop->tag.kernel |= DB_REFERENCED;	/* mark it */
		a_mutex_unlock(&PropertyLock);
		Return_Unify_List(vlist, tlst, el);
	    }
	}
    }
    else if (err != PERROR)
    {
	a_mutex_unlock(&PropertyLock);
	Bip_Error(err);
    }
    a_mutex_unlock(&PropertyLock);
    Fail_;
}


/*
 * recorded_list_body(key, list, module)
 *
 * builds a true prolog list (on the global stack) of all terms
 * recorded under the given key.
 * Used to implement recorded/2, but may also serve as a user builtin.
 */

static int
p_recorded_list_body(value vkey, type tkey, value vlist, type tlst, value vmod, type tmod)
{
    register pword	*prop, *s;
    pword		result;
    dident		key_did;
    int			err;

    Check_Output_List(tlst);
    Get_Key_Did(key_did,vkey,tkey);

    a_mutex_lock(&PropertyLock);

    if ((prop = GetPointerToRecord(key_did, vmod.did, tmod, &err)))
    {
	if (IsList(prop->tag))
	{
	    s = &result;
	    prop = prop->val.ptr + 1;
	    while (IsList(prop->tag))
	    {
		prop = prop->val.ptr;
		if (!IsErased(prop->tag))
		{
		    s->val.ptr = Gbl_Tg;
		    s->tag.kernel = TLIST;
		    s = Gbl_Tg;
		    Gbl_Tg += 2;
		    Check_Gc;
		    get_heapterm(prop++, s++);
		}
		else
		    prop += 1;
	    }
	    s->tag.kernel = TNIL;
	    a_mutex_unlock(&PropertyLock);
	    Return_Unify_Pw(vlist, tlst, result.val, result.tag);
	}
    }
    else if (err != PERROR)
    {
	a_mutex_unlock(&PropertyLock);
	Bip_Error(err);
    }
    a_mutex_unlock(&PropertyLock);
    Return_Unify_Nil(vlist, tlst);
}


/*
	get_record/3	 backtracking built in   (needed for erase)
	Unifies the element pointed to by vlist with vvalue.
	vlist has to be a non zero pointer to a element of the record 
	property list. vlist gets unified with vdref afterwards.
	As we go through the list we try to throw out erased items
	(they have a TEND tag) and link them into the ErasedDbRefs list.
*/
static int
p_get_record(value vlist, type tlst, value vvalue, type tvalue, value vdref, type tdref)
{
    register pword	*el,*t;
    pword		result;
    Prepare_Requests

    if (IsList(tlst))
	;
    else if (IsNil(tlst))
	{ Cut_External; Fail_; }
    else
	{ Bip_Error(TYPE_ERROR); }

    el = vlist.ptr;

    /* first skip records that have been erased since the last invocation.
     * we don't unlink the items here since we may be processing a chain
     * that is already unlinked
     */
    while (IsErased(el->tag))
    {
	if (IsNil((el+1)->tag))
	    { Cut_External; Fail_; }	/* there are only erased ones */
	el = (el+1)->val.ptr;
    }

    /* el now points to the element to return */
    t = el + 1;
    if (IsList(t->tag))
    {
    	while (IsErased(t->val.ptr->tag) && IsList((t->val.ptr + 1)->tag))
	{
	    t = t->val.ptr;
	    Save_List_Cell(t);  	 /* get rid of the erased item */
	    t += 1;
	    /* we can't unlink the last because of the difference list */
	}
	(el+1)->val.ptr = t->val.ptr;
    }

    /* t now points to the tail to remember (maybe []) */
    Remember(1, t->val, t->tag);

    get_heapterm(el, &result);
    if (!(IsRef(result.tag) && result.val.ptr == &result))
    {
	Request_Unify_Pw(vvalue,tvalue,result.val,result.tag);
    }
    Request_Unify_Pw(vdref, tdref, vlist, tdbref);
    Return_Unify;
}


/*
	erase_record/3
	Removes the element pointed to by vdref
        from the property list associated to vkey.
*/

/* ARGSUSED */
static int
p_erase_record(value vkey, type tkey, value vdref, type tdref, value vmod, type tmod)
{
    register pword	*prop,*prop2,*p;
    pword		*prev;
    dident		key_did;
    int			err;

    Get_Key_Did(key_did,vkey,tkey);

    a_mutex_lock(&PropertyLock);

    prop = GetPointerToRecord(key_did, vmod.did, tmod, &err);
    if (prop)
    {
	prop= prop->val.ptr;
	prop2= prop + 1;		/* beginning of list */

	while (IsList(prop2->tag) && prop2->val.ptr != vdref.ptr)
	    prop2 = prop2->val.ptr + 1;  /* next list element */

	if (IsList(prop2->tag))
	{
	    long referenced = prop->tag.kernel & DB_REFERENCED;

	    prev = prop2;  
	    p = prev->val.ptr;		/* element to erase	*/
	    *(prev) = *(p + 1);		/* remove the element	*/

	    if (IsNil(prev->tag))	/* it was the last element	*/
	    {
		if (IsNil((prop + 1) -> tag))	/* the only element	*/
		{ 
		    Make_Nil(prop);
		}
		else
		{
		    Make_List(prop, (prev - 1));
		    prop->tag.kernel |= referenced;
		}
	    }
	    free_heapterm(p);	/* free the proper record	*/
	    if (referenced)
	    {
		p->tag.kernel = TEND;
		Save_List_Cell(p);
	    }
	    else
		{ Free_List_Cell(p) }	/* free the list cell		*/
	}
    }
    else if (err != PERROR)
    {
	a_mutex_unlock(&PropertyLock);
	Bip_Error(err);
    }
    a_mutex_unlock(&PropertyLock);
    Succeed_;
}



/*
	erase/1
	Free the contents of the referenced item and mark it as garbage.
*/
static int
p_erase(value vdref, type tdref)
{
    if (!IsDbRef(tdref))
    {
	Error_If_Ref(tdref);
	Bip_Error(TYPE_ERROR);
    }
    if (IsErased(vdref.ptr->tag))
    {
	Fail_;
    }
    free_heapterm(vdref.ptr);
    vdref.ptr->tag.kernel = TEND;
    Succeed_;
}


/*
 * referenced_record(+DbRef, ?Term)
 *
 * return the referenced recorded term
 * this is like instance/2 in cprolog and quintus
 */

static int
p_referenced_record(value vdref, type tdref, value vinst, type tinst)
{
    pword result;

    if (!IsDbRef(tdref))
    {
	Error_If_Ref(tdref);
	Bip_Error(TYPE_ERROR);
    }
    a_mutex_lock(&PropertyLock);
    if (IsErased(vdref.ptr->tag))
    {
	a_mutex_unlock(&PropertyLock);
	Fail_;
    }
    get_heapterm(vdref.ptr, &result);
    a_mutex_unlock(&PropertyLock);
    if (IsRef(result.tag) && result.val.ptr == &result)
    {
	Succeed_;
    }
    else
    {
	Return_Unify_Pw(vinst, tinst, result.val, result.tag);
    }
}


/*
 * A last resort when running out of space due to too many erased
 * database references. After this the old references are re-used,
 * ie. no longer considered as erased !
 */
static int
p_free_erased_db_refs(void)
{
    register pword *p1, *p2;

    a_mutex_lock(&PropertyLock);
    p1 = ErasedDbRefs;
    ErasedDbRefs = (pword *) 0;
    a_mutex_unlock(&PropertyLock);

    while (p1)
    {
	p2 = p1->val.ptr;
	(p1+1)->tag.kernel = TNIL;
	Free_List_Cell(p1);
	p1 = p2;
    }
    Succeed_;
}


static int
p_local_record_body(value vkey, type tkey, value vmod, type tmod)
{
    pword	*prop, *p;
    dident	key_did;
    int		err;

    Get_Functor_Did(vkey, tkey, key_did);
    
    a_mutex_lock(&PropertyLock);

    prop = set_modular_property(key_did, IDB_PROP, vmod.did, tmod,
				LOCAL_PROP, &err);
    if (!prop)
    {
	a_mutex_unlock(&PropertyLock);
	if (err == PERROR)
	    { Succeed_; }	/* exists already */
	else
	    Bip_Error(err);
    }
    p = NewListCell();
    Debug_NewListCell(p);
    Make_Nil(p);
    Make_Nil(p + 1);
    Make_List(prop, p);

    a_mutex_unlock(&PropertyLock);
    Succeed_;
}

static int
p_global_record_body(value vkey, type tkey, value vmod, type tmod)
{
    pword	*prop, *p;
    dident	key_did;
    int		err;

    Get_Functor_Did(vkey, tkey, key_did);
    
    a_mutex_lock(&PropertyLock);

    prop = set_modular_property(key_did, IDB_PROP, vmod.did, tmod,
				GLOBAL_PROP, &err);
    if (!prop)
    {
	a_mutex_unlock(&PropertyLock);
	Bip_Error((err == PERROR) ? LOCAL_REC : err);
    }
    p = NewListCell();
    Debug_NewListCell(p);
    Make_Nil(p);
    Make_Nil(p + 1);
    Make_List(prop, p);

    a_mutex_unlock(&PropertyLock);
    Succeed_;
}

static int
p_abolish_record_body(value vkey, type tkey, value vmod, type tmod)
{
    dident	key_did;
    int		err;
    
    Get_Functor_Did(vkey, tkey, key_did);

    err = erase_modular_property(key_did, IDB_PROP, vmod.did,tmod, LOCAL_PROP);

    if (err < 0)
    {
	Bip_Error((err == PERROR) ? NO_LOCAL_REC : err);
    }
    else
	Succeed_;
}


#ifdef ANONYMOUS_RECORDS

/*----------------------------------------------------------------------
 * New record primitives:

    NOTE: The idea was to have anonymous records and key-records
    on top of them. There are problems mainly with the parallel system,
    proper reference counting (with stack copying) and locking.
    The bags above are not safe in that respect either: bag_dissolve
    assumes it is the only reference, doing the last access.
 *----------------------------------------------------------------------*/

/* INSTANCE TYPE DECLARATION */

typedef struct record_elem {
    short		ref_ctr;
    short		erased;		/* means: remove from list later */
    struct record_elem	*next;
    pword		term;		/* or last element, if header */
} t_heap_rec;


/* METHODS */


static t_heap_rec *
_rec_create(void)
{
    t_heap_rec *obj = (t_heap_rec *) hg_alloc_size(sizeof(t_heap_rec));
    obj->ref_ctr = 1;
    obj->erased = 0;
    obj->next = (t_heap_rec *) 0;
    obj->term.val.ptr = (pword *) obj;	/* pointer to last element (self) */
    obj->term.tag.kernel = TPTR;
    return obj;
}


static void
_rec_free_elem(t_heap_rec *this)
{
    if (--this->ref_ctr <= 0)
    {
	/* when this happens, the list that contained it is definitely gone */
	p_fprintf(current_err_, "\n_rec_free_elem(0x%x)", this);
	ec_flush(current_err_);
	this->next = (t_heap_rec *) 0;	/* not necessary */
	free_heapterm(&this->term);
	hg_free_size((generic_ptr) this, sizeof(t_heap_rec));
    }
}


static void
_rec_free_all(t_heap_rec *header)
{
    t_heap_rec *this = header->next;
    while (this)
    {
	t_heap_rec *next = this->next;
	this->next = (t_heap_rec *) 0;
	_rec_free_elem(this);
	this = next;
    }
    if (--header->ref_ctr <= 0)
    {
	p_fprintf(current_err_, "\n_rec_free_header(0x%x)", header);
	ec_flush(current_err_);
	hg_free_size((generic_ptr) header, sizeof(t_heap_rec));
    }
}


static t_heap_rec *
_rec_copy_elem(t_heap_rec *this)	/* this != NULL */
{
    ++this->ref_ctr;
    return this;
}

static t_heap_rec *
_rec_copy_all(t_heap_rec *header)	/* header != NULL */
{
    p_fprintf(current_err_, "\nECLiPSe: Can't copy record list");
    ec_flush(current_err_);
    return (t_heap_rec *) 0;
}

static void
_rec_mark_elem(t_heap_rec *this)	/* this != NULL */
{
    mark_dids_from_heapterm(&this->term);
}


static void
_rec_mark_all(t_heap_rec *header)	/* header != NULL */
{
    t_heap_rec *this = header->next;
    while (this)
    {
	_rec_mark_elem(this);
	this = this->next;
    }
}


static int
_rec_tostr_elem(t_heap_rec *obj, char *buf, int quoted)	/* obj != NULL */
{
#define STRSZ_DBREF 20
    sprintf(buf, "'DBREF'(16'%08x)", obj);
    return STRSZ_DBREF;
}

static int
_rec_strsz_elem(t_heap_rec *obj, int quoted) /* obj != NULL */
{
    return STRSZ_DBREF;
}


static int
_rec_tostr_all(t_heap_rec *obj, char *buf, int quoted) /* obj != NULL */
{
#define STRSZ_REC 18
    sprintf(buf, "'REC'(16'%08x)", obj);
    return STRSZ_REC;
}

static int
_rec_strsz_all(t_heap_rec *obj, int quoted) /* obj != NULL */
{
    return STRSZ_REC;
}



/* CLASS DESCRIPTOR (method table) */
t_ext_type heap_rec_tid = {
    (void (*)(t_ext_ptr)) _rec_free_elem,
    (t_ext_ptr (*)(t_ext_ptr)) _rec_copy_elem,
    (void (*)(t_ext_ptr)) _rec_mark_elem,
    (int (*)(t_ext_ptr,int)) _rec_strsz_elem,
    (int (*)(t_ext_ptr,char*,int)) _rec_tostr_elem,
    0,	/* equal */
    (t_ext_ptr (*)(t_ext_ptr)) _rec_copy_elem,
    0,	/* get */
    0	/* set */
};

t_ext_type heap_rec_header_tid = {
    (void (*)(t_ext_ptr)) _rec_free_all,
     0,		/* We would need to make a true copy here. We can't
		 * reuse the list since it gets destructively modified */
    (void (*)(t_ext_ptr)) _rec_mark_all,
    (int (*)(t_ext_ptr,int)) _rec_strsz_all,
    (int (*)(t_ext_ptr,char*,int)) _rec_tostr_all,
    0,	/* equal */
    0,	/* remote_copy */
    0,	/* get */
    0	/* set */
};


static int
_rec_all(t_heap_rec *header, int dbrefs_only, pword *result)
{
    t_heap_rec *prev, *el;

    for (prev = header; prev; prev = el)
    {
	for (el = prev->next; el && el->erased; )	/* skip junk */
	{
	    t_heap_rec *this = el;
	    el = el->next;
	    _rec_free_elem(this);
	}
	prev->next = el;				/* unlink junk */
	if (!el)
	{
	    header->term.val.ptr = (pword *) prev;	/* update tail ptr */
	}
	else	/* proper entry */
	{
	    pword *car = TG;
	    Make_List(result, car);
	    Push_List_Frame();
	    if (dbrefs_only)
		*car = ec_handle(&heap_rec_tid, (t_ext_ptr) _rec_copy_elem(el));
	    else
		get_heapterm(&el->term, car);
	    result = car+1;
	}
    }
    Make_Nil(result);
    Succeed_;
}


/* PROLOG INTERFACE */

static int
p_rec_create(value vrec, type trec)
{
    pword rec;
    Check_Ref(trec);
    rec = ec_handle(&heap_rec_header_tid, (t_ext_ptr) _rec_create());
    Return_Unify_Pw(vrec, trec, rec.val, rec.tag);
}

static int
p_rec_enter_a(value vrec, type trec, value vterm, type tterm)
{
    t_heap_rec *obj, *header;
    pword copy_pw, *pw;
    int err;

    /* 
     * Make the new list cell and copy the term.
     * This is still interruptable.
     */
    if ((err = create_heapterm(&copy_pw, vterm, tterm)) != PSUCCEED)
	{ Bip_Error(err); }
    obj = _rec_create();

    if (!SameTypeC(trec, THANDLE))
    {
	Get_Key_Did(key_did,vkey,tkey)
	a_mutex_lock(&PropertyLock);
	prop = GetPointerToRecord(val_did, vmod.did, tmod, &err);
	if (!prop)
	{
	    a_mutex_unlock(&PropertyLock);
	    return err;
	}
	vrec.all = prop->val.all;
	trec.all = prop->tag.all;
    }
    else
    {
	a_mutex_lock(&PropertyLock);
    }
    err = handle_get(vrec, trec, &heap_rec_header_tid, &header);
        if (err != PSUCCEED) goto _unlock_return_err_;

    a_mutex_lock(&SharedDataLock);
    if ((t_heap_rec *) header->term.val.ptr == header)
    {
	header->term.val.ptr = (pword *) obj;	/* update tail ptr */
    }
    obj->next = header->next;			/* insert before first */
    header->next = obj;
    move_heapterm(&copy_pw, &obj->term);
    a_mutex_unlock(&SharedDataLock);
    Succeed_;
_unlock_return_err_:
    a_mutex_unlock(&PropertyLock);
    return err;
}

static int
p_rec_enter_z(value vrec, type trec, value vterm, type tterm)
{
    t_heap_rec *obj, *header, *last;
    pword copy_pw, *pw;
    int err;

    Get_Typed_Object(vrec, trec, &heap_rec_header_tid, header);

    if ((err = create_heapterm(&copy_pw, vterm, tterm)) != PSUCCEED)
	{ Bip_Error(err); }
    obj = _rec_create();

    a_mutex_lock(&SharedDataLock);
    ((t_heap_rec *) header->term.val.ptr)->next = obj;	/* append */
    header->term.val.ptr =  (pword *) obj;	/* update tail ptr */
    move_heapterm(&copy_pw, &obj->term);
    a_mutex_unlock(&SharedDataLock);
    Succeed_;
}

static int
p_rec_all(value vrec, type trec, value vl, type tl)
{
    t_heap_rec *header;
    pword list;

    Get_Typed_Object(vrec, trec, &heap_rec_header_tid, header);
    Check_Output_List(tl);
    _rec_all(header, 0, &list);
    Return_Unify_Pw(vl, tl, list.val, list.tag);
}

static int
p_rec_list(value vrec, type trec, value vl, type tl)
{
    t_heap_rec *header;
    pword list;

    Get_Typed_Object(vrec, trec, &heap_rec_header_tid, header);
    Check_Output_List(tl);
    _rec_all(header, 1, &list);
    Return_Unify_Pw(vl, tl, list.val, list.tag);
}

static int
p_rec_term(value vrec, type trec, value vl, type tl)
{
    t_heap_rec *obj;
    pword result;

    Get_Typed_Object(vrec, trec, &heap_rec_tid, obj);
    if (obj->erased)
    	{ Fail_; }
    get_heapterm(obj->term, &result);
    if (IsRef(result.tag) && result.val.ptr == &result)
    {
	Succeed_;
    }
    Return_Unify_Pw(vl, tl, result.val, result.tag);
}

static int
p_rec_erase(value vrec, type trec)
{
    t_heap_rec *obj;
    pword result;

    Get_Typed_Object(vrec, trec, &heap_rec_tid, obj);
    if (obj->erased)
    	{ Fail_; }
    obj->erased = 1;
    Succeed_;
}

#endif


/*----------------------------------------------------------------------
 * the subsequent BIPs fail on error and set the global variable
 *----------------------------------------------------------------------*/

#undef Bip_Error
#define Bip_Error(N) Bip_Error_Fail(N)

/*
 * Check if key is a valid key for records
 */

/* ARGSUSED */
static int
p_valid_key(value v, type t)
{
    Error_If_Ref(t);
    if (IsAtom(t) || IsStructure(t) || IsNil(t) || IsList(t))
	{ Succeed_; }
    else
	{ Bip_Error(TYPE_ERROR) }
}

#undef Bip_Error
#define Bip_Error(N) return(N);

/*----------------------------------------------------------------------
 * End of fail on error BIPs
 *----------------------------------------------------------------------*/

