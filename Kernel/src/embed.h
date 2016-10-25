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
 * $Id: embed.h,v 1.14 2016/10/25 22:27:59 jschimpf Exp $
 *
 *
 * IDENTIFICATION:	embed.h
 *
 * AUTHOR:		Joachim Schimpf
 * AUTHOR:		Stefano Novello
 *
 * CONTENTS:		name/arity
 *
 * DESCRIPTION:
 *			External embedding interface and safe variables
 */


enum {
	EC_OPTION_MAPFILE		=0,
	EC_OPTION_PARALLEL_WORKER	=1,
	EC_OPTION_ARGC			=2,
	EC_OPTION_ARGV			=3,
	EC_OPTION_LOCALSIZE		=4,
	EC_OPTION_GLOBALSIZE		=5,
	EC_OPTION_PRIVATESIZE		=6,
	EC_OPTION_SHAREDSIZE		=7,
	EC_OPTION_PANIC			=8,
	EC_OPTION_ALLOCATION		=9,
	EC_OPTION_DEFAULT_MODULE	=10,
	EC_OPTION_ECLIPSEDIR		=11,
	EC_OPTION_IO			=12,
	EC_OPTION_INIT			=13,
	EC_OPTION_DEBUG_LEVEL		=14,
	EC_OPTION_CWD_SEPARATE		=15,
	EC_OPTION_DEFAULT_LANGUAGE	=16
};

/*
 * Data
 */

Extern DLLEXP t_eclipse_data		ec_;
Extern DLLEXP t_ext_type		ec_xt_double_arr;
Extern DLLEXP t_ext_type		ec_xt_long_arr;
Extern DLLEXP t_ext_type		ec_xt_char_arr;


/*
 * Initialisation options
 */
Extern DLLEXP int	Winapi	ec_set_option_int ARGS((int, int));
Extern DLLEXP int	Winapi	ec_set_option_long ARGS((int, word));
Extern DLLEXP int	Winapi	ecl_set_option_long ARGS((t_eclipse_options*, int, word));
Extern DLLEXP int	Winapi	ec_set_option_ptr ARGS((int, void*));
Extern DLLEXP int	Winapi	ecl_set_option_ptr ARGS((t_eclipse_options*, int, void*));

/* set the context module for resuming with goals */
Extern DLLEXP int	Winapi	ecl_set_context_module(ec_eng_t*,dident);

/*
 * Create and destroy an eclipse engine
 */
Extern DLLEXP int	Winapi	ec_init ARGS((void));
Extern DLLEXP int	Winapi	ecl_init ARGS((t_eclipse_options*,ec_eng_t**));
Extern DLLEXP int	Winapi	ec_cleanup ARGS((void));

Extern DLLEXP int	Winapi	ecl_engine_create(t_eclipse_options*,ec_eng_t*,ec_eng_t**);
Extern DLLEXP int	Winapi	ecl_acquire_engine(ec_eng_t*);
Extern DLLEXP void	Winapi	ecl_relinquish_engine(ec_eng_t*);
Extern DLLEXP int	Winapi	ecl_request_exit(ec_eng_t*,int);
Extern DLLEXP int	Winapi	ecl_request_throw(ec_eng_t*,pword);

/*
 * Restart an eclipse engine that has yielded
 */
Extern DLLEXP int	Winapi	ecl_post_goal ARGS((ec_eng_t*,const pword));

Extern DLLEXP int	Winapi	ecl_post_string ARGS((ec_eng_t*,const char *));

Extern DLLEXP int	Winapi	ecl_post_exdr ARGS((ec_eng_t*,int, const char *));

Extern DLLEXP int	Winapi	ecl_resume ARGS((ec_eng_t*));

Extern DLLEXP int	Winapi	ecl_resume1 ARGS((ec_eng_t*,ec_ref));

Extern DLLEXP int	Winapi	ecl_resume2 ARGS((ec_eng_t*,const pword,ec_ref));

Extern DLLEXP int	Winapi	ecl_resume_goal ARGS((ec_eng_t*,const pword,const pword,ec_ref,int));

Extern DLLEXP int	Winapi	ecl_resume_long ARGS((ec_eng_t*,long *));

Extern DLLEXP int	Winapi	ecl_resume_async ARGS((ec_eng_t*));
Extern DLLEXP int	Winapi	ecl_resume_async1 ARGS((ec_eng_t*,const pword,const pword));

Extern DLLEXP int	Winapi	ec_running ARGS((void));
Extern DLLEXP int	Winapi	ecl_resume_status ARGS((ec_eng_t*));
Extern DLLEXP int	Winapi	ecl_resume_status_long ARGS((ec_eng_t*,long *));
Extern DLLEXP int	Winapi	ecl_wait_resume_status_long ARGS((ec_eng_t*,long *, int));

Extern DLLEXP int	Winapi	ecl_join_acquire ARGS((ec_eng_t*, int));


/*
 * Send events to running engine and handle them
 * (Note that events can also be raised by queues)
 */
Extern DLLEXP int	Winapi	ecl_post_event ARGS((ec_eng_t*,pword));
Extern DLLEXP int	Winapi	ecl_post_event_unique ARGS((ec_eng_t*,pword));

Extern DLLEXP int	Winapi	ecl_post_event_string ARGS((ec_eng_t*,const char *));

Extern DLLEXP int	Winapi	ecl_handle_events ARGS((ec_eng_t*));

Extern DLLEXP int	Winapi	ecl_handle_events_long ARGS((ec_eng_t*,long*));

/*
 * Choicepoints
 */
Extern DLLEXP void	Winapi	ecl_cut_to_chp ARGS((ec_eng_t*,ec_ref));

/*
 * construct eclipse terms
 */
Extern DLLEXP pword	Winapi	ecl_string ARGS((ec_eng_t*,const char*));

Extern DLLEXP pword	Winapi	ecl_length_string ARGS((ec_eng_t*,int, const char*));

Extern DLLEXP pword	Winapi	ec_atom ARGS((const dident));

Extern DLLEXP pword	Winapi	ec_long ARGS((const long));
#ifdef HAVE_LONG_LONG
Extern DLLEXP pword	Winapi	ecl_long_long ARGS((ec_eng_t*,const long long));
#endif
Extern DLLEXP pword	Winapi	ecl_double ARGS((ec_eng_t*,const double));
Extern DLLEXP pword		ecl_term ARGS((ec_eng_t*,dident, ... /*pwords*/));
			/* can't use Winapi with varargs! */
Extern DLLEXP pword	Winapi	ecl_term_array ARGS((ec_eng_t*,const dident,const pword[]));
Extern DLLEXP pword	Winapi	ecl_list ARGS((ec_eng_t*,const pword,const pword));
Extern DLLEXP pword	Winapi	ecl_listofrefs ARGS((ec_eng_t*,ec_refs));
Extern DLLEXP pword	Winapi	ecl_listofdouble ARGS((ec_eng_t*,int, const double*));
Extern DLLEXP pword	Winapi	ecl_listoflong ARGS((ec_eng_t*,int, const long*));
Extern DLLEXP pword	Winapi	ecl_listofchar ARGS((ec_eng_t*,int, const char*));
Extern DLLEXP pword	Winapi	ecl_arrayofdouble ARGS((ec_eng_t*,int, const double*));
Extern DLLEXP pword	Winapi	ecl_matrixofdouble ARGS((ec_eng_t*,int, int, const double*));

Extern DLLEXP pword	Winapi	ecl_handle ARGS((ec_eng_t*,const t_ext_type*,const t_ext_ptr));

Extern DLLEXP pword	Winapi	ec_newvar ARGS((void));
Extern DLLEXP pword	Winapi	ec_nil ARGS((void));
Extern DLLEXP dident	Winapi	ec_did ARGS((const char *,const int));

Extern DLLEXP pword	Winapi	ecl_newvar ARGS((ec_eng_t*));


/*
 * inspect eclipse terms
 */
Extern DLLEXP int	Winapi	ec_get_string ARGS((const pword,char**));
Extern DLLEXP int	Winapi	ec_get_string_length ARGS((const pword,char**,long*));
Extern DLLEXP int	Winapi	ec_get_atom ARGS((const pword,dident*));
Extern DLLEXP int	Winapi	ec_get_long ARGS((const pword,long*));
#ifdef HAVE_LONG_LONG
Extern DLLEXP int	Winapi	ec_get_long_long ARGS((const pword,long long*));
#endif
Extern DLLEXP int	Winapi	ec_get_double ARGS((const pword,double*));
Extern DLLEXP int	Winapi	ec_get_nil ARGS((const pword));
Extern DLLEXP int	Winapi	ec_get_list ARGS((const pword,pword*,pword*));
Extern DLLEXP int	Winapi	ec_get_functor ARGS((const pword,dident*));
Extern DLLEXP int	Winapi	ec_arity ARGS((const pword));
Extern DLLEXP int	Winapi	ec_get_arg ARGS((const int,pword,pword*));
Extern DLLEXP int	Winapi	ec_get_handle ARGS((const pword,const t_ext_type*,t_ext_ptr*));
Extern DLLEXP int	Winapi	ec_is_var ARGS((const pword));

#define DidName(d)	((char *)(((dident)(d))->string + 1))
#define DidArity(d)	(((dident)(d))->arity)

/*
 * eclipse refs hold registered references to eclipse terms
 * which survive while the engine is running
 */
Extern DLLEXP ec_refs	Winapi	ecl_refs_create ARGS((ec_eng_t*,int,const pword));
Extern DLLEXP ec_refs	Winapi	ecl_refs_create_newvars ARGS((ec_eng_t*,int));
Extern DLLEXP void	Winapi	ec_refs_destroy ARGS((ec_refs));
Extern DLLEXP void	Winapi	ec_refs_set ARGS((ec_refs,int,const pword));
Extern DLLEXP pword	Winapi	ec_refs_get ARGS((const ec_refs,int));
Extern DLLEXP int	Winapi	ec_refs_size ARGS((const ec_refs));

Extern DLLEXP ec_ref	Winapi	ecl_ref_create ARGS((ec_eng_t*,pword));
Extern DLLEXP ec_ref	Winapi	ecl_ref_create_newvar ARGS((ec_eng_t*));
Extern DLLEXP void	Winapi	ec_ref_destroy ARGS((ec_ref));
Extern DLLEXP void	Winapi	ec_ref_set ARGS((ec_ref,const pword));
Extern DLLEXP pword	Winapi	ec_ref_get ARGS((const ec_ref));


/*
 * String-based interface
 */

Extern DLLEXP int	Winapi	ecl_exec_string ARGS((ec_eng_t*,char*,ec_ref));

Extern DLLEXP int	Winapi	ecl_var_lookup ARGS((ec_eng_t*,ec_ref,char*,pword*));

/*
 * External function interface
 */

Extern DLLEXP pword	Winapi	ecl_arg ARGS((ec_eng_t*,int));

Extern DLLEXP int	Winapi	ecl_unify ARGS((ec_eng_t*,pword,pword));

Extern DLLEXP int	Winapi	ecl_unify_arg ARGS((ec_eng_t*,int,pword));

Extern DLLEXP int	Winapi	ec_compare ARGS((pword,pword));
Extern DLLEXP int	Winapi	ec_visible_procedure ARGS((dident,pword,void**));

Extern DLLEXP int	Winapi	ecl_make_suspension ARGS((ec_eng_t*,pword,int,void*,pword*));

Extern DLLEXP int	Winapi	ecl_schedule_suspensions ARGS((ec_eng_t*,pword,int));

Extern DLLEXP int	Winapi	ec_free_handle ARGS((const pword, const t_ext_type*));
Extern DLLEXP int		ec_external ARGS((dident,int (*) (Dots),dident));

/*
 * Stream I/O
 */

Extern DLLEXP int	Winapi	ec_get_stream ARGS((const pword,stream_id*));
Extern DLLEXP void	Winapi	ec_release_stream ARGS((stream_id));

Extern DLLEXP int	Winapi	ec_stream_nr ARGS((const char *name));
Extern DLLEXP int	Winapi	ec_queue_write ARGS((int stream, char *data, int size));
Extern DLLEXP int	Winapi	ec_queue_read ARGS((int stream, char *data, int size));
Extern DLLEXP int	Winapi	ec_queue_avail ARGS((int stream));
Extern DLLEXP void	Winapi	ec_double_xdr ARGS((double * d, char * dest));
Extern DLLEXP void	Winapi	ec_int32_xdr ARGS((int32 * l, char * dest));
Extern DLLEXP void	Winapi	ec_xdr_int32 ARGS((char * buf , int32 * l));
Extern DLLEXP void	Winapi	ec_xdr_double ARGS((char * buf , double * d));

/*
 * Error handling
 */

Extern DLLEXP char *	Winapi	ec_error_string ARGS((int));
Extern DLLEXP void		ec_panic ARGS((const char* what, const char* where)); /* no Winapi */


/*
 * Backward compatibility definitions without engine argument.
 * These macros rely on a variable called 'ec_eng' being visible,
 * which refers to the engine to be used.
 */

#ifdef USES_NO_ENGINE_HANDLE
#define ec_eng (&ec_.m)
#endif

#define ec_term(dident,...)	ecl_term(ec_eng,dident,__VA_ARGS__)

#define ec_post_goal(g)		ecl_post_goal(ec_eng,g)
#define ec_post_string(s)	ecl_post_string(ec_eng,s)
#define ec_post_exdr(l,s)	ecl_post_exdr(ec_eng,l,s)
#define ec_resume()		ecl_resume(ec_eng)
#define ec_resume1(r)		ecl_resume1(ec_eng,r)
#define ec_resume2(p,r)		ecl_resume2(ec_eng,p,r)
#define ec_resume_long(i)	ecl_resume_long(ec_eng,i)
#define ec_resume_async()	ecl_resume_async(ec_eng)
#define ec_resume_async1(p)	ecl_resume_async1(ec_eng,p)
#define ec_resume_status(i,t)	ecl_resume_status(ec_eng)
#define ec_resume_status_long(i,t)	ecl_resume_status_long(ec_eng,i)
#define ec_wait_resume_status_long(i,t)	ecl_wait_resume_status_long(ec_eng,i,t)
#define ec_post_event(e)	ecl_post_event(ec_eng,e)
#define ec_post_event_string(s)	ecl_post_event_string(ec_eng,s)
#define ec_handle_events(pl)	ecl_handle_events_long(ec_eng,pl)
#define	ec_cut_to_chp(r)	ecl_cut_to_chp(ec_eng,r)

#define ec_exec_string(s,r)	ecl_exec_string(ec_eng,s,r)
#define ec_var_lookup(r,s,pw)	ecl_var_lookup(ec_eng,r,s,pw)

#define ec_string(s)		ecl_string(ec_eng, s)
#define ec_length_string(l, s)	ecl_length_string(ec_eng, l, s)
#define ec_long_long(l)		ecl_long_long(ec_eng, l)
#define ec_double(d)		ecl_double(ec_eng,d)
#define ec_handle(c,d)		ecl_handle(ec_eng,c,d)
#define ec_newvar()		ecl_newvar(ec_eng)

#define ec_refs_create(i,p)	ecl_refs_create(ec_eng,i,p)
#define ec_refs_create_newvars(i)	ecl_refs_create_newvars(ec_eng,i)
#define ec_ref_create(p)	ecl_ref_create(ec_eng,p)
#define ec_ref_create_newvar()	ecl_ref_create_newvar(ec_eng)

#define ec_term_array(d,a)	ecl_term_array(ec_eng,d,a)
#define ec_list(h,t)		ecl_list(ec_eng,h,t)
#define ec_listofrefs(r)	ecl_listofrefs(ec_eng,r)
#define ec_listofdouble(i,pd)	ecl_listofdouble(ec_eng,i,pd)
#define ec_listoflong(i,pl)	ecl_listoflong(ec_eng,i,pl)
#define ec_listofchar(i,pc)	ecl_listofchar(ec_eng,i,pc)
#define ec_arrayofdouble(i,pd)	ecl_arrayofdouble(ec_eng,i,pd)
#define ec_matrixofdouble(i,j,pd)	ecl_matrixofdouble(ec_eng,i,j,pd)

#define ec_arg(i)		ecl_arg(ec_eng,i)
#define ec_unify(p1,p2)		ecl_unify(ec_eng,p1,p2)
#define ec_unify_arg(i,p)	ecl_unify_arg(ec_eng,i,p)
#define	ec_make_suspension(g,i,p,s)	ecl_make_suspension(ec_eng,g,i,p,s)
#define ec_schedule_suspensions(p,i)	ecl_schedule_suspensions(ec_eng,p,i)



/*
 * The following is NOT (yet) part of the official embedding interface!
 */

#define current_input_	ec_.current_input
#define current_output_	ec_.current_output
#define current_err_	ec_.current_error
#define warning_output_	ec_.current_warning_output
#define log_output_	ec_.current_log_output
#define user_input_	ec_.user_input
#define user_output_	ec_.user_output
#define user_err_	ec_.user_error
#define null_		ec_.null_stream


Extern DLLEXP stream_id Winapi	ec_stream_id ARGS((int));
Extern DLLEXP	int		ec_outf ARGS((stream_id, const char*, int));
Extern DLLEXP	int		ec_outfc ARGS((stream_id, int));
Extern DLLEXP	int		ec_outfs ARGS((stream_id, const char*));
Extern DLLEXP	int		ec_flush ARGS((stream_id));
Extern DLLEXP	int		p_fprintf ARGS((stream_id nst, const char *fmt, ...));
Extern DLLEXP	int		ec_printff ARGS((stream_id nst, const char *fmt, ...));
Extern DLLEXP	int		ec_newline ARGS((stream_id));


/* ec_untrail_undo() used in gfd.cpp */

/* The context in which an undo function is being called */
#define UNDO_FAIL		0	/* untrail during fail */
#define UNDO_GC			1	/* untrail during gc */

/* Type of trailed data */
#define TRAILED_PWORD		0x0
#define TRAILED_REF		0x4
#define TRAILED_WORD32		0x8
#define TRAILED_COMP		0xc

#define ec_trail_undo(f,pi,ps,pd,s,t) ecl_trail_undo(ec_eng,f,pi,ps,pd,s,t)
Extern DLLEXP void		ecl_trail_undo ARGS((ec_eng_t*,void (*)(pword*,word*,int,int,ec_eng_t*), pword*, pword*, word*, int, int));


#ifndef EC_EMBED

Extern t_eclipse_options	ec_options;
Extern char			*ec_eclipse_home;


#define ec_assign(p,v,t) ecl_assign(ec_eng,p,v,t)
Extern	DLLEXP	int 	ecl_assign ARGS((ec_eng_t*, pword*, value, type));

Extern DLLEXP int		ecl_request(ec_eng_t*,int);

#if 0
Extern DLLEXP void		ec_trail_pwords ARGS((pword*, int, int));
#endif
Extern DLLEXP int		ec_unify_ ARGS((ec_eng_t*,value,type,value,type,pword**));
Extern DLLEXP int		ec_remember ARGS((ec_eng_t*,int,value,type));
Extern DLLEXP void		cut_external ARGS((ec_eng_t*));

Extern 		void		delayed_break ARGS((void));
Extern 		int		final_overflow ARGS((ec_eng_t*));
Extern DLLEXP void		global_ov ARGS((ec_eng_t*));
Extern DLLEXP void		trail_ov ARGS((ec_eng_t*));

Extern 		volatile int	it_disabled_, delayed_it_;

Extern DLLEXP	int		p_handle_free ARGS((value v_handle, type t_handle, ec_eng_t *));
Extern 		int		p_handle_free_on_cut ARGS((value v_handle, type t_handle, ec_eng_t *));

Extern DLLEXP 	pword * 	term_to_dbformat ARGS((ec_eng_t*, pword*, dident));
Extern DLLEXP 	pword * 	dbformat_to_term ARGS((ec_eng_t*, char*, dident, type));

Extern	DLLEXP	int		meta_index ARGS((dident));

Extern	void	ec_cleanup_unlock ARGS((void *));

Extern	void	ec_ref_set_safe(ec_ref variable, const pword w);

#endif /* !EC_EMBED */

