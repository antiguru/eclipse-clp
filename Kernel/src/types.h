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
 * Contributor(s): ECRC GmbH and IC-Parc.
 * 
 * END LICENSE BLOCK */

/*
 * ECLiPSe INCLUDE FILE
 *
 * $Id: types.h,v 1.16 2016/07/28 03:34:36 jschimpf Exp $
 *
 * IDENTIFICATION		types.h
 *
 * DESCRIPTION	
 *				include file containing the global 
 *				types of data	
 *
 * This file shall only contain typedefs, no macros, no extern decls.
 * It can be included/preprocessed in several environments,
 * identified by #defines, in order of increasing restrictedness:
 *
 * nothing			ECLiPSe kernel (all details visible)
 * EC_EXTERNAL			Obsolete ECLiPSe "external" API
 * EC_EXTERNAL|EC_EMBED		ECLiPSe embedding API
 */

#ifdef EC_EXTERNAL
#define HIDE_EXT(a,b) b
#else
#define HIDE_EXT(a,b) a
#endif

#ifndef EC_EXTERNAL
#include "memman.h"
#else
typedef void *generic_ptr;
#endif

#include "ec_general.h"


/* TEMPORARY: disable old locking primitives from parallel system */
/*typedef int a_mutex_t;*/
#define	a_mutex_init(m)			1
#define	a_mutex_lock(m)			1
#define	a_mutex_unlock(m)		1
#define	a_mutex_t int


/*---------------------------------------------------------------------------
 * Dictionary item
 *---------------------------------------------------------------------------*/

struct dict_item
{
    word		arity;		/* functor arity		     */
    struct s_pword	*string;	/* functor name string		     */
#ifndef EC_EXTERNAL
    unsigned		macro:1;	/* maybe a macro		     */
    unsigned		season:1;	/* season it was last accessible 0/1 */
    unsigned		module:2;	/* module * locked * unlocked	     */
    unsigned		eval:1;		/* unused			     */
    unsigned		stability:2;	/* PERMANENT, CODE_REF or VOLATILE   */
    unsigned		head:1;		/* head of the collision chain	     */
    unsigned		bitfield:19;	/* bit mask for the var names	     */
    unsigned		isop:3;		/* maybe an operator		     */
    unsigned		fill:2;
    struct pri		*procedure;	/* procedure chain		     */
    struct property	*properties;	/* property chain		     */
    struct dict_item	*next;		/* next did with same hash value     */
#endif
};

typedef struct dict_item	*dident;


/*---------------------------------------------------------------------------
 * Prolog word
 *---------------------------------------------------------------------------*/

typedef union
{
	uword	        all;
	uword		*wptr;
	struct s_pword	*ptr;
	char		*str;
	word		nint;
	dident		did;
#ifdef UNBOXED_DOUBLES
	double		dbl;
#endif
#ifndef EC_EXTERNAL
	struct pri	*priptr;
#endif
} value;

typedef union
{
	uword		all;
	word		kernel;
} type;

typedef struct s_pword
{
	value val;			/* value part first */
	type tag;			/* then tag part */
} pword;


/*---------------------------------------------------------------------------
 * The abstract machine code item
 *---------------------------------------------------------------------------*/

typedef uword vmcode;


/*---------------------------------------------------------------------------
 * Simplified types for Scheduler and message passing system.
 * For information hiding, we don't inlcude the real definitions here.
 * Make sure the sizes match the real ones!
 *---------------------------------------------------------------------------*/

#ifndef _PDS_TYPES_H_
typedef word		*void_ptr;
#endif /* _PDS_TYPES_H */

typedef uint32		aport_handle_t;		/* aport_id_t */

typedef aport_handle_t	site_handle_t;		/* site_id_t */

typedef struct st_handle_ds {
	site_handle_t	site;
	void_ptr	edge;
	void_ptr	knot;
} st_handle_t;					/* st_id_t */


/*---------------------------------------------------------------------
 * Huge, consecutive stacks (for the Prolog stacks)
 * They come in pairs, growing in opposite direction:
 *
 * start --> end ... <gap> ... end <-- start
 *
 * This descriptor is only for memory allocation purposes.
 * The end pointer indicates the end of the allocated (e.g. mapped)
 * area for the stack, not necessarily the top of the actually used stack.
 *---------------------------------------------------------------------*/

typedef struct stack_struct {
	char	*name;		/* symbolic name of the stack	*/
	uword	*start,		/* start of the allocated area	*/
		*end,		/* end of the allocated area	*/
		*peak;		/* highest value 'end' ever had	*/
} stack_pair[2];

/*---------------------------------------------------------------------------
 * Safe external references
 *---------------------------------------------------------------------------*/

enum ec_ref_state { EC_REF_FREE=0,EC_REF_C=1,EC_REF_P=2,EC_REF_C_P=3 };
typedef struct eclipse_ref_
{
	pword var; /* init val ~EC_REF_EC else actual value */
	struct eclipse_ref_ * prev;
	struct eclipse_ref_ * next;
	struct ec_eng_s *eng;
	int size;
	enum ec_ref_state refstate;
} * ec_ref;

typedef ec_ref ec_refs;


typedef struct globalref {
	struct globalref *next;
	pword *ptr;
	dident name;
	dident module;
} globalref;


/*---------------------------------------------------------------------------
 * Cleanup list
 *---------------------------------------------------------------------------*/

typedef struct action_list {
    struct action_list *up;
    struct action_list *down;
    void (*action)(void*);
    void *thing;
} action_list_t;


/*---------------------------------------------------------------------------
 * Stream descriptor
 *---------------------------------------------------------------------------*/

#ifndef EC_EXTERNAL

typedef struct stream_d {
    int			unit;		/* system identifier (fd)	*/
    void_ptr		methods;	/* I/O method table (io_channel_t *) */
    int			nref;		/* refs from handles, dids, etc	*/
    int			encoding;	/* bytes, utf8, etc.		*/
    int			mode;		/* flags			*/
    int			output_mode;	/* default output mode settings	*/
    int			print_depth;	/* default print depth		*/
    dident		name;		/* did of the file name		*/
    unsigned char 	*buf;		/* buffer address		*/
    unsigned char 	*wbuf;		/* write buffer (queues only)	*/
    word		size;		/* max size of the buffer	*/
    word 		cnt;		/* actual used buffer size	*/
    unsigned char 	*ptr;		/* next char to be read or written */
    unsigned char 	*lex_aux;	/* lexical analyser aux buffer	*/
    word		line;		/* number of read lines, if File */
    word		lex_size;	/* lex_aux buffer size		*/
    uword		offset;		/* current offset in the file	*/
    struct stream_d	*paired_stream;	/* for R/W streams (sockets)	*/
    struct stream_d	*prompt_stream;	/* input: the stream to output  */
    dident		prompt;		/* did of the prompt string	*/
    word		nr;		/* the stream number		*/
    int			fd_pid;		/* process that owns the fd	*/
    ec_mutex_t		lock;		/* shared memory lock (par)	*/
    aport_handle_t	aport;		/* stream handler's address (par) */
    generic_ptr		stdfile;	/* FILE stream for this fd	*/
    pword		event;		/* the event to raise ([] if none) */
    struct ec_eng_s *	event_eng;	/* engine to receive events	*/
   					/* (only set when event \= [])	*/
    uint32		rand;		/* random generator state	*/
    int			last_written;	/* last character written	*/
    void_ptr		signal_thread;	/* to simulate sigio on Windows	*/
    dident		pathname;	/* full file path name		*/
} stream_desc;

typedef stream_desc	*stream_id;
#else
typedef void_ptr	stream_id;
#endif


/*
 * Descriptor for a source location
 */

typedef struct {
	dident file;		/* file name atom (or '' if none) */
	uword line;		/* line number in stream */
	uword from;		/* stream offset of first char */
	uword to;		/* stream offset of last char + 1 */
} source_pos_t;

#define SOURCE_POS_SZ	4	/* words in the structure above */


/* ----------------------------------------------------------------------
 *  Handle type descriptor
 * ---------------------------------------------------------------------- */

typedef void *t_ext_ptr;


/* Method table */
typedef struct {
    void	(*free)ARGS((t_ext_ptr));
    t_ext_ptr 	(*copy)ARGS((t_ext_ptr));
    void	(*mark_dids)ARGS((t_ext_ptr));
    int		(*string_size)ARGS((t_ext_ptr obj, int quoted_or_radix));
    int		(*to_string)ARGS((t_ext_ptr obj, char *buf, int quoted_or_radix));
    int 	(*equal)ARGS((t_ext_ptr, t_ext_ptr));
    t_ext_ptr 	(*remote_copy)ARGS((t_ext_ptr));
    pword 	(*get)ARGS((t_ext_ptr, int, struct ec_eng_s*));
    /*int 	(*get)ARGS((t_ext_ptr, int, pword*, struct ec_eng_s*));*/
    int 	(*set)ARGS((t_ext_ptr, int, pword, struct ec_eng_s*));
    dident	(*kind)ARGS((void));
    int		(*lock)ARGS((t_ext_ptr));
    int		(*trylock)ARGS((t_ext_ptr));
    int		(*unlock)ARGS((t_ext_ptr));
    int		(*signal)ARGS((t_ext_ptr,int));
    int		(*wait)ARGS((t_ext_ptr,int));
} t_ext_type;


/*---------------------------------------------------------------------------
 * Tracer/debugger data
 *---------------------------------------------------------------------------*/

#ifndef EC_EXTERNAL

typedef struct
{
    word	invoc;
    struct pri	*proc;
    source_pos_t source_pos;

} fail_data_t;

#endif

typedef struct			/* debugger registers */
{
    pword	debug_top;
    HIDE_EXT(fail_data_t *, void_ptr) fail_trace;
    word	next_invoc;

				/* passing info into notify event */
    HIDE_EXT(struct pri *, void_ptr) call_proc;
    word	call_port;
    word	call_invoc;
    word	first_delay_invoc;

    word	redo_level;	/* level at which failure was caught */
    word	fail_drop;	/* number of failed levels */
    word	fail_culprit;	/* invoc of failure culprit */

    word	port_filter;	/* port pre-filtering */
    word	min_invoc;
    word	max_invoc;
    word	min_level;
    word	max_level;
    word	trace_mode;

    source_pos_t source_pos;	/* source position */
} trace_t;


/*---------------------------------------------------------------------------
 * the control stack
 *---------------------------------------------------------------------------*/

#ifndef EC_EXTERNAL

typedef union control {
    pword	 *args;
    struct choice_frame {
	pword		*sp;
	pword		*tg;
	pword		**tt;
	pword		*e;
	pword		*ld;
	/* arguments */
    }	*chp;
    struct parallel_frame {
	pword		*sp;
	pword		*tg;
	pword		**tt;
	pword		*e;
	pword		*ld;
	word		alt;	/* currently executing alternative */
	pword		*ppb;
	st_handle_t	node;
	/* arguments */
    }	*chp_par;
    struct invocation_frame {	/* must look like a parallel frame */
	pword		*sp;
	pword		*tg;
	pword		**tt;
	pword		*e;
	pword		*ld;
	word		alt;	/* currently executing alternative */
	pword		*ppb;
	st_handle_t	node;
	pword		*eb;
	pword		*gb;
	pword		*pb;
	uint32		flags;
	vmcode		*pp;
	pword           *de;
	pword           *mu;
	pword           *sv;
	int		wp;
	pword		wp_stamp;
	pword		postponed_list;
	pword		wl;
	int		load;
	pword           *oracle;
	char		*followed_oracle;
	char		*pending_oracle;
	int		global_bip_error;
	trace_t		trace_data;
	pword           *gctg;
	pword           *tg_soft_lim;
	pword		*tg_before;
	void_ptr	parser_env;
	int		nesting_level;
	pword		arg_0;
	/* arguments */
    }	*invoc;
    struct exception_frame {
	pword		*sp;
	pword		*tg;
	pword		**tt;
	pword		*e;
	pword           *ld;
	pword		*eb;
	pword		*gb;
	uint32		flags;
	pword           *de;
	pword           *mu;
	pword		*tg_soft_lim;
	int		wp;
#ifdef AS_EMU
	pword		*s;
	pword		tr;
#endif
	/* arguments */
    } *exception;
    struct top_frame {
	vmcode		*backtrack;
	union frame {
	    struct choice_frame		*chp;
	    struct parallel_frame	*chp_par;
	    struct invocation_frame	*invoc;
	    struct exception_frame      *exception;
	    struct top_frame		*top;
	    pword			*args;
	}	frame;
    }	*top;
#if !defined(__cplusplus)
    union frame		any_frame;
#endif
} control_ptr;

#if defined(__cplusplus)
typedef struct control::choice_frame	*chp_ptr;
typedef struct control::invocation_frame	*invoc_ptr;
typedef struct control::top_frame	*top_ptr;
#else
typedef struct choice_frame	*chp_ptr;
typedef struct invocation_frame	*invoc_ptr;
typedef struct top_frame	*top_ptr;
#endif

#endif /* EC_EXTERNAL */


/* ----------------------------------------------------------------------
 * Circular linked-list for unlimited event queue (synchronously posted)
 * ---------------------------------------------------------------------- */

typedef struct _dyn_event_q_slot_t {
    pword event_data;
    struct _dyn_event_q_slot_t *next;      
} dyn_event_q_slot_t;

typedef struct {
    dyn_event_q_slot_t *prehead;
    dyn_event_q_slot_t *tail;
    uword total_event_slots;
    uword free_event_slots;
} dyn_event_q_t;


/*---------------------------------------------------------------------------
 * Options for an ECLiPSe environment and/or engine
 *---------------------------------------------------------------------------*/

/*
 * ALLOC_PRE = fixed sizes pre-allocated
 * ALLOC_FIXED = virtual space at fixed addresses
 * ALLOC_VIRTUAL = virtual space pre-allocated , system allocates real memory
 */
enum t_allocation { ALLOC_PRE,ALLOC_FIXED,ALLOC_VIRTUAL } ;
enum t_io_option { SHARED_IO,OWN_IO,MEMORY_IO } ;

typedef struct
{
    /* memory in kbytes for megalog page buffers */
    int		option_p;

    /* flag for shared heap and the name of the corresponding mapfile */
    char	*mapfile;

    /* The number of this worker. 0 for sequential system */
    int		parallel_worker;

    /* How to initialise the standard I/O streams */
    int		io_option;

    /* for access the command line in the built-ins	*/
    char	**Argv;
    int		Argc;

    /* readline enabled */
    int		rl;

    /* sizes of stack pairs in bytes */
    uword	localsize;
    uword	globalsize;
    /* sizes of heaps in bytes */
    uword	privatesize;
    uword	sharedsize;

    /* panic callback */
    void	(*user_panic)ARGS((const char*,const char *));

    int		allocation; 

    /* the initial user module */
    char	*default_module;

    /* The directory where eclipse is installed. If this is not set
     * at initialisation time, the value of $ECLIPSEHOME gets filled in */
    char	*eclipse_home;

    /* flags for different engine initialisation options */
    int		init_flags;

    /* flag to enable internal debugging facilities (0=none, or >0) */
    int		debug_level;

    /* the initial default_language */
    char	*default_language;

    /* vm_options: oracles (-o) */
    int		vm_options;

} t_eclipse_options;


/*---------------------------------------------------------------------------
 * Abstract Machine descriptor
 *---------------------------------------------------------------------------*/

typedef struct ec_eng_s
{
    pword *	sp;		/* top of local stack */
    pword **	tt;		/* top of trail stack */
    pword *	tg;		/* top of global stack */
    pword *	e;		/* environment pointer */
    pword *	eb;		/* local stack backtrack pointer */
    pword *	gb;		/* global stack backtrack pointer */
    HIDE_EXT(control_ptr, pword *)
		b;		/* top of control stack */
    pword *	lca;		/* last cut action */
    int		vm_flags;	/* machine flags */
    volatile int event_flags;	/* flags that may be changed by signals */
    vmcode	*pp;		/* code pointer */

    pword *	de;		/* current suspension */
    pword *	ld;		/* list of all suspended goals (last delayed) */
    pword *	mu;		/* list of meta-unifications */
    pword *	sv;		/* list of suspending variables */
    pword 	wl;		/* global woken lists. This register has a
				 * tag because it is value-trailed, and the
				 * GC requires a tag in this case. */
    int		wp;		/* current running priority */
    pword	wp_stamp;	/* and its time-stamp */
    pword	postponed_list;	/* postponed goals */

    pword *	pb;		/* top of parallel choicepoints (optional) */
    pword *	ppb;		/* top of published parallel choicepoints */
    st_handle_t	*leaf;		/* scheduler node associated with this engine */
    int		load;		/* to keep track of unpublished load */

    pword *	occur_check_boundary;		/* occur check registers */
    pword *	top_constructed_structure;

    pword *	oracle;		/* top of recorded oracle */
    char *	followed_oracle; /* currently followed oracle */
    char *	pending_oracle;	/* will become followed_oracle */
    int		ntry;

    int		global_bip_error;

    trace_t	trace_data;

    pword *	gctg;		/* tg after last garbage collection */

    pword * volatile tg_soft_lim;	/* garbage collection trigger point */
    pword *	tg_soft_lim_shadow;	/* needed in case of faked overflow */
    volatile int irq_faked_overflow;	/* for faking overflow in interrupts */

    pword *	tg_limit;	/* stack limits for overflow checks */
    pword **	tt_limit;
    pword *	b_limit;	
    pword *	sp_limit;	

    stack_pair	global_trail;	/* stack allocation descriptors */
    stack_pair	control_local;

    word	segment_size;	/* garbage collection interval */
    int		nesting_level;	/* of recursive emulator invovations */

    void_ptr	parser_env;	/* parser data structure */
    void	*it_buf;	/* for throw via longjmp */

    pword	posted;		/* posted goals */

    struct eclipse_ref_ allrefs; /* list of ec_refs (externals) */
    globalref *	references;	/* list of "named references" */

    pword	emu_args[NARGREGS];

    int32	frand_state;	/* random generator state */
    dident	default_module;	/* for posted/resumed/rpc goals */

    t_eclipse_options options;	/* engine initialization options */

    void *	own_thread;	/* OS thread owned by this engine (NULL if sync engine) */
    void *	run_thread;	/* OS thread currently running this engine */
    				/* (thread can do longjmp_throw() on engine) */

    struct ec_eng_s *next, *prev;/* chain of all engines (under engine_list_lock) */

    ec_mutex_t	lock;		/* for access to next group of fields */
    ec_cond_t	cond;		/* for waiting on engine ownership */

	int volatile ref_ctr;	/* number of references to this engine */
	void *owner_thread;	/* OS thread currently owning this engine */
	int volatile paused;	/* engine is paused (encoded field) */
	dyn_event_q_t dyn_event_q; /* Dynamic synchronous event queue */

    int	requested_exit_code;	/* if (EVENT_FLAGS & EXIT_REQUEST) */
    pword requested_throw_ball;	/* if (EVENT_FLAGS & THROW_REQUEST) */

    int	needs_dgc_marking;	/* awaiting dictionary marking from this engine */
    struct ec_eng_s *parent_engine; /* engine waiting for this engine, */
    				/* NULL if no waiter, or if resumed from C */
    				/* (used for dictionary marking) */

    action_list_t *cleanup;	/* cleanup after returning from external pred */
    action_list_t *cleanup_bot;	/* (top and bottom of cleanup stack) */

#ifdef PRINTAM
#define MAX_BACKTRACE 1024
    vmcode	*stop_address;	/* address breakpoint in the emulator */
    int		bt_index;	/* index into backtrace[] */
    vmcode	*backtrace[MAX_BACKTRACE]; /* records recent PP values */
#endif

} ec_eng_t;


typedef void_ptr (*func_ptr)(void);
typedef func_ptr (*continuation_t)(ec_eng_t*);


/*---------------------------------------------------------------------------
 * Tag descriptor
 *---------------------------------------------------------------------------*/

struct tag_descriptor {
	type			tag;		/* tag bit pattern */
	word			super;		/* is subtype of ... */
	dident			tag_name;	/* did */
	dident			type_name;	/* did */
	int			numeric;	/* numeric type and order  */
	int			order;		/* standard term order */
	int	(* write)ARGS((int,stream_id,value,type));
	int	(* string_size)ARGS((value,type,int));
	int	(* to_string)ARGS((value,type,char*,int));
	int	(* from_string)ARGS((ec_eng_t*,char *,pword*,int));
	int	(* equal)ARGS((pword*,pword*));
	int	(* compare)ARGS((value,value));
	int	(* arith_compare)ARGS((value,value,int*));
	int	(* copy_size)ARGS((value,type));
	pword * (* copy_to_heap)ARGS((value,type,pword*,pword*));
	pword * (* copy_to_stack)ARGS((void));
	int	(* arith_op[ARITH_OPERATIONS])ARGS((Dots));
	int	(* coerce_to[NTYPES+1])ARGS((ec_eng_t*,value,value*));
};



/* ----------------------------------------------------------------------
 *  Heap copied goal (event handler)
 * ---------------------------------------------------------------------- */

typedef struct {
    pword               goal; /* Must be first field - same addr as struct */
    pword               module;
    word                ref_ctr;
    short               enabled;
    short               defers;	/* defer event handling when entering handler */
} t_heap_event;


/*---------------------------------------------------------------------------
 * Global data that has to go into shared memory
 *---------------------------------------------------------------------------*/

struct shared_data_t {
#if 0
	a_mutex_t
		general_lock,			/* if none of the others used */
		mod_desc_lock,			/* module descriptor */
		prop_desc_lock,			/* property descriptors */
		    prop_list_lock,		/* functor property list */
		proc_desc_lock,			/* procedure descriptors */
		    proc_list_lock,		/* functor procedure list */
		    proc_chain_lock;		/* shared procedure chains */
#else
	ec_mutex_t
		general_lock,
		engine_list_lock,
		arrays_lock;
#endif

	int	global_flags,
		print_depth,
		output_mode_mask,
		compile_id,
		code_heap_used,
		global_var_index,
		load_release_delay,
		publishing_param,
		nbstreams,
		nbstreams_free,
		shutdown_in_progress,
		user_error,
		max_errors,
		meta_arity;

	void_ptr
		dictionary,			/* has its own lock */

		abolished_procedures,		/* proc_chain_lock */
		compiled_structures,
		global_procedures,		/* proc_chain_lock */
		constant_table,

		meta_attribute,
		stream_descriptors,
		error_handler,
		default_error_handler,
		interrupt_handler,
		interrupt_handler_flags,
		interrupt_name,
		error_message,
		message,
		startup_goal,
		debug_macros,
		worker_statistics,

		extension_ptr,
		extension_ptr1,
		extension_ptr2,
		extension_ptr3,
		extension_ptr4,
		extension_ptr5,
		extension_ptr6,
		extension_ptr7;
};

/*---------------------------------------------------------------------------
 * A table of pre-computed DIDs
 * Watch out for conflict with C keywords: use true0 for true, etc.
 *---------------------------------------------------------------------------*/

typedef struct
{
    dident
	abort,
	apply2,
	at2,
	block,
	block_atomic,
	call_body,
	comma,
	cond,
	cut,
	emulate,
	exit_block,
	fail,
	false0,
	kernel_sepia,
	list,
	nil,
	semicolon,
	colon,
	sepia,
	true0,
	abolish,
	abs,
	acos,
	all,
	ampersand,
	and2,
	answer,
	append,
	arg,
	subscript,
	asin,
	atan,
	atom,
	atom0,
	atomic,
	bar,
	bitnot,
	bignum,
	break0,
	bsi,
	built_in,
	local_control_overflow,
	byte,
	call,
	call_explicit,
	clause,
	clause0,
	cn,
	command,
	comment,
	compile,
	compile_stream,
	compound,
	compound0,
	constrained,
	cos,
	cprolog,
	cut_to,
	debug,
	log_output,
	warning_output,
	debugger,
	default0,
	default_module,
	define_global_macro3,
	define_local_macro3,
	delay,
	denominator1,
	diff_reg,
	div,
	dummy_call,
	dummy_module,
	dynamic,
	e,
	eclipse_home,
	ellipsis,
	empty,
	eocl,
	eof,
	eoi,
	equal,
	erase_macro1,
	err,
	error,
	error_handler,
	exp,
	export1,
	exportb,
	external,
	fail_if,
	fail_if0,
	file_query,
	breal,
	breal1,
	breal_from_bounds,
	breal_min,
	breal_max,
	fix,
	float0,
	float1,
	floor1,
	flush,
	free,
	free1,
	from,
	functor,
	garbage_collect_dictionary,
	global,
	global0,
	globalb,
	go,
	goal,
	goalch,
	grammar,
	greater,
	greaterq,
	ground,
	throw1,
	halt,
	halt0,
        hang,
	identical,
	if2,
	import,
	import_fromb,
	inf,
	inf0,
	infq,
	infq0,
	input,
	integer,
	integer0,
	invoc,
	is_event,
	is_handle,
	is_list,
	is_suspension,
	double1,
	double0,
	is,
	global_trail_overflow,
	leash,
	less,
	lessq,
	ln,
	local,
	local_break,
	local0,
	localb,
	lock,
	lshift,
	macro,
	make_suspension,
	max,
	maxint,
	meta,
	meta0,
	metacall,
	min,
	minint,
	minus,
	minus0,
	minus1,
	mode,
	module0,
	module1,
	module_directive,
	modulo,
	nilcurbr,
	nilcurbr1,
	no_err_handler,
	nodebug,
        nohang,
	nonground,
	nonvar,
	naf,
	not0,
	not1,
	not_equal,
	not_identical,
	not_not,
	not_unify,
	notp0,
	notrace,
	null,
	number,
	numerator1,
	off,
	on,
	once,
	once0,
	or2,
	output,
	pcompile,
	pi,
	plus,
	plus0,
	plus1,
	plusplus,
	power,
	pragma,
	print,
	priority,
	prolog,
	protect_arg,
	question,
	quintus,
	quotient,
	rational0,
	rational1,
	read,
	read1,
	read2,
	real,
	real0,
	reset,
	round,
	rshift,
	rulech0,
	rulech1,
	rulech2,
	semi0,
	sicstus,
	sin,
	skip,
	softcut,
	some,
	spy,
	sqrt,
	state,
	stderr0,
	stdin0,
	stdout0,
	stop,
	string0,
	string,
	sup,
	sup0,
	supq,
	supq0,
	suspending,
	suspend_attr,
	syscut,
	syserror,
	system,
	system_debug,
	tan,
	term,
	times,
	top_only,
	trace,
	trace_frame,
	trans_term,
	unify,
	unify0,
	univ,
	universally_quantified,
	user_input,
	user_output,
	user_error,
	update,
	uref,
	user,
	var,
	var0,
	wake,
	with2,
	with_attributes2,
	woken,
	write,
	write1,
	write2,
	writeq1,
	writeq2,
	xor2;
} standard_dids;


/*
 * A structure containing all exported Eclipse data
 */

typedef struct 
{
    struct shared_data_t	*shared;
    struct tag_descriptor	td[NTYPES+1];
    standard_dids		d;

    stream_id			current_input;
    stream_id			current_output;
    stream_id			current_error;
    stream_id			current_warning_output;
    stream_id			current_log_output;
    stream_id			user_input;
    stream_id			user_output;
    stream_id			user_error;
    stream_id			null_stream;

    ec_eng_t			m;		/* default working engine */
    ec_eng_t			m_aux;		/* auxiliary engine */
    ec_eng_t			m_sig;		/* signal thread engine */
    ec_eng_t			m_timer;	/* timer thread engine */
} t_eclipse_data;

