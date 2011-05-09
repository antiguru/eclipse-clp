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
 * Copyright (C) 2009 Cisco Systems, Inc.  All Rights Reserved.
 * 
 * Contributor(s): Kish Shen
 * 
 * END LICENSE BLOCK */

#define SIZEOF_CHAR_P 4
#define SIZEOF_INT 4
#define SIZEOF_LONG 4

#include "gfd.hpp"
#include <eclipseclass.h>
#include <sepia.h>

#define EC_EXTERNAL_ERROR  -213  // from error.h
#define SPACE_HANDLE_POS   1
#define SPACE_STAMP_POS    2

inline EC_word
EC_argument(EC_word t, int i)
{
  EC_word e;
  int x = t.arg(i, e);
  //assert(x == EC_succeed);
  return e;
}

// Arg is an integer domain variable with index Idx 
#define ArgIsVarIdx(Arg, Idx)			\
    (EC_arg(Arg).functor(&f) == EC_succeed &&	\
	strcmp(f.name(),"_ivar") ==0 && \
	EC_argument(EC_arg(Arg),1).is_long(&Idx) == EC_succeed)

// Arg is an integer domain variable with associated boolean with index BIdx
#define ArgIsVarBoolIdx(Arg, BIdx)		     \
    (EC_arg(Arg).functor(&f) == EC_succeed &&	     \
	strcmp(f.name(),"_ivar") ==0 && \
	EC_argument(EC_arg(Arg),2).is_long(&BIdx) == EC_succeed)

// report any exceptions from Gecode - must be preceeded by a try {...}
#define CatchAndReportGecodeExceptions \
    catch(Exception& err) {            \
	p_fprintf(current_err_, "Gecode exception: %s\n", err.what()); \
	return EC_EXTERNAL_ERROR; \
    }

#define Get_Consistency_Level(N, cl) {		\
    EC_atom atm;   \
    if (EC_arg(N).is_atom(&atm) != EC_succeed) return TYPE_ERROR; \
    if (strcmp(atm.name(), "default") == 0) cl = ICL_DEF;         \
    else if (strcmp(atm.name(), "gfd_gac") == 0) cl = ICL_DOM;    \
    else if (strcmp(atm.name(), "gfd_bc") == 0) cl = ICL_BND;     \
    else if (strcmp(atm.name(), "gfd_vc") == 0) cl = ICL_VAL;     \
    else return RANGE_ERROR; \
}


extern "C" stream_id log_output_, warning_output_, current_err_;

extern "C" void ec_trail_undo(void(*f)(pword*,word*,int,int), pword*, pword*, word*, int, int);

extern "C" int ec_flush(stream_id);

extern "C" int p_fprintf(stream_id, const char*, ...);

#if defined(WIN32)

extern "C" stream_id Winapi ec_stream_id(int);
# define log_output_ ec_stream_id(ec_stream_nr("log_output"))
# define current_err_ ec_stream_id(ec_stream_nr("current_err"))

#endif

using namespace Gecode;


int
get_handle_from_arg(int arg, t_ext_type *method, void **obj)
{
    return EC_arg(arg).is_handle(method, obj);
}

static void _free_space_handle(GecodeSpace** solverp)
{
    if (*solverp != NULL) delete *solverp;
    delete solverp;
}

t_ext_type gfd_method = {
    (void (*)(t_ext_ptr)) _free_space_handle, /* free */
    NULL, /* copy */ 
    NULL, /* mark_dids */
    NULL, /* string_size */
    NULL, /* to_string */
    NULL, /* equal */
    NULL, /* remote_copy */
    NULL, /* get */
    NULL, /* set */
};

static void _free_search_handle(GecodeSearch* searchp)
{
    delete searchp;
}

t_ext_type gfdsearch_method = {
    (void (*)(t_ext_ptr)) _free_search_handle, /* free */
    NULL, /* copy */ 
    NULL, /* mark_dids */
    NULL, /* string_size */
    NULL, /* to_string */
    NULL, /* equal */
    NULL, /* remote_copy */
    NULL, /* get */
    NULL, /* set */
};

static void _g_delete_space(pword* phandle, word * dummy, int size, int flags)
{
    GecodeSpace** solverp;

    ec_get_handle(phandle[SPACE_HANDLE_POS], &gfd_method, (void**)&solverp);
    if (*solverp != NULL) {
	delete *solverp;
	*solverp = NULL;
    }
}

static void _free_domain_handle(IntSet* domainp)
{
    if (domainp != NULL) delete domainp;
}

t_ext_type domain_method = {
    (void (*)(t_ext_ptr)) _free_domain_handle, /* free */
    NULL, /* copy */ 
    NULL, /* mark_dids */
    NULL, /* string_size */
    NULL, /* to_string */
    NULL, /* equal */
    NULL, /* remote_copy */
    NULL, /* get */
    NULL, /* set */
};

int get_domain_intervals_from_ec_array(int size, EC_word ecarr, int r[][2])
{
    EC_word arg;
    long min, max;

    for (int i=0; i<size; i++) {
	arg = EC_argument(ecarr, i+1);
	if (arg.arity() == 2) {
	    if (EC_argument(arg, 1).is_long(&min) != EC_succeed) return TYPE_ERROR;
	    if (EC_argument(arg, 2).is_long(&max) != EC_succeed) return TYPE_ERROR;

	    r[i][0] = (int)min;
	    r[i][1] = (int)max;
	} else 
	    return TYPE_ERROR;
    }
    return EC_succeed;
}

int assign_IntVarArgs_from_ec_array(GecodeSpace* solver, int size, 
				    EC_word ecarr, IntVarArgs& vargs)
{
    EC_functor f;
    EC_word arg;
    long l;

    try {
	for(int i=0; i<size; i++) {
	    arg = EC_argument(ecarr, i+1);
	    if (arg.functor(&f) == EC_succeed) {
		if  (strcmp(f.name(), "_ivar") == 0
		     && EC_argument(arg, 1).is_long(&l) == EC_succeed) {
		    vargs[i] = solver->vInt[(int)l];
		} else
		    return RANGE_ERROR;
	    } else if (arg.is_long(&l) == EC_succeed) {
		vargs[i] = IntVar(*solver,(int)l,(int)l);
	    } else
		return TYPE_ERROR;
	}
	return EC_succeed;
    }
    CatchAndReportGecodeExceptions
}

int assign_IntArgs_from_ec_array(GecodeSpace* solver, int size, 
				 EC_word ecarr, IntArgs& vargs)
{
    EC_word arg;
    long l;

    try {
	for(int i=0; i<size; i++) {
	    arg = EC_argument(ecarr, i+1);
	    if (arg.is_long(&l) == EC_succeed) {
		vargs[i] = (int)l;
	    } else
		return TYPE_ERROR;
	}
	return EC_succeed;
    }
    CatchAndReportGecodeExceptions
}

int assign_BoolVarArgs_from_ec_array(GecodeSpace* solver, int size, 
				    EC_word ecarr, BoolVarArgs& vargs)
{
    EC_functor f;
    EC_word arg;
    long l;

    try {
	for(int i=0; i<size; i++) {
	    arg = EC_argument(ecarr, i+1);
	    if (arg.functor(&f) == EC_succeed) {
		if  (strcmp(f.name(), "_ivar") == 0
		     && EC_argument(arg, 2).is_long(&l) == EC_succeed) {
		    vargs[i] = solver->vBool[(int)l];
		} else
		    return RANGE_ERROR;
	    } else if (arg.is_long(&l) == EC_succeed) {
		if (l < 0 || l > 1) return RANGE_ERROR;
 		vargs[i] = BoolVar(*solver,(int)l,(int)l);
	    } else {
		return TYPE_ERROR;
	    }
	}
	return EC_succeed;
    }
    CatchAndReportGecodeExceptions
}

void cache_domain_sizes(GecodeSpace* solver) {
    int snapshotsize = solver->dom_snapshot.size();
    int varsize = solver->vInt.size();

    if (varsize > snapshotsize) {
	solver->dom_snapshot.reserve(varsize);
    }
    if (!solver->snapshot_valid()) {
	for (int i=1; i < snapshotsize; i++) {
	    solver->dom_snapshot[i] = solver->vInt[i].size();
	}
	solver->set_snapshot();
    }
    for (int i = snapshotsize; i<varsize; i++) {
	    solver->dom_snapshot.push_back(solver->vInt[i].size());
    }

}

extern "C"
int p_g_init()
{
    GecodeSpace** solverp = new GecodeSpace*;

    *solverp = NULL;
    return unify(EC_arg(1), handle(&gfd_method, solverp));
}

extern "C"
int p_g_trail_undo_for_event()
{
    GecodeSpace** solverp;
    EC_functor f;
    EC_word w;

    if (EC_arg(1).functor(&f) != EC_succeed) return TYPE_ERROR;
    if (strcmp(f.name(), "gfd_space") != 0) return TYPE_ERROR;
    EC_arg(1).arg(SPACE_HANDLE_POS, w);
    if (w.is_handle(&gfd_method, (void**)&solverp) != EC_succeed) 
	return TYPE_ERROR;

    if (*solverp == NULL) return TYPE_ERROR; // should not happen!

    ec_trail_undo(_g_delete_space, ec_arg(1).val.ptr, ec_arg(1).val.ptr+SPACE_STAMP_POS, NULL, 0, TRAILED_WORD32);
    return EC_succeed;
}

extern "C"
int p_g_state_is_stable()
{
    GecodeSpace** solverp;
    EC_functor f;
    EC_word w;

    if (EC_arg(1).functor(&f) != EC_succeed) return TYPE_ERROR;
    EC_arg(1).arg(SPACE_HANDLE_POS, w);
    if (w.is_handle(&gfd_method, (void**)&solverp) != EC_succeed) 
	return TYPE_ERROR;
    try {
	return ((*solverp)->stable() ? EC_succeed : EC_fail);
    }
    CatchAndReportGecodeExceptions

}

extern "C"
int p_g_check_handle()
{
    // trail_undo cannot be done here, because this space handle may be cloned
    // and become an ancestor and not used for the current event
    GecodeSpace** solverp;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp)) 
	return TYPE_ERROR;

    if (*solverp == NULL) { // no valid current solver space, clone ancestor
	if (EC_succeed == EC_arg(2).is_nil()) { // no ancestor, make new space
	    *solverp = new GecodeSpace();
	} else { // clone ancestor
	    GecodeSpace** ancestorp;
	    EC_functor f;
	    EC_word w;

	    if (EC_arg(2).functor(&f) != EC_succeed) return TYPE_ERROR;
	    if (strcmp(f.name(), "gfd_space") != 0) return TYPE_ERROR;
	    EC_arg(2).arg(SPACE_HANDLE_POS, w);
	    if (w.is_handle(&gfd_method, (void**)&ancestorp) != EC_succeed) 
		return TYPE_ERROR;
	    *solverp = static_cast<GecodeSpace*>((*ancestorp)->clone());
	}

	return unify(EC_arg(3), EC_word(1));
    } else {
	// not cloned
	return unify(EC_arg(3), EC_word(0));
    }
}


extern "C"
int p_g_delete()
{
    GecodeSpace** solverp;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp)) 
	return TYPE_ERROR;

    if (*solverp != NULL) delete *solverp;

    return EC_succeed;
}

extern "C"
int p_g_get_var_value()
{
    long idx;
    int val;
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return EC_fail;

    if (EC_succeed != EC_arg(2).is_long(&idx)) return(TYPE_ERROR);

    try {
	val = solver->vInt[(int)idx].val();
	return unify(EC_arg(3), EC_word((long)val));
    }
    catch(Int::ValOfUnassignedVar) {
	return EC_fail;
    }
    CatchAndReportGecodeExceptions
}

extern "C"
int p_g_check_val_is_in_var_domain()
{
    long idx, val;
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return EC_fail;

    if (EC_succeed != EC_arg(2).is_long(&idx)) return(TYPE_ERROR);

    if (EC_succeed != EC_arg(3).is_long(&val)) return(TYPE_ERROR);

    try {
	if (solver->vInt[(int)idx].in((int)val))
	    return EC_succeed; 
	else return EC_fail;
    }
    CatchAndReportGecodeExceptions

}

extern "C"
int p_g_get_var_bounds()
{
    long idx;
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (EC_succeed != EC_arg(2).is_long(&idx)) return(TYPE_ERROR);

    /* idx outside current array size ==> idx is for new variable that
       has not yet been added to the solver space. Normally the domain of 
       such variables cannot be accessed, but exceptions such as the tracer
       tracing the ECLiPSe level code can occur internally. Just return []
    */
    if (solver == NULL || idx >= solver->vInt.size()) return RANGE_ERROR;
    try {
	int res;
	res = unify(EC_arg(3), EC_word(solver->vInt[(int)idx].min()));
	if (res != EC_succeed) return res;
	return unify(EC_arg(4), EC_word(solver->vInt[(int)idx].max()));
    }
    CatchAndReportGecodeExceptions

}

extern "C"
int p_g_get_var_lwb()
{
    long idx;
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (EC_succeed != EC_arg(2).is_long(&idx)) return(TYPE_ERROR);

    if (solver == NULL || idx >= solver->vInt.size()) return RANGE_ERROR;

    try {
	return unify(EC_arg(3), EC_word(solver->vInt[(int)idx].min()));
    }
    CatchAndReportGecodeExceptions
}

extern "C"
int p_g_update_and_get_var_bound()
{
    long idx;
    EC_functor f;
    EC_word w;
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_arg(1).functor(&f) != EC_succeed) return TYPE_ERROR;
    if (strcmp(f.name(), "gfd_space") != 0) return TYPE_ERROR;
    EC_arg(1).arg(SPACE_HANDLE_POS, w);
    if (w.is_handle(&gfd_method, (void**)&solverp) != EC_succeed) 
	return TYPE_ERROR;

    if (*solverp == NULL) return TYPE_ERROR; 

    solver = *solverp;
    if (EC_succeed != EC_arg(2).is_long(&idx)) return(TYPE_ERROR);

    if (solver == NULL || idx >= solver->vInt.size()) return RANGE_ERROR;
    long val;
    if (EC_succeed != EC_arg(3).is_long(&val)) return(TYPE_ERROR);

    // update lower bound before getting it
    long which;
    if (EC_succeed != EC_arg(4).is_long(&which)) return(TYPE_ERROR);
    IntRelType relop = (which == -1 ? IRT_GR : IRT_LE);

    cache_domain_sizes(solver); // get snapshot 

    try {
	rel(*solver, solver->vInt[(int)idx], relop, (int)val);

	// we need to do trail undo here so that if failure occurs before
	// an event is executed, the current space is correctly discarded
	ec_trail_undo(_g_delete_space, ec_arg(1).val.ptr, ec_arg(1).val.ptr+SPACE_STAMP_POS, NULL, 0, TRAILED_WORD32);

	if (!solver->status()) return EC_fail;

	return (which == -1 ? 
		unify(EC_arg(5), EC_word(solver->vInt[(int)idx].min())) :
		unify(EC_arg(5), EC_word(solver->vInt[(int)idx].max())));

    }
    CatchAndReportGecodeExceptions
}

extern "C"
int p_g_get_var_upb()
{
    long idx;
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (EC_succeed != EC_arg(2).is_long(&idx)) return(TYPE_ERROR);

    if (solver == NULL || idx >= solver->vInt.size()) return RANGE_ERROR;

    try {
	return unify(EC_arg(3), EC_word(solver->vInt[(int)idx].max()));
    }
    CatchAndReportGecodeExceptions
}

extern "C"
int p_g_get_var_domain_size()
{
    long idx;
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (EC_succeed != EC_arg(2).is_long(&idx)) return(TYPE_ERROR);

    if (solver == NULL || idx >= solver->vInt.size()) return RANGE_ERROR;

    try {
	return unify(EC_arg(3), EC_word((long)solver->vInt[(int)idx].size()));
    }
    CatchAndReportGecodeExceptions
}

extern "C"
int p_g_get_var_domain_width()
{
    long idx;
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (EC_succeed != EC_arg(2).is_long(&idx)) return(TYPE_ERROR);

    if (solver == NULL || idx >= solver->vInt.size()) return RANGE_ERROR;

    try {
	return unify(EC_arg(3), EC_word((long)solver->vInt[(int)idx].width()));
    }
    CatchAndReportGecodeExceptions
}

extern "C"
int p_g_get_var_median()
{
    long idx;
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (EC_succeed != EC_arg(2).is_long(&idx)) return(TYPE_ERROR);

    if (solver == NULL || idx >= solver->vInt.size()) return RANGE_ERROR;

    try {
	return unify(EC_arg(3), EC_word((long)solver->vInt[(int)idx].med()));
    }
    CatchAndReportGecodeExceptions
}

extern "C"
int p_g_get_var_degree()
{
    long idx;
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (EC_succeed != EC_arg(2).is_long(&idx)) return(TYPE_ERROR);

    if (solver == NULL || idx >= solver->vInt.size()) return RANGE_ERROR;

    try {
	return unify(EC_arg(3), EC_word((long)solver->vInt[(int)idx].degree()));
    }
    CatchAndReportGecodeExceptions
}

extern "C"
int p_g_get_var_afc()
{
    long idx;
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (EC_succeed != EC_arg(2).is_long(&idx)) return(TYPE_ERROR);

    if (solver == NULL || idx >= solver->vInt.size()) return RANGE_ERROR;

    try {
	return unify(EC_arg(3), EC_word((long)solver->vInt[(int)idx].afc()));
    }
    CatchAndReportGecodeExceptions
}

extern "C"
int p_g_get_var_regret_lwb()
{
    long idx;
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (EC_succeed != EC_arg(2).is_long(&idx)) return(TYPE_ERROR);

    if (solver == NULL || idx >= solver->vInt.size()) return RANGE_ERROR;

    try {
	return unify(EC_arg(3), EC_word((long)solver->vInt[(int)idx].regret_min()));
    }
    CatchAndReportGecodeExceptions
}

extern "C"
int p_g_get_var_regret_upb()
{
    long idx;
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (EC_succeed != EC_arg(2).is_long(&idx)) return(TYPE_ERROR);

    if (solver == NULL || idx >= solver->vInt.size()) return RANGE_ERROR;

    try {
	return unify(EC_arg(3), EC_word((long)solver->vInt[(int)idx].regret_max()));
    }
    CatchAndReportGecodeExceptions
}

extern "C"
int p_g_get_var_domain()
{
    long idx;
    int min, max;
    int first = 0;
    EC_word l, tail, oldtail, domlist; 
    EC_functor dotdot((char*)"..", 2);
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (EC_succeed != EC_arg(2).is_long(&idx)) return(TYPE_ERROR);

    /* idx outside current array size ==> idx is for new variable that
       has not yet been added to the solver space. Normally the domain of 
       such variables cannot be accessed, but exceptions such as the tracer
       tracing the ECLiPSe level code can occur internally. Just return []
       Also return [] if solver not defined -- state have not been recomputed
    */
    try {
	if (solver == NULL || idx >= solver->vInt.size()) return unify(EC_arg(3), nil());
 
	for (IntVarRanges i(solver->vInt[idx]); i(); ++i) {
	    min = i.min();
	    max = i.max();
	    if (min == max) { // single number
		l = list(EC_word(max), tail = ec_newvar());
	    } else if (min == max - 1) { // 2 value interval
		l = list(EC_word(min), list(EC_word(max), tail = ec_newvar()));
	    } else {
		l = list(term(dotdot, EC_word(min), EC_word(max)), tail = ec_newvar());
	    }
	    if (first == 0) domlist = l;
	    else unify(oldtail, l);
	    first = 1;
	    oldtail = tail;
	}
    }
    CatchAndReportGecodeExceptions

    unify(oldtail, nil());

    return (unify(EC_arg(3), domlist));
}

extern "C"
int p_g_add_newvars_interval()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;
    long newsize, min, max;
    int oldsize, snapshotsize;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;
    if (EC_succeed != EC_arg(2).is_long(&newsize)) return(TYPE_ERROR);
    if (EC_succeed != EC_arg(3).is_long(&min)) return(TYPE_ERROR);
    if (EC_succeed != EC_arg(4).is_long(&max)) return(TYPE_ERROR);
    oldsize = solver->vInt.size();

    try {
      //	solver->vInt.resize(*solver, (int)++newsize); // ++ as we don't use 0
      for (int i=oldsize; i <= (int)newsize; i++) {
	  solver->vInt << IntVar(*solver, (int)min, (int)max);
      }
    }
    CatchAndReportGecodeExceptions

    return EC_succeed;
}


extern "C"
int p_g_add_newbool()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    try {
        solver->vBool << BoolVar(*solver,0,1);
	int bidx = solver->vBool.size()-1;
	long i;

	if (EC_succeed != EC_arg(2).is_long(&i)) return TYPE_ERROR;
	channel(*solver, solver->vInt[(int)i], solver->vBool[bidx]);
	return unify(EC_arg(3), EC_word(bidx));
    }
    CatchAndReportGecodeExceptions

}

extern "C"
int p_g_add_newvars_dom()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;
    long newsize;
    int oldsize;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;
    if (EC_succeed != EC_arg(2).is_long(&newsize)) return(TYPE_ERROR);
    oldsize = solver->vInt.size();

    EC_word darr = EC_arg(3);
    int dsize = darr.arity();
    int ranges[dsize][2];
    if (dsize == 0) return TYPE_ERROR;

    int res = get_domain_intervals_from_ec_array(dsize, darr, ranges);
    if (res != EC_succeed) return res;
    IntSet domset(ranges, dsize);

    try {
      //	solver->vInt.resize(*solver, (int)++newsize); // ++ to skip over idx 0
	for (int i=oldsize; i <= (int)newsize; i++)
	  solver->vInt << IntVar(*solver, domset);
    }
    CatchAndReportGecodeExceptions

    return EC_succeed;
}

#if 0
LinExpr<IntVar>
#else
LinExpr
#endif
ec2intexpr(EC_word e, GecodeSpace* solver)
{
    long l;
    int i;
    EC_functor f;
#if 0
    LinExpr<IntVar> arg1, arg2;
#else
    LinExpr arg1(BoolVar(*solver,1,1)), arg2(BoolVar(*solver,1,1));
#endif
    if (e.functor(&f) == EC_succeed) {
	if (strcmp(f.name(), "_ivar") == 0 && 
	    EC_argument(e, 1).is_long(&l) == EC_succeed) {
	    i = (int)l;
	    return solver->vInt[i];
	}
	switch (f.arity()) {
	    case 1: {
		if (strcmp(f.name(), "-") == 0) { // -Expr
		    arg2 = ec2intexpr(EC_argument(e, 1), solver);
		    return -arg2;
				      
		}
		if (strcmp(f.name(), "abs") == 0) { 
		    arg2 = ec2intexpr(EC_argument(e, 1), solver);
		    return abs(arg2);
		}
		if (strcmp(f.name(), "sqr") == 0) { 
		    arg2 = ec2intexpr(EC_argument(e, 1), solver);
		    return sqr(arg2);
		}
		if (strcmp(f.name(), "sqrt") == 0) { 
		    arg2 = ec2intexpr(EC_argument(e, 1), solver);
		    return sqrt(arg2);
		}
		if (strcmp(f.name(), "sum") == 0) { 
		  EC_word varr = EC_argument(e,1);
		  int size = varr.arity();
		  if (size > 0) {
		    IntVarArgs vars(size);
		    int res = assign_IntVarArgs_from_ec_array(solver, size, varr, vars);
		    return sum(vars);
		  }
		}
		if (strcmp(f.name(), "max") == 0) { 
		  EC_word varr = EC_argument(e,1);
		  int size = varr.arity();
		  if (size > 0) {
		    IntVarArgs vars(size);
		    int res = assign_IntVarArgs_from_ec_array(solver, size, varr, vars);
		    return max(vars);
		  }
		}
		if (strcmp(f.name(), "min") == 0) { 
		  EC_word varr = EC_argument(e,1);
		  int size = varr.arity();
		  if (size > 0) {
		    IntVarArgs vars(size);
		    if (assign_IntVarArgs_from_ec_array(solver, size, varr, vars) == EC_succeed)
		      return min(vars);
		  }
		}
		// Unknown unary expression or error found
		throw Ec2gcException();
	    }
	    case 2: {
	        if (strcmp(f.name(), "sum") == 0) {
		  EC_word carr = EC_argument(e,1);
		  EC_word varr = EC_argument(e,2);
		  int size = varr.arity();
		  if (size > 0) {
		    IntVarArgs vars(size);
		    if (assign_IntVarArgs_from_ec_array(solver, size, varr, vars) != EC_succeed)
		      throw Ec2gcException();
		    IntArgs cs(size);
		    if (assign_IntArgs_from_ec_array(solver, size, carr, cs) == EC_succeed)
		      return sum(cs, vars);
		  }
		} else {
		  arg2 = ec2intexpr(EC_argument(e, 2), solver);
		  if (strcmp(f.name(), "element") != 0) {
		    arg1 = ec2intexpr(EC_argument(e, 1), solver);
	
		    if (strcmp(f.name(), "+") == 0) { return (arg1 + arg2); };
		    if (strcmp(f.name(), "-") == 0) { return (arg1 - arg2); };
		    if (strcmp(f.name(), "*") == 0) { return (arg1 * arg2); };
		    if (strcmp(f.name(), "/") == 0) { return (arg1 / arg2); };
		    if (strcmp(f.name(), "mod") == 0) { return (arg1 % arg2); };
		    if (strcmp(f.name(), "min") == 0) { return min(arg1, arg2); };
		    if (strcmp(f.name(), "max") == 0) { return max(arg1, arg2); };
		  } else {
		    // element(<Vars>, <Expr>)
		    EC_word varr = EC_argument(e,2);
		    int size = varr.arity();
		    if (size > 0) {
		      IntVarArgs vars(size);
		      if (assign_IntVarArgs_from_ec_array(solver, size, varr, vars) == EC_succeed)
			return element(vars, arg2);
		    }
		  }
		}

		// Unknown binary expression
		throw Ec2gcException();
	    }

	    default:
		// Unknown compound expression
		throw Ec2gcException();
		break;
	}
    } else if (e.is_long(&l) == EC_succeed) { // Integer
        return (int)l;
        //i = (int)l;
	//	return (solver->vInt[0] + i); // vInt[0] has value 0, needed as dummy
    } else {
    // Unknown integer expression
    throw Ec2gcException();
    }
}

#if 0
LinRel<IntVar>
#else
LinRel
#endif
ec2intrel(EC_word c, GecodeSpace* solver)
{
    EC_functor f;

    if (c.functor(&f) == EC_succeed && f.arity() == 2) {
#if 0
	LinExpr<IntVar> arg1 = ec2intexpr(EC_argument(c,1), solver);
	LinExpr<IntVar> arg2 = ec2intexpr(EC_argument(c,2), solver);
#else
	LinExpr arg1 = ec2intexpr(EC_argument(c,1), solver);
	LinExpr arg2 = ec2intexpr(EC_argument(c,2), solver);
#endif	
	if (strcmp(f.name(), "#=") == 0) { return (arg1 == arg2); };
	if (strcmp(f.name(), "#\\=") == 0) { return (arg1 != arg2); };
	if (strcmp(f.name(), "#>") == 0) { return (arg1 > arg2); };
	if (strcmp(f.name(), "#<") == 0) { return (arg1 < arg2); };
	if (strcmp(f.name(), "#>=") == 0) { return (arg1 >= arg2); };
	if (strcmp(f.name(), "#=<") == 0) { return (arg1 <= arg2); };
	
	// Unknown binary constraint
	throw Ec2gcException();
    } else 
	throw Ec2gcException();
}

BoolExpr
ec2boolexpr(EC_word c, GecodeSpace*solver)
{
    EC_functor f;
    long l;

    if (c.functor(&f) == EC_succeed) { 
	switch (f.arity()) {
	    case 1: {
		if (strcmp(f.name(), "neg") == 0)
		    return ec2boolexpr(EC_argument(c,1), solver);
		break;
	    }
	    case 2: {
	
		if (strcmp(f.name(), "and") == 0) { 
		    return (ec2boolexpr(EC_argument(c,1), solver) && 
			    ec2boolexpr(EC_argument(c,2), solver)
			   ); 
		};
		if (strcmp(f.name(), "or") == 0) { 
		    return (ec2boolexpr(EC_argument(c,1), solver) || 
			    ec2boolexpr(EC_argument(c,2), solver)
			   ); 
		};
		if (strcmp(f.name(), "xor") == 0) { 
		    return (ec2boolexpr(EC_argument(c,1), solver) ^ 
			    ec2boolexpr(EC_argument(c,2), solver)
			   ); 
		};
		if (strcmp(f.name(), "=>") == 0) { 
		    return (ec2boolexpr(EC_argument(c,1), solver) >> 
			    ec2boolexpr(EC_argument(c,2), solver)
			   );
		};
		if (strcmp(f.name(), "<=>") == 0) { 
		    return (ec2boolexpr(EC_argument(c,1), solver) == 
			       ec2boolexpr(EC_argument(c,2), solver)
			   );
		};
		if (strcmp(f.name(), "_ivar") == 0) {
		    if (EC_argument(c,2).is_long(&l) != EC_succeed)
			throw Ec2gcException();
		    return solver->vBool[(int)l];
		}
		break;
	    }
	} 
	// otherwise, treat as linear relation
	return ec2intrel(c, solver);
    } else if (c.is_long(&l) == EC_succeed) {
	switch (l) {
	    case 0: 
		return solver->boolfalse;
		break;
	    case 1:
		return solver->booltrue;
		break;
	    default:
		throw Ec2gcException();
		break;
	}
    }

    // Unknown boolean expression
    throw Ec2gcException();
    }

extern "C"
int p_g_post_bool_connectives()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;
    IntConLevel cl;
    Get_Consistency_Level(4, cl);

    try {
	BoolExpr c = ec2boolexpr(EC_arg(3), solver);

	long first;
	if (EC_succeed != EC_arg(2).is_long(&first)) return TYPE_ERROR;
	if (first) cache_domain_sizes(solver);
	rel(*solver, c, cl);

	/* check for failure (without full propagation) */
	return (solver->failed() ? EC_fail : EC_succeed);
    }
    catch(Ec2gcException) {
	return TYPE_ERROR;
    }
    CatchAndReportGecodeExceptions
}


extern "C"
int p_g_post_intrel_cstr()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    IntConLevel cl;
    Get_Consistency_Level(4, cl);

    try {
#if 0
	LinRel<IntVar> c = ec2intrel(EC_arg(3), solver);
#else
	LinRel c = ec2intrel(EC_arg(3), solver);
#endif
	long first;
	if (EC_succeed != EC_arg(2).is_long(&first)) return TYPE_ERROR;
	if (first) cache_domain_sizes(solver);
	rel(*solver, c, cl);

	/* check for failure (without full propagation) */
	return (solver->failed() ? EC_fail : EC_succeed);
    }
    catch(Ec2gcException) {
	return TYPE_ERROR;
    }
    CatchAndReportGecodeExceptions
}

extern "C"
int p_g_post_setvar()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;
    long l;
    int idx, val;

    if (EC_arg(3).is_long(&l) != EC_succeed) return TYPE_ERROR;
    idx = (int) l;
    if (idx < 0) return RANGE_ERROR;
    if (EC_arg(4).is_long(&l) != EC_succeed) return TYPE_ERROR;
    val = (int) l;
    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;
    if (idx >= solver->vInt.size()) {
	return RANGE_ERROR;
    }

    long first;
    if (EC_succeed != EC_arg(2).is_long(&first)) return TYPE_ERROR;
    if (first) cache_domain_sizes(solver);

    try {
	dom(*solver, solver->vInt[idx], (int)val);
//    rel(*solver, solver->vInt[idx], IRT_EQ, (int)val);
	if (first) solver->dom_snapshot[idx] = 1; // just assigned!

	return (solver->failed() ? EC_fail : EC_succeed);
    }
    CatchAndReportGecodeExceptions

}

#define CheckAndMakeChanged(snapshotsolver, tail, chgtail) \
{ \
    int dsize; \
    for (int i=1; i<snapshotsize; i++) { \
	dsize = solver->vInt[i].size();  \
	if (snapshotsolver->dom_snapshot[i] > dsize) { \
	    if (dsize == 1) { \
		tail = list(i,tail); \
	    } else { \
		chgtail = list(i,chgtail); \
	    } \
	} \
    } \
}

extern "C"
int p_g_propagate()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    long first;
    if (EC_succeed != EC_arg(2).is_long(&first)) return TYPE_ERROR;

    try {
	if (!solver->status()) return EC_fail;
    }
    CatchAndReportGecodeExceptions
//    return EC_succeed;

//    if (first == 0) return EC_succeed;


    EC_word tail = nil(), chgtail = nil();

    if (first == 1 && solver->snapshot_valid()) {
//    int res;
	int snapshotsize = solver->dom_snapshot.size();

//    printf("size: %d,%d\n",EC_arg(2).arity(), snapshotsize);
//    if (EC_arg(3).arity() < snapshotsize) return RANGE_ERROR; 

	CheckAndMakeChanged(solver, tail, chgtail);
    }
    solver->clear_snapshot();

    if (unify(EC_arg(3), tail) != EC_succeed) {
	return EC_fail;
    } else {
	return unify(EC_arg(4), chgtail);
    }

}

extern "C"
int p_g_post_interval()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;
    long min, max;

    if (EC_succeed != EC_arg(4).is_long(&min)) return(TYPE_ERROR);
    if (EC_succeed != EC_arg(5).is_long(&max)) return(TYPE_ERROR);
    if (min > max) return RANGE_ERROR;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    EC_word varr =  EC_arg(3);
    int size = varr.arity();

    if (size == 0) return TYPE_ERROR;

    IntVarArgs vars(size);
    int res = assign_IntVarArgs_from_ec_array(solver, size, varr, vars);
    if (res != EC_succeed) return res;

    long first;
    if (EC_succeed != EC_arg(2).is_long(&first)) return TYPE_ERROR;
    if (first) cache_domain_sizes(solver);

    try {
	dom(*solver, vars, min, max);
    }
    CatchAndReportGecodeExceptions

    return (solver->failed() ? EC_fail : EC_succeed);
}

extern "C"
int p_g_post_var_interval_reif()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;
    long min, max;

    if (EC_succeed != EC_arg(4).is_long(&min)) return(TYPE_ERROR);
    if (EC_succeed != EC_arg(5).is_long(&max)) return(TYPE_ERROR);
//    if (min > max) return RANGE_ERROR;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    long xidx;
    IntVar x;
    EC_functor f;

    try {
	if (ArgIsVarIdx(3, xidx)) {
	    if (xidx < 1 || xidx >= solver->vInt.size()) return RANGE_ERROR;
	    x = solver->vInt[(int)xidx];
	} else if (EC_arg(3).is_long(&xidx) == EC_succeed) {
	    x = IntVar(*solver, (int)xidx, (int)xidx);
	} else
	    return TYPE_ERROR;

	long b;
	BoolVar reif;
	bool bool_is_set;

	if (ArgIsVarBoolIdx(6, b)) {
	    reif = solver->vBool[(int)b];
	    bool_is_set = false;
	} else if (EC_arg(6).is_long(&b) == EC_succeed) {
	    if (b < 0 || b > 1) return RANGE_ERROR;
	    bool_is_set = true;
	} else
	    return TYPE_ERROR;

	long first;
	if (EC_succeed != EC_arg(2).is_long(&first)) return TYPE_ERROR;
	if (first) cache_domain_sizes(solver);

	// Gecode does not seem to have the following:
	// dom(*solver, vars, min, max, reif);
	if (bool_is_set) {
	    if (b == 1) {
		dom(*solver, x, (int)min, (int)max);
		return (solver->failed() ? EC_fail : EC_succeed);
	    } else if (b == 0) 
		reif = BoolVar(*solver, 0, 0);
	    else
		return RANGE_ERROR;
	}

	dom(*solver, x, min, max, reif);
	return (solver->failed() ? EC_fail : EC_succeed);
    }
    CatchAndReportGecodeExceptions

}

extern "C"
int p_g_post_dom()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    EC_word darr = EC_arg(4);
    int dsize = darr.arity();
    int ranges[dsize][2];
    if (dsize == 0) return TYPE_ERROR;

    EC_word varr =  EC_arg(3);
    int size = varr.arity();
    if (size == 0) return TYPE_ERROR;

    int res = get_domain_intervals_from_ec_array(dsize, darr, ranges);
    if (res != EC_succeed) return res;
    try {
	IntSet domset(ranges, dsize);

	IntVarArgs vars(size);
	res = assign_IntVarArgs_from_ec_array(solver, size, varr, vars);
	if (res != EC_succeed) return res;

	long first;
	if (EC_succeed != EC_arg(2).is_long(&first)) return TYPE_ERROR;
	if (first) cache_domain_sizes(solver);

	dom(*solver, vars, domset);

	return (solver->failed() ? EC_fail : EC_succeed);
    }
    CatchAndReportGecodeExceptions
}

extern "C"
int p_g_post_var_dom_reif()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    try {
	long xidx;
	IntVar x;
	EC_functor f;

	if (ArgIsVarIdx(3, xidx)) {
	    if (xidx < 1 || xidx >= solver->vInt.size()) return RANGE_ERROR;
	    x = solver->vInt[(int)xidx];
	} else if (EC_arg(3).is_long(&xidx) == EC_succeed) {
	    x = IntVar(*solver, (int)xidx, (int)xidx);
	} else
	    return TYPE_ERROR;

	EC_word darr = EC_arg(4);
	int dsize = darr.arity();
	int ranges[dsize][2];
	if (dsize == 0) return TYPE_ERROR;

	int res = get_domain_intervals_from_ec_array(dsize, darr, ranges);
	if (res != EC_succeed) return res;
	IntSet domset(ranges, dsize);

	long b;
	BoolVar reif;
	bool bool_is_set;

	if (ArgIsVarBoolIdx(5, b)) {
	    reif = solver->vBool[(int)b];
	    bool_is_set = false;
	} else if (EC_arg(5).is_long(&b) == EC_succeed) {
	    if (b < 0 || b > 1) return RANGE_ERROR;
	    bool_is_set = true;
	} else
	    return TYPE_ERROR;

	long first;
	if (EC_succeed != EC_arg(2).is_long(&first)) return TYPE_ERROR;
	if (first) cache_domain_sizes(solver);

	if (bool_is_set) {
	    if (b == 1) {
		dom(*solver, x, domset);
		return (solver->failed() ? EC_fail : EC_succeed);
	    } else if (b == 0) 
		reif = BoolVar(*solver, 0, 0);
	    else 
		return RANGE_ERROR;
	}

	dom(*solver, x, domset, reif);
	return (solver->failed() ? EC_fail : EC_succeed);
    }
    CatchAndReportGecodeExceptions
}

extern "C"
int p_g_post_var_val_reif()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    long xidx;
    IntVar x;
    EC_functor f;

    try {
	if (ArgIsVarIdx(3, xidx)) {
	    if (xidx < 1 || xidx >= solver->vInt.size()) return RANGE_ERROR;
	    x = solver->vInt[(int)xidx];
	} else if (EC_arg(3).is_long(&xidx) == EC_succeed) {
	    x = IntVar(*solver, (int)xidx, (int)xidx);
	} else
	    return TYPE_ERROR;

	long val;

	if (EC_succeed != EC_arg(4).is_long(&val)) return(TYPE_ERROR);

	long b;
	BoolVar reif;
	bool bool_is_set;

	if (ArgIsVarBoolIdx(5, b)) {
	    reif = solver->vBool[(int)b];
	    bool_is_set = false;
	} else if (EC_arg(5).is_long(&b) == EC_succeed) {
	    if (b < 0 || b > 1) return RANGE_ERROR;
	    bool_is_set = true;
	} else
	    return TYPE_ERROR;

	long first;
	if (EC_succeed != EC_arg(2).is_long(&first)) return TYPE_ERROR;
	if (first) cache_domain_sizes(solver);

	if (bool_is_set) {
	    if (b == 1) {
		dom(*solver, x, (int)val);
		return (solver->failed() ? EC_fail : EC_succeed);
	    } else if (b == 0) 
		reif = BoolVar(*solver, 0, 0);
	    else 
		return RANGE_ERROR;
	}

	dom(*solver, x, (int)val, reif);
	return (solver->failed() ? EC_fail : EC_succeed);
    }
    CatchAndReportGecodeExceptions
}

extern "C"
int p_g_post_sum()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    EC_word varr = EC_arg(3);
    int size = varr.arity();
    if (size == 0) return TYPE_ERROR;

    IntVarArgs vars(size);
    int res = assign_IntVarArgs_from_ec_array(solver, size, varr, vars);
    if (res != EC_succeed) return res;

    IntRelType rel;
    EC_atom r;
    if (EC_arg(4).is_atom(&r) != EC_succeed) return TYPE_ERROR;
    if (strcmp(r.name(), "#=") == 0) rel = IRT_EQ;
    else if (strcmp(r.name(), "#\\=") == 0) rel = IRT_NQ;
    else if (strcmp(r.name(), "#>") == 0) rel = IRT_GR;
    else if (strcmp(r.name(), "#<") == 0) rel = IRT_LE;
    else if (strcmp(r.name(), "#>=") == 0) rel = IRT_GQ;
    else if (strcmp(r.name(), "#=<") == 0) rel = IRT_LQ;
    else return TYPE_ERROR;

    long c;
    IntVar cvar;
    EC_functor f;
    bool c_is_int;
    if (ArgIsVarIdx(5, c)) {
        if (c < 1 || c >= solver->vInt.size()) return RANGE_ERROR;
        cvar = solver->vInt[(int)c];
        c_is_int = false;
    } else if (EC_arg(5).is_long(&c) == EC_succeed) {
        c_is_int = true;
    } else
        return TYPE_ERROR;

    IntConLevel cl;
    Get_Consistency_Level(6, cl);

    long first;
    if (EC_succeed != EC_arg(2).is_long(&first)) return TYPE_ERROR;
    if (first) cache_domain_sizes(solver);

    try {
      if (c_is_int) 
	  linear(*solver, vars, rel, (int)c, cl);
      else
	  linear(*solver, vars, rel, cvar, cl);
      return EC_succeed;
    }
    CatchAndReportGecodeExceptions
}
 
extern "C"
int p_g_post_sum_reif()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    EC_word varr = EC_arg(3);
    int size = varr.arity();
    if (size == 0) return TYPE_ERROR;

    IntVarArgs vars(size);
    int res = assign_IntVarArgs_from_ec_array(solver, size, varr, vars);
    if (res != EC_succeed) return res;

    IntRelType rel;
    EC_atom r;
    if (EC_arg(4).is_atom(&r) != EC_succeed) return TYPE_ERROR;
    if (strcmp(r.name(), "#=") == 0) rel = IRT_EQ;
    else if (strcmp(r.name(), "#\\=") == 0) rel = IRT_NQ;
    else if (strcmp(r.name(), "#>") == 0) rel = IRT_GR;
    else if (strcmp(r.name(), "#<") == 0) rel = IRT_LE;
    else if (strcmp(r.name(), "#>=") == 0) rel = IRT_GQ;
    else if (strcmp(r.name(), "#=<") == 0) rel = IRT_LQ;
    else return TYPE_ERROR;

    long c;
    IntVar cvar;
    EC_functor f;
    bool c_is_int;
    if (ArgIsVarIdx(5, c)) {
        if (c < 1 || c >= solver->vInt.size()) return RANGE_ERROR;
        cvar = solver->vInt[(int)c];
        c_is_int = false;
    } else if (EC_arg(5).is_long(&c) == EC_succeed) {
        c_is_int = true;
    } else
        return TYPE_ERROR;

    IntConLevel cl;
    Get_Consistency_Level(7, cl);

    long b;
    BoolVar reif;
    bool bool_is_set;

    if (ArgIsVarBoolIdx(6, b)) {
        reif = solver->vBool[(int)b];
	    } else if (EC_arg(6).is_long(&b) == EC_succeed) {
        if (b < 0 || b > 1) return RANGE_ERROR;
	reif = BoolVar(*solver, (int)b, (int)b);
    } else
        return TYPE_ERROR;

    long first;
    if (EC_succeed != EC_arg(2).is_long(&first)) return TYPE_ERROR;
    if (first) cache_domain_sizes(solver);

    try {
      if (c_is_int) 
	linear(*solver, vars, rel, (int)c, reif, cl);
      else
	linear(*solver, vars, rel, cvar, reif, cl);
      return EC_succeed;
    }
    CatchAndReportGecodeExceptions
}
 
extern "C"
int p_g_post_lin()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    EC_word varr = EC_arg(3);
    int size = varr.arity();
    if (size == 0) return TYPE_ERROR;

    IntVarArgs vars(size);
    int res = assign_IntVarArgs_from_ec_array(solver, size, varr, vars);
    if (res != EC_succeed) return res;

    EC_word carr = EC_arg(4);
    if (carr.arity() != size) return TYPE_ERROR;

    IntArgs cs(size);
    res = assign_IntArgs_from_ec_array(solver, size, carr, cs);
    if (res != EC_succeed) return res;

    IntRelType rel;
    EC_atom r;
    if (EC_arg(5).is_atom(&r) != EC_succeed) return TYPE_ERROR;
    if (strcmp(r.name(), "#=") == 0) rel = IRT_EQ;
    else if (strcmp(r.name(), "#\\=") == 0) rel = IRT_NQ;
    else if (strcmp(r.name(), "#>") == 0) rel = IRT_GR;
    else if (strcmp(r.name(), "#<") == 0) rel = IRT_LE;
    else if (strcmp(r.name(), "#>=") == 0) rel = IRT_GQ;
    else if (strcmp(r.name(), "#=<") == 0) rel = IRT_LQ;
    else return TYPE_ERROR;

    long c;
    IntVar cvar;
    EC_functor f;
    bool c_is_int;
    if (ArgIsVarIdx(6, c)) {
        if (c < 1 || c >= solver->vInt.size()) return RANGE_ERROR;
        cvar = solver->vInt[(int)c];
        c_is_int = false;
    } else if (EC_arg(6).is_long(&c) == EC_succeed) {
        c_is_int = true;
    } else
        return TYPE_ERROR;

    IntConLevel cl;
    Get_Consistency_Level(7, cl);

    long first;
    if (EC_succeed != EC_arg(2).is_long(&first)) return TYPE_ERROR;
    if (first) cache_domain_sizes(solver);

    try {
      if (c_is_int) 
	  linear(*solver, cs, vars, rel, (int)c, cl);
      else
	  linear(*solver, cs, vars, rel, cvar, cl);
      return EC_succeed;
    }
    CatchAndReportGecodeExceptions
}
 
extern "C"
int p_g_post_lin_reif()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    EC_word varr = EC_arg(3);
    int size = varr.arity();
    if (size == 0) return TYPE_ERROR;

    IntVarArgs vars(size);
    int res = assign_IntVarArgs_from_ec_array(solver, size, varr, vars);
    if (res != EC_succeed) return res;

    EC_word carr = EC_arg(4);
    if (carr.arity() != size) return TYPE_ERROR;

    IntArgs cs(size);
    res = assign_IntArgs_from_ec_array(solver, size, carr, cs);
    if (res != EC_succeed) return res;

    IntRelType rel;
    EC_atom r;
    if (EC_arg(5).is_atom(&r) != EC_succeed) return TYPE_ERROR;
    if (strcmp(r.name(), "#=") == 0) rel = IRT_EQ;
    else if (strcmp(r.name(), "#\\=") == 0) rel = IRT_NQ;
    else if (strcmp(r.name(), "#>") == 0) rel = IRT_GR;
    else if (strcmp(r.name(), "#<") == 0) rel = IRT_LE;
    else if (strcmp(r.name(), "#>=") == 0) rel = IRT_GQ;
    else if (strcmp(r.name(), "#=<") == 0) rel = IRT_LQ;
    else return TYPE_ERROR;

    long c;
    IntVar cvar;
    EC_functor f;
    bool c_is_int;
    if (ArgIsVarIdx(6, c)) {
        if (c < 1 || c >= solver->vInt.size()) return RANGE_ERROR;
        cvar = solver->vInt[(int)c];
        c_is_int = false;
    } else if (EC_arg(6).is_long(&c) == EC_succeed) {
        c_is_int = true;
    } else
        return TYPE_ERROR;

    IntConLevel cl;
    Get_Consistency_Level(8, cl);

    long b;
    BoolVar reif;
    bool bool_is_set;

    if (ArgIsVarBoolIdx(7, b)) {
        reif = solver->vBool[(int)b];
	    } else if (EC_arg(7).is_long(&b) == EC_succeed) {
        if (b < 0 || b > 1) return RANGE_ERROR;
	reif = BoolVar(*solver, (int)b, (int)b);
    } else
        return TYPE_ERROR;

    long first;
    if (EC_succeed != EC_arg(2).is_long(&first)) return TYPE_ERROR;
    if (first) cache_domain_sizes(solver);

    try {
      if (c_is_int) 
	linear(*solver, cs, vars, rel, (int)c, reif, cl);
      else
	linear(*solver, cs, vars, rel, cvar, reif, cl);
      return EC_succeed;
    }
    CatchAndReportGecodeExceptions
}
 
extern "C"
int p_g_post_alldiff()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    EC_word varr = EC_arg(3);
    int size = varr.arity();
    if (size == 0) return TYPE_ERROR;

    IntVarArgs alldiff(size);
    int res = assign_IntVarArgs_from_ec_array(solver, size, varr, alldiff);
    if (res != EC_succeed) return res;

    IntConLevel cl;
    Get_Consistency_Level(4, cl);

    long first;
    if (EC_succeed != EC_arg(2).is_long(&first)) return TYPE_ERROR;
    if (first) cache_domain_sizes(solver);

    try {
	distinct(*solver, alldiff, cl);
    }
    CatchAndReportGecodeExceptions
    return EC_succeed;
}

extern "C"
int p_g_post_alldiff_offsets()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    EC_word varr = EC_arg(3);
    int size = varr.arity();
    if (size == 0) return TYPE_ERROR;

    IntVarArgs alldiff(size);
    int res = assign_IntVarArgs_from_ec_array(solver, size, varr, alldiff);
    if (res != EC_succeed) return res;

    EC_word oarr = EC_arg(4);
    if (oarr.arity() != size) return TYPE_ERROR;

    IntArgs offsets(size);
    res = assign_IntArgs_from_ec_array(solver, size, oarr, offsets);
    if (res != EC_succeed) return res;


    IntConLevel cl;
    Get_Consistency_Level(5, cl);

    long first;
    if (EC_succeed != EC_arg(2).is_long(&first)) return TYPE_ERROR;
    if (first) cache_domain_sizes(solver);

    try {
	distinct(*solver, offsets, alldiff, cl);
    }
    CatchAndReportGecodeExceptions

    return EC_succeed;
}

extern "C"
int p_g_post_count()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;
    IntVar val, n;
    long vidx, nidx; 

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    EC_functor f;
    if (ArgIsVarIdx(3, vidx)) {
        if (vidx < 1 || vidx >= solver->vInt.size()) return RANGE_ERROR;
	val = solver->vInt[(int)vidx];
    } else if (EC_arg(3).is_long(&vidx) == EC_succeed) {
	val = IntVar(*solver, (int)vidx, (int)vidx);
    } else
        return TYPE_ERROR;


    IntRelType rel;
    EC_atom ecrel;
    if (EC_arg(5).is_atom(&ecrel) != EC_succeed) return TYPE_ERROR;
    if (strcmp(ecrel.name(), "#=") == 0) rel = IRT_EQ;
    else if (strcmp(ecrel.name(), "#\\=") == 0) rel = IRT_NQ;
    else if (strcmp(ecrel.name(), "#>") == 0) rel = IRT_GR;
    else if (strcmp(ecrel.name(), "#<") == 0) rel = IRT_LE;
    else if (strcmp(ecrel.name(), "#>=") == 0) rel = IRT_GQ;
    else if (strcmp(ecrel.name(), "#=<") == 0) rel = IRT_LQ;
    else return TYPE_ERROR;

    if (ArgIsVarIdx(6, nidx)) {
        if (nidx < 1 || nidx >= solver->vInt.size()) return RANGE_ERROR;
	n = solver->vInt[(int)nidx];
    } else if (EC_arg(6).is_long(&nidx) == EC_succeed) {
        n = IntVar(*solver, (int)nidx, (int)nidx);
    } else
        return TYPE_ERROR;

    EC_word varr = EC_arg(4);
    int size = varr.arity();
    if (size == 0) return TYPE_ERROR;

    IntVarArgs vars(size);
    int res = assign_IntVarArgs_from_ec_array(solver, size, varr, vars);
    if (res != EC_succeed) return res;

    IntConLevel cl;
    Get_Consistency_Level(7, cl);

    long first;
    if (EC_succeed != EC_arg(2).is_long(&first)) return TYPE_ERROR;
    if (first) cache_domain_sizes(solver);

    try {
	count(*solver, vars, val, rel, n, cl);
    }
    CatchAndReportGecodeExceptions

    return EC_succeed;
}

extern "C"
int p_g_post_gcc()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;
    long val, n;
    bool n_is_int;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    EC_word pvals = EC_arg(3);
    int specsize = pvals.arity();
    if (specsize == 0) return TYPE_ERROR;

    IntArgs vals(specsize);
    int res = assign_IntArgs_from_ec_array(solver, specsize, pvals, vals);
    if (res != EC_succeed) return res;

    EC_word poccurs = EC_arg(4);
    if (specsize != poccurs.arity()) return RANGE_ERROR;

    IntVarArgs occurs(specsize);
    res = assign_IntVarArgs_from_ec_array(solver, specsize, poccurs, occurs);
    if (res != EC_succeed) return res;

    EC_word varr = EC_arg(5);
    int size = varr.arity();
    if (size == 0) return TYPE_ERROR;

    IntVarArgs vars(size);
    res = assign_IntVarArgs_from_ec_array(solver, size, varr, vars);
    if (res != EC_succeed) return res;

    IntConLevel cl;
    Get_Consistency_Level(6, cl);

    long first;
    if (EC_succeed != EC_arg(2).is_long(&first)) return TYPE_ERROR;
    if (first) cache_domain_sizes(solver);

    try {
	count(*solver, vars, occurs, vals, cl);
    }
    CatchAndReportGecodeExceptions

    return EC_succeed;
}

extern "C"
int p_g_post_element()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;
    long v, i;
    bool v_is_int;
    IntVar vvar, ivar;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    try {
	EC_functor f;

	if (ArgIsVarIdx(5, v)) {
	    if (v < 1 || v >= solver->vInt.size()) return RANGE_ERROR;
	    vvar = solver->vInt[(int)v];
	    v_is_int = false;
	} else if (EC_arg(5).is_long(&v) == EC_succeed) {
	    v_is_int = true;
	} else
	    return TYPE_ERROR;

	if (ArgIsVarIdx(3, i)) {
	    if (i < 1 || i >= solver->vInt.size()) return RANGE_ERROR;
	    ivar = solver->vInt[(int)i];
	} else if (EC_arg(3).is_long(&i) == EC_succeed) {
	    ivar = IntVar(*solver, (int)i, (int)i);
	} else
	    return TYPE_ERROR;

	EC_word arr = EC_arg(4);
	int size = arr.arity();
	if (size == 0) return TYPE_ERROR;

	IntVarArgs vals(size);
	int res = assign_IntVarArgs_from_ec_array(solver, size, arr, vals);
	if (res != EC_succeed) return res;

	IntConLevel cl;
	Get_Consistency_Level(6, cl);

	long first;
	if (EC_succeed != EC_arg(2).is_long(&first)) return TYPE_ERROR;
	if (first) cache_domain_sizes(solver);

	if (v_is_int)
	    element(*solver, vals, ivar, v, cl);
	else
	    element(*solver, vals, ivar, vvar, cl);
    }
    CatchAndReportGecodeExceptions

    return EC_succeed;
}

extern "C"
int p_g_post_sequence()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;
    long val, n;
    bool n_is_int;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    EC_word pvals = EC_arg(7);
    int specsize = pvals.arity();
    if (specsize == 0) return TYPE_ERROR;

    IntArgs vals(specsize);
    int res = assign_IntArgs_from_ec_array(solver, specsize, pvals, vals);
    if (res != EC_succeed) return res;
    IntSet valset(vals);

    EC_word varr = EC_arg(6);
    int size = varr.arity();
    if (size == 0) return TYPE_ERROR;

    IntVarArgs vars(size);
    res = assign_IntVarArgs_from_ec_array(solver, size, varr, vars);
    if (res != EC_succeed) return res;

    long lo, hi, k;
    if (EC_succeed != EC_arg(3).is_long(&lo)) return TYPE_ERROR;
    if (EC_succeed != EC_arg(4).is_long(&hi)) return TYPE_ERROR;
    if (EC_succeed != EC_arg(5).is_long(&k)) return TYPE_ERROR;

    IntConLevel cl;
    Get_Consistency_Level(8, cl);

    long first;
    if (EC_succeed != EC_arg(2).is_long(&first)) return TYPE_ERROR;
    if (first) cache_domain_sizes(solver);

    try {
	sequence(*solver, vars, valset, k, lo, hi, cl);
    }
    CatchAndReportGecodeExceptions

    return EC_succeed;
}

extern "C"
int p_g_post_sequence_01()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;
    long val, n;
    bool n_is_int;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    IntSet valset(1,1);

    EC_word varr = EC_arg(6);
    int size = varr.arity();
    if (size == 0) return TYPE_ERROR;

    BoolVarArgs vars(size);
    int res = assign_BoolVarArgs_from_ec_array(solver, size, varr, vars);
    if (res != EC_succeed) return res;

    long lo, hi, k;
    if (EC_succeed != EC_arg(3).is_long(&lo)) return TYPE_ERROR;
    if (EC_succeed != EC_arg(4).is_long(&hi)) return TYPE_ERROR;
    if (EC_succeed != EC_arg(5).is_long(&k)) return TYPE_ERROR;

    IntConLevel cl;
    Get_Consistency_Level(7, cl);

    long first;
    if (EC_succeed != EC_arg(2).is_long(&first)) return TYPE_ERROR;
    if (first) cache_domain_sizes(solver);

    try {
	sequence(*solver, vars, valset, k, lo, hi, cl);
    }
    CatchAndReportGecodeExceptions

    return EC_succeed;
}

extern "C"
int p_g_post_sorted2()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    EC_word uarr = EC_arg(3);
    int size = uarr.arity();
    if (size == 0) return TYPE_ERROR;

    IntVarArgs unsort(size);
    int res = assign_IntVarArgs_from_ec_array(solver, size, uarr, unsort);
    if (res != EC_succeed) return res;

    EC_word sarr = EC_arg(4);
    if (sarr.arity() != size) return RANGE_ERROR;

    IntVarArgs sort(size);
    res = assign_IntVarArgs_from_ec_array(solver, size, sarr, sort);
    if (res != EC_succeed) return res;

    IntConLevel cl;
    Get_Consistency_Level(5, cl);

    long first;
    if (EC_succeed != EC_arg(2).is_long(&first)) return TYPE_ERROR;
    if (first) cache_domain_sizes(solver);

    try {
	sorted(*solver, unsort, sort, cl);
    }
    CatchAndReportGecodeExceptions

    return EC_succeed;
}

extern "C"
int p_g_post_sorted()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    EC_word uarr = EC_arg(3);
    int size = uarr.arity();
    if (size == 0) return TYPE_ERROR;

    IntVarArgs unsort(size);
    int res = assign_IntVarArgs_from_ec_array(solver, size, uarr, unsort);
    if (res != EC_succeed) return res;

    EC_word sarr = EC_arg(4);
    if (sarr.arity() != size) return RANGE_ERROR;

    IntVarArgs sort(size);
    res = assign_IntVarArgs_from_ec_array(solver, size, sarr, sort);
    if (res != EC_succeed) return res;

    EC_word parr = EC_arg(5);
    if (parr.arity() != size) return RANGE_ERROR;

    IntVarArgs pos(size);
    res = assign_IntVarArgs_from_ec_array(solver, size, parr, pos);
    if (res != EC_succeed) return res;

    IntConLevel cl;
    Get_Consistency_Level(6, cl);

    long first;
    if (EC_succeed != EC_arg(2).is_long(&first)) return TYPE_ERROR;
    if (first) cache_domain_sizes(solver);

    try {
	sorted(*solver, unsort, sort, pos, cl);
    }
    CatchAndReportGecodeExceptions

    return EC_succeed;
}


extern "C"
int p_g_post_disj()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    EC_word sarr = EC_arg(3);
    int size = sarr.arity();
    if (size == 0) return TYPE_ERROR;

    IntVarArgs starts(size);
    int res = assign_IntVarArgs_from_ec_array(solver, size, sarr, starts);
    if (res != EC_succeed) return res;

    EC_word darr = EC_arg(4);
    if (darr.arity() != size) return RANGE_ERROR;

//not in 3.3    IntVarArgs durations(size);
//    res = assign_IntVarArgs_from_ec_array(solver, size, darr, durations);
    IntArgs durations(size);
    res = assign_IntArgs_from_ec_array(solver, size, darr, durations);
    if (res != EC_succeed) return res;

    EC_word barr = EC_arg(5);
    BoolVarArgs scheduled(size);
    bool has_optional = (barr.arity() != 0);
    if (has_optional) {
	if (size != barr.arity()) return TYPE_ERROR;

	res = assign_BoolVarArgs_from_ec_array(solver, size, barr, scheduled);
	if (res != EC_succeed) return res;
    }

    long first;
    if (EC_succeed != EC_arg(2).is_long(&first)) return TYPE_ERROR;
    if (first) cache_domain_sizes(solver);

    try {
	// consistency level not supported!
	if (has_optional) {
	    unary(*solver, starts, durations, scheduled);
	} else {
	    unary(*solver, starts, durations);
	}
    }
    CatchAndReportGecodeExceptions

    return EC_succeed;
}


extern "C"
int p_g_post_cumulatives()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    EC_word sarr = EC_arg(3);
    int size = sarr.arity();
    if (size == 0) return TYPE_ERROR;

    IntVarArgs starts(size);
    int res = assign_IntVarArgs_from_ec_array(solver, size, sarr, starts);
    if (res != EC_succeed) return res;

    EC_word darr = EC_arg(4);
    if (darr.arity() != size) return RANGE_ERROR;

    IntVarArgs durations(size);
    res = assign_IntVarArgs_from_ec_array(solver, size, darr, durations);

    EC_word earr = EC_arg(5);
    if (earr.arity() != size) return RANGE_ERROR;

    IntVarArgs ends(size);
    res = assign_IntVarArgs_from_ec_array(solver, size, earr, ends);

    EC_word uarr = EC_arg(6);
    if (uarr.arity() != size) return RANGE_ERROR;

    IntVarArgs usages(size);
    res = assign_IntVarArgs_from_ec_array(solver, size, uarr, usages);

    EC_word usedarr = EC_arg(7);
    if (usedarr.arity() != size) return RANGE_ERROR;

    IntVarArgs used(size);
    res = assign_IntVarArgs_from_ec_array(solver, size, usedarr, used);

    EC_word larr = EC_arg(8);
    int nmachines = larr.arity();

    IntArgs limits(nmachines);
    res = assign_IntArgs_from_ec_array(solver, nmachines, larr, limits);
    if (res != EC_succeed) return res;

    long ec_atmost;
    bool atmost;
    if (EC_succeed != EC_arg(9).is_long(&ec_atmost)) return TYPE_ERROR;
    atmost = (ec_atmost ? true : false);

    long first;
    if (EC_succeed != EC_arg(2).is_long(&first)) return TYPE_ERROR;
    if (first) cache_domain_sizes(solver);

    try {
	cumulatives(*solver, used, starts, durations, ends, usages, limits, ec_atmost);
    }
    CatchAndReportGecodeExceptions

    return EC_succeed;
}

extern "C"
int p_g_post_circuit()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    EC_word arr = EC_arg(3);
    int size = arr.arity();
    if (size == 0) return TYPE_ERROR;

    IntVarArgs succ(size);
    int res = assign_IntVarArgs_from_ec_array(solver, size, arr, succ);
    if (res != EC_succeed) return res;

    IntConLevel cl;
    Get_Consistency_Level(4, cl);

    long first;
    if (EC_succeed != EC_arg(2).is_long(&first)) return TYPE_ERROR;
    if (first) cache_domain_sizes(solver);

    try {
	circuit(*solver, succ, cl);
    }
    CatchAndReportGecodeExceptions

    return EC_succeed;
}

extern "C"
int p_g_post_circuit_cost()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    EC_word arr = EC_arg(3);
    int size = arr.arity();
    if (size == 0) return TYPE_ERROR;

    IntVarArgs succ(size);
    int res = assign_IntVarArgs_from_ec_array(solver, size, arr, succ);
    if (res != EC_succeed) return res;

    EC_word cmarr = EC_arg(4);
    int cmsize = cmarr.arity();
    if (cmsize == 0) return TYPE_ERROR;
    if (cmsize != size*size) return RANGE_ERROR;
    
    IntArgs cm(cmsize);
    res = assign_IntArgs_from_ec_array(solver, cmsize, cmarr, cm);
    if (res != EC_succeed) return res;

    EC_functor f;
    long cidx;
    IntVar c;

    if (ArgIsVarIdx(6, cidx)) {
	if (cidx < 1 || cidx >= solver->vInt.size()) return RANGE_ERROR;
	c = solver->vInt[(int)cidx];
    } else if (EC_arg(6).is_long(&cidx) == EC_succeed) {
	c = IntVar(*solver, (int)cidx, (int)cidx);
    } else
	return TYPE_ERROR;

    IntConLevel cl;
    Get_Consistency_Level(7, cl);

    long first;
    if (EC_succeed != EC_arg(2).is_long(&first)) return TYPE_ERROR;
    if (first) cache_domain_sizes(solver);

    try {
	if (EC_arg(5).is_nil() != EC_succeed) {
	    EC_word acarr = EC_arg(5);
	    if (size != acarr.arity()) return RANGE_ERROR;
	    IntVarArgs arccosts(size);
	    res = assign_IntVarArgs_from_ec_array(solver, size, acarr, arccosts);
	    if (res != EC_succeed) return res;
	    circuit(*solver, cm, succ, arccosts, c, cl);
	} else 
	    circuit(*solver, cm, succ, c, cl);
    }
    CatchAndReportGecodeExceptions

    return EC_succeed;
}

extern "C"
int p_g_post_sqrt()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    try {
	EC_functor f;
	long xidx, yidx;
	IntVar x, y;

	if (ArgIsVarIdx(3, xidx)) {
	    if (xidx < 1 || xidx >= solver->vInt.size()) return RANGE_ERROR;
	    x = solver->vInt[(int)xidx];
	} else if (EC_arg(3).is_long(&xidx) == EC_succeed) {
	    x = IntVar(*solver, (int)xidx, (int)xidx);
	} else
	    return TYPE_ERROR;

	if (ArgIsVarIdx(4, yidx)) {
	    if (yidx < 1 || yidx >= solver->vInt.size()) return RANGE_ERROR;
	    y = solver->vInt[(int)yidx];
	} else if (EC_arg(4).is_long(&yidx) == EC_succeed) {
	    y = IntVar(*solver, (int)yidx, (int)yidx);
	} else
	    return TYPE_ERROR;

	IntConLevel cl;
	Get_Consistency_Level(5, cl);

	long first;
	if (EC_succeed != EC_arg(2).is_long(&first)) return TYPE_ERROR;
	if (first) cache_domain_sizes(solver);

	sqrt(*solver, y, x, cl);
    }
    CatchAndReportGecodeExceptions

    return EC_succeed;
}

extern "C"
int p_g_post_sq()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    try {
	EC_functor f;
	long xidx, yidx;
	IntVar x, y;

	if (ArgIsVarIdx(3, xidx)) {
	    if (xidx < 1 || xidx >= solver->vInt.size()) return RANGE_ERROR;
	    x = solver->vInt[(int)xidx];
	} else if (EC_arg(3).is_long(&xidx) == EC_succeed) {
	    x = IntVar(*solver, (int)xidx, (int)xidx);
	} else
	    return TYPE_ERROR;

	if (ArgIsVarIdx(4, yidx)) {
	    if (yidx < 1 || yidx >= solver->vInt.size()) return RANGE_ERROR;
	    y = solver->vInt[(int)yidx];
	} else if (EC_arg(4).is_long(&yidx) == EC_succeed) {
	    y = IntVar(*solver, (int)yidx, (int)yidx);
	} else
	    return TYPE_ERROR;

	IntConLevel cl;
	Get_Consistency_Level(5, cl);

	long first;
	if (EC_succeed != EC_arg(2).is_long(&first)) return TYPE_ERROR;
	if (first) cache_domain_sizes(solver);

	sqr(*solver, y, x, cl);
    }
    CatchAndReportGecodeExceptions

    return EC_succeed;
}


extern "C"
int p_g_post_abs()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    try {
	EC_functor f;
	long xidx, yidx;
	IntVar x, y;

	if (ArgIsVarIdx(3, xidx)) {
	    if (xidx < 1 || xidx >= solver->vInt.size()) return RANGE_ERROR;
	    x = solver->vInt[(int)xidx];
	} else if (EC_arg(3).is_long(&xidx) == EC_succeed) {
	    x = IntVar(*solver, (int)xidx, (int)xidx);
	} else
	    return TYPE_ERROR;

	if (ArgIsVarIdx(4, yidx)) {
	    if (yidx < 1 || yidx >= solver->vInt.size()) return RANGE_ERROR;
	    y = solver->vInt[(int)yidx];
	} else if (EC_arg(4).is_long(&yidx) == EC_succeed) {
	    y = IntVar(*solver, (int)yidx, (int)yidx);
	} else
	    return TYPE_ERROR;

	long first;
	if (EC_succeed != EC_arg(2).is_long(&first)) return TYPE_ERROR;
	if (first) cache_domain_sizes(solver);

	abs(*solver, y, x);
    }
    CatchAndReportGecodeExceptions

    return EC_succeed;
}


extern "C"
int p_g_post_mult()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    try {
	EC_functor f;
	long xidx, yidx, zidx;
	IntVar x, y, z;

	if (ArgIsVarIdx(3, xidx)) {
	    if (xidx < 1 || xidx >= solver->vInt.size()) return RANGE_ERROR;
	    x = solver->vInt[xidx];
	} else if (EC_arg(3).is_long(&xidx) == EC_succeed) {
	    x = IntVar(*solver, (int)xidx, (int)xidx);
	} else
	    return TYPE_ERROR;

	if (ArgIsVarIdx(4, yidx)) {
	    if (yidx < 1 || yidx >= solver->vInt.size()) return RANGE_ERROR;
	    y = solver->vInt[yidx];
	} else if (EC_arg(4).is_long(&yidx) == EC_succeed) {
	    y = IntVar(*solver, (int)yidx, (int)yidx);
	} else
	    return TYPE_ERROR;

	if (ArgIsVarIdx(5, zidx)) {
	    if (zidx < 1 || zidx >= solver->vInt.size()) return RANGE_ERROR;
	    z = solver->vInt[zidx];
	} else if (EC_arg(5).is_long(&zidx) == EC_succeed) {
	    z = IntVar(*solver, (int)zidx, (int)zidx);
	} else
	    return TYPE_ERROR;

	IntConLevel cl;
	Get_Consistency_Level(6, cl);

	long first;
	if (EC_succeed != EC_arg(2).is_long(&first)) return TYPE_ERROR;
	if (first) cache_domain_sizes(solver);

	mult(*solver, y, z, x, cl);
    }
    CatchAndReportGecodeExceptions

    return EC_succeed;
}

extern "C"
int p_g_post_div()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    try {
	EC_functor f;
	long xidx, yidx, zidx;
	IntVar x, y, z;

	if (ArgIsVarIdx(3, xidx)) {
	    if (xidx < 1 || xidx >= solver->vInt.size()) return RANGE_ERROR;
	    x = solver->vInt[(int)xidx];
	} else if (EC_arg(3).is_long(&xidx) == EC_succeed) {
	    x = IntVar(*solver, (int)xidx, (int)xidx);
	} else
	    return TYPE_ERROR;

	if (ArgIsVarIdx(4, yidx)) {
	    if (yidx < 1 || yidx >= solver->vInt.size()) return RANGE_ERROR;
	    y = solver->vInt[(int)yidx];
	} else if (EC_arg(4).is_long(&yidx) == EC_succeed) {
	    y = IntVar(*solver, (int)yidx, (int)yidx);
	} else
	    return TYPE_ERROR;

	if (ArgIsVarIdx(5, zidx)) {
	    if (zidx < 1 || zidx >= solver->vInt.size()) return RANGE_ERROR;
	    z = solver->vInt[(int)zidx];
	} else if (EC_arg(5).is_long(&zidx) == EC_succeed) {
	    z = IntVar(*solver, (int)zidx, (int)zidx);
	} else
	    return TYPE_ERROR;

	long first;
	if (EC_succeed != EC_arg(2).is_long(&first)) return TYPE_ERROR;
	if (first) cache_domain_sizes(solver);

	div(*solver, y, z, x);
    }
    CatchAndReportGecodeExceptions

    return EC_succeed;
}

extern "C"
int p_g_post_mod()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    try {
	EC_functor f;
	long xidx, yidx, zidx;
	IntVar x, y, z;

	if (ArgIsVarIdx(3, xidx)) {
	    if (xidx < 1 || xidx >= solver->vInt.size()) return RANGE_ERROR;
	    x = solver->vInt[(int)xidx];
	} else if (EC_arg(3).is_long(&xidx) == EC_succeed) {
	    x = IntVar(*solver, (int)xidx, (int)xidx);
	} else
	    return TYPE_ERROR;

	if (ArgIsVarIdx(4, yidx)) {
	    if (yidx < 1 || yidx >= solver->vInt.size()) return RANGE_ERROR;
	    y = solver->vInt[(int)yidx];
	} else if (EC_arg(4).is_long(&yidx) == EC_succeed) {
	    y = IntVar(*solver, (int)yidx, (int)yidx);
	} else
	    return TYPE_ERROR;

	if (ArgIsVarIdx(5, zidx)) {
	    if (zidx < 1 || zidx >= solver->vInt.size()) return RANGE_ERROR;
	    z = solver->vInt[(int)zidx];
	} else if (EC_arg(5).is_long(&zidx) == EC_succeed) {
	    z = IntVar(*solver, (int)zidx, (int)zidx);
	} else
	    return TYPE_ERROR;

	long first;
	if (EC_succeed != EC_arg(2).is_long(&first)) return TYPE_ERROR;
	if (first) cache_domain_sizes(solver);

	mod(*solver, y, z, x);
    }
    CatchAndReportGecodeExceptions

    return EC_succeed;
}

extern "C"
int p_g_post_divmod()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    try {
	EC_functor f;
	long xidx, yidx, qidx, midx;
	IntVar x, y, q, m;

	if (ArgIsVarIdx(6, midx)) {
	    if (midx < 1 || midx >= solver->vInt.size()) return RANGE_ERROR;
	    m = solver->vInt[(int)midx];
	} else if (EC_arg(6).is_long(&midx) == EC_succeed) {
	    m = IntVar(*solver, (int)midx, (int)midx);
	} else
	    return TYPE_ERROR;

	if (ArgIsVarIdx(4, yidx)) {
	    if (yidx < 1 || yidx >= solver->vInt.size()) return RANGE_ERROR;
	    y = solver->vInt[(int)yidx];
	} else if (EC_arg(4).is_long(&yidx) == EC_succeed) {
	    y = IntVar(*solver, (int)yidx, (int)yidx);
	} else
	    return TYPE_ERROR;

	if (ArgIsVarIdx(3, xidx)) {
	    if (xidx < 1 || xidx >= solver->vInt.size()) return RANGE_ERROR;
	    x = solver->vInt[(int)xidx];
	} else if (EC_arg(3).is_long(&xidx) == EC_succeed) {
	    x = IntVar(*solver, (int)xidx, (int)xidx);
	} else
	    return TYPE_ERROR;

	if (ArgIsVarIdx(5, qidx)) {
	    if (qidx < 1 || qidx >= solver->vInt.size()) return RANGE_ERROR;
	    q = solver->vInt[(int)qidx];
	} else if (EC_arg(5).is_long(&qidx) == EC_succeed) {
	    q = IntVar(*solver, (int)qidx, (int)qidx);
	} else
	    return TYPE_ERROR;

	long first;
	if (EC_succeed != EC_arg(2).is_long(&first)) return TYPE_ERROR;
	if (first) cache_domain_sizes(solver);

	divmod(*solver, x, y, q, m);
    }
    CatchAndReportGecodeExceptions

    return EC_succeed;
}

extern "C"
int p_g_post_max2()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    try {
	EC_functor f;
	long xidx, yidx, zidx;
	IntVar x, y, z;

	if (ArgIsVarIdx(3, xidx)) {
	    if (xidx < 1 || xidx >= solver->vInt.size()) return RANGE_ERROR;
	    x = solver->vInt[(int)xidx];
	} else if (EC_arg(3).is_long(&xidx) == EC_succeed) {
	    x = IntVar(*solver, (int)xidx, (int)xidx);
	} else
	    return TYPE_ERROR;

	if (ArgIsVarIdx(4, yidx)) {
	    if (yidx < 1 || yidx >= solver->vInt.size()) return RANGE_ERROR;
	    y = solver->vInt[(int)yidx];
	} else if (EC_arg(4).is_long(&yidx) == EC_succeed) {
	    y = IntVar(*solver, (int)yidx, (int)yidx);
	} else
	    return TYPE_ERROR;

	if (ArgIsVarIdx(5, zidx)) {
	    if (zidx < 1 || zidx >= solver->vInt.size()) return RANGE_ERROR;
	    z = solver->vInt[(int)zidx];
	} else if (EC_arg(5).is_long(&zidx) == EC_succeed) {
	    z = IntVar(*solver, (int)zidx, (int)zidx);
	} else
	    return TYPE_ERROR;

	IntConLevel cl;
	Get_Consistency_Level(6, cl);

	long first;
	if (EC_succeed != EC_arg(2).is_long(&first)) return TYPE_ERROR;
	if (first) cache_domain_sizes(solver);

	max(*solver, y, z, x, cl);
    }
    CatchAndReportGecodeExceptions

    return EC_succeed;
}

extern "C"
int p_g_post_min2()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    try {
	EC_functor f;
	long xidx, yidx, zidx;
	IntVar x, y, z;

	if (ArgIsVarIdx(3, xidx)) {
	    if (xidx < 1 || xidx >= solver->vInt.size()) return RANGE_ERROR;
	    x = solver->vInt[(int)xidx];
	} else if (EC_arg(3).is_long(&xidx) == EC_succeed) {
	    x = IntVar(*solver, (int)xidx, (int)xidx);
	} else
	    return TYPE_ERROR;

	if (ArgIsVarIdx(4, yidx)) {
	    if (yidx < 1 || yidx >= solver->vInt.size()) return RANGE_ERROR;
	    y = solver->vInt[(int)yidx];
	} else if (EC_arg(4).is_long(&yidx) == EC_succeed) {
	    y = IntVar(*solver, (int)yidx, (int)yidx);
	} else
	    return TYPE_ERROR;

	if (ArgIsVarIdx(5, zidx)) {
	    if (zidx < 1 || zidx >= solver->vInt.size()) return RANGE_ERROR;
	    z = solver->vInt[(int)zidx];
	} else if (EC_arg(5).is_long(&zidx) == EC_succeed) {
	    z = IntVar(*solver, (int)zidx, (int)zidx);
	} else
	    return TYPE_ERROR;

	IntConLevel cl;
	Get_Consistency_Level(6, cl);

	long first;
	if (EC_succeed != EC_arg(2).is_long(&first)) return TYPE_ERROR;
	if (first) cache_domain_sizes(solver);

	min(*solver, y, z, x, cl);
    }
    CatchAndReportGecodeExceptions

    return EC_succeed;
}

extern "C"
int p_g_post_maxlist()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    try {
	EC_functor f;
	long xidx;
	IntVar x;

	if (ArgIsVarIdx(3, xidx)) {
	    if (xidx < 1 || xidx >= solver->vInt.size()) return RANGE_ERROR;
	    x = solver->vInt[(int)xidx];
	} else if (EC_arg(3).is_long(&xidx) == EC_succeed) {
	    x = IntVar(*solver, (int)xidx, (int)xidx);
	} else
	    return TYPE_ERROR;

	EC_word varr = EC_arg(4);
	int size = varr.arity();
	if (size == 0) return TYPE_ERROR;

	IntVarArgs vars(size);
	int res = assign_IntVarArgs_from_ec_array(solver, size, varr, vars);
	if (res != EC_succeed) return res;

	IntConLevel cl;
	Get_Consistency_Level(5, cl);

	long first;
	if (EC_succeed != EC_arg(2).is_long(&first)) return TYPE_ERROR;
	if (first) cache_domain_sizes(solver);

	max(*solver, vars, x, cl);
    }
    CatchAndReportGecodeExceptions

    return EC_succeed;
}

extern "C"
int p_g_post_minlist()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    try {
	EC_functor f;
	long xidx;
	IntVar x;

	if (ArgIsVarIdx(3, xidx)) {
	    if (xidx < 1 || xidx >= solver->vInt.size()) return RANGE_ERROR;
	    x = solver->vInt[(int)xidx];
	} else if (EC_arg(3).is_long(&xidx) == EC_succeed) {
	    x = IntVar(*solver, (int)xidx, (int)xidx);
	} else
	    return TYPE_ERROR;

	EC_word varr = EC_arg(4);
	int size = varr.arity();
	if (size == 0) return TYPE_ERROR;

	IntVarArgs vars(size);
	int res = assign_IntVarArgs_from_ec_array(solver, size, varr, vars);
	if (res != EC_succeed) return res;

	IntConLevel cl;
	Get_Consistency_Level(5, cl);

	long first;
	if (EC_succeed != EC_arg(2).is_long(&first)) return TYPE_ERROR;
	if (first) cache_domain_sizes(solver);

	min(*solver, vars, x, cl);
    }
    CatchAndReportGecodeExceptions

    return EC_succeed;
}

extern "C"
int p_g_post_boolchannel()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    try {
	EC_functor f;
	long xidx;
	IntVar x;

	if (ArgIsVarIdx(3, xidx)) {
	    if (xidx < 1 || xidx >= solver->vInt.size()) return RANGE_ERROR;
	    x = solver->vInt[(int)xidx];
	} else if (EC_arg(3).is_long(&xidx) == EC_succeed) {
	    x = IntVar(*solver, (int)xidx, (int)xidx);
	} else
	    return TYPE_ERROR;

	EC_word barr = EC_arg(4);
	int size = barr.arity();
	if (size == 0) return TYPE_ERROR;

	BoolVarArgs vars(size);
	int res = assign_BoolVarArgs_from_ec_array(solver, size, barr, vars);
	if (res != EC_succeed) return res;

	long min;
	if (EC_succeed != EC_arg(5).is_long(&min)) return TYPE_ERROR;

	IntConLevel cl;
	Get_Consistency_Level(5, cl);

	long first;
	if (EC_succeed != EC_arg(2).is_long(&first)) return TYPE_ERROR;
	if (first) cache_domain_sizes(solver);

	channel(*solver, vars, x, min, cl);
    }
    CatchAndReportGecodeExceptions

    return EC_succeed;
}

extern "C"
int p_g_post_inverse()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    EC_word arr1 = EC_arg(3);
    int size = arr1.arity();
    if (size == 0) return TYPE_ERROR;

    EC_word arr2 = EC_arg(4);
    if (size != arr2.arity()) return TYPE_ERROR;

    IntVarArgs vars1(size);
    int res = assign_IntVarArgs_from_ec_array(solver, size, arr1, vars1);
    if (res != EC_succeed) return res;

    IntVarArgs vars2(size);
    res = assign_IntVarArgs_from_ec_array(solver, size, arr2, vars2);
    if (res != EC_succeed) return res;

    IntConLevel cl;
    Get_Consistency_Level(5, cl);

    long first;
    if (EC_succeed != EC_arg(2).is_long(&first)) return TYPE_ERROR;
    if (first) cache_domain_sizes(solver);

    try {
	channel(*solver, vars1, vars2, cl);
    }
    CatchAndReportGecodeExceptions

    return EC_succeed;
}

extern "C"
int p_g_post_inverse_offset()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    long off1;
    if (EC_succeed != EC_arg(4).is_long(&off1)) return TYPE_ERROR;

    long off2;
    if (EC_succeed != EC_arg(6).is_long(&off2)) return TYPE_ERROR;

    EC_word arr1 = EC_arg(3);
    int size = arr1.arity();
    if (size == 0) return TYPE_ERROR;

    EC_word arr2 = EC_arg(5);
    if (size != arr2.arity()) return TYPE_ERROR;

    IntVarArgs vars1(size);
    int res = assign_IntVarArgs_from_ec_array(solver, size, arr1, vars1);
    if (res != EC_succeed) return res;

    IntVarArgs vars2(size);
    res = assign_IntVarArgs_from_ec_array(solver, size, arr2, vars2);
    if (res != EC_succeed) return res;

    IntConLevel cl;
    Get_Consistency_Level(7, cl);

    long first;
    if (EC_succeed != EC_arg(2).is_long(&first)) return TYPE_ERROR;
    if (first) cache_domain_sizes(solver);

    try {
	channel(*solver, vars1, off1, vars2, off2, cl);
    }
    CatchAndReportGecodeExceptions

    return EC_succeed;
}

#define GFDSTATSIZE    5
#define GFDCONTROLSIZE 2

extern "C"
int p_g_setup_search()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;


    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    try {
	EC_atom atm;

	bool do_tiebreak = false;
	IntVarBranch varselect, tiebreakselect;

	if (EC_arg(6).is_atom(&atm) != EC_succeed) return TYPE_ERROR;
	if (strcmp(atm.name(), "none") != 0) {
	    do_tiebreak = true;
	    if (strcmp(atm.name(), "input_order") == 0) 
		tiebreakselect = INT_VAR_NONE;
	    else if (strcmp(atm.name(), "first_fail") == 0) 
		tiebreakselect = INT_VAR_SIZE_MIN;
	    else if (strcmp(atm.name(), "anti_first_fail") == 0) 
		tiebreakselect = INT_VAR_SIZE_MAX;
	    else if (strcmp(atm.name(), "occurrence") == 0) 
		tiebreakselect = INT_VAR_DEGREE_MAX;
	    else if (strcmp(atm.name(), "anti_occurrence") == 0) 
		tiebreakselect = INT_VAR_DEGREE_MIN;
	    else if (strcmp(atm.name(), "largest") == 0) 
		tiebreakselect = INT_VAR_MAX_MAX;
	    else if (strcmp(atm.name(), "smallest") == 0) 
		tiebreakselect = INT_VAR_MIN_MIN;
	    else if (strcmp(atm.name(), "most_constrained_per_value") == 0) 
		tiebreakselect = INT_VAR_SIZE_DEGREE_MAX;
	    else if (strcmp(atm.name(), "least_constrained_per_value") == 0) 
		tiebreakselect = INT_VAR_SIZE_DEGREE_MIN;
	    else if (strcmp(atm.name(), "max_regret") == 0) 
		tiebreakselect = INT_VAR_REGRET_MIN_MAX;
	    else if (strcmp(atm.name(), "max_regret_lwb") == 0) 
		tiebreakselect = INT_VAR_REGRET_MIN_MAX;
	    else if (strcmp(atm.name(), "min_regret_lwb") == 0) 
		tiebreakselect = INT_VAR_REGRET_MIN_MIN;
	    else if (strcmp(atm.name(), "max_regret_upb") == 0) 
		tiebreakselect = INT_VAR_REGRET_MAX_MAX;
	    else if (strcmp(atm.name(), "min_regret_upb") == 0) 
		tiebreakselect = INT_VAR_REGRET_MAX_MIN;
	    else if (strcmp(atm.name(), "random") == 0) 
		tiebreakselect = INT_VAR_RND;
	    else return RANGE_ERROR;
	}

	if (EC_arg(3).is_atom(&atm) != EC_succeed) return TYPE_ERROR;
	if (strcmp(atm.name(), "input_order") == 0) varselect = INT_VAR_NONE;
	else if (strcmp(atm.name(), "first_fail") == 0) varselect = INT_VAR_SIZE_MIN;
	else if (strcmp(atm.name(), "anti_first_fail") == 0) varselect = INT_VAR_SIZE_MAX;
	else if (strcmp(atm.name(), "occurrence") == 0) varselect = INT_VAR_DEGREE_MAX;
	else if (strcmp(atm.name(), "anti_occurrence") == 0) varselect = INT_VAR_DEGREE_MIN;
	else if (strcmp(atm.name(), "largest") == 0) varselect = INT_VAR_MAX_MAX;
	else if (strcmp(atm.name(), "smallest") == 0) varselect = INT_VAR_MIN_MIN;
	else if (strcmp(atm.name(), "largest_lwb") == 0) varselect = INT_VAR_MAX_MIN;
	else if (strcmp(atm.name(), "smallest_upb") == 0) varselect = INT_VAR_MIN_MAX;
	else if (strcmp(atm.name(), "most_constrained") == 0) {
	    varselect = INT_VAR_SIZE_MIN;
	    do_tiebreak = true;
	    tiebreakselect = INT_VAR_DEGREE_MAX;
	} else if (strcmp(atm.name(), "most_constrained_per_value") == 0) varselect = INT_VAR_SIZE_DEGREE_MAX;
	else if (strcmp(atm.name(), "least_constrained_per_value") == 0) varselect = INT_VAR_SIZE_DEGREE_MIN;
	else if (strcmp(atm.name(), "max_regret") == 0) varselect = INT_VAR_REGRET_MIN_MAX;
	else if (strcmp(atm.name(), "max_regret_lwb") == 0) varselect = INT_VAR_REGRET_MIN_MAX;
	else if (strcmp(atm.name(), "min_regret_lwb") == 0) varselect = INT_VAR_REGRET_MIN_MIN;
	else if (strcmp(atm.name(), "max_regret_upb") == 0) varselect = INT_VAR_REGRET_MAX_MAX;
	else if (strcmp(atm.name(), "min_regret_upb") == 0) varselect = INT_VAR_REGRET_MAX_MIN;
	else if (strcmp(atm.name(), "random") == 0) varselect = INT_VAR_RND;
	else if (strcmp(atm.name(), "max_weighted_degree") == 0) varselect = INT_VAR_AFC_MAX; 
	else if (strcmp(atm.name(), "min_weighted_degree") == 0) varselect = INT_VAR_AFC_MIN; 
	else if (strcmp(atm.name(), "max_weighted_degree_per_value") == 0) varselect = INT_VAR_SIZE_AFC_MAX; 
	else if (strcmp(atm.name(), "min_weighted_degree_per_value") == 0) varselect = INT_VAR_SIZE_AFC_MIN; 
	else return RANGE_ERROR;

	IntValBranch valchoice;
	if (EC_arg(4).is_atom(&atm) != EC_succeed) return TYPE_ERROR;
	if (strcmp(atm.name(), "indomain_min") == 0) valchoice = INT_VAL_MIN;
	else if (strcmp(atm.name(), "indomain_max") == 0) valchoice = INT_VAL_MAX;
	else if (strcmp(atm.name(), "indomain_median") == 0) valchoice = INT_VAL_MED;
	else if (strcmp(atm.name(), "indomain_random") == 0) valchoice = INT_VAL_RND;
	else if (strcmp(atm.name(), "indomain_split") == 0) valchoice = INT_VAL_SPLIT_MIN;
	else if (strcmp(atm.name(), "indomain_reverse_split") == 0) valchoice = INT_VAL_SPLIT_MAX;
	else if (strcmp(atm.name(), "indomain") == 0) valchoice = INT_VALUES_MIN;
	else if (strcmp(atm.name(), "indomain_from_max") == 0) valchoice = INT_VALUES_MAX;
	else if (strcmp(atm.name(), "indomain_interval") == 0) valchoice = INT_VAL_RANGE_MIN;
	else if (strcmp(atm.name(), "indomain_interval_min") == 0) valchoice = INT_VAL_RANGE_MIN;
	else if (strcmp(atm.name(), "indomain_interval_max") == 0) valchoice = INT_VAL_RANGE_MAX;
	else return RANGE_ERROR;

	EC_functor f;
	long l = 0;
	SearchMethod method;

	switch (EC_arg(5).arity()) {
	    case 0:
		if (EC_arg(5).is_atom(&atm) == EC_succeed) {
		    if (strcmp(atm.name(), "complete") == 0) {
			method = METHOD_COMPLETE;
		    } else return RANGE_ERROR;
		} else return RANGE_ERROR;
		break;
	    case 1:
		EC_arg(5).functor(&f); // must be compound -- arity 1
		if (strcmp(f.name(),"lds") == 0) {
		    if (EC_argument(EC_arg(5),1).is_long(&l) != EC_succeed)
			return TYPE_ERROR;
		    method = METHOD_LDS;
		} else if (strcmp(f.name(),"bb_min") == 0) {
		    if (EC_argument(EC_arg(5),1).is_long(&l) != EC_succeed)
			return TYPE_ERROR;
		    method = METHOD_BAB;
		} else if (strcmp(f.name(),"restart_min") == 0) {
		    if (EC_argument(EC_arg(5),1).is_long(&l) != EC_succeed)
			return TYPE_ERROR;
		    method = METHOD_RESTART;
		} else return RANGE_ERROR;
		break;
	    default:
		return RANGE_ERROR; 
	}

	EC_word varr = EC_arg(2);
	int res, size = varr.arity();
	if (size == 0) return TYPE_ERROR;
	IntVarArgs vars(size);
	res = assign_IntVarArgs_from_ec_array(solver, size, varr, vars);
	if (res != EC_succeed) return res;

	long timeout;
	if (EC_succeed != EC_arg(7).is_long(&timeout)) return TYPE_ERROR;

	// these positions must correspond to gfd.ecl's struct:
	// struct(gfd_stats(prop,fail,node,depth,mem)).
	long fail_lim = 0, node_lim = 0, mem_lim = 0;
	if ((EC_arg(8).functor(&f) == EC_succeed) && (f.arity() == GFDSTATSIZE)
	    && (strcmp(f.name(), "gfd_stats") == 0)) {
	    if (EC_argument(EC_arg(8),2).is_long(&fail_lim) != EC_succeed)
		fail_lim = 0;
	    if (EC_argument(EC_arg(8),3).is_long(&node_lim) != EC_succeed)
		node_lim = 0;
	    if (EC_argument(EC_arg(8),5).is_long(&mem_lim) != EC_succeed)
		mem_lim = 0;
	}

	long adaptived = 0, commitd = 0;
	if ((EC_arg(9).functor(&f) == EC_succeed) && (f.arity() == GFDCONTROLSIZE)
	    && (strcmp(f.name(), "gfd_control") == 0)) {
	    if (EC_argument(EC_arg(9),1).is_long(&commitd) != EC_succeed)
		commitd = 0;
	    if (EC_argument(EC_arg(9),2).is_long(&adaptived) != EC_succeed)
		adaptived = 0;
	}

	solver->clear_snapshot(); // make sure we do cache the current values!
	cache_domain_sizes(solver);
	if (!do_tiebreak) {
	    branch(*solver, vars, varselect, valchoice);
	} else {
	    branch(*solver, vars, tiebreak(varselect, tiebreakselect), valchoice);
	}

	Search::Options o;

	Cutoff* cutoffp;
	if (timeout > 0 || fail_lim > 0 || node_lim > 0 || mem_lim > 0) {
	    cutoffp = new Cutoff((unsigned)node_lim,(unsigned)fail_lim,
				 (unsigned)timeout,(size_t)mem_lim);
	    o.stop = cutoffp;
	} else
	    cutoffp = NULL;

	if (adaptived > 0) o.a_d = adaptived;
	if (commitd > 0) o.c_d = commitd;

	GecodeSearch* searchp = new GecodeSearch(solver, o, (unsigned) l, cutoffp, method);

	return unify(EC_arg(10), handle(&gfdsearch_method, searchp));
    }
    CatchAndReportGecodeExceptions
}


extern "C"
int p_g_do_search()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;

    EC_functor f;
    EC_word w;

    if (EC_arg(1).functor(&f) != EC_succeed) return TYPE_ERROR;
    if (strcmp(f.name(), "gfd_space") != 0) return TYPE_ERROR;
    EC_arg(1).arg(SPACE_HANDLE_POS, w);
    if (w.is_handle(&gfd_method, (void**)&solverp) != EC_succeed) 
	return TYPE_ERROR;
    solver = *solverp;
    if (solver != NULL) delete solver;  

    GecodeSpace** presearchp;
    GecodeSpace* presearch;

    if (EC_succeed != get_handle_from_arg(3, &gfd_method, (void**)&presearchp))
	return TYPE_ERROR;
    presearch = *presearchp;

    unsigned long nprop, nfail, nnode, depth;
    size_t mem;

    GecodeSearch* searchp;
    if (EC_succeed != get_handle_from_arg(2, &gfdsearch_method, (void**)&searchp))
	return TYPE_ERROR;

    try {
	long status;

	if (searchp->stopp != NULL) searchp->stopp->reset();
	switch (searchp->method) {
	    case METHOD_COMPLETE: 
	    case METHOD_LDS: 

		*solverp = searchp->next();
		status = (*solverp == NULL ? 0 : 1); 
		break;

	    case METHOD_BAB:
	    case METHOD_RESTART: {
		GecodeSpace* last_sol = NULL;
		bool has_aborted = false;
		do {
		    solver = searchp->next();
		    if (solver != NULL) {
			if (solver->vCost.assigned()) {
			    p_fprintf(log_output_,"Found a solution with cost %d\n", solver->vCost.val());
			} else {
			    p_fprintf(log_output_, "Cost variable not instantiated.\n");
			    has_aborted = true;
			    break;
			}
			ec_flush(log_output_);
			last_sol = solver;
		    } else
			*solverp = last_sol;
		} while (solver != NULL);
		status =  (has_aborted ? 2/*1<<2*/ : (last_sol == NULL ? 0 : 1)); 
		break;
	    }

	    default: 
		return RANGE_ERROR;
		break;

	}

	Search::Statistics stat = searchp->statistics();
	nprop = stat.propagate;
	nfail = stat.fail;
	nnode = stat.node;
	depth = stat.depth;
	mem = stat.memory;

	// these must correspond to gfd_stats struct in gfd.ecl
	EC_functor sf("gfd_stats", GFDSTATSIZE);
	EC_word stats = term(sf, (long)nprop, (long)nfail, (long)nnode,
			     (long)depth, (long)mem);
	if (unify(EC_arg(6), stats) != EC_succeed) return EC_fail;

	if (searchp->stopped()) {
	    status |=  searchp->stopp->reason();
	}

	if (unify(EC_arg(7), EC_word(status)) != EC_succeed) return EC_fail;

	if (*solverp != NULL) {// there is a solution
	    solver = *solverp;
	    // unlike normal creation of a cloned space, here it is not done immediately
	    // before an event, so just trail the undo function unconditionally
	    ec_trail_undo(_g_delete_space, ec_arg(1).val.ptr, NULL, NULL, 0, TRAILED_WORD32);

	    int snapshotsize = presearch->dom_snapshot.size();
	    int dsize;
	    EC_word tail = nil(),chgtail = nil();

	    CheckAndMakeChanged(presearch, tail, chgtail);

	    if (unify(EC_arg(4), tail) != EC_succeed) {
		return EC_fail;
	    } else {
		return unify(EC_arg(5), chgtail);
	    }
	} else {
	    return unify(EC_arg(4), nil());
	}
    }
    CatchAndReportGecodeExceptions
}
    
extern "C"
int p_g_get_gfd_maxint()
{
    return unify(EC_arg(1), EC_word(Int::Limits::max));
}

extern "C"
int p_g_get_gfd_minint()
{
    return unify(EC_arg(1), EC_word(Int::Limits::min));
}


extern "C"
int p_g_get_var_domain_handle()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    long xidx;
    IntVar x;
    EC_functor f;

    if (ArgIsVarIdx(2, xidx)) {
	if (xidx < 1 || xidx >= solver->vInt.size()) return RANGE_ERROR;
	x = solver->vInt[(int)xidx];
    } else if (EC_arg(3).is_long(&xidx) == EC_succeed) {
	x = IntVar(*solver, (int)xidx, (int)xidx);
    } else
	return TYPE_ERROR;
    
    IntVarRanges r(x);

    IntSet* setp = new IntSet(r);

    return unify(EC_arg(3), handle(&domain_method, setp));

}

extern "C"
int p_g_add_newvars_dom_union()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;
    long newsize;
    int oldsize;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;
    if (EC_succeed != EC_arg(2).is_long(&newsize)) return(TYPE_ERROR);
    oldsize = solver->vInt.size();

    long xidx;
    IntVar x;
    EC_functor f;

    if (ArgIsVarIdx(3, xidx)) {
	if (xidx < 1 || xidx >= solver->vInt.size()) return RANGE_ERROR;
	x = solver->vInt[(int)xidx];
    } else if (EC_arg(3).is_long(&xidx) == EC_succeed) {
	x = IntVar(*solver, (int)xidx, (int)xidx);
    } else
	return TYPE_ERROR;
    
    long yidx;
    IntVar y;

    if (ArgIsVarIdx(4, yidx)) {
	if (yidx < 1 || yidx >= solver->vInt.size()) return RANGE_ERROR;
	y = solver->vInt[(int)yidx];
    } else if (EC_arg(4).is_long(&yidx) == EC_succeed) {
	y = IntVar(*solver, (int)yidx, (int)yidx);
    } else
	return TYPE_ERROR;

    try {
	IntVarRanges rx(x);
	IntVarRanges ry(y);

	Iter::Ranges::Union<IntVarRanges, IntVarRanges> unioniter(rx, ry);

	IntSet  newdom(unioniter);

	//	solver->vInt.resize(*solver, (int)++newsize); // ++ to skip over idx 0
	for (int i=oldsize; i <= (int)newsize; i++)
	  solver->vInt << IntVar(*solver, newdom);

	return EC_succeed;
    }
    CatchAndReportGecodeExceptions
}

extern "C"
int p_g_add_newvar_copy()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;
    long newsize;
    int oldsize;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;
    if (EC_succeed != EC_arg(2).is_long(&newsize)) return(TYPE_ERROR);
    oldsize = solver->vInt.size();

    long oldidx;

    if (EC_succeed != EC_arg(3).is_long(&oldidx)) return TYPE_ERROR;

    try {
	IntVar y(*solver, Int::Limits::min, Int::Limits::max);
	Int::IntView yv(y);
	IntVarRanges iter(solver->vInt[(int)oldidx]);
	(void) yv.narrow_r(*solver, iter, false);

	//	solver->vInt.resize(*solver, (int)++newsize); // ++ to skip over idx 0
	solver->vInt << y;

	return EC_succeed;
    }
    CatchAndReportGecodeExceptions
}

extern "C"
int p_g_gecode_version()
{
  return unify(EC_arg(1), EC_word(GECODE_VERSION));
}

