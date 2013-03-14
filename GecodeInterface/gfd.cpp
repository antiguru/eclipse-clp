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

// Arg is an integer domain variable with index Idx (use EC_functor f)
#define ArgIsVarIdx(Arg, Idx)			\
    (EC_arg(Arg).functor(&f) == EC_succeed &&	\
	strcmp(f.name(),"_ivar") ==0 && \
	EC_argument(EC_arg(Arg),1).is_long(&Idx) == EC_succeed)

// Arg is an integer domain variable with associated boolean with index BIdx (use EC_functor f)
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

// get the interger consistency level cl from argument N 
#define Get_Consistency_Level(N, cl) {		\
    EC_atom atm;   \
    if (EC_arg(N).is_atom(&atm) != EC_succeed) return TYPE_ERROR; \
    dident ldid = atm.d;					  \
    if (ldid == d_default) cl = ICL_DEF;                          \
    else if (ldid == d_gac) cl = ICL_DOM;			  \
    else if (ldid == d_bc) cl = ICL_BND;			  \
    else if (ldid == d_vc) cl = ICL_VAL;			  \
    else return RANGE_ERROR;				  \
}

// get IntRelType r from argument N
#define Get_IntRelType(N, r) { \
    EC_atom ecrel; \
    if (EC_arg(N).is_atom(&ecrel) != EC_succeed) return TYPE_ERROR; \
    dident rdid = ecrel.d;				    \
    if (rdid == d_eq) r = IRT_EQ;				    \
    else if (rdid == d_geq) r = IRT_GQ;				    \
    else if (rdid == d_gt) r = IRT_GR;				    \
    else if (rdid == d_leq) r = IRT_LQ;				    \
    else if (rdid == d_lt) r = IRT_LE;				    \
    else if (rdid == d_neq) r = IRT_NQ;				    \
    else return TYPE_ERROR;				    \
}

// Assign IntVar var from argument N, var is assigned to either an existing 
// vInt[] variable if argN is a variable, or to an integer value if argN is 
// integer (use EC_functor f)
#define Assign_IntVar(N, idx, var) { \
    if (ArgIsVarIdx(N, idx)) {    \
        if (idx < 1 || idx >= solver->vInt.size()) return RANGE_ERROR; \
	var = solver->vInt[(int)idx]; \
    } else if (EC_arg(N).is_long(&idx) == EC_succeed) { \
	var = IntVar(*solver, (int)idx, (int)idx);      \
    } else \
        return TYPE_ERROR; \
}

// Assign the value of argument N to either IntVar var or int i, is_int is
// set to true if i is assigned, false if var is assigned (use EC_functor f)
#define Assign_IntVar_or_Int(N, i, var, is_int) { \
    if (ArgIsVarIdx(N, i)) { \
        if (i < 1 || i >= solver->vInt.size()) return RANGE_ERROR; \
        var = solver->vInt[(int)i]; \
        is_int = false; \
    } else if (EC_arg(N).is_long(&i) == EC_succeed) { \
        is_int = true; \
    } else \
        return TYPE_ERROR; \
}

extern "C" VisAtt stream_id log_output_, warning_output_, current_err_;

extern "C" VisAtt void ec_trail_undo(void(*f)(pword*,word*,int,int), pword*, pword*, word*, int, int);

extern "C" VisAtt int ec_flush(stream_id);

extern "C" VisAtt int p_fprintf(stream_id, const char*, ...);

#if defined(WIN32)

extern "C" VisAtt stream_id Winapi ec_stream_id(int);
# define log_output_ ec_stream_id(ec_stream_nr("log_output"))
# define current_err_ ec_stream_id(ec_stream_nr("current_err"))

#endif

static dident d_max_wdeg, d_min_wdeg, 
    d_max_wdeg_per_val, d_min_wdeg_per_val,
    d_ff, d_antiff, 
    d_occ, d_antiocc, 
    d_smallest, d_largest, d_smallest_upb, d_largest_lwb, 
    d_max_regret, d_max_regret_lwb, 
    d_min_regret_lwb, d_max_regret_upb, d_min_regret_upb,
    d_most_constrained_per_val, d_least_constrained_per_val, 
    d_most_constrained, d_input_order, d_random,
    d_default, d_gac, d_bc, d_vc,
    d_eq, d_neq, d_gt, d_lt, d_geq, d_leq,
    d_iv2, d_sum2, d_element2, d_plus2, d_minus2, 
    d_mult2, d_div2, d_rem2, d_min2, d_max2, 
    d_minus1, d_abs1, d_sqr1, d_isqrt1, d_sum1, d_max1, d_min1,
    d_eq2, d_gt2, d_geq2, d_lt2, d_leq2, d_neq2,
    d_and2, d_or2, d_xor2, d_imp2, d_equ2, d_neg1;

using namespace Gecode;


int
get_handle_from_arg(int arg, t_ext_type *method, void **obj)
{
    return EC_arg(arg).is_handle(method, obj);
}

static void _free_dfa_handle(DFA* dfap)
{
    delete dfap;
}

t_ext_type dfa_method = {
    (void (*)(t_ext_ptr)) _free_dfa_handle, /* free */
    NULL, /* copy */ 
    NULL, /* mark_dids */
    NULL, /* string_size */
    NULL, /* to_string */
    NULL, /* equal */
    NULL, /* remote_copy */
    NULL, /* get */
    NULL, /* set */
};

static void _free_tupleset_handle(TupleSet* dtset)
{
    delete dtset;
}

t_ext_type tupleset_method = {
    (void (*)(t_ext_ptr)) _free_tupleset_handle, /* free */
    NULL, /* copy */ 
    NULL, /* mark_dids */
    NULL, /* string_size */
    NULL, /* to_string */
    NULL, /* equal */
    NULL, /* remote_copy */
    NULL, /* get */
    NULL, /* set */
};

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

static void _free_intptr_handle(int* intarrp)
{
    if (intarrp != NULL) delete intarrp;
}

t_ext_type intptr_method = {
    (void (*)(t_ext_ptr)) _free_intptr_handle, /* free */
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

int assign_IntArgs_from_ec_array(int size, EC_word ecarr, IntArgs& vargs)
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

extern "C" VisAtt 
int p_g_init()
{
    d_max_wdeg = ec_did("max_weighted_degree", 0);
    d_min_wdeg = ec_did("min_weighted_degree", 0);
    d_max_wdeg_per_val = ec_did("max_weighted_degree_per_value", 0);
    d_min_wdeg_per_val = ec_did("min_weighted_degree_per_value", 0);
    d_ff = ec_did("first_fail", 0);
    d_antiff = ec_did("anti_first_fail", 0);
    d_occ = ec_did("occurrence", 0);
    d_antiocc = ec_did("anti_occurrence", 0);
    d_smallest = ec_did("smallest", 0);
    d_largest = ec_did("largest", 0);
    d_smallest_upb = ec_did("smallest_upb", 0);
    d_largest_lwb = ec_did("largest_lwb", 0);
    d_max_regret = ec_did("max_regret", 0);
    d_max_regret_lwb = ec_did("max_regret_lwb", 0);
    d_max_regret_upb = ec_did("max_regret_upb", 0);
    d_min_regret_lwb = ec_did("min_regret_lwb", 0);
    d_min_regret_upb = ec_did("min_regret_upb", 0);
    d_most_constrained_per_val = ec_did("most_constrained_per_value", 0);
    d_most_constrained = ec_did("most_constrained", 0);
    d_least_constrained_per_val = ec_did("least_constrained_per_value", 0);
    d_input_order = ec_did("input_order", 0);
    d_random = ec_did("random", 0);

    d_default = ec_did("default", 0);
    d_gac = ec_did("gfd_gac", 0);
    d_bc = ec_did("gfd_bc", 0);
    d_vc = ec_did("gfd_vc", 0);

    d_eq = ec_did("#=", 0);
    d_neq = ec_did("#\\=", 0);
    d_gt = ec_did("#>", 0);
    d_lt = ec_did("#<", 0);
    d_geq = ec_did("#>=", 0);
    d_leq = ec_did("#=<", 0);

    d_eq2 = ec_did("#=", 2);
    d_neq2 = ec_did("#\\=", 2);
    d_gt2 = ec_did("#>", 2);
    d_lt2 = ec_did("#<", 2);
    d_geq2 = ec_did("#>=", 2);
    d_leq2 = ec_did("#=<", 2);

    d_sum2 = ec_did("sum", 2);
    d_element2 = ec_did("element", 2);
    d_max2 = ec_did("max", 2);
    d_min2 = ec_did("min", 2);
    d_rem2 = ec_did("rem", 2);
    d_plus2 = ec_did("+", 2);
    d_minus2 = ec_did("-", 2);
    d_mult2 = ec_did("*", 2);
    d_div2 = ec_did("//", 2);
    d_minus1 = ec_did("-", 1);
    d_abs1 = ec_did("abs", 1);
    d_sqr1 = ec_did("sqr", 1);
    d_isqrt1 = ec_did("isqrt", 1);
    d_sum1 = ec_did("sum", 1);
    d_max1 = ec_did("max", 1);
    d_min1 = ec_did("min", 1);
    d_and2 = ec_did("and", 2);
    d_or2 = ec_did("or", 2);
    d_xor2 = ec_did("xor", 2);
    d_imp2 = ec_did("=>", 2);
    d_equ2 = ec_did("<=>", 2);
    d_neg1 = ec_did("neg", 1);

    d_iv2 = ec_did("_ivar", 2);

    GecodeSpace** solverp = new GecodeSpace*;

    *solverp = NULL;
    return unify(EC_arg(1), handle(&gfd_method, solverp));
}

extern "C" VisAtt
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

extern "C" VisAtt
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

extern "C" VisAtt
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

	// cloned -- instantiate arg(3) to []	
	return unify(EC_arg(3), nil());
    } else {
	// not cloned -- leave arg(3) uninstantiated
	return EC_succeed;
	}
}


extern "C" VisAtt
int p_g_delete()
{
    GecodeSpace** solverp;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp)) 
	return TYPE_ERROR;

    if (*solverp != NULL) delete *solverp;
    *solverp = NULL;

    return EC_succeed;
}

extern "C" VisAtt
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

extern "C" VisAtt
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

extern "C" VisAtt
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

extern "C" VisAtt
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

extern "C" VisAtt
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

extern "C" VisAtt
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

extern "C" VisAtt
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

extern "C" VisAtt
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

extern "C" VisAtt
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

extern "C" VisAtt
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

extern "C" VisAtt
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

extern "C" VisAtt
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

extern "C" VisAtt
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

extern "C" VisAtt
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

extern "C" VisAtt
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


extern "C" VisAtt
int p_g_add_newvars_as_bool()
{
    // add new vars as booleans to problem -- add new IntVars and link them 
    // them with new BoolVars
    GecodeSpace** solverp;
    GecodeSpace* solver;
    long newsize;
    int oldsize, snapshotsize;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;
    if (EC_succeed != EC_arg(2).is_long(&newsize)) return(TYPE_ERROR);
    oldsize = solver->vInt.size();
    EC_word varr = EC_arg(3);

    int varrsize = varr.arity();
    if (varrsize == 0) return TYPE_ERROR;
    // ++newsize as we don't use 0 for index
    if (varrsize != ++newsize - oldsize) return RANGE_ERROR;
 

    try {
	for (int i=oldsize,argi=1; i < (int)newsize; i++,argi++) {
	    solver->vInt << IntVar(*solver, 0, 1);
	    solver->vBool << BoolVar(*solver,0, 1);
	    int bidx = solver->vBool.size() - 1;
	    channel(*solver,  solver->vInt[i], solver->vBool[bidx]);
	    EC_word arg = EC_argument(varr, argi);
	    EC_functor f;
	    if (arg.functor(&f) == EC_succeed && 
		strcmp(f.name(), "_ivar") == 0 &&
		arg.arity() == 2) {
		int res = unify(EC_argument(arg, 2), EC_word((long)bidx));
		if (res != EC_succeed) return res;

		res = unify(EC_argument(arg,1), EC_word((long)i));
		if (res != EC_succeed) return res;

	    } else return TYPE_ERROR;
	}
    }
    CatchAndReportGecodeExceptions

    return EC_succeed;
}

extern "C" VisAtt
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

extern "C" VisAtt
int p_g_link_newbools()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    EC_word varr = EC_arg(2);
    int varrsize = varr.arity();
    if (varrsize == 0) return TYPE_ERROR;
 

    try {
	for (int i=1; i <= varrsize; i++) {

	    EC_word arg = EC_argument(varr, i);
	    EC_functor f;
	    long vidx;
	    if (arg.functor(&f) == EC_succeed && 
		strcmp(f.name(), "_ivar") == 0 &&
		arg.arity() == 2 &&
		EC_argument(arg,1).is_long(&vidx) == EC_succeed) {

		long bidx;
		if (EC_argument(arg, 2).is_long(&bidx) != EC_succeed) {
		    // not yet linked, create link to new BoolVar
		    solver->vBool << BoolVar(*solver,0,1);
		    bidx = (long) solver->vBool.size()-1;
		    channel(*solver,  solver->vInt[vidx], solver->vBool[(int)bidx]);
		    int res = unify(EC_argument(arg, 2), EC_word((long)bidx));
		    if (res != EC_succeed) return res;
		}
	    } else return TYPE_ERROR;
	}

    }
    CatchAndReportGecodeExceptions
	
    return EC_succeed;
}

extern "C" VisAtt
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

extern "C" VisAtt
int p_g_add_newvars_dom_handle()
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

    IntSet* domp;
    if (EC_succeed != get_handle_from_arg(3, &domain_method, (void**)&domp))
	return TYPE_ERROR;
    if (domp == NULL) return TYPE_ERROR;

    try {
      //	solver->vInt.resize(*solver, (int)++newsize); // ++ to skip over idx 0
	for (int i=oldsize; i <= (int)newsize; i++)
	  solver->vInt << IntVar(*solver, *domp);
    }
    CatchAndReportGecodeExceptions

    return EC_succeed;
}

#define Return_ExprListArg(e, solver, EXPR) {	\
     EC_word varr = EC_argument(e,1); \
     int size = varr.arity(); \
     if (size > 0) { \
	 IntVarArgs vars(size); \
	 if (assign_IntVarArgs_from_ec_array(solver, size, varr, vars) \
	     != EC_succeed) \
		throw Ec2gcException(); \
         return EXPR; \
     } \
}

LinExpr
ec2intexpr(EC_word e, GecodeSpace* solver)
{
    long l;
    int i;
    EC_functor f;
    LinExpr arg1(BoolVar(*solver,1,1)), arg2(BoolVar(*solver,1,1));

    if (e.functor(&f) == EC_succeed) {
	if (f.d == d_iv2 && 
	    //if (strcmp(f.name(), "_ivar") == 0 && 
	    EC_argument(e, 1).is_long(&l) == EC_succeed) {
	    i = (int)l;
	    return solver->vInt[i];
	} else {
	    switch (f.arity()) {
	    case 1: {
		if (f.d == d_sum1) {
		    Return_ExprListArg(e, solver, sum(vars));

		} else if (f.d == d_max1) {
		    Return_ExprListArg(e, solver, max(vars));

		} else if (f.d == d_min1) {
		    Return_ExprListArg(e, solver, min(vars));


		} else {
		    arg2 = ec2intexpr(EC_argument(e, 1), solver);

		    if (f.d == d_minus1) return -arg2;
		    else if (f.d == d_abs1) return abs(arg2);
		    else if (f.d == d_sqr1) return sqr(arg2);
		    else if (f.d == d_isqrt1) return sqrt(arg2);
		}
		break;
	    }
	    case 2: {
		if (f.d == d_sum2) {
		  EC_word carr = EC_argument(e,1);
		  EC_word varr = EC_argument(e,2);
		  int size = varr.arity();
		  if (size > 0) {
		    IntVarArgs vars(size);
		    if (assign_IntVarArgs_from_ec_array(solver, size, varr, vars) != EC_succeed)
		      throw Ec2gcException();
		    IntArgs cs(size);
		    if (assign_IntArgs_from_ec_array(size, carr, cs) == EC_succeed)
		      return sum(cs, vars);
		  }
		} else {
		    arg2 = ec2intexpr(EC_argument(e, 2), solver);
		    if (f.d != d_element2) {
			arg1 = ec2intexpr(EC_argument(e, 1), solver);
	
			if (f.d == d_plus2)       return (arg1 + arg2); 
			else if (f.d == d_minus2) return (arg1 - arg2); 
			else if (f.d == d_mult2)  return (arg1 * arg2); 
			else if (f.d == d_min2)   return  min(arg1, arg2); 
			else if (f.d == d_max2)   return max(arg1, arg2); 
			else if (f.d == d_rem2)   return (arg1 % arg2); 
			else if (f.d == d_div2)   return (arg1 / arg2); 

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
		break;
	    }} /* switch */
	}
    } else if (e.is_long(&l) == EC_succeed) { // Integer
        return (int)l;
        //i = (int)l;
	//	return (solver->vInt[0] + i); // vInt[0] has value 0, needed as dummy
    }

    // Unknown integer expression
    throw Ec2gcException();
}

LinRel
ec2intrel(EC_word c, GecodeSpace* solver)
{
    EC_functor f;

    if (c.functor(&f) == EC_succeed && f.arity() == 2) {
	LinExpr arg1 = ec2intexpr(EC_argument(c,1), solver);
	LinExpr arg2 = ec2intexpr(EC_argument(c,2), solver);

	if (f.d == d_eq2) return (arg1 == arg2);
	else if (f.d == d_gt2) return (arg1 > arg2);
	else if (f.d == d_geq2) return (arg1 >= arg2);
	else if (f.d == d_lt2) return (arg1 < arg2);
	else if (f.d == d_leq2) return (arg1 <= arg2);
	else if (f.d == d_neq2) return (arg1 != arg2);
	
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
	if (f.d == d_iv2) {
	    if (EC_argument(c,2).is_long(&l) != EC_succeed)
		throw Ec2gcException();
	    return solver->vBool[(int)l];
	    
	} else if (f.d == d_and2) {
	    return (ec2boolexpr(EC_argument(c,1), solver) && 
		    ec2boolexpr(EC_argument(c,2), solver)
		    );
	} else if (f.d == d_or2) {
	    return (ec2boolexpr(EC_argument(c,1), solver) || 
		    ec2boolexpr(EC_argument(c,2), solver)
		    );
	} else if (f.d == d_xor2) {
	    return (ec2boolexpr(EC_argument(c,1), solver) ^
		    ec2boolexpr(EC_argument(c,2), solver)
		    );
	} else if (f.d == d_imp2) {
	    return (ec2boolexpr(EC_argument(c,1), solver) >> 
		    ec2boolexpr(EC_argument(c,2), solver)
		    );
	} else if (f.d == d_equ2) {
	    return (ec2boolexpr(EC_argument(c,1), solver) == 
		    ec2boolexpr(EC_argument(c,2), solver)
		    );

	} else if (f.d == d_neg1) return ec2boolexpr(EC_argument(c,1), solver);

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

REG
ec2reg(EC_word e)
{
  long l;
  EC_functor f, f2;
  EC_word e2;
  REG arg1, arg2;

  if (e.functor(&f) == EC_succeed) {
    if (strcmp(f.name(), "[]") == 0) {
      int size =  f.arity();
      IntArgs alts(size);

      if (assign_IntArgs_from_ec_array(size, e, alts) != EC_succeed)
	throw Ec2gcException();
      return REG(alts);
    }

    switch (f.arity()) {
    case 1:
      if (strcmp(f.name(), "*") == 0) {
	arg1 = ec2reg(EC_argument(e, 1));
	return *arg1;
      } else
      if (strcmp(f.name(), "+") == 0) {
	arg1 = ec2reg(EC_argument(e, 1));
	return +arg1;
      } else
	throw Ec2gcException();
      break;
    case 2:
      if (strcmp(f.name(), "+") == 0) {
	arg1 = ec2reg(EC_argument(e, 1));
	arg2 = ec2reg(EC_argument(e, 2));
	return arg1 + arg2;
      } else
      if (strcmp(f.name(), "|") == 0) {
	arg1 = ec2reg(EC_argument(e, 1));
	arg2 = ec2reg(EC_argument(e, 2));
	return arg1 | arg2;
      } else
      if (strcmp(f.name(), ",") == 0) {
	arg1 = ec2reg(EC_argument(e, 1));
	e2 = EC_argument(e,2);

	if (e2.functor(&f2) == EC_succeed) {
	  long n, m;

	  switch (f2.arity()) {
	  case 1:
	    // assume f2 is {}/1: {n}
	    if (EC_argument(e2,1).is_long(&n) == EC_succeed) {
	      return arg1((unsigned int)n);
	    } else
	      throw Ec2gcException();
	    break;
	  case 2:
	    // assume f2 is r/2: r(n,m)
	    if (EC_argument(e2,1).is_long(&n) == EC_succeed && 
		EC_argument(e2,2).is_long(&m) == EC_succeed) {
	      return arg1((unsigned int)n,(unsigned int)m);
	    } else
	      throw Ec2gcException();
	    break;
	  default:
	    throw Ec2gcException();
	  }
	} else
	  throw Ec2gcException();
      } else
	throw Ec2gcException();

    default:
      throw Ec2gcException();
    }
  } else if (e.is_long(&l) == EC_succeed) {
    return REG((int)l);
  } else
    throw Ec2gcException();

}


extern "C" VisAtt
int p_g_post_bool_connectives()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;
    IntConLevel cl;
    Get_Consistency_Level(3, cl);

    try {
	BoolExpr c = ec2boolexpr(EC_arg(2), solver);

	if (solver->is_first()) cache_domain_sizes(solver);
	rel(*solver, c, cl);

	/* check for failure (without full propagation) */
	return (solver->failed() ? EC_fail : EC_succeed);
    }
    catch(Ec2gcException) {
	return TYPE_ERROR;
    }
    CatchAndReportGecodeExceptions
}


extern "C" VisAtt
int p_g_post_intrel_cstr()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    IntConLevel cl;
    Get_Consistency_Level(3, cl);

    try {
	LinRel c = ec2intrel(EC_arg(2), solver);

	if (solver->is_first()) cache_domain_sizes(solver);
	rel(*solver, c, cl);

	/* check for failure (without full propagation) */
	return (solver->failed() ? EC_fail : EC_succeed);
    }
    catch(Ec2gcException) {
	return TYPE_ERROR;
    }
    CatchAndReportGecodeExceptions
}

extern "C" VisAtt
int p_g_post_setvar()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;
    long l;
    int idx, val;

    if (EC_arg(2).is_long(&l) != EC_succeed) return TYPE_ERROR;
    idx = (int) l;
    if (idx < 0) return RANGE_ERROR;
    if (EC_arg(3).is_long(&l) != EC_succeed) return TYPE_ERROR;
    val = (int) l;
    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;
    if (idx >= solver->vInt.size()) {
	return RANGE_ERROR;
    }

    if (solver->is_first()) cache_domain_sizes(solver);

    try {
	dom(*solver, solver->vInt[idx], (int)val);
//    rel(*solver, solver->vInt[idx], IRT_EQ, (int)val);
	if (solver->is_first()) solver->dom_snapshot[idx] = 1; // just assigned!

	return (solver->failed() ? EC_fail : EC_succeed);
    }
    CatchAndReportGecodeExceptions

}

extern "C" VisAtt
int p_g_post_exclude_var_val()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;
    long l;
    int idx, val;

    if (EC_arg(2).is_long(&l) != EC_succeed) return TYPE_ERROR;
    idx = (int) l;
    if (idx < 0) return RANGE_ERROR;
    if (EC_arg(3).is_long(&l) != EC_succeed) return TYPE_ERROR;
    val = (int) l;
    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver ==  NULL) return TYPE_ERROR;
    if (idx >= solver->vInt.size()) {
	return RANGE_ERROR;
    }

    if (solver->is_first()) cache_domain_sizes(solver);

    try {
	// Guido Tack suggests that this is the most efficient
	// way of excluding a value from a variable (2013-02-20)
	Int::IntView vv(solver->vInt[idx]);
	if (me_failed(vv.nq(*solver, (int)val))) solver->fail();
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

extern "C" VisAtt
int p_g_stop_caching()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    solver->stop_caching();
    return EC_succeed;

}

extern "C" VisAtt
int p_g_start_caching()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    solver->start_caching();
    return EC_succeed;

}

extern "C" VisAtt
int p_g_propagate_recompute()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    try {
	if (!solver->status()) return EC_fail;
    }
    CatchAndReportGecodeExceptions

    solver->start_caching();
    return EC_succeed;

}

extern "C" VisAtt
int p_g_propagate()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    try {
	if (!solver->status()) return EC_fail;
    }
    CatchAndReportGecodeExceptions
//    return EC_succeed;

//    if (first == 0) return EC_succeed;


    EC_word tail = nil(), chgtail = nil();

    if (solver->snapshot_valid()) {
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

extern "C" VisAtt
int p_g_post_interval()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;
    long min, max;

    if (EC_succeed != EC_arg(3).is_long(&min)) return(TYPE_ERROR);
    if (EC_succeed != EC_arg(4).is_long(&max)) return(TYPE_ERROR);
    if (min > max) return RANGE_ERROR;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    EC_word varr =  EC_arg(2);
    int size = varr.arity();

    IntVarArgs vars(size);
    int res = assign_IntVarArgs_from_ec_array(solver, size, varr, vars);
    if (res != EC_succeed) return res;

    if (solver->is_first()) cache_domain_sizes(solver);

    try {
	dom(*solver, vars, min, max);
    }
    CatchAndReportGecodeExceptions

    return (solver->failed() ? EC_fail : EC_succeed);
}

extern "C" VisAtt
int p_g_post_var_interval_reif()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;
    long min, max;

    if (EC_succeed != EC_arg(3).is_long(&min)) return(TYPE_ERROR);
    if (EC_succeed != EC_arg(4).is_long(&max)) return(TYPE_ERROR);
//    if (min > max) return RANGE_ERROR;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    long xidx;
    IntVar x;
    EC_functor f;

    try {
        Assign_IntVar(2, xidx, x);

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

	if (solver->is_first()) cache_domain_sizes(solver);

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

extern "C" VisAtt
int p_g_post_dom()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    EC_word darr = EC_arg(3);
    int dsize = darr.arity();
    int ranges[dsize][2];
    if (dsize == 0) return TYPE_ERROR;

    EC_word varr =  EC_arg(2);
    int size = varr.arity();

    int res = get_domain_intervals_from_ec_array(dsize, darr, ranges);
    if (res != EC_succeed) return res;
    try {
	IntSet domset(ranges, dsize);

	IntVarArgs vars(size);
	res = assign_IntVarArgs_from_ec_array(solver, size, varr, vars);
	if (res != EC_succeed) return res;

	if (solver->is_first()) cache_domain_sizes(solver);

	dom(*solver, vars, domset);

	return (solver->failed() ? EC_fail : EC_succeed);
    }
    CatchAndReportGecodeExceptions
}

extern "C" VisAtt
int p_g_post_dom_handle()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    IntSet* domp;
    if (EC_succeed != get_handle_from_arg(3, &domain_method, (void**)&domp))
	return TYPE_ERROR;
    if (domp == NULL) return TYPE_ERROR;

    EC_word varr =  EC_arg(2);
    int size = varr.arity();

    try {
	IntVarArgs vars(size);
	int res = assign_IntVarArgs_from_ec_array(solver, size, varr, vars);
	if (res != EC_succeed) return res;

	if (solver->is_first()) cache_domain_sizes(solver);

	dom(*solver, vars, *domp);

	return (solver->failed() ? EC_fail : EC_succeed);
    }
    CatchAndReportGecodeExceptions
}

extern "C" VisAtt
int p_g_post_exclude_dom()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    EC_word darr = EC_arg(3);
    int dsize = darr.arity();
    int ranges[dsize][2];
    if (dsize == 0) return TYPE_ERROR;

    int res = get_domain_intervals_from_ec_array(dsize, darr, ranges);
    if (res != EC_succeed) return res;
    if (solver->is_first()) cache_domain_sizes(solver);

    EC_word varr =  EC_arg(2);
    int size = varr.arity();

    try {
	IntSet domset(ranges, dsize);

	BoolVar reif(*solver, 0, 0);

	for (int i=1; i<=size; i++) {
	    EC_word arg = EC_argument(varr, i);
	    EC_functor f;
	    long l;
	    IntVar v;

	    if (arg.functor(&f) == EC_succeed) {
		if  (strcmp(f.name(), "_ivar") == 0
		     && EC_argument(arg, 1).is_long(&l) == EC_succeed) {
		    v = solver->vInt[(int)l];
		} else
		    return RANGE_ERROR;
	    } else if (arg.is_long(&l) == EC_succeed) {
		v = IntVar(*solver,(int)l,(int)l);
	    } else
		return TYPE_ERROR;

	    dom(*solver, v, domset, reif);

	    if (solver->failed()) return EC_fail;
	}
	return EC_succeed;
    }
    CatchAndReportGecodeExceptions
}

extern "C" VisAtt
int p_g_post_exclude_dom_handle()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    IntSet* domp;
    if (EC_succeed != get_handle_from_arg(3, &domain_method, (void**)&domp))
	return TYPE_ERROR;
    if (domp == NULL) return TYPE_ERROR;

    EC_word varr =  EC_arg(2);
    int size = varr.arity();

    try {
	BoolVar reif(*solver, 0, 0);

	for (int i=1; i<=size; i++) {
	    EC_word arg = EC_argument(varr, i);
	    EC_functor f;
	    long l;
	    IntVar v;

	    if (arg.functor(&f) == EC_succeed) {
		if  (strcmp(f.name(), "_ivar") == 0
		     && EC_argument(arg, 1).is_long(&l) == EC_succeed) {
		    v = solver->vInt[(int)l];
		} else
		    return RANGE_ERROR;
	    } else if (arg.is_long(&l) == EC_succeed) {
		v = IntVar(*solver,(int)l,(int)l);
	    } else
		return TYPE_ERROR;

	    dom(*solver, v, *domp, reif);

	    if (solver->failed()) return EC_fail;
	}
	return EC_succeed;
    }
    CatchAndReportGecodeExceptions
}

extern "C" VisAtt
int p_g_post_exclude_val()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    long val;
    if (EC_succeed != EC_arg(3).is_long(&val)) return(TYPE_ERROR);

    EC_word varr =  EC_arg(2);
    int size = varr.arity();

    try {

	for (int i=1; i<=size; i++) {
	    EC_word arg = EC_argument(varr, i);
	    long l;

	    if (arg.is_long(&l) != EC_succeed) return TYPE_ERROR;

	    Int::IntView vv(solver->vInt[(int)l]);
	    if (me_failed(vv.nq(*solver, (int)val))) {
		solver->fail();
		return EC_fail;
	    }
	
	}
	return EC_succeed;
    }
    CatchAndReportGecodeExceptions
}

extern "C" VisAtt
int p_g_post_exclude_range()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    long lo, hi;
    if (EC_succeed != EC_arg(3).is_long(&lo)) return(TYPE_ERROR);
    if (EC_succeed != EC_arg(4).is_long(&hi)) return(TYPE_ERROR);

    EC_word varr =  EC_arg(2);
    int size = varr.arity();

    try {
	BoolVar reif(*solver, 0, 0);

	for (int i=1; i<=size; i++) {
	    EC_word arg = EC_argument(varr, i);
	    EC_functor f;
	    long l;
	    IntVar v;

	    if (arg.is_long(&l) == EC_succeed) {
		v = solver->vInt[(int)l];
	    } else
		return TYPE_ERROR;

	    dom(*solver, v, (int)lo, (int)hi, reif);

	    if (solver->failed()) return EC_fail;
	}
	return EC_succeed;
    }
    CatchAndReportGecodeExceptions
}

extern "C" VisAtt
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

	Assign_IntVar(2, xidx, x);

	EC_word darr = EC_arg(3);
	int dsize = darr.arity();
	int ranges[dsize][2];
	if (dsize == 0) return TYPE_ERROR;

	int res = get_domain_intervals_from_ec_array(dsize, darr, ranges);
	if (res != EC_succeed) return res;
	IntSet domset(ranges, dsize);

	long b;
	BoolVar reif;
	bool bool_is_set;

	if (ArgIsVarBoolIdx(4, b)) {
	    reif = solver->vBool[(int)b];
	    bool_is_set = false;
	} else if (EC_arg(4).is_long(&b) == EC_succeed) {
	    if (b < 0 || b > 1) return RANGE_ERROR;
	    bool_is_set = true;
	} else
	    return TYPE_ERROR;

	if (solver->is_first()) cache_domain_sizes(solver);

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

extern "C" VisAtt
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
        Assign_IntVar(2, xidx, x);

	long val;

	if (EC_succeed != EC_arg(3).is_long(&val)) return(TYPE_ERROR);

	long b;
	BoolVar reif;
	bool bool_is_set;

	if (ArgIsVarBoolIdx(4, b)) {
	    reif = solver->vBool[(int)b];
	    bool_is_set = false;
	} else if (EC_arg(4).is_long(&b) == EC_succeed) {
	    if (b < 0 || b > 1) return RANGE_ERROR;
	    bool_is_set = true;
	} else
	    return TYPE_ERROR;

	if (solver->is_first()) cache_domain_sizes(solver);

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

extern "C" VisAtt
int p_g_post_sum()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    EC_word varr = EC_arg(2);
    int size = varr.arity();

    IntVarArgs vars(size);
    int res = assign_IntVarArgs_from_ec_array(solver, size, varr, vars);
    if (res != EC_succeed) return res;

    IntRelType rel;
    Get_IntRelType(3, rel);

    long c;
    IntVar cvar;
    EC_functor f;
    bool c_is_int;
    Assign_IntVar_or_Int(4, c, cvar, c_is_int);

    IntConLevel cl;
    Get_Consistency_Level(5, cl);

    if (solver->is_first()) cache_domain_sizes(solver);

    try {
      if (c_is_int) 
	  linear(*solver, vars, rel, (int)c, cl);
      else
	  linear(*solver, vars, rel, cvar, cl);
      return EC_succeed;
    }
    CatchAndReportGecodeExceptions
}
 
extern "C" VisAtt
int p_g_post_sum_reif()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    EC_word varr = EC_arg(2);
    int size = varr.arity();

    IntVarArgs vars(size);
    int res = assign_IntVarArgs_from_ec_array(solver, size, varr, vars);
    if (res != EC_succeed) return res;

    IntRelType rel;
    Get_IntRelType(3, rel);

    long c;
    IntVar cvar;
    EC_functor f;
    bool c_is_int;
    Assign_IntVar_or_Int(4, c, cvar, c_is_int);

    IntConLevel cl;
    Get_Consistency_Level(6, cl);

    long b;
    BoolVar reif;

    if (ArgIsVarBoolIdx(5, b)) {
        reif = solver->vBool[(int)b];
	    } else if (EC_arg(5).is_long(&b) == EC_succeed) {
        if (b < 0 || b > 1) return RANGE_ERROR;
	reif = BoolVar(*solver, (int)b, (int)b);
    } else
        return TYPE_ERROR;

    if (solver->is_first()) cache_domain_sizes(solver);

    try {
      if (c_is_int) 
	linear(*solver, vars, rel, (int)c, reif, cl);
      else
	linear(*solver, vars, rel, cvar, reif, cl);
      return EC_succeed;
    }
    CatchAndReportGecodeExceptions
}
 
extern "C" VisAtt
int p_g_post_lin()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    EC_word varr = EC_arg(2);
    int size = varr.arity();

    IntVarArgs vars(size);
    int res = assign_IntVarArgs_from_ec_array(solver, size, varr, vars);
    if (res != EC_succeed) return res;

    EC_word carr = EC_arg(3);
    if (carr.arity() != size) return TYPE_ERROR;

    IntArgs cs(size);
    res = assign_IntArgs_from_ec_array(size, carr, cs);
    if (res != EC_succeed) return res;

    IntRelType rel;
    Get_IntRelType(4, rel);

    long c;
    IntVar cvar;
    EC_functor f;
    bool c_is_int;
    Assign_IntVar_or_Int(5, c, cvar, c_is_int);

    IntConLevel cl;
    Get_Consistency_Level(6, cl);

    if (solver->is_first()) cache_domain_sizes(solver);

    try {
      if (c_is_int) 
	  linear(*solver, cs, vars, rel, (int)c, cl);
      else
	  linear(*solver, cs, vars, rel, cvar, cl);
      return EC_succeed;
    }
    CatchAndReportGecodeExceptions
}
 
extern "C" VisAtt
int p_g_post_lin_reif()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    EC_word varr = EC_arg(2);
    int size = varr.arity();

    IntVarArgs vars(size);
    int res = assign_IntVarArgs_from_ec_array(solver, size, varr, vars);
    if (res != EC_succeed) return res;

    EC_word carr = EC_arg(3);
    if (carr.arity() != size) return TYPE_ERROR;

    IntArgs cs(size);
    res = assign_IntArgs_from_ec_array(size, carr, cs);
    if (res != EC_succeed) return res;

    IntRelType rel;
    Get_IntRelType(4, rel);

    long c;
    IntVar cvar;
    EC_functor f;
    bool c_is_int;
    Assign_IntVar_or_Int(5, c, cvar, c_is_int);

    IntConLevel cl;
    Get_Consistency_Level(7, cl);

    long b;
    BoolVar reif;

    if (ArgIsVarBoolIdx(6, b)) {
        reif = solver->vBool[(int)b];
    } else if (EC_arg(6).is_long(&b) == EC_succeed) {
        if (b < 0 || b > 1) return RANGE_ERROR;
	reif = BoolVar(*solver, (int)b, (int)b);
    } else
        return TYPE_ERROR;

    if (solver->is_first()) cache_domain_sizes(solver);

    try {
      if (c_is_int) 
	linear(*solver, cs, vars, rel, (int)c, reif, cl);
      else
	linear(*solver, cs, vars, rel, cvar, reif, cl);
      return EC_succeed;
    }
    CatchAndReportGecodeExceptions
}
 
extern "C" VisAtt
int p_g_post_alldiff()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    EC_word varr = EC_arg(2);
    int size = varr.arity();

    IntVarArgs alldiff(size);
    int res = assign_IntVarArgs_from_ec_array(solver, size, varr, alldiff);
    if (res != EC_succeed) return res;

    IntConLevel cl;
    Get_Consistency_Level(3, cl);

    if (solver->is_first()) cache_domain_sizes(solver);

    try {
	distinct(*solver, alldiff, cl);
    }
    CatchAndReportGecodeExceptions
    return EC_succeed;
}

extern "C" VisAtt
int p_g_post_alldiff_offsets()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    EC_word varr = EC_arg(2);
    int size = varr.arity();

    IntVarArgs alldiff(size);
    int res = assign_IntVarArgs_from_ec_array(solver, size, varr, alldiff);
    if (res != EC_succeed) return res;

    EC_word oarr = EC_arg(3);
    if (oarr.arity() != size) return TYPE_ERROR;

    IntArgs offsets(size);
    res = assign_IntArgs_from_ec_array(size, oarr, offsets);
    if (res != EC_succeed) return res;


    IntConLevel cl;
    Get_Consistency_Level(4, cl);

    if (solver->is_first()) cache_domain_sizes(solver);

    try {
	distinct(*solver, offsets, alldiff, cl);
    }
    CatchAndReportGecodeExceptions

    return EC_succeed;
}

extern "C" VisAtt
int p_g_post_mem()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    EC_word varr = EC_arg(2);
    int size = varr.arity();

    IntVarArgs vs(size);
    int res = assign_IntVarArgs_from_ec_array(solver, size, varr, vs);
    if (res != EC_succeed) return res;

    IntVar mvar;
    long m;
    EC_functor f;
    Assign_IntVar(3, m, mvar);

    if (solver->is_first()) cache_domain_sizes(solver);

    try {
      member(*solver, vs, mvar);
    }
    CatchAndReportGecodeExceptions
    return EC_succeed;
}

extern "C" VisAtt
int p_g_post_mem_reif()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    EC_word varr = EC_arg(2);
    int size = varr.arity();

    IntVarArgs vs(size);
    int res = assign_IntVarArgs_from_ec_array(solver, size, varr, vs);
    if (res != EC_succeed) return res;

    IntVar mvar;
    long m;
    EC_functor f;
    Assign_IntVar(3, m, mvar);

    long b;
    BoolVar reif;

    if (ArgIsVarBoolIdx(4, b)) {
        reif = solver->vBool[(int)b];
	    } else if (EC_arg(4).is_long(&b) == EC_succeed) {
        if (b < 0 || b > 1) return RANGE_ERROR;
	reif = BoolVar(*solver, (int)b, (int)b);
    } else
        return TYPE_ERROR;

    if (solver->is_first()) cache_domain_sizes(solver);

    try {
	member(*solver, vs, mvar, reif);
    }
    CatchAndReportGecodeExceptions
    return EC_succeed;
}

extern "C" VisAtt
int p_g_post_nvalues()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    EC_word varr = EC_arg(2);
    int size = varr.arity();
 
    IntVarArgs vs(size);
    int res = assign_IntVarArgs_from_ec_array(solver, size, varr, vs);
    if (res != EC_succeed) return res;

    IntRelType rel;
    Get_IntRelType(3, rel);

    IntVar valvar;
    long val;
    EC_functor f;
    Assign_IntVar(4, val, valvar);

    if (solver->is_first()) cache_domain_sizes(solver);

    try {
      nvalues(*solver, vs, rel, valvar);
    }
    CatchAndReportGecodeExceptions
    return EC_succeed;
}

extern "C" VisAtt
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
    Assign_IntVar(2, vidx, val);

    IntRelType rel;
    Get_IntRelType(4, rel);

    Assign_IntVar(5, nidx, n);

    EC_word varr = EC_arg(3);
    int size = varr.arity();

    IntVarArgs vars(size);
    int res = assign_IntVarArgs_from_ec_array(solver, size, varr, vars);
    if (res != EC_succeed) return res;

    IntConLevel cl;
    Get_Consistency_Level(6, cl);

    if (solver->is_first()) cache_domain_sizes(solver);

    try {
	count(*solver, vars, val, rel, n, cl);
    }
    CatchAndReportGecodeExceptions

    return EC_succeed;
}

extern "C" VisAtt
int p_g_post_among()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;
    IntVar val, n;
    long vidx, nidx; 

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    IntRelType rel;
    Get_IntRelType(4, rel);

    EC_functor f;
    Assign_IntVar(5, nidx, n);

    EC_word varr = EC_arg(3);
    int size = varr.arity();

    IntVarArgs vars(size);
    int res = assign_IntVarArgs_from_ec_array(solver, size, varr, vars);
    if (res != EC_succeed) return res;

    if (solver->is_first()) cache_domain_sizes(solver);

    try {
	EC_word darr = EC_arg(2);
	int dsize = darr.arity();
	int ranges[dsize][2];

	res = get_domain_intervals_from_ec_array(dsize, darr, ranges);
	if (res != EC_succeed) return res;

	IntSet valset(ranges, dsize);
	count(*solver, vars, valset, rel, n);
    }
    CatchAndReportGecodeExceptions

    return EC_succeed;
}

extern "C" VisAtt
int p_g_post_count_matches()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;
    IntVar val, n;
    long vidx, nidx; 

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    IntRelType rel;
    Get_IntRelType(4, rel);

    EC_functor f;
    Assign_IntVar(5, nidx, n);

    EC_word varr = EC_arg(3);
    int size = varr.arity();

    IntVarArgs vars(size);
    int res = assign_IntVarArgs_from_ec_array(solver, size, varr, vars);
    if (res != EC_succeed) return res;

    if (solver->is_first()) cache_domain_sizes(solver);

    try {
	EC_word valarr = EC_arg(2);
	if (valarr.arity() != size) return RANGE_ERROR;

	IntArgs vals(size);
	res = assign_IntArgs_from_ec_array(size, valarr, vals);
	if (res != EC_succeed) return res;

	count(*solver, vars, vals, rel, n);
    }
    CatchAndReportGecodeExceptions

    return EC_succeed;
}

extern "C" VisAtt
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

    EC_word pvals = EC_arg(2);
    int specsize = pvals.arity();

    IntArgs vals(specsize);
    int res = assign_IntArgs_from_ec_array(specsize, pvals, vals);
    if (res != EC_succeed) return res;

    EC_word poccurs = EC_arg(3);
    if (specsize != poccurs.arity()) return RANGE_ERROR;

    IntVarArgs occurs(specsize);
    res = assign_IntVarArgs_from_ec_array(solver, specsize, poccurs, occurs);
    if (res != EC_succeed) return res;

    EC_word varr = EC_arg(4);
    int size = varr.arity();

    IntVarArgs vars(size);
    res = assign_IntVarArgs_from_ec_array(solver, size, varr, vars);
    if (res != EC_succeed) return res;

    IntConLevel cl;
    Get_Consistency_Level(5, cl);

    if (solver->is_first()) cache_domain_sizes(solver);

    try {
	count(*solver, vars, occurs, vals, cl);
    }
    CatchAndReportGecodeExceptions

    return EC_succeed;
}

extern "C" VisAtt
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

	Assign_IntVar_or_Int(4, v, vvar, v_is_int);

	Assign_IntVar(2, i, ivar);

	EC_word arr = EC_arg(3);
	int size = arr.arity();

	IntVarArgs vals(size);
	int res = assign_IntVarArgs_from_ec_array(solver, size, arr, vals);
	if (res != EC_succeed) return res;

	IntConLevel cl;
	Get_Consistency_Level(5, cl);

	if (solver->is_first()) cache_domain_sizes(solver);

	if (v_is_int)
	    element(*solver, vals, ivar, v, cl);
	else
	    element(*solver, vals, ivar, vvar, cl);
    }
    CatchAndReportGecodeExceptions

    return EC_succeed;
}

extern "C" VisAtt
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

    EC_word pvals = EC_arg(6);
    int specsize = pvals.arity();

    IntArgs vals(specsize);
    int res = assign_IntArgs_from_ec_array(specsize, pvals, vals);
    if (res != EC_succeed) return res;
    IntSet valset(vals);

    EC_word varr = EC_arg(5);
    int size = varr.arity();

    IntVarArgs vars(size);
    res = assign_IntVarArgs_from_ec_array(solver, size, varr, vars);
    if (res != EC_succeed) return res;

    long lo, hi, k;
    if (EC_succeed != EC_arg(2).is_long(&lo)) return TYPE_ERROR;
    if (EC_succeed != EC_arg(3).is_long(&hi)) return TYPE_ERROR;
    if (EC_succeed != EC_arg(4).is_long(&k)) return TYPE_ERROR;

    IntConLevel cl;
    Get_Consistency_Level(7, cl);

    if (solver->is_first()) cache_domain_sizes(solver);

    try {
	sequence(*solver, vars, valset, k, lo, hi, cl);
    }
    CatchAndReportGecodeExceptions

    return EC_succeed;
}

extern "C" VisAtt
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

    EC_word varr = EC_arg(5);
    int size = varr.arity();

    BoolVarArgs vars(size);
    int res = assign_BoolVarArgs_from_ec_array(solver, size, varr, vars);
    if (res != EC_succeed) return res;

    long lo, hi, k;
    if (EC_succeed != EC_arg(2).is_long(&lo)) return TYPE_ERROR;
    if (EC_succeed != EC_arg(3).is_long(&hi)) return TYPE_ERROR;
    if (EC_succeed != EC_arg(4).is_long(&k)) return TYPE_ERROR;

    IntConLevel cl;
    Get_Consistency_Level(6, cl);

    if (solver->is_first()) cache_domain_sizes(solver);

    try {
	sequence(*solver, vars, valset, k, lo, hi, cl);
    }
    CatchAndReportGecodeExceptions

    return EC_succeed;
}

extern "C" VisAtt
int p_g_post_sorted2()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    EC_word uarr = EC_arg(2);
    int size = uarr.arity();

    IntVarArgs unsort(size);
    int res = assign_IntVarArgs_from_ec_array(solver, size, uarr, unsort);
    if (res != EC_succeed) return res;

    EC_word sarr = EC_arg(3);
    if (sarr.arity() != size) return RANGE_ERROR;

    IntVarArgs sort(size);
    res = assign_IntVarArgs_from_ec_array(solver, size, sarr, sort);
    if (res != EC_succeed) return res;

    IntConLevel cl;
    Get_Consistency_Level(4, cl);

    if (solver->is_first()) cache_domain_sizes(solver);

    try {
	sorted(*solver, unsort, sort, cl);
    }
    CatchAndReportGecodeExceptions

    return EC_succeed;
}

extern "C" VisAtt
int p_g_post_sorted()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    EC_word uarr = EC_arg(2);
    int size = uarr.arity();

    IntVarArgs unsort(size);
    int res = assign_IntVarArgs_from_ec_array(solver, size, uarr, unsort);
    if (res != EC_succeed) return res;

    EC_word sarr = EC_arg(3);
    if (sarr.arity() != size) return RANGE_ERROR;

    IntVarArgs sort(size);
    res = assign_IntVarArgs_from_ec_array(solver, size, sarr, sort);
    if (res != EC_succeed) return res;

    EC_word parr = EC_arg(4);
    if (parr.arity() != size) return RANGE_ERROR;

    IntVarArgs pos(size);
    res = assign_IntVarArgs_from_ec_array(solver, size, parr, pos);
    if (res != EC_succeed) return res;

    IntConLevel cl;
    Get_Consistency_Level(5, cl);

    if (solver->is_first()) cache_domain_sizes(solver);

    try {
	sorted(*solver, unsort, sort, pos, cl);
    }
    CatchAndReportGecodeExceptions

    return EC_succeed;
}


extern "C" VisAtt
int p_g_post_disj()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    EC_word sarr = EC_arg(2);
    int size = sarr.arity();

    IntVarArgs starts(size);
    int res = assign_IntVarArgs_from_ec_array(solver, size, sarr, starts);
    if (res != EC_succeed) return res;

    EC_word darr = EC_arg(3);
    if (darr.arity() != size) return RANGE_ERROR;

    IntArgs durations(size);
    res = assign_IntArgs_from_ec_array(size, darr, durations);
    if (res != EC_succeed) return res;

    EC_word barr = EC_arg(4);
    BoolVarArgs scheduled(size);
    bool has_optional = (barr.arity() != 0);
    if (has_optional) {
	if (size != barr.arity()) return TYPE_ERROR;

	res = assign_BoolVarArgs_from_ec_array(solver, size, barr, scheduled);
	if (res != EC_succeed) return res;
    }

    if (solver->is_first()) cache_domain_sizes(solver);

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

extern "C" VisAtt
int p_g_post_disjflex()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    EC_word sarr = EC_arg(2);
    int size = sarr.arity();

    IntVarArgs starts(size);
    int res = assign_IntVarArgs_from_ec_array(solver, size, sarr, starts);
    if (res != EC_succeed) return res;

    EC_word darr = EC_arg(3);
    if (darr.arity() != size) return RANGE_ERROR;

    IntVarArgs durations(size);
    res = assign_IntVarArgs_from_ec_array(solver, size, darr, durations);
    if (res != EC_succeed) return res;

    EC_word earr = EC_arg(4);
    if (earr.arity() != size) return RANGE_ERROR;

    IntVarArgs ends(size);
    res = assign_IntVarArgs_from_ec_array(solver, size, earr, ends);
    if (res != EC_succeed) return res;

    EC_word barr = EC_arg(5);
    BoolVarArgs scheduled(size);
    bool has_optional = (barr.arity() != 0);
    if (has_optional) {
	if (size != barr.arity()) return TYPE_ERROR;

	res = assign_BoolVarArgs_from_ec_array(solver, size, barr, scheduled);
	if (res != EC_succeed) return res;
    }

    if (solver->is_first()) cache_domain_sizes(solver);

    try {
	// consistency level not supported!
	if (has_optional) {
	  unary(*solver, starts, durations, ends, scheduled);
	} else {
	  unary(*solver, starts, ends, durations);
	}
    }
    CatchAndReportGecodeExceptions

    return EC_succeed;
}

extern "C" VisAtt
int p_g_post_cumulatives()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    EC_word sarr = EC_arg(2);
    int size = sarr.arity();

    IntVarArgs starts(size);
    int res = assign_IntVarArgs_from_ec_array(solver, size, sarr, starts);
    if (res != EC_succeed) return res;

    EC_word darr = EC_arg(3);
    if (darr.arity() != size) return RANGE_ERROR;

    IntVarArgs durations(size);
    res = assign_IntVarArgs_from_ec_array(solver, size, darr, durations);

    EC_word earr = EC_arg(4);
    if (earr.arity() != size) return RANGE_ERROR;

    IntVarArgs ends(size);
    res = assign_IntVarArgs_from_ec_array(solver, size, earr, ends);

    EC_word uarr = EC_arg(5);
    if (uarr.arity() != size) return RANGE_ERROR;

    IntVarArgs usages(size);
    res = assign_IntVarArgs_from_ec_array(solver, size, uarr, usages);

    EC_word usedarr = EC_arg(6);
    if (usedarr.arity() != size) return RANGE_ERROR;

    IntVarArgs used(size);
    res = assign_IntVarArgs_from_ec_array(solver, size, usedarr, used);

    EC_word larr = EC_arg(7);
    int nmachines = larr.arity();

    IntArgs limits(nmachines);
    res = assign_IntArgs_from_ec_array(nmachines, larr, limits);
    if (res != EC_succeed) return res;

    long ec_atmost;
    bool atmost;
    if (EC_succeed != EC_arg(8).is_long(&ec_atmost)) return TYPE_ERROR;
    atmost = (ec_atmost ? true : false);

    if (solver->is_first()) cache_domain_sizes(solver);

    try {
	cumulatives(*solver, used, starts, durations, ends, usages, limits, ec_atmost);
    }
    CatchAndReportGecodeExceptions

    return EC_succeed;
}

extern "C" VisAtt
int p_g_post_cumulative()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    EC_word sarr = EC_arg(2);
    int size = sarr.arity();

    IntVarArgs starts(size);
    int res = assign_IntVarArgs_from_ec_array(solver, size, sarr, starts);
    if (res != EC_succeed) return res;

    EC_word darr = EC_arg(3);
    if (darr.arity() != size) return RANGE_ERROR;

    IntArgs durations(size);
    res = assign_IntArgs_from_ec_array(size, darr, durations);

    EC_word uarr = EC_arg(4);
    if (uarr.arity() != size) return RANGE_ERROR;

    IntArgs usages(size);
    res = assign_IntArgs_from_ec_array(size, uarr, usages);

    long lidx;
    IntVar limit;
    EC_functor f;
    Assign_IntVar(5, lidx, limit);

    if (solver->is_first()) cache_domain_sizes(solver);

    EC_word barr = EC_arg(6);
    bool has_optional = (barr.arity() != 0);
    if (has_optional) {
	if (size != barr.arity()) return TYPE_ERROR;

	BoolVarArgs scheduled(size);
	res = assign_BoolVarArgs_from_ec_array(solver, size, barr, scheduled);
	if (res != EC_succeed) return res;
	try {
	  cumulative(*solver, limit, starts, durations, usages, scheduled);
	}
	CatchAndReportGecodeExceptions

    } else {

      try {
	cumulative(*solver, limit, starts, durations, usages);
      }
      CatchAndReportGecodeExceptions
    }

    return EC_succeed;
}

extern "C" VisAtt
int p_g_post_cumulativeflex()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    EC_word sarr = EC_arg(2);
    int size = sarr.arity();

    IntVarArgs starts(size);
    int res = assign_IntVarArgs_from_ec_array(solver, size, sarr, starts);
    if (res != EC_succeed) return res;

    EC_word darr = EC_arg(3);
    if (darr.arity() != size) return RANGE_ERROR;

    IntVarArgs durations(size);
    res = assign_IntVarArgs_from_ec_array(solver, size, darr, durations);

    EC_word earr = EC_arg(4);
    if (earr.arity() != size) return RANGE_ERROR;

    IntVarArgs ends(size);
    res = assign_IntVarArgs_from_ec_array(solver, size, earr, ends);

    EC_word uarr = EC_arg(5);
    if (uarr.arity() != size) return RANGE_ERROR;

    IntArgs usages(size);
    res = assign_IntArgs_from_ec_array(size, uarr, usages);

    long lidx;
    IntVar limit;
    EC_functor f;
    Assign_IntVar(6, lidx, limit);

    if (solver->is_first()) cache_domain_sizes(solver);

    EC_word barr = EC_arg(7);
    bool has_optional = (barr.arity() != 0);
    if (has_optional) {
	if (size != barr.arity()) return TYPE_ERROR;

	BoolVarArgs scheduled(size);
	res = assign_BoolVarArgs_from_ec_array(solver, size, barr, scheduled);
	if (res != EC_succeed) return res;
	try {
	  cumulative(*solver, limit, starts, durations, ends, usages, scheduled);
	}
	CatchAndReportGecodeExceptions

    } else {

      try {
	cumulative(*solver, limit, starts, durations, ends, usages);
      }
      CatchAndReportGecodeExceptions
    }

    return EC_succeed;
}

extern "C" VisAtt
int p_g_post_circuit()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    EC_word arr = EC_arg(2);
    int size = arr.arity();

    IntVarArgs succ(size);
    int res = assign_IntVarArgs_from_ec_array(solver, size, arr, succ);
    if (res != EC_succeed) return res;

    IntConLevel cl;
    Get_Consistency_Level(4, cl);

    if (solver->is_first()) cache_domain_sizes(solver);

    try {
	long offset;
	if (EC_arg(3).is_long(&offset) != EC_succeed) return TYPE_ERROR;
	if (offset == 0)
	    circuit(*solver, succ, cl);
	else 
	    circuit(*solver, (int)offset, succ, cl);
    }
    CatchAndReportGecodeExceptions

    return EC_succeed;
}

extern "C" VisAtt
int p_g_post_circuit_cost()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    EC_word arr = EC_arg(2);
    int size = arr.arity();

    IntVarArgs succ(size);
    int res = assign_IntVarArgs_from_ec_array(solver, size, arr, succ);
    if (res != EC_succeed) return res;

    EC_word cmarr = EC_arg(3);
    int cmsize = cmarr.arity();
    if (cmsize != size*size) return RANGE_ERROR;
    
    IntArgs cm(cmsize);
    res = assign_IntArgs_from_ec_array(cmsize, cmarr, cm);
    if (res != EC_succeed) return res;

    EC_functor f;
    long cidx;
    IntVar c;
    Assign_IntVar(5, cidx, c);

    long offset;
    if (EC_arg(6).is_long(&offset) != EC_succeed) return TYPE_ERROR;

    IntConLevel cl;
    Get_Consistency_Level(7, cl);

    if (solver->is_first()) cache_domain_sizes(solver);

    try {
	if (EC_arg(4).is_nil() != EC_succeed) {
	    EC_word acarr = EC_arg(4);
	    if (size != acarr.arity()) return RANGE_ERROR;
	    IntVarArgs arccosts(size);
	    res = assign_IntVarArgs_from_ec_array(solver, size, acarr, arccosts);
	    if (res != EC_succeed) return res;
	    if (offset == 0) 
		circuit(*solver, cm, succ, arccosts, c, cl);
	    else 
	      circuit(*solver, cm, (int)offset, succ, arccosts, c, cl);
	} else {
	    if (offset == 0) 
		circuit(*solver, cm, succ, c, cl);
	    else 
	      circuit(*solver, cm, (int)offset, succ, c, cl);
	}
    }
    CatchAndReportGecodeExceptions

    return EC_succeed;
}

extern "C" VisAtt
int p_g_post_ham_path()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    long idx;
    EC_functor f;
    IntVar start;
    Assign_IntVar(2, idx, start);

    IntVar end;
    Assign_IntVar(3, idx, end);

    EC_word arr = EC_arg(4);
    int size = arr.arity();

    IntVarArgs succ(size);
    int res = assign_IntVarArgs_from_ec_array(solver, size, arr, succ);
    if (res != EC_succeed) return res;

    IntConLevel cl;
    Get_Consistency_Level(6, cl);

    if (solver->is_first()) cache_domain_sizes(solver);

    try {
	long offset;
	if (EC_arg(5).is_long(&offset) != EC_succeed) return TYPE_ERROR;
	if (offset == 0)
	  path(*solver, succ, start, end, cl);
	else 
	  path(*solver, (int)offset, succ, start, end, cl);
    }
    CatchAndReportGecodeExceptions

    return EC_succeed;
}

extern "C" VisAtt
int p_g_post_ham_path_cost()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    long idx;
    EC_functor f;
    IntVar start;
    Assign_IntVar(2, idx, start);

    IntVar end;
    Assign_IntVar(3, idx, end);

    EC_word arr = EC_arg(4);
    int size = arr.arity();

    IntVarArgs succ(size);
    int res = assign_IntVarArgs_from_ec_array(solver, size, arr, succ);
    if (res != EC_succeed) return res;

    EC_word cmarr = EC_arg(5);
    int cmsize = cmarr.arity();
    if (cmsize != size*size) return RANGE_ERROR;
    
    IntArgs cm(cmsize);
    res = assign_IntArgs_from_ec_array(cmsize, cmarr, cm);
    if (res != EC_succeed) return res;

    long cidx;
    IntVar c;
    Assign_IntVar(7, cidx, c);

    long offset;
    if (EC_arg(8).is_long(&offset) != EC_succeed) return TYPE_ERROR;

    IntConLevel cl;
    Get_Consistency_Level(9, cl);

    if (solver->is_first()) cache_domain_sizes(solver);

    try {
	if (EC_arg(6).is_nil() != EC_succeed) {
	    EC_word acarr = EC_arg(6);
	    if (size != acarr.arity()) return RANGE_ERROR;
	    IntVarArgs arccosts(size);
	    res = assign_IntVarArgs_from_ec_array(solver, size, acarr, arccosts);
	    if (res != EC_succeed) return res;
	    if (offset == 0) 
	       path(*solver, cm, succ, start, end, arccosts, c, cl);
	    else 
	      path(*solver, cm, (int)offset, succ, start, end, arccosts, c, cl);
	} else {
	    if (offset == 0) 
	      path(*solver, cm, succ, start, end, c, cl);
	    else 
	      path(*solver, cm, (int)offset, succ, start, end, c, cl);
	}
    }
    CatchAndReportGecodeExceptions

    return EC_succeed;
}

extern "C" VisAtt
int p_g_post_precede()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;
    long s, t;

    if (EC_succeed != EC_arg(2).is_long(&s)) return(TYPE_ERROR);
    if (EC_succeed != EC_arg(3).is_long(&t)) return(TYPE_ERROR);

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    EC_word varr =  EC_arg(4);
    int size = varr.arity();

    IntVarArgs vars(size);
    int res = assign_IntVarArgs_from_ec_array(solver, size, varr, vars);
    if (res != EC_succeed) return res;

    if (solver->is_first()) cache_domain_sizes(solver);

    try {
      precede(*solver, vars, (int)s, (int)t);
    }
    CatchAndReportGecodeExceptions

    return EC_succeed;
}

extern "C" VisAtt
int p_g_post_precede_chain()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    EC_word varr =  EC_arg(3);
    int size = varr.arity();

    IntVarArgs vars(size);
    int res = assign_IntVarArgs_from_ec_array(solver, size, varr, vars);
    if (res != EC_succeed) return res;

    EC_word valarr =  EC_arg(2);
    size = valarr.arity();

    IntArgs vals(size);
    res = assign_IntArgs_from_ec_array(size, valarr, vals);
    if (res != EC_succeed) return res;

    if (solver->is_first()) cache_domain_sizes(solver);

    try {
	precede(*solver, vars, vals);
    }
    CatchAndReportGecodeExceptions

    return EC_succeed;
}

extern "C" VisAtt
int p_g_post_disjoint2()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    EC_word xarr =  EC_arg(2);
    int size = xarr.arity();

    IntVarArgs xs(size);
    int res = assign_IntVarArgs_from_ec_array(solver, size, xarr, xs);
    if (res != EC_succeed) return res;

    EC_word warr =  EC_arg(3);
    if (warr.arity() != size) return RANGE_ERROR;

    IntArgs ws(size);
    res = assign_IntArgs_from_ec_array(size, warr, ws);
    if (res != EC_succeed) return res;

    EC_word yarr =  EC_arg(4);
    if (yarr.arity() != size) return RANGE_ERROR;

    IntVarArgs ys(size);
    res = assign_IntVarArgs_from_ec_array(solver, size, yarr, ys);
    if (res != EC_succeed) return res;

    EC_word harr =  EC_arg(5);
    if (harr.arity() != size) return RANGE_ERROR;

    IntArgs hs(size);
    res = assign_IntArgs_from_ec_array(size, harr, hs);
    if (res != EC_succeed) return res;

    try {
	if (EC_arg(6).is_nil() != EC_succeed) {
	    EC_word oarr = EC_arg(6);
	    if (size != oarr.arity()) return RANGE_ERROR;

	    BoolVarArgs os(size);
	    res = assign_BoolVarArgs_from_ec_array(solver, size, oarr, os);

	    if (solver->is_first()) cache_domain_sizes(solver);
	    nooverlap(*solver, xs, ws, ys, hs, os);
	} else {
	    if (solver->is_first()) cache_domain_sizes(solver);
	    nooverlap(*solver, xs, ws, ys, hs);
	}
    }
    CatchAndReportGecodeExceptions

    return EC_succeed;
}

extern "C" VisAtt
int p_g_post_disjointflex2()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    EC_word x1arr =  EC_arg(2);
    int size = x1arr.arity();

    IntVarArgs x1s(size);
    int res = assign_IntVarArgs_from_ec_array(solver, size, x1arr, x1s);
    if (res != EC_succeed) return res;

    EC_word warr =  EC_arg(3);
    if (warr.arity() != size) return RANGE_ERROR;

    IntVarArgs ws(size);
    res = assign_IntVarArgs_from_ec_array(solver, size, warr, ws);
    if (res != EC_succeed) return res;

    EC_word y1arr =  EC_arg(4);
    if (y1arr.arity() != size) return RANGE_ERROR;

    IntVarArgs y1s(size);
    res = assign_IntVarArgs_from_ec_array(solver, size, y1arr, y1s);
    if (res != EC_succeed) return res;

    EC_word harr =  EC_arg(5);
    if (harr.arity() != size) return RANGE_ERROR;

    IntVarArgs hs(size);
    res = assign_IntVarArgs_from_ec_array(solver, size, harr, hs);
    if (res != EC_succeed) return res;

    EC_word x2arr =  EC_arg(7);
    if (x2arr.arity() != size) return RANGE_ERROR;

    IntVarArgs x2s(size);
    res = assign_IntVarArgs_from_ec_array(solver, size, x2arr, x2s);
    if (res != EC_succeed) return res;

    EC_word y2arr =  EC_arg(8);
    if (y2arr.arity() != size) return RANGE_ERROR;

    IntVarArgs y2s(size);
    res = assign_IntVarArgs_from_ec_array(solver, size, y2arr, y2s);
    if (res != EC_succeed) return res;

    try {
	if (EC_arg(6).is_nil() != EC_succeed) {
	    EC_word oarr = EC_arg(6);
	    if (size != oarr.arity()) return RANGE_ERROR;

	    BoolVarArgs os(size);
	    res = assign_BoolVarArgs_from_ec_array(solver, size, oarr, os);

	    if (solver->is_first()) cache_domain_sizes(solver);
	    nooverlap(*solver, x1s, ws, x2s, y1s, hs, y2s, os);
	} else {
	    if (solver->is_first()) cache_domain_sizes(solver);
	    nooverlap(*solver, x1s, ws, x2s, y1s, hs, y2s);
	}
    }
    CatchAndReportGecodeExceptions

    return EC_succeed;
}

extern "C" VisAtt
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
	Assign_IntVar(2, xidx, x);
	Assign_IntVar(3, yidx, y);

	IntConLevel cl;
	Get_Consistency_Level(4, cl);

	if (solver->is_first()) cache_domain_sizes(solver);

	sqrt(*solver, y, x, cl);
    }
    CatchAndReportGecodeExceptions

    return EC_succeed;
}

extern "C" VisAtt
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
	Assign_IntVar(2, xidx, x);
	Assign_IntVar(3, yidx, y);

	IntConLevel cl;
	Get_Consistency_Level(4, cl);

	if (solver->is_first()) cache_domain_sizes(solver);

	sqr(*solver, y, x, cl);
    }
    CatchAndReportGecodeExceptions

    return EC_succeed;
}


extern "C" VisAtt
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
	Assign_IntVar(2, xidx, x);
	Assign_IntVar(3, yidx, y);

	if (solver->is_first()) cache_domain_sizes(solver);

	abs(*solver, y, x);
    }
    CatchAndReportGecodeExceptions

    return EC_succeed;
}


extern "C" VisAtt
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
	Assign_IntVar(2, xidx, x);
	Assign_IntVar(3, yidx, y);
	Assign_IntVar(4, zidx, z);

	IntConLevel cl;
	Get_Consistency_Level(5, cl);

	if (solver->is_first()) cache_domain_sizes(solver);

	mult(*solver, y, z, x, cl);
    }
    CatchAndReportGecodeExceptions

    return EC_succeed;
}

extern "C" VisAtt
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
	Assign_IntVar(2, xidx, x);
	Assign_IntVar(3, yidx, y);
	Assign_IntVar(4, zidx, z);

	if (solver->is_first()) cache_domain_sizes(solver);

	div(*solver, y, z, x);
    }
    CatchAndReportGecodeExceptions

    return EC_succeed;
}

extern "C" VisAtt
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
	Assign_IntVar(2, xidx, x);
	Assign_IntVar(3, yidx, y);
	Assign_IntVar(4, zidx, z);

	if (solver->is_first()) cache_domain_sizes(solver);

	mod(*solver, y, z, x);
    }
    CatchAndReportGecodeExceptions

    return EC_succeed;
}

extern "C" VisAtt
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
	Assign_IntVar(2, xidx, x);
	Assign_IntVar(3, yidx, y);
	Assign_IntVar(4, qidx, q);
	Assign_IntVar(5, midx, m);

	if (solver->is_first()) cache_domain_sizes(solver);

	divmod(*solver, x, y, q, m);
    }
    CatchAndReportGecodeExceptions

    return EC_succeed;
}

extern "C" VisAtt
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
	Assign_IntVar(2, xidx, x);
	Assign_IntVar(3, yidx, y);
	Assign_IntVar(4, zidx, z);

	IntConLevel cl;
	Get_Consistency_Level(5, cl);

	if (solver->is_first()) cache_domain_sizes(solver);

	max(*solver, y, z, x, cl);
    }
    CatchAndReportGecodeExceptions

    return EC_succeed;
}

extern "C" VisAtt
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
	Assign_IntVar(2, xidx, x);
	Assign_IntVar(3, yidx, y);
	Assign_IntVar(4, zidx, z);

	IntConLevel cl;
	Get_Consistency_Level(5, cl);

	if (solver->is_first()) cache_domain_sizes(solver);

	min(*solver, y, z, x, cl);
    }
    CatchAndReportGecodeExceptions

    return EC_succeed;
}

extern "C" VisAtt
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
	Assign_IntVar(2, xidx, x);

	EC_word varr = EC_arg(3);
	int size = varr.arity();

	IntVarArgs vars(size);
	int res = assign_IntVarArgs_from_ec_array(solver, size, varr, vars);
	if (res != EC_succeed) return res;

	IntConLevel cl;
	Get_Consistency_Level(4, cl);

	if (solver->is_first()) cache_domain_sizes(solver);

	max(*solver, vars, x, cl);
    }
    CatchAndReportGecodeExceptions

    return EC_succeed;
}

extern "C" VisAtt
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
	Assign_IntVar(2, xidx, x);

	EC_word varr = EC_arg(3);
	int size = varr.arity();

	IntVarArgs vars(size);
	int res = assign_IntVarArgs_from_ec_array(solver, size, varr, vars);
	if (res != EC_succeed) return res;

	IntConLevel cl;
	Get_Consistency_Level(4, cl);

	if (solver->is_first()) cache_domain_sizes(solver);

	min(*solver, vars, x, cl);
    }
    CatchAndReportGecodeExceptions

    return EC_succeed;
}

extern "C" VisAtt
int p_g_post_rel()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    try {
	EC_functor f;
	long i;
	IntVar x, y;
	Assign_IntVar(2, i, x);
    
        bool y_is_int = false;
	Assign_IntVar_or_Int(4, i, y, y_is_int);

	IntRelType r;
	Get_IntRelType(3, r);

	IntConLevel cl;
	Get_Consistency_Level(5, cl);

	if (solver->is_first()) cache_domain_sizes(solver);

	if (y_is_int) 
	  rel(*solver, x, r, (int)i, cl);
	else
	    rel(*solver, x, r, y, cl);
    }
    CatchAndReportGecodeExceptions

    return EC_succeed;
}

extern "C" VisAtt
int p_g_post_collection_rel()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    try {
	EC_word xarr = EC_arg(2);
	long i = xarr.arity();
	if (i == 0) return TYPE_ERROR;

	IntVarArgs xs(i);
	int res = assign_IntVarArgs_from_ec_array(solver, i, xarr, xs);
	if (res != EC_succeed) return res;
    
	IntVar y;
        bool y_is_int = false;
	EC_functor f;
	Assign_IntVar_or_Int(4, i, y, y_is_int);

	IntRelType r;
	Get_IntRelType(3, r);

	IntConLevel cl;
	Get_Consistency_Level(5, cl);

	if (solver->is_first()) cache_domain_sizes(solver);

	if (y_is_int) 
	    rel(*solver, xs, r, (int)i, cl);
	else
	    rel(*solver, xs, r, y, cl);
    }
    CatchAndReportGecodeExceptions

    return EC_succeed;
}

extern "C" VisAtt
int p_g_post_lwb()
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
	Assign_IntVar(2, xidx, x);

	long lwb;
	if (EC_arg(3).is_long(&lwb) != EC_succeed) return TYPE_ERROR;

	if (solver->is_first()) cache_domain_sizes(solver);

	rel(*solver, x, IRT_GQ, (int)lwb);
    }
    CatchAndReportGecodeExceptions

    return EC_succeed;
}

extern "C" VisAtt
int p_g_post_upb()
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
	Assign_IntVar(2, xidx, x);

	long upb;
	if (EC_arg(3).is_long(&upb) != EC_succeed) return TYPE_ERROR;

	if (solver->is_first()) cache_domain_sizes(solver);

	rel(*solver, x, IRT_LQ, (int)upb);
    }
    CatchAndReportGecodeExceptions

    return EC_succeed;
}

extern "C" VisAtt
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
	Assign_IntVar(2, xidx, x);

	EC_word barr = EC_arg(3);
	int size = barr.arity();

	BoolVarArgs vars(size);
	int res = assign_BoolVarArgs_from_ec_array(solver, size, barr, vars);
	if (res != EC_succeed) return res;

	long min;
	if (EC_succeed != EC_arg(4).is_long(&min)) return TYPE_ERROR;

	IntConLevel cl;
	Get_Consistency_Level(5, cl);

	if (solver->is_first()) cache_domain_sizes(solver);

	channel(*solver, vars, x, min, cl);
    }
    CatchAndReportGecodeExceptions

    return EC_succeed;
}

extern "C" VisAtt
int p_g_post_inverse()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    EC_word arr1 = EC_arg(2);
    int size = arr1.arity();

    EC_word arr2 = EC_arg(3);
    if (size != arr2.arity()) return TYPE_ERROR;

    IntVarArgs vars1(size);
    int res = assign_IntVarArgs_from_ec_array(solver, size, arr1, vars1);
    if (res != EC_succeed) return res;

    IntVarArgs vars2(size);
    res = assign_IntVarArgs_from_ec_array(solver, size, arr2, vars2);
    if (res != EC_succeed) return res;

    IntConLevel cl;
    Get_Consistency_Level(4, cl);

    if (solver->is_first()) cache_domain_sizes(solver);

    try {
	channel(*solver, vars1, vars2, cl);
    }
    CatchAndReportGecodeExceptions

    return EC_succeed;
}

extern "C" VisAtt
int p_g_post_inverse_offset()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    long off1;
    if (EC_succeed != EC_arg(3).is_long(&off1)) return TYPE_ERROR;

    long off2;
    if (EC_succeed != EC_arg(5).is_long(&off2)) return TYPE_ERROR;

    EC_word arr1 = EC_arg(2);
    int size = arr1.arity();

    EC_word arr2 = EC_arg(4);
    if (size != arr2.arity()) return TYPE_ERROR;

    IntVarArgs vars1(size);
    int res = assign_IntVarArgs_from_ec_array(solver, size, arr1, vars1);
    if (res != EC_succeed) return res;

    IntVarArgs vars2(size);
    res = assign_IntVarArgs_from_ec_array(solver, size, arr2, vars2);
    if (res != EC_succeed) return res;

    IntConLevel cl;
    Get_Consistency_Level(6, cl);

    if (solver->is_first()) cache_domain_sizes(solver);

    try {
	channel(*solver, vars1, off1, vars2, off2, cl);
    }
    CatchAndReportGecodeExceptions

    return EC_succeed;
}

extern "C" VisAtt
int p_g_post_ordered()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    IntRelType r;
    Get_IntRelType(3, r);

    EC_word varr = EC_arg(2);
    int size = varr.arity();

    IntVarArgs vars(size);
    int res = assign_IntVarArgs_from_ec_array(solver, size, varr, vars);
    if (res != EC_succeed) return res;

    IntConLevel cl;
    Get_Consistency_Level(4, cl);

    if (solver->is_first()) cache_domain_sizes(solver);

    try {
      rel(*solver, vars, r, cl);
    }
    CatchAndReportGecodeExceptions

    return EC_succeed;
}

extern "C" VisAtt
int p_g_post_lex_order()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    IntRelType r;
    Get_IntRelType(3, r);

    EC_word xarr = EC_arg(2);
    int size = xarr.arity(); // can be 0

    IntVarArgs xvars(size);
    int res = assign_IntVarArgs_from_ec_array(solver, size, xarr, xvars);
    if (res != EC_succeed) return res;

    EC_word yarr = EC_arg(4);
    size = yarr.arity(); // can be 0

    IntVarArgs yvars(size);
    res = assign_IntVarArgs_from_ec_array(solver, size, yarr, yvars);
    if (res != EC_succeed) return res;

    IntConLevel cl;
    Get_Consistency_Level(5, cl);

    if (solver->is_first()) cache_domain_sizes(solver);

    try {
      rel(*solver, xvars, r, yvars, cl);
    }
    CatchAndReportGecodeExceptions

    return EC_succeed;
}

extern "C" VisAtt
int p_g_post_bin_packing()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    EC_word iarr = EC_arg(2);
    int size = iarr.arity();

    IntVarArgs ivars(size);
    int res = assign_IntVarArgs_from_ec_array(solver, size, iarr, ivars);
    if (res != EC_succeed) return res;

    EC_word sarr = EC_arg(3);
    if (size != sarr.arity()) return TYPE_ERROR;

    IntArgs sizes(size);
    res = assign_IntArgs_from_ec_array(size, sarr, sizes);
    if (res != EC_succeed) return res;

    EC_word larr = EC_arg(4);
    size = larr.arity();

    IntVarArgs lvars(size);
    res = assign_IntVarArgs_from_ec_array(solver, size, larr, lvars);
    if (res != EC_succeed) return res;

    if (solver->is_first()) cache_domain_sizes(solver);

    try {
      binpacking(*solver, lvars, ivars, sizes);
    }
    CatchAndReportGecodeExceptions

    return EC_succeed;
}

extern "C" VisAtt
int p_g_create_tupleset_handle()
{
    long size;
    if (EC_arg(2).is_long(&size) != EC_succeed) return TYPE_ERROR;

      EC_word ts = EC_arg(1), tarr;

      IntArgs tuple((int)size);

    try {
	TupleSet* tsetp = new(TupleSet);

	while (ts.is_nil() != EC_succeed) {

	    if (ts.is_list(tarr, ts) != EC_succeed) return TYPE_ERROR;		
	    if (tarr.arity() != size) return TYPE_ERROR;
	    int res = assign_IntArgs_from_ec_array(size, tarr, tuple);
	    if (res != EC_succeed) return res;
	    tsetp->add(tuple);
	} 
	tsetp->finalize();

	return unify(EC_arg(3), handle(&tupleset_method, tsetp));

    }
    CatchAndReportGecodeExceptions

}

extern "C" VisAtt
int p_g_post_table()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    long size;
    if (EC_arg(4).is_long(&size) != EC_succeed) return TYPE_ERROR;

    EC_atom emph;
    ExtensionalPropKind epk;
    if (EC_arg(5).is_atom(&emph) != EC_succeed) return TYPE_ERROR;
    if (strcmp(emph.name(), "mem") == 0) epk = EPK_MEMORY;
    else if (strcmp(emph.name(), "speed") == 0) epk = EPK_SPEED;
    else epk = EPK_DEF;

    TupleSet* tsetp;
    if (EC_succeed != get_handle_from_arg(3, &tupleset_method, (void**)&tsetp))
	    return TYPE_ERROR;
    if (tsetp == NULL) return TYPE_ERROR;

    IntConLevel cl;
    Get_Consistency_Level(6, cl);

    try {

      if (solver->is_first()) cache_domain_sizes(solver);

      EC_word varr,  vtail = EC_arg(2);
      IntVarArgs vvars(size);

      do {
	if (vtail.is_list(varr, vtail) != EC_succeed) return TYPE_ERROR;
	if (varr.arity() != size) return TYPE_ERROR;
	int res = assign_IntVarArgs_from_ec_array(solver, size, varr, vvars);
	if (res != EC_succeed) return res;
	    
	extensional(*solver, vvars, *tsetp, epk, cl);
      } while (vtail.is_nil() != EC_succeed);
    }
    CatchAndReportGecodeExceptions

    return EC_succeed;
}

extern "C" VisAtt
int p_g_create_dfa_handle()
{

    EC_word tarr = EC_arg(1), triple, item;
    int n = tarr.arity();
    DFA::Transition ts[n];
    long from, to, sym;

    for (int i=0; i < n; i++) {
	if (tarr.arg(i+1, triple) != EC_succeed) return TYPE_ERROR;
	if (triple.arity() != 3) return TYPE_ERROR;
 
	// these arg positon must correspond to that defined for 
	// struct(dfa_transition(...) in gfd.ecl
	triple.arg(1,item); 
	if (item.is_long(&from) != EC_succeed) return TYPE_ERROR;
	triple.arg(2, item);
	if (item.is_long(&sym) != EC_succeed) return TYPE_ERROR;
	triple.arg(3, item);
	if (item.is_long(&to) != EC_succeed) return TYPE_ERROR;
	ts[i].i_state = (int)from;
	ts[i].symbol = (int)sym;
	ts[i].o_state = (int)to;
    }

    long start;
    if (EC_arg(2).is_long(&start) != EC_succeed) return TYPE_ERROR;

    n = EC_arg(3).arity();
    if (n < 1) return TYPE_ERROR;
    int finals[n];
    
    for (int i=0; i < n; i++) {
	EC_arg(3).arg(i+1, item);
	long j;
	if (item.is_long(&j) != EC_succeed) return TYPE_ERROR;
	finals[i] = (int) j;
    }
	    
    try {

      DFA* dfap = new DFA((int)start, ts, finals);
      return unify(EC_arg(4), handle(&dfa_method, dfap));

    }
    CatchAndReportGecodeExceptions

}

extern "C" VisAtt
int p_g_create_regdfa_handle()
{
    try {

      REG r  = ec2reg(EC_arg(1));
      DFA* dfap = new DFA(r);

      return unify(EC_arg(2), handle(&dfa_method, dfap));
    }
    catch(Ec2gcException) {
	return TYPE_ERROR;
    }
    CatchAndReportGecodeExceptions

}

extern "C" VisAtt
int p_g_post_extensional()
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

	DFA* dfap;
	if (EC_succeed != get_handle_from_arg(3, &dfa_method, (void**)&dfap))
	    return TYPE_ERROR;
	if (dfap == NULL) return TYPE_ERROR;

	if (solver->is_first()) cache_domain_sizes(solver);

	EC_word varr,  vtail = EC_arg(2);
	do {
	    if (vtail.is_list(varr, vtail) != EC_succeed) return TYPE_ERROR;
	    int n = varr.arity();
	    IntVarArgs vvars(n);
	    int res = assign_IntVarArgs_from_ec_array(solver, n, varr, vvars);
	    if (res != EC_succeed) return res;

	    extensional(*solver, vvars, *dfap);

	} while (vtail.is_nil() != EC_succeed);
    }
    catch(Ec2gcException) {
	return TYPE_ERROR;
    }
    CatchAndReportGecodeExceptions

    return EC_succeed;
}


#define GFDSTATSIZE    5

extern "C" VisAtt
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
	    dident selectdid = atm.d;
	    if (selectdid == d_input_order)
		tiebreakselect = INT_VAR_NONE;
	    else if (selectdid == d_ff )
		tiebreakselect = INT_VAR_SIZE_MIN;
	    else if (selectdid == d_antiff )
		tiebreakselect = INT_VAR_SIZE_MAX;
	    else if (selectdid == d_occ )
		tiebreakselect = INT_VAR_DEGREE_MAX;
	    else if (selectdid == d_antiocc )
		tiebreakselect = INT_VAR_DEGREE_MIN;
	    else if (selectdid == d_largest )
		tiebreakselect = INT_VAR_MAX_MAX;
	    else if (selectdid == d_smallest )
		tiebreakselect = INT_VAR_MIN_MIN;
	    else if (selectdid == d_largest_lwb )
		tiebreakselect = INT_VAR_MAX_MIN;
	    else if (selectdid == d_smallest_upb )
		tiebreakselect = INT_VAR_MIN_MAX;
	    else if (selectdid == d_most_constrained_per_val )
		tiebreakselect = INT_VAR_SIZE_DEGREE_MAX;
	    else if (selectdid == d_least_constrained_per_val )
		tiebreakselect = INT_VAR_SIZE_DEGREE_MIN;
	    else if (selectdid == d_max_regret )
		tiebreakselect = INT_VAR_REGRET_MIN_MAX;
	    else if (selectdid == d_max_regret_lwb )
		tiebreakselect = INT_VAR_REGRET_MIN_MAX;
	    else if (selectdid == d_min_regret_lwb )
		tiebreakselect = INT_VAR_REGRET_MIN_MIN;
	    else if (selectdid == d_max_regret_upb )
		tiebreakselect = INT_VAR_REGRET_MAX_MAX;
	    else if (selectdid == d_min_regret_upb )
		tiebreakselect = INT_VAR_REGRET_MAX_MIN;
	    else if (selectdid == d_random )
		tiebreakselect = INT_VAR_RND;
	    else if (selectdid == d_max_wdeg )
		tiebreakselect = INT_VAR_AFC_MAX;
	    else if (selectdid == d_min_wdeg )
		tiebreakselect = INT_VAR_AFC_MIN;
	    else if (selectdid == d_max_wdeg_per_val )
		tiebreakselect = INT_VAR_SIZE_AFC_MAX;
	    else if (selectdid == d_min_wdeg_per_val )
		tiebreakselect = INT_VAR_SIZE_AFC_MIN;
	    else return RANGE_ERROR;
	}

	if (EC_arg(3).is_atom(&atm) != EC_succeed) return TYPE_ERROR;
	dident selectdid = atm.d;
	if (selectdid == d_input_order) varselect = INT_VAR_NONE;
	else if (selectdid == d_ff) varselect = INT_VAR_SIZE_MIN;
	else if (selectdid == d_antiff) varselect = INT_VAR_SIZE_MAX;
	else if (selectdid == d_occ) varselect = INT_VAR_DEGREE_MAX;
	else if (selectdid == d_antiocc) varselect = INT_VAR_DEGREE_MIN;
	else if (selectdid == d_largest) varselect = INT_VAR_MAX_MAX;
	else if (selectdid == d_smallest) varselect = INT_VAR_MIN_MIN;
	else if (selectdid == d_largest_lwb) varselect = INT_VAR_MAX_MIN;
	else if (selectdid == d_smallest_upb) varselect = INT_VAR_MIN_MAX;
	else if (selectdid == d_most_constrained) {
	    varselect = INT_VAR_SIZE_MIN;
	    do_tiebreak = true;
	    tiebreakselect = INT_VAR_DEGREE_MAX;
	} else if (selectdid == d_most_constrained_per_val) varselect = INT_VAR_SIZE_DEGREE_MAX;
	else if (selectdid == d_least_constrained_per_val) varselect = INT_VAR_SIZE_DEGREE_MIN;
	else if (selectdid == d_max_regret) varselect = INT_VAR_REGRET_MIN_MAX;
	else if (selectdid == d_max_regret_lwb) varselect = INT_VAR_REGRET_MIN_MAX;
	else if (selectdid == d_min_regret_lwb) varselect = INT_VAR_REGRET_MIN_MIN;
	else if (selectdid == d_max_regret_upb) varselect = INT_VAR_REGRET_MAX_MAX;
	else if (selectdid == d_min_regret_upb) varselect = INT_VAR_REGRET_MAX_MIN;
	else if (selectdid == d_random) varselect = INT_VAR_RND;
	else if (selectdid == d_max_wdeg) varselect = INT_VAR_AFC_MAX; 
	else if (selectdid == d_min_wdeg) varselect = INT_VAR_AFC_MIN; 
	else if (selectdid == d_max_wdeg_per_val) varselect = INT_VAR_SIZE_AFC_MAX; 
	else if (selectdid == d_min_wdeg_per_val) varselect = INT_VAR_SIZE_AFC_MIN; 
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
	else if (strcmp(atm.name(), "indomain_reverse_enum") == 0) valchoice = INT_VALUES_MAX;
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

	long adaptived = 0, commitd = 0, ithreads;
	double threads = 1.0;
	if ((EC_arg(9).functor(&f) == EC_succeed) 
	    && (strcmp(f.name(), "gfd_control") == 0)) {
	  // these positions must correspond to their definition in
	  // the struct(gdf_control(...)) in gfd.ecl
	    if (EC_argument(EC_arg(9),1).is_long(&commitd) != EC_succeed)
	        commitd = 0;  // commit_distance, arg 1
	    if (EC_argument(EC_arg(9),2).is_long(&adaptived) != EC_succeed)
	        adaptived = 0; // adaptive_distance, arg 2 
	    if (EC_argument(EC_arg(9),3).is_long(&ithreads) == EC_succeed)
	        threads = (double) ithreads; // threads, arg 3 (set as int)
	    else if (EC_argument(EC_arg(9),3).is_double(&threads) != EC_succeed)
	        threads = 1.0;
	}

	solver->clear_snapshot(); // make sure we do cache the current values!
	cache_domain_sizes(solver);
	if (!do_tiebreak) {
	    // time() sets the seeds for the random var/val methods
	    // setting user defined seed to be added later
	    branch(*solver, vars, varselect, valchoice, 
		   VarBranchOptions::time(), ValBranchOptions::time());
	} else {
	    branch(*solver, vars, tiebreak(varselect, tiebreakselect), valchoice,
		   VarBranchOptions::time(), ValBranchOptions::time());
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
	if (threads != 1.0) o.threads = threads;

	GecodeSearch* searchp = new GecodeSearch(solver, o, (unsigned) l, cutoffp, method);

	return unify(EC_arg(10), handle(&gfdsearch_method, searchp));
    }
    CatchAndReportGecodeExceptions
}


extern "C" VisAtt
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
    
extern "C" VisAtt
int p_g_get_gfd_maxint()
{
    return unify(EC_arg(1), EC_word(Int::Limits::max));
}

extern "C" VisAtt
int p_g_get_gfd_minint()
{
    return unify(EC_arg(1), EC_word(Int::Limits::min));
}

extern "C" VisAtt
int p_g_create_idxs_handle()
{
    EC_word idxs = EC_arg(1);
    long size = idxs.arity();

    int* idxarr = (int *)malloc(sizeof(int)*(size+1));
    idxarr[0] = size;

    EC_word w;
    idxs.arg(1, w);
    long i;
    for (int j=1; j <= size; j++) {
	long i;
	EC_word w;

	if (EC_fail == idxs.arg(j, w)) return RANGE_ERROR;
	if (EC_succeed == w.is_long(&i)) {
	    idxarr[j] = (int)i;
	} else return TYPE_ERROR;
    }

    return unify(EC_arg(2), handle(&intptr_method, idxarr));

}

#define Find_Best_IntVar_For(InitialVal, Best, Current, Checkbest) {	\
    Best = InitialVal;					\
    for (int j=1; j <= size; j++) {\
	int idx = (int) idxarr[j];\
	if ((dsize = solver->vInt[idx].size()) > 1) {	\
	    if Checkbest {\
		Best = Current;			\
		bestidx = idx;\
	    }\
	}\
    }\
}

extern "C" VisAtt
int p_g_select()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    int* idxarr;
    if (EC_succeed != get_handle_from_arg(2, &intptr_method, (void **)&idxarr))
	return TYPE_ERROR;

    EC_atom select;
    if (EC_arg(3).is_atom(&select) != EC_succeed) return TYPE_ERROR;
    dident selectdid = select.d;
    int size =  idxarr[0], bestidx = -1, best;   
    unsigned int it, dsize;
    double ft, fbest;

    try {
	if (selectdid == d_ff) {
	    Find_Best_IntVar_For(Int::Limits::max, best, dsize,
				 ((int)dsize < best));
	} else if (selectdid == d_max_wdeg) {
	    Find_Best_IntVar_For(Int::Limits::min, best, it,
				 ((int)(it=solver->vInt[idx].afc()) > best));
	} else if (selectdid == d_min_wdeg) {
	    Find_Best_IntVar_For(Int::Limits::max, best, it,
				 ((int)(it=solver->vInt[idx].afc()) < best));
	} else if (selectdid == d_occ) {
	    Find_Best_IntVar_For(Int::Limits::min, best, it,
				 ((int)(it=solver->vInt[idx].degree()) > best));
	} else if (selectdid == d_antiocc) {
	    Find_Best_IntVar_For(Int::Limits::max, best, it,
				 ((int)(it=solver->vInt[idx].degree()) < best));
	} else if (selectdid == d_input_order) {
	    for (int j=1; j <= size; j++) {	
		int idx =  idxarr[j];
		if (solver->vInt[idx].size() > 1) {
		    bestidx = idx;
		    break;
		}
	    }
	} else if (selectdid == d_smallest) {
	    Find_Best_IntVar_For(Int::Limits::max, best, it,
				 ((int)(it=solver->vInt[idx].min()) < best));
	} else if (selectdid == d_largest) {
	    Find_Best_IntVar_For(Int::Limits::min, best, it,
				 ((int)(it=solver->vInt[idx].max()) > best));
	} else if (selectdid == d_smallest_upb) {
	    Find_Best_IntVar_For(Int::Limits::max, best, it,
				 ((int)(it=solver->vInt[idx].max()) < best));
	} else if (selectdid == d_largest_lwb) {
	    Find_Best_IntVar_For(Int::Limits::min, best, it,
				 ((int)(it=solver->vInt[idx].min()) > best));
	} else if (selectdid == d_max_wdeg_per_val) {
	    Find_Best_IntVar_For(Int::Limits::min, fbest, ft,
				 ((ft=dsize/(double)solver->vInt[idx].afc()) > fbest));
	} else if (selectdid == d_most_constrained) {
	    int best2 = Int::Limits::min;
	    best = Int::Limits::max;
	    for (int j=1; j <= size; j++) {	
		int idx =  idxarr[j];
		if ((dsize = (int)solver->vInt[idx].size()) > 1) {	
		    if ((int)dsize <= best && 
			((it=(int)solver->vInt[idx].degree()),(int)dsize < best || it > best2)) {
			best = dsize;
			best2 = it;
			bestidx = idx;
		    }
		}
	    }

	} else if (selectdid == d_max_regret_lwb) {
	    Find_Best_IntVar_For(Int::Limits::min, best, it,
				 ((int)(it=solver->vInt[idx].regret_min()) > best));
	} else if (selectdid == d_max_regret_upb) {
	    Find_Best_IntVar_For(Int::Limits::min, best, it,
				 ((int)(it=solver->vInt[idx].regret_max()) > best));
	} else if (selectdid == d_min_regret_lwb) {
	    Find_Best_IntVar_For(Int::Limits::max, best, it,
				 ((int)(it=solver->vInt[idx].regret_min()) < best));
	} else if (selectdid == d_min_regret_upb) {
	    Find_Best_IntVar_For(Int::Limits::max, best, it,
				 ((int)(it=solver->vInt[idx].regret_max()) < best));
	} else if (selectdid == d_min_wdeg_per_val) {
	    Find_Best_IntVar_For(Int::Limits::max, fbest, ft,
				 ((ft=dsize/(double)solver->vInt[idx].afc()) < fbest));
	} else if (selectdid == d_most_constrained_per_val) {
	    Find_Best_IntVar_For(Int::Limits::min, fbest, ft,
				 ((ft=dsize/(double)solver->vInt[idx].degree()) > fbest));
	} else if (selectdid == d_least_constrained_per_val) {
	    Find_Best_IntVar_For(Int::Limits::max, fbest, ft,
	    ((ft=dsize/(double)solver->vInt[idx].degree()) < fbest));
	} else if (selectdid == d_antiff) {
	    Find_Best_IntVar_For(Int::Limits::min, best, dsize,
				 ((int)dsize > best));
	} else
	    return RANGE_ERROR;
    }
    CatchAndReportGecodeExceptions

    if (bestidx > 0) return unify(EC_arg(4), EC_word((long)bestidx));
    else return EC_fail; // No var selected

}


extern "C" VisAtt
int p_g_get_var_domain_handle()
{
    GecodeSpace** solverp;
    GecodeSpace* solver;

    if (EC_succeed != get_handle_from_arg(1, &gfd_method, (void**)&solverp))
	return TYPE_ERROR;
    solver = *solverp;
    if (solver == NULL) return TYPE_ERROR;

    long xidx;
    if (EC_succeed != EC_arg(2).is_long(&xidx)) return(TYPE_ERROR);
    
    IntVarRanges r(solver->vInt[(int)xidx]);

    IntSet* setp = new IntSet(r);

    return unify(EC_arg(3), handle(&domain_method, setp));

}

extern "C" VisAtt
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
    Assign_IntVar(3, xidx, x);
    
    long yidx;
    IntVar y;
    Assign_IntVar(4, yidx, y);

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

extern "C" VisAtt
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

extern "C" VisAtt
int p_g_gecode_version()
{
  return unify(EC_arg(1), EC_word(GECODE_VERSION));
}

