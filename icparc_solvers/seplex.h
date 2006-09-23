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
 * Contributor(s): Joachim Schimpf, Kish Shen and Andrew Eremin, IC-Parc
 * 
 * END LICENSE BLOCK */

#define PROBLEM_LP	0	/* these correspond to cplex_problem_code/2 */
#define PROBLEM_MIP	1
#define PROBLEM_QP	2
#define PROBLEM_MIQP    3 
#define PROBLEM_FIXEDL	4
#define PROBLEM_FIXEDQ	5 
#define PROBLEM_RELAXEDL  6
#define PROBLEM_RELAXEDQ  7

#define METHOD_NONE    (-1)     /* correspond to cplex_crossover_code/1 */
#define METHOD_PRIMAL	0	/* these correspond to cplex_method_code/2 */
#define METHOD_DUAL	1
#define METHOD_NET      2
#define METHOD_BAR      3
#define METHOD_SIFT     4
#define METHOD_DEFAULT	5

#define SENSE_MIN	1	/* these correspond to cplex_objsense/2 */
#define SENSE_MAX	(-1)

#define IsMIPProb(t) ((t) == PROBLEM_MIP || (t) == PROBLEM_MIQP)
#define IsQPProb(t)  ((t) == PROBLEM_QP || (t) == PROBLEM_MIQP)

#ifdef COIN

# define CPXLPptr COINprob *

typedef enum
{
    state_success,
    state_fail,
    state_mipsemisucc,
    state_mipsemifail,
    state_lpaborted,
    state_unbounded,
    state_unknown
} state_t;


#endif

#if defined(COIN) || defined(XPRESS)

/* position of stream types in solver_stream[].
   (for XPRESS, = message type - 1 */
#define LogType		0   /* log */
#define ResType		1   /* result */
#define WrnType		2   /* warning */
#define ErrType		3   /* error */

#endif

/*
 * Our extended problem descriptor
 */

typedef struct {

    int		descr_state;

    CPXLPptr	lp;		/* CPLEX/XPRESS handle */
    CPXLPptr    lpcopy;         /* copy of problem to allow MIP modifications */
#ifdef USE_PROBLEM_ARRAY
    int		matno;		/* XPRESS matrix number if descriptor not
                                   current (or matrix number for logging) */
#endif

    int         start_mac;       /* initial number of valid columns */
    /* here we collect the input parameters for cplex */
    int		prob_type;
    int		presolve;       /* presolve for problem */
    int		sense;
    int		mac;		/* number of  valid colunms */
    int		macadded;	/* columns already added to problem */
    int		mar;		/* number of valid rows */
    int		matnz;		/* used nonzeros in column-wise nz arrays */
    int		macsz;          /* size of column buffer arrays */
    int		marsz;
    double	*rhsx;		/* rhs coeff. initial matrix + added rows */
    char	*senx;		/* constraint sense initial matrix + added rows */
    /* column-wise representation of matrix coefficients
       initial matrix + added columns
    */
    int		*matbeg;	/* index into matind for each column */
    int		*matcnt;	/* non-zeros count in each column */
    int		*matind;	/* non-zeros row indicies  */
    double	*matval;	/* non-zeros coefficient  */

    /* column information initial matrix + added columns */
    double	*bdl;		/* [macsz growable] */
    double	*bdu;		/* [macsz growable] */
    char        dirtybdflag;    /* bit-wise flag to indicate "dirty" bound arrays */
    char	*ctype;		/* column type */
    double	*objx;		/* objective coefficients */
    char	*qgtype;	/* types of non-continuous vars */
    int		*mgcols;	/* col-nos of non-continuous vars */
#ifdef XPRESS
    char        *probname;      /* problem name (for XPRESS files) */
    int		ngents;		/* number of non-continuous vars */
# ifdef SOLVE_MIP_COPY
    int         copystatus;     /* status of copy of problem */
# endif
#endif

    int		sossz;		/* size of sos */
    int		nsos;		/* number of SOSs */
    int		sosnzsz;	/* size of sos arrays */
    int		nsosnz;		/* total number of sos members */
    char	*sostype;	/* [sosnzsz] sos type '1' or '2' */
    int		*sosbeg;	/* [sossz+1 !! ] index into sosind[] */
    int		*sosind;	/* [sosnzsz] col-no */
    double	*sosref;	/* [sosnzsz] */

    double	*rngval;
    char	**cname;
    char	*cstore;
    char	**rname;
    char	*rstore;
    unsigned	cstorsz;
    unsigned	rstorsz;

    /* A set of growable arrays, used as temporary storage for
     * - setting up the quadratic objective coefficients
     * - modifying linear objective coefficients
     * - modifying quadratic objective coefficients
     * These all use a sparse representation for columns
     */
    int		cb_sz;		/* size of cb_... arrays */
    int		cb_cnt;		/* next index to be used */
    int		*cb_index;	/* [cb_sz] */
    int		*cb_index2;	/* [cb_sz] for quad obj setup only (may be NULL) */
    double	*cb_value;	/* [cb_sz] coeff value */

    /* to add constraints (and retrieve constraints) */
    int		nr_sz;		/* size of new row arrays */
    int		nnz_sz;		/* size of new nonzero arrays */
    int		nr;		/* next new row index */
    int		nnz;		/* next new nonzero index */
    int		*rmatbeg;	/* [nr_sz] */
    int		*rmatind;	/* [nnz_sz] */
    double	*rmatval;	/* [nnz_sz] */
    int		numsz;		/* auxiliary array for delrows/delcols */
    int		*numbers;	/* [numsz], contains numbers 0..numsz-1 */
    int		*zeroes;	/* [numsz], contains int zeroes */
    double      *dzeroes;       /* [numsz], contains double zeroes */

    /* cutpools - *2 variants are conditional */
/*    int		cp_nr_sz;	/* size of cutpool row arrays */
    int		cp_nr_sz2;	/* size of cond cutpool row arrays */
/*    int		cp_nz_sz;	/* size of cutpool nonzero arrays */
    int		cp_nz_sz2;	/* size of cond cutpool nonzero arrays */
/*    int		cp_nr;		/* next cutpool row index */
    int		cp_nr2;		/* next cond cutpool row index */
/*    int		cp_nnz;		/* next cuypool nonzero index */
    int		cp_nnz2;	/* next cond cutpool nonzero index */
    int         cp_nact2;       /* no. of active constraints in last solve */
/*    int		*cp_rmatbeg;	/* [cp_nr_sz] */
    int		*cp_rmatbeg2;	/* [cp_nr_sz2] */
/*    int		*cp_rmatind;	/* [cp_nz_sz] */
    int		*cp_rmatind2;	/* [cp_nz_sz2] */
/*    double	*cp_rmatval;	/* [cp_nz_sz] */
    double	*cp_rmatval2;	/* [cp_nz_sz2] */
/*    double	*cp_rhsx;	/* [cp_nr_sz] cutpool rhs coeffs */
    double	*cp_rhsx2;	/* [cp_nr_sz2] cond  cutpool rhs coeffs */
/*    char	*cp_senx;	/* [cp_nr_sz] cutpool constraint */
    char	*cp_senx2;	/* [cp_nr_sz2] cond cutpool constraint */
    char        *cp_active2;    /* [cp_nr_sz2] default state */
    char        *cp_initial_add2;  /* [cp_nr_sz2] added initially or not */
    /* named conditional cutpools */
    int         cp_npools2;
    int         cp_npools_sz2;
    int         *cp_pools_max2; /* [cp_npools_sz2] max cstr in each pool */
    int         *cp_pools_sz2;  /* [cp_npools_sz2] size for each pool */
    int         **cp_pools2;    /* [cp_npools_sz2] of int arrays of idxs */
    char        **cp_names2;    /* [cp_npools_sz2] of cutpool names */

    double      objval;         /* the most recent objective value */
    /* the most recent return status */
    int		sol_state;

    /* statistics for most recent solver call */
    int         sol_itcnt;
    int         sol_nodnum;

    /* (global) statistics */
    int		optimum_ctr;
    int		infeas_ctr;
    int		abort_ctr;
} lp_desc;

