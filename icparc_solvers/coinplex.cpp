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
 * Copyright (C) 2006 Cisco Systems, Inc.  All Rights Reserved.
 * 
 * Contributor(s): Kish Shen, CrossCore Optimization. 
 * 
 * END LICENSE BLOCK */

// NOECLIPSE for debugging logged calls without ECLiPSe. 
//#define NOECLIPSE
#undef NOECLIPSE
// LOG_CALLS defined when generating logged calls, but not when linking
// with the debugging logged call C program
//#define LOG_CALLS
#undef LOG_CALLS

// this is needed to keep seplex.c's lpd consistent during LOG_CALLS
#if defined(LOG_CALLS) 
# define USE_PROBLEM_ARRAY
#endif

#ifdef COIN_USE_CLP
#define GetCbcSolver(lp) ((lp)->mipmodel)
#define CBC_IS_MIPSOLVER

#include "OsiSolverInterface.hpp"
#include "OsiClpSolverInterface.hpp"
#include "CbcModel.hpp"
// Cut Generation and Heuristics
#include "CbcCutGenerator.hpp"
#include "CbcStrategy.hpp"
#include "CbcHeuristic.hpp"
#include "CbcHeuristicLocal.hpp"
#include "CbcBranchUser.hpp"
#include "CbcBranchActual.hpp"
#include "CbcCompareUser.hpp"
#include "CglGomory.hpp"
#include "CglProbing.hpp"
#include "CglKnapsackCover.hpp"
//#include "CglOddHole.hpp"
#include "CglRedSplit.hpp"
#include "CglClique.hpp"
#include "CglFlowCover.hpp"
#include "CglMixedIntegerRounding2.hpp"
// Preprocessing
#include "CglPreProcess.hpp"
// Barrier
#include "ClpInterior.hpp"
#include "ClpSimplex.hpp"
#ifdef UFL_BARRIER
// Use the University of Florida AMD library for pre-ordering sparse matrices
// This considerably improves the Barrier performance. Clp must also be 
// compiled with UFL_BARRIER support. Note that the UFL AMD library is not
// part of COIN-OR and is distributed under GNU LGPL license. 
#include "ClpCholeskyUfl.hpp"
#endif
#include "ClpCholeskyDense.hpp"
typedef OsiClpSolverInterface OsiXxxSolverInterface;

#endif

#ifdef COIN_USE_CBC
#define GetCbcSolver(lp) ((lp)->Solver->getModelPtr())
#define CBC_IS_MIPSOLVER

#include "OsiCbcSolverInterface.hpp"
#include "OsiClpSolverInterface.hpp"
#undef COIN_USE_CLP
#include "CbcModel.hpp"
// Cut Generation and Heuristics
#include "CbcCutGenerator.hpp"
#include "CbcStrategy.hpp"
#include "CbcHeuristic.hpp"
#include "CbcHeuristicLocal.hpp"
#include "CbcBranchUser.hpp"
#include "CbcCompareUser.hpp"
#include "CglGomory.hpp"
#include "CglProbing.hpp"
#include "CglKnapsackCover.hpp"
//#include "CglOddHole.hpp"
#include "CglRedSplit.hpp"
#include "CglClique.hpp"
#include "CglFlowCover.hpp"
#include "CglMixedIntegerRounding2.hpp"
// Preprocessing
#include "CglPreProcess.hpp"
// Barrier
#include "ClpInterior.hpp"
#include "ClpSimplex.hpp"
#ifdef UFL_BARRIER
#include "ClpCholeskyUfl.hpp"
#endif
#include "ClpCholeskyDense.hpp"
#include "ClpCholeskyWssmp.hpp"

typedef OsiCbcSolverInterface OsiXxxSolverInterface;

#endif

#ifdef COIN_USE_SYM

#include "OsiSymSolverInterface.hpp"

typedef OsiSymSolverInterface OsiXxxSolverInterface;

#endif

#ifdef COIN_USE_GLPK

#include "OsiGlpkSolverInterface.hpp"

typedef OsiGlpkSolverInterface OsiXxxSolverInterface;


#endif

#include "CoinPackedVector.hpp"
#include "CoinPackedMatrix.hpp"
#include "CoinBuild.hpp"
#include "CoinError.hpp"
#include "CoinMessageHandler.hpp"
#include <stdio.h>

#include <fstream>

// must be defined before seplex.h
typedef struct {
    OsiXxxSolverInterface * Solver;
    char** varnames;     /* for names of variables (columns) */
    unsigned int vnsize; /* number of variable names */
    char notfirst; /* has problem been solved? */
    /* solver specific */
#ifdef COIN_USE_CLP
    char mipIsShared; /* 1 if shared with Solver, 0 if copied */
    CbcModel* mipmodel;
    ClpInterior* interiormodel;
    //CbcObject** mipobjects; // information such as SOS to be added to mipmodel 
    // int nsos; // number of SOSs
    double timeout;
#endif
} COINprob;

#include "seplex.h"


// utility to check if a file exists (and is readable)
bool fileExists(const char* file)
{
    std::fstream fin;
    fin.open(file, std::ios::in);
    if (fin.is_open() )
    {
	fin.close();
	return true;
    }
    fin.close();
    return false;
}

/*****************************************************************************
 * Handlers                                                                  *
 *****************************************************************************/

#ifndef NOECLIPSE /* normal */

#include "external.h"

extern "C"
void eclipse_out(int msgtype, const char* message);

#else /* debug without ECLiPSe */

void eclipse_out(int msgtype, const char* message)
{
    printf(message);
    printf("\n");
}
#endif

class DerivedHandler : public CoinMessageHandler 
{
public:
    virtual int print();
};

int DerivedHandler::print()
{
    int id = currentMessage().externalNumber();
    int mtype = (id<3000 ? LogType : (id<6000 ? WrnType : ErrType));
    eclipse_out(mtype, messageBuffer());

    return 0;
}

void coin_error_handler(CoinError &e)
{
    eclipse_out(ErrType, e.message().c_str());
}

/*************************************************************************
 * Solver Specific Code                                                  *
 *************************************************************************/

#if defined(COIN_USE_CLP) || defined(COIN_USE_CBC)

/* these parameters must correspond to their COIN Solver* declarations
   in eplex_params.h
*/
#define NUMSOLVERINTPARAMS 		2
#define NUMSOLVERDBLPARAMS		4

static CbcModel::CbcIntParam cbc_iparam[] = {CbcModel::CbcMaxNumNode, 
					     CbcModel::CbcMaxNumSol};

static CbcModel::CbcDblParam cbc_dparam[] = {CbcModel::CbcIntegerTolerance,
					     CbcModel::CbcAllowableGap,
					     CbcModel::CbcAllowableFractionGap,
					     CbcModel::CbcCutoffIncrement};


int coin_branchAndBound(lp_desc* lpd)
{

    OsiSolverInterface* mipsolver;
    lpd->lp->Solver->initialSolve();
    bool preprocess_failed = false;
    CglPreProcess process;

    // copying original bounds before presolve -- Cbc's integer presolve and
    // MIP branch-and-bound can fix some column bounds. The original bounds
    // needs to be restored before continuing
    int mac = lpd->lp->Solver->getNumCols();
    double* ups = new double[mac];
    double* lws = new double[mac];
    memcpy(ups, lpd->lp->Solver->getColUpper(), mac*sizeof(double));
    memcpy(lws, lpd->lp->Solver->getColLower(), mac*sizeof(double));

    if (lpd->presolve)
    {
	DerivedHandler* cglMessageHandler = new DerivedHandler;
	process.passInMessageHandler(cglMessageHandler);
	process.messageHandler()->setLogLevel(1);
	mipsolver = process.preProcess(*lpd->lp->Solver, false, 5);
	if (mipsolver == NULL)
	{// preprocessing failed -- problem infeasible 
	 //  but we can't simply return, as we need a valid mipmodel
	    mipsolver = lpd->lp->Solver;
	    // restore original bounds
	    for (int i=0; i<mac; i++) 
		lpd->lp->Solver->setColBounds(i,lws[i],ups[i]);
	    preprocess_failed = true;
	}
	else 
	    mipsolver->resolve();
    } 
    else
    {
	mipsolver = lpd->lp->Solver;
    }

# ifdef COIN_USE_CBC
    CbcModel* model = lpd->lp->Solver->getModelPtr();
# endif

# ifdef COIN_USE_CLP
    {
	int* iparams; 
	double* dparams; 

	if (lpd->lp->mipmodel != NULL) 
	{
	    // copy the parameters
	    iparams = new int[NUMSOLVERINTPARAMS];
	    dparams = new double[NUMSOLVERDBLPARAMS];

	    for (int i=0; i<NUMSOLVERINTPARAMS; i++)
		iparams[i] = lpd->lp->mipmodel->getIntParam(cbc_iparam[i]);
	    for (int i=0; i<NUMSOLVERDBLPARAMS; i++)
		dparams[i] = lpd->lp->mipmodel->getDblParam(cbc_dparam[i]);
	    if (!lpd->lp->mipIsShared)
		delete lpd->lp->mipmodel;
	}
	
	lpd->lp->mipmodel = new CbcModel(static_cast<OsiSolverInterface &>(*mipsolver));
	if (lpd->presolve && !preprocess_failed)
	{// preprocessed mipmodel, not shared with Solver
	    lpd->lp->mipIsShared = 0;
	    for (int i=0; i<NUMSOLVERINTPARAMS; i++)
		lpd->lp->mipmodel->setIntParam(cbc_iparam[i], iparams[i]);
	    for (int i=0; i<NUMSOLVERDBLPARAMS; i++)
		lpd->lp->mipmodel->setDblParam(cbc_dparam[i], dparams[i]);
	    delete [] iparams;
	    delete [] dparams;

	}
	else
	{
	    lpd->lp->mipIsShared = 1;
	}
    }
	
    CbcModel* model = lpd->lp->mipmodel;
    if (lpd->lp->timeout > 0) model->setMaximumSeconds(lpd->lp->timeout);
# endif
# ifdef COIN_USE_CBC
    model->saveReferenceSolver();
# endif
    DerivedHandler* mipMessageHandler = new DerivedHandler;
    model->passInMessageHandler(mipMessageHandler);
    model->messageHandler()->setLogLevel(1);
    model->solver()->setHintParam(OsiDoReducePrint,true,OsiHintTry);
    if (preprocess_failed)
    {/* if preprocessing failed, just do branch and bound now to get a MIP
	result and return
     */
	model->branchAndBound();
	return 0;
    }
    model->initialSolve();


    CglProbing generator1;
    generator1.setUsingObjective(true);
    generator1.setMaxPass(1);
    generator1.setMaxPassRoot(5);
    // Number of unsatisfied variables to look at
    generator1.setMaxProbe(10);
    generator1.setMaxProbeRoot(1000);
    // How far to follow the consequences
    generator1.setMaxLook(50);
    generator1.setMaxLookRoot(500);
    // Only look at rows with fewer than this number of elements
    generator1.setMaxElements(200);
    generator1.setRowCuts(3);

    CglGomory generator2;
    // try larger limit
    generator2.setLimit(300);

    CglKnapsackCover generator3;
    // Decided too slow
    //CglOddHole generator4;
    //generator4.setMinimumViolation(0.005);
    //generator4.setMinimumViolationPer(0.00002);
    // try larger limit
    //generator4.setMaximumEntries(200);

    //CglRedSplit generator4;
    // try larger limit
    //generator4.setLimit(200);

    CglClique generator5;
    generator5.setStarCliqueReport(false);
    generator5.setRowCliqueReport(false);

    CglMixedIntegerRounding2 mixedGen;
    CglFlowCover flowGen;
  
    // Add in generators
    // Experiment with -1 and -99 etc
    model->addCutGenerator(&generator1,-5,"Probing");
    model->addCutGenerator(&generator2,-5,"Gomory");
    model->addCutGenerator(&generator3,-5,"Knapsack");
    //model->addCutGenerator(&generator4,-1,"RedSplit");
    model->addCutGenerator(&generator5,-5,"Clique");
    model->addCutGenerator(&flowGen,-5,"FlowCover");
    model->addCutGenerator(&mixedGen,-5,"MixedIntegerRounding");
    // Say we want timings
    int numberGenerators = model->numberCutGenerators();
    int iGenerator;
    for (iGenerator=0;iGenerator<numberGenerators;iGenerator++) {
	CbcCutGenerator * generator = model->cutGenerator(iGenerator);
	generator->setTiming(true);
    }

    OsiXxxSolverInterface * osiclp = dynamic_cast< OsiXxxSolverInterface*> (model->solver());

    // go faster stripes
    if (osiclp) {
	// Turn this off if you get problems
	// Used to be automatically set
	osiclp->setSpecialOptions(128);
	if (osiclp->getNumRows()<300&&osiclp->getNumCols()<500) {
	    //osiclp->setupForRepeatedUse(2,0);

	    osiclp->setupForRepeatedUse(0,0);
	}
    }
    CbcRounding heuristic1(*model);
    model->addHeuristic(&heuristic1);

    // And local search when new solution found

    CbcHeuristicLocal heuristic2(*model);
    model->addHeuristic(&heuristic2);

    // Redundant definition of default branching (as Default == User)
    CbcBranchUserDecision branch;
    model->setBranchingMethod(&branch);

    // Definition of node choice
    CbcCompareUser compare;
    model->setNodeComparison(compare);

    model->initialSolve();

    // Could tune more
    double objValue = model->solver()->getObjSense()*model->solver()->getObjValue();
    double minimumDropA=CoinMin(1.0,fabs(objValue)*1.0e-3+1.0e-4);
    double minimumDrop= fabs(objValue)*1.0e-4+1.0e-4;
    model->setMinimumDrop(minimumDrop);

    if (model->getNumCols()<500) {
	model->setMaximumCutPassesAtRoot(-100); // always do 100 if possible
    } else if (model->getNumCols()<5000) {
	model->setMaximumCutPassesAtRoot(100); // use minimum drop
    } else {
	model->setMaximumCutPassesAtRoot(20);
    }
    model->setMaximumCutPasses(10);
    //model.setMaximumCutPasses(2);

    // Switch off strong branching if wanted
    // model.setNumberStrong(0);
    // Do more strong branching if small
    if (model->getNumCols()<5000)
	model->setNumberStrong(10);
    model->setNumberStrong(20);
    //model->setNumberStrong(5);
    model->setNumberBeforeTrust(5);

    model->solver()->setIntParam(OsiMaxNumIterationHotStart,100);

    // Switch off most output
    /*
    if (model->getNumCols()<3000) {
	model->messageHandler()->setLogLevel(1);

	//model.solver()->messageHandler()->setLogLevel(0);
    } else {
	model->messageHandler()->setLogLevel(2);
	model->solver()->messageHandler()->setLogLevel(1);
    }
    */
    //model->messageHandler()->setLogLevel(2);
    //model->solver()->messageHandler()->setLogLevel(2);
    model->setPrintFrequency(500);

    // Default strategy will leave cut generators as they exist already
    // so cutsOnlyAtRoot (1) ignored
    // numberStrong (2) is 5 (default)
    // numberBeforeTrust (3) is 5 (default is 0)
    // printLevel (4) defaults (0)
    CbcStrategyDefault strategy(true,5,5);
    // Set up pre-processing to find sos if wanted
    if (lpd->presolve) strategy.setupPreProcessing(2);
    //strategy.setupPrinting(*model, 0);
    model->setStrategy(strategy);

    // Do complete search
    //CbcModel *model2 = model.integerPresolve();
    //model2->branchAndBound();
    model->branchAndBound();

    //if (model->isProvenOptimal()) fprintf(stdout, "optimal!\n");
    //if (model->isProvenInfeasible()) fprintf(stdout, "infeasible\n");
    //if (model->isSecondsLimitReached()) fprintf(stdout, "timeouted\n");
    if (mac != lpd->mac) 
    {
	eclipse_out(ErrType, "Eplex Error: columns number in original model does not match eplex.\n");
	return -1;
    }

    if (lpd->presolve) process.postProcess(*model->solver());
# ifdef COIN_USE_CLP
    else 
	lpd->lp->Solver = dynamic_cast< OsiXxxSolverInterface*>(model->solver());
# endif

    if (lpd->prob_type == PROBLEM_FIXEDL && 
	lpd->lp->Solver->isProvenOptimal())
    {
	/* integer col bounds are already fixed to their sol values */
	lpd->lp->Solver->initialSolve();
    }

    // reset the column bounds (undo fixed bounds for integer cols)
    for (int i=0; i<mac; i++) 
	lpd->lp->Solver->setColBounds(i,lws[i],ups[i]);

    //std::cout<< lpd->lp->Solver->getIterationCount();

    return 0;
}

int coin_solveLinear(lp_desc* lpd, int meth, int auxmeth)
{
    switch (meth)
    {
    case METHOD_BAR:
	{
#ifdef UFL_BARRIER
	ClpCholeskyUfl* cholesky = new ClpCholeskyUfl(-1);
#else
	ClpCholeskyBase* cholesky = new ClpCholeskyDense();
#endif
	ClpModel* clpmodel = lpd->lp->Solver->getModelPtr();
	lpd->lp->interiormodel = new ClpInterior;
	lpd->lp->interiormodel->borrowModel(*clpmodel);
	// Quadratic QP aparently needs a KKT factorization
	if (lpd->prob_type == PROBLEM_QP) cholesky->setKKT(true);
	lpd->lp->interiormodel->setCholesky(cholesky);
	lpd->lp->interiormodel->primalDual();
	// Barrier done

	//lpd->lp->interiormodel->checkSolution();
	if (lpd->lp->interiormodel->isProvenOptimal()
	    // infeasibility not correctly detected by ClpInterior, so need
	    // the next test to make sure solution is feasible
	    && lpd->lp->interiormodel->sumPrimalInfeasibilities() < 1e-5)
	{
	    // Do crossover if optimal...
	    ClpSimplex model2(*lpd->lp->interiormodel);
	    // make sure no status left
	    model2.createStatus();
	    switch (auxmeth)
		{
		case METHOD_PRIMAL:
		case METHOD_DEFAULT:
		    model2.primal(1);
		    break;
		case METHOD_DUAL:
		    model2.dual(1);
		    break;
		case METHOD_NONE:
		    break;
		}
	}
	lpd->lp->interiormodel->returnModel(*clpmodel);
	}

	break;
    case METHOD_PRIMAL:
    case METHOD_DUAL:
    case METHOD_DEFAULT:
	if (lpd->lp->notfirst)
	{
	    lpd->lp->Solver->resolve();
	}
	else
	{
	    //lpd->lp->Solver->writeLp("cointest");
	    lpd->lp->Solver->initialSolve();
	    lpd->lp->notfirst= 1;
	    /* timeout for CLP not turned off here, but only before 
	       branchAndBound() is called, because the timeout setting is 
	       needed for detecting if timeout happened or not
	    */ 
	}
	break;
    }
}


extern "C"
int coin_set_timeout(COINprob* lp, double timeout)
{
#ifdef COIN_USE_CLP
    if (timeout > 0) lp->timeout = timeout;
#endif
    
    return 0;
}


#else 

int coin_branchAndBound(lp_desc *lpd)
{
    try
    {
	lpd->lp->Solver->branchAndBound();
	if (lpd->prob_type == PROBLEM_FIXEDL && 
	    lpd->lp->Solver->isProvenOptimal())
	{
	    int mac = lpd->lp->Solver->getNumCols();
	    double* ups = new double[mac];
	    double* lws = new double[mac];
	    memcpy(ups, lpd->lp->Solver->getColUpper(), mac*sizeof(double));
	    memcpy(lws, lpd->lp->Solver->getColLower(), mac*sizeof(double));
	    //fix
	    lpd->lp->Solver->initialSolve();

	}
    }
    catch (CoinError e)
    {
	coin_error_handler(e);
	return -1;
    }

    return 0;
}

int coin_solveLinear(lp_desc* lpd, int meth, int aux_meth)
{
#ifndef COIN_USE_SYM
    // with OsiSym, resolve seem to ignore added constraints
    if (lpd->lp->notfirst)
    {
	lpd->lp->Solver->resolve();
    }
    else
#endif
    {
	//lpd->lp->Solver->writeLp("cointest");
	lpd->lp->Solver->initialSolve();
	lpd->lp->notfirst= 1;
	/* timeout for CLP not turned off here, but only before 
	   branchAndBound() is called, because the timeout setting is 
	   needed for detecting if timeout happened or not
	*/ 
    }
}

extern "C"
int coin_set_timeout(COINprob* lp, double timeout)
{
    // Osi does not provide a generic timeout, do nothing by default
    return 0;
}

#endif
#ifdef COIN_USE_CLP
extern "C"
int coin_get_solver_dblparam(COINprob* lp, int key, double &value)
{
    if (lp->mipmodel == NULL) return -1; // should not happen
    value = lp->mipmodel->getDblParam(cbc_dparam[key]);

    return 0;
}

extern "C"
int coin_get_solver_intparam(COINprob* lp, int key, int &value)
{
    if (lp->mipmodel == NULL) return -1; // should not happen
    value = lp->mipmodel->getIntParam(cbc_iparam[key]);

    return 0;
}

extern "C"
int coin_set_solver_intparam(COINprob* lp, int key, int value)
{
    if (lp->mipmodel == NULL) return -1;
    lp->mipmodel->setIntParam(cbc_iparam[key], value);

    return 0;
}

extern "C"
int coin_set_solver_dblparam(COINprob* lp, int key, double value)
{
    if (lp->mipmodel == NULL) return -1;
    lp->mipmodel->setDblParam(cbc_dparam[key], value);

    return 0;
}

void coin_set_solver_outputs(OsiXxxSolverInterface* Solver)
{
    DerivedHandler* solMessageHandler = new DerivedHandler;
    Solver->getModelPtr()->passInMessageHandler(solMessageHandler);
}

void coin_free_solver_handlers(OsiXxxSolverInterface* Solver)
{
}


#elif defined(COIN_USE_CBC)

extern "C"
int coin_get_solver_dblparam(COINprob* lp, int key, double &value)
{
    CbcModel* model = lp->Solver->getModelPtr();
    
    value = model->getDblParam(cbc_dparam[key]);

    return 0;
}

extern "C"
int coin_get_solver_intparam(COINprob* lp, int key, int &value)
{
    CbcModel* model = lp->Solver->getModelPtr();
    
    value = model->getIntParam(cbc_iparam[key]);

    return 0;
}

extern "C"
int coin_set_solver_intparam(COINprob* lp, int key, int value)
{
    CbcModel* model = lp->Solver->getModelPtr();

    model->setIntParam(cbc_iparam[key], value);
    return 0;
}

extern "C"
int coin_set_solver_dblparam(COINprob* lp, int key, double value)
{
    CbcModel* model = lp->Solver->getModelPtr();

    model->setDblParam(cbc_dparam[key], value);
    return 0;
}

void coin_set_solver_outputs(OsiXxxSolverInterface* Solver)
{
    CbcModel* model = Solver->getModelPtr();
    DerivedHandler* cbcMessageHandler = new DerivedHandler;
    model->passInMessageHandler(cbcMessageHandler);
    model->messageHandler()->setLogLevel(1);

    OsiClpSolverInterface* clp = 
	dynamic_cast< OsiClpSolverInterface*> (Solver->getRealSolverPtr());
    if (clp != NULL)
    {/* != NULL if using CLP */
	DerivedHandler* clpMessageHandler = new DerivedHandler;
	clp->passInMessageHandler(clpMessageHandler);
	clp->messageHandler()->setLogLevel(1);
    }
}

void coin_free_solver_handlers(OsiXxxSolverInterface* Solver)
{
    CbcModel* model = Solver->getModelPtr();
    delete model->messageHandler();

    OsiClpSolverInterface* clp = 
	dynamic_cast< OsiClpSolverInterface*> (Solver->getRealSolverPtr());
    if (clp != NULL) 
    {/* != NULL if using CLP */
	delete clp->messageHandler();
    }
}

#else

extern "C"
int coin_get_solver_dblparam(COINprob* lp, int key, double &value)
{
    return -1;
}

extern "C"
int coin_set_solver_dblparam(COINprob* lp, int key, double value)
{
    return -1;
}

extern "C"
int coin_get_solver_intparam(COINprob* lp, int key, int &value)
{
    return -1;
}

extern "C"
int coin_set_solver_intparam(COINprob* lp, int key, int value)
{
    return -1;
}

extern "C"
int coin_set_solver_methods(lp_desc* lpd, int method, int auxmethod, 
			   int node_meth, int node_auxmeth)
{
    return -1;
}

void coin_set_solver_outputs(OsiXxxSolverInterface* Solver)
{
}

void coin_free_solver_handlers(OsiXxxSolverInterface* Solver)
{
}


#endif


/*****************************************************************************
 * Generic OSI/Coin Code                                                     *
 *****************************************************************************/


extern "C"
int coin_getrhs(COINprob* lp, double* rhs, int start, int end)
{
    const double *rhs0 = lp->Solver->getRightHandSide();
    int j=0;

    if (start < 0) return -1;
    if (end >= lp->Solver->getNumRows()) return -1;

    for (int i=start; i<=end; i++)
    {
	rhs[j++] = rhs0[i];
    }
    return 0;
}

extern "C"
int coin_getrowsense(COINprob* lp, char* rsense, int start, int end)
{
    const char* rsense0 = lp->Solver->getRowSense();
    int j=0;

    if (start < 0) return -1;
    if (end >= lp->Solver->getNumRows()) return -1;

    for (int i=start; i<=end; i++)
    {
	// sense type is the same for OSI and CPLEX/Xpress
	rsense[j++] = rsense0[i];
    }
    return 0;
}

extern "C"
int coin_getlb(COINprob* lp, double* lb, int start, int end)
{
    const double* lb0 = lp->Solver->getColLower();
    int j=0;

    if (start < 0) return -1;
    if (end >= lp->Solver->getNumCols()) return -1;

    for (int i=start; i<=end; i++)
    {
	lb[j++] = lb0[i];
    }
    return 0;
}

extern "C"
int coin_getub(COINprob* lp, double* ub, int start, int end)
{
    const double* ub0 = lp->Solver->getColUpper();
    int j=0;

    if (start < 0) return -1;
    if (end >= lp->Solver->getNumCols()) return -1;

    for (int i=start; i<=end; i++)
    {
	ub[j++] = ub0[i];
    }
    return 0;
}

extern "C"
int coin_getcoltype(COINprob* lp, char* ctype, int start, int end)
{
    int j=0;

    if (start < 0) return -1;
    if (end >= lp->Solver->getNumCols()) return -1;

    for (int i=start; i<=end; i++)
    {
	if (lp->Solver->isContinuous(i)) ctype[j++] = 'C';
	else if (lp->Solver->isInteger(i)) ctype[j++] = 'I';
	else if (lp->Solver->isBinary(i)) ctype[j++] = 'B';
	else return -1;  /* unknown type -- error! */
    }
    return 0;
}

extern "C"
int coin_chgcoltype(COINprob* lp, int cnt, int* idxs, char* ctype)
{

    for (int i=0; i<cnt; i++)
    {
	int j=idxs[i];
	if (j < 0 || j >= lp->Solver->getNumCols()) return -1;

	if (ctype[i] == 'C')      lp->Solver->setContinuous(j);
	else if (ctype[i] == 'I') lp->Solver->setInteger(j);
	else if (ctype[i] == 'B') lp->Solver->setInteger(j); // no setBinary()
	else return -1;  /* unknown type -- error! */
    }
    return 0;
}

extern "C"
int coin_chgbds(COINprob* lp, int cnt, int* idxs, char* lu, double* bd)
{

    for (int i=0; i<cnt; i++)
    {
	int j=idxs[i];
	if (j < 0 || j >= lp->Solver->getNumCols()) return -1;

	if (lu[i] == 'U')      lp->Solver->setColUpper(j, bd[i]);
	else if (lu[i] == 'L') lp->Solver->setColLower(j, bd[i]);
	else if (lu[i] == 'B') lp->Solver->setColBounds(j, bd[i], bd[i]);
	else return -1; /* unknown type -- error! */
    }
    return 0;
}

extern "C"
int coin_loadbasis(COINprob* lp, const int* cbase, const int* rbase)
{
#ifndef COIN_USE_CBC
    lp->Solver->setBasisStatus(cbase, rbase);
#endif
    return 0;
}

extern "C"
int coin_getbasis(COINprob* lp, int* cbase, int* rbase)
{
#ifndef COIN_USE_CBC
    // coin_getbasis() is only called when both cbase and rbase != NULL
    lp->Solver->getBasisStatus(cbase,rbase);
#endif

    return 0;
}

extern "C"
int coin_getobjval(COINprob* lp, double &objval)
{
#ifdef COIN_USE_SYM
    // solving empty problem with OsiSym core dumps
    if (lp->Solver->getNumCols() == 0) objval = 0;
    else
#endif
    objval = lp->Solver->getObjValue();
    return 0;
}

extern "C"
int coin_get_lpobjval(lp_desc* lpd, double &objval)
{
#ifdef COIN_USE_CLP
    /* return the current linear objective value */
    if (IsMIPProb(lpd->prob_type))
    {
	objval = lpd->lp->mipmodel->getCurrentObjValue();
	return 0;
    }
#endif
#ifdef COIN_USE_SYM
    // solving empty problem with OsiSym core dumps
    if (lpd->lp->Solver->getNumCols() == 0) objval = 0;
    else
#endif
    objval = lpd->lp->Solver->getObjValue();
    return 0;
}

extern "C"
int coin_get_mipobjval(COINprob* lp, double &objval)
{
#ifdef COIN_USE_CLP
    objval = lp->mipmodel->getObjValue();
#else
    objval = lp->Solver->getObjValue();
#endif
    return 0;
}

extern "C"
int coin_get_bestmipbound(COINprob* lp, double& bound)
{
#ifdef COIN_USE_CLP
    bound = lp->mipmodel->getBestPossibleObjValue();
#elif defined(COIN_USE_CBC)
    bound = lp->Solver->getModelPtr()->getBestPossibleObjValue();
#else
    // generic: just return the right infinity (i.e. no information)
    bound = (lp->Solver->getObjSense() == 1 ? -1*lp->Solver->getInfinity() : lp->Solver->getInfinity());
#endif
    return 0;
}

extern "C"
int coin_get_objcoeffs(COINprob* lp, double* objc, int start, int end)
{
    const double* objc0 = lp->Solver->getObjCoefficients();
    int j=0;

    if (start < 0) return -1;
    if (end >= lp->Solver->getNumCols()) return -1;

    for (int i=start; i<=end; i++)
    {
	objc[j++] = objc0[i];
    }
    return 0;
}

extern "C"
int coin_chg_objcoeffs(COINprob* lp, int cnt, int* idxs, double* values)
{

    for (int i=0; i<cnt; i++)
    {
	int j=idxs[i];
	if (j < 0 || j >= lp->Solver->getNumCols()) return -1;

	lp->Solver->setObjCoeff(j,values[i]);
    }
    return 0;
}

extern "C"
int coin_get_order(COINprob* lp, int cnt, int* idxs, int* prio, int* direction)
{
    return -1;
}

extern "C"
int coin_set_qobj(COINprob* lp, int mac, int cb_cnt, int* cb_index,
		  int* cb_index2, double* cb_value)
{
#ifdef COIN_USE_CLP
    if (cb_cnt > 0)
    {
	CoinBigIndex* starts = new CoinBigIndex[mac+1];
	int* colidx = new int[cb_cnt];
	double* coeffs = new double[cb_cnt];
	int cur_col = 0;
	int cur_start = 0;

	for (int i=0; i<cb_cnt; i++)
	{
	    // cb_index are sorted in ascending order
	    colidx[i] = cb_index2[i];
	    coeffs[i] = cb_value[i];
	    if (cb_index[i] > cur_col)
	    {// starting coeffs for new col
	        for (int s=cur_col; s < cb_index[i]; s++) 
		    starts[s] = cur_start;
		cur_start = i;
		cur_col = cb_index[i];
	    }
	}
	// no coeffs for rest of cols, fill the starts in
	starts[cur_col] = cur_start; // last col with coeffs
	for (int s=cur_col+1; s <= mac; s++) starts[s] = cb_cnt;

	lp->Solver->getModelPtr()->loadQuadraticObjective(mac, starts, colidx, coeffs);

	delete [] starts;
	delete [] colidx;
	delete [] coeffs;

    }

    return 0;

#else

    return -1;

#endif
}

extern "C"
int coin_chgqobj(COINprob* lp, int i, int j, double value)
{
    return -1;
}

extern "C"
int coin_chgrhs(COINprob* lp, int cnt, int* idxs, double* values)
{

    const char* rsen = lp->Solver->getRowSense();
    for (int i=0; i<cnt; i++)
    {
	int j=idxs[i];
	if (j < 0 || j >= lp->Solver->getNumRows()) return -1;

	switch (rsen[j])
	{
	case 'L': lp->Solver->setRowUpper(j, values[i]); break;
	case 'G': lp->Solver->setRowLower(j, values[i]); break;
	case 'E': lp->Solver->setRowBounds(j,values[i],values[i]);; break;
	default: return -1;
	}
    }
    return 0;
}

extern "C"
int coin_getnumnz(COINprob* lp)
{
    return lp->Solver->getNumElements();
}

extern "C"
int coin_getnumint(COINprob* lp)
{
    return lp->Solver->getNumIntegers();
}

extern "C"
int coin_loadprob(COINprob* lp, int mac, int mar, int objsen, double* objx, 
		  double* rhsx, char* senx, 
		  int* matbeg, int* matcnt, int* matind, double* matval, 
		  double* lb, double* ub)
{
    double* range = new double[mar];

    /* coin doesn't use matcnt, but needs the matbeg for one more column to 
       be specifified
    */
    matbeg[mac] = (mac > 0 ? matbeg[mac-1]+matcnt[mac-1] : 0); 
    for (int i=0; i<mar; i++) range[i] = 0.0;

    //    CoinPackedMatrix* mat = 
    //new CoinPackedMatrix(true, mar, mac, matbeg[mac],
    //			   matval, matind, matbeg, matcnt, 0.6, 0.6);
    lp->Solver->loadProblem(mac, mar, matbeg, matind, matval, lb, ub, objx, senx, rhsx, range);
    //lp->Solver->loadProblem(*mat,lb, ub, objx, senx, rhsx, range);
    lp->Solver->setObjSense((objsen == SENSE_MIN ? 1 : -1));
    delete [] range;
    //delete mat;
    return 0;
}

extern "C"
int coin_setcoltype(COINprob* lp, char *ctype)
{
    int mac = lp->Solver->getNumCols();
    for (int i=0; i<mac; i++)
    {
	if (ctype[i] == 'C') lp->Solver->setContinuous(i);
	else if (ctype[i] == 'I') lp->Solver->setInteger(i);
	else if (ctype[i] == 'B') lp->Solver->setInteger(i); // no setBinary() 
	else return -1;  /* unknown type -- error! */
    }
    return 0;
}

extern "C"
int coin_addcols(COINprob* lp, int coladded, int matnz, const double* objx, 
		 int* matbeg, const int* matind, const double* matval, 
		 const double* bdl, const double* bdu)
{

    matbeg[coladded] = matnz;

    CoinBuild build;

    for (int i=0; i<coladded; i++)
    {
	build.addColumn(matbeg[i+1]-matbeg[i],
			&(matind[matbeg[i]]),
			&(matval[matbeg[i]]),
			bdl[i], bdu[i], objx[i]);
    }

    static_cast<OsiSolverInterface* >(lp->Solver)->addCols(build);

    return 0;
}

extern "C"
int coin_addrows(COINprob* lp, const int rowadded, int nzadded, 
		 const double* rhsx, const char* senx,
		 int* rmatbeg, int* rmatind, double* rmatval)
{
    //CoinPackedVector * rows = new CoinPackedVector[rowadded];

    rmatbeg[rowadded] = nzadded;

    //    double*  rrange = new double[rowadded];
    double inf = lp->Solver->getInfinity();

    CoinBuild build;
    for (int i=0; i < rowadded; i++)
    {
	double ub, lb;
	if (senx[i] == 'L') {lb= -inf; ub= rhsx[i];}
	else if (senx[i] == 'E') {lb= ub= rhsx[i];}
	else if (senx[i] == 'G') {lb= rhsx[i]; ub= inf;}
	else return -1;

	build.addRow(rmatbeg[i+1]-rmatbeg[i],
		     &(rmatind[rmatbeg[i]]),
		     &(rmatval[rmatbeg[i]]),
		     lb, ub);
    }
    static_cast<OsiSolverInterface* >(lp->Solver)->addRows(build);
    /*
    for (int i=0; i < rowadded; i++)
    {
	rrange[i] = 0;

	rows[i].setVector(rmatbeg[i+1]-rmatbeg[i],
			  &(rmatind[rmatbeg[i]]),
			  &(rmatval[rmatbeg[i]]),
			  false);
    }


        CoinPackedVectorBase* rows1 = static_cast<CoinPackedVectorBase *>(rows);
    lp->Solver->addRows(rowadded, const_cast<CoinPackedVectorBase* const*>(&rows1), senx, rhsx, const_cast<const double *>(rrange)); 

    for (int l=0; l<rowadded; l++) {
	double* elms=rows[l].getElements();
	for (int m=0; m<rows[l].getNumElements(); m++) std::cout<<elms[m]<<" ";
	std::cout<<std::endl;
    }
    const CoinPackedMatrix* mat = lp->Solver->getMatrixByRow();
    const double* elms = mat->getElements();
    for (int k=0; k<mat->getNumElements(); k++)  std::cout<<elms[k]<<" ";
    std::cout<<std::endl;
    */
    return 0;
}

extern "C"
int coin_chgobjsen(COINprob* lp, int objsen)
{
    lp->Solver->setObjSense((objsen == SENSE_MIN ? 1 : -1));
    return 0;
}

extern "C"
int coin_get_row(COINprob* lp, int &nnz, int* rmatind, double* rmatval, int idx)
{
    try
    {
	const CoinShallowPackedVector row = 
	    lp->Solver->getMatrixByRow()->getVector(idx);

	nnz = row.getNumElements();
	memcpy(rmatind, row.getIndices(), nnz*sizeof(int));
	memcpy(rmatval, row.getElements(), nnz*sizeof(double));
    }
    catch (CoinError e)
    {
	coin_error_handler(e);
	return -1;
    }

    return 0;
}

extern "C"
int coin_delrows(COINprob* lp, int ndr, int* idxs)
{
    lp->Solver->deleteRows(ndr, idxs);
	    
    return 0;
}

extern "C"
int coin_delcols(COINprob* lp, int ndr, int* idxs)
{
    lp->Solver->deleteCols(ndr, idxs);

    return 0;
}

extern "C"
int coin_get_bar_primal_objval(COINprob* lp, double &objval)
{
#ifdef COIN_USE_CLP
    if (lp->interiormodel != NULL)
    {
	objval = lp->interiormodel->rawObjectiveValue()*lp->interiormodel->optimizationDirection();
	return 0;
    }
#endif
    return -1;
}

extern "C"
int coin_get_bar_dual_objval(COINprob* lp, double &objval)
{
#ifdef COIN_USE_CLP
    if (lp->interiormodel != NULL)
    {
	// no information at the moment, just return the right infinity 
	objval =  ( 
		   lp->interiormodel->isProvenOptimal()
		   // optimal, just return obj. value 
		   ? lp->interiormodel->rawObjectiveValue()*lp->interiormodel->optimizationDirection()
		   // opt.dir = -1 max, 1 min => inf for max, -inf for min
		   : -1.0*lp->interiormodel->optimizationDirection()*lp->Solver->getInfinity() 
		   );

	return 0;
    }
#endif
    return -1;
}

/* this should be called soon after a call to coin_solve_problem(), before
   any backtracking, because the result state etc. are not stored logically
*/
extern "C"
state_t coin_get_result_state(lp_desc* lpd)
{
    //    OsiXxxSolverInterface* Solver = lpd->lp->Solver;
#ifdef COIN_USE_SYM
    // solving empty problem with OsiSym core dumps
    if (lpd->lp->Solver->getNumCols() == 0) return state_success;
#endif
#ifdef COIN_USE_CLP
    // get more MIP information using CLP specific methods...
    if (IsMIPProb(lpd->prob_type))
    {
	CbcModel* model = lpd->lp->mipmodel;
	if (model->isProvenOptimal()) return state_success;
	if (model->isInitialSolveProvenOptimal()) // succeeded at root
	{
	    if (model->isProvenInfeasible()) return state_fail;
	    if (model->bestSolution()) return state_mipsemisucc;
	    return state_mipsemifail;
	}
	if (model->isInitialSolveAbandoned()) return state_lpaborted;
	// unbounded at root => MIP can be unbounded or infeasible
	if (model->isContinuousUnbounded()) return state_unknown; 
	if (model->isInitialSolveProvenPrimalInfeasible()) return state_fail;
	//if (model->isInitialSolveProvenDualInfeasible()) return state_unbounded;
	return state_mipsemifail;
    }
    if (lpd->lp->interiormodel != NULL)
    {// CLP's interior needs special test to detect failure
	if (lpd->lp->Solver->isProvenOptimal())
        {
	    if (lpd->lp->interiormodel->sumPrimalInfeasibilities() < 1e-5)
		return state_success;
	    else
		return state_fail;
	}
    } else if (lpd->lp->Solver->isProvenOptimal()) return state_success;

#else // !COIN_USE_CLP

    if (lpd->lp->Solver->isProvenOptimal()) return state_success;
#endif
    // isAbandoned() due to numeric difficulties only
    if (lpd->lp->Solver->isAbandoned()) 
    {
	if (IsMIPProb(lpd->prob_type)) return state_mipsemifail;
	else return state_lpaborted;
    }
    if (lpd->lp->Solver->isProvenPrimalInfeasible()) return state_fail;
    if (lpd->lp->Solver->isProvenDualInfeasible()) return state_unbounded;

    // problem is not optimal, infeasible or unbounded, solving is incomplete. 
    // For MIP, we need to extract information from the MIP to determine
    // if there is any feasible solution or not. OSI's API does not provide
    // this, so we return semifail by default if solver specific methods
    // are not used earlier
    if (IsMIPProb(lpd->prob_type)) return state_mipsemifail;
    // is LP...
#ifdef COIN_USE_CLP
    // hit max. iterations *or timeout* 
    if (lpd->lp->Solver->getModelPtr()->hitMaximumIterations() ||
#else
    if (lpd->lp->Solver->isIterationLimitReached() || 
#endif
	lpd->lp->Solver->isPrimalObjectiveLimitReached() ||
	lpd->lp->Solver->isDualObjectiveLimitReached())
	return state_lpaborted;
    // no better information....
    return state_unknown; 
}

extern "C"
int coin_get_mipcutoff(COINprob* lp, double &cutoff)
{
#ifdef CBC_IS_MIPSOLVER
    CbcModel* mip = GetCbcSolver(lp);

    cutoff = (lp->Solver->getObjSense() == 1 ? mip->getCutoff() : -1.0*mip->getCutoff());
#else
    cutoff = (lp->Solver->getObjSense() == 1 ? lp->Solver->getInfinity() : -1.0*lp->Solver->getInfinity());
#endif 

    return 0;
}

extern "C"
double coin_infinity(COINprob* lp)
{
    return lp->Solver->getInfinity();
}

extern "C"
int coin_getdblparam(COINprob* lp, int key, double &value)
{
    lp->Solver->getDblParam(OsiDblParam(key), value);
    return 0;
}

extern "C"
int coin_getintparam(COINprob* lp, int key, int &value)
{
    lp->Solver->getIntParam(OsiIntParam(key), value);
    return 0;
}

extern "C"
int coin_setdblparam(COINprob* lp, int key, double value)
{
    lp->Solver->setDblParam(OsiDblParam(key), value);
    return 0;
}

extern "C"
int coin_setintparam(COINprob* lp, int key, int value)
{
    lp->Solver->setIntParam(OsiIntParam(key), value);
    return 0;
}


extern "C"
int coin_solve_problem(lp_desc* lpd, 
		       int meth, int auxmeth, int node_meth, int node_auxmeth)
{
    bool doDual;

#ifdef COIN_USE_SYM
    // solving empty problem with OsiSym core dumps
    if (lpd->lp->Solver->getNumCols() == 0) return 0;
    if (lpd->lp->Solver->getNumRows() == 0) return 0;
#endif
#ifdef COIN_USE_CLP
    // delete any old interior model of problem
    if (lpd->lp->interiormodel != NULL)
    {
	delete lpd->lp->interiormodel;
	lpd->lp->interiormodel = NULL;
    }
#endif
    switch (meth)
    {// OSI allows only primal/dual to be specified. Barrier done later
    default:
    case METHOD_DEFAULT:
    case METHOD_DUAL:
	doDual = true;
	break;
    case METHOD_PRIMAL:
	doDual = false;
	break;
    }

    lpd->lp->Solver->setHintParam(OsiDoDualInInitial, doDual, OsiHintDo, NULL);
    lpd->lp->Solver->setHintParam(OsiDoDualInResolve, doDual, OsiHintDo, NULL);

    lpd->lp->Solver->setHintParam(OsiDoPresolveInInitial, lpd->presolve, OsiHintDo, NULL);
    lpd->lp->Solver->setHintParam(OsiDoPresolveInResolve, lpd->presolve, OsiHintDo, NULL);


    switch (lpd->prob_type)
    {
    case PROBLEM_MIP:
    case PROBLEM_FIXEDL:
#ifdef COIN_USE_CLP
	/* turn off timeout in CLP (otherwise may cause problems for MIP */
	lpd->lp->Solver->getModelPtr()->setMaximumSeconds(-1);
#endif
	lpd->lp->Solver->setHintParam(OsiDoInBranchAndCut, true, OsiHintDo);
	coin_branchAndBound(lpd);
	break;
    case PROBLEM_LP:
    case PROBLEM_RELAXEDL:
#ifdef COIN_USE_CLP
    case PROBLEM_QP:
    // case PROBLEM_RELAXEDQ:
	//	lpd->lp->Solver->getModelPtr()->setPerturbation(50);
	if (lpd->lp->timeout > 0) 
	    lpd->lp->Solver->getModelPtr()->setMaximumSeconds(lpd->lp->timeout);
#endif
	//lpd->lp->Solver->setHintParam(OsiDoCrash, true, OsiHintDo);
	lpd->lp->Solver->setHintParam(OsiDoInBranchAndCut, false, OsiHintDo);
	coin_solveLinear(lpd, meth, auxmeth);
	break;

    default:
	eclipse_out(ErrType, "Eplex Error: cannot solve problem type with this solver.\n"); 
	return -1;
	break;
    }

    return 0;
}

extern "C"
int coin_get_stats(lp_desc* lpd)
{
#ifdef COIN_USE_SYM
    if (lpd->lp->Solver->getNumCols() == 0) 
    {
	lpd->sol_itcnt = 0;
	return 0;
    }
#endif
    lpd->sol_itcnt = lpd->lp->Solver->getIterationCount();
    return 0;
}

extern "C"
int coin_get_soln_state(lp_desc* lpd, double* sols, double* pis, double* slacks,
			double* djs, int* cbase, int* rbase)
{
    int mac = lpd->lp->Solver->getNumCols();
    int mar = lpd->lp->Solver->getNumRows();

#ifdef COIN_USE_SYM
    // solving empty problem with OsiSym core dumps
    if (mac == 0) return 0;
#endif
    if (lpd->mar != mar || lpd->mac != mac)
    {
	eclipse_out(ErrType, "Eplex Error: rows and columns does not match the problem!\n");
	return -1;
    }

    if (sols != NULL)
	memcpy(sols, lpd->lp->Solver->getColSolution(), mac*sizeof(double));
    if (pis != NULL)
	memcpy(pis, lpd->lp->Solver->getRowPrice(), mar*sizeof(double));
    if (slacks != NULL)
    {
	const double* act = lpd->lp->Solver->getRowActivity();
	const double* rhs = lpd->lp->Solver->getRightHandSide();

	for (int i=0; i<mar; i++) 
	{
	    slacks[i] = rhs[i] - act[i];
	}
    }
    if (djs != NULL)
	memcpy(djs, lpd->lp->Solver->getReducedCost(), mac*sizeof(double));
#ifndef COIN_USE_CBC
    // basis not available for CBC 
    if (cbase != NULL || rbase != NULL)
    {
	int* cbase0 = (cbase == NULL ? new int[mac] : cbase);
	int* rbase0 = (rbase == NULL ? new int[mar] : rbase);

	lpd->lp->Solver->getBasisStatus(cbase0, rbase0);
	if (cbase == NULL) delete [] cbase0; 
	if (rbase == NULL) delete [] rbase0;
    }
#endif
    return 0;
}

extern "C"
int coin_get_objsen(COINprob* lp)
{
    return (lp->Solver->getObjSense() == 1 ? SENSE_MIN : SENSE_MAX);
}

extern "C"
int coin_get_numcols(COINprob* lp)
{
    return (lp->Solver->getNumCols());
}

extern "C"
int coin_get_numrows(COINprob* lp)
{
    return (lp->Solver->getNumRows());
}

extern "C"
int coin_get_probtype(COINprob* lp)
{
#ifdef COIN_USE_CBC
    CbcModel* model = lp->Solver->getModelPtr();

    if (model->numberIntegers() > 0) return PROBLEM_MIP;
    else return PROBLEM_LP;
#else
    int mac = lp->Solver->getNumCols();
    // there is no constant time method of getting the integer cols info yet
    // so this should be more efficient than getNumIntegers()
    for (int i=0; i<mac; i++)
    {
	if (lp->Solver->isInteger(i)) return PROBLEM_MIP;
    }
    return PROBLEM_LP; // no integer columns - LP problem
#endif
    
}

extern "C"
int coin_create_prob(COINprob* &lp, COINprob* def)
{
    // def is `default' problem with default settings. NULL if creating default
    DerivedHandler* coinMessageHandler = new DerivedHandler;
    lp = new COINprob;
    lp->Solver = new OsiXxxSolverInterface();
    lp->notfirst = 0;
    lp->varnames = NULL;
    lp->vnsize = 0;
#ifdef COIN_USE_CLP
    lp->mipmodel = new CbcModel(static_cast<OsiSolverInterface &>(*lp->Solver));
    //lp->mipobjects = NULL;
    //lp->nsos = 0;
    lp->interiormodel = NULL;
    lp->mipIsShared = 1;
    if (def)
    {// copy the parameter values from default
	for (int i=0; i<NUMSOLVERINTPARAMS; i++)
	    lp->mipmodel->setIntParam(cbc_iparam[i], def->mipmodel->getIntParam(cbc_iparam[i]));
	for (int i=0; i<NUMSOLVERDBLPARAMS; i++)
	    lp->mipmodel->setDblParam(cbc_dparam[i], def->mipmodel->getDblParam(cbc_dparam[i]));
    }

    lp->timeout = -1; // no timeouts
#else
    if (def) 
    {// this should copy the parameters from def to lp, but it does not
     // seem to work, so should add specific code to copy params 
	lp->Solver->copyParameters(*def->Solver);
    }
#endif

    lp->Solver->passInMessageHandler(coinMessageHandler);
    lp->Solver->messageHandler()->setLogLevel(1);
    coin_set_solver_outputs(lp->Solver);

    return 0;
}

extern "C"
int coin_get_dual_infeas(COINprob* lp, int &infeas)
{
#ifdef COIN_USE_CLP
    ClpSimplex* simplex = dynamic_cast<ClpSimplex*>( lp->Solver->getModelPtr());
    if (simplex == NULL) return -1;
    infeas = simplex->numberDualInfeasibilities();
#endif
    return 0;
}

extern "C"
int coin_get_primal_infeas(COINprob* lp, int &infeas)
{
#ifdef COIN_USE_CLP
    ClpSimplex* simplex = dynamic_cast<ClpSimplex*>( lp->Solver->getModelPtr());
    if (simplex == NULL) return -1;
    infeas = simplex->numberPrimalInfeasibilities();
#endif
    return 0;
}

extern "C"
int coin_bar_is_dual_feas(COINprob* lp)
{
#ifdef COIN_USE_CLP
    if (lp->interiormodel != NULL)
    {
	return lp->interiormodel->dualFeasible();
    }
#endif
    return 0;
}

extern "C"
int coin_bar_is_primal_feas(COINprob* lp)
{
#ifdef COIN_USE_CLP
    if (lp->interiormodel != NULL)
    {
	return lp->interiormodel->primalFeasible();
    }
#endif
    return 0;
}

extern "C"
int coin_reset_prob(lp_desc* lpd)
{
    // CBC modifies the problem state (including column bounds), and this 
    // needs to be reset before we can solve the problem again
#ifdef COIN_USE_CBC
    if (lpd->prob_type == PROBLEM_MIP)
    {
	CbcModel*  model = lpd->lp->Solver->getModelPtr();
	model->resetToReferenceSolver();
    }
#endif
    return 0;
}

extern "C"
int coin_writeprob(COINprob* lp, const char* file, char* otype)
{

    try
    {
	if (strcmp(otype, "LP") == 0) 
	{
	    //	    lp->Solver->writeLp(file, "", 1e-5, 10, 10, lp->Solver->getObjSense());
	    lp->Solver->writeLpNative(file, NULL, lp->varnames, 1e-5, 10, 10, 
				      lp->Solver->getObjSense());
	}
	else if (strcmp(otype, "MPS") == 0)
	{
	    //	    lp->Solver->writeMps(file, "", lp->Solver->getObjSense());
	    lp->Solver->writeMpsNative(file, NULL, 
				       const_cast<const char**>(lp->varnames), 1, 2, 
				       lp->Solver->getObjSense());
	}
	else return -1;
    }
    catch (CoinError e)
    {
	coin_error_handler(e);
	return -1;
    }
    return 0;
}

bool coin_read_prob_file(OsiXxxSolverInterface* Solver, 
			 const char* file,
			 const char* ext,
			 int format)
{
    char* file1 = new char[strlen(file)+strlen(ext)];
    int err = 0;
    try
    {
	strcpy(file1, file);
	strcat(file1, ext);
	// check for file existance as exit() is called if there is anything
	// wrong with the file!
	if (!fileExists(file1)) return false;
	switch (format)
	{
	case 1: // LP
	    err = Solver->readLp(file1);
	    break;
	case 2: // MPS
	    err = Solver->readMps(file1,"");
	    break;
	}
	delete [] file1;
	return (err ? false : true);
    }
    catch (CoinError e)
    {
	delete [] file1;
	coin_error_handler(e);
	return false;
    }
}

extern "C"
int coin_readprob(COINprob* lp, const char* file, char* otype)
{

    if (strcmp(otype, "LP") == 0) 
    {
	if (coin_read_prob_file(lp->Solver, file, "", 1)) return 0;
	else if (coin_read_prob_file(lp->Solver, file, ".lp", 1)) return 0;
	else return -1;
    }
    else if (strcmp(otype, "MPS") == 0)
    {
	if (coin_read_prob_file(lp->Solver, file, "", 2)) return 0;
	else if (coin_read_prob_file(lp->Solver, file, ".mps", 2)) return 0;
	else if (coin_read_prob_file(lp->Solver, file, ".mat", 2)) return 0;
	else return -1;
    }
    else return -1;
}


extern "C"
int coin_set_name(COINprob* lp, char ntype, int idx, char *const name)
{

    if (ntype == 'c')
    {
	int nc = lp->Solver->getNumCols();
	if (lp->vnsize < nc)
	{
	    int newvnsize = (int) ceil(nc*1.5)+100;
	    lp->varnames = (char**) realloc(lp->varnames, newvnsize*sizeof(char**));
	    for (int i=lp->vnsize; i < newvnsize; i++) 
	    {
		// adapted from write.c's _int_to_string()
		int number = i, pos = 0;
		do
		{
		    ++pos;
		    number /= 10;
		} while (number);
		pos += 1; // leading 'x' and terminating '\0'
		lp->varnames[i] = new char[pos+1];
		/* use x as default varname -- not valid var name in ECLiPSE,
                   so cannot conflict with user supplied var names */
		lp->varnames[i][0] = 'x'; 
		lp->varnames[i][pos--] = '\0';
		number = i;
		do
		{
		    int ch = number % 10;
		    lp->varnames[i][pos--] = ch + '0';
		    number /= 10;
		} while (number);

	    }

	    lp->vnsize = newvnsize;
	}
	if (idx < 0 || idx >= nc)
	    return -1;
	delete lp->varnames[idx]; // get rid of old name
	lp->varnames[idx] = new char[strlen(name)];
	strcpy(lp->varnames[idx], name);
    }
    else return -1; // row names not supported 
    return 0;
}

extern "C"
int coin_load_sos(COINprob* lp, int nsos, int nsosnz, char* sostype, 
		  int* sosbeg, int* sosind, double* soswt)
{
#ifdef NOT_USED 
    /*COIN_USE_CLP - not used yet as SOS cannot be added to a preprocessed
      problem*/
    lp->mipobjects = new CbcObject * [nsos];

    for (int i=0; i<nsos-1; i++)
    {
	lp->mipobjects[i] = new CbcSOS(lp->mipmodel, sosbeg[i+1]-sosbeg[i], 
				       &sosind[i], &soswt[i], i, 
				       (sostype[i] == '1' ? 1 : 2));
    }
    if (nsos > 0)
    {// last set
	int i = nsos - 1;
	lp->objects[i] = new CbcSOS(lp->mipmodel, nsosnz-sosbeg[i], 
				    &sosind[i], &soswt[i], i, 
				    (sostype[i] == '1' ? 1 : 2));
    }

    lp->nsos = nsos;
    lp->mipmodel->addObjects(nsos,objects);
    for (int i=0; i<nsos; i++) delete objects[i];
    delete [] objects;

    return 0;
#else

    // unimplemented
    return -1;
#endif

}

extern "C"
int coin_free_prob(COINprob* lp)
{
    if (lp == NULL) return 0;
    if (lp->varnames != NULL)
    {
	for (int i=0; i < lp->Solver->getNumCols(); i++) 
	    delete lp->varnames[i];
    }
    free(lp->varnames);

    delete lp->Solver->messageHandler();
    /* solver specific stuff */
    coin_free_solver_handlers(lp->Solver);
#ifdef COIN_USE_CLP
    if (!lp->mipIsShared)
	delete lp->mipmodel;
    if (lp->interiormodel != NULL) delete lp->interiormodel;
#endif
    delete lp->Solver;
    delete lp;
    return 0;
}
