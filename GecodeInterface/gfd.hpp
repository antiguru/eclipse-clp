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

#include <gecode/int.hh>
#include <gecode/minimodel.hh>
#include <gecode/search.hh>
/* not used in Gecode >= 3.6.0
#include <gecode/graph.hh>
#include <gecode/scheduling.hh>
*/
#include <vector>

#if defined(__APPLE__) && defined(__MACH__)
/* Mac OS X */

# define VisAtt __attribute__((visibility ("default")))

#else

# define VisAtt

#endif

using namespace Gecode;

class GecodeSpace : public Gecode::MinimizeSpace {
public:
    Gecode::IntVarArgs vInt;
    Gecode::BoolVarArgs vBool;
    Gecode::BoolVar booltrue;
    Gecode::BoolVar boolfalse;
    Gecode::IntVar vCost;

    GecodeSpace(void) : vInt(*this, 1, 0, 0), vBool(*this, 0, 0, 0), 
			booltrue(*this, 1, 1), boolfalse(*this, 0, 0),
			vCost(*this, 0, 0),
			first(true),
			valid_snapshot(false) {}

    std::vector<int>  dom_snapshot;

    void set_snapshot() {valid_snapshot = true;}
    void clear_snapshot() {valid_snapshot = false;}
    bool snapshot_valid() {return valid_snapshot;}

    void stop_caching() {first = false;}
    void start_caching() {first = true;}
    bool is_first() {return first;}

  GecodeSpace(bool share, GecodeSpace& s) : vInt(s.vInt.size()), vBool(s.vBool.size()),
					    valid_snapshot(s.valid_snapshot),
					    first(true),
	                                    booltrue(*this, 1, 1), boolfalse(*this, 0, 0),
					    Gecode::MinimizeSpace(share,s) {
//	valid_snapshot = s.valid_snapshot;
	if (snapshot_valid()) dom_snapshot = s.dom_snapshot;
	for (int i=vInt.size(); i--;)
	  vInt[i].update(*this, share, s.vInt[i]);
	for (int i=vBool.size(); i--;)
	  vBool[i].update(*this, share, s.vBool[i]);
	vCost.update(*this, share, s.vCost);
	
    }

    virtual Gecode::MinimizeSpace*
    copy(bool share) {
	return new GecodeSpace(share,*this);
    }

    virtual Gecode::IntVar cost(void)  const {
	return vCost;
    }

private:
    bool valid_snapshot;
    bool first;
};

enum SearchMethod {METHOD_COMPLETE, 
		   METHOD_LDS, 
		   METHOD_BAB,
		   METHOD_RESTART};

// taken from gecode/driver/script.hpp, by Christian Schulte
class Cutoff : public Gecode::Search::Stop {
private:
    Search::NodeStop* ns; ///< Used node stop object
    Search::FailStop* fs; ///< Used fail stop object
    Search::TimeStop* ts; ///< Used time stop object
    Search::MemoryStop* ms; ///< Used memory stop object
    long stop_reason;
public:
    /// Initialize stop object
    Cutoff(unsigned int node, unsigned int fail, unsigned int time, size_t mem)
	: ns((node > 0) ? new Search::NodeStop(node) : NULL),
	  fs((fail > 0) ? new Search::FailStop(fail) : NULL),
	  ts((time > 0) ? new Search::TimeStop(time) : NULL),
	  ms((mem  > 0) ? new Search::MemoryStop(mem) : NULL),
	  stop_reason(0) {}
    /// Reason why search has been stopped
    enum {
	SR_NODE = 1 << 2, ///< Node limit reached
	SR_FAIL = 1 << 3, ///< Fail limit reached
	SR_TIME = 1 << 4, ///< Time limit reached
	SR_MEM  = 1 << 5  ///< Memory limit reached
    };
    /// Test whether search must be stopped
    virtual bool stop(const Search::Statistics& s, const Search::Options& o) {
	bool stopping;
	stopping =
	    ((ns != NULL) && ns->stop(s,o)) ||
	    ((fs != NULL) && fs->stop(s,o)) ||
	    ((ts != NULL) && ts->stop(s,o)) ||
	    ((ms != NULL) && ms->stop(s,o));
	if (stopping) {
	    this->stop_reason =
		(((ns != NULL) && ns->stop(s,o)) ? SR_NODE : 0) |
		(((fs != NULL) && fs->stop(s,o)) ? SR_FAIL : 0) |
		(((ts != NULL) && ts->stop(s,o)) ? SR_TIME : 0) |
		(((ms != NULL) && ms->stop(s,o)) ? SR_MEM  : 0);
	}
	return stopping;
    }
    /// Report reason why search has been stopped
    long reason(void) {
	return this->stop_reason;
    }
    /// Reset (currently only for timer)
    void reset(void) {
	if (ts != NULL) ts->reset();
    }
    /// Destructor
    ~Cutoff(void) {
	delete ns; delete fs; delete ts; delete ms;
    }
};


class GecodeSearch {
private:
    Search::Engine* sengine;
public:
    SearchMethod method;
    Cutoff* stopp;

    GecodeSearch(GecodeSpace* solver, Search::Options o, unsigned extra,
		 Cutoff* cutoffp, 
		 const SearchMethod& m) : stopp(cutoffp), method(m) {
	switch (m) {
	    case METHOD_COMPLETE:
		sengine = Search::dfs(solver,sizeof(GecodeSpace),o);
		break;
		/* lds no longer supported
	    case METHOD_LDS:
		o.d = extra;
		sengine = Search::lds(solver,sizeof(GecodeSpace),o);
		break; */
	    case METHOD_BAB:
		solver->vCost = solver->vInt[extra];
		sengine = Search::bab(solver,sizeof(GecodeSpace),o);
		break;
	    case METHOD_RESTART:
		solver->vCost = solver->vInt[extra];
		sengine = Search::restart(solver,sizeof(GecodeSpace),o);
		break;
	}
    }

    GecodeSpace* next(void) {
	return static_cast<GecodeSpace*>(sengine->next());
    }

    Search::Statistics statistics(void) const {
	return sengine->statistics();
    }

    bool stopped(void) const {
	return sengine->stopped();
    }

    ~GecodeSearch(void) {
	delete sengine;
	if (stopp != NULL) delete stopp;
    }
};
							       
                                        
class Ec2gcException {};
