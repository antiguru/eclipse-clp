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
#include <vector>

class GecodeSpace : public Gecode::MinimizeSpace {
public:
    Gecode::IntVarArray vInt;
    Gecode::BoolVarArray vBool;
    Gecode::BoolVar booltrue;
    Gecode::BoolVar boolfalse;
    Gecode::IntVar vCost;

    GecodeSpace(void) : vInt(*this, 1, 0, 0), vBool(*this, 0, 0, 0), 
			booltrue(*this, 1, 1), boolfalse(*this, 0, 0),
			vCost(*this, 0, 0),
			valid_snapshot(false) {}

    std::vector<int>  dom_snapshot;

    void set_snapshot() {valid_snapshot = true;}
    void clear_snapshot() {valid_snapshot = false;}
    bool snapshot_valid() {return valid_snapshot;}

    GecodeSpace(bool share, GecodeSpace& s) : valid_snapshot(s.valid_snapshot),
	                                      booltrue(*this, 1, 1), boolfalse(*this, 0, 0),
					      Gecode::MinimizeSpace(share,s) {
//	valid_snapshot = s.valid_snapshot;
	if (snapshot_valid()) dom_snapshot = s.dom_snapshot;
	vInt.update(*this, share, s.vInt);
	vBool.update(*this, share, s.vBool);
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
};

class Ec2gcException {};
