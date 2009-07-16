% BEGIN LICENSE BLOCK
% Version: CMPL 1.1
%
% The contents of this file are subject to the Cisco-style Mozilla Public
% License Version 1.1 (the "License"); you may not use this file except
% in compliance with the License.  You may obtain a copy of the License
% at www.eclipse-clp.org/license.
% 
% Software distributed under the License is distributed on an "AS IS"
% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.  See
% the License for the specific language governing rights and limitations
% under the License. 
% 
% The Original Code is  The ECLiPSe Constraint Logic Programming System. 
% The Initial Developer of the Original Code is  Cisco Systems, Inc. 
% Portions created by the Initial Developer are
% Copyright (C) 2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): Andrew Eremin, IC-Parc
% 
% END LICENSE BLOCK

:- comment(categories, ["Algorithms","Constraints"]).
:- comment(summary, "Column generation library").
:- comment(author, "Andrew Eremin").
:- comment(copyright, "Cisco Systems, Inc.").
:- comment(date, "$Date: 2009/07/16 09:11:25 $").
:- comment(status, prototype).

:- comment(desc, html("\
<P>

   This library lets you use hybrid column generation. Partial linear
   constraints are posted to a solver and further variables added to
   them during search as they become profitable. The generated
   variables will have a column of coefficients in the constraints of
   the colgen instance associated with them corresponding to
   particular instantiations of the variables of a subproblem. The
   predicate to find profitable subproblem variable instantiations is
   supplied by the user. When a user-defined branching predicate is
   provided the library can also be sued for hybrid branch-and-price.
</P><P>
   The library uses the eplex library to solve LP master
   problems, from which dual values are used to create cost functions
   for the user-defined subproblem. Solution of master and subproblems
   will iterate until no further subproblem solutions are posted to
   the colgen instance.
</P> ")).

:- comment(minimize/3, [
template:  "ColgenInstance:minimize(+SolveSubProblem, +Obj, -ObjVal)",
    args:  ["SolveSubProblem": "Subproblem solution predicate",
            "Obj": "The objective function to minimize",
            "ObjVal": "The optimal solution cost"
           ],
    summary: "Minimizes the problem associated with the colgen instance ColgenInstance.",
    desc: html("\
  <P>
  Minimize the partial linear expression <TT>Obj</TT> for the problem
  associated with the colgen instance <TT>ColgenInstance</TT>, using
  the user-defined predicate <TT>SolveSubProblem</TT> to provide
  profitable variables during solution. The optimal solution cost is
  unified with <TT>ObjVal</TT>.
  </P><P>
  The first argument of the subproblem solution predicate must be a
  subproblem structure:
 <PRE>
      sp_prob(master_pool, cutoff, cost, coeff_vars, aux, module)
 </PRE>
  where and <TT>master_pool</TT> will be unified with the colgen
  instance <TT>ColgenInstance</TT> so that solutions can be posted to
  it from within the solution predicate, <TT>cutoff</TT> is a minimum
  acceptable value for the cost of subproblem solutions that will be
  updated before calling the predicate, <TT>cost</TT> is the variable
  occurring in the implicit sum term of <TT>obj</TT> (if any)
  representing the contribution of new subproblem solutions to the
  master problem solution cost, <TT>coeff_vars</TT> is a list of all
  subproblem variables occurring in the implicit sum terms of master
  problem constraints, and <TT>module</TT> is the module in which the
  solution predicate should be called.
  </P>
  ")
]).

:- comment(cg_subproblem_solution/1, [
template:  "ColgenInstance:cg_subproblem_solution(++Value)",
    args:  ["Value": "Subproblem solution (sp_sol structure) or list"
                     " of subproblem solutions"
           ],
    summary: "Posts new subproblem solution(s) to the colgen instance ColgenInstance.",
    desc: html("\
  <P>
  Post subproblem solution(s) corresponding to a column of coefficients
  for a new master problem variable to the colgen instance
  <TT>ColgenInstance</TT>. The argument must be a <TT>sp_sol</TT>
  structure or list of such structures:
 <PRE>
      sp_sol(cost, coeff_vars, aux)
 </PRE>
  where <TT>cost</TT> is the master problem cost function coefficient
  of the solution, <TT>coeff_vars</TT> is a list of <TT>Id-Val</TT>
  pairs corresponding to the subproblem variable solution values and
  identifier of the constraint in which it occurred as an implicit sum
  term for those subproblem variables with a non-zero solution
  value. <TT>aux</TT> should contain any problem specific information
  which is of interest that is not represented uniquely by the cost
  and objective coefficients.
  </P>
  ")
]).

:- comment(identified_constraint/2, [
template:  "ColgenInstance:identified_constraint(+Cstr, ?Id)",
    args:  ["Cstr": "colgen constraint"],
    summary: "Post an identified constraint to the colgen instance ColgenInstance.",
    desc: html("\
  <P>
  Post a constraint to the colgen instance <TT>ColgenInstance</TT>
  which will be associated with the identifier <TT>Id</TT>. The
  constraint <TT>Cstr</TT> must be a valid colgen constraint of type
  <TT>>=/2,=:=/2,=</2,$>=/2,$=/2,$=</2</TT>. If <TT>Id</TT> is
  uninstantiated it will be unified with the external solver row
  number of the constraint when this is set up. The identifier can
  later be used to retrieve the dual value or subproblem cost function
  term associated with the constraint.
  </P> "),
    see_also: [(>=)/2,(=:=)/2,(=<)/2,($>=)/2,($=)/2,($=<)/2,get/2 ] ]).

:- comment(get/2, [
template:  "ColgenInstance:get(++What, -Value)",
args:      ["What":  "Specification for information wanted (atom)",
	    "Value": "Returned value of What"
           ],
summary:   "Obtain global problem information.",
desc:      html("\
<P>
   Retrieve information about solver constraints and results, for the
   colgen instance ColgenInstance. What can take one of the following values:

<DL>
    <DT><TT>dual(+Id)</TT>
      <DD>Returns the floating-point value of the
      current dual value for the constraint having identifier Id. See
      also <TT>sp_obj</TT> below.
<P>
    <DT><TT>sp_obj(+Id)</TT>
      <DD>Returns the sub problem objective terms currently associated
      associated with the constraint having identifier Id. This will
      be a term <TT>Val*Var</TT> where Val is the current dual value
      for the constraint as also returned by
      <TT>ColgenInstance:get(dual(Id), Val)</TT> and Var is the
      subproblem variable in the implicit sum term of the
      constraint. It is the users responsibility to get all relevant
      terms of the current cost function and ensure that subproblem
      solutions posted to the colgen instance have a non-negative
      cost.
<P>
    <DT><TT>vars</TT>
      <DD>Returns a list of all variables currently
      associated with the colgen instance <TT>ColgenInstance</TT>.
<P>
    <DT><TT>non_zero_vars</TT>
      <DD>Returns a list of all variables currently associated with
      the colgen instance <TT>ColgenInstance</TT> that have a non-zero
      optimal solution. This may be more efficient than retrieving all
      problem variables after solution, since very many variables can
      be generated and most will have a zero value in the optimal
      solution.
<P>
    <DT><TT>frac_vars</TT>
      <DD>Returns a list of all variables currently associated with
      the colgen instance <TT>ColgenInstance</TT> that have a
      fractional optimal solution. This is intended for use primarily
      in user-defined problem branching predicates.
</DL>") ]).

:- comment(var_get/3, [
template:  "ColgenInstance:var_get(+Var, ++What, -Value)",
args:      ["Var":   "A solver problem variable for the solver associated with ColgenInstance",
            "What":  "Specification for information wanted (atom)",
	    "Value": "Returned value of What"
           ],
summary:   "Obtain information for an individual solver problem variable Var.",
desc:      html("\
<P>
   Retrieve information about solver constraints and results related to a
   particular variable, for the colgen instance ColgenInstance.
   What can take one of the following values:
<DL>
    <DT><TT>mp_val</TT>
    <DD>Returns the floating-point solution for variable Var.
<P>
    <DT><TT>cost</TT>
    <DD>Returns the master problem objective coefficient
    associated with the variable Var.
<P>
    <DT><TT>coeffs</TT>
    <DD>Returns a list of Id-Val pairs representing the constraint
    identifiers and coefficient values for the master problem
    constraints in which the coefficient is non-zero associated
    with the variable Var.
<P>
    <DT><TT>aux</TT>
    <DD>Returns the auxiliary information associated with the
    variable Var. The intended use is for subproblem information
    not represented in the master problem constraint coefficients.
</DL>")
]).

:- comment(colgen_instance/1, [
    amode:colgen_instance(++),
    args:  ["ColgenInstance": "Colgen instance name (atom)"
           ],
    summary: "Initialises the colgen instance ColgenInstance.",
    desc: html("\
  <P>
  Initialises the colgen instance ColgenInstance. A colgen instance is an
  instance of the colgen solver, to which colgen partial linear arithmetic
  constraints can be posted, and to which an external LP solver can be
  associated and used to optimise the posted constraints with respect
  to some objective.
  </P><P>
  If ColgenInstance is not an already existing colgen instance, a new colgen
  instance will be created and initialised. If it is an existing colgen
  instance, and it is not currently being used (having no outstanding posted
  constraints and no associated solver), it is effectively reinitialised.
  Otherwise, the predicate aborts with an error. Note that a colgen instance
  is a module, and each colgen instance can be associated with at most one
  solver at any time and vice versa.
  </P>
  "),
    see_also:   [(>=)/2,(=:=)/2,(=<)/2,($>=)/2,($=)/2,($=<)/2,var_get/3]
]).

:- comment((>=)/2,  [
    template:  "ColgenInstance:(?X >= ?Y)",
    args:      ["X":    "Partial linear expression",
		"Y":    "Partial linear expression"
	       ],
    see_also:  [(=:=)/2,(=<)/2,($=)/2,($=<)/2,($>=)/2,var_get/3], 

    summary:   "Constrains X to be greater than or equal to Y.",
    desc:      html("\
	Logically: Constrains X to be greater than or equal to Y. X
	and Y are partial linear expressions. Partial linear
	expressions may contain terms of the form
	<TT>implicit_sum(+Var)</TT> in addition to any terms allowed
	within a standard linear expression. Variables occurring
	inside <TT>implicit_sum/1</TT> terms are taken to be
	subproblem variables whose instantiation will correspond to
	the coefficient of a generated master problem variable in this
	constraint. Operationally, the constraint gets delayed until
	the external solver state for ColgenInstance is invoked.")
    ]).

:- comment((=<)/2,  [
    template:  "ColgenInstance:(?X =< ?Y)",
    args:      ["X":    "Partial linear expression",
		"Y":    "Partial linear expression"
	       ],
    see_also:  [(=:=)/2,(>=)/2,($=)/2,($=<)/2,($>=)/2,var_get/3],
    summary:   "Constrains X to be less than or equal to Y.",
    desc:      html("\
	Logically: Constrains X to be less than or equal to Y. X and
	Y are partial linear expressions. Partial linear expressions
	may contain terms of the form <TT>implicit_sum(+Var)</TT> in
	addition to any terms allowed within a standard linear
	expression. Variables occurring inside <TT>implicit_sum/1</TT>
	terms are taken to be subproblem variables whose instantiation
	will correspond to the coefficient of a generated master
	problem variable in this constraint. Operationally, the
	constraint gets delayed until the external solver state for
	ColgenInstance is invoked.")  ]).

:- comment((=:=)/2,  [
    template:  "ColgenInstance:(?X =:= ?Y)",
    args:      ["X":    "Partial linear expression",
		"Y":    "Partial linear expression"
	       ],
    see_also:  [(=<)/2,(>=)/2,($=)/2,($=<)/2,($>=)/2,var_get/3],
    summary:   "Constrains X to be equal to Y.",
    desc:      html("\
	Logically: Constrains X to be equal to Y. X and Y are partial
	linear expressions. Partial linear expressions may contain
	terms of the form <TT>implicit_sum(+Var)</TT> in addition to
	any terms allowed within a standard linear
	expression. Variables occurring inside <TT>implicit_sum/1</TT>
	terms are taken to be subproblem variables whose instantiation
	will correspond to the coefficient of a generated master
	problem variable in this constraint. Operationally, the
	constraint gets delayed until the external solver state for
	ColgenInstance is invoked.")  ]).

:- comment(($>=)/2,  [
    template:  "ColgenInstance:(?X $>= ?Y)",
    args:      ["X":    "Partial linear expression",
		"Y":    "Partial linear expression"
	       ],
    see_also:  [($=)/2,($=<)/2,(=:=)/2,(=<)/2,(>=)/2,var_get/3], 

    summary:   "Constrains X to be greater than or equal to Y.",
    desc:      html("\
    <P>
    Logically: Constrains X to be greater than or equal to Y. X and Y
    are partial linear expressions. Partial linear expressions may
    contain terms of the form <TT>implicit_sum(+Var)</TT> in addition
    to any terms allowed within a standard linear
    expression. Variables occurring inside <TT>implicit_sum/1</TT>
    terms are taken to be subproblem variables whose instantiation
    will correspond to the coefficient of a generated master problem
    variable in this constraint. Operationally, the constraint gets
    delayed until the external solver state for ColgenInstance is
    invoked.
    </P><P>
    The $ version of the arithmetic constraints are
    provided to allow code to be written which does not specify the
    solver. They are equivalent to their colgen instance counterparts
    without the $ prefix.
    </P>
 ")
]).

:- comment(($=<)/2,  [
    template:  "ColgenInstance:(?X $=< ?Y)",
    args:      ["X":    "Partial linear expression",
		"Y":    "Partial linear expression"
	       ],
    see_also:  [($=)/2,($>=)/2,(=:=)/2,(=<)/2,(>=)/2,var_get/3],
    summary:   "Constrains X to be less than or equal to Y.",
    desc:      html("\
    <P>
    Logically: Constrains X to be less than or equal to Y.  X and Y
    are partial linear expressions. Partial linear expressions may
    contain terms of the form <TT>implicit_sum(+Var)</TT> in addition
    to any terms allowed within a standard linear
    expression. Variables occurring inside <TT>implicit_sum/1</TT>
    terms are taken to be subproblem variables whose instantiation
    will correspond to the coefficient of a generated master problem
    variable in this constraint. Operationally, the constraint gets
    delayed until the external solver state for ColgenInstance is
    invoked.
    </P><P>
    The $ version of the arithmetic constraints are provided to allow
    code to be written which does not specify the solver. They are
    equivalent to their colgen instance counterparts without the $
    prefix.
    </P>
 ")
]).

:- comment(($=)/2,  [
    template:  "ColgenInstance:(?X $= ?Y)",
    args:      ["X":    "Partial linear expression",
		"Y":    "Partial linear expression"
	       ],
    see_also:  [($=<)/2,($>=)/2,(=:=)/2,(=<)/2,(>=)/2,var_get/3],
    summary:   "Constrains X to be equal to Y.",
    desc:      html("\
    <P>
    Logically: Constrains X to be less than or equal to Y.  X and Y
    are partial linear expressions. Partial linear expressions may
    contain terms of the form <TT>implicit_sum(+Var)</TT> in addition
    to any terms allowed within a standard linear
    expression. Variables occurring inside <TT>implicit_sum/1</TT>
    terms are taken to be subproblem variables whose instantiation
    will correspond to the coefficient of a generated master problem
    variable in this constraint. Operationally, the constraint gets
    delayed until the external solver state for ColgenInstance is
    invoked.
    </P><P>
    The $ version of the arithmetic constraints are provided to allow
    code to be written which does not specify the solver. They are
    equivalent to their colgen instance counterparts without the $
    prefix.
    </P>
 ")
]).
