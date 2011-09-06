:- comment(categories, ["Constraints"]).
:- comment(summary, "Interface to gecode solver for integer finite domains").

:- comment(author, "Kish Shen").

:- comment(desc, html("<P>
   The GFD library is an interface to the gecode finite domain constraint
   solver. Gecode (www.gecode.org) is an open-source toolkit for developing
   constraint-based systems in C++, and includes a high-performance constraint 
   solver.</P><P>

   This interface provides a high degree of compatibility with the finite 
   domain portion of the IC library, and to a lesser extent, with the FD
   library as well. This means that programs originally written for the
   IC library should run with GFD with little modifications, beyond 
   renaming any explicit calls to the ic family of modules.</P><P>

   The main differences from the IC library are:
<UL>
       <LI>Real interval arithmetic and variables are not supported.

       <LI>Domain variables have finite bounds, and the maximum bounds are
       determined by gecode. Like FD, default finite bounds are given to 
       domain variables that are not explicitly given bounds.

       <LI>Constraint propagation is performed in gecode, and each propagation
       phase is atomic at the ECLiPSe level. Posting of constraints and 
       propagation of their consequences are separate in gecode. GFD use a
       demon suspended goal to perform the propagation: after the posting
       of any constraint (and other changes to the problem that needs 
       propagation), the suspended goal is scheduled and woken. When the
       woken goal is executed, propagation is perform. The goal is suspended
       at priority 10, so if the posting of the constraint is executed at
       normal priority (12), the propagation will happen immediately. However,
       if the posting is done at a priority 10 or higher, then the propagation
       is delayed, allowing multiple constraints to be posted without
       propagation. 

       <LI>GFD supports constraints that are supported by the gecode library,
       so the exact set of constraints supported is different from IC> 
       However, the same basic arithmetic operators and relations are 
       supported, allowing for very similar arithmetic expressions. Also,
       many, if not most, of the constraints in IC are supported, along with
       some constraints not supported by IC. See the detailed documentation
       for more information.

       <LI>All constraints can be called from the gfd module, and in
       addition, some constraints can be called from modules that specify
       the consistency level: gfd_gac (generalised arc consistency, aka
       domain consistency), gfd_bc (bounds consistency), gfd_vc (value
       consistency (naive)). The calls to gfd uses the default consistency 
       defined for the constraint by gecode. These consistency levels maps 
       directly to those defined for the constraints, so if gecode supports 
       different consistency levels for a constraint, GFD supports it as 
       well. In  particular (and unlike IC), most arithmetic operations can 
       be bounds (the default) as well as domain consistent.

       <LI>gfd:search/6 interfaces to gecode's search-engines, where the
       entire search is performed in gecode, and the whole search appears
       atomic at the ECLiPSe level. The syntax for search/6 is designed to
       be as compatible with IC's search/6, although there are some 
       differences. The exact equivalent to IC's search/6, where the search
       is performed at the ECLiPSe level, is available via the gfd_search
       module (in fact it shares the same code with IC's search/6). This
       provides more flexibility, but is likely to be less efficient.

       <LI>The suspension lists supported by GFD are different from IC.
       Currently, only the 'any' suspension list (for any changes to the
       variable's domain) found in FD but not IC, is supported. Not that
       the GFD constraints are implemented in gecode directly, and therefore
       do not use GFD's suspension lists. 

      <LI>Constraint expressions are parsed at run-time, so IC's eval/1 is
      not needed. However, for compatibility reasons, it is allowed in
      arithmetic expressions, but does nothing.
</UL><P>
      <P>
   The following can be used inside arithmetic constraint expressions:
   <DL>
   <DT><STRONG>X</STRONG><DD>
	    Variables.  If X is not yet a domain variable, it is turned 
	    into one.

   <DT><STRONG>123</STRONG><DD>
	    Integer constants.

   <DT><STRONG>+Expr</STRONG><DD>
	    Identity.

   <DT><STRONG>-Expr</STRONG><DD>
	    Sign change.

   <DT><STRONG>abs(Expr)</STRONG><DD>
	    The absolute value of Expr.

   <DT><STRONG>E1+E2</STRONG><DD>
	    Addition.

   <DT><STRONG>E1-E2</STRONG><DD>
	    Subtraction.

   <DT><STRONG>E1*E2</STRONG><DD>
	    Multiplication.

   <DT><STRONG>E1//E2</STRONG><DD>
	    Integer division. Truncate towards zero.

   <DT><STRONG>E1/E2</STRONG><DD>
	    Integer division (alias for compatibility).

   <DT><STRONG>E1 mod E2</STRONG><DD>
	    Integer modulus.

   <DT><STRONG>divmod(E1,E2,E3)</STRONG><DD>
	    Integer modulus, in addition E3 #= E1//E2.

   <DT><STRONG>moddiv(E1,E2,E3)</STRONG><DD>
	    Integer division, in addition E3 #= E1 mod E2.

   <DT><STRONG>Expr^2</STRONG><DD>
	    Square. Equivalent to sqr(Expr).

   <DT><STRONG>min(E1,E2)</STRONG><DD>
	    Minimum.

   <DT><STRONG>max(E1,E2)</STRONG><DD>
	    Maximum.

   <DT><STRONG>sqr(Expr)</STRONG><DD>
	    Square.  Logically equivalent to Expr*Expr.

   <DT><STRONG>sqrt(Expr)</STRONG><DD>
	    Square root (always positive). Truncated to nearest smaller integer.

   <DT><STRONG>sum(ExprCol)</STRONG><DD>
	    Sum of a collection of expressions.

   <DT><STRONG>sum(IntCol*ExprCol)</STRONG><DD>
	    Scalar product of a collection of integers and expressions.
            IntCol and ExprCol must be the same size.

   <DT><STRONG>min(ExprCol)</STRONG><DD>
	    Minimum of a collection of expressions.

   <DT><STRONG>max(ExprCol)</STRONG><DD>
	    Maximum of a collection of expressions.

   <DT><STRONG>Functional/reified constraints</STRONG><DD>
            Written without last argument, which is taken as the value of
            the expression. Only reified constraints (whose last argument
            is the 0/1 boolean) and constraints that can be written as 
            functions (last argument is a domain variable) are allowed.

   <DT><STRONG>and</STRONG><DD>
	    Reified constraint conjunction.  e.g. X&gt;3 and Y&lt;8

   <DT><STRONG>or</STRONG><DD>
	    Reified constraint disjunction.  e.g. X&gt;3 or Y&lt;8
            These are restricted to the top-level of an expression,
            and for reifiable expressions only,

   <DT><STRONG>xor</STRONG><DD>
	    Reified constraint exclusive disjunction.  e.g. X&gt;3 xor Y&lt;8
            These are restricted to the top-level of an expression,
            and for reifiable expressions only,

   <DT><STRONG>=&gt;</STRONG><DD>
	    Reified constraint implication.  e.g. X&gt;3 =&gt; Y&lt;8
            These are restricted to the top-level of an expression,
            and for reifiable expressions only,

   <DT><STRONG>neg</STRONG><DD>
	    Reified constraint negation.  e.g. neg X&gt;3
            These are restricted to the top-level of an expression,
            and for reifiable expressions only,

   <DT><STRONG>eval(Expr)</STRONG><DD>
	    Equivalent to Expr.
   </DL>
")).


%---------------------------------------------------------------------

:- comment(integers/1, [
    amode: (integers(-) is det),
    amode: (integers(+) is semidet),
%    template: "integers(?Vars)",
    args: [
	"Vars": "Variable or integer, or a list or sub-matrix of variables/integers"
    ],
    summary: "Vars' domain is the integer numbers.",
    see_also: [_:integers/1],
%    fail_if: "variables already a non-integer.",
    desc: html("<P>
   Constrain the variables to integer values.  If any variable is a non-domain
   variable, a default domain will be created for it.
")
]).

%---------------------------------------------------------------------

:- comment(is_solver_var/1, [
    amode: (is_solver_var(?) is semidet),
    args: [
	"Term": "A Term"
    ],
    summary: "Succeeds iff Term is an GFD domain variable.",
    fail_if: "Var is not an GFD domain variable.",
    desc: html("<P>
   Test if the term Term is an GFD domain variable.  Succeed if it is, fail
   otherwise.</P>
")
]).

%---------------------------------------------------------------------

:- comment(is_exact_solver_var/1, [
    amode: (is_exact_solver_var(?) is semidet),
    args: [
	"Term": "A Term"
    ],
    summary: "Succeeds iff Term is an GFD domain variable.",
    fail_if: "Var is not an GFD domain variable.",
    desc: html("<P>
   Test if the term Term is an GFD domain variable. This is an alias for
   is_solver_var/1 in GFD.
   </P>
")
]).

%---------------------------------------------------------------------

:- comment(is_solver_type/1, [
    amode: (is_solver_type(?) is semidet),
    args: [
	"Term": "A Term"
    ],
    summary: "Succeeds iff Term is a GFD domain variable or an integer.",
    fail_if: "Var is not a GFD domain variable or an integer.",
    desc: html("<P>
   Test if the term Term is a GFD domain variable or an integer.
   Succeed if it is, fail otherwise.</P>
")
]).

%---------------------------------------------------------------------

:- comment(get_bounds/3, [
    amode: (get_bounds(?, -, -) is det),
    args: [
	"Var": "A variable or a number",
	"Lo":  "Lower bound",
	"Hi":  "Upper bound"
    ],
    summary: "Retrieve the current bounds of Var.",
%    fail_if: "Var is not a variable or an integer.",
    see_also: [get_min/2, get_max/2, 
		get_integer_bounds/3, get_finite_integer_bounds/3,
		get_delta/2, get_median/2],
    desc: html("<P>
   Primitive for retrieving the upper and lower bounds of Var.  Lo and Hi
   return the minimum and maximum (respectively) of the variable's interval.
   If Var has not been declared before, it will be turned into a domain
   variable with default interval.  If Var is an integer, Lo and Hi will
   be set to Var.</P>
")
]).

%---------------------------------------------------------------------

:- comment(get_min/2, [
    amode: (get_min(?, -) is det),
    args: [
	"Var": "A variable or an integer",
	"Lo":  "Lower bound"
    ],
    summary: "Retrieve the current lower bound of Var.",
%    fail_if: "Var is not a variable or an integer.",
    see_also: [get_bounds/3, 
		get_integer_bounds/3, get_finite_integer_bounds/3],
    desc: html("<P>
   Primitive for retrieving the lower bound of Var.  Lo returns the minimum
   of the variable's interval. If Var has not been declared before, it
   will be turned into a domain variable with default interval.</P>
")
]).

%---------------------------------------------------------------------

:- comment(get_max/2, [
    amode: (get_max(?, -) is det),
    args: [
	"Var": "A variable or an integer",
	"Hi":  "Upper bound"
    ],
    summary: "Retrieve the current upper bound of Var.",
%    fail_if: "Var is not a variable or an integer.",
    see_also: [get_bounds/3, 
		get_integer_bounds/3, get_finite_integer_bounds/3],
    desc: html("<P>
   Primitive for retrieving the upper bound of Var.  Hi returns the maximum
   of the variable's interval. If Var has not been declared before, it
   will be turned into a domain variable with default interval.</P>
")
]).

%---------------------------------------------------------------------

:- comment(get_integer_bounds/3, [
    amode: (get_integer_bounds(?, -, -) is det),
    args: [
	"Var": "A variable or an integer",
	"Lo":  "Lower bound",
	"Hi":  "Upper bound"
    ],
    summary: "Retrieve the current bounds of Var.",
%    fail_if: "Var is not a variable or an integer.",
    see_also: [get_bounds/3, get_finite_integer_bounds/3],
    desc: html("<P>
   This is provided for compatibility with IC, and is an alias for 
   get_bounds/3.</P>
")
]).

%---------------------------------------------------------------------

:- comment(get_finite_integer_bounds/3, [
    amode: (get_finite_integer_bounds(?, -, -) is det),
    args: [
	"Var": "A variable or a number",
	"Lo":  "Lower bound",
	"Hi":  "Upper bound"
    ],
    summary: "Retrieve the current (finite, integral) bounds of Var.",
%    fail_if: "Var is not a variable or a number.",
    see_also: [get_bounds/3, get_integer_bounds/3],
    desc: html("<P>
   This is provided for compatibility with IC, and is an alias for 
   get_bounds/3.</P>
")
]).

%---------------------------------------------------------------------

:- comment(get_domain_size/2, [
    amode: (get_domain_size(?, -) is det),
    args: [
	"Var":   "An GFD domain variable or an integer",
        "Size":  "A variable (or integer)"
    ],
    summary: "Size is the number of integer elements in the GFD domain for Var",
    see_also: [get_delta/2],
    desc: html("<P>
   If Var is an GFD domain variable, Size will be set to the number of 
   integer values in the domain of Var.  If Var is a number, then Size 
   will be set to 1.</P><P>
"),
   exceptions: [
        5: "Var is neither a GFD variable or integer."
   ],
   fail_if: "The initial value of Size fails to unify with the returned value."
]).

%---------------------------------------------------------------------
:- comment(get_domain/2, [
    amode: (get_domain(?, -) is det),
    args: [
	"Var":    "A GFD domain variable or an integer.",
        "Domain": "A ground representation of the domain of Var."
    ],
    summary: "Returns a ground representation of the current GFD domain of a variable.",
    see_also: [get_domain_as_list/2, get_bounds/3],
    desc: html("<P>
   If Var is an integer, Domain will be unified with a singleton list
   with that integer.</P><P>

   If Var is an GFD domain variable with no holes in its domain, Domain will
   be unified with the term Lo..Hi where Lo and Hi are integers
   corresponding to the current lower and upper bounds of Var, respectively.</P><P>

   If Var is an GFD domain variable with holes in its domain, Domain will
   be unified with an ordered list of integers and/or terms Lo..Hi where Lo
   and Hi are integers; in this case the elements of the domain of Var are
   exactly those integers appearing directly in the list or falling within
   any of the intervals Lo..Hi.</P>
"),
   exceptions: [
        5: "Var is neither an IC variable or number."
   ],
   fail_if: "The initial value of DomainList fails to unify with the returned value."
]).

%---------------------------------------------------------------------
:- comment(get_domain_as_list/2, [
    amode: (get_domain_as_list(?, -) is det),
    args: [
	"Var":   "A GFD domain variable or a number",
        "DomainList":  "The domain of Var as a list of elements."
    ],
    summary: "List of all the elements in the GFD domain of Var",
    see_also: [get_domain/2, get_bounds/3],
    desc: html("<P>
   If Var is a GFD domain variable, DomainList will be set to an ordered
   list containing each element in the domain of Var.  If Var is a number,
   then DomainList will be set to a singleton list containing the number.</P><P>
"),
   exceptions: [
        5: "Var is neither a GFD variable or integer."
   ],
   fail_if: "The initial value of DomainList fails to unify with the returned value."
]).

%---------------------------------------------------------------------

:- comment(get_median/2, [
    amode: (get_median(?, -) is det),
    args: [
	"Var":    "A variable or an integer",
	"Median": "The median of the domain"
    ],
    summary: "Returns the median of the domain of the GFD domain variable Var.",
    see_also: [get_delta/2, get_bounds/3],
    desc: html("<P>
   Returns the median of the domain of Var, i.e. if there are N values in
   the domain, the N/2'th (rounded down if N is odd) value is returned.
   Note that this is different from the definition used in IC, where the
   median (a float) of the interval is returned. If Var is an integer, the 
   median is unified with that number.
"),
    eg: "\
[eclipse 2]: X :: 10..1000, get_median(X, M).

X = X{10 .. 1000}
M = 500

[eclipse 2]: A :: [2,3,100,1000], get_median(A,M).

A = A{[2, 3, 100, 1000]}
M = 3

[eclipse 2]: get_median(3, M).

M = 3
"
]).

%---------------------------------------------------------------------

:- comment(get_delta/2, [
    amode: (get_delta(?, -) is det),
    args: [
	"Var":   "A variable or a number",
	"Width": "Width of the interval"
    ],
    summary: "Returns the width of the interval of Var.",
    see_also: [get_median/2, get_bounds/3],
    desc: html("<P>
   Returns the width (Hi - Lo) of the interval of Var. If Var is an integer,
   0 will be returned as the width.</P>
")
]).

%---------------------------------------------------------------------

:- comment(get_constraints_number/2, [
    amode: (get_constraints_number(?, -) is det),
    args: [
	"Var":   "A variable or a term",
	"Degree": "Variable (instantiates to a non-negative integer)"
    ],
    summary: "Returns the number of propagators attached to the gecode"
             " variable representing Var.",
    desc: html("<P>
   Returns the number of propagators attached to the gecode variable
   representing Var, This approximates the number of constraints attach
   to the variables, and is known as the degree of the variable in the
   literature. 
</P><P>
   If Var is not a variable, a very large number (1.0Inf) is returned. If
   Var is a variable but not a domain variable, 0 will be returned.
")
]).

%---------------------------------------------------------------------

:- comment(get_weighted_degree/2, [
    amode: (get_weighted_degree(?, -) is semidet),
    args: [
	"Var":   "A domain variable",
	"WD": "Current wighted degree for variable"
    ],
    summary: "Returns the weighted degree of an existing domain variable Var.",
    desc: html("<P>
   Returns the weighted degree for a domain variable. Weighted degree is call 
   AFC (accumulated failure count) in gecode, and is a count of the number of 
   failures so far of propagators associated with the variable, plus the 
   number of propagator attached to the variable (to give reasonable 
   starting values when there are not failures yet).  This is usually used 
   in selecting a variable for labelling.
")
]).

%---------------------------------------------------------------------

:- comment(get_regret_lwb/2, [
    amode: (get_regret_lwb(?, -) is semidet),
    args: [
	"Var":   "A domain variable",
	"Regret": "Regret value"
    ],
    summary: "Returns the regret value for the lower bound of Var.",
    desc: html("<P>
   Returns the regret value for the lower bound of the variable, that is, 
   the magnitude of the difference between the lowest and second lowest value 
   in the variable's domain. This can be used in selecting a variable for 
   labelling.
")
]).

%---------------------------------------------------------------------

:- comment(get_regret_upb/2, [
    amode: (get_regret_upb(?, -) is semidet),
    args: [
	"Var":   "A domain variable",
	"Regret": "Regret value"
    ],
    summary: "Returns the regret value for the upper bound of Var.",
    desc: html("<P>
   Returns the regret value for the upper bound of the variable, that is, 
   the magnitude of the difference between the largest and second largest value 
   in the variable's domain. This can be used in selecting a variable for 
   labelling.
")
]).

%---------------------------------------------------------------------

:- comment(is_in_domain/2, [
    amode: (is_in_domain(++,?) is semidet),
    args: [
        "Val": "An integer",
        "Var": "A GFD domain variable or an integer"
    ],
    summary: "Succeeds iff Val is in the domain of Var",
    exceptions:[5: "Val is not an integer"],
    see_also:[is_in_domain/3],
    desc: html("<P>
   Low level predicate which succeeds when Val is in the domain of Var.
")
]).

%---------------------------------------------------------------------

:- comment(is_in_domain/3, [
    amode: (is_in_domain(++,?,-) is det),
    args: [
        "Val": "A number",
    	"Var": "A GFD domain variable or an integer",
        "Result": "An atom"
    ],
    summary: "Binds Result to indicate presence of Val in domain of Var",
    exceptions:[5: "Val is not an integer"],
    see_also:[is_in_domain/2],
    desc: html("<P>
   Low level predicate which succeeds when Val is in the domain of Var with
   Result bound to the atom 'yes'.  When Val is not in the domain of Var,
   the predicate succeeds binding Result to the atom 'no'. 
")
]).

%---------------------------------------------------------------------

:- comment((#::)/2, [
    amode: #::(?, ++),
    template: "?Vars #:: ++Domain",
    args: [
	"Vars":   "Variable or collection (a la collection_to_list/2) of variables",
	"Domain": "Domain specification"
    ],
    summary: "Constrain Vars to have the domain Domain.",
    see_also: [(::)/2],
    desc: html("<P>
   Alias of ::/2. See ::/2 for more details.
")]).

:- comment((::)/2, [
    amode: ::(?, ++),
    template: "?Vars :: ++Domain",
    args: [
	"Vars":   "Variable or collection (a la collection_to_list/2) of variables",
	"Domain": "Domain specification"
    ],
    summary: "Constrain Vars to have the domain Domain.",
    see_also: [integers/1, reals/1, _:(::)/2, (::)/3, (#::)/2, ($::)/2],
    desc: html("<P>
   Constrains Vars to take only values from the domain specified by Domain.  
   Vars may be a variable or a collection of variables (as accepted by 
   collection_to_list/2).  Domain can be specified as a simple range Lo .. Hi, 
   or as a list of sub-ranges and/or individual elements. All domain elements
   must be integers within the range allowed by gecode.
<P>
   For instance:
<PRE>
     X :: 0..1                  % boolean
     X :: -1..5                 % integer between -1 and 5
     X :: [0..3, 5, 8..10]      % any integer from 0 to 10 except 4 and 6
     [X, Y, Z] :: 1..8          % apply domain to X, Y and Z
     M[2..4, 5] :: 1..8         % apply to rows 2..4, column 5 of matrix M
     X :: [0.0..5.0, 7.0..9.0]  % Type error
     X :: [a, b, c]             % Type error
</PRE>
"),
    eg: "\
[eclipse 2]: X :: 0..1.

X = X{[0, 1]}

[eclipse 2]: X :: -1..5.

X = X{[-1 .. 5]}

[eclipse 2]: X :: 0.0..1.0.
type error in X :: 0.0 .. 1.0
Abort

[eclipse 2]: [X,Y] :: [1..10, -1, 0, 7, 21].

X = X{[-1 .. 10, 21]}
Y = Y{[-1 .. 10, 21]}
"
]).

:- comment((::)/3, [
    amode: ::(?, ++, ?),
    template: "::(?Var, ++Domain, ?Bool)",
    args: [
	"Var":    "Variable",
	"Domain": "Domain specification",
        "Bool":   "Reified truth value"
    ],
    summary: "Reflect into Bool the truth of Var having the domain Domain.",
    see_also: [_:(::)/2, (::)/2],
    desc: html("<P>
   Provides a reified form of the ::/2 domain assignment predicate.  This
   reified ::/3 is defined only to work for one variable (unlike ::/2).
<P>
   For a single variable, V, the Bool will be instantiated to 0 if the
   current domain of V does not intersect with Domain.  It will be
   instantiated to 1 iff the domain of V is wholly contained within Domain.
   Finally the Boolean will remain a domain variable in the range 0..1, if
   neither of the above two conditions hold.
<P>
   Instantiating Bool to 1, will cause the constraint to behave exactly like
   ::/2.  Instantiating Bool to 0 will cause Domain to be excluded from the
   domain of the variable.
<P>
   Note that calling the reified form of :: will result in the Variable
   becoming a domain variable, even if Bool is uninstantiated.
<P>
   Further note that, like other reified predicates, :: can be used infix in
   a GFD  expression, e.g. B #= (X :: [1..10]) is equivalent to
   ::(X, [1..10], B).
"),
    eg: "\
[eclipse 2]: ::(X, [1..10, 12..30], 1).

X = X{[1 .. 10, 12 .. 30]}


[eclipse 2]: ::(X, [1..10, 12..30], 0).

X = X{[-1000000 .. 0, 11, 31 .. 1000000]}

[eclipse 2]: ::(X, [1..10, 12..30], B).

X = X{[-1000000 .. 1000000]}
B = B{[0, 1]}

[eclipse 2]: gfd:( B #= (X :: [1..10, 12..30])).

B = B{[0, 1]}
X = X{[-1000000 .. 1000000]}

"
]).

:- comment((#::)/3, [
    amode: #::(?, ++, ?),
    template: "#::(?Var, ++Domain, ?Bool)",
    args: [
	"Var":    "Variable",
	"Domain": "Domain specification",
        "Bool":   "Reified truth value"
    ],
    summary: "Reflect into Bool the truth of Var having the domain Domain.",
    see_also: [_:(::)/2, (::)/3],
    desc: html("<P>
  An alias for ::/3. See ::/3 for more details.</P>
")
]).

%---------------------------------------------------------------------

:- comment((#=)/2, [
    amode: #=(?, ?),
    template: "<ConsistencyModule:> ?ExprX #= ?ExprY",
    args: [
	"ExprX": "Integer arithmetic expression",
	"ExprY": "Integer arithmetic expression"
    ],
    summary: "ExprX is equal to ExprY.",
    see_also: [(#<)/2, (#=<)/2, (#>=)/2, (#>)/2, (#\=)/2,
               (#=)/3, _:(#=)/2],
    desc: html("<P>
   Constrains ExprX and ExprY to be equal.  Also constrains all variables
   appearing in ExprX and ExprY to be domain variables and checks that all 
   constants and ground sub-expressions are integers.</P><P>

   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: gfd_bc
   for bounds consistency, and gfd_gac for domain (generalised arc) 
   consistency. Note that some operators (integer division, modulus)
   only support bounds consistency and will be propagated with bounds
   consistency even when posted with gfd_gac.
")
]).
:- comment((#=)/3, [
    amode: #=(?, ?, ?),
    template: "<ConsistencyModule:> #=(?ExprX, ?ExprY, ?Bool)",
    args: [
	"ExprX": "Integer arithmetic expression",
	"ExprY": "Integer arithmetic expression",
	"Bool": "Reified truth value of the constraint"
    ],
    summary: "Reified ExprX is equal to ExprY.",
    see_also: [(#<)/3, (#=<)/3, (#>=)/3, (#>)/3, (#\=)/3,
               (#=)/2, _:(#=)/3],
    desc: html("<P>
   This predicate is a reified constraint: it succeeds if and only if the
   truth value of its associated constraint (the constraint with arity one
   less and the same arguments except for B) is equal to B, where the value
   0 means false and 1 true.  This constraint can be used both to test the
   validity of the associated constraint (entailment test) and to impose
   this constraint or its negation.  For the former, B will be instantiated
   as soon as either the associated constraint or its negation is subsumed
   by the current state of its domain variables.  For the latter, when B is
   instantiated, then depending on its value, either the associated
   constraint or its negation will be imposed on its arguments.</P>

   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: gfd_bc
   for bounds consistency, and gfd_gac for domain (generalised arc) 
   consistency. Note that some operators (integer division, modulus)
   only support bounds consistency and will be propagated with bounds
   consistency even when posted with gfd_gac.
")
]).

%---------------------------------------------------------------------

:- comment((#>=)/2, [
    amode: #>=(?, ?),
    template: "<ConsistencyModule:> ?ExprX #>= ?ExprY",
    args: [
	"ExprX": "Integer arithmetic expression",
	"ExprY": "Integer arithmetic expression"
    ],
    summary: "ExprX is greater than or equal to ExprY.",
    see_also: [(#<)/2, (#=<)/2, (#=)/2, (#>)/2, (#\=)/2,
               (#>=)/3, _:(#>=)/2],
    desc: html("<P>
   Constrains ExprX to be greater than or equal to ExprY.  Also constrains
   all variables appearing in ExprX and ExprY to be domain variables and 
   checks that all constants and ground sub-expressions are integers.</P><P>

   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: gfd_bc
   for bounds consistency, and gfd_gac for domain (generalised arc) 
   consistency. Note that some operators (integer division, modulus)
   only support bounds consistency and will be propagated with bounds
   consistency even when posted with gfd_gac.
")
]).

:- comment((#>=)/3, [
    amode: #>=(?, ?, ?),
    template: "<ConsistencyModule:> #>=(?ExprX, ?ExprY, ?Bool)",
    args: [
	"ExprX": "Integer arithmetic expression",
	"ExprY": "Integer arithmetic expression",
	"Bool": "Reified truth value of the constraint"
    ],
    summary: "Reified ExprX is greater than or equal to ExprY.",
    see_also: [(#<)/3, (#=<)/3, (#=)/3, (#>)/3, (#\=)/3,
               (>=)/3, (#>=)/2, _:(#>=)/3],
    desc: html("<P>
   This predicate is a reified constraint: it succeeds if and only if the
   truth value of its associated constraint (the constraint with arity one
   less and the same arguments except for B) is equal to B, where the value
   0 means false and 1 true.  This constraint can be used both to test the
   validity of the associated constraint (entailment test) and to impose
   this constraint or its negation.  For the former, B will be instantiated
   as soon as either the associated constraint or its negation is subsumed
   by the current state of its domain variables.  For the latter, when B is
   instantiated, then depending on its value, either the associated
   constraint or its negation will be imposed on its arguments.</P><P>

   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: gfd_bc
   for bounds consistency, and gfd_gac for domain (generalised arc) 
   consistency. Note that some operators (integer division, modulus)
   only support bounds consistency and will be propagated with bounds
   consistency even when posted with gfd_gac.
")
]).

%---------------------------------------------------------------------

:- comment((#=<)/2, [
    amode: #=<(?, ?),
    template: "<ConsistencyModule:> ?ExprX #=< ?ExprY",
    args: [
	"ExprX": "Integer arithmetic expression",
	"ExprY": "Integer arithmetic expression"
    ],
    summary: "ExprX is less than or equal to ExprY.",
    see_also: [(#<)/2, (#=)/2, (#>=)/2, (#>)/2, (#\=)/2,
               (#=<)/3, _:(#=<)/2],
    desc: html("<P>
   Constrains ExprX to be less than or equal to ExprY.  Also constrains all
   variables appearing in ExprX and ExprY to be integral and checks that all
   constants are integers.</P><P>

   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: gfd_bc
   for bounds consistency, and gfd_gac for domain (generalised arc) 
   consistency. Note that some operators (integer division, modulus)
   only support bounds consistency and will be propagated with bounds
   consistency even when posted with gfd_gac.
")
]).
:- comment((#=<)/3, [
    amode: #=<(?, ?, ?),
    template: "<ConsistencyModule:> #=<(?ExprX, ?ExprY, ?Bool)",
    args: [
	"ExprX": "Integer arithmetic expression",
	"ExprY": "Integer arithmetic expression",
	"Bool": "Reified truth value of the constraint"
    ],
    summary: "Reified ExprX is less than or equal to ExprY.",
    see_also: [(#<)/3, (#=)/3, (#>=)/3, (#>)/3, (#\=)/3,
               (#=<)/2, _:(#=<)/3],
    desc: html("<P>
   This predicate is a reified constraint: it succeeds if and only if the
   truth value of its associated constraint (the constraint with arity one
   less and the same arguments except for B) is equal to B, where the value
   0 means false and 1 true.  This constraint can be used both to test the
   validity of the associated constraint (entailment test) and to impose
   this constraint or its negation.  For the former, B will be instantiated
   as soon as either the associated constraint or its negation is subsumed
   by the current state of its domain variables.  For the latter, when B is
   instantiated, then depending on its value, either the associated
   constraint or its negation will be imposed on its arguments.</P><P>

   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: gfd_bc
   for bounds consistency, and gfd_gac for domain (generalised arc) 
   consistency. Note that some operators (integer division, modulus)
   only support bounds consistency and will be propagated with bounds
   consistency even when posted with gfd_gac.
")
]).

%---------------------------------------------------------------------

:- comment((#>)/2, [
    amode: #>(?, ?),
    template: "<ConsistencyModule:> ?ExprX #> ?ExprY",
    args: [
	"ExprX": "Integer arithmetic expression",
	"ExprY": "Integer arithmetic expression"
    ],
    summary: "ExprX is strictly greater than ExprY.",
    see_also: [(#<)/2, (#=<)/2, (#=)/2, (#>=)/2, (#\=)/2,
               (#>)/3, _:(#>)/2],
    desc: html("<P>
   Constrains ExprX to be greater than ExprY.  Also constrains all variables
   appearing in ExprX and ExprY to be domain variables and checks that all 
   constants and ground sub-expressions are integers.</P><P>

   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: gfd_bc
   for bounds consistency, and gfd_gac for domain (generalised arc) 
   consistency. Note that some operators (integer division, modulus)
   only support bounds consistency and will be propagated with bounds
   consistency even when posted with gfd_gac.
")
]).
:- comment((#>)/3, [
    amode: #>(?, ?, ?),
    template: "<ConsistencyModule:> #>(?ExprX, ?ExprY, ?Bool)",
    args: [
	"ExprX": "Integer arithmetic expression",
	"ExprY": "Integer arithmetic expression",
	"Bool": "Reified truth value of the constraint"
    ],
    summary: "Reified ExprX is strictly greater than ExprY.",
    see_also: [(#<)/3, (#=<)/3, (#=)/3, (#>=)/3, (#\=)/3,
               (#>)/2, _:(#>)/3],
    desc: html("<P>
   This predicate is a reified constraint: it succeeds if and only if the
   truth value of its associated constraint (the constraint with arity one
   less and the same arguments except for B) is equal to B, where the value
   0 means false and 1 true.  This constraint can be used both to test the
   validity of the associated constraint (entailment test) and to impose
   this constraint or its negation.  For the former, B will be instantiated
   as soon as either the associated constraint or its negation is subsumed
   by the current state of its domain variables.  For the latter, when B is
   instantiated, then depending on its value, either the associated
   constraint or its negation will be imposed on its arguments.</P><P>

   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: gfd_bc
   for bounds consistency, and gfd_gac for domain (generalised arc) 
   consistency. Note that some operators (integer division, modulus)
   only support bounds consistency and will be propagated with bounds
   consistency even when posted with gfd_gac.
")
]).

%---------------------------------------------------------------------

:- comment((#<)/2, [
    amode: #<(?, ?),
    template: "<ConsistencyModule:> ?ExprX #< ?ExprY",
    args: [
	"ExprX": "Integer arithmetic expression",
	"ExprY": "Integer arithmetic expression"
    ],
    summary: "ExprX is less than ExprY.",
    see_also: [(#=<)/2, (#=)/2, (#>=)/2, (#>)/2, (#\=)/2,
               (#<)/3, _:(#<)/2],
    desc: html("<P>
   Constrains ExprX to be less than ExprY.  Also constrains all variables
   appearing in ExprX and ExprY to be domain variables and checks that all 
   constants and ground sub-expressions are integers.</P><P>

   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: gfd_bc
   for bounds consistency, and gfd_gac for domain (generalised arc) 
   consistency. Note that some operators (integer division, modulus)
   only support bounds consistency and will be propagated with bounds
   consistency even when posted with gfd_gac.
")
]).

:- comment((#<)/3, [
    amode: #<(?, ?, ?),
    template: "<ConsistencyModule:> #<(?ExprX, ?ExprY, ?Bool)",
    args: [
	"ExprX": "Integer arithmetic expression",
	"ExprY": "Integer arithmetic expression",
	"Bool": "Reified truth value of the constraint"
    ],
    summary: "Reified ExprX is less than ExprY.",
    see_also: [(#=<)/3, (#=)/3, (#>=)/3, (#>)/3, (#\=)/3,
               (<)/3, (#<)/2, _:(#<)/3],
    desc: html("<P>
   This predicate is a reified constraint: it succeeds if and only if the
   truth value of its associated constraint (the constraint with arity one
   less and the same arguments except for B) is equal to B, where the value
   0 means false and 1 true.  This constraint can be used both to test the
   validity of the associated constraint (entailment test) and to impose
   this constraint or its negation.  For the former, B will be instantiated
   as soon as either the associated constraint or its negation is subsumed
   by the current state of its domain variables.  For the latter, when B is
   instantiated, then depending on its value, either the associated
   constraint or its negation will be imposed on its arguments.</P>

   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: gfd_bc
   for bounds consistency, and gfd_gac for domain (generalised arc) 
   consistency. Note that some operators (integer division, modulus)
   only support bounds consistency and will be propagated with bounds
   consistency even when posted with gfd_gac.
")
]).

%---------------------------------------------------------------------

:- comment((#\=)/2, [
    amode: #\=(?, ?),
    template: "<ConsistencyModule:> ?ExprX #\\= ?ExprY",
    args: [
	"ExprX": "Integer arithmetic expression",
	"ExprY": "Integer arithmetic expression"
    ],
    summary: "ExprX is not equal to ExprY.",
    see_also: [(#<)/2, (#=<)/2, (#=)/2, (#>=)/2, (#>)/2,
               (#\=)/3, _:(#\=)/2],
    desc: html("<P>
   Constrains ExprX to be not equal to ExprY.  Also constrains all variables
   appearing in ExprX and ExprY to be domain variable and checks that all 
   constants and ground sub-expressions are integers.</P><P>

   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: gfd_bc
   for bounds consistency, and gfd_gac for domain (generalised arc) 
   consistency. Note that some operators (integer division, modulus)
   only support bounds consistency and will be propagated with bounds
   consistency even when posted with gfd_gac.
")
]).

:- comment((#\=)/3, [
    amode: #\=(?, ?, ?),
    template: "<ConsistencyModule:> #\\=(?ExprX, ?ExprY, ?Bool)",
    args: [
	"ExprX": "Integer arithmetic expression",
	"ExprY": "Integer arithmetic expression",
	"Bool": "Reified truth value of the constraint"
    ],
    summary: "Reified ExprX is not equal to ExprY.",
    see_also: [(#<)/3, (#=<)/3, (#=)/3, (#>=)/3, (#>)/3,
               (=\=)/3, (#\=)/2, _:(#\=)/3],
    desc: html("<P>
   This predicate is a reified constraint: it succeeds if and only if the
   truth value of its associated constraint (the constraint with arity one
   less and the same arguments except for B) is equal to B, where the value
   0 means false and 1 true.  This constraint can be used both to test the
   validity of the associated constraint (entailment test) and to impose
   this constraint or its negation.  For the former, B will be instantiated
   as soon as either the associated constraint or its negation is subsumed
   by the current state of its domain variables.  For the latter, when B is
   instantiated, then depending on its value, either the associated
   constraint or its negation will be imposed on its arguments.</P><P>

   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: gfd_bc
   for bounds consistency, and gfd_gac for domain (generalised arc) 
   consistency. Note that some operators (integer division, modulus)
   only support bounds consistency and will be propagated with bounds
   consistency even when posted with gfd_gac.
")
]).


%---------------------------------------------------------------------

:- comment(indomain/1, [
    amode: indomain(?),
    args: [
    	"Var": "An GFD domain variable or an integer"
    ],
    resat: "Yes.",
    summary: "Instantiates a domain GFD variable to an element of its domain.",
    see_also: [indomain/2, labeling/1, (::)/2, _:indomain/1],
    desc: html("<P>
   Simple predicate for instantiating a GFD domain variable to an element
   of its domain.  It starts with the smallest element, and upon
   backtracking tries successive elements until the entire domain has been
   explored, at which point the predicate fails.</P><P>

   If Var is already a ground integer, then this predicate simply succeeds
   exactly once without leaving a choicepoint.</P><P>

   Note that this predicate is not very efficient, because the whole domain 
   for Var is obtained from gecode at the start. Use indomain/2 for more
   efficient ways to set Var's value. 
")
]).

%---------------------------------------------------------------------

:- comment(labeling/1, [
    amode: labeling(+),
    args: [
    	"Vars": "A collection (a la collection_to_list/2) of integer IC variables or integers"
    ],
    resat: "Yes.",
    summary: "Instantiates all variables in a collection to elements of their domains.",
    see_also: [indomain/2, _:labeling/1, collection_to_list/2],
    desc: html("<P>
   Simple predicate for instantiating a collection of GFD domain variables
   to elements of their domains.  (Integers are also allowed in the
   collection; they are effectively ignored.)  The variables are
   instantiated in the order in which they appear in the collection; the
   implementation is essentially:
<PRE>
	labeling(Vars) :-
		collection_to_list(Vars, List),
		( foreach(Var,List) do
		    indomain(Var,min)
		).
</PRE></P>
")
]).

%---------------------------------------------------------------------

:- comment(alldifferent/1, [
    amode: alldifferent(+),
    template: "<ConsistencyModule:> alldifferent(+Vars)",
    args: [
    	"Vars": "A collection (a la collection_to_list/2) of variables or integers"
    ],
    summary: "All elements of Vars are different.",
    see_also: [(#\=)/2, collection_to_list/2],
    desc: html("<P>
   Constrains all elements of a collection to be different from each other.
   Semantically, all elements of the collection are pairwise different.

   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: 
   gfd_vc for value consistency (naive), gfd_bc for bounds consistency, 
   and gfd_gac for domain (generalised arc) consistency. 
")
]).

%---------------------------------------------------------------------

:- comment(alldifferent_cst/2, [
    amode: alldifferent_cst(+,++),
    template: "<ConsistencyModule:> alldifferent_offsets(+Vars,++Offsets)",
    args: [
    	"Vars": "A collection (a la collection_to_list/2) of variables or integers",
        "Offsets": "A collection (a la collection_to_list/2) of integers, with"
                   " the same cardinality as Vars."
    ],
    summary: "The values of each element plus corresponding offset are pair-wised different.",
    see_also: [(#\=)/2, collection_to_list/2],
    desc: html("<P>
   Constrains all elements of Vars plus its corresponding offset value in
   Offset to be different. That is, 
<PRE>
        Vari + Offseti #\\= Varj + Offsetj, i #\\= j
</PRE>
   where Vari, Offseti are the i'th element of Vars and Offsets, and
   Varj, Offsetj are the j'th element.</P><P>

   This constraint is also known as alldifferent_cst in the global constraints 
   catalog.</P><P>

   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: 
   gfd_vc for value consistency (naive), gfd_bc for bounds consistency, 
   and gfd_gac for domain (generalised arc) consistency. 
")
]).

%---------------------------------------------------------------------

:- comment(max/3, [
    summary:"Max is the maximum of X and Y.",
    template: "<ConsistencyModule:> max(?X,?Y,?Max)",
    amode:max(?,?,?),
    args:[
        "X":"An integer or domain variable",
        "Y":"An integer or domain variable",
        "Max":"An integer or domain variable"
    ],
    desc:html("\
   Constrains Max to be the maximum of X and Y. 
   </P><P> 
   You may find it more convenient to embed <TT>max(X,Y)</TT> in a
   constraint expression, but the cost of posting max(X,Y,Max) is
   likely to be cheaper if X and Y are integers or domain variables.
   </P><P> 
   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: 
   gfd_bc for bounds consistency, and gfd_gac for domain (generalised 
   arc) consistency. 
"),
    see_also: [min/3]
]).

%---------------------------------------------------------------------

:- comment(min/3, [
    summary:"Min is the minimum of X and Y.",
    template: "<ConsistencyModule:> min(?X,?Y,?Min)",
    amode:min(?,?,?),
    args:[
        "X":"An integer or domain variable",
        "Y":"An integer or domain variable",
        "Min":"An integer or domain variable"
    ],
    desc:html("\
   Constrains Min to be the minimum of X and Y. 
   </P><P> 
   You may find it more convenient to embed <TT>min(X,Y)</TT> in a
   constraint expression, but the cost of posting min(X,Y,Min) is
   likely to be cheaper if X and Y are integers or domain variables.
   </P><P> 
   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: 
   gfd_bc for bounds consistency, and gfd_gac for domain (generalised 
   arc) consistency. 
"),
    see_also: [max/3]
]).

%---------------------------------------------------------------------

:- comment(plus/3, [
    summary:"Constrains Z to X + Y.",
    template: "<ConsistencyModule:> plus(?X,?Y,?Z)",
    amode:mult(?,?,?),
    args:[
        "X":"An integer or domain variable",
        "Y":"An integer or domain variable",
        "Z":"An integer or domain variable"
    ],
    desc:html("\
   Constrains Z to be the the sum of X and Y (X+Y). 
   </P><P> 
   You may find it more convenient to embed <TT>X+Y</TT> in a
   constraint expression, but the cost of posting <TT>plus(X,Y,Z)</TT> is
   likely to be cheaper if X and Y are integers or domain variables.
   </P><P> 
   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: 
   gfd_bc for bounds consistency, and gfd_gac for domain (generalised 
   arc) consistency. 
"),
    see_also: [mult/3,divide/3,mod/3]
]).

%---------------------------------------------------------------------

:- comment(mult/3, [
    summary:"Constrains Z to X * Y.",
    template: "<ConsistencyModule:> mult(?X,?Y,?Z)",
    amode:mult(?,?,?),
    args:[
        "X":"An integer or domain variable",
        "Y":"An integer or domain variable",
        "Z":"An integer or domain variable"
    ],
    desc:html("\
   Constrains Z to be the the product of X and Y (X*Y). 
   </P><P> 
   You may find it more convenient to embed <TT>X*Y</TT> in a
   constraint expression, but the cost of posting <TT>mult(X,Y,Z)</TT> is
   likely to be cheaper if X and Y are integers or domain variables.
   </P><P> 
   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: 
   gfd_bc for bounds consistency, and gfd_gac for domain (generalised 
   arc) consistency. 
"),
    see_also: [plus/3,divide/3,mod/3]
]).

%---------------------------------------------------------------------

:- comment(mod/3, [
    summary:"Constrains Z to X mod Y.",
    template: "<ConsistencyModule:> mod(?X,?Y,?Z)",
    amode:mult(?,?,?),
    args:[
        "X":"An integer or domain variable",
        "Y":"An integer or domain variable",
        "Z":"An integer or domain variable"
    ],
    desc:html("\
   Constrains Z to be the modulus of X and Y (X mod Y). 
   </P><P> 
   You may find it more convenient to embed <TT>X mod Y</TT> in a
   constraint expression, but the cost of posting <TT>mod(X,Y,Z)</TT> is
   likely to be cheaper if X and Y are integers or domain variables.
   </P><P> 
   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: 
   gfd_bc for bounds consistency.
"),
    see_also: [plus/3,divide/3,mult/3]
]).

%---------------------------------------------------------------------

:- comment(divide/3, [
    summary:"Constrains Z to X // Y (integer division, round towards 0).",
    template: "<ConsistencyModule:> divide(?X,?Y,?Z)",
    amode:divide(?,?,?),
    args:[
        "X":"An integer or domain variable",
        "Y":"An integer or domain variable",
        "Z":"An integer or domain variable"
    ],
    desc:html("\
   Constrains Z to be the integer quotient of X and Y (X // Y). Z is rounded 
   towards 0.
   </P><P> 
   You may find it more convenient to embed <TT>X // Y</TT> in a
   constraint expression, but the cost of posting <TT>divide(X,Y,Z)</TT> is
   likely to be cheaper if X and Y are integers or domain variables.
   </P><P> 
   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: 
   gfd_bc for bounds consistency.
"),
    see_also: [plus/3,mod/3,mult/3]
]).

%---------------------------------------------------------------------

:- comment(divmod/4, [
    summary:"Constrains Q to X // Y, and M to X mod Y.",
    template: "<ConsistencyModule:> divmod(?X,?Y,?Z)",
    amode:divmod(?,?,?,?),
    args:[
        "X":"An integer or domain variable",
        "Y":"An integer or domain variable",
        "Q":"An integer or domain variable",
        "M":"An integer or domain variable"
    ],
    desc:html("\
   Constrains Q to be the integer quotient of X and Y (X // Y), and M to
   be the modulus of X and Y (X mod Y). Q is rounded towards 0.
   </P><P>
   You may find it more convenient to embed <TT>divmod(X,Y,Q)</TT> or
   <TT>moddiv(X,Y,M)</TT> in a constraint expression, but the cost of 
   posting <TT>divmod(X,Y,Q,M)</TT> is likely to be cheaper if X, Y, 
   Q and M are integers or domain variables.
   </P><P> 
   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: 
   gfd_bc for bounds consistency.
")
]).

%---------------------------------------------------------------------

:- comment(sqrt/2, [
    summary:"Constrains Y to be the (non-negative integer) square root of X.",
    template: "<ConsistencyModule:> sqrt(?X,?Y)",
    amode:sqrt(?,?),
    args:[
        "X":"An integer or domain variable",
        "Y":"An integer or domain variable"
    ],
    desc:html("\
   Constrains Y to be the non-negative integer square root of X. X is
   the truncated integer value if the exact square root is not an integer.   
   </P><P> 
   You may find it more convenient to embed <TT>sqrt(X)</TT> in a
   constraint expression, but the cost of posting <TT>sqrt(X,Y)</TT> is
   likely to be cheaper if X and Y are integers or domain variables.
   </P><P> 
   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: 
   gfd_bc for bounds consistency, and gfd_gac for domain (generalised 
   arc) consistency. 
"),
    see_also: [sqr/2]
]).


%---------------------------------------------------------------------

:- comment(sqr/2, [
    summary:"Constrains Y to be the square of X (X*X).",
    template: "<ConsistencyModule:> sqr(?X,?Y)",
    amode:sqr(?,?),
    args:[
        "X":"An integer or domain variable",
        "Y":"An integer or domain variable"
    ],
    desc:html("\
   Constrains Y to be the square of X (X*X). 
   </P><P> 
   You may find it more convenient to embed <TT>sqr(X)</TT> or <TT>X^2</TT>
   in a constraint expression, but the cost of posting <TT>sqr(X,Y)</TT> is
   likely to be cheaper if X and Y are integers or domain variables.
   </P><P> 
   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: 
   gfd_bc for bounds consistency, and gfd_gac for domain (generalised 
   arc) consistency. 
"),
    see_also: [sqrt/2]
]).

%---------------------------------------------------------------------

:- comment(abs/2, [
    summary:"Constrains Y to be the absolute value of X.",
    template: "<ConsistencyModule:> abs(?X,?Y)",
    amode:sqr(?,?),
    args:[
        "X":"An integer or domain variable",
        "Y":"An integer or domain variable"
    ],
    desc:html("\
   Constrains Y to be the absolute value of X. 
   </P><P> 
   You may find it more convenient to embed <TT>abs(X)</TT> in a 
   constraint expression, but the cost of posting <TT>abs(X,Y)</TT> is
   likely to be cheaper if X and Y are integers or domain variables.
   </P><P> 
   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: 
   gfd_bc for bounds consistency, and gfd_gac for domain (generalised 
   arc) consistency. 
"),
    see_also: [sqrt/2]
]).

%---------------------------------------------------------------------

:- comment(maxlist/2, [
    summary:"Max is the maximum of the values in Collection",
    template: "<ConsistencyModule:> maxlist(+Collection,?Max)",
    amode:maxlist(+,?),
    args:[
	"Collection":"A collection (a la collection_to_list/2) of integers or domain variables",
	"Max":"Variable or integer"
    ],
    desc:html("\
	Max is the maximum of the values in Collection.</P><P>

        You may find it more convenient to embed <TT>maxlist(Vars)</TT> in a
        constraint expression.
        </P><P> 
        ConsistencyModule is the optional module specification to give the 
        consistency level for the propagation for this constraint: 
        gfd_bc for bounds consistency, and gfd_gac for domain (generalised 
        arc) consistency. 

"),
    see_also:[minlist/2,sumlist/2,collection_to_list/2]
    ]).

:- comment(max/2, [
    amode: max(+, ?),
    args: [
	"Vars": "Collection (a la collection_to_list/2) of variables or numbers (NOT arbitrary expressions)",
	"Max":  "Maximum element of Vars"
    ],
    summary: "Constrains Max to be the largest element in Vars.",
    see_also: [min/2, collection_to_list/2],
    desc: html("<P>
   An alias for maxlist/2. provided for compatibility with IC.</P>

   You may find it more convenient to embed <TT>max(Vars)</TT> in a
   constraint expression.
")
]).

%---------------------------------------------------------------------

:- comment(minlist/2, [
    summary:"Min is the minimum of the values in Collection",
    amode:minlist(+,?),
    template: "<ConsistencyModule:> minlist(+Collection,?Max)",
    args:[
	"Collection":"A collection (a la collection_to_list/2) of integers or domain variables",
	"Min":"Variable or integer"
    ],
    desc:html("\
    	Min is the minimum of the values in Collection.</P><P>  

        You may find it more convenient to embed <TT>minlist(Vars)</TT> in a
        constraint expression.
        </P><P> 
        ConsistencyModule is the optional module specification to give the 
        consistency level for the propagation for this constraint: 
        gfd_bc for bounds consistency, and gfd_gac for domain (generalised 
        arc) consistency. 
"),
    see_also:[maxlist/2,sumlist/2,collection_to_list/2]
    ]).

:- comment(min/2, [
    amode: min(+, ?),
    args: [
	"Vars": "Collection (a la collection_to_list/2) of variables or numbers (NOT arbitrary expressions)",
	"Min":  "Minimum element of Vars"
    ],
    summary: "Constrains Min to be the smallest element in Vars.",
    see_also: [max/2, collection_to_list/2],
    desc: html("<P>
   An alias for minlist/2. provided for compatibility with IC.</P>

   You may find it more convenient to embed <TT>min(Vars)</TT> in a
   constraint expression.
")
]).

%----------------------------------------------------------------------

:- comment(sumlist/2, [
    summary:"The sum (Collection) or scalar product (IntCollection*Collection) of the Collection elements is Sum",
    template: "<ConsistencyModule:> sumlist(+Collection,?Sum)",
    amode:sumlist(+,?),
    args:[
	"Collection or Coeffs*Collection":
        "Collection: collection of N integers or domain variables. Coeffs: collection of N integers."
	"Sum":"Variable or integer"
    ],
    desc:html("<P>\
          Constrains Sum to be the sum of the elements in Collection if
          the first argument is a collection of integers or domain variables.
	  </P><P>
          Constrains Sum to be the scalar product of a collection of integers 
          and a collection of integers or domain variables if the first
          argument is Coeffs*Collection. Coeffs and Collection
          must have the same number of elements, and the scalar product 
          is the sum of the coefficients in Coeffs with the corresponding
          element in Collection.
	  </P><P>
	  Any input variables which are not already domain variable will be
          turn into domain variables with default bounds.</P><P>
	  </P><P>
          You may find it more convenient to embed <TT>sumlist(Vars)</TT> in a
          constraint expression.
	  </P><P>
          ConsistencyModule is the optional module specification to give the 
          consistency level for the propagation for this constraint: 
          gfd_bc for bounds consistency, and 
          gfd_gac for domain (generalised arc) consistency.") 
    ]).

:- comment(sum/2, [
    summary:"The sum (Collection) or scalar product (IntCollection*Collection) of the Collection elements is Sum",
    template: "<ConsistencyModule:> sum(+Collection,?Sum)",
    amode:sum(+,?),
    args:[
	"Collection or Coeffs*Collection":
        "Collection: collection of N integers or domain variables. Coeffs: collection of N integers."
	"Sum":"Variable or integer"
    ],
    desc:html("<P>\
   An alias for sumlist/2. provided for compatibility with IC.</P>

   You may find it more convenient to embed <TT>sum(Vars)</TT> in a
   constraint expression.
")
    ]).

%----------------------------------------------------------------------

:- comment(sum/3, [
    summary:"Constrains the sum of the elements of Collection to"
            " satisfy the relation sum(Collection) Rel Sum.",
    template: "<ConsistencyModule:> sum(+Collection,+Rel,?Sum)",
    amode:sum(+,+,?),
    args:[
	"Collection": "Collection of integers or domain variables.",
        "RelOp": "One of the atom: #>, #>=, #<, #=<, #=, #\\=",
	"Sum":"Variable or integer"
    ],
    desc:html("<P>\
          Constrains the sum of the elements in Collection to satisfy
          the relation sum(Collection) Rel Sum.
	  </P><P>
          Rel can be one of #>, #>=, #<, #=<, #=, #\\= (or equivalently,
          >, >=, <, =<, =, \\=).
	  </P><P>
	  Any input variables which are not already domain variable will be
          turn into domain variables with default bounds.</P><P>
	  </P><P>
          You may find it more convenient to embed <TT>sumlist(Vars)</TT> in a
          constraint expression.
	  </P><P>
          ConsistencyModule is the optional module specification to give the 
          consistency level for the propagation for this constraint: 
          gfd_bc for bounds consistency, and 
          gfd_gac for domain (generalised arc) consistency.
          </P><P>
          Domain consistency is different from bounds consistency only if
          Rel is #=.
    ") 
]).

%----------------------------------------------------------------------

:- comment(sum/4, [
    summary:"Reflect into Bool the truth of the sum of the elements of Collection"
            " satisfying the relation sum(Collection) Rel Sum.",
    template: "<ConsistencyModule:> sum(+Collection,+Rel,?Sum,?Bool)",
    amode:sum(+,+,?,?),
    args:[
	"Collection": "Collection of integers or domain variables.",
        "RelOp": "One of the atom: #>, #>=, #<, #=<, #=, #\\=",
	"Sum":"Variable or integer",
        "Bool":"Variable or the integer 0 or 1"
    ],
    desc:html("<P>\
          This is the reified form of sum/3, which constrains the sum
          of  the elements in Collection to satisfy the relation 
          sum(Collection) Rel Sum.
	  </P><P>
          Rel can be one of #>, #>=, #<, #=<, #=, #\\= (or equivalently,
          >, >=, <, =<, =, \\=).
	  </P><P>
	  Any input variables which are not already domain variable will be
          turn into domain variables with default bounds.</P><P>
	  </P><P>
          ConsistencyModule is the optional module specification to give the 
          consistency level for the propagation for this constraint: 
          gfd_bc for bounds consistency 
    ")
]).

%----------------------------------------------------------------------

:- comment(scalar_product/4, [
    summary:"Constrains the scalar product of the elements of Coeffs"
            " and Collection to satisfy the relation sum(Coeffs*Collection) Rel P.",
    template: "<ConsistencyModule:> scalar_product(++Coeffs,+Collection,+Rel,?Sum)",
    amode:scalar_product(++,+,+,?),
    args:[
	"Coeffs": "Collection of N integers.",
	"Collection": "Collection of N integers or domain variables.",
        "RelOp": "One of the atom: #>, #>=, #<, #=<, #=, #\\=",
	"P":"Variable or integer"
    ],
    desc:html("<P>\
          Constrains the scalar product of the elements in Collection to satisfy
          the relation sum(Coeffs*Collection) Rel P.
	  </P><P>
          Rel can be one of #>, #>=, #<, #=<, #=, #\\= (or equivalently,
          >, >=, <, =<, =, \\=).
	  </P><P>
          The Scalar Product of the collection of N integers in Coeffs and
          the collection of N domain variables or integers in Collection 
          is the sum of all Ci*Vi, where Ci is a element in Coeffs and
          Vi the corresponding element in Collection.
          </P><P>
	  Any input variables which are not already domain variable will be
          turn into domain variables with default bounds.
	  </P><P>
          You may find it more convenient to embed <TT>sumlist(Vars)</TT> in a
          constraint expression.
	  </P><P>
          ConsistencyModule is the optional module specification to give the 
          consistency level for the propagation for this constraint: 
          gfd_bc for bounds consistency, and 
          gfd_gac for domain (generalised arc) consistency.
          </P><P>
          Domain consistency is different from bounds consistency only if
          Rel is #=.
    ") 
]).

%----------------------------------------------------------------------

:- comment(scalar_product/5, [
    summary:"Reflect into Bool the truth of the scalar product of the"
            " elements of Coeffs and Collection satisfying the relation "
            " sum(Coeffs*Collection) Rel Sum.",
    template: "<ConsistencyModule:> scalar_product(++Coeffs,+Collection,+Rel,?Sum,?Bool)",
    amode:scalar_product(++,+,+,?,?),
    args:[
	"Coeffs": "Collection of N integers.",
	"Collection": "Collection of N integers or domain variables.",
        "RelOp": "One of the atom: #>, #>=, #<, #=<, #=, #\\=",
	"P":"Variable or integer",
        "Bool":"Variable or the integer 0 or 1"
    ],
    desc:html("<P>\
          This is the reified form of scalar_product/4, which constrains the
          scalar product of  the elements in Coeffs and Collection to satisfy
          the relation 
          sum(Coeffs*Collection) Rel P.
	  </P><P>
          Rel can be one of #>, #>=, #<, #=<, #=, #\\= (or equivalently,
          >, >=, <, =<, =, \\=).
          </P><P>
	  Any input variables which are not already domain variable will be
          turn into domain variables with default bounds.</P><P>
	  </P><P>
          ConsistencyModule is the optional module specification to give the 
          consistency level for the propagation for this constraint: 
          gfd_bc for bounds consistency 
    ")
]).

%---------------------------------------------------------------------
:- comment((and)/2, [
    amode: and(+, +),
    template: "<ConsistencyModule:> +ConX and +ConY",
    args: [
	"ConX": "Constraint",
	"ConY": "Constraint"
    ],
    summary: "Constraints ConX and ConY must both be true.",
    see_also: [(and)/3, (neg)/1, (neg)/2, (or)/2, (or)/3, (=>)/2,
               (=>)/3],
    desc: html("<P>
   Equivalent to BX #= (ConX), BY #= (ConY), BX + BY #= 2</P>
   <P>
   The two constraints are reified in such a way that both must be true.
   ConX and ConY must be a constraints that have a corresponding reified
   form.</P><P>

   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: 
   gfd_bc for bounds consistency, and gfd_gac for domain (generalised arc) 
   consistency. 

")
]).

:- comment((and)/3, [
    amode: and(+, +, ?),
    template: "<ConsistencyModule:> and(+ConX,+ConY,Bool)",
    args: [
	"ConX": "Constraint",
	"ConY": "Constraint",
        "Bool": "Reified truth value of the constraint"
    ],
    summary: "Bool is the reified truth of both constraints ConX and ConY being true.",
    see_also: [(and)/2, (neg)/1, (neg)/2, (or)/2, (or)/3, (=>)/2,
               (=>)/3],
    desc: html("<P>
   Equivalent to BX #= (ConX), BY #= (ConY), Bool #= (BX + BY #= 2)</P>
   <P>
   The two constraints are reified in such a way that Bool reflects the
   truth of both being true.  ConX and ConY must be constraints that have a
   corresponding reified form.<P></P>

   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: 
   gfd_bc for bounds consistency, and gfd_gac for domain (generalised arc) 
   consistency. 
")
]).

%---------------------------------------------------------------------
:- comment((or)/2, [
    amode: or(+, +),
    template: "<ConsistencyModule:> +ConX or +ConY",
    args: [
	"ConX": "Constraint",
	"ConY": "Constraint"
    ],
    summary: "At least one of the constraints ConX or ConY must be true.",
    see_also: [(or)/3, (neg)/1, (neg)/2, (and)/2, (and)/3, (=>)/2,
               (=>)/3],
    desc: html("<P>
   Equivalent to BX #= (ConX), BY #= (ConY), BX + BY #>= 1</P>
   <P>
   The two constraints are reified in such a way that at least one must be
   true.  ConX and ConY must be constraints that have a corresponding
   reified form.<P></P>

   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: 
   gfd_bc for bounds consistency, and gfd_gac for domain (generalised arc) 
   consistency. 
")
]).

:- comment((or)/3, [
    amode: or(+, +, ?),
    template: "<ConsistencyModule:> or(+ConX,+ConY,Bool)",
    args: [
	"ConX": "Constraint",
	"ConY": "Constraint",
        "Bool": "Reified truth value of the constraint"
    ],
    summary: "Bool is the reified truth of at least one of the constraints ConX or ConY being true.",
    see_also: [(or)/2, (neg)/1, (neg)/2, (and)/2, (and)/3, (=>)/2,
               (=>)/3, (=:=)/3, (=<)/3, (=\=)/3, (>=)/3, (>)/3, (<)/3],
    desc: html("<P>
   Equivalent to BX #= (ConX), BY #= (ConY), Bool #= (BX + BY #>= 1)</P>
   <P>
   The two constraints are reified in such a way that Bool reflects the
   truth of at least one being true.  ConX and ConY must be constraints that
   have a corresponding reified form.<P></P>

   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: 
   gfd_bc for bounds consistency, and gfd_gac for domain (generalised arc) 
   consistency. 
")
]).

%---------------------------------------------------------------------
:- comment((xor)/2, [
    amode: xor(+, +),
    template: "<ConsistencyModule:> +ConX xor +ConY",
    args: [
	"ConX": "Constraint",
	"ConY": "Constraint"
    ],
    summary: "One of the constraints ConX or ConY must be true.",
    see_also: [(or)/3, (neg)/1, (neg)/2, (and)/2, (and)/3, (=>)/2,
               (=>)/3],
    desc: html("<P>
   Equivalent to BX #= (ConX), BY #= (ConY), BX + BY #= 1</P>
   <P>
   The two constraints are reified in such a way that one and only one must be
   true.  ConX and ConY must be constraints that have a corresponding
   reified form.<P></P>

   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: 
   gfd_bc for bounds consistency, and gfd_gac for domain (generalised arc) 
   consistency. 
")
]).

:- comment((xor)/3, [
    amode: xor(+, +, ?),
    template: "<ConsistencyModule:> xor(+ConX,+ConY,Bool)",
    args: [
	"ConX": "Constraint",
	"ConY": "Constraint",
        "Bool": "Reified truth value of the constraint"
    ],
    summary: "Bool is the reified truth of one of the constraints ConX or ConY being true.",
    see_also: [(or)/2, (neg)/1, (neg)/2, (and)/2, (and)/3, (=>)/2,
               (=>)/3, (=:=)/3, (=<)/3, (=\=)/3, (>=)/3, (>)/3, (<)/3],
    desc: html("<P>
   Equivalent to BX #= (ConX), BY #= (ConY), Bool #= (BX + BY #= 1)</P>
   <P>
   The two constraints are reified in such a way that Bool reflects the
   truth of one (and only one) being true.  ConX and ConY must be 
   constraints that have a corresponding reified form.<P></P>

   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: 
   gfd_bc for bounds consistency, and gfd_gac for domain (generalised arc) 
   consistency. 
")
]).

%---------------------------------------------------------------------
:- comment((=>)/2, [
    amode: =>(+, +),
    template: "<ConsistencyModule:> +ConX => +ConY",
    args: [
	"ConX": "Constraint",
	"ConY": "Constraint"
    ],
    summary: "Constraint ConX being true implies ConY must both be true.",
    see_also: [(=>)/3, (neg)/1, (neg)/2, (or)/2, (or)/3, (and)/2,
               (and)/3],
    desc: html("<P>
   Equivalent to BX #= (ConX), BY #= (ConY), BX #=< BY</P>
   <P>
   The two constraints are reified in such a way that ConX being true
   implies that ConY must also be true.  ConX and ConY must be constraints
   that have a corresponding reified form.<P></P>

   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: 
   gfd_bc for bounds consistency, and gfd_gac for domain (generalised arc) 
   consistency. 
")
]).

:- comment((=>)/3, [
    amode: =>(+, +, ?),
    template: "<ConsistencyModule:> =>(+ConX,+ConY,Bool)",
    args: [
	"ConX": "Constraint",
	"ConY": "Constraint",
        "Bool": "Reified truth value of the constraint"
    ],
    summary: "Bool is the reified truth of constraint ConX implying the truth of ConY.",
    see_also: [(=>)/2, (neg)/1, (neg)/2, (or)/2, (or)/3, (and)/2,
               (and)/3, (=:=)/3, (=<)/3, (=\=)/3, (>=)/3, (>)/3, (<)/3],
    desc: html("<P>
   Equivalent to BX #= (ConX), BY #= (ConY), Bool #= (BX #=< BY)</P>
   <P>
   The two constraints are reified in such a way that Bool is true if ConX
   being true implies that ConY must also be true.  ConX and ConY must be
   constraints that have a corresponding reified form.<P></P>

   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: 
   gfd_bc for bounds consistency, and gfd_gac for domain (generalised arc) 
   consistency. 
")
]).

%---------------------------------------------------------------------
:- comment((<=>)/2, [
    amode: <=>(+, +),
    template: "<ConsistencyModule:> +ConX <=> +ConY",
    args: [
	"ConX": "Constraint",
	"ConY": "Constraint"
    ],
    summary: "Constraint ConX has the equivalent truth value as ConY.",
    see_also: [(=>)/3, (neg)/1, (neg)/2, (or)/2, (or)/3, (and)/2,
               (and)/3],
    desc: html("<P>
   Equivalent to BX #= (ConX), BY #= (ConY), BX #= BY</P>
   <P>
   The two constraints are reified in such a way that ConX and ConY constrained
   to the same truth value. ConX and ConY must be constraints that have a 
   corresponding reified form.</P><P>

   This connective is not available in IC because #=/2 can be used instead.
   It is provided in GFD as it maps directly to gecode's equivalence 
   connective.</P><P> 

   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: 
   gfd_bc for bounds consistency, and gfd_gac for domain (generalised arc) 
   consistency. 
")
]).

:- comment((<=>)/3, [
    amode: <=>(+, +, ?),
    template: "<ConsistencyModule:> <=>(+ConX,+ConY,Bool)",
    args: [
	"ConX": "Constraint",
	"ConY": "Constraint",
        "Bool": "Reified truth value of the constraint"
    ],
    summary: "Bool is the reified truth of constraint ConX is equivalent to the truth of ConY.",
    see_also: [(=>)/2, (neg)/1, (neg)/2, (or)/2, (or)/3, (and)/2,
               (and)/3],
    desc: html("<P>
   Equivalent to BX #= (ConX), BY #= (ConY), Bool #= (BX #= BY)</P>
   <P>
   The two constraints are reified in such a way that Bool is true if ConX
   and ConY have the same truth value.  ConX and ConY must be
   constraints that have a corresponding reified form.<P></P>

   This reified connective is not available in IC because #=/3 can be 
   used instead. It is provided in GFD as <=> maps to gecode's 
   equivalence connective.</P><P> 

   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: 
   gfd_bc for bounds consistency, and gfd_gac for domain (generalised arc) 
   consistency. 
")
]).

%---------------------------------------------------------------------

:- comment((neg)/1, [
    amode: neg(+),
    template: "<ConsistencyModule:> neg(+Con)",
    args: [
	"Con": "Constraint"
    ],
    summary: "Constraints Con is negated.",
    see_also: [(and)/2, (and)/3, (neg)/2, (or)/2, (or)/3, (=>)/2,
               (=>)/3, (=:=)/3, (=<)/3, (=\=)/3, (>=)/3, (>)/3, (<)/3],
    desc: html("<P>
   Equivalent to 0 #= (Con)</P>
   <P>
   The reified constraint Con is negated.  Con must be a constraint that has
   a corresponding reified form.<P></P>

   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: 
   gfd_bc for bounds consistency, and gfd_gac for domain (generalised arc) 
   consistency. 
")
]).

:- comment((neg)/2, [
    amode: neg(+, ?),
    template: "<ConsistencyModule:> neg(+Con,Bool)",
    args: [
	"Con": "Constraint",
        "Bool": "Reified truth value of the constraint"
    ],
    summary: "Bool is the logical negation of the reified truth constraints Con.",
    see_also: [(and)/2, (and)/3, (neg)/1, (or)/2, (or)/3, (=>)/2,
               (=>)/3, (=:=)/3, (=<)/3, (=\=)/3, (>=)/3, (>)/3, (<)/3],
    desc: html("<P>
   Equivalent to B #= (Con), Bool #= 1-B</P>
   <P>
   Bool is the logical negation of the reified truth constraint Con.  Con
   must be a constraint that has a corresponding reified form.<P></P>

   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: 
   gfd_bc for bounds consistency, and gfd_gac for domain (generalised arc) 
   consistency. 
")
]).

%---------------------------------------------------------------------

:- comment(element/3, [
	summary:"Value is the Index'th element of the integer list List.",
	template:"<ConsistencyModule:> element(?Index, +Collection, ?Value)",
	args:[
	    "?Index" : "A domain variable or an integer.",
	    "+Collection" : "A non-empty collection of integers or domain variable.",
	    "?Value" : "A domain variable or an integer."
	],
	resat:"No.",
	fail_if:"Fails if Value is not the Index'th element of Collection.",
	desc:html("This constraint can be used in a variety of programs to state a
   relation between two domain variables.  Collection is a collection of 
   integers and the constraint states that its Index'th element is equal to 
   Value, i.e.
<P>
<PRE>
			     Collection_Index = Value
</PRE>
   the domain of the other variable is updated accordingly. Index starts from 1.
<P>
   Note that unlike the element constraint in IC, the values in Collection 
   can be domain variables as well as integers. Also note that the actual
   Gecode constraint has an index that starts from 0 - a dummy element
   is added to start of Collection to map Index to ECLiPSe style index
   starting from 1. A version of this constraint that uses the native 
   Gecode indexing is element_g/3. 
<P>
   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: 
   gfd_bc for bounds consistency, and gfd_gac for domain (generalised arc) 
   consistency. 
"),
	eg:"
[eclipse 2]: element(I, [1,3,6,3], V).

I = I{[1 .. 4]}
V = V{[1, 3, 6]}


[eclipse 2]: element(I, [1,3,6,3], V),  V #\\= 3.

I = I{[1, 3]}
V = V{[1, 6]}

[eclipse 2]:  X :: [1..10], gfd_gac: element(I, [1,X,6,3],  V), V #\\= 3.


Z = Z{[1 .. 10]}
I = I{[1 .. 3]}
V = V{[1, 2, 4 .. 10]}


",
	see_also:[/(::, 2), element_g/3]
    ]).


:- comment(element_g/3, [
	summary:"Value is the Index'th element of the integer list List, with native Gecode indexing.",
	template:"<ConsistencyModule:> element_g(?Index, ++List, ?Value)",
	args:[
	    "?Index" : "A domain variable or an integer.",
	    "+Collection" : "A non-empty collection of integers or domain variable.",
	    "?Value" : "A domain variable or an integer."
	],
	resat:"No.",
	fail_if:"Fails if Value is not the Index'th element of Collection.",
	see_also: [element/3],
	desc:html("\
  This version of element/3 uses the native Gecode indexing, which starts 
  from 0, i.e. the first element of Collection has index 0. This is different 
  from normal ECLiPSe's indexing, which starts from 1.
</p><p>
  This predicate maps directly to Gecode's native implementation of the
  constraint, and may therefore be more efficient, but could also be
  incompatible with existing ECLiPSe code. 
</p><p>
  See element/3 for a more detailed description of this predicate.")
]).   

%----------------------------------------------------------------------

:- comment(occurrences/3, [
    summary:"The value Value occurs in Vars N times",
    template:"<ConsistencyModule:> occurrences(++Value,+Vars.?N)",
    amode:occurrences(++,+,?),
    args:[
	"Value":"Integer",
	"Vars":"Collection (a la collection_to_list/2) of integers or domain variables",
	"N":"Domain variable or integer"
    ],
    desc:html("\
    	  The value Value occurs in Vars N times.
<P>
          ConsistencyModule is the optional module specification to give the 
          consistency level for the propagation for this constraint: 
          gfd_gac for domain (generalised arc) consistency. 
"),
    see_also:[element/3, atmost/3, collection_to_list/2]
    ]).

%----------------------------------------------------------------------

:- comment(atmost/3, [
	summary:"At most N elements of Vars have the value V.",
	template:"<ConsistencyModule:> atmost(+N, ?Vars, +V)",
	desc:html("\
   This constraint ensures that at most N element of Vars have the value V.
<P>
   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: 
   gfd_gac for domain (generalised arc) consistency. 
"),
	args:["+N" : "An integer",
	      "?Vars" : "A collection (a la collection_to_list/2) of domain variables or integers",
	      "+V" : "An integer"],
	resat:"   No.",
	fail_if:"   Fails if more than N elements of Vars can be instantiated to V.",
	see_also:[element/3, occurrences/3, collection_to_list/2]]).

%----------------------------------------------------------------------

:- comment(atleast/3, [
	summary:"Atleast N elements of Vars have the value V.",
	template:"<ConsistencyModule:> atleast(+N, ?Vars, +V)",
	desc:html("\
   This constraint ensures that atleast N element of Vars have the value V.
<P>
   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: 
   gfd_gac for domain (generalised arc) consistency. 
"),
	args:["+N" : "An integer",
	      "?Vars" : "A collection (a la collection_to_list/2) of domain variables or integers",
	      "+V" : "An integer"],
	resat:"   No.",
	fail_if:"   Fails if less than N elements of Vars can be instantiated to V.",
	see_also:[element/3, occurrences/3, collection_to_list/2]]).


%----------------------------------------------------------------------

:- comment(count/4, [
	summary: "Constrain the number of occurrence of Value in Vars (Occ) to satisfy  the relation N Rel Occ",
	template:"<ConsistencyModule:> count(+Value, ?Vars, +Rel, ?N)",
	args:["+Value" : "An integer",
	      "?Vars" : "A collection (a la collection_to_list/2) of domain variables or integers",
              "+Rel":"One of the atom: #>, #>=, #<, #=<, #=, #\\=",
	      "?N" : "An integer or domain variable"],
	desc:html("<P>\
   Constrain the number of occurrences of Value in Vars to satisfy the
   constraint defined by Rel:
<PRE>
          N Rel <number of occurrences of Value in Vars>
</PRE><P>
     Rel can be one of #>, #>=, #<, #=<, #=, #\\= (or equivalently,
     >, >=, <, =<, =, \\=)
</P><P>
   occurrences/3, atmost/3, atleast/3 are defined using count/3. For example,
<PRE>
         atmost(N, Vars, Value)
</PRE>
   is defined by:
<PRE>
        count(Value, Vars, (#=<), N)
</PRE><P>
   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: 
   gfd_gac for domain (generalised arc) consistency. 
")
]).

%----------------------------------------------------------------------
:- comment(sorted/2, [
    summary:"Sorted is a sorted permutation of List",
    amode:sorted(+,+),
    amode:sorted(+,-),
    amode:sorted(-,+),
    template:"<ConsistencyModule:> sorted(?List, ?Sorted)",
    args:["List":"List or collection of N domain variables or integers",
    	"Sorted":"List or collection of N domain variables or integers"],
    desc:html("\
    Declaratively: The two lists have the same length and Sorted is a
    sorted permutation of List.
<P>
    Operationally:  the elements in both collections are constrained such
    that their domains are consistent with the assumption that the
    collection Sorted is the sorted version of the collection List.
<P>
    One of the two arguments can be uninstantiated or partial lists
    at call time.
<P>
    Any input variables which is not already a domain variable will be
    turned into a domain variable with default bounds.
<P>
    ConsistencyModule is the optional module specification to give the 
    consistency level for the propagation for this constraint: 
    gfd_bc for bounds consistency.
 "),
    eg:"
[eclipse 2]: length(Xs,4), Xs::0..100, sorted(Xs,Ys), Xs = [8,20|_].

Xs = [8, 20, _694{[0 .. 100]}, _714{[0 .. 100]}]
Ys = [_774{[0 .. 8]}, _794{[0 .. 20]}, _814{[8 .. 100]}, _834{[20 .. 100]}]

[eclipse 2]: length(Ys,4), Ys::0..100, sorted(Xs,Ys), Ys = [8,20|_].

Ys = [8, 20, _694{[20 .. 100]}, _714{[20 .. 100]}]
Xs = [_832{[8 .. 100]}, _852{[8 .. 100]}, _872{[8 .. 100]}, _892{[8 .. 100]}]

    ",
    see_also:[sorted/3,ordered/2]
    ]).

:- comment(sorted/3, [
    summary:"Sorted is a sorted permutation (described by Positions) of List",
    amode:sorted(+,?,?),
    amode:sorted(?,+,?),
    amode:sorted(?,?,+),
    template:"<ConsistencyModule:> sorted(?List, ?Sorted, ?Positions)",
    args:["List":"Collection of N domain variables or integers",
    	"Sorted":"Collection of N domain variables or integers",
    	"Positions":"Collection of N domain variables or integers"],
    desc:html("\
    Declaratively:  Sorted is a sorted permutation of List.  Positions
    is a collection whose elements range from 1 to N (where N is the 
    cardinality of the collections) indicating the position of each 
    unsorted list element within the sorted list.  The positions are all 
    different. The three collections are constrained to have the same size.
<P>
    Operationally:  the elements in all three lists are constrained
    such that their domains are consistent with the declarative
    meaning.
<P>
    Two of the three arguments can be uninstantiated or partial lists
    at call time.
<P>
    Any input variables which is not already a domain variable will be
    turned into a domain variable with default bounds.
<P>
   Note that the gecode implementation of the constraint use 
   positions starting from 0. An extra dummy element smaller than all
   the elements in List is added so that the position returned correspond
   to the usual ECLiPSe index starting from 1. In addition, the complexity
   of the algorithm used by gecode is linear in time with respect to 
   Max - Min, where Max and Min are the Maximum and Minimum possible values
   for elements in List, respectively. Therefore, this constraint will 
   behave badly for variables with large domain widths. For a version of this
   constraint that uses native Gecode indexing, see sorted_g/3.
<P>
    ConsistencyModule is the optional module specification to give the 
    consistency level for the propagation for this constraint: 
    gfd_bc for bounds consistency.
    "),
    eg:"
[eclipse 2]: length(Xs,4),Xs :: 1 .. 100,sorted(Xs, Ys, Ps),Xs = [8, 20|_].

Xs = [8, 20, _715{[1 .. 100]}, _735{[1 .. 100]}]
Ys = [_804{[1 .. 8]}, _824{[1 .. 20]}, _844{[8 .. 100]}, _864{[20 .. 100]}]
Ps = [_969{[1 .. 3]}, _989{[2 .. 4]}, _1009{[1 .. 4]}, _1029{[1 .. 4]}]

    ",
    see_also:[sorted/2,ordered/2,sorted_g/3]
    ]).

:- comment(sorted_g/3, [
    summary:"Sorted is a sorted permutation (described by Positions) of List, with native Gecode indexing.",
    amode:sorted(+,?,?),
    amode:sorted(?,+,?),
    amode:sorted(?,?,+),
    template:"<ConsistencyModule:> sorted_g(?List, ?Sorted, ?Positions)",
    args:["List":"Collection of N domain variables or integers",
    	"Sorted":"Collection of N domain variables or integers",
    	"Positions":"Collection of N domain variables or integers"],
    see_also: [sorted/3],	
    desc:html("\
  This version of sorted/3 uses the native Gecode indexing, which starts 
  from 0, i.e. the first element of the collections has index 0. This is 
  different from normal ECLiPSe's indexing, which starts from 1.
</p><p>
  This predicate maps more directly to Gecode's native implementation of 
  the constraint, without the conversion between Gecode and ECLiPSe
  indexing of sorted/3. It may therefore be more efficient, but could also
  be incompatible with existing ECLiPSe code. 
</p><p>
  See sorted/3 for a more detailed description of this predicate.")
]).   

%----------------------------------------------------------------------

:- comment(bool_channeling/3, [
        amode: bool_channeling(?, +, +),
       template:"<ConsistencyModule:> bool_channeling(?Var, +DomainBools, +Min)",
        args: ["Var": "A domain variable",
               "DomainBools": "A collection of N 0/1 domain variables or"
                           " integers",
               "Min": "An integer"],
        summary: "Channel the domain values of Vars to the 0/1 boolean"
                 " variables in DomainBools",
        desc: html("\
<P>
    Var is a domain variable whose initial interval is Min..(Min+N),
    and this constraint links the domain values of Var with the N 0/1
    variables in DomainBools such that the i'th variable in DomainBools
    represents the value Min+i, and its value is 0 if the value is not in
    Var's domain, and 1 if Var is assigned the value [Thus, only one variable
    in DomainBools can take the value 1].
</P><P>
    A variant of this constraint, called 'domain_constraint' is in the global 
    constraint catalog. There, instead of having DomainBools and Min, there
    is a collection of Value-Bool pairs, representing a possible domain value
    and its associated 0/1 variable. 
</P><P>
    ConsistencyModule is the optional module specification to give the 
    consistency level for the propagation for this constraint: 
    gfd_gac for generalised arc consistency (domain consistency), and
    gfd_vc for value consistency.
")]).


%----------------------------------------------------------------------

:- comment(gcc/2, [
        amode: gcc(+,+),
        template:"<ConsistencyModule:> gcc(+Bounds,+Vars)",
        args: ["Bounds":"A list of elements specifying the cardinality of"
                        " values occurring in Vars of the form "
                        "gcc(Low,High,Value) or occ(Occ,Value).",
               "Vars":"A collection of different variables or integers"
              ],
        summary:"Constrain the cardinality of each Value according to the specification in Bounds.",
        desc:html("\
<P>
    This constraint ensures that the cardinality (the number of occurrences)
    of values in Vars conforms to the specifications in Bounds. Bounds is a
    list of specifications in one of the following forms:
<DL><P>
 <DT><STRONG><TT>gcc(Low,High,Value)</TT></STRONG>
    <DD>where Value is an integer, a value that Vars is to be assigned to, 
    and must occur only once as a Value in Bounds, and whose cardinality 
    |Value| is specified by Low =< |Value| =< High, where Low and High are 
    non-negative integers.
 <DT><STRONG><TT>occ(Occ,Value)</TT></STRONG>
    <DD>where Value is an integer, a value that Vars is to be assigned to, 
    and must occur only once as a Value in Bounds, and whose cardinality 
    |Value| is specified by Occ. Occ is either a non-negative integer, or 
    a domain variable whose domain are the possible cardinality for Value.
</DL></P><P>
    Note that all values that Vars can take must be specified in Bounds.
</P><P>
    ConsistencyModule is the optional module specification to give the 
    consistency level for the propagation for this constraint: 
    gfd_gac for generalised arc consistency (domain consistency), 
    gfd_bc for bounds consistency, and gfd_vc for value consistency.

")
                  ]).


%----------------------------------------------------------------------

:- comment(inverse/2, [
        amode: inverse(+,+),
        args: ["Succ":"A collection of N different variables or integers",
               "Pred":"A collection  of N different variables or integers"
              ],
        template:"<ConsistencyModule:> inverse(+Succ,+Pred)",
        summary: "Constrains elements of Succ to be the successors and"
                 " Pred to be the predecessors of nodes in a digraph",
	see_also:[inverse_g/2],
        desc: html("\
<P>
     Succ and Pred are collections of N elements, representing a digraph of 
     N nodes, where the i'th element of Succ and Pred represents the 
     successor and predecessor of the node i respectively. The constraint 
     enforces each node in the digraph to have one successor and one 
     predecessor node, and that if node y is the successor of node x, then 
     node x is the predecessor of node y.
</P><P>
    One of the two arguments can be uninstantiated or partial list
    at call time.
</P><P>
     This constraint is known as inverse in the global constraint catalog,
     but with implicit node index based on the position in the list.  
</P><P>
     Note that the Gecode implementation of this constraint actually index
     from 0. A dummy element is added to the start of Succ and Pred so that
     the indices returned corresponds to ECLiPSe's (starting from 1). A
     version of this constraint using native Gecode indexing is available
     as inverse_g/2.
</P><P>
    ConsistencyModule is the optional module specification to give the 
    consistency level for the propagation for this constraint: 
    gfd_gac for generalised arc consistency (domain consistency), 
    and gfd_vc for value consistency.

")]).

:- comment(inverse_g/2, [
        amode: inverse_g(+,+),
        args: ["Succ":"A collection of N different variables or integers",
               "Pred":"A collection  of N different variables or integers"
              ],
        template:"<ConsistencyModule:> inverse_g(+Succ,+Pred)",
        summary: "Constrains elements of Succ to be the successors and"
                 " Pred to be the predecessors of nodes in a digraph, using"
		 " native Gecode indexing.",
	see_also:[inverse/2],
        desc: html("\
  This version of inverse/2 uses the native Gecode indexing, which starts 
  from 0, i.e. the first elements in Succ and Pred has position 0. This is 
  different from normal ECLiPSe's indexing, which starts from 1.
</p><p>
  This predicate maps more directly to Gecode's native implementation of 
  the constraint, without the conversion between Gecode and ECLiPSe
  indexing of inverse/2. It may therefore be more efficient, but could also
  be incompatible with existing ECLiPSe code. 
</p><p>
  See inverse/2 for a more detailed description of this predicate.")
]).   

:- comment(inverse/4, [
        amode: inverse(+,+,+,+),
        args: ["Succ":"A collection of N different variables or integers",
               "SuccOffset":"An integer.",
               "Pred":"A collection  of N different variables or integers",
               "PredOffset":"An integer."
              ],
        template:"<ConsistencyModule:> inverse(+Succ,+SuccOffset,+Pred,+PredOffset)",
        summary: "Constrains elements of Succ to be the successors and"
                 " Pred to be the predecessors of nodes in a digraph",
	see_also:[inverse_g/4],
        desc: html("\
<P>
     Succ and Pred are list of N elements, representing a digraph of N nodes,
     where the i'th element of Succ and Pred represents the successor and
     predecessor of the node i respectively. The constraint enforces each
     node in the digraph to have one successor and one predecessor node, and
     that if the successor of node y minus SuccOffset is equal to x, then
     the predecessor of node x minus PredOffset is equal to y.
</P><P>
    One of the two collection arguments (Succ and Pred) can be uninstantiated 
    or partial list at call time.
</P><P>
     This constraint is known as inverse_offset in the global constraint 
     catalog, but with implicit node index based on the position in the list.  
</P><P>
     Note that the gecode implementation of this constraint actually index
     from 0. The SuccOfffset and PredOffset are adjusted accordingly before
     posting to gecode so that the indices returned corresponds to 
     ECLiPSe's (starting from 1).
</P><P>
    ConsistencyModule is the optional module specification to give the 
    consistency level for the propagation for this constraint: 
    gfd_gac for generalised arc consistency (domain consistency), 
    and gfd_vc for value consistency.

")]).

:- comment(inverse_g/4, [
        amode: inverse_g(+,+,+,+),
        args: ["Succ":"A collection of N different variables or integers",
               "SuccOffset":"An integer.",
               "Pred":"A collection  of N different variables or integers",
               "PredOffset":"An integer."
              ],
        template:"<ConsistencyModule:> inverse_g(+Succ,+SuccOffset,+Pred,+PredOffset)",
        summary: "Constrains elements of Succ to be the successors and"
                 " Pred to be the predecessors of nodes in a digraph",
	see_also:[inverse/4],
        desc: html("\
  This version of inverse/4 uses the native Gecode indexing, which starts 
  from 0, i.e. the first elements in Succ and Pred has position 0. This is 
  different from normal ECLiPSe's indexing, which starts from 1.
</p><p>
  This predicate maps directly to Gecode's native implementation of 
  the constraint, without the offset adjustments of inverse/4.
</p><p>
  See inverse/4 for a more detailed description of this predicate.")
]).   

%----------------------------------------------------------------------

:- comment(circuit/1, [
        amode: circuit(+),
        args: ["Succ":"A collection of different variables or integers"
              ],
        template:"<ConsistencyModule:> circuit(+Succ)",
        summary: "Constrains elements in Succ to form a Hamiltonian circuit.", 
	see_also: [circuit_g/1],
        desc: html("<P>\
  Succ is a collection of N elements presenting a digraph of N nodes, where
  the i'th element of Succ represents the successor to node i. The constraint
  enforces Succ to form a Hamiltonian circuit, a path through every node in
  the graph, visiting each node once and forming a circuit.</P><P>

  Note that the Gecode implementation of this constraint has index (node id)
  starting from 0, rather than 1. These indices are constrained to 
  ECLiPSe node id with a constraint for each element. A version of this
  constraint with native Gecode indexing is available as circuit_g/1.
</P><P>

  ConsistencyModule is the optional module specification to give the 
  consistency level for the propagation for this constraint: 
  gfd_gac for generalised arc consistency (domain consistency), 
  and gfd_vc for value consistency.
")
                      ]).

:- comment(circuit_g/1, [
        amode: circuit_g(+),
        args: ["Succ":"A collection of different variables or integers"
              ],
        template:"<ConsistencyModule:> circuit_g(+Succ)",
        summary: "Constrains elements in Succ to form a Hamiltonian circuit, with native Gecode indexing.", 
	see_also: [circuit/1],
        desc: html("<P>\
  This version of circuit/1 uses the native Gecode indexing, which starts 
  from 0. This is different from normal ECLiPSe's indexing, which starts from 1.
</p><p>
  This predicate maps more directly to Gecode's native implementation of 
  the constraint, without the conversion between Gecode and ECLiPSe
  indexing of circuit/1. It may therefore be more efficient, but could also
  be incompatible with existing ECLiPSe code. 
</p><p>
  See circuit/1 for a more detailed description of this predicate.")
]).   

%----------------------------------------------------------------------

:- comment(circuit/3, [
        amode: circuit(+,++,?),
        args: ["Succ":"A collection of N different variables or integers",
               "CostMatrix":"A NxN matrix of integers.",
               "Cost": "An domain variable or integer."
              ],
        template:"<ConsistencyModule:> circuit(+Succ,++CostMatrix,?Cost)",
        summary: "Constrains elements in Succ to form a Hamiltonian circuit with cost Cost.", 
        desc: html("<P>\
  Succ is a collection of N elements presenting a digraph of N nodes, where
  the i'th element of Succ represents the successor to node i. The constraint
  enforces Succ to form a Hamiltonian circuit, a path through every node in
  the graph, visiting each node once and forming a circuit. Additionally,
  CostMatrix specifies the cost for traversing between each pair of nodes:
  CostMatrix[i,j] represents the cost of travelling from node i to j, and 
  Cost is constrained to the total cost for the circuit.
</P><P>

  Note that the Gecode implementation of this constraint has index (node id)
  starting from 0, rather than 1. These indices are constrained to 
  ECLiPSe node id with a constraint for each element. A version of this
  constraint with native Gecode indexing is available as circuit_g/3,
</P><P>
  This constraint is known as circuit in the global constraint catalog.
</P><P>
  ConsistencyModule is the optional module specification to give the 
  consistency level for the propagation for this constraint: 
  gfd_gac for generalised arc consistency (domain consistency), 
  and gfd_vc for value consistency.
")
                      ]).

:- comment(circuit_g/3, [
        amode: circuit_g(+,++,?),
        args: ["Succ":"A collection of N different variables or integers",
               "CostMatrix":"A NxN matrix of integers.",
               "Cost": "An domain variable or integer."
              ],
        template:"<ConsistencyModule:> circuit_g(+Succ,++CostMatrix,?Cost)",
        summary: "Constrains elements in Succ to form a Hamiltonian circuit with cost Cost. This version uses native Gecode indexing.", 
	see_also: [circuit/3],
        desc: html("<P>\
  This version of circuit/3 uses the native Gecode indexing, which starts 
  from 0. This is different from normal ECLiPSe's indexing, which starts from 1.
</p><p>
  This predicate maps more directly to Gecode's native implementation of 
  the constraint, without the conversion between Gecode and ECLiPSe
  indexing of circuit/3. It may therefore be more efficient, but could also
  be incompatible with existing ECLiPSe code. 
</p><p>
  See circuit/3 for a more detailed description of this predicate.")
]).   

%----------------------------------------------------------------------

:- comment(circuit/4, [
        amode: circuit(+,++,+,?),
        args: ["Succ":"A collection of N different variables or integers",
               "CostMatrix":"A NxN matrix of integers",
               "ArcCosts": "A collection of N variables or integers.",
               "Cost": "An domain variable or integer."
              ],
        template:"<ConsistencyModule:> circuit(+Succ,++CostMatrix,?Cost)",
	see_also:[circuit/1,circuit/3,circuit_g/4],
        summary: "Constrains elements in Succ to form a Hamiltonian circuit with cost Cost.", 
        desc: html("<P>\
  Succ is a collection of N elements presenting a digraph of N nodes, where
  the i'th element of Succ represents the successor to node i. The constraint
  enforces Succ to form a Hamiltonian circuit, a path through every node in
  the graph, visiting each node once and forming a circuit. Additionally,
  CostMatrix specifies the cost for traversing between each pair of nodes:
  CostMatrix[i,j] represents the cost of travelling from node i to j, and 
  Cost is constrained to the total Cost for the circuit. The i'th element of 
  ArcCosts is constrained to the cost of the arc in the circuit from node i.
</P><P>

  Note that the gecode implementation of this constraint has index (node id)
  starting from 0, rather than 1. These indices are constrained to 
  ECLiPSe node id with a constraint for each element.A version of this
  constraint with native Gecode indexing is available as circuit_g/4.
</P><P>

  ConsistencyModule is the optional module specification to give the 
  consistency level for the propagation for this constraint: 
  gfd_gac for generalised arc consistency (domain consistency), 
  and gfd_vc for value consistency.
")
                      ]).

:- comment(circuit_g/4, [
        amode: circuit_g(+,++,+,?),
        args: ["Succ":"A collection of N different variables or integers",
               "CostMatrix":"A NxN matrix of integers",
               "ArcCosts": "A collection of N variables or integers.",
               "Cost": "An domain variable or integer."
              ],
        template:"<ConsistencyModule:> circuit_g(+Succ,++CostMatrix,?Cost)",
        summary: "Constrains elements in Succ to form a Hamiltonian circuit with cost Cost, using native Gecode indexing.", 
	see_also:[circuit/4],
        desc: html("<P>\
  This version of circuit/4 uses the native Gecode indexing, which starts 
  from 0. This is different from normal ECLiPSe's indexing, which starts from 1.
</p><p>
  This predicate maps more directly to Gecode's native implementation of 
  the constraint, without the conversion between Gecode and ECLiPSe
  indexing of circuit/4. It may therefore be more efficient, but could also
  be incompatible with existing ECLiPSe code. 
</p><p>
  See circuit/4 for a more detailed description of this predicate.")
]).   

%----------------------------------------------------------------------

:- comment(disjunctive/2, [
  amode:   disjunctive(+,+),
  args:    ["StartTimes":  "Collection of N start times for tasks (domain variables or integers)",
            "Durations":   "Collection of N durations for tasks (non-negative domain variables or integers)"
           ],
  summary: "Constrain the tasks with specified start times and durations to not overlap in time.",
  see_also: [collection_to_list/2],
  desc:    html("\
<P>
    A disjunctive scheduling constraint. StartTimes and Durations are
    collections (a la collection_to_list/2) of equal size N of integer
    variables or integers. Durations must be non-negative. 
    The declarative meaning is that the N tasks with the given start 
    times and durations do not overlap at any point in time.
</P><P>
    Note that the constraint is implemented by different Gecode propagators,
    depending on if Durations contains domain variables or not. If
    Durations does have domain variables, the Gecode propagator requires
    an extra End domain variable specifying the end time, and a constraint 
<PRE>        
      End #= Start + Duration  
</PRE>
    for each task. These are posted as part of the constraint (the End 
    variables are not accessible by the user).
</P><P>
    Any input variables which are not already domain variables will be
    converted into domain variables with default bounds.
</P>")
]).

:- comment(disjunctive_optional/3, [
  amode:   disjunctive_optional(+,+,+),
  args:    ["StartTimes":  "Collection of N start times for tasks (domain variables or integers)",
            "Durations":   "Collection of N durations for tasks (non-negative domain variables or integers)"
            "Scheduled":   "Collection of N scheduled booleans for task (0/1"
" domain variables or integers)"
           ],
  summary: "Constrain the optional tasks with specified start times and durations to"
           " not overlap in time.",
  see_also: [collection_to_list/2],
  desc:    html("\
<P>  
    A disjunctive scheduling constraint. StartTimes, Durations and Scheduled
    are collections (a la collection_to_list/2) of equal size N. Durations
    must be non-negative, and Scheduled are booleans (0/1).
    The declarative meaning is that the scheduled tasks with the given start 
    times and durations do not overlap at any point in time. A task would not
    be scheduled if its Scheduled boolean is 0, and must be scheduled if 1.
</P><P>
    Note that the constraint is implemented by different Gecode propagators,
    depending on if Durations contains domain variables or not. If
    Durations does have domain variables, the Gecode propagator requires
    an extra End domain variable specifying the end time, and a constraint 
<PRE>        
      End #= Start + Duration  
</PRE>
    for each task. These are posted as part of the constraint (the End 
    variables are not accessible by the user).
</P><P>
    Any input variables which are not already domain variables will be
    converted into domain variables with default bounds.
</P>")
]).


%----------------------------------------------------------------------

:- comment(cumulative/4, [
  amode: cumulative(+,+,+,+),
  args:  ["StartTimes":  "Collection of start times for tasks (integer variables or integers)",
          "Durations":   "Collection of duration for tasks (non-negative integer variables or integers)",
          "Usages":   "Collection of resource usages (positive integers)",
          "ResourceLimit": "Maximum amount of resource available (positive integer)"
         ],
  summary: "Single resource cumulative constraint on scheduling tasks.",
  see_also: [disjunctive/2, cumulative_optional/5, collection_to_list/2, _:cumulative/4],
  desc:    html("\
<P>
   A cumulative scheduling constraint. StartTimes, Durations and Usages 
   are collections (a la collection_to_list/2) of equal size N representing
   N tasks. Durations are non-negative, Usages and ResourceLimit are 
   strictly positive. The declarative meaning is:
</P><P>
   The N tasks, each starting at a certain start time, having
   a certain duration and consuming a certain (constant) amount of
   resource, then the sum of resource usage of all the tasks does not
   exceed ResourceLimit at any time. 
</P><P>
   Note that the constraint is implemented by different Gecode propagators,
   depending on if Durations contains domain variables or not. If
   Durations does have domain variables, the Gecode propagator requires
   an extra End domain variable specifying the end time, and a constraint 
<PRE>        
     End #= Start + Duration  
</PRE>
   for each task. These are posted as part of the constraint (the End 
   variables are not accessible by the user).
</P><P>
   Any input variables which are not already domain variables are turned
   into domain variables with default domain.
</P>")
]).

:- comment(cumulative_optional/5, [
  amode: cumulative(+,+,+,+,+),
  args:  ["StartTimes":  "Collection of start times for tasks (integer variables or integers)",
          "Durations":   "Collection of duration for tasks (non-negative integer variables or integers)",
          "Usages":   "Collection of resource usages (positive integers)",
          "ResourceLimit": "Maximum amount of resource available
                            (positive integer)",
          "Scheduled":   "Collection of N scheduled booleans for task (0/1"
" domain variables or integers)"
         ],
  summary: "Single resource cumulative constraint on scheduling optional tasks.",
  see_also: [disjunctive/2, disjunctive_optional/3, cumulative/4, collection_to_list/2, _:cumulative/4],
  desc:    html("\
<P>
   A cumulative scheduling constraint. StartTimes, Durations, Usages and 
   Scheduled are collections (a la collection_to_list/2) of equal size N,
   representing N task. Durations must be non-negative, Usages and 
   ResourceLimit must be strictly positive, and Scheduled are booleans 
   (values of 0/1). The declarative meaning is:
</P><P>
   The N tasks, each starting at a certain start time, having
   a certain duration and consuming a certain (constant) amount of
   resource, then the sum of resource usage of all the tasks does not
   exceed ResourceLimit at any time. A task would not be scheduled 
   if its Scheduled boolean is 0, and must be scheduled if 1.
</P><P>
   Note that the constraint is implemented by different Gecode propagators,
   depending on if Durations contains domain variables or not. If
   Durations does have domain variables, the Gecode propagator requires
   an extra End domain variable specifying the end time, and a constraint 
<PRE>        
     End #= Start + Duration  
</PRE>
   for each task. These are posted as part of the constraint (the End 
   variables are not accessible by the user).
</P><P>
   Any input variables which are not already domain variables are turned
   into domain variables with default domain.
</P><P>
")
]).


%----------------------------------------------------------------------

:- comment(cumulatives/5, [
  amode: cumulatives(+,+,+,+,++),
  template:"<ConsistencyModule:> cumulatives(+StartTimes, +Durations, +Hights, +Assigned, +MachineCapacities)",
  args:  ["StartTimes":  "Collection of N start times for tasks (domain variables or integers)",
          "Durations":   "Collection of N duration for tasks (domain variables or integers)",
          "Hights":   "Collection of N resource usages (positive) or productions"
" (negative) by tasks (domain variables or integers) with the assigned machine",
          "Assigned": "Collection of N ID of machine assigned to tasks"
" (domain variables or integers)",
          "MachineCapacities": "Collection of M maximum amount of resource"
" available for machines (integers)"
         ],
  summary: "Multi-resource cumulatives constraint on specified tasks.",
  see_also: [disjunctive/2, cumulative/5, collection_to_list/2, _:cumulative/4,cumulatives_g/5],
  desc:    html("\
<P>
   A multi-resource cumulatives scheduling constraint - scheduling of M
   machines providing resources for N tasks. StartTimes, Durations, Heights 
   and Assigned are collections (a la collection_to_list/2) of equal size N 
   of domain variables or integers. MachineLimits is a collection of M 
   integers. The declarative meaning is:
   If there are N tasks and M machines, each machine having a limit of 
   resource that can be consumed at any single time-point, and each task 
   starting at a certain start time, having a certain duration and 
   consuming/producing a certain (constant) amount of resource for the 
   machine assigned to the task, then the sum of resource usage for each
   machine by all the tasks does not exceed the capacity for that machine at
   any time. 
</P><P>
   Any input variables which are not already domain variables are turned
   into domain variables with default domain.
</P><P>
    Note that the Gecode implementation of this constraint has index starting
    from 0, i.e. the numbering for the machines starts from 0. These native 
    indices are mapped to the  ECLiPSe indices starting from 1 with an 
    additional dummy zero'th machine that is not used. A version of this 
    constraint that uses native Gecode indexing is available 
    as cumulatives_g/5.
</P><P>
    ConsistencyModule is the optional module specification to give the 
    consistency level for the propagation for this constraint: 
    gfd_vc for value consistency.
</P>")
]).



:- comment(cumulatives_g/5, [
  amode: cumulatives_g(+,+,+,+,++),
  template:"<ConsistencyModule:> cumulatives_g(+StartTimes, +Durations, +Hights, +Assigned, +MachineCapacities)",
  args:  ["StartTimes":  "Collection of N start times for tasks (domain variables or integers)",
          "Durations":   "Collection of N duration for tasks (domain variables or integers)",
          "Hights":   "Collection of N resource usages (positive) or productions"
" (negative) by tasks (domain variables or integers) with the assigned machine",
          "Assigned": "Collection of N ID of machine assigned to tasks"
" (domain variables or integers)",
          "MachineCapacities": "Collection of M maximum amount of resource"
" available for machines (integers)"
         ],
  summary: "Multi-resource cumulatives constraint on specified tasks, using native Gecode indexing.",
  see_also: [cumulatives/5],
  desc:    html("\
  This version of the constraint uses the native Gecode indexing, which starts 
  from 0. This is different from normal ECLiPSe's indexing, which starts from 1.
</p><p>
  This predicate maps more directly to Gecode's native implementation of 
  the constraint, without the conversion between Gecode and ECLiPSe
  indexing of cumulatives/5. It may therefore be more efficient, but could 
  also be incompatible with existing ECLiPSe code. 
</p><p>
  See cumulatives/5 for a more detailed description of this predicate.")
]).   

:- comment(cumulatives_min/5, [
  amode: cumulatives_min(+,+,+,+,++),
  template:"<ConsistencyModule:> cumulatives_min(+StartTimes, +Durations, +Hights, +Assigned, +MachineConsumptions)",
  args:  ["StartTimes":  "Collection of N start times for tasks (domain variables or integers)",
          "Durations":   "Collection of N duration for tasks (domain variables or integers)",
          "Hights":   "Collection of N resource usages (positive) or productions"
" (negative) by tasks (domain variables or integers) with the assigned machine",
          "Assigned": "Collection of N ID of machine assigned to tasks"
" (domains variables or integers)",
          "MachineConsumptions": "Collection of M minimum amount of resource"
" consumptions for machines (integers)"
         ],
  summary: "Multi-resource cumulatives constraint on specified tasks with"
" required minimum resource consumptions.",
  see_also: [disjunctive/2, cumulative/5, collection_to_list/2, _:cumulative/4],
  desc:    html("\
<P>
   A multi-resource cumulatives scheduling constraint - scheduling of M
   machines providing resources for N tasks. StartTimes, Durations, Heights 
   and Assigned are collections (a la collection_to_list/2) of equal size N 
   of domain variables or integers. MinUsages is a collection of M 
   integers. The declarative meaning is:
   If there are N tasks and M machines, each machine having a minimum of 
   produce that must be consumed at any single time-point, and each task 
   starting at a certain start  time, having a certain duration and 
   consuming/producing a certain (constant) amount of produce for the 
   machine assigned to the task, then the sum of resource consumption for each
   machine by all the tasks must be at least the minimum for that machine at
   any time. 
</P><P>
   Any input variables which are not already domain variables are turned
   into domain variables with default domain.
</P><P>

    Note that the Gecode implementation of this constraint has index starting
    from 0, i.e. the numbering for the machines starts from 0. These native 
    indices are mapped to the  ECLiPSe indices starting from 1 with an 
    additional dummy zero'th machine that is not used. A version of this 
    constraint that uses native Gecode indexing is available 
    as cumulatives_min_g/5.
</P><P>
    ConsistencyModule is the optional module specification to give the 
    consistency level for the propagation for this constraint: 
    gfd_vc for value consistency.
</P>"
)
]).


:- comment(cumulatives_min_g/5, [
  amode: cumulatives_min_g(+,+,+,+,++),
  template:"<ConsistencyModule:> cumulatives_min_g(+StartTimes, +Durations, +Hights, +Assigned, +MachineConsumptions)",
  args:  ["StartTimes":  "Collection of N start times for tasks (domain variables or integers)",
          "Durations":   "Collection of N duration for tasks (domain variables or integers)",
          "Hights":   "Collection of N resource usages (positive) or productions"
" (negative) by tasks (domain variables or integers) with the assigned machine",
          "Assigned": "Collection of N ID of machine assigned to tasks"
" (domains variables or integers)",
          "MachineConsumptions": "Collection of M minimum amount of resource"
" consumptions for machines (integers)"
         ],
  summary: "Multi-resource cumulatives constraint on specified tasks with"
" required minimum resource consumptions, using native Gecode indexing.",
  see_also: [cumulatives_min/5],
  desc:    html("\
  This version of the constraint uses the native Gecode indexing, which starts 
  from 0. This is different from normal ECLiPSe's indexing, which starts from 1.
</p><p>
  This predicate maps more directly to Gecode's native implementation of 
  the constraint, without the conversion between Gecode and ECLiPSe
  indexing of cumulatives_min/5. It may therefore be more efficient, but 
  could also be incompatible with existing ECLiPSe code. 
</p><p>
  See cumulatives_min/5 for a more detailed description of this predicate.")
]).   

% ----------------------------------------------------------------------

:- comment(sequence/5, [
        template:"<ConsistencyModule:> sequence(+Low,+High,+K,+Vars,++Values)",
        amode: sequence(+,+,+,+,++),
        args: ["Low":"Non-negative integer",
               "High":"Positive integer",
               "K": "Positive integer",
               "Vars": "A list of variables or integers",
               "Values": "A list of (different) integers"
              ],
        summary: "The number of values taken from Values is between Low and"
                 " High for all sequences of K variables in Vars.", 
        see_also: [sequence/4],
        desc: html("\
<P>
    This constraint ensures that the number of values taken from the set
    specified in Values is at least Low and at most High for all sequences 
    of K consecutive variables/values in Vars. 
</P><P>
    This constraint is known as among_seq in the global constraint catalog.
</P><P>
    ConsistencyModule is the optional module specification to give the 
    consistency level for the propagation for this constraint: 
    gfd_gac for generalised arc consistency (domain consistency), 
") 
         ]
).

:- comment(sequence/4, [
        amode: sequence(+,+,+,+),
        template:"<ConsistencyModule:> sequence(+Low,+High,+K,+ZeroOnes)",
        args: ["Low":"Non-negative integer",
               "High":"Positive integer",
               "K": "Positive integer",
               "ZeroOnes": "A collection of 0/1 variables or integers"
              ],
        summary: "The number of occurrences of the value 1 is between Low and"
                 " High for all sequences of K variables in ZeroOnes", 
        see_also: [sequence/5],
        desc: html("\
<P>
    This constraint ensures that the number of occurrences of the value 1
    is at least Low and at most High for all sequences of K consecutive 
    variables/values in ZeroOnes. ZeroOnes are 0/1 variables (or integers), 
    i.e. they have the domain [0,1]. 
</P><P>
    The ZeroOnes can be interpreted as the fulfillment of various
    conditions if the variables are linked to these conditions. 
</P><P>
    ConsistencyModule is the optional module specification to give the 
    consistency level for the propagation for this constraint: 
    gfd_gac for generalised arc consistency (domain consistency), 
") 
         ]
).


%----------------------------------------------------------------------

:- comment(bin_packing/3, [
       amode: bin_packing(+,++,+),
       args: ["Items": "A collection of M variables or integers (domain/value"
                       " between 1 and N)",
              "ItemSizes": "A collection of M non-negative integers",
              "BinLoads": "A collection of N variables or non-negative integers"
             ],
       see_also:[bin_packing/4,bin_packing_g/3],
       summary:"The one-dimensional bin packing constraint with loads: packing "
               "M items into N bins, each bin having a load",
       desc: html("\
   This constraint is for one-dimensional bin-packing, that is, to pack M
   items with individual sizes into N bins, such that the sum of sizes of
   items in each bin equals the load of that bin, as specified in BinLoads.
   Each element of Items and its corresponding element in ItemSizes
   represents an item, such that the i'th element of ItemSizes is the size
   of the i'th item, and the i'th element of Item is the bin this item is
   packed into. BinLoads represent the load of each bin, i.e. the sum
   of the sizes of items assigned to that bin, with the j'th element 
   representing the load for bin j. An (integer finite domain) variable for 
   the load allows a constraint on the load to be specified, such as a
   minimum and/or maximum load for the bin.
</P><P>
   This constraint and the algorithm used to implement it is described in
   P. Shaw, 'A Constraint for Bin Packing', CP'2004, and is described in
   the global constraint catalog as bin_packing, in the variation where
   the CAPACITY parameter is replaced by a collection of domain variables
   (BinLoads).
</P><P>
    Note that the Gecode implementation of this constraint has index starting
    from 0, i.e. the numbering for the bins starts from 0. These native 
    indices are mapped to the  ECLiPSe indices starting from 1 with an 
    additional dummy zero'th bin that is assigned a dummy item 0. A version
    of this constraint that uses native Gecode indexing is available 
    as bin_packing_g/3.
</p>
")
          ]).

:- comment(bin_packing_g/3, [
       amode: bin_packing(+,++,+),
       args: ["Items": "A collection of M variables or integers (domain/value"
                       " between 0 and N-1)",
              "ItemSizes": "A collection of M non-negative integers",
              "BinLoads": "A collection of N variables or non-negative integers"
             ],
       see_also:[bin_packing/4,bin_packing/3],
       summary:"The one-dimensional bin packing constraint with loads, using native Gecode indexing",
  desc:    html("\
  This version of the constraint uses the native Gecode indexing, which starts 
  from 0. This is different from normal ECLiPSe's indexing, which starts from 1.
</p><p>
  This predicate maps more directly to Gecode's native implementation of 
  the constraint, without the conversion between Gecode and ECLiPSe
  indexing of cumulatives_min/5. It may therefore be more efficient, but 
  could also be incompatible with existing ECLiPSe code. 
</p><p>
  See bin_packing/3 for a more detailed description of this predicate.")
]).   

:- comment(bin_packing/4, [
       amode: bin_packing(+,++,+,+),
       args: ["Items": "A collection of M variables or integers (domain/value"
                       " between 1 and N)",
              "ItemSizes": "A collection of M non-negative integers",
              "N": "A positive Integer",
              "BinSize": "A non-negative integer"
             ],
       see_also:[bin_packing/3, cumulative/4],
       summary:"The one-dimensional bin packing constraint: packing M items"
               " into N bins of size BinSize.",
       desc: html("\
   This constraint is for one-dimensional bin-packing, that is, to pack M
   items with individual sizes into N bins, such that the sum of sizes of 
   items in each bin does not exceed BinSize. Each element of Items and its 
   corresponding element in ItemSizes represents an item, such that the i'th 
   element of ItemSizes is the size of the i'th item, and the i'th element in
   Items is the bin this item is packed into. 
</P><P>
   This constraint can be seen as a special case of the cumulative/4
   constraint, where all task durations are equal to 1, each bin
   represents a time point, and BinSize corresponds to the Resource.
</P><P>
   This constraint and the algorithm used to implement it is described in
   P. Shaw, 'A Constraint for Bin Packing', CP'2004, with a fixed size for 
   the bins. It is also described in the global constraint catalog as 
   bin_packing, but with slightly different arguments: in the catalog, N
   (the number of bins) is implicitly defined by the domain of the variables 
   in Items, and the representation of item is grouped into a single argument
   of collection of pairs, each pair representing an item: the bin to pack 
   the item, and its size.
</p><p>
   Note that this constraint is implemented using the more general 
   bin_packing/4, where each bin has its own size, represented by a domain
   variable, as this is what is implemented by Gecode. This form of
   the constraint with a fixed BinSize is more common. so it is
   provided for convenience and compatibility. Note that this constraint
   uses ECLiPSe indexing -- bins are indexed starting from 1. There is no     
   Gecode indexing version of this constraint as it is not implemented
   directly in Gecode.
</p>
")
          ]).


% ----------------------------------------------------------------------

:- comment(lex_le/2, [
    summary:"Collection1 is lexicographically less or equal to Collection2",
    amode:lex_le(+,+),
    args:[
	"Collection1":"Collection of integers or domain variables",
	"Collection2":"Collection of integers or domain variables"
    ],
    desc:html("\
    	Imposes a lexicographic ordering between the two lists. 
	I.e.  either is the first element of Collection1 strictly smaller
	than the first element of Collection2, or the first elements are
	equal and the lexicographic order holds between the two list
	tails. A non-existing element (i.e. when the end of list is 
        reached) is strictly smaller than any existing element.
</P><P>
        Restrictions in the Gecode version used requires the two
        collections to have equal lengths.
</P><P>
        This constraint is known as lex_lesseq in the global constraint
        catalog. 
")
]).


:- comment(lex_lt/2, [
    summary:"Collection1 is lexicographically less than  Collection2",
    amode:lex_lt(+,+),
    args:[
	"Collection1":"Collection of integers or domain variables",
	"Collection2":"Collection of integers or domain variables"
    ],
    desc:html("\
    	Imposes a lexicographic ordering between the two lists. 
	I.e.  either is the first element of Collection1 strictly smaller
	than the first element of Collection2, or the first elements are
	equal and the lexicographic order holds between the two list
	tails. A non-existing element (i.e. when the end of list is 
        reached)is strictly smaller than any existing element.
</P><P>
        Restrictions in the Gecode version used requires the two
        collections to have equal lengths.
</P><P>
        This constraint is known as lex_less in the global constraint
        catalog. 
")
]).

:- comment(lex_ge/2, [
    summary:"Collection1 is lexicographically greater or equal to Collection2",
    amode:lex_le(+,+),
    args:[
	"Collection1":"Collection of integers or domain variables",
	"Collection2":"Collection of integers or domain variables"
    ],
    desc:html("\
    	Imposes a lexicographic ordering between the two lists. 
	I.e.  either is the first element of Collection1 strictly larger
	than the first element of Collection2, or the first elements are
	equal and the lexicographic order holds between the two list
	tails. A non-existing element (i.e. when the end of list is 
        reached) is strictly smaller than any existing element.
</P><P>
        Restrictions in the Gecode version used requires the two
        collections to have equal lengths.
</P><P>
        This constraint is known as lex_greatereq in the global constraint
        catalog. 
")
]).


:- comment(lex_gt/2, [
    summary:"Collection1 is lexicographically greater than  Collection2",
    amode:lex_lt(+,+),
    args:[
	"Collection1":"Collection of integers or domain variables",
	"Collection2":"Collection of integers or domain variables"
    ],
    desc:html("\
    	Imposes a lexicographic ordering between the two lists. 
	I.e.  either is the first element of Collection1 strictly greater
	than the first element of Collection2, or the first elements are
	equal and the lexicographic order holds between the two list
	tails. A non-existing element (i.e. when the end of list is 
        reached)is strictly smaller than any existing element.
</P><P>
        Restrictions in the Gecode version used requires the two
        collections to have equal lengths.
</P><P>
        This constraint is known as lex_greater in the global constraint
        catalog. 
")
]).

:- comment(lex_eq/2, [
    summary:"Collection1 is lexicographically equal to Collection2",
    amode:lex_lt(+,+),
    args:[
	"Collection1":"Collection of integers or domain variables",
	"Collection2":"Collection of integers or domain variables"
    ],
    desc:html("\
    	Constrains the two collections to be lexicographically equal, i.e.
	the two collections are the same length, and each
        element is identical to its corresponding element in the
        other collection.
</P><P>
        Restrictions in the Gecode version used requires the two
        collections to have equal lengths.
</P><P>
        This constraint is known as lex_equal in the global constraint
        catalog. 
")
]).

:- comment(lex_neq/2, [
    summary:"Collection1 is lexicographically not equal to Collection2",
    amode:lex_lt(+,+),
    args:[
	"Collection1":"Collection of integers or domain variables",
	"Collection2":"Collection of integers or domain variables"
    ],
    desc:html("\
    	Constrains the two collections to be lexicographically different, i.e.
	the two collections are either different lengths, or at least
        one element in one collection is different from its corresponding
        element in the other collection.
</P><P>
        Restrictions in the Gecode version used requires the two
        collections to have equal lengths.
</P><P>
        This constraint is known as lex_different in the global constraint
        catalog. 
")
]).


%----------------------------------------------------------------------

:- comment(ordered/2, [
    summary:"Constrains List to be ordered according to Relation",
    template:"<ConsistencyModule:> ordered(+Relation,+List)",
    amode:ordered(++,+),
    args:[
	"Relation":"One of the atoms #<, #=<, #>, #>=, #=, #\\=",
	"List":"Collection of integers or domain variables"
    ],
    desc: html("\
      Constrains the elements in List to be ordered according to Relation,
      which is one of #<, #=<, #>, #>=, #=, #\\= (and equivalently, for 
      compatibility, <, =<, >, >=, =, \\=).
</P><P>
      ConsistencyModule is the optional module specification to give the 
      consistency level for the propagation for this constraint: 
        gfd_gac for generalised arc consistency (domain consistency), 
        gfd_bc for bounds consistency, and
        gfd_vc for value consistency.
    "),
    see_also:[lex_le/2,lex_lt/2,lex_ge/2,lex_gt/2,sorted/2,collection_to_list/2]
    ]).


%----------------------------------------------------------------------

:-comment(search/6,[
summary:"Interface to gecode search-engines to perform search in gecode.",
amode:search(+,++,++,+,++,+),

args:[
      "L" : "is a collection (a la collection_to_list/2) of domain
	    variables (Arg = 0) or a collection of terms (Arg > 0)",

      "Arg" :"is an integer, which is 0 if L is a collection of
	    domain variables or greater than 0 if L consists of terms of
	    arity greater than Arg, the value Arg indicates the
	    selected argument of the term",


      "Select" :  "is a predefined variable selection method. Predefined methods are 
            input_order, first_fail, anti_first_fail, smallest, largest, 
            occurrence, anti_occurrence, most_constrained, 
            most_constrained_per_value, least_constrained_per_value, 
            max_regret, max_regret_lwb, min_regret_lwb, max_regret_upb,
            min_regret_upb, random, max_weighted_degree, min_weighted_degree, 
            max_weighted_degree_per_value, min_weighted_degree_per_value",

      "Choice" :  "is the name of a predefine value choice method for choosing
            the value to try for a variable; Predefined choice methods are:
            indomain, indomain_from_max, indomain_min, indomain_max, 
            indomain_middle, indomain_median, indomain_split, 
            indomain_reverse_split, indomain_random, indomain_interval,
            indomain_interval_min, indomain_interval_max",

      "Method" :  "is one of the following:  complete,
            bb_min(Cost:domain variable),
	    restart_min(Cost:domain variable)",

       "Option" :  "is a list of option terms.  Currently recognized are:
	  tiebreak(+Select), stats(+Stats), limits(+Stop), 
          timeout(+Seconds), control(+Control), backtrack(-N), 
          node(+Call), nodes(+N)"
],
desc:html("<b>Search/6</b> provides an interface to gecode's search-engine,
to allow search to be performed by gecode. It is designed to have the same 
arguments as the generic search/6 routine available for integer domain solvers.
so that for common cases, the call will work for both search/6. The generic
search/6 is available in the gfd_search module. The difference is that here
the search is performed by gecode, and is an atomic step when viewed from
ECLiPSe. For the non-optimising search method, backtracking into this
predicate will produce the next solution if it exists. By changing the 
<b>Method</b> argument, different gecode search-engines (implementation 
of different complete, partial and optimising search algorithms (and 
their parameters)) can be selected and controlled. The availability of 
optimising search-engines means that this predicate also provide some of 
the functionality of lib(branch_and_bound). The predicate also provides a 
number of pre-defined variable selection  methods (to choose which variable 
will be assigned next) and some pre-defined value assignment methods 
(to try out the possible values for the selected variable in some 
heuristic order).
</P><P>
In order to allow more structure in the application program, it is possible 
to pass a collection of terms rather than only a collection of domain 
variables. In this way all information about some entity can be easily 
grouped together. 
<p>
The variable selection and value choice methods are defined by gecode. They
are mapped to the closest matching methods in the generic search/6 (or with
a name following the same convention if the method have no correspondence).
For variable selection, if several entries
have the same heuristic value, then a tiebreak selection method, specified by
the tiebreak method, can be used to chose from these entries. Note that
there are some differences from ECLiPSe search in how the methods are 
applied: variable selection is always performed before each value selection:
in ECLiPSe, once a variable is selected, all the possible values for that
variable are tried on backtracking without re-selecting the variable. 
<p>
The pre-defined <b>selection methods</b> (with the gecode name in brackets) 
use the following criteria:
<ul>
<li><b>input_order</b> (INT_VAR_NONE) the first entry in the list is selected</li>
<li><b>first_fail</b> (INT_VAR_SIZE_MIN) the entry with the smallest domain size is selected</li>
<li><b>anti_first_fail</b> (INT_VAR_SIZE_MAX) the entry with the largest domain size is selected</li>
<li><b>smallest</b> (INT_VAR_MIN_MIN) the entry with the smallest value in the domain is selected</li>
<li><b>largest</b> (INT_VAR_MAX_MAX) the entry with the largest value in the domain is selected</li>
<li><b>occurrence</b> (INT_VAR_DEGREE_MAX) the entry whose corresponding gecode variable with the
largest number of attached propagators is selected</li>

<li><b>anti_occurrence</b> (INT_VAR_DEGREE_MIN) the entry whose corresponding gecode variable with the
smallest number of attached propagators is selected</li>

<li><b>most_constrained</b> (INT_VAR_SIZE_MIN, INT_VAR_DEGREE_MAX) the entry with the smallest domain size is
 selected. If several entries have the same domain size, the entry with the
 largest number of attached constraints is selected. This is provided for
 compatibility, as this define a tiebreak method (occurrence). Any tiebreak
 method defined in options is ignored.</li>

<li><b>max_regret</b> (INT_VAR_REGRET_MIN_MAX) the entry with the largest difference between the
smallest and second smallest value in the domain is selected. This method is
typically used if the variable represents a cost, and we are interested in the
choice which could increase overall cost the most if the best possibility is
not taken. Unfortunately, the implementation sometimes does not always
work. If two decision variables incur the same minimal cost, the regret is not
calculated as zero, but as the difference from this minimal value to the next
greater value. Note this is an alias for max_regret_lwb</li>

<li><b>random</b> (INT_VAR_RND) an entry is selected at random.</li>
<li><b>max_regret_lwb</b> (INT_VAR_REGRET_MIN_MAX) is an alias to max_regret.</li>

<li><b>min_regret_lwb</b> (INT_VAR_REGRET_MIN_MIN) the entry with the smallest difference between the
smallest and second smallest value in the domain is selected.</li>

<li><b>max_regret_upb</b> (INT_VAR_REGRET_MAX_MAX) the entry with the largest difference between the
largest and second largest value in the domain is selected.</li>

<li><b>min_regret_upb</b> (INT_VAR_REGRET_MAX_MIN) the entry with the smallest difference between the
largest and second largest value in the domain is selected.</li>

<li><b>most_constrained_per_value</b> (INT_VAR_SIZE_DEGREE_MAX) the entry with the smallest domain size
divided by the number of attached propagators.</li> 

<li><b>least_constrained_per_value</b> (INT_VAR_SIZE_DEGREE_MIN) the entry with the largest domain size
divided by the number of attached propagators.</li> 

<li><b>max_weighted_degree</b> (INT_VAR_AFC_MAX) the entry with the largest
weighted degree is selected. Weighted degree is call AFC (accumulated failure
count) in gecode, and is a count of the number of failures so far of
propagators associated with the variable, plus the number of propagator
attached to the variable (to give reasonable starting values when there are
not failures yet).</li>

<li><b>min_weighted_degree</b> (INT_VAR_AFC_MIN) the entry with the smallest
weighted degree is selected.</li>

<li><b>max_weighted_degree_per_value</b> (INT_VAR_SIZE_AFC_MAX) the entry with
 the largest domain size divided by weighted degree is selected.</li> 

<li><b>min_weighted_degree_per_value</b> (INT_VAR_SIZE_AFC_MIN) the entry with
 the smallest domain size divided by weighted degree is selected.</li> 

</ul><p>
The pre-defined <b>choice methods</b> (with gecode name in brackets) have the following meaning:
<ul>
<li><b>indomain</b> (INT_VALUES_MIN)
Values are tried in increasing order. 
On failure, the previously tested value is not removed.</li>

<li><b>indomain_from_max</b> (INT_VALUES_MAX)
Values are tried in decreasing order. 
On failure, the previously tested value is not removed.</li>

<li><b>indomain_min</b> (INT_VAL_MIN)
Values are tried in increasing order.  On failure, the previously
tested value is removed.  The values are tested in the same order as
for <b>indomain</b>, but backtracking may occur earlier.</li>

<li><b>indomain_max</b> (INT_VAL_MAX)
Values are tried in decreasing order.  On failure, the previously
tested value is removed.</li>

<li><b>indomain_median</b>(INT_VAL_MED)
Values are tried beginning from the median value of the domain.  On
failure, the previously tested value is removed, and the new median value will
be chosen next.</li>

<li><b>indomain_split</b> (INT_VAL_SPLIT_MIN)
Values are tried by successive domain splitting, trying the lower half
of the domain first.  On failure, the tried interval is removed.  This
enumerates values in the same order as indomain or indomain_min, but
may fail earlier.</li>

<li><b>indomain_reverse_split</b> (INT_VAL_SPLIT_MAX)
Values are tried by successive domain splitting, trying the upper half
of the domain first.  On failure, the tried interval is removed.  This
enumerates values in the same order as indomain or indomain_max, but
may fail earlier.</li>

<li><b>indomain_random</b> (INT_VAL_RND)
Values are tried in a random order.  On backtracking, the previously
tried value is removed.  Using this routine may lead to unreproducible
results, as another call will create random numbers in a different
sequence. </li> 

<li><b>indomain_interval</b> (INT_VAL_RANGE_MIN)
If the domain consists of several intervals, we first branch on the choice of
the interval, choosing the smallest interval.  For one interval, we use domain
splitting.</li>

<li><b>indomain_interval_min</b> (INT_VAL_RANGE_MIN)
Alias for indomain interval.</li>

<li><b>indomain_interval_max</b> (INT_VAL_RANGE_MAX)
If the domain consists of several intervals, we first branch on the choice of
the interval, choosing the largest interval.  For one interval, we use reverse 
domain splitting.</li>

</ul><p>

<p>
The different <b>search methods</b> are
<ul>
<li><b>complete</b> (DFS)
a complete search routine which explores all alternative choices.</li>


<li><b>bb_min(Cost)</b> (BAB)
Branch-and-bound search to find the minimal value for the cost variable Cost.
This should be a domain variable that is instantiated at the end of the
search. The search will return an optimal solution, unless terminated early,
in which case, the best solution found (if there is one) is returned. If Cost
variable is not instantiated at the end of the search, the search is aborted.
This provide some of the functionality of branch-and-bound search in
lib(branch_and_bound), but is less flexible (no user defined search) but is 
likely to be faster.

<li><b>restart_min(Cost)</b> (Restart)
Branch-and-bound search as in bb_min, but the search is restarted after finding
a new solution and imposing the new bound.        

</ul>
The option list is used to pass additional parameters to and from the
procedure.  The currently recognized options are:
<ul>
<li><b>tiebreak(+Selection)</b>
Selection is one of the variable selection methods, and is used as a tie-break
if the primary selection method yields more than one candidate. Obviously not
all combinations of selection methods makes sense (e.g. it should not be the 
same as the primary), but no check is done, they are simply passed to gecode.</li>
 <li><b>stats(+Stats)</b>
Stats is a named gfd_stats structure, defined as:
<pre>
:- export struct(gfd_stats(prop,fail,node,depth,mem)).
</pre>
The fields of the structure should be uninstantiated, and the search predicate
will instantiate the fields with statistics obtained from gecode for the search:
prop for the number of propagations, fail for the number of failed nodes,
node for number of nodes expanded, depth for maximum depth of search stack,
mem for peak memory allocated (in bytes).</li>
<li><b>timeout(+Seconds)</b>
Specify the number of seconds that the search will be performed before it is
terminated. Seconds can be a real or integer number, and 0 means there is
no timeout. The timer is reset each time a new solution is obtained (for the
non-optimising search).

<li><b>limits(+Stats)</b>
Specify limits to stop the search. Stats is the same gfd_stats struct used for
obtaining statistics. To specify a limit for a particular statistics, the
corresponding field should be instantiated to the limit. Only the prop, node, 
fail and mem fields are significant. Entries in the other fields are ignored.</li>

<li><b>control(+Control)</b>
 Control is a named gfd_control structure, defined as:
 <pre>
 :- export struct(gfd_control(commit_distance,adaptive_distance)).
</pre>
This is used to pass information to gecode to control the search. The
 corresponding field should be instantiated to the value passed to gecode. 
See the gecode manual for more details on the options.</li>
 <li><b>backtrack(-N)</b>
Provided for compatibility with generic search/6. Returns the number of fail
 nodes (fail field of statistics.</li> 

<li><b>nodes(++N)</b>
Provided for compatibility with generic search/6. Equivalent to setting the
node field of limits. The node field will be unified with N</li>

</ul>
"),
fail_if:"Fails if the search engine does not find any solution.
For partial search methods, this does not mean that the problem does not 
have a solution.",
resat: 'yes (non-optimising searches)',
eg:"
top:-
	length(L,8),
	L :: 1..8,
	search(L,0,input_order,indomain,complete,[]).

top:-
        length(L,8),
        L::1..8,
        L = [Cost|L],
        search(L,0,input_order,indomain_max,bb_min(Cost),[]).
",

see_also:[indomain/1,indomain/2,labeling/1,delete/5,gfd_search:search/6] 

]).


:-comment(delete/5,[
summary:"Choose a domain variable from a list according to selection criteria.",
amode:delete(-,+,-,++,++),

args:[
      "X" : " a free variable",
      "List" : " a list of variables or terms ",
      "R" : " a free variable ",
      "Arg" : " an integer",
      "Select" : "is a predefined selection method or the name of a"
	         " predicate of arity 2."
],
desc:html("
This predicate chooses one entry in a list of variables or terms based
on some selection criteria. The selected entry is returned in X, with
the rest of the list without X returned in R.<p></p>

Select can be one of the predefined selection method or the name of a user
defined selection method. Predefined methods are:
 input_order, first_fail, anti_first_fail, smallest, largest,
 occurrence, max_regret, max_regret_upb,max_weighted_degree,
 most_constrained, most_constrained_per_value,
 max_weighted_degree_per_value. These are a subset of the variable selection
methods available for search/6, where they are described in more detail.
Any other name is taken as the name of a user-defined predicate of arity 2
which is expected to compute a selection criterion (typically a number), e.g.
my_select for
<pre>
my_select(X,Criterion) :-
	...	% compute Criterion from variable X
</pre>
The variable-selection will then select the variable with the lowest
value of Criterion.  If several variables have the same value, the first
one is selected.
<p>
Unlike the variable selection in search/6, this predicate does the selection
in ECLiPSe, and the coding is based on delete/6 from generic search
 (gfd_search), except for how the property for selection is obtained. This
allowed selection methods not supported by generic search, such as 
weighted degree, to be used. 
"),
fail_if:"fails if the list is empty",
resat:no,

see_also:[indomain/2,search/6, gfd_search:search/6]

]).


:-comment(indomain/2,[
summary:"a flexible way to assign values to finite domain variables",
amode:indomain(?,++),

args:[
"Var":"a domain variable or an integer",
"Method":"one of the atoms min, max, middle, median, split, interval, random or an integer"],
desc:html("This predicate provides a flexible way to assign values to finite 
domain variables.<p>
The available methods are:
<ul>
<li><b>enum</b> Identical to indomain/1. Start enumeration from the smallest
    value upwards, without first removing previously tried values.</li>

<li><b>min</b> Start the enumeration from the smallest value upwards. 
    This behaves like the built-in <b>indomain/1</b>, except that it
    removes previously tested values on backtracking.</li>

<li><b>max</b> Start the enumeration from the largest value
    downwards, removing previously tested values on backtracking.</li>

<li><b>middle</b> Try the enumeration starting from the middle of the
    domain.  On backtracking, this chooses alternatively values above and
    below the middle value, until all alternatives have been tested.</li>

<li><b>median</b> Try the enumeration starting from the median value
    of the domain.  On backtracking, this chooses alternatively values
    above and below the median value, until all alternatives have been
    tested.</li>

<li><b>Value:integer</b> Like middle, but start with the given integer
    <b>Value</b></li>

</ul>
On backtracking, all methods except enum first remove the previously 
tested value before choosing a new one.  This sometimes can have a 
huge impact on the constraint propagation, and normally does not cause 
much overhead, even if no additional propagation occurs.
</p><p>
This predicate is partly based on indomain/2 from generic search. It is
optimised for gfd compared to generic search's indomain/2, and should 
be more efficient. Only a subset of the available methods available to
gfd_search's indomain/2 are implemented.
"),
fail_if:"No",
resat:yes,
eg:"
top:-
	X :: 1..10,
	indomain(X,min),
	write(X),put(32),
	fail.
top.

% writes 1 2 3 4 5 6 7 8 9 10

top:-
	X :: 1..10,
	indomain(X,max),
	write(X),put(32),
	fail.
top.

% writes 10 9 8 7 6 5 4 3 2 1

top:-
	X :: 1..10,
	indomain(X,middle),
	write(X),put(32),
	fail.
top.

% writes 5 6 4 7 3 8 2 9 1 10

top:-
	X :: 1..10,
	indomain(X,median),
	write(X),put(32),
	fail.
top.

% writes 5 6 4 7 3 8 2 9 1 10

top:-
	X :: 1..10,
	indomain(X,3),
	write(X),put(32),
	fail.
top.

% writes 3 4 2 5 1 6 7 8 9 10

",
see_also:[search/6,indomain/1,gfd_search:indomain/2]

]).


%---------------------------------------------------------------------

:- comment(impose_min/2, [
    amode: impose_min(?, ++),
    args: [
    	"Var":   "Variable or integer",
	"Bound": "Lower bound (integer)"
    ],
    summary: "Update (if required) the lower bound of Var.",
    see_also: [impose_max/2, impose_bounds/3, impose_domain/2, exclude/2, exclude_range/3],
    desc: html("<P>
   Primitive for updating the lower bound of Var so that it is at least
   Bound.  A bound update on a variable may fail (when the update empties
   the domain), succeed (possibly updating the variable's bounds), or
   instantiate the variable (in the case where the domain gets restricted to
   a singleton value).</P><P>
</P><P>
   Note that this predicate is intended for use only in implementing
   co-operation with other solvers and constraint propagators at the
   ECLiPSe level, and should not be called from ordinary user code.
   It differs from the usual constraint in that the propagation is not
   performed immediately. Normally, propagation in gecode is performed by
   a suspended goal that is scheduled and woken after each constraint is
   posted.  However, impose_min/2 does not call the woken goal
   scheduler (wake/0), so the propagation goal may not be executed 
   immediately. (It is possible that under some circumstances the goal 
   will be executed, if wake/0 is called indirectly - one example would be
   by the unify handler if the variable becomes ground - but this should not
   be relied upon.) To ensure that the goals are eventually executed, the 
   caller should arrange for wake/0 to be called at some appropriate point 
   in the subsequent execution.  Please see the \"Advanced Control Features\" 
   section of the User Manual for more information about woken goal 
   management.</P>
")

]).

%---------------------------------------------------------------------

:- comment(impose_max/2, [
    amode: impose_max(?, ++),
    args: [
    	"Var":   "Variable or integer",
	"Bound": "Upper bound (integer)"
    ],
    summary: "Update (if required) the upper bound of Var.",
    see_also: [impose_min/2, impose_bounds/3, impose_domain/2, exclude/2, exclude_range/3],
    desc: html("<P>
   Primitive for updating the upper bound of Var so that it is at most
   Bound.  A bound update on a variable may fail (when the update empties
   the domain), succeed (possibly updating the variable's bounds), or
   instantiate the variable (in the case where the domain gets restricted to
   a singleton value).  Note that if the variable's type is integer, its
   bounds will always be adjusted to integral values.</P><P>
</P><P>
   Note that this predicate is intended for use only in implementing
   co-operation with other solvers and constraint propagators at the
   ECLiPSe level, and should not be called from ordinary user code.
   It differs from the usual constraint in that the propagation is not
   performed immediately. Normally, propagation in gecode is performed by
   a suspended goal that is scheduled and woken after each constraint is
   posted.  However, impose_min/2 does not call the woken goal
   scheduler (wake/0), so the propagation goal may not be executed 
   immediately. (It is possible that under some circumstances the goal 
   will be executed, if wake/0 is called indirectly - one example would be
   by the unify handler if the variable becomes ground - but this should not
   be relied upon.) To ensure that the goals are eventually executed, the 
   caller should arrange for wake/0 to be called at some appropriate point 
   in the subsequent execution.  Please see the \"Advanced Control Features\" 
   section of the User Manual for more information about woken goal 
   management.</P>
")
]).


%---------------------------------------------------------------------

:- comment(impose_bounds/3, [
    amode: impose_bounds(?, ++, ++),
    args: [
    	"Var": "Variable or integer",
	"Lo":  "Lower bound (integer)",
	"Hi":  "Upper bound (integer)"
    ],
    summary: "Update (if required) the bounds of Var.",
    see_also: [impose_min/2, impose_max/2],
    desc: html("<P>
   Primitive for updating the upper and lower bounds of Var, As with 
   impose_min/2 and impose_max/2, it is intended for use in implementing 
   co-operation with other solvers, and constraint propagators at the
   ECLiPSe level, and should not be called from ordinary user 
   code (use ::/2 instead).  Its semantics is essentially:
<PRE>
       impose_min(Var, Lo), impose_max(Var, Hi), wake.
</PRE>
")
]).


%---------------------------------------------------------------------

:- comment(exclude/2, [
    amode: exclude(?, ++),
    args: [
    	"Var":  "Domain variable or integer",
	"Excl": "Integer value to exclude"
    ],
    summary: "Exclude the element Excl from the domain of Var.",
    see_also: [exclude_range/3, impose_min/2, impose_max/2, impose_domain/2],
    desc: html("<P>
   Primitive for excluding an element from the domain of a variable.
   The call may fail (when Var is the same integer as Excl),
   succeed (possibly updating the variable's domain), or instantiate the
   variable (when Excl was one of only two domain elements left).</P><P>
</P><P>
   Note that this predicate is intended for use only in implementing
   co-operation with other solvers and constraint propagators at the
   ECLiPSe level, and should not be called from ordinary user code.
   It differs from the usual constraint in that the propagation is not
   performed immediately. Normally, propagation in gecode is performed by
   a suspended goal that is scheduled and woken after each constraint is
   posted.  However, impose_min/2 does not call the woken goal
   scheduler (wake/0), so the propagation goal may not be executed 
   immediately. (It is possible that under some circumstances the goal 
   will be executed, if wake/0 is called indirectly - one example would be
   by the unify handler if the variable becomes ground - but this should not
   be relied upon.) To ensure that the goals are eventually executed, the 
   caller should arrange for wake/0 to be called at some appropriate point 
   in the subsequent execution.  Please see the \"Advanced Control Features\" 
   section of the User Manual for more information about woken goal 
   management.</P>
")
]).


%---------------------------------------------------------------------

:- comment(exclude_range/3, [
    amode: exclude_range(?, ++, ++),
    args: [
    	"Var": "Domain variable or integer",
	"Lo":  "Integer lower bound of range to exclude",
	"Hi":  "Integer upper bound of range to exclude"
    ],
    summary: "Exclude the elements Lo..Hi from the domain of Var.",
    see_also: [exclude/2, impose_min/2, impose_max/2],
    desc: html("<P>
   Primitive for excluding the integers between Lo and Hi (inclusive) from
   the domain of an integer variable.  The call may fail (when the domain of
   Var has no elements outside the range Lo..Hi), succeed (possibly updating
   the variable's domain), or instantiate the variable (in the case where
   the domain gets restricted to a singleton value).</P><P>
</P><P>
   Note that this predicate is intended for use only in implementing
   co-operation with other solvers and constraint propagators at the
   ECLiPSe level, and should not be called from ordinary user code.
   It differs from the usual constraint in that the propagation is not
   performed immediately. Normally, propagation in gecode is performed by
   a suspended goal that is scheduled and woken after each constraint is
   posted.  However, impose_min/2 does not call the woken goal
   scheduler (wake/0), so the propagation goal may not be executed 
   immediately. (It is possible that under some circumstances the goal 
   will be executed, if wake/0 is called indirectly - one example would be
   by the unify handler if the variable becomes ground - but this should not
   be relied upon.) To ensure that the goals are eventually executed, the 
   caller should arrange for wake/0 to be called at some appropriate point 
   in the subsequent execution.  Please see the \"Advanced Control Features\" 
   section of the User Manual for more information about woken goal 
   management.</P>
")
]).


%---------------------------------------------------------------------

:- comment(gfd_maxint/1, [
     amode: gfd_maxint(-),
     args: ["Var": "Variable"],
     summary: "Returns the maximum value allowed in gecode's domain.",
     see_also: [gfd_minint/1],
     desc: html("<P>\
   Returns the maximum value allowed in gecode's domain. It is strongly
   recommended that the user values used in the domain to not approach
   this value, because propagation can easily lead to values outside
   what gecode can support.")]
 ).


%---------------------------------------------------------------------

:- comment(gfd_minint/1, [
     amode: gfd_minint(-),
     args: ["Var": "Variable"],
     summary: "Returns the minimum value allowed in gecode's domain.",
     see_also: [gfd_minint/1],
     desc: html("<P>\
   Returns the minimum value allowed in gecode's domain. It is strongly
   recommended that the user values used in the domain to not approach
   this value, because propagation can easily lead to values outside
   what gecode can support.")]
 ).

%---------------------------------------------------------------------

:- comment(gfd_set_default/2, [
     amode: gfd_set_default(+,+),
     args: ["Parameter": "GFD parameter to set (atom).",
            "DefaultValue": "Default value for Parameter."
           ],
     summary: "Set the default value for GFD Parameter.",
     see_also: [gfd_set_default/2],
     desc: html("<P>\
   Set the default value for parameters:
 <UL>
    <li><b>interval_min</b>
          Minimum for the default interval for domain variables. When a domain
          variable is created implicitly in a constraint, it is given a
          default interval, and this interval should be as small as possible.
          as the efficiency of various propagator depends on the domain
          size. (integer).</li>
    <li><b>interval_max</b>
          Maximum for the default interval for domain variables. (integer).</li>
    <li><b>array_size</b>
          Initial size for the variable array for storing domain variables
          When more variables than can be accommodated in the array is required,
          a new array double the size is created, and the old variables copied
          to the new. Changing the initial size can reduce or avoid this 
          copying overhead. (positive integer).</li>
    <li><b>cloning_distance</b>
          This controls how often the gecode state is cloned. The smaller
          the distance, the more frequent the cloning. Cloning is only done
          at places where the new clone might be useful, roughly if there are
          changes to the state since the last clone, and it is possible to 
          backtrack and make use of the new clone (i.e. there should be
          at least one choice-point between the last clone and the current
          one. Distance is a measure of such points, so a distance of 1 is 
          the minimal distance where a clone may be needed. (positive 
          integer).</li>
</ul>")]
).


%---------------------------------------------------------------------

:- comment(gfd_get_default/2, [
     amode: gfd_get_default(+,-),
     args: ["Parameter": "GFD parameter (atom).",
            "DefaultValue": "Current default value for Parameter."
           ],
     summary: "Get the current default value for GFD Parameter.",
     see_also: [gfd_set_default/2],
     desc: html("<P>\
   Get the default value for parameters:
 <UL>
    <li><b>interval_min</b>
          Minimum for the default interval for domain variables. 
          Initial value: -1000000.</li>
    <li><b>interval_max</b>
          Maximum for the default interval for domain variables. 
          Initial value: 1000000.</li>
    <li><b>array_size</b>
          Initial size for the variable array for storing domain variables
          Initial value: 100.</li>
    <li><b>cloning_distance</b>
          This controls how often the gecode state is cloned. The smaller
          the distance, the more frequent the cloning. Initial value: 2.</li>
</ul>")]
).


%---------------------------------------------------------------------

:- comment(msg/3, [
amode: msg(-,-,?), amode:msg(++,++,?),
args:  ["Var1": "A variable or number",
	"Var2": "A variable or number",
	"MSG": "Most specific generalisation (variable)"
       ],
summary: "Computes the most specific generalisation of Var1 and Var2 that is expressible with GFD variables.",
desc: html("\
<P>
   The most specific generalisation of two intervals is computed and
   returned as MSG.  MSG will be the union of the domains of the two
   variables, with integers treated as a singleton domain.
   If either Var1 or Var2 are domain-less, or have values that cannot be
   expressed as domains, MSG remains unbound.
</P>")
]).


%---------------------------------------------------------------------
:- comment(gfd_var_print/2, hidden).
:- comment(gfd_handle_tr_out/2, hidden).


%---------------------------------------------------------------------
%
% Named struct documentation
%
%---------------------------------------------------------------------

:- comment(struct(gfd_stats), [
        summary: "Structure for obtaining statistics or providing stopping"
                 " limits for gecode search-engines",
        amode: gfd_stats(-,?,?,-,?),
        amode: gfd_stats(-,-,-,-,-),
        desc: "\
  This structure is used in search/6 predicate, which interface to gecode's
  search-engines. The structure can be used to obtain statistics of the
  search via the stats option, in this case the fields of the structure 
  should be uninstantiated, and search/6 will instantiate it when a solution
  is returned. Secondly, the struct can be used in the limits option, to
  specify limits for the search, such that the search will be terminated when 
  the specified limit is exceeded. In this case, the fields for which limits 
  are required should be set. Note that not all fields can be used as limits.
  If the field cannot be used as a limit, it will be ignored.",
        fields: [
            "prop" : "Number of propagations performed. (stats only)",
            "fail":  "Number of failed nodes.",
            "nodes": "Number of nodes expanded.",
            "depth": "Maximum depth of search stack. (stats only)",
            "mem": "peak memory usage (in bytes) by gecode." 
                ]
         ]).

:- comment(struct(gfd_control), [
        summary: "Structure for passing low-level control parameters to gecode"
                 " search-engines.",
        amode: gfd_control(?,?),
        desc: "\
  This structure is used in search/6 predicate, which interface to gecode's
  search-engines. The structure is used by the control option to pass values
  for low-level parameters that control the behaviour gecode search-engine.
  See the gecode documentation for more details explanation of the
  parameters.",
        fields: [
            "commit_distance": "the commit recomputation distance "
                               "(member c_d of Gecode::Search::Options)",
            
            "commit_distance": "the adaptive recomputation distance "
                               "(member a_d of Gecode::Search::Options)"
        ]
  ]).


:- comment(struct(gcc), [
        summary: "Bounds specification for gcc constraint.",
        amode: gcc(+,+,+),
        desc: html("\
    This structure is used to specify the cardinality (number of occurrences)
    of one value for the gcc constraint."),
        fields: ["Low": "Lower bound on the cardinality of Value (integer).",
                 "Hi": "Upper bound on the cardinality of Value (integer).",
                 "Value": "Value whose cardinality is being specified."
        ]
   ]).


:- comment(struct(occ), [
        summary: "Bounds specification for gcc constraint.",
        amode: occ(?,+),
        desc: html("\
    This structure is used to specify the cardinality (number of occurrences)
    of one value for the gcc constraint."),
        fields: ["Occ": "Domain variable or integer specifying the cardinality of Value.",
                 "Value": "Value whose cardinality is being specified."
        ]
   ]).

:- comment(struct(gfd), hidden).

:- comment(struct(gfd_prob), hidden).

:- comment(struct(gfd_space), hidden).
