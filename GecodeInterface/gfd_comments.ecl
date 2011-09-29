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
       provides more flexibility, but is likely to be less efficient,
       because the search is done in ECLiPSe and is therefore not
       tightly integrated with gecode, and also because the 
       gfd_search is not optimised for use with gecode. In addition,
       gfd also provide predicates for both variable selection and
       value choice that are optimised for gecode, which should be more
       efficient than gfd_search.

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
   or as a list of sub-ranges and/or individual elements. Each element
   of the specification is an integer, or is a ground expression that evaluates
   to an integer. All domain elements must be integers within the range allowed 
   by gecode. 
<P>
   For instance:
<PRE>
     X :: 0..1                  % boolean
     X :: -1..5                 % integer between -1 and 5
     X :: [0..3, 5, 8..10]      % any integer from 0 to 10 except 4 and 6
     X :: 0..N*N                % integer between 0 and N*N (N bound
                                % to integer at time of calling)
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
   Note that labeling perform the search in ECLiPSe, but it uses
   indomain/2 with min, which is optimised for use with gecode. 
</P>
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

   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: 
   gfd_vc for value consistency (naive), gfd_bc for bounds consistency, 
   and gfd_gac for domain (generalised arc) consistency.</P><P> 

   This constraint is also known as alldifferent_cst in the global constraints 
   catalog, and is implmntd using Gecode's distinct() constraint.
")
]).

%---------------------------------------------------------------------

:- comment(nvalues/3, [
    summary:"Constrains N, the number of distinct values assigned to "
            "Collection to satisfy the relation N Rel Limit.",
    amode:nvalues(+,+,?),
    args:[
	"Collection": "Collection of integers or domain variables.",
        "RelOp": "One of the atom: #>, #>=, #<, #=<, #=, #\\=",
	"Limit":"Variable or integer"
    ],
    eg:"\
[eclipse 21]: nvalues([4,5,5,4,1,5], (#=), N).

N = 3

[eclipse 22]: nvalues([A,B,C,D], (#>), N).

A = A{[-1000000 .. 1000000]}
B = B{[-1000000 .. 1000000]}
C = C{[-1000000 .. 1000000]}
D = D{[-1000000 .. 1000000]}
N = N{[-1000000 .. 3]}

[eclipse 23]: nvalues([A,B,C,D], (#=), N).

A = A{[-1000000 .. 1000000]}
B = B{[-1000000 .. 1000000]}
C = C{[-1000000 .. 1000000]}
D = D{[-1000000 .. 1000000]}
N = N{[1 .. 4]}

",
    
    desc:html("<P>\
  Constrains N, the number of distinct values assigned to Collection
  to satisfy the relation N Rel Limit.
</P><P>
  Rel can be one of #&gt;, #&gt;=, #&lt;, #=&lt;, #=, #\\= (or equivalently,
  &gt;, &gt;=, &lt;, =&lt;, =, \\=).
</P><P>
  Any input variables which are not already domain variable will be turned 
  into domain variables with default bounds.
</P><P>
  This constraint can be embedded in a constraint expression in its
  functional form (without the last argument).
</P><P>
  This constraint is also known as nvalues in the global constraint catalog. 
  It is implemented by Gecode's nvalue() constraint.
    ") 
]).

%---------------------------------------------------------------------

:- comment(le/2, [
    summary:"Constrains X to be less than or equal to Y.",
    template: "<ConsistencyModule:> le(?X,?Y)",
    amode:le(?,?),
    amode:le(+,?),
    args:[
        "X":"An integer or domain variable, or a collection of integers
 or domain variables",
        "Y":"An integer or domain variable"
    ],
    eg: "\
[eclipse 2]: le([X,Y,Z],3).

X = X{[-1000000 .. 3]}
Y = Y{[-1000000 .. 3]}
Z = Z{[-1000000 .. 3]}

[eclipse 3]: [X,Y] :: 1..10, Z :: 2..5, A :: [1,3..5], le([X,Y,Z], A).


X = X{[1 .. 5]}
Y = Y{[1 .. 5]}
Z = Z{[2 .. 5]}
A = A{[3 .. 5]}

[eclipse 4]: le([2,3,4], 3).   % fail

[eclipse 5]: le(2,2). % succeeds

",
    desc:html("\
   Primitive to constrain X to be less than or equal to  Y (X #=&lt; Y). 
   If X is a collection, then the relation holds for every element of
   X over Y.
   </P><P> 
   Unlike X #=&lt; Y, X must be a either a variable or integer, or a
   collection of variables or integers, and Y must be a variable or 
   integer, and not expressions, because it interfaces directly to Gecode's
   <TT>rel</TT> propagator. The cost of posting this primitive should
   be less than posting X #=&lt; Y, so if you are posting a lot of
   simple constraints of this form, it may be worth your while to use
   this primiatve, but that this primitive is specific to lib(gfd).
   </P><P> 
   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: 
   gfd_bc for bounds consistency, and gfd_gac for domain (generalised 
   arc) consistency. 
</P><P>
   The variant of this constraint with X being a collection is known
   as arith (with the less than or equal relation) in the Global
   Constraint Catalog. The variant with X and Y both being variables
   is known as leq in the Global Constraint Catalog. This constraint
   is implemented using Gecode's rel() constraint.
"),
    see_also: [(#=<)/2,lt/2,gt/2,ge/2,ne/2,eq/2]
]).

%---------------------------------------------------------------------

:- comment(lt/2, [
    summary:"Constrains X to be less than Y.",
    template: "<ConsistencyModule:> lt(?X,?Y)",
    amode:lt(?,?),
    args:[
        "X":"An integer or domain variable, or a collection of integers",
        "Y":"An integer or domain variable"
    ],
    eg: "\
[eclipse 16]:   lt([A,B,C,D], 3).

A = A{[-1000000 .. 2]}
B = B{[-1000000 .. 2]}
C = C{[-1000000 .. 2]}
D = D{[-1000000 .. 2]}

[eclipse 17]: lt([0,1,2], 3).     % succeeds

[eclipse 18]: lt([0,1,2,3], 3).   % fails

[eclipse 20]: lt(3,3).            % fails

[eclipse 21]: lt(3,10).           % succeeds

[eclipse 22]: lt(3,X).

X = X{[4 .. 1000000]}

",
    desc:html("\
   Primitive to constrain X to be less than  Y (X #&lt; Y). 
   If X is a collection, then the relation holds for every element of
   X over Y.
   </P><P> 
   Unlike X #&lt; Y, X must be a either a variable or integer, or a
   collection of variables or integers, and Y must be a variable or 
   integer, and not expressions, because it interfaces directly to Gecode's
   <TT>rel</TT> propagator. The cost of posting this primitive should
   be less than posting X #&lt; Y, so if you are posting a lot of
   simple constraints of this form, it may be worth your while to use
   this primiatve, but that this primitive is specific to lib(gfd).
   </P><P> 
   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: 
   gfd_bc for bounds consistency, and gfd_gac for domain (generalised 
   arc) consistency. 
</P><P>
   The variant of this constraint with X being a collection is known
   as arith (with the less than relation) in the Global Constraint
   Catalog. The variant with X and Y both being variables is known
   as lt in the Global Constraint Catalog. This constraiant is
   implemented using Gecode's rel() constraint.
"),
    see_also: [(#=<)/2,le/2,gt/2,ge/2,ne/2,eq/2]
]).

%---------------------------------------------------------------------

:- comment(ge/2, [
    summary:"Constrains X to be greater than or equal to Y.",
    template: "<ConsistencyModule:> le(?X,?Y)",
    amode:ge(?,?),
    eg:"\
[eclipse 34]: [X,Y,Z] :: 1..10, ge([X,Y,Z], 5).

X = X{[5 .. 10]}
Y = Y{[5 .. 10]}
Z = Z{[5 .. 10]}

[eclipse 35]: [X,Y,Z] :: 1..10, ge([X,Y,Z], A).

X = X{[1 .. 10]}
Y = Y{[1 .. 10]}
Z = Z{[1 .. 10]}
A = A{[-1000000 .. 10]}

[eclipse 36]: ge([3,4,5],3).            % succeed

[eclipse 37]: ge([2,3,4,5],3).          % fail

[eclipse 40]: ge(5,3).                  % succeed

[eclipse 43]: ge(3,5).                  % fail

[eclipse 42]: ge(3,3).                  % succeed

",
    args:[
        "X":"An integer or domain variable, or a collection of integers",
        "Y":"An integer or domain variable"
    ],
    desc:html("\
   Primitive to constrain X to be greater than or equal to  Y (X #&gt;= Y). 
   If X is a collection, then the relation holds for every element of
   X over Y.
   </P><P> 
   Unlike X #&gt;= Y, X must be a either a variable or integer, or a
   collection of variables or integers, and Y must be a variable or 
   integer, and not expressions, because it interfaces directly to Gecode's
   <TT>rel</TT> propagator. The cost of posting this primitive should
   be less than posting X #&gt;= Y, so if you are posting a lot of
   simple constraints of this form, it may be worth your while to use
   this primiatve, but that this primitive is specific to lib(gfd).
   </P><P> 
   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: 
   gfd_bc for bounds consistency, and gfd_gac for domain (generalised 
   arc) consistency. 
</P><P>
   The variant of this constraint with X being a collection is known
   as arith (with the greater than or equal relation) in the Global
   Constraint Catalog. The variant with X and Y both being variables
   is known as geq in the Global Constraint Catalog. This constraint
   is implemented using Gecode's rel() constraint.
"),
    see_also: [(#>=)/2,lt/2,gt/2,le/2,ne/2,eq/2]
]).

%---------------------------------------------------------------------

:- comment(gt/2, [
    summary:"Constrains X to be greater than Y.",
    template: "<ConsistencyModule:> gt(?X,?Y)",
    amode:gt(?,?),
    args:[
        "X":"An integer or domain variable, or a collection of integers",
        "Y":"An integer or domain variable"
    ],
    eg:"\
[eclipse 27]: gt([4,5,6,7], 4).      % succeed

[eclipse 28]: gt([5,6,7], 4).        % fail

[eclipse 30]: gt(4,3).               % succeed
 
[eclipse 31]: gt(3,4).               % fail

[eclipse 32]: gt(1000,X).

X = X{[-1000000 .. 999]}

[eclipse 33]: gt(X,Y).

X = X{[-999999 .. 1000000]}
Y = Y{[-1000000 .. 999999]}

",
    desc:html("\
   Primitive to constrain X to be greater than  Y (X #&gt; Y). 
   If X is a collection, then the relation holds for every element of
   X over Y.
   </P><P> 
   Unlike X #&gt; Y, X must be a either a variable or integer, or a
   collection of variables or integers, and Y must be a variable or 
   integer, and not expressions, because it interfaces directly to Gecode's
   <TT>rel</TT> propagator. The cost of posting this primitive should
   be less than posting X #&gt; Y, so if you are posting a lot of
   simple constraints of this form, it may be worth your while to use
   this primiatve, but that this primitive is specific to lib(gfd).
   </P><P> 
   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: 
   gfd_bc for bounds consistency, and gfd_gac for domain (generalised 
</P><P>
   The variant of this constraint with X being a collection is known
   as arith (with the greater than relation) in the Global
   Constraint Catalog. The variant with X and Y both being variables
   is known as gt in the Global Constraint Catalog. This constraint
   is implemented using Gecode's rel() constraint.
   arc) consistency. 
"),
    see_also: [(#<)/2,le/2,lt/2,ge/2,ne/2,eq/2]
]).

%---------------------------------------------------------------------

:- comment(ne/2, [
    summary:"Constrains X to be not equal to Y.",
    template: "<ConsistencyModule:> ne(?X,?Y)",
    amode:ne(?,?),
    args:[
        "X":"An integer or domain variable, or a collection of integers",
        "Y":"An integer or domain variable"
    ],
    eg:"\
[eclipse 45]:  X :: 1..10, ne([1,3,4,5], X).

X = X{[2, 6 .. 10]}

[eclipse 46]: ne([1,3,4,5], 2).       % succeed

[eclipse 47]: ne([1,3,4,5], 4).       % fail

[eclipse 48]: ne(3,3).                % fail

[eclipse 49]: ne(3,5).                % succeed

",
    desc:html("\
   Primitive to constrain X to be not equal to  Y (X #\\= Y). 
   If X is a collection, then the relation holds for every element of
   X over Y.
   </P><P> 
   Unlike X #\\= Y, X must be a either a variable or integer, or a
   collection of variables or integers, and Y must be a variable or 
   integer, and not expressions, because it interfaces directly to Gecode's
   <TT>rel</TT> propagator. The cost of posting this primitive should
   be less than posting X #\\= Y, so if you are posting a lot of
   simple constraints of this form, it may be worth your while to use
   this primiatve, but that this primitive is specific to lib(gfd).
   </P><P> 
   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: 
   gfd_bc for bounds consistency, and gfd_gac for domain (generalised 
   arc) consistency. 
</P><P>
   The variant of this constraint with X being a collection is known
   as arith (with the not equal relation) in the Global Constraint
   Catalog. The variant with X and Y both being variables is 
   known as neq in the Global Constraint Catalog. This conastraint
   is implemented using Gecode's rel() constraint.
"),
    see_also: [(#=<)/2,le/2,gt/2,ge/2,lt/2,eq/2]
]).

%---------------------------------------------------------------------

:- comment(eq/2, [
    summary:"Constrains X to be equal to Y.",
    template: "<ConsistencyModule:> eq(?X,?Y)",
    amode:eq(?,?),
    args:[
        "X":"An integer or domain variable, or a collection of integers",
        "Y":"An integer or domain variable"
    ],
    eg:"\
[eclipse 51]: eq([1,2,3,5], X).     % fail

[eclipse 52]: eq([1,1,1,1], X).

X = 1

[eclipse 53]: [X,Y] :: 0..10, Z :: 9..15, eq([X,Y,Z], A).

X = X{[9, 10]}
Y = Y{[9, 10]}
Z = Z{[9, 10]}
A = A{[9, 10]}
[eclipse 55]: eq(3,X).

X = 3

Yes (0.00s cpu)
[eclipse 56]: eq(X,3).

X = 3

",
    desc:html("\
   Primitive to constrain X to be equal to  Y (X #= Y). 
   If X is a collection, then the relation holds for every element of
   X over Y.
   </P><P> 
   Unlike X #= Y, X must be a either a variable or integer, or a
   collection of variables or integers, and Y must be a variable or 
   integer, and not expressions, because it interfaces directly to Gecode's
   <TT>rel</TT> propagator. The cost of posting this primitive should
   be less than posting X #= Y, so if you are posting a lot of
   simple constraints of this form, it may be worth your while to use
   this primiatve, but that this primitive is specific to lib(gfd).
   </P><P> 
   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: 
   gfd_bc for bounds consistency, and gfd_gac for domain (generalised 
   arc) consistency. 
</P><P>
   The variant of this constraint with X being a collection is known
   as arith (with the equal to relation) in the Global
   Constraint Catalog. The variant with X and Y both being variables
   is known as eq in the Global Constraint Catalog. This constraint
   is implemented using Gecode's rel() constraint.
"),
    see_also: [(#=<)/2,le/2,gt/2,ge/2,lt/2,ne/2]
]).

%---------------------------------------------------------------------

:- comment(max/3, [
    summary:"Constrain Max to be the maximum of X and Y.",
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
    summary:"Constrains Min to be the minimum of X and Y.",
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
    amode:plus(?,?,?),
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
    amode:mod(?,?,?),
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
    amode:abs(?,?),
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
          Rel can be one of #&gt;, #&gt;=, #&lt;, #=&lt;, #=, #\\= (or equivalently,
          &gt;, &gt;=, &lt;, =&lt;, =, \\=).
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
          Rel can be one of #&gt;, #&gt;=, #&lt;, #=&lt;, #=, #\\= (or equivalently,
          &gt;, &gt;=, &lt;, =&lt;, =, \\=).
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

:- comment(mem/2, [
    amode: mem(+, ?),
    template: "<ConsistencyModule:> mem(+Vars,?Member)",
    args:[
	"Vars": "Collection (a la collection_to_list/2) of variables or integers (NOT arbitrary expressions)",
	"Member":  "Member element of Vars (domain variable or integer)"
    ],
    summary: "Constrains Member to be the a member element in Vars.",
    see_also: [mem/3, collection_to_list/2],
    eg: "\
[eclipse 7]: A :: 1..10, B :: 2..20, mem([A,B], M).

A = A{[1 .. 10]}
B = B{[2 .. 20]}
M = M{[1 .. 20]}


[eclipse 8]: A :: 1..10, B :: 2..20, mem([A,B], M), M #< 5.

A = A{[1 .. 10]}
B = B{[2 .. 20]}
M = M{[1 .. 4]}


[eclipse 9]: A :: 1..10, B :: 2..20, mem([A,B], M), M #< 2.

A = A{[1 .. 10]}
B = B{[2 .. 20]}
M = 1

[eclipse 10]: mem([4,5,5,4,1,5], C).

C = C{[1, 4, 5]}


",
    desc: html("<P>
   Constrains Member to be a one of the elements in Vars.
</P><P>
   Note that this constraint has the same declarative semantics as the
   standard member/2 predicate, but the order of the arguments are
   reversed to allow the constraint to be used in constraint
   expressions.
</P><P>
   This constraint is implemented by Gecode's member() constraint.
</P><P>
   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: 
       gfd_gac for domain (generalised arc) consistency.
")
]).

:- comment(mem/3, [
    amode: mem(+, ?, ?),
    template: "<ConsistencyModule:> mem(+Vars,?Member,?Bool)",
    args:[
	"Vars": "Collection (a la collection_to_list/2) of variables or integers (NOT arbitrary expressions)",
	"Member":  "Member element of Vars (domain variable or
 integer)",
        "Bool": "Reified truth value (0/1 integer or domain variable)"
    
    ],
    summary: "Reflect into Bool the truth of Member being a member element of Vars.",
    see_also: [mem/2, collection_to_list/2],
    eg: "\
[eclipse 11]: A :: 1..10, B :: 2..20, mem([A,B], M, Bool), M #< 2.

A = A{[1 .. 10]}
B = B{[2 .. 20]}
M = M{[-1000000 .. 1]}
Bool = Bool{[0, 1]}


Delayed goals:
        gfd : gfd_do_propagate(gfd_prob(nvars(4)))
Yes (0.00s cpu)
[eclipse 12]: A :: 1..10, B :: 2..20, mem([A,B], M, Bool), M #< 2, Bool = 1.

A = A{[1 .. 10]}
B = B{[2 .. 20]}
M = 1
Bool = 1

[eclipse 13]: A :: 1..10, B :: 2..20, mem([A,B], M, Bool), M #< 2, Bool = 0.

A = A{[1 .. 10]}
B = B{[2 .. 20]}
M = M{[-1000000 .. 1]}
Bool = 0

",
    desc: html("<P>
   Reified form of the mem/2 constraint, which constrains Member to be
   one of the elements in Vars.
</P><P>
   This constraint is implemented by Gecode's member() constraint 
   (reified version).
</P><P>
   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: 
       gfd_gac for domain (generalised arc) consistency.
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
          Rel can be one of #&gt;, #&gt;=, #&lt;, #=&lt;, #=, #\\= (or equivalently,
          &gt;, &gt;=, &lt;, =&lt;, =, \\=).
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
          Rel can be one of #&gt;, #&gt;=, #&lt;, #=&lt;, #=, #\\= (or equivalently,
          &gt;, &gt;=, &lt;, =&lt;, =, \\=).
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
	summary: "Constrain the number of occurrence of Value in Vars (Occ) to satisfy  the relation Occ Rel N",
	template:"<ConsistencyModule:> count(+Value, ?Vars, +Rel, ?N)",
	args:["+Value" : "An integer",
	      "?Vars" : "A collection (a la collection_to_list/2) of domain variables or integers",
              "+Rel":"One of the atom: #>, #>=, #<, #=<, #=, #\\=",
	      "?N" : "An integer or domain variable"],
	desc:html("<P>\
   Constrain the number of occurrences of Value in Vars to satisfy the
   constraint defined by Rel:
<PRE>
          <number of occurrences of Value in Vars> Rel N
</PRE><P>
   Rel can be one of #&gt;, #&gt;=, #&lt;, #=&lt;, #=, #\\= (or equivalently,
   &gt;, &gt;=, &lt;, =&lt;, =, \\=).
</P><P>
   occurrences/3, atmost/3, atleast/3 are defined using count/3. For example,
<PRE>
         atmost(N, Vars, Value)
</PRE>
   is defined by:
<PRE>
        count(Value, Vars, (#=<), N)
</PRE><P>
  This constraint can be embedded in a constraint expression in its
  functional form (without the last argument).
</P><P>
   This constraint is known as count in the global constraint catalog.
   It is implemented using gecode's count() constraint (variants with
   int or IntVar for argument representing Value).
</P><P>
   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: 
   gfd_gac for domain (generalised arc) consistency. 
")
]).

%----------------------------------------------------------------------

:- comment(among/4, [
	summary: "The number of occurrence (Occ) in Vars of values taken from the set of values specified in Values satisfy  the relation Occ Rel N",
	template:"<ConsistencyModule:> among(+Values, ?Vars, +Rel, ?N)",
	args:["+Values" : "A collection of specifications for integer values",
	      "?Vars" : "A collection (a la collection_to_list/2) of domain variables or integers",
              "+Rel":"One of the atom: #>, #>=, #<, #=<, #=, #\\=",
	      "?N" : "An integer or domain variable"],
        eg:"\
[eclipse 24]: among([1,3,4,9], [4,5,5,4,1,5], (#=), N).

N = 3


[eclipse 25]: among([1..4,9],  [4,5,5,4,1,5], (#=), N).

N = 3


[eclipse 26]:  among([1..4,3,9], [4,5,5,4,1,5], (#=), N). % repeated value

N = 3

[eclipse 2]: among([], [4,5,5,4,1,5], (#=), N).

N = 0

[eclipse 3]: among([1,2,3], [], (#=), N).

N = 0

[eclipse 5]: among([1,3,4,9], [4,5,5,4,1,5], (#\\=), N).

N = N{[-1000000 .. 2, 4 .. 1000000]}

",
        desc:html("<P>\
   Constrain the number of occurrences in Vars of values taken from the set of
   values specified in Value to satisfy the constraint defined by Rel:
<PRE>
          <number of occurrences of values among Values in Vars> Rel N
</PRE><P>
   Rel can be one of #&gt;, #&gt;=, #&lt;, #=&lt;, #=, #\\= (or equivalently,
   &gt;, &gt;=, &lt;, =&lt;, =, \\=).
</P><P>
   Values specify the values whose occurrence are counted, and accept
   the same syntax as domain specification, i.e. each item can be a
   a simple element, or a range Lo .. Hi. Each element is either an
   integer, or is a ground expression that evaluates to an integer. 
</P><P>
   This constraint can be embedded in a constraint expression in its
   functional form (without the last argument).
</P><P>
   This constraint is known as counts in the global constraint catalog,
   with among and among_vars being the specialised form with #= as 
   the Rel (i.e. The number of occurrences of values from Values is
   exactly N), the name among is used here to better distinguish this
   constraint from count/4, for counting the occurrences of a single value.
   This constraint is implemented by gecode's count() contraint (the variant
   with an IntSet argument for Values).
</P><P>
   ConsistencyModule is the optional module specification to give the 
   consistency level for the propagation for this constraint: 
   gfd_gac for domain (generalised arc) consistency. 
")
]).

%----------------------------------------------------------------------

:- comment(count_matches/4, [
	summary: "The number of the elements in Vars that
 match its corresponding value in Values, Matches, satisfies the
 relation Matches Rel N.",
	template:"<ConsistencyModule:> count_matches(+Values, ?Vars, +Rel, ?N)",
	args:["+Values" : "A collection of M integer values",
	      "?Vars" : "A collection of M domain variables or integers",
              "+Rel":"One of the atom: #>, #>=, #<, #=<, #=, #\\=",
	      "?N" : "An integer or domain variable"],
        eg: "\
[eclipse 5]: count_matches([1,2,3,4], [A,B,C,D], (#=), N).

A = A{[-1000000 .. 1000000]}
B = B{[-1000000 .. 1000000]}
C = C{[-1000000 .. 1000000]}
D = D{[-1000000 .. 1000000]}
N = N{[0 .. 4]}

[eclipse 6]: L = [A,B,C,D], L :: 4..10, count_matches([1,2,3,4], L, (#=), N).

L = [A{[4 .. 10]}, B{[4 .. 10]}, C{[4 .. 10]}, D{[4 .. 10]}]
A = A{[4 .. 10]}
B = B{[4 .. 10]}
C = C{[4 .. 10]}
D = D{[4 .. 10]}
N = N{[0, 1]}

[eclipse 15]: count_matches([1,2,3,4], [4,3,2,1], (#=), N).

N = 0

[eclipse 16]: count_matches([1,2,3,4], [2,2,3,5],  (#=), N).

N = 2

[eclipse 17]:  count_matches([], [], (#=), N).

N = 0

",
        desc:html("<P>\
   Values and Vars are collections of the same size, and the
   number of elements in Vars taking on the value given by its corresponding 
   element in Values, Matches, is constrained by the relation:
<PRE>
          <Matches> Rel N
</PRE><P>
   Rel can be one of #&gt;, #&gt;=, #&lt;, #=&lt;, #=, #\\= (or equivalently,
   &gt;, &gt;=, &lt;, =&lt;, =, \\=).
</P><P>
   This constraint can be embedded in a constraint expression in its
   functional form (without the last argument).
</P><P>
   This constraint is implemented by gecode's count() constraint 
   (variant with an IntArgs for Values). 
</P><P>
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
    This constraint is known as sort in the global constraint catalog.
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
    This constraint is known as sort_permutation in the global
    constraint catalog.
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
    amode:sorted_g(+,?,?),
    amode:sorted_g(?,+,?),
    amode:sorted_g(?,?,+),
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
	see_also: [circuit_g/1,circuit_offset_g/2],
        eg: "\
[eclipse 7]: circuit([2,A,4,1]).

A = 3

[eclipse 2]: circuit([]).

No (0.00s cpu)

[eclipse 11]: circuit([A]).

A = 1

",
        desc: html("<P>\
  Succ is a collection of N elements presenting a digraph of N nodes, where
  the i'th element of Succ represents the successor to node i. The constraint
  enforces Succ to form a Hamiltonian circuit, a path through every node in
  the graph, visiting each node once and forming a circuit.</P><P>

  Note that the Gecode implementation of this constraint has index (node id)
  starting from 0, rather than 1. This constraint is actually posted
  as circuit_offset_g/2 with an offset of 1. A version of this constraint
  with native Gecode indexing is available as circuit_g/1.
</P><P>
  ConsistencyModule is the optional module specification to give the 
  consistency level for the propagation for this constraint: 
  gfd_gac for generalised arc consistency (domain consistency), 
  and gfd_vc for value consistency.
</P><P>
  This constraint is known as circuit in the global constraint catalog. It is
  implemented with Gecode's circuit() constraint with an offset of 1.
")
                      ]).

:- comment(circuit_g/1, [
        amode: circuit_g(+),
        args: ["Succ":"A collection of different variables or integers"
              ],
        template:"<ConsistencyModule:> circuit_g(+Succ)",
        summary: "Constrains elements in Succ to form a Hamiltonian circuit, with native Gecode indexing.", 
	see_also: [circuit/1],
        eg: "
circuit_g([A,2,3,0])

A = 1


",
        desc: html("<P>\
  This version of circuit/1 uses the native Gecode indexing, which starts 
  from 0. This is different from normal ECLiPSe's indexing, which starts 
  from 1, and may be incompatible with existing ECLiPSe code. 
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
        see_also: [circuit_offset_g/4, circuit/1,
                   circuit/4, circuit_g/3],
        eg: "\
[eclipse 9]: CostM = []([](0,3,5,7),[](4,0,9,6),[](2,1,0,5),[](-7,8,-2,0)),
             circuit([2,3,4,1], CostM, C).   

CostM = []([](0, 3, 5, 7), [](4, 0, 9, 6), [](2, 1, 0, 5), [](-7, 8, -2, 0))
C = 10
",
        desc: html("<P>\
  Succ is a collection of N elements presenting a digraph of N nodes, where
  the i'th element of Succ represents the successor to node i. The constraint
  enforces Succ to form a Hamiltonian circuit, a path through every node in
  the graph, visiting each node once and forming a circuit. Additionally,
  CostMatrix specifies the cost for traversing between each pair of nodes:
  CostMatrix[i,j] represents the cost of travelling from node i to j, and 
  Cost is constrained to the total cost for the circuit.
</P><P>
  This constraint can be embedded in a constraint expression in its
  functional form (without the last argument).
</P><P>
  Note that the Gecode implementation of this constraint has index (node id)
  starting from 0, rather than 1. This constraint is actually posted
  as circuit_offset_g/4 with an offset of 1. A version of this constraint
  with native Gecode indexing is available as circuit_g/3.
</P><P>
  ConsistencyModule is the optional module specification to give the 
  consistency level for the propagation for this constraint: 
  gfd_gac for generalised arc consistency (domain consistency), 
  and gfd_vc for value consistency.
</P><P>
  This constraint is implemented by Gecode's circuit() constraint (variant with 
  cost), using an offset of 1.
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
        eg: "\
[eclipse 10]: CostM = []([](0,3,5,7),[](4,0,9,6),[](2,1,0,5),[](-7,8,-2,0)),
        circuit_g([1,2,3,0], CostM, C).   

CostM = []([](0, 3, 5, 7), [](4, 0, 9, 6), [](2, 1, 0, 5), [](-7, 8, -2, 0))
C = 10


[eclipse 6]: CostM = []([](0,3,5,7),[](4,0,9,6),[](2,1,0,5),[](-7,8,-2,0)),
        C #= circuit([2,3,4,1], CostM) + 1.

CostM = []([](0, 3, 5, 7), [](4, 0, 9, 6), [](2, 1, 0, 5), [](-7, 8, -2, 0))
C = 11

",
        desc: html("<P>\
  This version of circuit/3 uses the native Gecode indexing, which starts 
  from 0. This is different from normal ECLiPSe's indexing, which starts from 1.</p><p>
  This constraint can be embedded in a constraint expression in its
  functional form (without the last argument).
</P><P>
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
        template:"<ConsistencyModule:> circuit(+Succ,++CostMatrix,+ArcCosts,?Cost)",
	see_also:[circuit/1,circuit/3,circuit_g/4],
        summary: "Constrains elements in Succ to form a Hamiltonian"
                 " circuit with cost Cost.", 
        eg:"\
[eclipse 5]: CostM = []([](0,3,5,7),[](4,0,9,6),[](2,1,0,5),[](-7,8,-2,0)),
        circuit([2,3,4,1], CostM,        [C1,C2,C3,C4], C).

CostM = []([](0, 3, 5, 7), [](4, 0, 9, 6), [](2, 1, 0, 5), [](-7, 8, -2, 0))
C1 = 3
C2 = 9
C3 = 5
C4 = -7
C = 10
",
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
  Note that the Gecode implementation of this constraint has index (node id)
  starting from 0, rather than 1. This constraint is actually posted
  as circuit_offset_g/5 with an offset of 1. A version of this constraint
  with native Gecode indexing is available as circuit_g/4.
</P><P>
  This constraint can be embedded in a constraint expression in its
  functional form (without the last argument).
</P><P>
  ConsistencyModule is the optional module specification to give the 
  consistency level for the propagation for this constraint: 
  gfd_gac for generalised arc consistency (domain consistency), 
  and gfd_vc for value consistency.
</P><P>
  This constraint is implemented by Gecode's circuit() constraint (variant with 
  cost and arc costs), using an offset of 1.
")
                      ]).

:- comment(circuit_g/4, [
        amode: circuit_g(+,++,+,?),
        args: ["Succ":"A collection of N different variables or integers",
               "CostMatrix":"A NxN matrix of integers",
               "ArcCosts": "A collection of N variables or integers.",
               "Cost": "An domain variable or integer."
              ],
        template:"<ConsistencyModule:> circuit_g(+Succ,++CostMatrix,+ArcCosts,?Cost)",
        summary: "Constrains elements in Succ to form a Hamiltonian circuit with cost Cost, using native Gecode indexing.", 
	see_also:[circuit/4],
        eg: "\
CostM = []([](0,3,5,7),[](4,0,9,6),[](2,1,0,5),[](-7,8,-2,0)),
        circuit_g([1,2,3,0], CostM, [C0,C1,C2,C3], C).
",
        desc: html("<P>\
  This version of circuit/4 uses the native Gecode indexing, which starts 
  from 0. This is different from normal ECLiPSe's indexing, which starts from 1.
</P><P>
  This constraint can be embedded in a constraint expression in its
  functional form (without the last argument).
</p><p>
  See circuit/4 for a more detailed description of this predicate.")
]).   

%----------------------------------------------------------------------

:- comment(circuit_offset/2, [
        amode: circuit_offset(+,+),
        args: ["Succ":"A collection of different variables or integers",
               "Offset":"Offset for Succ (An integer)"
              ],
        template:"<ConsistencyModule:> circuit_offset(+Succ,+Offset)",
        summary: "Constrains elements (offset by Offset) in Succ to form a Hamiltonian circuit.", 
	see_also: [circuit_offset_g/2],
        desc: html("<P>\
  Succ is a collection of N elements presenting a digraph of N nodes, where
  the value of the i'th element of Succ - Offset represents the successor to 
  node i. The constraint enforces Succ to form a Hamiltonian circuit,
  a path through every node in the graph, visiting each node once and
  forming a circuit.</P><P>

  Note that the Gecode implementation of this constraint has index (node id)
  starting from 0, rather than 1. The value of Offset is incremented by 1 
  when the constraint is posted to Gecode.  A version of this constraint with
  native Gecode indexing, i.e. without adjusting Offset, is available 
  as circuit_offset_g/2.
</P><P>
  ConsistencyModule is the optional module specification to give the 
  consistency level for the propagation for this constraint: 
  gfd_gac for generalised arc consistency (domain consistency), 
  and gfd_vc for value consistency.
</P><P>
  This constraint is implemented by Gecode's circuit() constraint, using an 
  offset of Offset + 1.
")
                      ]).

:- comment(circuit_offset_g/2, [
        amode: circuit_offset_g(+,+),
        args: ["Succ":"A collection of different variables or integers",
               "Offset":"Offset for Succ (An integer)"
              ],
        template:"<ConsistencyModule:> circuit_offset_g(+Succ, +Offset)",
        summary: "Constrains elements (offset by Offset) in Succ to form a Hamiltonian circuit, with native Gecode indexing.", 
	see_also: [circuit_offset/2],
        desc: html("<P>\
  This version of circuit_offset/2 uses the native Gecode indexing, which
  starts from 0. This is different from normal ECLiPSe's indexing, which
  starts from 1. Offset is not adjusted in this versin. This version of 
  the constraint is provided for completeness, in case the user is using
  native Gecode indexing in their code, so that Offset does not need to
  be adjusted manually by the user. 
 </p><p>
  See circuit_offset/2 for a more detailed description of this predicate.")
]).   

%----------------------------------------------------------------------

:- comment(circuit_offset/4, [
        amode: circuit_offset(+,+,++,?),
        args: ["Succ":"A collection of N different variables or integers",
               "Offset":"Offset for Succ (An integer)",
               "CostMatrix":"A NxN matrix of integers.",
               "Cost": "An domain variable or integer."
              ],
        template:"<ConsistencyModule:> circuit_offset(+Succ,+Offset,++CostMatrix,?Cost)",
        summary: "Constrains elements in Succ (offset by Offset) to form a Hamiltonian circuit with cost Cost.", 
        see_also: [circuit_offset_g/4, circuit_offset/2,
                   circuit_offset/5, circuit/3],
        desc: html("<P>\
  Succ is a collection of N elements presenting a digraph of N nodes, where
  the value of the i'th element of Succ - Offset represents the successor to 
  node i. The constraint enforces Succ to form a Hamiltonian circuit,
  a path through every node in the graph, visiting each node once and
  forming a circuit. Additionally,CostMatrix specifies the cost for 
  traversing between each pair of nodes:
  CostMatrix[i,j] represents the cost of travelling from node i to j, and 
  Cost is constrained to the total cost for the circuit.
</P><P>
  Note that the Gecode implementation of this constraint has index (node id)
  starting from 0, rather than 1. The value of Offset is incremented by 1 
  when the constraint is posted to Gecode.  A version of this constraint with
  native Gecode indexing, i.e. without adjusting Offset, is available 
  as circuit_offset_g/4.
</P><P>
  This constraint can be embedded in a constraint expression in its
  functional form (without the last argument).
</P><P>
  ConsistencyModule is the optional module specification to give the 
  consistency level for the propagation for this constraint: 
  gfd_gac for generalised arc consistency (domain consistency), 
  and gfd_vc for value consistency.
</P><P>
  This constraint is implemented by Gecode's circuit() constraint (variant with 
  cost), using an offset of Offset + 1.
")
                      ]).

:- comment(circuit_offset_g/4, [
        amode: circuit_offset_g(+,+,++,?),
        args: ["Succ":"A collection of N different variables or integers",
               "Offset":"Offset for Succ (An integer)",
               "CostMatrix":"A NxN matrix of integers.",
               "Cost": "An domain variable or integer."
              ],
        template:"<ConsistencyModule:> circuit_offset_g(+Succ,+Offset,++CostMatrix,?Cost)",
        summary: "Constrains elements in Succ (offset by Offset) to form a Hamiltonian circuit with cost Cost. This version uses native Gecode indexing.", 
	see_also: [circuit_offset/4],
        desc: html("<P>\
  This version of circuit_offset/4 uses the native Gecode indexing, which
  starts from 0. This is different from normal ECLiPSe's indexing, which
  starts from 1. Offset is not adjusted in this versin. This version of 
  the constraint is provided for completeness, in case the user is using
  native Gecode indexing in their code, so that Offset does not need to
  be adjusted manually by the user. 
</P><P>
  This constraint can be embedded in a constraint expression in its
  functional form (without the last argument).
</p><p>
  See circuit_offset/4 for a more detailed description of this predicate.")
]).   

%----------------------------------------------------------------------

:- comment(circuit_offset/5, [
        amode: circuit_offset(+,+,++,+,?),
        args: ["Succ":"A collection of N different variables or integers",
               "Offset":"Offset for Succ (An integer)",
               "CostMatrix":"A NxN matrix of integers",
               "ArcCosts": "A collection of N variables or integers.",
               "Cost": "An domain variable or integer."
              ],
        template:"<ConsistencyModule:> circuit_offset(+Succ,+Offset,++CostMatrix,+ArcCosts,?Cost)",
	see_also:[circuit_offset/2,circuit_offset/4,circuit_offset_g/5],
        summary: "Constrains elements in Succ (offset by Offset) to form a Hamiltonian circuit with cost Cost.", 
        desc: html("<P>\
  Succ is a collection of N elements presenting a digraph of N nodes, where
  the value of the i'th element of Succ - Offset represents the successor to 
  node i. The constraint enforces Succ to form a Hamiltonian circuit,
  a path through every node in the graph, visiting each node once and
  forming a circuit. Additionally,CostMatrix specifies the cost for 
  traversing between each pair of nodes:
  CostMatrix[i,j] represents the cost of travelling from node i to j, and 
  Cost is constrained to the total cost for the circuit. The i'th element of 
  ArcCosts is constrained to the cost of the arc in the circuit from node i.
</P><P>
  Note that the Gecode implementation of this constraint has index (node id)
  starting from 0, rather than 1. The value of Offset is incremented by 1 
  when the constraint is posted to Gecode.  A version of this constraint with
  native Gecode indexing, i.e. without adjusting Offset, is available 
  as circuit_offset_g/5.
</P><P>
  This constraint can be embedded in a constraint expression in its
  functional form (without the last argument).
</P><P>
  ConsistencyModule is the optional module specification to give the 
  consistency level for the propagation for this constraint: 
  gfd_gac for generalised arc consistency (domain consistency), 
  and gfd_vc for value consistency.
</P><P>
  This constraint is implemented by Gecode's circuit() constraint (variant with 
  cost and arc costs), using an offset of Offset + 1.
")
                      ]).

:- comment(circuit_offset_g/5, [
        amode: circuit_offset_g(+,+,++,+,?),
        args: ["Succ":"A collection of N different variables or integers",
               "Offset":"Offset for Succ (An integer)",
               "CostMatrix":"A NxN matrix of integers",
               "ArcCosts": "A collection of N variables or integers.",
               "Cost": "An domain variable or integer."
              ],
        template:"<ConsistencyModule:> circuit_offset_g(+Succ,+Offset,++CostMatrix,+ArcCosts,?Cost)",
        summary: "Constrains elements in Succ (offset by Offset) to form a Hamiltonian circuit with cost Cost, using native Gecode indexing.", 
	see_also:[circuit_offset/5],
        desc: html("<P>\
  This version of circuit_offset/5 uses the native Gecode indexing, which starts 
  from 0. This is different from normal ECLiPSe's indexing, which starts from 1.
</P><P>
  This constraint can be embedded in a constraint expression in its
  functional form (without the last argument).
</p><p>
  See circuit_offset/5 for a more detailed description of this predicate.")
]).   

%----------------------------------------------------------------------

:- comment(ham_path/3, [
        amode: ham_path(?,?,+),
        args: ["Start": "An integer or domain variable",
               "End": "An integer or domain variable",
               "Succ":"A collection of different variables or integers"
              ],
        template:"<ConsistencyModule:> ham_path(?Start,?End,+Succ)",
        summary: "Constrains elements in Succ to form a Hamiltonian path from Start to End.",
	see_also: [ham_path_g/3,ham_path_offset_g/4],
        eg: "\
[eclipse 5]: ham_path(S,E,[X]).

S = 1
E = 1
X = 2


[eclipse 2]: ham_path(S,E,[A,B]).

S = S{[1, 2]}
E = E{[1, 2]}
A = A{[2, 3]}
B = B{[1, 3]}

[eclipse 3]: ham_path(2,1,[A,B]).

A = 3
B = 1

[eclipse 5]: ham_path(S,E,[2,4,1]).

S = 3
E = 2

",
        desc: html("<P>\
  Succ is a collection of N elements presenting a digraph of N nodes, where
  the i'th element of Succ represents the successor to node i. The constraint
  enforces Succ to form a Hamiltonian path, a path through every node in
  the graph, visiting each node once, with Start giving the first node
  of the path, and End giving the last node of the path. Note that the
  Succ of the last node will be N+1, i.e. a dummy node not in the graph. 
</P><P>
  Note that the Gecode implementation of this constraint has index (node id)
  starting from 0, rather than 1. This constraint is actually posted
  as ham_path_offset_g/3 with an offset of 1. A version of this constraint
  with native Gecode indexing is available as circuit_g/1.
</P><P>
  ConsistencyModule is the optional module specification to give the 
  consistency level for the propagation for this constraint: 
  gfd_gac for generalised arc consistency (domain consistency), 
  and gfd_vc for value consistency.
</P><P>
  This constraint is implemented by Gecode's path() constraint, using an offset 
  of 1.
")
                      ]).

:- comment(ham_path_g/3, [
        amode: ham_path_g(?,?,+),
        args: ["Start": "An integer or domain variable",
               "End": "An integer or domain variable",
               "Succ":"A collection of different variables or integers"
              ],
        template:"<ConsistencyModule:> ham_path_g(?Start,?End,+Succ)",
        summary: "Constrains elements in Succ to form a Hamiltonian path from Start to End, with native Gecode indexing.", 
	see_also: [ham_path/3],
        eg: "\
[eclipse 6]: ham_path_g(S,E,[A,B]).

S = S{[0, 1]}
E = E{[0, 1]}
A = A{[1, 2]}
B = B{[0, 2]}

[eclipse 7]: ham_path_g(1,0,[A,B]).

A = 2
B = 0


[eclipse 8]: ham_path_g(S,E,[1,3,0]).

S = 2
E = 1

",
        desc: html("<P>\
  This version of ham_path/3 uses the native Gecode indexing, which starts 
  from 0. This is different from normal ECLiPSe's indexing, which starts 
  from 1, and may be incompatible with existing ECLiPSe code. 
</p><p>
  See ham_path/3 for a more detailed description of this predicate.")
]).   

%----------------------------------------------------------------------

:- comment(ham_path/5, [
        amode: ham_path(?,?,+,++,?),
        args: ["Start": "An integer or domain variable",
               "End": "An integer or domain variable",
               "Succ":"A collection of N different variables or integers",
               "CostMatrix":"A NxN matrix of integers.",
               "Cost": "An domain variable or integer."
              ],
        template:"<ConsistencyModule:> ham_path(?Start,?End,+Succ,++CostMatrix,?Cost)",
        summary: "Constrains elements in Succ to form a Hamiltonian path from Start to End with cost Cost.",
        see_also: [ham_path_offset_g/6, ham_path/3,
                   ham_path/6, ham_path_g/5],
        eg: "\
[eclipse 2]: CostM = []([](0,3,5,7),[](4,0,9,6),[](2,1,0,5),[](-7,8,-2,0)),
        ham_path(4,3,[2,3,5,1], CostM, C).

CostM = []([](0, 3, 5, 7), [](4, 0, 9, 6), [](2, 1, 0, 5), [](-7, 8, -2, 0))
C = 5

",
        desc: html("<P>\
  Succ is a collection of N elements presenting a digraph of N nodes, where
  the i'th element of Succ represents the successor to node i. The constraint
  enforces Succ to form a Hamiltonian path, a path through every node in
  the graph, visiting each node once, with Start giving the first node
  of the path, and End giving the last node of the path. Note that the
  Succ of the last node will be N+1, i.e. a dummy node not in the graph. 
  Additionally, CostMatrix specifies the cost for traversing between
  each pair of nodes: CostMatrix[i,j] represents the cost of
  travelling from node i to j, and Cost is constrained to the total cost 
  for the path.
</P><P>
  Note that the Gecode implementation of this constraint has index (node id)
  starting from 0, rather than 1. This constraint is actually posted
  as ham_path_offset_g/4 with an offset of 1. A version of this constraint
  with native Gecode indexing is available as ham_path_g/3.
</P><P>
  This constraint can be embedded in a constraint expression in its
  functional form (without the last argument).
</P><P>
  ConsistencyModule is the optional module specification to give the 
  consistency level for the propagation for this constraint: 
  gfd_gac for generalised arc consistency (domain consistency), 
  and gfd_vc for value consistency.
</P><P>
  This constraint is implemented by Gecode's path() constraint (variant with 
  cost), using an offset of 1.
")
                      ]).

:- comment(ham_path_g/5, [
        amode: ham_path_g(?,?,+,++,?),
        args: ["Start": "An integer or domain variable",
               "End": "An integer or domain variable",
               "Succ":"A collection of N different variables or integers",
               "CostMatrix":"A NxN matrix of integers.",
               "Cost": "An domain variable or integer."
              ],
        template:"<ConsistencyModule:> ham_path_g(?Start,?End,+Succ,++CostMatrix,?Cost)",
        summary: "Constrains elements in Succ to form a Hamiltonian path from Start to End with cost Cost. This version uses native Gecode indexing.", 
	see_also: [ham_path/5],
        eg: "\
[eclipse 2]: CostM = []([](0,3,5,7),[](4,0,9,6),[](2,1,0,5),[](-7,8,-2,0)),
        ham_path_g(3,2,[1,2,4,0], CostM, C).

CostM = []([](0, 3, 5, 7), [](4, 0, 9, 6), [](2, 1, 0, 5), [](-7, 8, -2, 0))
C = 5
",
        desc: html("<P>\
  This version of ham_path/5 uses the native Gecode indexing, which starts 
  from 0. This is different from normal ECLiPSe's indexing, which starts from 1.
</P><P>
  This constraint can be embedded in a constraint expression in its
  functional form (without the last argument).
</p><p>
  See ham_path/5 for a more detailed description of this predicate.")
]).   

%----------------------------------------------------------------------

:- comment(ham_path/6, [
        amode: ham_path(?,?,+,++,+,?),
        args: ["Start": "An integer or domain variable",
               "End": "An integer or domain variable",
               "Succ":"A collection of N different variables or integers",
               "CostMatrix":"A NxN matrix of integers",
               "ArcCosts": "A collection of N variables or integers.",
               "Cost": "An domain variable or integer."
              ],
        template:"<ConsistencyModule:> ham_path(?Start,?End,+Succ,++CostMatrix,+ArcCosts,?Cost)",
	see_also:[ham_path/3,ham_path/5,ham_path_g/6],
        summary: "Constrains elements in Succ to form a Hamiltonian path with cost Cost.", 
        eg: "\
[eclipse 2]:  CostM = []([](0,3,5,7),[](4,0,9,6),[](2,1,0,5),[](-7,8,-2,0)),
              ham_path(4,3,[2,3,5,1], CostM, [C1,C2,C3,C4], C).

CostM = []([](0, 3, 5, 7), [](4, 0, 9, 6), [](2, 1, 0, 5), [](-7, 8, -2, 0))
C1 = 3
C2 = 9
C3 = 0
C4 = -7
C = 5
",

        desc: html("<P>\
  Succ is a collection of N elements presenting a digraph of N nodes, where
  the i'th element of Succ represents the successor to node i. The constraint
  enforces Succ to form a Hamiltonian path, a path through every node in
  the graph, visiting each node once, with Start giving the first node
  of the path, and End giving the last node of the path. Note that the
  Succ of the last node will be N+1, i.e. a dummy node not in the graph. 
  Additionally, CostMatrix specifies the cost for traversing between
  each pair of nodes: CostMatrix[i,j] represents the cost of
  travelling from node i to j, and Cost is constrained to the total cost 
  for the path. The i'th element of ArcCosts is constrained to the cost of 
  the arc in the path from node i.
</P><P>
  Note that the Gecode implementation of this constraint has index (node id)
  starting from 0, rather than 1. This constraint is actually posted
  as ham_path_offset_g/7 with an offset of 1. A version of this constraint
  with native Gecode indexing is available as ham_path_g/6.
</P><P>
  This constraint can be embedded in a constraint expression in its
  functional form (without the last argument).
</P><P>
  ConsistencyModule is the optional module specification to give the 
  consistency level for the propagation for this constraint: 
  gfd_gac for generalised arc consistency (domain consistency), 
  and gfd_vc for value consistency.
</P><P>
  This constraint is implemented by Gecode's path() constraint (variant with 
  cost and arc costs), using an offset of 1.
")
                      ]).

:- comment(ham_path_g/6, [
        amode: ham_path_g(?,?,+,++,+,?),
        args: ["Start": "An integer or domain variable",
               "End": "An integer or domain variable",
               "Succ":"A collection of N different variables or integers",
               "CostMatrix":"A NxN matrix of integers",
               "ArcCosts": "A collection of N variables or integers.",
               "Cost": "An domain variable or integer."
              ],
        template:"<ConsistencyModule:> ham_path_g(?Start,?End,+Succ,++CostMatrix,+ArcCosts,?Cost)",
        summary: "Constrains elements in Succ to form a Hamiltonian path from Start to End, with cost Cost, using native Gecode indexing.", 
	see_also:[ham_path/6],
        eg:"\
[eclipse 3]: CostM = []([](0,3,5,7),[](4,0,9,6),[](2,1,0,5),[](-7,8,-2,0)),
        ham_path_g(3,2,[1,2,4,0], CostM, [C0,C1,C2,C3], C).

CostM = []([](0, 3, 5, 7), [](4, 0, 9, 6), [](2, 1, 0, 5), [](-7, 8, -2, 0))
C0 = 3
C1 = 9
C2 = 0
C3 = -7
C = 5
",
        desc: html("<P>\
  This version of ham_path/6 uses the native Gecode indexing, which starts 
  from 0. This is different from normal ECLiPSe's indexing, which starts from 1.
</P><P>
  This constraint can be embedded in a constraint expression in its
  functional form (without the last argument).
</p><p>
  See ham_path/6 for a more detailed description of this constraint.")
]).   

%----------------------------------------------------------------------

:- comment(ham_path_offset/4, [
        amode: ham_path_offset(?,?,+,+),
        args: ["Start": "An integer or domain variable",
               "End": "An integer or domain variable",
               "Succ":"A collection of different variables or integers",
               "Offset":"Offset for Succ (An integer)"
              ],
        template:"<ConsistencyModule:> ham_path(?Start,?End,+Succ,+Offset)",
        summary: "Constrains elements (offset by Offset) in Succ to form a Hamiltonian path from Start to End.", 
	see_also: [ham_path_offset_g/4],
        desc: html("<P>\
  Succ is a collection of N elements presenting a digraph of N nodes, where
  the i'th element of Succ represents the successor to node i. The constraint
  enforces (Succ -Offset) to form a Hamiltonian path, a path through
  every node in the graph, visiting each node once, with Start giving
  the first node of the path, and End giving the last node of the path.
  Note that the Succ of the last node will be N+1, i.e. a dummy node
  not in the graph. 
</P><P>
  Note that the Gecode implementation of this constraint has index (node id)
  starting from 0, rather than 1. The value of Offset is incremented by 1 
  when the constraint is posted to Gecode.  A version of this constraint with
  native Gecode indexing, i.e. without adjusting Offset, is available 
  as ham_path_offset_g/4.
</P><P>
  ConsistencyModule is the optional module specification to give the 
  consistency level for the propagation for this constraint: 
  gfd_gac for generalised arc consistency (domain consistency), 
  and gfd_vc for value consistency.
</P><P>
  This constraint is implemented by Gecode's path() constraint, with
  an actual offset of 1 + Offset.
")
                      ]).

:- comment(ham_path_offset_g/4, [
        amode: ham_path_offset_g(?,?,+,+),
        args: ["Start": "An integer or domain variable",
               "End": "An integer or domain variable",
               "Succ":"A collection of different variables or integers",
               "Offset":"Offset for Succ (An integer)"
              ],
        template:"<ConsistencyModule:> ham_path_g(?Start,?End,+Succ,+Offset)",
        summary: "Constrains elements (offset by Offset) in Succ to form a Hamiltonian path from Start to End, with native Gecode indexing.", 
	see_also: [ham_path_offset/4],
        desc: html("<P>\
  This version of ham_path_offset/4 uses the native Gecode indexing, which
  starts from 0. This is different from normal ECLiPSe's indexing, which
  starts from 1. Offset is not adjusted in this versin. This version of 
  the constraint is provided for completeness, in case the user is using
  native Gecode indexing in their code, so that Offset does not need to
  be adjusted manually by the user. 
 </p><p>
  See ham_path_offset/4 for a more detailed description of this predicate.")
]).   

%----------------------------------------------------------------------

:- comment(ham_path_offset/6, [
        amode: ham_path_offset(?,?,+,+,++,?),
        args: ["Start": "An integer or domain variable",
               "End": "An integer or domain variable",
               "Succ":"A collection of N different variables or integers",
               "Offset":"Offset for Succ (An integer)",
               "CostMatrix":"A NxN matrix of integers.",
               "Cost": "An domain variable or integer."
              ],
        template:"<ConsistencyModule:> ham_path_offset(?Start,?End,+Succ,+Offset,++CostMatrix,?Cost)",
        summary: "Constrains elements in Succ (offset by Offset) to form a Hamiltonian path from Start to End with cost Cost.", 
        see_also: [ham_path_offset_g/6, ham_path_offset/4,
                   ham_path_offset/7, ham_path/5],
        desc: html("<P>\
  Succ is a collection of N elements presenting a digraph of N nodes, where the
  i'th element of (Succ - Offset) represents the successor to node i.The
  constraint enforces Succ to form a Hamiltonian path, a path through every
  node in the graph, visiting each node once, with Start giving the first node
  of the path, and End giving the last node of the path. Note that the
  Succ of the last node will be N+1, i.e. a dummy node not in the graph. 
  Additionally, CostMatrix specifies the cost for traversing between
  each pair of nodes: CostMatrix[i,j] represents the cost of
  travelling from node i to j, and Cost is constrained to the total cost 
  for the path.
</P><P>
  Note that the Gecode implementation of this constraint has index (node id)
  starting from 0, rather than 1. The value of Offset is incremented by 1 
  when the constraint is posted to Gecode.  A version of this constraint with
  native Gecode indexing, i.e. without adjusting Offset, is available 
  as ham_path_offset_g/6.
</P><P>
  This constraint can be embedded in a constraint expression in its
  functional form (without the last argument).
</P><P>
  ConsistencyModule is the optional module specification to give the 
  consistency level for the propagation for this constraint: 
  gfd_gac for generalised arc consistency (domain consistency), 
  and gfd_vc for value consistency.
</P><P>
  This constraint is implemented by Gecode's path() constraint (variant with 
  cost), using an actual offset of Offset + 1.
")
                      ]).

:- comment(ham_path_offset_g/6, [
        amode: ham_path_offset_g(?,?,+,+,++,?),
        args: ["Start": "An integer or domain variable",
               "End": "An integer or domain variable",
               "Succ":"A collection of N different variables or integers",
               "Offset":"Offset for Succ (An integer)",
               "CostMatrix":"A NxN matrix of integers.",
               "Cost": "An domain variable or integer."
              ],
        template:"<ConsistencyModule:> ham_path_offset_g(?Start,?End,+Succ,+Offset,++CostMatrix,?Cost)",
        summary: "Constrains elements in Succ (offset by Offset) to form a Hamiltonian ham_path with cost Cost. This version uses native Gecode indexing.", 
	see_also: [ham_path_offset/6],
        desc: html("<P>\
  This version of ham_path_offset/4 uses the native Gecode indexing, which
  starts from 0. This is different from normal ECLiPSe's indexing, which
  starts from 1. Offset is not adjusted in this versin. This version of 
  the constraint is provided for completeness, in case the user is using
  native Gecode indexing in their code, so that Offset does not need to
  be adjusted manually by the user. 
</P><P>
  This constraint can be embedded in a constraint expression in its
  functional form (without the last argument).
</p><p>
  See ham_path_offset/6 for a more detailed description of this predicate.")
]).   

%----------------------------------------------------------------------

:- comment(ham_path_offset/7, [
        amode: ham_path_offset(?,?,+,+,++,+,?),
        args: ["Start": "An integer or domain variable",
               "End": "An integer or domain variable",
               "Succ":"A collection of N different variables or integers",
               "Offset":"Offset for Succ (An integer)",
               "CostMatrix":"A NxN matrix of integers",
               "ArcCosts": "A collection of N variables or integers.",
               "Cost": "An domain variable or integer."
              ],
        template:"<ConsistencyModule:> ham_path_offset(?Start,?End,+Succ,+Offset,++CostMatrix,+ArcCosts,?Cost)",
	see_also:[ham_path_offset/4,ham_path_offset/6,ham_path_offset_g/7],
        summary: "Constrains elements in Succ (offset by Offset) to form a Hamiltonian ham_path with cost Cost.", 
        desc: html("<P>\
  Succ is a collection of N elements presenting a digraph of N nodes, where the
  i'th element of (Succ - Offset) represents the successor to node i. The
  constraint enforces Succ to form a Hamiltonian path, a path through every
  node in the graph, visiting each node once, with Start giving the first node
  of the path, and End giving the last node of the path. Note that the
  Succ of the last node will be N+1, i.e. a dummy node not in the graph. 
  Additionally, CostMatrix specifies the cost for traversing between
  each pair of nodes: CostMatrix[i,j] represents the cost of
  travelling from node i to j, and Cost is constrained to the total cost 
  for the path. The i'th element of ArcCosts is constrained to the cost of 
  the arc in the path from node i.
</P><P>
  Note that the Gecode implementation of this constraint has index (node id)
  starting from 0, rather than 1. The value of Offset is incremented by 1 
  when the constraint is posted to Gecode.  A version of this constraint with
  native Gecode indexing, i.e. without adjusting Offset, is available 
  as ham_path_offset_g/5.
</P><P>
  This constraint can be embedded in a constraint expression in its
  functional form (without the last argument).
</P><P>
  ConsistencyModule is the optional module specification to give the 
  consistency level for the propagation for this constraint: 
  gfd_gac for generalised arc consistency (domain consistency), 
  and gfd_vc for value consistency.
</P><P>
  This constraint is implemented by Gecode's path() constraint (variant with 
  cost and arc costs), using an offset of Offset + 1.
")
                      ]).

:- comment(ham_path_offset_g/7, [
        amode: ham_path_offset_g(?,?,+,+,++,+,?),
        args: ["Start": "An integer or domain variable",
               "End": "An integer or domain variable",
               "Succ":"A collection of N different variables or integers",
               "Offset":"Offset for Succ (An integer)",
               "CostMatrix":"A NxN matrix of integers",
               "ArcCosts": "A collection of N variables or integers.",
               "Cost": "An domain variable or integer."
              ],
        template:"<ConsistencyModule:> ham_path_offset_g(?Start,?End,+Succ,+Offset,++CostMatrix,+ArcCosts,?Cost)",
        summary: "Constrains elements in Succ (offset by Offset) to form a Hamiltonian path from Start to End with cost Cost, using native Gecode indexing.", 
	see_also:[ham_path_offset/7],
        desc: html("<P>\
  This version of ham_path_offset/7 uses the native Gecode indexing, which starts 
  from 0. This is different from normal ECLiPSe's indexing, which starts from 1.
</P><P>
  This constraint can be embedded in a constraint expression in its
  functional form (without the last argument).
</p><p>
  See ham_path_offset/7 for a more detailed description of this predicate.")
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
            "Durations":   "Collection of N durations for tasks (non-negative domain variables or integers)",
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

:- comment(disjoint2/1, [
  amode:   disjoint2(+),
  args:    ["Rectangles":  "Collection of rect{} structures specifying
 the position and size of rectangles on a grid."
           ],
  summary: "Constrains the position (and possibly size) of the rectangles in Rectangles so that none overlaps.",
  see_also: [disjoint2_optional/1,collection_to_list/2],
  eg: "\
[eclipse 17]: disjoint2([rect{x:2,y:1,w:2,h:3},rect{x:4,y:3,w:4,h:3},
                   rect{x:9,w:2,y:4,h:3}]).    % succeed

",
  desc:    html("\
<P>
    A two dimensional disjunctive constraint that constrains the replacement
    of a collection of rectangles specified by Rectangles to not overlap in
    their areas. 
</P><P>
    Each rectangle is defined by a rect named structure, using the
    following fields:
<DL>
      <DT>x:<DD> The x co-ordinate of the left side of the rectangle 
      <DT>y:<DD> The y co-ordinate of the bottom side of the rectangle.
      <DT>w:<DD> The width of the rectangle
      <DT>h:<DD> The heigth of the rectangle
</DL>
    x, y, w, h can be domain variables or integers. If w and h are
    integers, then the rectangle is of a fixed size. Note the rect{}
    structure has an additional 'o' field, which is ignored for this
    constraint as it is only used for disjoint2_optional/1.
</P><P>
    Note that the constraint is implemented by different Gecode propagators,
    depending on if all the rectangles are of fixed size or not. If at
    least one rectangle is not of fixed size, then the Gecode
    propagator requires additional variables for the right and top
    sides of the rectangles, plus additional constraints
<PRE>        
      Xright #= Xleft + Width
      Ytop   #= Ybot + Height
</PRE>
    for each rectangles. These are posted as part of the constraint (the Xright 
    and Ytop variables are not accessible by the user).
</P><P>
    Any input variables which are not already domain variables will be
    converted into domain variables with default bounds.
</P><P>
    A version of this constraint, generalised from two to multi-
    dimension, is known as diffn in the Global Constraint Catalog.
    It is implemented using Gecode's nooverlap() constraint.
</P>")
]).

:- comment(disjoint2_optional/1, [
  amode:   disjoint2_optional(+),
  args:    ["Rectangles":  "Collection of rect{} structures specifying
 the position and size of rectangles on a grid."
           ],
  summary: "Constrains the position (and possibly size) of the (possibly optional) rectangles in Rectangles so that none overlaps.",
  see_also: [disjoint2/1, collection_to_list/2],
  desc:    html("\
<P>
    A two dimensional disjunctive constraint that constrains the replacement
    of a collection of rectangles specified by Rectangles to not overlap in
    their areas. The placement of each rectangle can be optional, i.e.
    they may not need to be placed.
</P><P>
    Each rectangle is defined by a rect named structure, using the
    following fields:
<DL>
      <DT>x:<DD> The x co-ordinate of the left side of the rectangle 
      <DT>y:<DD> The y co-ordinate of the bottom side of the rectangle.
      <DT>w:<DD> The width of the rectangle
      <DT>h:<DD> The heigth of the rectangle
      <DT>b:<DD> Boolean specifying if rectangle is placed or not
</DL>
    x, y, w, h can be domain variables or integers. If w and h are
    integers, then the rectangle is of a fixed size. o is a 0/1
    integer or domain variable, 1 specifies that the rectangle needs
    to be placed, and 0 that it is not placed. 
</P><P>
    Note that the constraint is implemented by different Gecode propagators,
    depending on if all the rectangles are of fixed size or not. If at
    least one rectangle is not of fixed size, then the Gecode
    propagator requires additional variables for the right and top
    sides of the rectangles, plus additional constraints
<PRE>        
      Xright #= Xleft + Width
      Ytop   #= Ybot + Height
</PRE>
    for each rectangles. These are posted as part of the constraint (the Xright 
    and Ytop variables are not accessible by the user).
</P><P>
    Any input variables which are not already domain variables will be
    converted into domain variables with default bounds.
</P><P>
    A version of this constraint, generalised from two to multi-
    dimension, and without optional placement, is known as diffn in
    the Global Constraint Catalog. It is implemented using Gecode's 
    nooverlap() constraint (variant with optional placement).
</P>")
]).

%----------------------------------------------------------------------

:- comment(cumulative/4, [
  amode: cumulative(+,+,+,+),
  args:  ["StartTimes":  "Collection of start times for tasks (integer variables or integers)",
          "Durations":   "Collection of duration for tasks (non-negative integer variables or integers)",
          "Usages":   "Collection of resource usages (positive integers)",
          "ResourceLimit": "Maximum amount of resource available (integer variable)"
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
  amode: cumulative_optional(+,+,+,+,+),
  args:  ["StartTimes":  "Collection of start times for tasks (integer variables or integers)",
          "Durations":   "Collection of duration for tasks (non-negative integer variables or integers)",
          "Usages":   "Collection of resource usages (positive integers)",
          "ResourceLimit": "Maximum amount of resource available
                            (integer variable)",
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
       amode: bin_packing_g(+,++,+),
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
        This constraint is known as lex_less in the global constraint
        catalog. 
")
]).

:- comment(lex_ge/2, [
    summary:"Collection1 is lexicographically greater or equal to Collection2",
    amode:lex_ge(+,+),
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
        This constraint is known as lex_greatereq in the global constraint
        catalog. 
")
]).


:- comment(lex_gt/2, [
    summary:"Collection1 is lexicographically greater than  Collection2",
    amode:lex_gt(+,+),
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
        This constraint is known as lex_greater in the global constraint
        catalog. 
")
]).

:- comment(lex_eq/2, [
    summary:"Collection1 is lexicographically equal to Collection2",
    amode:lex_eq(+,+),
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
        This constraint is known as lex_equal in the global constraint
        catalog. 
")
]).

:- comment(lex_neq/2, [
    summary:"Collection1 is lexicographically not equal to Collection2",
    amode:lex_neq(+,+),
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
	"Vars":"Collection of integers or domain variables"
    ],
    eg: "\
[eclipse 9]: ordered((#<), [1,2,3,4]).

Yes (0.00s cpu)
[eclipse 10]: ordered((#<), [1,2,2,3,4]).

No (0.00s cpu)
[eclipse 11]: ordered((#=<), [1,2,3,4]).

Yes (0.00s cpu)
[eclipse 12]: ordered((#=<),  [1,2,2,3,4]).

Yes (0.00s cpu)
[eclipse 13]: ordered((#>), [4,3,2,1]).

Yes (0.00s cpu)
[eclipse 14]: ordered((#>), [4,3,3,2,1]).

No (0.00s cpu)
[eclipse 15]: ordered((#>=), [4,3,2,1]).

Yes (0.00s cpu)

[eclipse 16]:  ordered((#>=), [4,3,3,2,1]).

Yes (0.00s cpu)
[eclipse 17]: ordered((#=), [2,2,3,3]).

No (0.00s cpu)
[eclipse 18]: ordered((#=), [2,2,2,2]).

Yes (0.00s cpu)
[eclipse 19]: ordered((#\\=), [2,2,3,3]).

Yes (0.00s cpu)
[eclipse 20]: ordered((#\\=), [2,2,2,2]).

No (0.00s cpu)
[eclipse 21]: ordered((#>), [2]).

Yes (0.00s cpu)
[eclipse 22]: ordered((#\\=), [X]).

No (0.00s cpu)
[eclipse 23]: [A,B] :: 3..7, [C,D] :: 4..10, ordered((#=), [A,B,C,D]).

A = A{[4 .. 7]}
B = B{[4 .. 7]}
C = C{[4 .. 7]}
D = D{[4 .. 7]}

Yes (0.00s cpu)
[eclipse 24]:  [A,B] :: 3..7, [C,D] :: 4..10, ordered((#>), [A,B,C,D]).

A = 7
B = 6
C = 5
D = 4

",
    desc: html("\
      Constrains the elements in Vars to be ordered according to Relation,
      which is one of  #&gt;, #&gt;=, #&lt;, #=&lt;, #=, #\\= (or equivalently,
      &gt;, &gt;=, &lt;, =&lt;, =, \\=). Except for #\\=, the relation must
      hold between any two adjacent two elements in Vars, for  #\\=, the #\\=
      must hold for at least one adjacent pair of elements in Vars, i.e.
      not elements in Vars are equal.
</P><P>
      ConsistencyModule is the optional module specification to give the 
      consistency level for the propagation for this constraint: 
        gfd_gac for generalised arc consistency (domain consistency), 
        gfd_bc for bounds consistency, and
        gfd_vc for value consistency
</P><P>
     This constraint is known as strictly_increasing (#&gt), increasing (#=&gt;), .
     strictly_decreasing (#&lt;), decreasing (#&lt;=), all_equal (#=), 
     not_all_equal (#\\=) in the Global Constraint Catalog, and is implemented
     using Gecode's rel() constraint (variant with an IntVarArgs and an 
     IntRelType).
    "),
    see_also:[lex_le/2,lex_lt/2,lex_ge/2,lex_gt/2,sorted/2,collection_to_list/2]
    ]).


%----------------------------------------------------------------------

:- comment(precede/3, [
    summary:"Constrains S to precede T in Collection",
    amode:precede(+,+,+),
    args:[
	"S": "Integer",
	"T": "Integer",
	"Collectiont":"Collection of integers or domain variables"
    ],
    eg: "\
[eclipse 14]: precede(0,1, [4,0,6,1,0]).  % succeed (0 appears before 1)

[eclipse 15]: precede(0,1, [](4,0,6,1,0)). % succeed (0 appears before 1)

[eclipse 16]: precede(0,1, [A,B,C,D,E]).

A = A{[-1000000 .. 0, 2 .. 1000000]}
B = B{[-1000000 .. 1000000]}
C = C{[-1000000 .. 1000000]}
D = D{[-1000000 .. 1000000]}
E = E{[-1000000 .. 1000000]}

[eclipse 17]:  precede(0,1, [4,1,6,0,0]).   % fail (1 appears before 0)

",

    desc: html("\
      Constrains the first appearance of value S to precede the first
      appearance of value T in the ordered collection of elements in 
      Collection. S and T do not have to appear in Collection: if only
      S appears, the constraint will succeed, and if only T appears,
      the constraint will fail. If neither appears, the constraint will
      succeed.
</P><P>
      This constraint is known as int_value_precede in the Global
      Constrain Catalog, and is implemented using Gecode's precede()
      constraint (variant with int arguments for s and t).
    "),
    see_also:[precede/2,collection_to_list/2]
    ]).

%----------------------------------------------------------------------

:- comment(precede/2, [
    summary:"Constrains each value in Values to precede its succeeding
 value in Collection",
    amode:precede(++,+),
    args:[
	"Values": "Collection of integers",
	"Collectiont":"Collection of integers or domain variables"
    ],
    eg: "\
[eclipse 18]: precede([4,0,1], [4,0,6,1,0]).   % succeed
[eclipse 19]: precede([4,0,1], [4,0,6,1,0]).  % succeed
[eclipse 20]: precede([4,0,1], [4,1,6,1,0]).   % fail

[eclipse 21]: precede([4,0,1], [A,B,C,D,E]).

A = A{[-1000000 .. -1, 2 .. 1000000]}
B = B{[-1000000 .. 0, 2 .. 1000000]}
C = C{[-1000000 .. 1000000]}
D = D{[-1000000 .. 1000000]}
E = E{[-1000000 .. 1000000]}

",


    desc: html("\
      Constrains the first appearance of every value of the ordered
      collection of integers in Values to precede the first
      appearance of the next value in Values in the ordered collection of 
      elements in Collection, i.e. the precede/3 constraint to hold
      for every adjacent integers in Values.
</P><P>
      This constraint is known as int_value_precede_chain in the Global
      Constrain Catalog, and is implemented using Gecode's precede()
      constraint (variant with IntArg argument for Values).
    "),
    see_also:[precede/3,collection_to_list/2]
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
 :- export struct(gfd_control(commit_distance,adaptive_distance,threads)).
</pre>
This is used to pass information to gecode to control the search. The
 corresponding field should be instantiated to the value passed to gecode. 
 threads may be of most interest as if threads is set to a value &gt;= 2,
 this will allow parallel search. See the gecode manual for more
 details on the options.</li>
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
/*        
        amode: gfd_stats(-,?,?,-,?),
        amode: gfd_stats(-,-,-,-,-),
*/
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
          desc: "\
  This structure is used in search/6 predicate, which interface to gecode's
  search-engines. The structure is used by the control option to pass values
  for low-level parameters that control the behaviour gecode search-engine.
  See the gecode documentation for more details explanation of the
  parameters. For threads, if >= 1, this specifies number of threads
  to use in search, but for < 1, this specify the number of threads in
  relation to the number of processors on the machine, see the gecode
  documentation for more detail. ",
        fields: [
            "commit_distance": "the commit recomputation distance (integer)"
                               "(member c_d of Gecode::Search::Options)",
            
            "adaptive_distance": "the adaptive recomputation distance (integer)"
                               "(member a_d of Gecode::Search::Options)",
            "threads": "number of threads to use in search (integer or float)"
                               "(member threads of Gecode::Search::Options)"
        ]
  ]).


:- comment(struct(gcc), [
        summary: "Bounds specification for gcc constraint.",
          desc: html("\
    This structure is used to specify the cardinality (number of occurrences)
    of one value for the gcc constraint."),
        fields: ["low": "Lower bound on the cardinality of Value (integer).",
                 "high": "Upper bound on the cardinality of Value (integer).",
                 "value": "Value whose cardinality is being specified."
        ]
   ]).


:- comment(struct(occ), [
        summary: "Bounds specification for gcc constraint.",
          desc: html("\
    This structure is used to specify the cardinality (number of occurrences)
    of one value for the gcc constraint."),
        fields: ["occ": "Domain variable or integer specifying the cardinality of Value.",
                 "value": "Value whose cardinality is being specified."
        ]
   ]).

:- comment(struct(rect), [
        summary: "Specification for rectangles used in disjoint2 and disjoint2_optional constraints.",
        desc: "This structure is used for specify the rectangles used
 in disjoint2 and disjoint2_optional constraints. These rectangles are
 placed on a grid.",
        fields: ["x": "the x co-ordinate of the left-side of the rectangle",
                 "y": "the y co-ordinate of the bottom-side of the rectangle",
                 "w": "the width of the rectangle",
                 "h": "the height of the rectangle",
                 "b": "boolean specifying if rectangle is placed (1=placed)"
                ]
                         ]).

%:- comment(struct(gfd), hidden).

%:- comment(struct(gfd_prob), hidden).

%:- comment(struct(gfd_space), hidden).
