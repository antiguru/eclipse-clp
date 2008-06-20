%-----------------------------------------------------------------------------%
% MiniZinc global constraints library, specialised for ECLiPSe(fd)
%
% To support a constraint natively: comment out the definition here,
% replace it with a declaration only, and export a corresponding
% predicate from the ECLiPSe library lib(fzn_fd).
%
%-----------------------------------------------------------------------------%

%-----------------------------------------------------------------------------%
% Constrains the array of objects 'x' to be all different.
%-----------------------------------------------------------------------------%
predicate all_different(array[int] of var int: x);
%predicate all_different(array[int] of var int: x) =
%    forall(i,j in index_set(x) where i < j) ( x[i] != x[j] );

predicate all_different(array[int] of var set of int: x) =
    forall(i,j in index_set(x) where i < j) ( x[i] != x[j] );

%-----------------------------------------------------------------------------%
% Ensures that every pair of sets in the array 'S' is disjoint.
%-----------------------------------------------------------------------------%
predicate all_disjoint(array[int] of var set of int: S);
%predicate all_disjoint(array[int] of var set of int: S) =
%    forall(i,j in index_set(S) where i < j) ( disjoint(S[i], S[j]) );

%-----------------------------------------------------------------------------%
% Requires exactly 'n' variables in 'x' to take one of the values in 'v'.
%-----------------------------------------------------------------------------%
predicate among(var int: n, array[int] of var int: x, set of int: v) =
    n == sum(i in index_set(x)) ( bool2int(x[i] in v) );

%-----------------------------------------------------------------------------%
% Requires at least 'n' variables in 'x' to take the value 'v'.
%-----------------------------------------------------------------------------%
predicate at_least(int: n, array[int] of var int: x, int: v);
%predicate at_least(int: n, array[int] of var int: x, int: v) =
%    sum(i in index_set(x)) ( bool2int(x[i] == v) ) >= n;

predicate at_least(int: n, array[int] of var set of int: x, set of int: v) =
    sum(i in index_set(x)) ( bool2int(x[i] == v) ) >= n;

%-----------------------------------------------------------------------------%
% Requires at most 'n' variables in 'x' to take the value 'v'.
%-----------------------------------------------------------------------------%
predicate at_most(int: n, array[int] of var int: x, int: v);
%predicate at_most(int: n, array[int] of var int: x, int: v) =
%    sum(i in index_set(x)) ( bool2int(x[i] == v) ) <= n;

predicate at_most(int: n, array[int] of var set of int: x, set of int: v) =
    sum(i in index_set(x)) ( bool2int(x[i] == v) ) <= n;

%-----------------------------------------------------------------------------%
% Constrains 'c' to be the number of occurrences of 'y' in 'x'. This
% is less general than the Beldiceanu version, which takes an extra
% relational operation argument, and so allows "equal" in the previous
% sentence to be something else, eg. greater-than.
%-----------------------------------------------------------------------------%
predicate count(array[int] of var int: x, int: y, var int: c);

predicate count(array[int] of var int: x, var int: y, var int: c) =
    c = sum(i in index_set(x)) ( bool2int(x[i] == y) );

%-----------------------------------------------------------------------------%
% Requires that a set of tasks given by start times 's', durations 'd', and
% resource requirements 'r', never require more than a global resource bound
% 'b' at any one time.
% Assumptions:
% - forall i, d[i] >= 0 and r[i] >= 0
%-----------------------------------------------------------------------------%
predicate cumulative(array[int] of var int: s,
                     array[int] of int: d,
                     array[int] of int: r, int: b);

predicate cumulative(array[int] of var int: s,
                     array[int] of var int: d,
                     array[int] of var int: r, var int: b) =
    assert(index_set(s) == index_set(d) /\ index_set(s) == index_set(r),
        "cumulative: the 3 array arguments must have identical index sets",
        assert(lb(d) >= 0 /\ lb(r) >= 0,
            "cumulative: durations and resource usages must be non-negative",
            let { set of int: times =
                    min([lb(s[i]) | i in index_set(s)]) ..
                    max([ub(s[i]) + ub(d[i]) | i in index_set(s)]) } in
            forall(t in times) (
                  b >= sum(i in index_set(s)) (
                           bool2int(s[i] <= t /\ t < s[i] + d[i]) * r[i]
                       )
            )
        )
    );

%-----------------------------------------------------------------------------%
% Requires that sets 's1' and 's2' do not intersect.
%-----------------------------------------------------------------------------%
predicate disjoint(var set of int: s1, var set of int: s2);
%predicate disjoint(var set of int: s1, var set of int: s2) =
%    s1 intersect s2 == {};

%-----------------------------------------------------------------------------%
% Requires that 'card[i]' is the number of occurences of 'value[i]' in 'base'.
%
% XXX: currently the values in 'value' need not be distinct.  Perhaps they
%      should be?
%-----------------------------------------------------------------------------%
predicate distribute(array[int] of var int: card,
                     array[int] of var int: value,
                     array[int] of var int: base) =
    assert(index_set(card) == index_set(value),
        "distribute: card and value arrays must have identical index sets",
        forall (i in index_set(card)) (
            card[i] == sum(j in index_set(base)) (
                            bool2int(value[i] = base[j])
                       )
        )
    );

%-----------------------------------------------------------------------------%
% Requires exactly 'n' variables in 'x' to take the value 'v'.
%-----------------------------------------------------------------------------%
predicate exactly(int: n, array[int] of var int: x, int: v);
%predicate exactly(int: n, array[int] of var int: x, int: v) =
%    n == sum(i in index_set(x)) ( bool2int(x[i] == v) );

predicate exactly(int: n, array[int] of var set of int: x, set of int: v) =
    n == sum(i in index_set(x)) ( bool2int(x[i] == v) );

%-----------------------------------------------------------------------------%
% Requires that 'c[i]' is the number of occurences of 'i' in 'x'.
%-----------------------------------------------------------------------------%
predicate global_cardinality(array[int] of var int: x,
                             array[int] of var int: c) =
    forall(i in index_set(c)) ( count(x, i, c[i]) );

%-----------------------------------------------------------------------------%
% Requires that the array 'x' is in increasing order (duplicates are allowed).
%-----------------------------------------------------------------------------%
predicate increasing(array[int] of var int: x) =
    forall(i in index_set(x) diff { min(index_set(x)) }) (x[i-1] <= x[i]);

predicate increasing(array[int] of var bool: x) =
    forall(i in index_set(x) diff { min(index_set(x)) }) (x[i-1] <= x[i]);

predicate increasing(array[int] of var float: x) =
    forall(i in index_set(x) diff { min(index_set(x)) }) (x[i-1] <= x[i]);

predicate increasing(array[int] of var set of int: x) =
    forall(i in index_set(x) diff { min(index_set(x)) }) (x[i-1] <= x[i]);

%-----------------------------------------------------------------------------%
% Requires that array of int variables 'x' and array of set variables 'y'
% are related such that '(x[i] = j) <-> (i in y[j])'.
%-----------------------------------------------------------------------------%
predicate int_set_channel(array[int] of var int: x,
                          array[int] of var set of int: y) =
    forall(i in index_set(x), j in index_set(y)) ( x[i] == j <-> i in y[j] );

%-----------------------------------------------------------------------------%
% Constrains two arrays of int variables, 'f' and 'invf', to represent
% inverse functions.  All the values in each array must be within the index
% set of the other array.
%-----------------------------------------------------------------------------%
predicate inverse(array[int] of var int: f,
                  array[int] of var int: invf) =
    forall(i in index_set(f), j in index_set(invf)) (
           f[i] in index_set(invf) /\
        invf[j] in index_set(f   ) /\
        (j == f[i] <-> i == invf[j])
    );

%-----------------------------------------------------------------------------%
% Constrains two arrays of set of int variables, 'f' and 'invf', so that
% a 'j in f[i]' iff 'i in invf[j]'.  All the values in each array's sets
% must be within the index set of the other array.
%-----------------------------------------------------------------------------%
predicate inverse_set(array[int] of var set of int: f,
                      array[int] of var set of int: invf) =
    forall(i in index_set(f), j in index_set(invf)) (
           f[i] subset index_set(invf) /\
        invf[j] subset index_set(f   ) /\
        j in f[i] <-> i in invf[j]
    );

%-----------------------------------------------------------------------------%
% Requires that the array 'x' is lexicographically less than array 'y'.
% Compares them from first to last element, regardless of indices
%-----------------------------------------------------------------------------%
predicate lex_less(array[int] of var int: x,
                   array[int] of var int: y) =
    let { int: lx = min(index_set(x)),
          int: ux = max(index_set(x)),
          int: ly = min(index_set(y)),
          int: uy = max(index_set(y)),
          int: size = max(ux - lx, uy - ly),
          array[0..size+1] of var bool: b }
    in
        b[size+1] = (ux - lx < uy - ly) /\
        forall (i in 0..size) (
            b[i] = ((x[lx+i] < y[ly+i])
                     \/ ((x[lx+i] == y[ly+i]) /\ b[i+1]))
        ) /\ b[0];

predicate lex_less(array[int] of var float: x,
                   array[int] of var float: y) =
    let { int: lx = min(index_set(x)),
          int: ux = max(index_set(x)),
          int: ly = min(index_set(y)),
          int: uy = max(index_set(y)),
          int: size = max(ux - lx, uy - ly),
          array[0..size+1] of var bool: b }
    in
        b[size+1] = (ux - lx < uy - ly) /\
        forall (i in 0..size) (
            b[i] = ((x[lx+i] < y[ly+i])
                     \/ ((x[lx+i] == y[ly+i]) /\ b[i+1]))
        ) /\ b[0];

predicate lex_less(array[int] of var set of int: x,
                   array[int] of var set of int: y) =
    let { int: lx = min(index_set(x)),
          int: ux = max(index_set(x)),
          int: ly = min(index_set(y)),
          int: uy = max(index_set(y)),
          int: size = max(ux - lx, uy - ly),
          array[0..size+1] of var bool: b }
    in
        b[size+1] = (ux - lx < uy - ly) /\
        forall (i in 0..size) (
            b[i] = ((x[lx+i] < y[ly+i])
                     \/ ((x[lx+i] == y[ly+i]) /\ b[i+1]))
        ) /\ b[0];

%-----------------------------------------------------------------------------%
% the array of booleans 'b' is a representation of the set 's'
%-----------------------------------------------------------------------------%
predicate link_set_to_booleans(var set of int: s, array[int] of var bool: b);
%predicate link_set_to_booleans(var set of int: s, array[int] of var bool: b) =
%    assert(ub(s) subset index_set(b),
%        "link_set_to_booleans: the index set of b must be a superset of the possible values of s",
%        forall(i in index_set(b)) ( b[i] <-> i in s )
%    );

%-----------------------------------------------------------------------------%
% Constrains 'm' to be the minimum of the values in 'x'.
% Assumptions: |x| > 0.
%-----------------------------------------------------------------------------%
predicate minimum(var int: m, array[int] of var int: x) =
    let { int: l = min(index_set(x)),
          int: u = max(index_set(x)),
          int: ly = lb(x),
          int: uy = ub(x),
          array[l..u] of var ly..uy: y } in
    y[l] = x[l] /\
    m = y[u] /\
    forall (i in l+1 .. u) ( y[i] == min(x[i],y[i-1]) );

predicate minimum(var float: m, array[int] of var float: x) =
    let { int: l = min(index_set(x)),
          int: u = max(index_set(x)),
          float: ly = lb(x),
          float: uy = ub(x),
          array[l..u] of var ly..uy: y } in
    y[l] = x[l] /\
    m = y[u] /\
    forall (i in l+1 .. u) ( y[i] == min(x[i],y[i-1]) );

    % XXX: currently doesn't work:  'uy' is used as a type but that isn't
    %      allowed because it's not a global variable.
%predicate minimum(var set of int: m, array[int] of var set of int: x) =
%    let { int: l = min(index_set(x)),
%          int: u = max(index_set(x)),
%          set of int: uy = ub(x),
%          array[l..u] of var set of uy: y } in
%    y[l] = x[l] /\
%    m = y[u] /\
%    forall (i in l+1 .. u) ( y[i] == min(x[i],y[i-1]) );

%-----------------------------------------------------------------------------%
% Constrains 'm' to be the maximum of the values in 'x'.
% Assumptions: |x| > 0.
%-----------------------------------------------------------------------------%
predicate maximum(var int: m, array[int] of var int: x) =
    let { int: l = min(index_set(x)),
          int: u = max(index_set(x)),
          int: ly = lb(x),
          int: uy = ub(x),
          array[l..u] of var ly..uy: y } in
    y[l] = x[l] /\
    m = y[u] /\
    forall (i in l+1 .. u) ( y[i] == max(x[i],y[i-1]) );

predicate maximum(var float: m, array[int] of var float: x) =
    let { int: l = min(index_set(x)),
          int: u = max(index_set(x)),
          float: ly = lb(x),
          float: uy = ub(x),
          array[l..u] of var ly..uy: y } in
    y[l] = x[l] /\
    m = y[u] /\
    forall (i in l+1 .. u) ( y[i] == max(x[i],y[i-1]) );

    % XXX: currently doesn't work:  same problem as 'minimum' above.
%predicate maximum(var set of int: m, array[int] of var set of int: x) =
%    let { int: l = min(index_set(x)),
%          int: u = max(index_set(x)),
%          set of int: uy = ub(x),
%          array[l..u] of var set of uy: y } in
%    y[l] = x[l] /\
%    m = y[u] /\
%    forall (i in l+1 .. u) ( y[i] == max(x[i],y[i-1]) );

%-----------------------------------------------------------------------------%
% Partitions a 'universe' into disjoint sets.
%-----------------------------------------------------------------------------%
predicate partition_set(array[int] of var set of int: S,
                        set of int: universe) =
    all_disjoint(S) /\ universe == array_union(i in index_set(S)) ( S[i] );

%-----------------------------------------------------------------------------%
% Requires that the image of function 'x' (represented as an array) on
% set of values 's' is 't'.
%-----------------------------------------------------------------------------%
predicate range(array[int] of var int: x, var set of int: s,
                                          var set of int: t) =
    forall(i in ub(t)) (
        i in t -> exists(j in ub(s)) ( j in s -> x[j] == i )
    );

%-----------------------------------------------------------------------------%
% The sequence of values in array 'x' is accepted by the DFA of 'Q' states
% with input 1..S and transition function 'd' and initial state 'q0' and
% accepting states 'F'.  We reserve state 0 to be an always failing state.
%-----------------------------------------------------------------------------%
predicate regular(array[int] of var int: x, int: Q, int: S,
                  array[int,int] of int: d, int: q0, set of int: F) =
    assert(index_set_1of2(d) = 1..Q /\ index_set_2of2(d) == 1..S,
        "regular: the transition function 'd' must be [1..Q,1..S]",
        let { int: l = min(index_set(x)),
	      int: u = max(index_set(x)),
              array[l-1..u,0..Q] of var bool: p } in
        % initialize the fail states
        forall(i in l-1..u) ( p[i,0] = false ) /\
        % initialize the start states
        forall(i in 1..Q diff {q0}) ( p[l-1,i] = false ) /\
        p[l-1,q0] = true /\
        forall(i in l..u-1, q in 1..Q) (
            p[i,q] <-> (exists(j in 1..Q) ( p[i-1,j] /\ d[j,x[i]] == q )
                        /\
                        p[i+1,d[q,x[i+1]]])
        ) /\
        % set the final states
        forall(i in 1..Q diff F) ( p[u,i] = false ) /\
        forall(i in F) (
            p[u,i] = exists(j in 1..Q) ( p[u-1,j] /\ d[j,x[u]] == i )
        )
    );

%-----------------------------------------------------------------------------%
% Requires that 'x[i] in t' for all 'i in s'
%-----------------------------------------------------------------------------%
predicate roots(array[int] of var int: x, var set of int: s,
                                          var set of int: t) =
    forall(i in ub(s)) ( i in s -> x[i] in t );

%-----------------------------------------------------------------------------%
% Requires that in each subsequence 'x[i], ..., x[i + l - 1]' the sum of the
% variables is between 'mn' and 'mx'.
%-----------------------------------------------------------------------------%
% XXX: this version is something like O((length(x) - l + 1) * l).  An
% O(length(x)) version may be possible.
predicate sequence(array[int] of var int: x, int: l, int: mn, int: mx) =
    let { int: lx = min(index_set(x)),
          int: ux = max(index_set(x)),
        } in
        forall (i in lx .. ux-l+1) ( 
            let { var int: sum_of_l = sum(j in i..i+l-1) ( x[j] ) } in
                mn <= sum_of_l /\ sum_of_l <= mx
        );

%-----------------------------------------------------------------------------%
% Requires that the multiset of values in 'x' are the same as the
% multiset of values in 'y' but 'y' is in sorted order.
%-----------------------------------------------------------------------------%
predicate sort(array[int] of var int: x, array[int] of var int: y);
%predicate sort(array[int] of var int: x, array[int] of var int: y) =
%    assert(card(index_set(x)) == card(index_set(y)),
%        "sort: x and y must be same sized arrays",
%        let { int: lx = min(index_set(x)),
%              int: ux = max(index_set(x)),
%              int: ly = min(index_set(y)),
%              int: uy = max(index_set(y)),
%              array[lx..ux] of var ly..uy: p } in
%        forall(i in index_set(x)) ( y[p[i]] == x[i] ) /\
%        all_different(p) /\
%        increasing(y)
%    );

%-----------------------------------------------------------------------------%
% Requires that the sum of cs[i1]...cs[iN] equals 's', where i1...iN are the
% elements of the 'i'th set in 'sets'.
%
% Nb: not called 'sum' as in the constraints catalog because 'sum' is a
% MiniZinc built-in function, and we cannot overload a name as both a
% function and a predicate.
%-----------------------------------------------------------------------------%
predicate sum_pred(var int: i, array[int] of set of int: sets,
                   array[int] of int: cs, var int: s) =
    s == sum(j in index_set(cs)) ( bool2int(j in sets[i]) * cs[j] );


%=============================================================================%
% The following predicates should probably not be used, but the Minizinc
% equivalent written directly in the model.  They are added for completeness.
%=============================================================================%

%-----------------------------------------------------------------------------%
% Requires that 'y' occurs in the array or set 'x'.
%-----------------------------------------------------------------------------%
predicate member(array[int] of var int: x, var int: y) =
    exists(i in index_set(x)) ( x[i] == y );

predicate member(array[int] of var float: x, var float: y) =
    exists(i in index_set(x)) ( x[i] == y );

predicate member(array[int] of var bool: x, var bool: y) =
    exists(i in index_set(x)) ( x[i] == y );

predicate member(array[int] of var set of int: x, var set of int: y) =
    exists(i in index_set(x)) ( x[i] == y );

predicate member(var set of int: x, var int: y) =
    y in x;

%-----------------------------------------------------------------------------%
% Requires that 'y' is the ith element of the array 'x'.
%-----------------------------------------------------------------------------%
predicate element(var int: i, array[int] of var int: x, var int: y) =
    y = x[i];

predicate element(var int: i, array[int] of var bool: x, var bool: y) =
    y = x[i];

predicate element(var int: i, array[int] of var float: x, var float: y) =
    y = x[i];

predicate element(var int: i, array[int] of var set of int: x,
                  var set of int: y) =
    y = x[i];

