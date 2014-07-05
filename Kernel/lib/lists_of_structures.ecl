% ----------------------------------------------------------------------
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
% The Original Code is  The 'lists_of_structures' library for ECLiPSe.
% The Initial Developer of the Original Code is  Joachim Schimpf.
% Portions created by the Initial Developer are
% Copyright (C) 2013.  All Rights Reserved.
% 
% END LICENSE BLOCK
% ----------------------------------------------------------------------

:- module(lists_of_structures).

:- comment(categories, ["Data Structures"]).
:- comment(summary, "Operations on lists of Structures").
:- comment(author, "Joachim Schimpf").
:- comment(copyright, "Joachim Schimpf, 2013").
:- comment(date, "$Date: 2014/07/05 15:52:16 $").
:- comment(see_also, [
    	hash:list_to_hash/4,
	sort/4,
	number_sort/4,
	merge/5,
	number_merge/5,
	struct/1
    ]).
:- comment(desc, html("<p>
    This library contains predicates that operate on lists of structures,
    in particular structures where one argument can be considered a `key'.
    Such lists are very common, and often occur in sorted form.
    </p><p>
    The ECLiPSe kernel and other libraries support such lists as well, e.g.
<UL>
    <LI>the sorting built-ins (sort/4, merge/5, number_sort/4, number_merge/5)
    <LI>hash:list_to_hash/4 for turning such lists into hash tables
</UL>
    </p><p>
    If you have declared your structures using the
    :- local(struct(...)) declaration, then you can use field names to
    identify the key arguments in all these predicates, e.g.
<PRE>
    :- local struct(emp(name,age,salary)).
    ?- Emps = [emp{name:joe,salary:100}, emp{name:bob,salary:200}],
       sort(salary of emp, >=, Emps, Descending),
       args(name of emp, Descending, Names).
    Emps = [emp(joe, _, 100), emp(bob, _, 200)]
    Descending = [emp(bob, _, 200), emp(joe, _, 100)]
    Names = [bob, joe]
    yes
</PRE>
</p>")).


:- comment(args/3, [
    summary:"Extract arguments from a list of structures",
    amode:(args(+,+,-) is det),
    args:[
	"Key":"Key argument position (non-negative integer, or list of those)",
        "Structs":"List of structures",
        "Args":"Variable, or list of arguments"],
    see_also:[arg/3],
    desc:html("<p>
    Structs is a list of structures, and Args is a list of the Key'th
    arguments of these structures.
    </p><p>
    This is equivalent to either
<pre>
    maplist(arg(Key), Structs, Args)
</pre>
    or
<pre>
    ( foreach(S,Structs), foreach(A,Args), param(Key) do arg(Key,S,A) )
</pre>
    </p>"),
    exceptions:[4:"Structs is insufficiently instantiated",
	6:"Some structure in Structs does not have a Key'th argument"],
    eg:"
    ?- args(2, [f(3,a),f(2,b),f(1,c)], Args).
    Args = [a,b,c]
    yes

    % extract keys from pairs
    ?- args(1, [a-1,a-2,b-2,c-2,c-5], Keys).
    Keys = [a,a,b,c,c]
    yes

    % extract values from pairs
    ?- args(2, [a-1,a-2,b-2,c-2,c-5], Values).
    Values = [1,2,2,2,5]
    yes
"]).

:- export args/3.
args(_Key, [], []).
args(Key, [S|Ss], [A|As]) :-
	arg(Key, S, A),
	args(Key, Ss, As).
	

:- comment(terms_functor/3, [
    summary:"All list elements have the given functor or atomic value",
    amode:(terms_functor(+,-,-) is semidet),
    amode:(terms_functor(-,+,+) is multi),
    fail_if:"Not all list elements have the same functor",
    args:["Structs":"List of terms, or variable",
        "Name":"Atomic or variable",
        "Arity":"Integer or variable"],
    see_also:[functor/3],
    desc:html("<p>
    Structs is a list of terms which all have the same functor Name/Arity.
    Operationally, this can be used to either check the functors of existing
    structures in the list, or to bind uninstantiated list elements to
    new structures.
    </p><p>
    This is equivalent (modulo nondeterminism) to
<pre>
    ( foreach(S,Structs), param(Name,Arity) do functor(S,Name,Arity) )
</pre>
    </p><p>
    Note that, like in the underlying functor/3 predicate, Name can be
    any atomic term (including number and string) if Arity is zero.
    </p>"),
    exceptions:[4:"Arguments are insufficiently instantiated",
	5:"Name is not atomic",
	5:"Arity is not an integer",
	5:"Arity is greater than 0 and Name is not an atom",
	6:"Some structure in Structs does not have a Key'th argument"],
    eg:"
    % fill a list with structure skeletons
    ?- length(Ss,3), terms_functor(Ss,f,2).
    Ss = [f(_275,_276), f(_278,_279), f(_281,_282)]
    Yes (0.00s cpu)

    % check whether all elements have same toplevel functor
    ?- terms_functor([f(a),f(b)], F, A).
    F = f
    A = 1
    Yes (0.00s cpu)

    % fill a list with an atomic term (arity 0)
    ?- length(Ss,3), terms_functor(Ss,99,0).
    Ss = [99, 99, 99]
    Yes (0.00s cpu)

"]).

:- export terms_functor/3.
terms_functor([], _, _).
terms_functor([S|Ss], F, N) :-
	functor(S, F, N),
	terms_functor(Ss, F, N).
	

:- comment(inserta/4, [
    summary:"Insert Struct into a sorted list",
    amode:(inserta(+,+,+,-) is det),
    args:[
	"Key":"Key argument position (non-negative integer, or list of those)",
	"Struct":"Structure",
	"Old":"Ordered list of structures",
	"New":"Ordered list of structures, or variable"],
    see_also:[merge/5,sort/4],
    desc:html("<p>
    Inserts the compound term Struct into the ordered list Old, giving New.
    The lists are both in ascending order (according to their elements' Key'th
    argument), and may contain duplicates.  If Old already contains elements
    with the same key as Struct, then Struct is inserted before those.
    </p><p>
    The list New is always one element longer than the list Old.
    </p>"),
    exceptions:[4:"Arguments are insufficiently instantiated",
	5:"Some argument or its components are of the wrong type",
	6:"Some structure does not have a Key'th argument"],
    eg:"
    ?- inserta(1, f(2,new), [f(1,a),f(2,b),f(2,c),f(3,d)], New).
    New = [f(1,a), f(2,new), f(2,b), f(2,c), f(3,d)]
    Yes (0.00s cpu)

"]).

:- export inserta/4.
inserta(Key, Term, Structs0, Structs) :-
	merge(Key, =<, [Term], Structs0, Structs).


:- comment(insertz/4, [
    summary:"Insert Struct into a sorted list",
    amode:(insertz(+,+,+,-) is det),
    args:[
	"Key":"Key argument position (non-negative integer, or list of those)",
	"Struct":"Structure",
	"Old":"Ordered list of structures",
	"New":"Ordered list of structures, or variable"],
    see_also:[merge/5,sort/4],
    desc:html("<p>
    Inserts the compound term Struct into the ordered list Old, giving New.
    The lists are both in ascending order (according to their elements' Key'th
    argument), and may contain duplicates.  If Old already contains elements
    with the same key as Struct, then Struct is inserted after those.
    </p><p>
    The list New is always one element longer than the list Old.
    </p>"),
    exceptions:[4:"Arguments are insufficiently instantiated",
	5:"Some argument or its components are of the wrong type",
	6:"Some structure does not have a Key'th argument"],
    eg:"
    ?- insertz(1, f(2,new), [f(1,a),f(2,b),f(2,c),f(3,d)], New).
    New = [f(1,a), f(2,b), f(2,c), f(2,new), f(3,d)]
    Yes (0.00s cpu)
"]).

:- export insertz/4.
insertz(Key, Term, Structs0, Structs) :-
	merge(Key, =<, Structs0, [Term], Structs).


:- comment(update/4, [
    summary:"Incorporate Struct into a duplicate-free sorted list",
    amode:(update(+,+,+,-) is det),
    args:[
	"Key":"Key argument position (non-negative integer, or list of those)",
	"Struct":"Structure",
	"Old":"Strictly ordered list of structures",
	"New":"Strictly ordered list of structures, or variable"],
    see_also:[merge/5,sort/4],
    desc:html("<p>
    Inserts the compound term Struct into the ordered list Old, giving New.
    The lists are both in strictly ascending order (according to their
    elements' Key'th argument), i.e. without duplicates.  If Old already
    contains an element with the same key as Struct, then Struct replaces
    this existing element.  Otherwise, Struct is inserted at the correct
    position to maintain the list order.
    </p><p>
    The list New is either of the same length as Old, or one longer.
    </p>"),
    exceptions:[4:"Arguments are insufficiently instantiated",
	5:"Some argument or its components are of the wrong type",
	6:"Some structure does not have a Key'th argument"],
    eg:"
    ?- update(1, f(2,b), [f(1,a),f(3,c)], New).
    New = [f(1,a), f(2,b), f(3,c)]
    Yes (0.00s cpu)

    ?- update(1, f(2,new), [f(1,a),f(2,b),f(3,c)], New).
    New = [f(1,a), f(2,new), f(3,c)]
    Yes (0.00s cpu)
"]).

:- export update/4.
update(Key, Term, Structs0, Structs) :-
	merge(Key, <, [Term], Structs0, Structs).


:- comment(partition_by_key/5, [
    summary:"Partition the elements of a list according to a key value",
    amode:(partition_by_key(+,+,+,-,-) is det),
    args:[
	"Key":"Key argument position (non-negative integer, or list of those)",
	"Value":"A term",
	"All":"List of structures",
	"Matches":"Variable, or list of structures",
	"Others":"Variable, or list of structures"],
    see_also:[prefix_by_key/5],
    desc:html("<p>
    The list All is split into two sublists Matches and Others, such that
    Matches contains the elements whose Key argument is identical (in the
    sense of ==/2) to Value, and Others contains the remaining elements.  The
    element order in the sublists corresponds to the order in the original list.
    </p>"),
    exceptions:[4:"Arguments are insufficiently instantiated",
	5:"Some argument or its components are of the wrong type",
	6:"Some structure does not have a Key'th argument"],
    eg:"
    ?- partition_by_key(1, b, [f(a,1),f(b,2),f(c,3),f(b,4)], Bs, Ns).
    Bs = [f(b,2), f(b,4)]
    Ns = [f(a,1), f(c,3)]
    Yes (0.00s cpu)
"]).

:- export partition_by_key/5.
partition_by_key(_Key, _, [], [], []).
partition_by_key(Key, Val, [S|Ss], YYs, NNs) :-
	arg(Key, S, Arg),
	( Arg==Val ->
	    YYs = [S|Ys], NNs = Ns
	;
	    YYs = Ys, NNs = [S|Ns]
	),
	partition_by_key(Key, Val, Ss, Ys, Ns).


:- comment(prefix_by_key/5, [
    summary:"Get the maximum prefix of a list whose elements match a key value",
    amode:(prefix_by_key(+,+,+,-,-) is det),
    args:[
	"Key":"Key argument position (non-negative integer, or list of those)",
	"Value":"A term",
	"All":"List of structures",
	"Prefix":"Variable, or list of structures",
	"Rest":"Variable, or list of structures"],
    see_also:[partition_by_key/5],
    desc:html("<p>
    The list All is split into two sublists Prefix and Rest, such that
    Prefix contains all the leading elements of All whose Key argument is
    identical to Value, and Rest contains the remainder of the list.
    Concatenating the Prefix and Rest will yield the original list All.
    </p>"),
    exceptions:[4:"Arguments are insufficiently instantiated",
	5:"Some argument or its components are of the wrong type",
	6:"Some structure does not have a Key'th argument"],
    eg:"
    ?- prefix_by_key(1, a, [f(a,1),f(a,2),f(c,3),f(b,4),f(a,5)], Prefix, Rest).
    Prefix = [f(a,1), f(a,2)]
    Rest = [f(c,3), f(b,4), f(a,5)]
    Yes (0.00s cpu)

    ?- prefix_by_key(1, b, [f(a,1),f(a,2),f(c,3),f(b,4),f(a,5)], Prefix, Rest).
    Prefix = []
    Rest = [f(a,1), f(a,2), f(c,3), f(b,4), f(a,5)]
    Yes (0.00s cpu)

"]).

:- export prefix_by_key/5.
prefix_by_key(_Key, _Val, [], [], []).
prefix_by_key(Key, Val, SSs, YYs, Ns) :-
	SSs= [S|Ss],
	arg(Key, S, Arg),
	( Arg==Val ->
	    YYs = [S|Ys],
	    prefix_by_key(Key, Val, Ss, Ys, Ns)
	;
	    Ns = SSs, YYs = []
	).


:- comment(group_by_key/3, [
    summary:"Partition a list into sublists by key",
    amode:(group_by_key(+,+,-) is det),
    args:[
	"Key":"Key argument position (non-negative integer, or list of those)",
	"Structs":"List of structures",
	"Grouped":"Variable, or list of list of structures"],
    see_also:[group_with_key/3,sort/4,merge/5],
    desc:html("<p>
    The list Structs is grouped into maximal sublists of consecutive
    elements with identical keys.  Concatenating all these sublists will
    yield the original list.  If the original list was ordered according
    to the Key'th argument, then the sublists represent a partitioning of
    the original elements according to their different key values.
    </p>"),
    exceptions:[4:"Arguments are insufficiently instantiated",
	5:"Some argument or its components are of the wrong type",
	6:"Some structure does not have a Key'th argument"],
    eg:"
    ?- group_by_key(1, [f(a,1),f(a,2),f(b,2),f(c,2),f(c,5)], Groups).
    Groups = [[f(a,1),f(a,2)], [f(b,2)], [f(c,2),f(c,5)]]
    Yes (0.00s cpu)

    ?- group_by_key(2, [f(a,1),f(a,2),f(b,2),f(c,2),f(c,5)], Groups).
    Groups = [[f(a,1)], [f(a,2),f(b,2),f(c,2)], [f(c,5)]]
    Yes (0.00s cpu)

    ?- group_by_key(1, [a-1,a-2,b-2,c-2,c-5], Groups).
    Groups = [[a-1,a-2], [b-2], [c-2,c-5]]
    Yes (0.00s cpu)

"]).

:- export group_by_key/3.
group_by_key(_Pos, [], []).
group_by_key(Pos, [KV|List], [[KV|KVs]|GroupedList]) :-
	arg(Pos, KV, K),
        group_by_key(Pos, List, K, KVs, GroupedList).

    group_by_key(_Pos, [], _, [], []).
    group_by_key(Pos, [KV|List], K, [KV|KVs], GroupedList) :-
	arg(Pos, KV, K1), K == K1, !,
        group_by_key(Pos, List, K, KVs, GroupedList).
    group_by_key(Pos, [KV|List], _K, [], [[KV|KVs]|GroupedList]) :-
	arg(Pos, KV, K),
        group_by_key(Pos, List, K, KVs, GroupedList).


:- comment(group_with_key/3, [
    summary:"Partition a list into sublists by key",
    amode:(group_with_key(+,+,-) is det),
    args:[
	"Key":"Key argument position (non-negative integer, or list of those)",
	"Structs":"List of structures",
	"Grouped":"Variable, or list of KeyVal-SubList structures"],
    see_also:[group_by_key/3,sort/4,merge/5],
    desc:html("<p>
    The list Structs is grouped into maximal sublists of consecutive elements
    with identical keys, such that concatenating all these sublists would
    yield the original list.  If the original list was ordered according
    to the Key'th argument, then the sublists represent a partitioning of
    the original elements according to their different key values.
    </p><p>
    The output argument Grouped is bound to a list of KeyVal-SubList pairs,
    where KeyVal is the value of the Key'th argument that all the structures
    in SubList have in common.
    </p>"),
    exceptions:[4:"Arguments are insufficiently instantiated",
	5:"Some argument or its components are of the wrong type",
	6:"Some structure does not have a Key'th argument"],
    eg:"
    ?- group_with_key(1, [f(a,1), f(a,2), f(b,2), f(c,2), f(c,5)], Groups).
    Groups = [a - [f(a,1), f(a,2)], b - [f(b,2)], c - [f(c,2),f(c,5)]]
    Yes (0.00s cpu)

    ?- group_with_key(2, [f(a,1), f(a,2), f(b,2), f(c,2), f(c,5)], Groups).
    Groups = [1 - [f(a,1)], 2 - [f(a,2),f(b,2),f(c,2)], 5 - [f(c,5)]]
    Yes (0.00s cpu)
"]).


:- export group_with_key/3.
group_with_key(_Pos, [], []).
group_with_key(Pos, [KV|List], [K-[KV|KVs]|GroupedList]) :-
	arg(Pos, KV, K),
        group_with_key(Pos, List, K, KVs, GroupedList).

    group_with_key(_Pos, [], _, [], []).
    group_with_key(Pos, [KV|List], K, [KV|KVs], GroupedList) :-
	arg(Pos, KV, K1), K == K1, !,
        group_with_key(Pos, List, K, KVs, GroupedList).
    group_with_key(Pos, [KV|List], _K, [], [K-[KV|KVs]|GroupedList]) :-
	arg(Pos, KV, K),
        group_with_key(Pos, List, K, KVs, GroupedList).


:- comment(lists_structs/2, [
    summary:"Mapping between a structure of lists and a list of structures",
    amode:(lists_structs(+,-) is det),
    amode:(lists_structs(-,+) is det),
    args:[
	"Lists":"Structure with list arguments, or variable",
	"Structs":"List of structures, or variable"],
    see_also:[terms_functor/3,args/3],
    desc:html("<p>
    Maps a single structure with functor F/N whose arguments are all lists
    of length M into a single list of length M of F/N structures, and vice
    versa.  The arguments of the K'th structure on the right hand side
    correspond to the K'th list elements on the left hand side.
    </p><p>
    The main purpose of this predicate is to build a list of structures from
    several lists of arguments.  The simplest example is building a Key-Value
    pair list from corresponding lists of Keys and Values.
    </p><p>
    The reverse direction is used to extract multiple argument lists from a list
    of structures.  The simplest example is getting the Keys and Values from
    a Key-Value pair list.  However, if only a single argument list is wanted,
    it is more appropriate to use args/3.
    </p>"),
    exceptions:[4:"Arguments are insufficiently instantiated",
	5:"Some argument or its components are of the wrong type",
	6:"Some structure does not have a Key'th argument"],
    eg:"
    ?- lists_structs(f([a,b,c],[1,2,3],[x,y,z]), Structs).
    Structs = [f(a,1,x), f(b,2,y), f(c,3,z)]
    Yes (0.00s cpu)

    ?- lists_structs(Lists, [f(a,1,x), f(b,2,y), f(c,3,z)]).
    Lists = f([a,b,c], [1,2,3], [x,y,z])
    Yes (0.00s cpu)

    ?- Keys=[1,2,3], Vals=[a,b,c], lists_structs(Keys-Vals, Pairs).
    Keys = [1, 2, 3]
    Vals = [a, b, c]
    Pairs = [1-a, 2-b, 3-c]
    Yes (0.00s cpu)

    ?- lists_structs(Lists, [1-a, 2-b, 3-c]).
    Lists = [1,2,3] - [a,b,c]
    Yes (0.00s cpu)
"]).

:- export lists_structs/2.
lists_structs(SoL, Structs) :-
	var(SoL),
	(
	    foreach(Struct,Structs),
	    fromto(SoL,SoL1,SoL2,SoE)
	do
	    functor(Struct, F, N),
	    functor(SoL1, F, N),
	    functor(SoL2, F, N),
	    ( for(I,1,N), param(SoL1,SoL2,Struct) do
		arg(I,SoL1,[A|As]),
		arg(I,Struct,A),
		arg(I,SoL2,As)
	    )
	),
	( foreacharg([],SoE) do true ).
lists_structs(SoL, Structs) :-
	nonvar(SoL),
	functor(SoL, F, N),
	functor(SoE, F, N),
	( foreacharg([],SoE) do true ),
	(
	    foreach(Struct,Structs),
	    fromto(SoL,SoL1,SoL2,SoE)
	do
	    functor(SoL1, F, N),
	    functor(SoL2, F, N),
	    functor(Struct, F, N),
	    ( for(I,1,N), param(SoL1,SoL2,Struct) do
		arg(I,SoL1,[A|As]),
		arg(I,Struct,A),
		arg(I,SoL2,As)
	    )
	).


%----------------------------------------------------------------------
end_of_file.
%----------------------------------------------------------------------

% Specific to Key-Val pairs
% 
% structs_keyed(Key,Ss,Ps) :-
% 	args(Key, Ss, Ks),
% 	lists_structs(Ks-Ss,Ps).
% 
:- export structs_keyed/3.
structs_keyed(_Key, [], []).
structs_keyed(Key, [S|Ss], [A-S|ASs]) :-
	arg(Key, S, A),
	structs_keyed(Key, Ss, ASs).
	

/*

O'Keefe suggestions:

pairs_keys_values(Pairs, Keys, Values) both ways
pairs_keys(Pairs, Keys) both ways
pairs_values(Pairs, Values) both ways

join_by_key/2 = group_same_key_values/2.  SWI: group_by_key/2


Quintus Prolog, library(clumps) -- 1989 -- has several related predicates.

% clumps(+Items, ?Clumps)
% is true when Clumps is a list of lists such that
% (a) append(Clumps, Items)
% (b) for each Clump in Clumps, all the elements of Clump are identical
% (==). Items must be a proper list of terms for which sorting would
% have been sound. In fact, it usually is the result of sorting.

For example,

clumps([m, i, s,s, i, s,s, i, p,p, i],
[[m],[i],[s,s],[i],[s,s],[i],[p,p], [i]])

% keyclumps(+Pairs, ?Clumps)
% is true when Pairs is a list of pairs and Clumps a list of lists
% such that
% (a) append(Clumps, Pairs)
% (b) for each Clump in Clumps, all of the Key-Value pairs in Clump
% have identical (==) Keys.
% Pairs must be a proper list of pairs for which keysorting would have
% been sound. In fact, it usually is the result of keysorting.

This isn't quite what you want.

% clumped(+Items, ?Counts)
% is true when Counts is a list of Item-Count pairs such that
% if clumps(Items, Clumps), then each Item-Count pair in Counts
% corresponds to an element [Item/*1*/,...,Item/*Count*/] of Clumps.
% Items must be a proper list of terms for which sorting would have
% been sound. In fact, it usually is the result of sorting.

For example,
clumped([m, i, s,s,i, s,s,i, p,p,i],
[m-1,i-1,s-2,i-1,s-2,i-1,p-2,i-1])

% keyclumped(+Pairs, ?Groups)
% is true when Pairs is a list of Key-Item pairs and
% Groups is a list of Key-Items pairs such that
% if keyclumps(Pairs, Clumps), then for each K-[I1,...,In] pair in
% Groups there is a [K-I1,...,K-In] clump in Clumps.
% Pairs must be a proper list of pairs for which keysorting would have
% been sound. In fact, it usually is the result of keysorting.

This, I think, is the one you want.

I dislike the name join_by_key/2 (because it is NOT a join) about as
much as I dislike the name keyclumped (because it really isn't obvious
what it means; back in 1989 I still had a lot to learn about naming).

The obvious other such predicate is

transpose(Pairs, Transposed) :-
'flip for transpose'(Pairs, Flipped),
keysort(Flipped, Transposed).

'flip for transpose'([], []).
'flip for transpose'([Key-Val|Pairs], [Val-Key|Flipped]) :-
'flip for transpose'(Pairs, Flipped).



*/
