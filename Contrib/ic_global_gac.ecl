%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
% The Original Code is The Generalized Arc Consistent all-different global 
% constraint.    
% The Initial Developer of the Original Code is  Helmut Simonis
% Portions created by the Initial Developer are  Copyright (C)2008.
% All Rights Reserved.
% 
% Contributor(s): Helmut Simonis, 4C, University College Cork
% 
% END LICENSE BLOCK
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:-module(ic_global_gac).
:- comment(categories, ["Constraints","Algorithms"]).
:-comment(summary,"Library of global constraints which achieve"
                        " generalized arc consistency").
:-comment(desc,"This library is intended for global constraints for"
               " which GAC (generalized arc consistency, also called hyper arc"
               " consistency, or domain consistency) is maintained."
               " The first example is a version of the alldifferent"
               " constraint which performs more pruning than the bound"
               " consistent version in the ic_global library.").
:-comment(author,"H. Simonis, 4C, University College Cork").
:-comment(copyright,"2008, H. Simonis, 4C, University College Cork").
:-comment(status,prototype).
:-comment(date,"2008").

:-comment(alldifferent/1,[summary:"GAC version of alldifferent",
                          amode:alldifferent(+),
                          args:["L":"List of integers or domain"
                                    " variables, or a collection a la collection_to_list/2"],
                          desc:html("This predicate implements a GAC"
                                    " (generalized arc consistency)"
                                    " version of the alldifferent"
                                    " constraint. It uses the"
                                    " classical bitpartite matching"
                                    " implementation using the"
                                    " graph_algorithms library. This"
                                    " version often removes more"
                                    " values than the bound consistent"
                                    " alldifferent in the ic_global"
                                    " library, or the forward checking"
                                    " variant in the ic library, but"
                                    " may spend much more time doing"
                                    " this."),
                          fail_if:"fails if there is no bipartite matching"
                                  " between all variables and the"
                                  " possible values",
                          see_also:[matching/2,
                                    ic:alldifferent/1,
                                    ic_global:alldifferent/1]
                         ]).

:-comment(matching/2,[summary:"Get a matching between a list of"
                             " domain variables and their possible"
                              " values",
                      amode:matching(+,-),
                      args:["L":"A list of integers or domain"
                                " variables, or a collection a la collection_to_list/2",
                            "K":"A free variable, will be bound to a"
                                " list of integers"
                           ],
                      fail_if:"The predicate fails if no matching"
                              " exists",
                      desc:html("This predicate can be used to get the"
                                " matching into the user program."
                                " Sometimes it is a good starting"
                                " point for heuristics. It only gets"
                                " the current matching and does not do"
                                " any propagation, the matching is not"
                                " updated when values are removed, the"
                                " predicate must be called again in"
                                " the user program if this is"
                                " required"),
                      see_also:[alldifferent/1]
                     ]).

:-export(alldifferent/1).
:-export(matching/2).

%----------------------------------------------------------------------
% Output transformations
%----------------------------------------------------------------------

:-pragma(nodebug).

:-comment(tr_global_gac_out/2,hidden).
:- export tr_global_gac_out/2.

:- export portray(update_alldifferent/4, tr_global_gac_out/2, [goal]).

tr_global_gac_out(update_alldifferent(_, _,List,_), alldifferent(List)).


:-lib(ic).
:-lib(ic_kernel).
:-lib(graph_algorithms).
:-lib(hash).
:-lib(lists).

:-local struct(mapping(n,
                       value2node,
                       var_array)).

:-local struct(remember(matching,
                        solved)).

% this is the top-level entry point
% it sets up a delay on each variable, and then calls the checker
alldifferent(Vars):-
        collection_to_list(Vars,L),
        init_remember(Remember),
        check_alldifferent(L,Remember),
        shrink(L,L1),
        (L1 = [] ->
            true
        ; L1 = [_] ->
            true
        ;
            suspend(update_alldifferent(L1,Remember,L,Susp),
                    4,[L1->ic:min,
                       L1->ic:max,
                       L1->ic:hole],Susp)
        ).

shrink(L,L1):-
        (foreach(X,L),
         fromto(L1,A1,A,[]) do
            (var(X) ->
                A1 = [X|A]
            ;
                A1 = A
            )
        ).

% this is called whenever a variable changes
% it removes the constraint once it is solved
% the third argument is passed to allow output of original argument
:-demon(update_alldifferent/4).
update_alldifferent(L,Remember,_OrigArg,Susp):-  
        check_alldifferent(L,Remember),
        (is_solved(Remember) ->
            kill_suspension(Susp)
        ;
            true
        ).

% this is the actual constraint check code
check_alldifferent(_L,Remember):-
        is_solved(Remember),
        !.
check_alldifferent(L,Remember):-
        length(L,N),
%        writeln(check),
        % create a list of the variable nodes
        % create a list of edges
        create_nodes_and_edges(L,N,Mapping,VarNodes,Edges,LastNode,Ground),
        (Ground = 1 ->
            mark_solved(Remember),
%            writeln(ground),
            true
        ;
            N1 is N+1,
        % build a list of the value nodes in the graph
        value_nodes(N1,LastNode,ValueNodes),
        % test if there are at least as many values as variables
        % if not the constraint must fail here
        LastNode - N >= N,
        % create graph data structure
        make_graph(LastNode,Edges,Graph),
            remembered_matching(Mapping,L,Remember,OldMatching,
                                SizeOld),
%            writeln(old(SizeOld,OldMatching)),
        % call the matching algorithm, if old matching does not work
            (SizeOld = N ->
                MaximalM = OldMatching
            ;
                maximum_matching_hopcroft_karp(Graph, VarNodes,
                                               ValueNodes, 
                                               MaximalM),
           
                length(MaximalM,SizeM),
%                writeln(matching(SizeM,MaximalM)),
        % check that every variable is matched, otherwise fail
                SizeM = N,
                remember_matching(N,MaximalM,Remember,Mapping)
            ),
        % invert the edges not in the matching
        invert_edges(Edges,MaximalM,InvertedEdges,Edges1),
        % build a new graph with matching and inverted edges
        make_graph(LastNode,Edges1,Graph1),
        % search for strongly connected components in new graph
        strong_components(Graph1, StrongComponents),
%        writeln(strong(StrongComponents)),
        % extract edges between nodes in different components
        mark_components(StrongComponents,LastNode,InvertedEdges,
                        NotMarked),
%        writeln(not_marked(NotMarked)),
        % find value nodes not in matching, they are possible starts
        % of alternating paths; for permutations there should be none
        unmatched_value_nodes(N1,LastNode,MaximalM,MFree),
        % MFree is a list of unmatched value nodes
        % find edges on alternate paths starting in unmatched values
        % and mark them
%        writeln(mfree(MFree)),
        alternate_paths(MFree,Graph1,NotMarked,FinalNotMarked),
        % remove the values which correspond to unmarked edges in the
        % graph; note that values are the sources of edges
        remove_unmarked_edges(FinalNotMarked,Mapping)

        ),

        % start the propagation after all value removals have been done
        wake.



create_nodes_and_edges(L,N,Mapping,VarNodes,Edges,LastNode,Ground):-
        mapping_create(N,Mapping),
        hash_create(UniqueValues),
        (foreach(X,L),
         foreach(VarNode,VarNodes),
         fromto([],A,A1,Edges),
         fromto(1,B,B1,Ground),
         fromto(N,NextV,NextV1,LastNode),
         count(VarNode,1,_),
         param(Mapping,UniqueValues) do
            % create a node for each variable
            node_map(Mapping,VarNode,X),
            (integer(X) ->
                B1 = B,
                (hash_find(UniqueValues,X,_) ->
                    fail
                ;
                    hash_add(UniqueValues,X,X)
                )
            ;
                B1  = 0
            ),
            get_domain_as_list(X,Dom),
            % create an edge for every value in the domain of a variable
            (foreach(V,Dom),
             fromto(NextV,NextValue,NextValue1,NextV1),
             fromto(A,B,[e(VarNode,ValueNode,V)|B],A1),
             param(Mapping,VarNode) do
                % create a node for the value, if not already there
                value_insert(Mapping,V,ValueNode,NextValue,NextValue1)
            )
        ).

value_nodes(N1,LastNode,ValueNodes):-
        (for(J,N1,LastNode),
         foreach(J,ValueNodes) do
            true
        ).

unmatched_value_nodes(N1,LastNode,MaximalM,MFree):-
        dim(MNodes,[LastNode]),
        (foreach(e(_From,To,_),MaximalM),
         param(MNodes) do
            subscript(MNodes,[To],1)
        ),
        (for(J,N1,LastNode),
         fromto([],A,A1,MFree),
         param(MNodes) do
            subscript(MNodes,[J],V),
            (var(V) ->
                A1 = [J|A]
            ;
                A1 = A
            )
        ).

% special case for no unmatched value nodes = permutation
alternate_paths([],_Graph,NotMarked,NotMarked):-
        !.
alternate_paths(MFree,Graph,NotMarked,FinalNotMarked):-
        % hash table with nodes as key, dummy values
        hash_create(CheckedNodes),
        % hash table with Edges as key, dummy values 
        hash_create(MarkedEdges),
        scan_edges(MFree,CheckedNodes,Graph,MarkedEdges),
        reduce_unmarked_edges(NotMarked,MarkedEdges,FinalNotMarked).

scan_edges([],_CheckedNodes,_Graph,_MarkedEdges).
scan_edges([H|T],CheckedNodes,Graph,MarkedEdges):-
        hash_create(NewNodes),
        scan([H|T],NewNodes,CheckedNodes,Graph,MarkedEdges),
        hash_list(NewNodes,NewNodesList,_),
        scan_edges(NewNodesList,CheckedNodes,Graph,MarkedEdges).

scan([],_NewNodes,_CheckedNodes,_Graph,_MarkedEdges).
scan([H|T],NewNodes,CheckedNodes,Graph,MarkedEdges):-
        graph_get_adjacent_edges(Graph,H,Edges),
        scan_outbound(Edges,NewNodes,CheckedNodes,MarkedEdges),
        hash_add(CheckedNodes,H,H), 
        scan(T,NewNodes,CheckedNodes,Graph,MarkedEdges).

scan_outbound([],_NewNodes,_CheckedNodes,_MarkedEdges).
scan_outbound([Edge|Edge1],NewNodes,CheckedNodes,MarkedEdges):-
        consider_edge(Edge,NewNodes,CheckedNodes,MarkedEdges),
        scan_outbound(Edge1,NewNodes,CheckedNodes,MarkedEdges).

consider_edge(Edge,NewNodes,CheckedNodes,MarkedEdges):-
        Edge = e(_From,To,_Info),
        % if node has been checked before or
        % node is already on ToDo list, do not add
        ((hash_find(CheckedNodes,To,_);hash_find(NewNodes,To,_)) ->
            true
        ;
            hash_add(NewNodes,To,To)
        ),
        hash_add(MarkedEdges,Edge,0).

% if edge is in hash table, then it is marked, and should not be removed
reduce_unmarked_edges(L,Hash,K):-
        (foreach(Edge,L),
         fromto(K,A1,A,[]),
         param(Hash) do
            (hash_find(Hash,Edge,_) ->
                A1 = A
            ;
                A1 = [Edge|A]
            )
        ).

% remove values from variables corresponding to unmarked edges
remove_unmarked_edges(FinalNotMarked,Mapping):-
        call_priority((foreach(e(_ValueN,VarN,Value),FinalNotMarked),
         param(Mapping) do
            node_map(Mapping,VarN,Var),
%            writeln(rem(VarN,Var,ValueN,Value)),
            exclude(Var,Value)
        ),2).
/*

Dealing with var/value <-> node mapping
vars are numbered 1..n
values are numbered n+1..lastnode

this allows for arbitrary values in the domains
we can not use nodenames for this as we deal with variables
*/

mapping_create(N,mapping{n:N,
                         value2node:Value2Node,
                         var_array:VarArray}):-
        hash_create(Value2Node),
        N1 is N+1,
        dim(VarArray,[N1]).

node_map(mapping{var_array:VarArray},Node,Var):-
        subscript(VarArray,[Node],Var).

% find the node for a given value
value_insert(mapping{value2node:Hash},V,ValueNode,Next,Next):-
        hash_find(Hash,V,ValueNode),
        !.
value_insert(mapping{value2node:V2N},V,Next1,Next,Next1):-
        Next1 is Next+1,
        hash_add(V2N,V,Next1).

value_lookup(mapping{value2node:Hash},V,ValueNode):-
        hash_find(Hash,V,ValueNode),
        !.


matching(L,K):-
        length(L,N),
        % create a list of the variable nodes
        % create a list of edges
        create_nodes_and_edges(L,N,_Mapping,VarNodes,Edges,LastNode,_),
        N1 is N+1,
        % build a list of the value nodes in the graph
        value_nodes(N1,LastNode,ValueNodes),
        % test if there are at least as many values as variables
        % if not the constraint must fail here
        LastNode - N >= N,
        % create graph data structure
        make_graph(LastNode,Edges,Graph),
        % call the matching algorithm
        maximum_matching_hopcroft_karp(Graph, VarNodes,ValueNodes, MaximalM),
        length(MaximalM,SizeM),
        % check that every variable is matched, otherwise fail
        SizeM = N,
        sort(1,=<,MaximalM,Sorted),
        (foreach(e(_From,_To,Value),Sorted),
         foreach(Value,K) do
            true
        ).

/*
invert the edges in a list which are not in matching
uses hash table to avoid list lookup
also returns all edges in new graph
*/
invert_edges(Edges,MaximalM,InvertedEdges,AllEdges):-
        hash_create(Match),
        (foreach(Edge,MaximalM),
         param(Match) do
            hash_add(Match,Edge,1)
        ),
        (foreach(Edge,Edges),
         fromto([],A,A1,InvertedEdges),
         fromto(MaximalM,B,B1,AllEdges),
         param(Match) do
            direct_edge(Edge,Match,A,A1,B,B1)
        ).

% ignore edge if it is in the matching
% otherwise invert its direction and put into accumulator
direct_edge(Edge,Match,A,A,B,B):-
        hash_find(Match,Edge,_),
        !.
direct_edge(e(From,To,W),_Match,A,[e(To,From,W)|A],
            B,[e(To,From,W)|B]).

/*
extract the list of edges whose ends are in different SCC
StrongComponents is a list of list of node indices
*/
mark_components(StrongComponents,NrNodes,InvertedEdges,NotMarked):-
        dim(Components,[NrNodes]),
        (foreach(Set,StrongComponents),
         count(J,1,_),
         param(Components) do
            (foreach(Sx,Set),
             param(J,Components) do
                subscript(Components,[Sx],J)
            )
        ),
        (foreach(Edge,InvertedEdges),
         fromto([],A,A1,NotMarked),
         param(Components) do
            marked(Edge,Components,A,A1)
        ).

% if ends are in same SCC, ignore the edge
% otherwise add to accumulator
marked(e(Value,Var,_W),Components,A,A):-
        subscript(Components,[Value],X),
        subscript(Components,[Var],X),
        !.
marked(Edge,_Components,A,[Edge|A]).

init_remember(remember{}).

remembered_matching(_Mapping,_L,remember{matching:Array},[],0):-
        var(Array),
        !.
remembered_matching(Mapping,L,remember{matching:Array},
                    OldMatching,
                    SizeOld):-
        
        (foreach(X,L),
         count(J,1,_),
         fromto(OldMatching,A1,A,[]),
         fromto(0,B,B1,SizeOld),
         param(Mapping,Array) do
            subscript(Array,[J],Old),
            (is_in_domain(Old,X) ->
                value_lookup(Mapping,Old,To),
                A1 = [e(J,To,Old)|A],
                B1 is B+1
            ;
                A1 = A,
                B1 = B
            )
        ).

remember_matching(N,MaximalM,Remember,_Mapping):-
        N1 is N+1,
        dim(OldMatch,[N1]),
        (foreach(e(Node,_,Value),MaximalM),
         param(OldMatch) do
            subscript(OldMatch,[Node],Value)
        ),
        setarg(1,Remember,OldMatch).

mark_solved(remember{solved:1}).

is_solved(remember{solved:Flag}):-
        integer(Flag).

:-pragma(debug).
