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
% The Original Code is The Maximum Flow Library
% The Initial Developer of the Original Code is  CrossCore Optimization Ltd.
% Portions created by the Initial Developer are  Copyright (C) 2006-2007.
% All Rights Reserved.
% 
% 
% END LICENSE BLOCK
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:-module(max_flow).
:-comment(summary,"Ford-Fulkerson maximum flow algorithm").
:-comment(author,"CrossCore Optimization Ltd").
:-comment(copyright,"2007, CrossCore Optimization Ltd").
:-comment(status,prototype).
:-comment(date,"2006-2007").

:-lib(graph_algorithms).
:-lib(hash).

:-export(max_flow/5).
:-comment(max_flow/5,
          [
              summary:"Ford-Fulkerson maximum flow algorithm",
              amode:max_flow(+,+,+,+,-),
              args:[
                       "Graph": "a graph structure, no parallel edges,"
                                " e(Src,Dest,EdgeData)", 
                       "CapacityArg": "which argument of EdgeData to use as"
                                      " edge capacity (integer), (0 if"
                                      " EdgeData is a single number and -1"
                                      " if every edge capacity is 1)",
                       "SourceNode": "source node number (integer)",
                       "SinkNode": "sink node number (integer)",
                       "MaxFlowValue": "value of the maximum flow"
                   ],
              desc:html("This predicate provides an implementation of"
                        " the Ford-Fulkerson max-flow algorithm"
                        " between two nodes in a graph. It the returns"
                        " the maximal achievable flow allowed by the"
                        " capacities in the network."),
              see_also:[max_flow:max_flow/5,
                        max_flow:max_flow/7,
                        max_flow_eplex:max_flow_eplex/5,
                        max_flow_eplex:max_flow_eplex_dual/5,
                        max_flow_eplex:max_flow_eplex_dual/7,
                        all_min_cuts:all_min_cuts/8,
                        all_min_cuts:all_min_cuts/9,
                        all_min_cuts:all_min_cuts_list/5,
                        all_min_cuts_eplex:all_min_cuts_eplex/7,
                        all_min_cuts_eplex:all_min_cuts_eplex/8
                       ]
                        
          ]
         ).

:-export(max_flow/7).
:-comment(max_flow/7,
          [
              summary:"Ford-Fulkerson maximum flow algorithm",
              amode:max_flow(+,+,+,+,-,-,-),
              args:[
                       "Graph": "a graph structure, no parallel edges,"
                                " e(Src,Dest,EdgeData)", 
                       "CapacityArg": "which argument of EdgeData to use as"
                                      " edge capacity (integer), (0 if"
                                      " EdgeData is a single number and -1"
                                      " if every edge capacity is 1)",
                       "SourceNode": "source node number (integer)",
                       "SinkNode": "sink node number (integer)",
                       "MaxFlowValue": "value of the maximum flow",
                       "MaxFlowEdges": "list denoting edges with non-zero"
                                       " flow (form: Flow-Edge)",
                       "MaxFlowEdgesGraph": "a graph structure, original"
                                            " nodes (as in Graph) but only"
                                            " the edges that are in max flow"
                   ],
              desc:html("This predicate provides an implementation of"
                        " the Ford-Fulkerson max-flow algorithm"
                        " between two nodes in a graph. It the returns"
                        " the maximal achievable flow allowed by the"
                        " capacities in the network, a list of all"
                        " edges with non-zero flow, and a graph of the"
                        " edges with non-zero flow."),
              see_also:[max_flow:max_flow/5,
                        max_flow:max_flow/7,
                        max_flow_eplex:max_flow_eplex/5,
                        max_flow_eplex:max_flow_eplex_dual/5,
                        max_flow_eplex:max_flow_eplex_dual/7,
                        all_min_cuts:all_min_cuts/8,
                        all_min_cuts:all_min_cuts/9,
                        all_min_cuts:all_min_cuts_list/5,
                        all_min_cuts_eplex:all_min_cuts_eplex/7,
                        all_min_cuts_eplex:all_min_cuts_eplex/8
                       ]
          ]
         ).


max_flow(Graph, CapacityArg, SourceNode, SinkNode, MaxFlowValue):-
        graph_get_maxnode(Graph,N),
        graph_get_all_edges(Graph,Edges),
        initialize_residual_capacities(N,Edges,CapacityArg,ResidualCapacities),
        max_flow_aux(N,ResidualCapacities,SourceNode,SinkNode,0,MaxFlowValue). 
        
max_flow(Graph, CapacityArg, SourceNode, SinkNode, MaxFlowValue,
         MaxFlowEdges, MaxFlowEdgesGraph):-
        max_flow(Graph, CapacityArg, SourceNode, SinkNode, MaxFlowValue,
                 MaxFlowEdges, MaxFlowEdgesGraph,_).
        
max_flow(Graph, CapacityArg, SourceNode, SinkNode, MaxFlowValue,
         MaxFlowEdges, MaxFlowEdgesGraph, ResidualCapacities):-
        graph_get_maxnode(Graph,N),
        graph_get_all_edges(Graph,Edges),
        initialize_residual_capacities(N,Edges,CapacityArg,ResidualCapacities),
        max_flow_aux(N,ResidualCapacities,SourceNode,SinkNode,0,MaxFlowValue), 
        get_max_flow_edges(N, Edges, CapacityArg,ResidualCapacities,
                           MaxFlowEdges,MaxFlowEdgesGraph). 


max_flow_aux(N,ResidualCapacities,SourceNode, SinkNode, 
             FlowValue, MaxFlowValue):-
        
        dim(NodeLabels,[N]),
        dim(Predecessors,[N]),
        initialize_node_labels(NodeLabels),
        initialize_predecessor_array(Predecessors),
        
        label(NodeLabels,SourceNode),
        (
            find_path(ResidualCapacities, SinkNode,
                      NodeLabels, Predecessors, [SourceNode])
        ->
            augment(ResidualCapacities,SourceNode, SinkNode,
                    Predecessors,FlowValue,NewFlowValue), 
            max_flow_aux(N,ResidualCapacities, SourceNode,
                         SinkNode, NewFlowValue, MaxFlowValue) 
        ;
            MaxFlowValue = FlowValue
        ).
        
        
find_path(_ResidualCapacities, SinkNode, NodeLabels,
          _Predecessors, 
          _List):-
        labeled(NodeLabels,SinkNode),
        !.

find_path(ResidualCapacities, SinkNode, NodeLabels,
          Predecessors, 
          List):-
        List = [I|Rest],
        get_valid_adjacent_nodes(ResidualCapacities,I,Js),
        (
            foreach(J,Js),
            fromto(Rest,In,Out,NewList),
            param(NodeLabels, Predecessors, I)
        do
            (
                unlabeled(NodeLabels,J)
            ->
                setarg(J,Predecessors,I),
                label(NodeLabels,J),
                Out = [J|In]
            ;
                In = Out
            )
        ),
        
        find_path(ResidualCapacities, SinkNode, NodeLabels,
                  Predecessors, 
                  NewList).
        
            
augment(ResidualCapacities, SourceNode, SinkNode,
        Predecessors, FlowValue, NewFlowValue):-
        arg(SinkNode,Predecessors,Pred),
        
        % residual_capacity of the last edge of the path
        get_edge_residual_capacity(ResidualCapacities,Pred,SinkNode,
                                   ResCapacity),
        
        augment_aux(ResidualCapacities,SourceNode,
                    Predecessors,Pred,ResCapacity,
                    [e(Pred,SinkNode,ResCapacity)],
                    MinResCapacity, Path), 

        update_residual_capacities(ResidualCapacities, Path, MinResCapacity),
        NewFlowValue is FlowValue + MinResCapacity.
        

augment_aux(_ResidualCapacities,SourceNode,_Predecessors,
            SourceNode,MinResCapacity,Path, MinResCapacity, Path):- 
        % source node reached, stop
        !.
augment_aux(ResidualCapacities,SourceNode,Predecessors,CurrentNode,
            CurrentMinResCapacity, PartialPath, MinResCapacity,Path):-  
        arg(CurrentNode,Predecessors,Pred),
        get_edge_residual_capacity(ResidualCapacities,Pred,CurrentNode,
                                   ResCapacity),
        
        (
            ResCapacity < CurrentMinResCapacity
        ->
            NewMinResCapacity = ResCapacity
        ;
            NewMinResCapacity = CurrentMinResCapacity
        ),
        augment_aux(ResidualCapacities,SourceNode,Predecessors,Pred,
                    NewMinResCapacity, [e(Pred,CurrentNode,
                                          ResCapacity)|PartialPath],  
                    MinResCapacity,Path).        

        

%% About the residual graph / residual capacity data structure:

%% ResidualCapacities is an array (arg = source node S) where
%% each item is a hash table (hash key = dest node D of an adjacent edge
%% for S, hash value = residual capacity for edge (S,D)

%% When finding a path, we get the valid adjacent edges from this data, and
%% no additional residual graph representation is needed.


%% Why is it done in this way instead of using residual graph as a graph?
%% Because of all tested residual graph / residual capacity
%% data structures, this appeared to be clearly the fastest representation
%% for this algorithm, where residual capacities of edges are continuously
%% updated (maybe you have a more elegant way to do it in the source of
%% graph_algorithms?) 




initialize_residual_capacities(N,Edges,CapacityArg,
                               ResidualCapacities):- 
        dim(ResidualCapacities,[N]),
        (
            foreacharg(AdjEdges,ResidualCapacities)
        do
            hash_create(AdjEdges)
        ),

        (
            foreach(e(Src,Dst,Info),Edges),
            param(CapacityArg,ResidualCapacities)
        do
            capacity(CapacityArg,Info,Capacity),
            arg(Src,ResidualCapacities,AdjEdges),
            (
		hash_get(AdjEdges,Dst,0)
            ->
                % arc added already as opposite edge
                hash_set(AdjEdges,Dst,Capacity)
            ;
                % arc not added yet
                hash_add(AdjEdges,Dst,Capacity),
                % and the opposite
		arg(Dst,ResidualCapacities,OppAdjEdges),
                hash_add(OppAdjEdges,Src,0)
            )
        ).



capacity(-1,_EdgeInfo,1):-!.
capacity(0,EdgeInfo,EdgeInfo):-!.
capacity(CapacityArg,EdgeInfo,Capacity):-
        CapacityArg > 0,
        !,
        arg(CapacityArg,EdgeInfo,Capacity).
capacity(_,_,_):-!,fail.

        
get_edge_residual_capacity(ResidualCapacities,Src,Dest,ResCapacity):-
	arg(Src,ResidualCapacities,AdjEdges),
	hash_get(AdjEdges,Dest,ResCapacity).

initialize_node_labels(NodeLabels):-
        (
            foreacharg(0,NodeLabels)
        do
            true
        ).
        

initialize_predecessor_array(Predecessors):-
        (
            foreacharg(0,Predecessors)
        do
            true
        ).
        
        
label(NodeLabels,Node):-
        setarg(Node,NodeLabels,1).

labeled(NodeLabels,Node):-
        arg(Node,NodeLabels,Val),
        Val == 1.
unlabeled(NodeLabels,Node):-
        arg(Node,NodeLabels,Val),
        Val == 0.
        
        
update_residual_capacities(ResidualCapacities, Path, PathSize):-      
        (
            foreach(e(S,D,ResCapacity),Path),
            param(ResidualCapacities,PathSize)
        do
            NewResCapacity is ResCapacity - PathSize,
            arg(S,ResidualCapacities,AdjEdges),
            hash_set(AdjEdges,D,NewResCapacity),

            % and _add_ PathSize to opposite direction
	    arg(D,ResidualCapacities,OppAdjEdges),
	    hash_get(OppAdjEdges,S,OppResCapacity),

            NewOppResCapacity is OppResCapacity + PathSize,
            hash_set(OppAdjEdges,S,NewOppResCapacity)
        ).
  
  
get_valid_adjacent_nodes(ResidualCapacities,Src,Neighbours):-
        arg(Src,ResidualCapacities,AdjEdges),
        hash_list(AdjEdges,Dests,ResCapacities),
        (
            fromto([],In,Out,Neighbours),
            foreach(D,Dests),
            foreach(ResCapacity,ResCapacities)
        do
            (
                ResCapacity > 0
            ->
                Out = [D|In]
            ;
                Out = In
            )
        ).

        
get_max_flow_edges(N, Edges, CapacityArg,ResidualCapacities, MaxFlowEdges, 
                   MaxFlowEdgesGraph):-
        (
            foreach(e(Src,Dst,Info),Edges),
            fromto([],In,Out,MaxFlowEdges),
            fromto([],In1,Out1,MaxFlowJustEdges),
            param(CapacityArg,ResidualCapacities)
        do
            capacity(CapacityArg,Info,Capacity),
            get_edge_residual_capacity(ResidualCapacities,Src,Dst,ResCapacity),
            (
                ResCapacity < Capacity
            ->
                Flow is Capacity - ResCapacity,
                Out = [Flow-e(Src,Dst,Info)|In],
                Out1 = [e(Src,Dst,Info)|In1]
            ;
                Out = In,
                Out1 = In1
            )
        ),
        make_graph(N,MaxFlowJustEdges,MaxFlowEdgesGraph).







%% the following are exported for module all_min_cuts


:-comment(initialize_residual_capacities/4,hidden).
:-export(initialize_residual_capacities/4).

:-comment(max_flow_aux/6,hidden).
:-export(max_flow_aux/6).

:-comment(get_max_flow_edges/6,hidden).
:-export(get_max_flow_edges/6).

:-comment(max_flow/8,hidden).
:-export(max_flow/8).



