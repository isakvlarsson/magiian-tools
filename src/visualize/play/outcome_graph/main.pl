:- module(visualize_outcome_graph, [
  view_outcome_graph/2
]).

:- use_module(library(gv)).
:- use_module('../../game/mkbsc/location_pointer', [actual_location/3]).

view_outcome_graph(G, K) :-
  gv_view(
    {G, K}/[Out]>>outcome_graph_to_dot(Out, G, K),
    [directed(true), method(dot)]
  ).

outcome_graph_to_dot(Out, G, K) :-
  outcome_graph_nodes_to_dot(Out, G, K),
  outcome_graph_edges_to_dot(Out, G, K).

outcome_graph_nodes_to_dot(Out, G, K) :-
  forall(
    outcome_graph_node(G, K, L, N),
    (
      actual_location(G, L, Actual),
      dot_node_id(Out, N, [label(Actual)])
    )
  ).

outcome_graph_edges_to_dot(Out, G, K) :-
  forall(
    outcome_graph_edge(G, K, N1, Act, N2),
    dot_arc_id(Out, N1, N2, [label(Act)])
  ).
