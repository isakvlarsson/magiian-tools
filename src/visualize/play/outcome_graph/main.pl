:- module(visualize_outcome_graph, [
  view_outcome_graph/2
]).

:- use_module(library(gv)).
:- use_module(library(term_ext)).
:- use_module('../../game/mkbsc/location_pointer', [actual_location/3]).

view_outcome_graph(G, K) :-
  gv_view(
    {G, K}/[Out]>>outcome_graph_to_dot(Out, G, K),
    [directed(true), method(dot)]
  ).

outcome_graph_to_dot(Out, G, K) :-
  outcome_graph_nodes_to_dot(Out, G, K),
  outcome_graph_edges_to_dot(Out, G, K),
  outcome_graph_loops_to_dot(Out, G, K).

outcome_graph_nodes_to_dot(Out, G, K) :-
  forall(
    outcome_graph_node(G, K, L, N),
    (
      % handle loop start nodes and normal nodes
      (
        outcome_graph_loop(G, K, _, _, N) ->
          actual_location(G, L, Actual),
          format(Out, '~w [style="filled" fillcolor="gray" label="~w"]\n', [N, Actual])
        ;
          actual_location(G, L, Actual),
          dot_node_id(Out, N, [label(Actual)])
      )
    )
  ).

outcome_graph_edges_to_dot(Out, G, K) :-
  forall(
    outcome_graph_edge(G, K, N1, Act, N2),
    dot_arc_id(Out, N1, N2, [label(Act)])
  ).

outcome_graph_loops_to_dot(Out, G, K) :-
  forall(
    outcome_graph_loop(G, K, End, Act, Start),
    (
      random_id(Id),
      format(Out, '~w [label= "", shape="none",height=.0,width=.0]', [Id]),
      dot_arc_id(Out, End, Id, [label(Act)])
    )
  ).

random_id(Id) :-
  uuid(U),
  ascii_id(U, Id).

