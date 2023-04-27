:- module(visualize_outcome_graph, [
  view_outcome_graph/2,
  export_outcome_graph/2
]).

:- use_module(library(gv)).
:- use_module(library(term_ext)).
:- use_module('../../../game/mkbsc/location_pointer', [actual_location/3]).

view_outcome_graph(G, K) :-
  gv_view(
    {G, K}/[Out]>>outcome_graph_to_dot(Out, G, K),
    [directed(true), method(dot)]
  ).

export_outcome_graph(G, K) :-
  format(atom(Filename), 'images/~a_K~a_outcome_graph.png', [G, K]),
  gv_export(
    Filename,
    {G, K}/[Out]>>outcome_graph_to_dot(Out, G, K),
    [directed(true), method(dot)]
  ).

outcome_graph_to_dot(Out, G, K) :-
  outcome_graph_nodes_to_dot(Out, G, K),
  outcome_graph_edges_to_dot(Out, G, K),
  outcome_graph_gotos_to_dot(Out, G, K).

outcome_graph_nodes_to_dot(Out, G, K) :-
  forall(
    outcome_graph_node(G, K, L, N),
    (
      % handle loop start nodes and normal nodes
      (
        outcome_graph_goto(G, K, _, N, _, _) ->
          actual_location(G, L, Actual),
          format(Out, '~w [style="filled" fillcolor="gray" label="~w"]\n', [N, Actual])
        ;
          actual_location(G, L, Actual),
          dot_node_id(Out, N, [label(Actual)])
      )
    )
  ).

outcome_graph_edges_to_dot(Out, G, K) :-
  setofall(
    [From, To, Type]-Act,
    outcome_graph_edge(G, K, From, Act, To, Type),
    Transitions
  ),
  group_pairs_by_key(Transitions, GroupedTransitions),
  forall(
    member([From, To, Type]-Acts, GroupedTransitions),
    (
      % deterministic
      Type = det ->
        dot_arc_id(Out, From, To, [label(Acts)])
      ;
      % nondeterministic
        format(Out, '~w -> ~w [style="dashed" label="~w"]', [From, To, Acts]) 
    )
  ).

outcome_graph_gotos_to_dot(Out, G, K) :-
  % first the 'nodes'
  forall(
    outcome_graph_goto(G, K, _, _, Backsteps, Id),
    (
      Backsteps1 is Backsteps + 1,
      format(Out, '~w [shape="diamond" label="up ~w"]', [Id, Backsteps1])
    )
  ),
  % then the 'edges'
  setofall(
    [End, Id, Type]-Act,
    outcome_graph_goto_edge(G, K, End, _, Id, Act, Type, _, _),
    Edges
  ),
  group_pairs_by_key(Edges, GroupedEdges),
  forall(
    member([End, Id, Type]-Acts, GroupedEdges),
    (
      % deterministic
      Type = det ->
        dot_arc_id(Out, End, Id, [label(Acts)])
      ;
      % nondeterministic
        format(Out, '~w -> ~w [style="dashed" label="~w"]', [End, Id, Acts]) 
    )
  ).

