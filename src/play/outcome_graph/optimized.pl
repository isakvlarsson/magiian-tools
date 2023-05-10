:- module(optimized_outcome_graph, [
  create_optimized_outcome_graph/2
]).

:- use_module(strategy).
:- use_module(main).
:- use_module(vague_strategy).
:- use_module('../../utils').

create_optimized_outcome_graph(G, K) :-
  unload_outcome_graph(G, K),
  game(G, K, initial(Init)),
  create_outcome_graph_node(G, K, Init, InitNode),
  empty_strategy(G, K, S),
  empty_visited(G, K, V),
  create_optimized_outcome_graph(G, K, InitNode, S, V, []),
  !.

create_optimized_outcome_graph(G, K, Node, S, V, History) :-
  outcome_graph_node(G, K, Loc, Node),
  setofall(
    Next-Act,
    game(G, K, transition(Loc, Act, Next)),
    Transitions
  ),
  group_pairs_by_key(Transitions, ActsToNext),
  forall(
    member(Next-Acts, ActsToNext),
    (
      %% put the set of actions leading to Next
      % in the strategy
      vague_put_strategy(Loc, S, Acts, S1),
      %% handle observations - actions must be the
      % same on all locaitons here, so there may already
      % be an action 'taken' for a location.
      % In that case we do not take it.
      (
        max_acts(Loc, S1, 1) ->
          possible_action_profiles(G, K, Loc, S1, Acts),
          inc_visited(Loc, V, V1),
          take_action(G, K, Node, Next-Acts, S1, V1, [Loc-Node|History])
        ;
          % we dont want to stop if the above fails
          % (because forall should be true for everything...)
          true
      )
    )
  ),
  !.

take_action(G, K, Node, Next-Acts, S, V, History) :-
  % normal transition
  %\+memberchk(Next-_, V),
  max_visited(Next, V, 0),
  create_outcome_graph_node(G, K, Next, NextNode),
  create_outcome_graph_edge(G, K, Node, Acts, NextNode),
  create_optimized_outcome_graph(G, K, NextNode, S, V, History),
  !.

take_action(G, K, Node, Next-Acts, S, V, History) :-
  % there is a loop
  %memberchk(Next-LoopStartNode, V),
  \+max_visited(Next, V, 0),
  nth0(Backsteps, History, Next-LoopStartNode),
  create_outcome_graph_goto(G, K, Node, LoopStartNode, Backsteps, Id),
  create_outcome_graph_goto_edge(G, K, Node, LoopStartNode, Id, Acts, S, History),
  !.

