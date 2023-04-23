:- module(outcome_graph, [
  create_outcome_graph/2,
  create_outcome_graph/3,
  unload_outcome_graph/2,
  outcome_graph_node/4,
  outcome_graph_edge/5
]).
:- use_module(strategy).
:- use_module('../../utils').
:- use_module(library(term_ext)).

:- dynamic outcome_graph_node/4, outcome_graph_edge/5.

%% defaults to memoryless strategy
create_outcome_graph(G, K) :-
  create_outcome_graph(G, K, 1).

create_outcome_graph(G, K, MaxMem) :-
  unload_outcome_graph(G, K),
  empty_visited(G, K, V),
  empty_strategy(G, K, S),
  game(G, K, initial(Init)),
  create_outcome_graph_node(G, K, Init, InitNode),
  create_outcome_graph(G, K, MaxMem, InitNode, S, V),
  !.

create_outcome_graph(G, K, MaxMem, Node, S, V) :-
  outcome_graph_node(G, K, Loc, Node),
  setofall(
    Act-Next,
    game(G, K, transition(Loc, Act, Next)),
    Transitions
  ),
  group_pairs_by_key(Transitions, ActTransitions),
  forall(
    member(Act-Nexts, ActTransitions),
    (
      % `put_strategy` automatically handles if the action
      % for an agent is the same as the last one taken 
      put_strategy(Loc, S, Act, S1), 
      max_acts(Loc, S1, MaxMem),
      forall(
        member(Next, Nexts),
        %% handle non-determinism by reseting visited
        (
          length(Nexts, TransitionsForAction),
          visited_inc(Loc, V, V1),
          (
            % nondeterminism
            TransitionsForAction > 1 ->
              empty_visited(G, K, V0),
              take_action(G, K, MaxMem, Node, Act, Next, S1, V0);
            % normal transition
            
            % loop encountered
            
          )
        )
      )
    )
  ), 
  !.

create_outcome_graph(_, _, _, _, _, _) :- !.

take_action(G, K, MaxMem, Node, Act, Next, S, V) :-
  create_outcome_graph_node(G, K, Next, NextNode),
  create_outcome_graph_edge(G, K, Node, Act, NextNode),
  create_outcome_graph(G, K, MaxMem, NextNode, S, V).

%% these are random names for
% locations, so that we can
% separate 'same' nodes in
% different outcomes
create_outcome_graph_node(G, K, L, N) :-
  uuid(U), 
  ascii_id(U, N),
  assertz(outcome_graph_node(G, K, L, N)).

create_outcome_graph_edge(G, K, N1, Act, N2) :-
  assertz(outcome_graph_edge(G, K, N1, Act, N2)).

unload_outcome_graph(G, K) :-
  retractall(outcome_graph_node(G, K, _, _)),
  retractall(outcome_graph_edge(G, K, _, _, _)).

% ################ helpers #####################

empty_visited(G, K, V) :-
  findall(L, game(G, K, location(L)), Ls),
  maplist(pair_default(0), Ls, Ps),
  list_to_assoc(Ps, V).

visited_inc(Loc, V0, V1) :-
  get_assoc(Loc, V0, Num),
  Num1 is Num + 1,
  put_assoc(Loc, V0, Num1, V1).

%% true if the Loc is not visited 
% more times than Num
max_visited(Loc, V, Num) :-
  get_assoc(Loc, V, N),
  N =< Num.

%% get the last added node in a path that
% contains Last
last_loc_node_in_path(G, K, Loc, Node, Last) :-
  outcome_graph_edge(G, K, Prev, _, Last),
  (
    outcome_graph_node(G, K, Loc, Prev) -> Node = Prev;
    last_loc_node_in_path(G, K, Loc, Node, Prev)
  ).

%% true if no agent has more than Num
% elements in their acts at Loc
max_acts(Loc, S, Num) :-
  get_strategy(Loc, S, Acts),
  maplist(length, Acts, ActsLen),
  forall(member(X, ActsLen), X =< Num).

