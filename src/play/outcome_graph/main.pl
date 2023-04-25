:- module(outcome_graph, [
  create_outcome_graph/2,
  unload_outcome_graph/2,
  outcome_graph_node/4,
  outcome_graph_edge/5,
  outcome_graph_goto/6,
  outcome_graph_goto_edge/5
]).
:- use_module(strategy).
:- use_module('../../utils').
:- use_module(library(term_ext)).
:- use_module('../visited').

:- dynamic outcome_graph_node/4,
           outcome_graph_edge/5,
           outcome_graph_goto/6,
           outcome_graph_goto_edge/5.

create_outcome_graph(G, K) :-
  unload_outcome_graph(G, K),
  game(G, K, initial(Init)),
  create_outcome_graph_node(G, K, Init, InitNode),
  empty_strategy(G, K, S),
  create_outcome_graph(G, K, InitNode, S, []),
  !.

create_outcome_graph(G, K, Node, S, V) :-
  outcome_graph_node(G, K, Loc, Node),
  setofall(
    Act,
    game(G, K, transition(Loc, Act, _)),
    Acts
  ),
  forall(
    member(Act, Acts),
    (
      put_strategy(Loc, S, Act, S1),
      %% handle observations - actions must be the
      % same on all locaitons here, so there may already
      % be an action 'taken' for a location.
      % In that case we do not take it.
      (
        max_acts(Loc, S1, 1) ->
          (
            setofall(
              Act-Next,
              game(G, K, transition(Loc, Act, Next)),
              ActTransitions
            ),
            group_pairs_by_key(ActTransitions, [Act-Nexts]),
            take_action(G, K, Node, S1, [Loc-Node|V], Act-Nexts)
          )
        ;
          % we dont want to stop if the above fails
          % (because forall should be true for everything...)
          true
      )
    )
  ),
  !.

take_action(G, K, Node, S, _, Act-Nexts) :-
  % non-deterministic transition
  length(Nexts, NextsLen),
  NextsLen > 1,
  outcome_graph_node(G, K, Loc, Node),
  forall(
    member(Next, Nexts),
    take_action(G, K, Node, S, [Loc-Node], Act-[Next])
  ),
  !.

take_action(G, K, Node, S, V, Act-[Next]) :-
  % normal transition
  \+memberchk(Next-_, V),
  (
    % this is an optimization to reuse nextnodes
    % if there are one available. It also makes
    % the outcome graph clearer to read and easier
    % to work with, because we mostly care about
    % the locations in the outcome.
    outcome_graph_node(G, K, Next, NextNode),
    outcome_graph_edge(G, K, Node, _, NextNode)->
      create_outcome_graph_edge(G, K, Node, Act, NextNode),
      create_outcome_graph(G, K, NextNode, S, V)
    ;
      create_outcome_graph_node(G, K, Next, NextNode),
      create_outcome_graph_edge(G, K, Node, Act, NextNode),
      create_outcome_graph(G, K, NextNode, S, V)
  ),
  !.

take_action(G, K, Node, _, V, Act-[Next]) :-
  % there is a loop
  memberchk(Next-LoopStartNode, V),
  nth0(Backsteps, V, Next-LoopStartNode),
  (
    % as with nodes, we reuse some of these
    % if they point back to the same node
    outcome_graph_goto(G, K, Node, LoopStartNode, Backsteps, Id) ->
      create_outcome_graph_goto_edge(G, K, Node, Id, Act)
    ;
      create_outcome_graph_goto(G, K, Node, LoopStartNode, Backsteps, Id),
      create_outcome_graph_goto_edge(G, K, Node, Id, Act)
  ),
  !.

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

create_outcome_graph_goto(G, K, End, Start, Back, Id) :-
  random_id(Id),
  assertz(outcome_graph_goto(G, K, End, Start, Back, Id)).

create_outcome_graph_goto_edge(G, K, End, Id, Act) :-
  assertz(outcome_graph_goto_edge(G, K, End, Id, Act)).

unload_outcome_graph(G, K) :-
  retractall(outcome_graph_node(G, K, _, _)),
  retractall(outcome_graph_edge(G, K, _, _, _)),
  retractall(outcome_graph_goto(G, K, _, _, _, _)),
  retractall(outcome_graph_goto_edge(G, K, _, _, _)).

random_id(Id) :-
  uuid(U),
  ascii_id(U, Id).
