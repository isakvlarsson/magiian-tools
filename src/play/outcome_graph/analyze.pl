:- module(outcome_graph_analyze, [
  linked_outcomes/3
]).


% finds outcomes that has compatible strategies
linked_outcomes(G, K, Ids) :-
  outcome_graph_goto_edge(G, K, _, _, Id1, _, _, S1, _),
  findall(
    Id2,
    (
      Id2 \== Id,
      outcome_graph_goto_edge(G, K, _, _, Id2, _, _, S2, _),
      
    )
    Ids
  ).

strategies_are_compatible(G, K, S1, S2) :-
  forall(
    game(G, K, location(L)),
    (
      get_strategy(L, S1, Acts1),
      get_strategy(L, S2, Acts2),
      agent_strategies_are_compatible(G, K, Acts1, Acts2)
    )
).

agent_strategies_are_compatible(G, K, Acts1, Acts2) :-
  reverse(Acts1, Acts1Rev),
  reverse(Acts2, Acts2Rev),
  same_start(Acts1Rev, Acts2Rev).

same_start([H|T1], [H|T2]) :-
  same_start(T1, T2), !. 
same_start([], _) :- 
  !.
same_start(_, []) :-
  !.
