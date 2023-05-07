:- module(outcome_graph_optimized_analyze, [
  find_linked_outcomes/3
]).

:- use_module(vague_strategy).
:- use_module(strategy).

% finds outcomes that have compatible strategies
find_linked_outcomes(G, K, Ids) :-
  outcome_graph_goto_edge(G, K, _, _, Id, _, S, _),
  find_linked_outcomes(G, K, [Id], S, Ids).
find_linked_outcomes(G, K, IdsAcc, S, Ids) :-
  compatible_strategies(G, K, IdsAcc-S, Id2-S2),
  merge_strategies(G, K, S1, S2, MergedS),
  find_linked_outcomes(G, K, [Id2|IdsAcc], MergedS, Ids).
find_linked_outcomes(G, K, IdsAcc, S, IdsAcc) :-
  \+compatible_strategies(G, K, IdsAcc-S, Id2-S2).

merge_strategies(G, K, S1, S2, Merged) :-
  setofall(L, game(G, K, location(L)), Ls),
  maplist(location_actionprofile(G, K, S1), Ls, Pairs1),
  maplist(location_actionprofile(G, K, S2), Ls, Pairs2),
  append(Pairs1, Pairs2, Pairs),
  sort(Pairs, PairsS),
  group_pairs_by_key(PairsS, PairsG),
  maplist(union_of_location_actionprofile, PairsG, PairsU),
  populate_merged_strategy(G, K, PairsU, Merged),
  !.
  
populate_merged_strategy(G, K, LActs, S) :-
  empty_strategy(G, K, S0), 
  populate_merged_strategy(LActs, S0, S).
populate_merged_strategy([L-Acts|T], S1, S) :-
  vague_put_strategy(L, S1, Acts, S2),
  populate_merged_strategy(T, S2, S).
populate_merged_strategy([], S, S).

location_actionprofile(G, K, S, L, L-Act) :-
  possible_action_profiles(G, K, L, S, Act),
  !;
  Act = [].

union_of_location_actionprofile(L-Acts, L-Unions) :-
  union_all(Acts, Unions).

% put in a strategy (can be a merged one)
% and check for compatible strategies
compatible_strategies(G, K, Ids-S1, Id2-S2) :-
  outcome_graph_goto_edge(G, K, _, _, Id2, _, S2, _),
  \+memberchk(Id2, Ids),
  % (a, b) = (b, a), remove duplicates
  %sort([Id1, Id2], [Id1, Id2]),
  forall(
    game(G, K, location(L)),
    (
      possible_action_profiles(G, K, L, S1, Acts1),
      writeln(L-Acts1),
      possible_action_profiles(G, K, L, S2, Acts2),
      (
        % eiter Acts1 is a subset of Acts2 or vice versa
        subset(Acts1, Acts2),
        subset(Acts2, Acts1)
      )
      % forall has to succed, because it could be the case
      % that no actions are assigned to an agent, and that
      % should also be a compatible strategy, because then
      % that agent could be asssigne actions there.
    )
  ).
