:- module(greedy,
  [
    greedy/3
  ]).
:- use_module(strategy).

%% All outcomes could be generated
% without a Strategy. The outcomes
% could then be searched for the
% preferable outcome and the strategy
% to get this part of the outcome is
% then the choices that was made
% to get it.

greedy(G, K, Strategy) :- 
  empty_strategy(G, K, Strategy),
  empty_visited(G, K, Visited),
  game(G, K, initial(Initial)),
  greedy(G, K, Visited, Strategy, Initial).

greedy(G, K, Visited, Strategy, L) :-
  % get some information
  % and we only want memoryless strategies
  get_assoc(L, Visited, 0),
  get_assoc(L, Strategy, PastActs),
  % find all possible actions
  findall(Act-To, game(G, K, transition(L, Act, To)), Ts),
  sort(Ts, SortedTs),
  group_pairs_by_key(SortedTs, Paths),
  member(Act-To, Paths),
  put_strategy(L, Strategy, [Act|PastActs], NewStrategy),
  (
    length(Paths, 1) -> put_assoc(L, Visited, 1, NewVisited);
    empty_visited(G, K, NewVisited)
  ),
  maplist(greedy(G, K, NewVisited, NewStrategy), To).


% ################ helpers #####################
empty_visited(G, K, V) :-
  findall(L, game(G, K, location(L)), Ls),
  maplist(pair_default(0), Ls, Ps),
  list_to_assoc(Ps, Strat).

