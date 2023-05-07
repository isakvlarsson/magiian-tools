:- module(visited, [
  empty_visited/3,
  inc_visited/3,
  max_visited/3
]). 

empty_visited(G, K, V0) :-
  setofall(
    L-0,
    game(G, K, location(L)),
    Ls
  ),
  list_to_assoc(Ls, V0).

inc_visited(L, V1, V2) :-
  get_assoc(L, V1, Count),
  NewCount is Count + 1,
  put_assoc(L, V1, NewCount, V2).

max_visited(L, V, MaxCount) :-
  get_assoc(L, V, Count),
  Count =< MaxCount.
