:- module(play_visited, [
    empty_visited/3,
    inc_visited/3,
    max_visited/3,
    new_visited/4
]).

%% create an empty visited
empty_visited(G, K, V) :-
  findall(L, game(G, K, location(L)), Ls),
  maplist(pair_default(0), Ls, Ps),
  list_to_assoc(Ps, V).

%% increment the visited count for a locaiton
inc_visited(Loc, V0, V1) :-
  get_assoc(Loc, V0, Num),
  Num1 is Num + 1,
  put_assoc(Loc, V0, Num1, V1).

%% true if the Loc is not visited 
% more times than Num
max_visited(Loc, V, Num) :-
  get_assoc(Loc, V, N),
  N =< Num.

%% get a new visited that only has
% visited one locaiton
new_visited(G, K, Loc, V) :-
  empty_visited(G, K, V0),
  inc_visited(Loc, V0, V).
