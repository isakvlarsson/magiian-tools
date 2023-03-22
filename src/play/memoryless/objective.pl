:- module(memoryless_objective, [
  has_reachability_objective/3,
  has_safety_objective/3,
  has_buchi_objective/3,
  has_co_buchi_objective/3,
  reachability_outcome/5
]).

% ################## Find all Objectives ########################
%% a game has a reachability objective to the
% locations in reachable with this expansion
% (not nececarilly for the same strategy)
has_reachability_objective(Game, Expansion, Reachable) :-
  findall(
    R,
    (
      memoryless_strategy(Game, Expansion, Strategy),
      outcome(Game, Expansion, Strategy, Outcome),
      reached_in_outcome(Outcome, R)
    ),
    Reached
  ),
  union_all(Reached, Reachable).

%% a game has a safety objective with the
% locations in Avoided with this expansion
has_safety_objective(Game, Expansion, Avoided) :-
  findall(L, game(Game, location(L)), Locations),
  has_reachability_objective(Game, Expansion, Reachable),
  subtract(Locations, Reachable, Avoided).
  
%% a game has a buchi objective with the locations
% in Buchi with this Expansion
has_buchi_objective(Game, Expansion, Buchi) :-
  findall(
    R,
    (
      memoryless_strategy(Game, Expansion, Strategy),
      outcome(Game, Expansion, Strategy, Outcome),
      inf_in_outcome(Outcome, R)
    ),
    Reached
  ),
  union_all(Reached, Buchi).

%% a game has a co-buchi objective with the locations
% in CoBuchi with this expansion
has_co_buchi_objective(Game, Expansion, CoBuchi) :-
  findall(L, game(Game, location(L)), Locations),
  buchi_objective(Game, Expansion, Buchi),
  subtract(Locations, Buchi, CoBuchi).

% ################# Find objectives and show them ######
reachability_outcome(G, K, Ls, S, Outcome) :-
  memoryless_strategy(G, K, S),
  outcome(G, K, S, Outcome),
  reached_in_outcome(Outcome, Reached),
  subset(Ls, Reached).


% ################## Helpers ###########################
%% make statements about outcomes

%% the members of Reached are guaranteed to
% be a part of an outcome, atleast
% one time
reached_in_outcome(Outcome, Reached) :-
  reached_in_outcome(Outcome, [], Reached).
reached_in_outcome([fork(Forks)|T], Acc, Reached) :-
  % the members of Reached must be members of all forks
  maplist(reached_in_outcome, Forks, ForksReached),
  intersection_all(ForksReached, AllReached),
  append(AllReached, Acc, NewAcc),
  reached_in_outcome(T, NewAcc, Reached), !.
reached_in_outcome([loop(Loop)|T], Acc, Reached) :-
  % the members of a loop are all reached
  setofall(
    Reach,
    member(Reach, Loop),
    LoopReached
  ), 
  append(LoopReached, Acc, NewAcc),
  reached_in_outcome(T, NewAcc, Reached), !.
reached_in_outcome([H|T], Acc, Reached) :-
  reached_in_outcome(T, [H|Acc], Reached).
reached_in_outcome([], Acc, Reached) :-
  % remove duplicates
  sort(Acc, Reached).

%% the members of Inf occurs
% an infinite number of times
% in an outcome
inf_in_outcome(Outcome, Inf) :-
  inf_in_outcome(Outcome, [], Inf).
inf_in_outcome([], Acc, Inf) :-
  % remove duplicates
  sort(Acc, Inf), !.
inf_in_outcome([loop(Loop)|T], Acc, Inf) :-
  append(Loop, Acc, NewAcc),
  inf_in_outcome(T, NewAcc, Inf), !.
inf_in_outcome([fork(Forks)| T], Acc, Inf) :-
  % the infs must be infs in all the forks
  maplist(inf_in_outcome, Forks, ForksInf),
  intersection_all(ForksInf, AllInf),
  append(AllInf, Acc, NewAcc),
  inf_in_outcome(T, NewAcc, Inf), !.
inf_in_outcome([H|T], Acc, Inf) :-
  % normal locations are just ignored
  inf_in_outcome(T, Acc, Inf).
