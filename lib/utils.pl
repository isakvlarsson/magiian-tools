/*
 * ################### General helpers ########################
 * Hera are a collection of helpers that are more generall
 * */


%% predicate to generate a set of all solutions
% sort of lake findall but with only unique solutions
setofall(Template, Goal, Set) :-
  findall(Template, Goal, Bag),
  sort(Bag, Set).


%% The interseciton of all lists
intersection_all(Lists, I) :-
  concatenation(Lists, F), sort(F, Acc),
  intersection_all(Lists, Acc, I).
intersection_all([], I, I).
intersection_all([H|T], Acc, I) :-
  intersection(H, Acc, NewAcc),
  intersection_all(T, NewAcc, I).


%% concatenation of all lists in a list
concatenation([], []).
concatenation([H|T], Concat) :-
  concatenation(T, SubConcat),
  append(H, SubConcat, Concat).


%% all elements in a list are the same
same([H|T]) :-
  same(T, H).
same([], _).
same([H|T], H) :-
  same(T, H).
