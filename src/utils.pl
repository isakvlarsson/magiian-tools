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


%% finds the elements that are in all the lists (and sublists) of a list
% I assume that lists and non-list-elements wont be mixed inside a single list
nested_intersection(NestedList, Intersection) :-
  flatten(NestedList, Elements),
  nested_intersection(NestedList, Elements, Intersection).

nested_intersection([H|T], Acc, I) :-
  \+is_list(H),
  intersection([H|T], Acc, I).
nested_intersection([], _, _).
nested_intersection([H|T], Acc, I) :-
  is_list(H),
  nested_intersection(H, Acc, InnerAcc1),
  nested_intersection(T, InnerAcc1, InnerAcc2),
  intersection(InnerAcc1, InnerAcc2, I), !.
