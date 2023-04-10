:- module(utils, [
  setofall/3,
  intersection_all/2,
  union_all/2,
  zip_pair/3,
  head/2,
  tail/2,
  same/2,
  pair_default/3,
  max/2
]).

:- meta_predicate setofall(?, 0, -).
/*
 * ################### General helpers ########################
 * Hera are a collection of helpers that are more generall
 *
 * Not all of these are used.
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

%% The union of all lists
union_all(Lists, I) :-
  concatenation(Lists, F), sort(F, Acc),
  union_all(Lists, Acc, I).
union_all([], I, I).
union_all([H|T], Acc, I) :-
  union(H, Acc, NewAcc),
  union_all(T, NewAcc, I).

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


%% pair two lists (of the same length)
% into a list of pairs
zip_pair(L1, L2, Pairs):-
  zip_pair(L1, L2, [], P),
  reverse(P, Pairs).
zip_pair([], [], Pairs, Pairs).
zip_pair([H1|T1], [H2|T2], Acc, Pairs) :-
  zip_pair(T1, T2, [H1-H2|Acc], Pairs).

%% get the largest integer in a list
max([H|T], Max) :-
  max(T, H, Max).
max([], M, M).
max([H|T], Acc, Max) :-
  H > Acc -> 
    max(T, H, Max);
    max(T, Acc, Max).

%% a constructor for creating pairs with a default value 
% with maplist
pair_default(Val, Key, Key-Val).

%% get the head of a list
head([H|T], H).
%% get the tail of a list
tail([], []).
tail([H|T], T).
