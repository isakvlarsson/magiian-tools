%% predicate to generate a set of all solutions
% sort of lake findall but with only unique solutions
setofall(Template, Goal, Set) :-
  findall(Template, Goal, Bag),
  sort(Bag, Set).


intersection_all(Lists, I) :-
  flatten(Lists, F), sort(F, Acc),
  intersection_all(Lists, Acc, I).

intersection_all([], I, I).
intersection_all([H|T], Acc, I) :-
  intersection(H, Acc, NewAcc),
  intersection_all(T, NewAcc, I).


%% flattens a list but only one layer of depth
my_flatten([], []).
my_flatten([A|B],L) :- is_list(A), my_flatten(B,B1), !, append(A,B1,L).
my_flatten([A|B],[A|B1]) :- my_flatten(B,B1).


