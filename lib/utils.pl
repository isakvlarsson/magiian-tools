singleton(Elem, [Elem]).

singleton_term(Term, SingletonTerm) :-
  Term =.. [Predicate|Arguments],
  maplist(singleton, Arguments, SingletonArguments),
  SingletonTerm =.. [Predicate|SingletonArguments].

% stolen from https://stackoverflow.com/questions/65581395/powerset-o-a-setlist-in-prolog
% my_subset(Set, Subset).
my_subset([], []).
my_subset([_|T], R):- my_subset(T, R).     % does not include first element
my_subset([H|T], [H|R]):- my_subset(T,R).  % do include first element  

powerset(Set, Powerset) :-
  findall(Subset, my_subset(Set, Subset), Powerset).
