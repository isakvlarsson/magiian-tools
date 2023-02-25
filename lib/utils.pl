singleton(Elem, [Elem]).

singleton_term(Term, SingletonTerm) :-
  Term =.. [Predicate|Arguments],
  maplist(singleton, Arguments, SingletonArguments),
  SingletonTerm =.. [Predicate|SingletonArguments].

