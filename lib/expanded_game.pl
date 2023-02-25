% An MKBSC expansion of a game
expanded_game(Game, 0, Terms) :-
  agents(Game, Agents),
  actions(Game, Actions),
  % make locations to singletons
  locations(Game, Locations),
  maplist(singleton_term, Locations, Locations0),
  initial(Game, Initial),
  singleton_term(Initial, Initial0),
  transitions(Game, Transitions),
  maplist(singleton_term, Transitions, Transitions0),
  observations(Game, Observations),
  Terms = [Agents, Actions, Locations0, Initial0, Transitions0, Observations], !.
expanded_game(Game, Expansion, Terms) :-
  Expansion > 0,
  writeln('not implemented').

