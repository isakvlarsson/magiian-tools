:- [lib/game].
:- [lib/visualize].
:- [lib/utils].
:- [lib/strategy].

main :-
  Game = wagon_game,
  load_game(Game),
  create_expanded_game(Game, 5).
