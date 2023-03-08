:- [lib/game].
:- [lib/visualize].
:- [lib/expanded_game].
:- [lib/utils].

main :-
  load_game(wagon_game),
  create_expanded_game(wagon_game, 0).
