:- [lib/game].
:- [lib/visualize].
:- [lib/utils].

main :-
  Game = wagon_game,
  load_game(Game),
  forall(
    game(Game, agent(Agent)),
    (
      create_projection(Game, 0, Agent),
      create_projection_expansion(Game, 0, Agent)
    )
  ).
