view_memoryless_strategy(Game, Expansion, Strategy) :-
  view_memoryless_strategy(Game, Expansion, Strategy, standard).
view_memoryless_strategy(Game, Expansion, Strategy, Mode) :-
  gv_view(
    {Game, Expansion, Strategy, Mode}/[Out0]>>memoryless_strategy_to_dot(Out0, Game, Expansion, Strategy, Mode),
    [directed(true), method(dot)]
  ).

export_memoryless_strategy(Game, Expansion, Strategy) :-
  export_memoryless_strategy(Game, Expansion, Strategy, standard).
export_memoryless_strategy(Game, Expansion, Strategy, Mode) :-
  format(atom(Filename), 'images/~a_K~a_strategy.png', [Game, Expansion]),
  gv_export(
    Filename,
    {Game, Expansion, Strategy, Mode}/[Out0]>>memoryless_strategy_to_dot(Out0, Game, Expansion, Strategy, Mode),
    [directed(true), method(dot)]
  ).

memoryless_strategy_to_dot(Out, Game, Expansion, Strategy, Mode) :-
  locations_to_dot(Out, Game, Expansion, Mode),
  strategy_transitions_to_dot(Out, Game, Expansion, Strategy).


strategy_transitions_to_dot(Out, Game, Expansion, Strategy) :-
  forall(
    game(Game, Expansion, location(From)),
    (
      memberchk(From-ActionProfile, Strategy), 
      forall(
        game(Game, Expansion, transition(From, ActionProfile, To)),
        (
          dot_arc_id(Out, From, To, [label(ActionProfile)]);
          % when there is no transition between two locations
          % we write nothing to the stream
          format(Out, '', [])
        )
      )
    )
  ).
