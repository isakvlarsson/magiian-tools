:- module(visualize_memoryless_strategy, [
  view_memoryless_strategy/3,
  view_memoryless_strategy/4,
  export_memoryless_strategy/3,
  export_memoryless_strategy/4
]).

:- use_module(library(gv)).
:- use_module('../game/main', [observations_to_dot/3]).
:- use_module('dot').

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
  transitions_to_dot(Out, Game, Expansion, Strategy),
  observations_to_dot(Out, Game, Expansion).


transitions_to_dot(Out, Game, Expansion, Strategy) :-
  forall(
    game(Game, Expansion, location(From)), 
    (
      get_memoryless_strategy(From, Strategy, Act),
      forall(
        game(Game, Expansion, transition(From, Act, To)),
        strategy_edge_id(Out, From, To, Act)
      ),
      forall(
        game(Game, Expansion, location(To)),
        (
          findall(
            To-Other,
            (
              game(Game, Expansion, transition(From, Other, To)),
              Other \= Act
            ),
            Transitions
          ),
          sort(Transitions, TransitionsSorted), 
          group_pairs_by_key(TransitionsSorted, Ts),
          forall(
            member(T-A, Ts),
            dot_arc_id(Out, From, To, [label(A)])
          )
        )
      )
    )
  ).



