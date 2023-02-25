:- use_module(library(gv)).

view_game():-
  game(Agt, Act, Loc, Tra, Obs),
  view_transitions(Tra).

view_transitions(Transitions) :-
  gv_view(
    {Transitions}/[Out0]>>export_game(Out0, Transitions),
    [directed(true), method(dot)]
  ).

export() :-
  game(Game),
  gv_export(
    'game.png',
    {Game}/[Out0]>>export_game(Out0, Game),
    [directed(true)]
  ).

export_game(Out, Transitions) :-
  write(Transitions),
  maplist(export_transition(Out), Transitions).

export_transition(Out, Transition) :-
  transition(From, Actions, To) = Transition,
  dot_node(Out, From),
  dot_node(Out, To),
  dot_arc(Out, From, To, [label(Actions)]).

