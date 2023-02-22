:- [query].
:- use_module(library(gv)).

game(Game) :-
  query_agents(Agents),
  query_locations(Locations),
  query_actions(Actions),
  query_transitions(Transitions),
  query_observations(Observations),
  Game = [Agents, Locations, Actions, Transitions, Observations].

view_game():-
  game(Game),
  gv_view(
    {Game}/[Out0]>>export_game(Out0, Game),
    [directed(true), method(dot)]
  ).

export_game(Out, Game) :-
  [Agents, Locations, Actions, Transitions, Observations] = Game,
  maplist(export_transition(Out), Transitions).

export_transition(Out, Transition) :-
  (From, Arc, To) = Transition,
  dot_node(Out, From),
  dot_node(Out, To),
  dot_arc(Out, From, To, []).

  
