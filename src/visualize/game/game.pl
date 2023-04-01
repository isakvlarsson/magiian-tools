:- module(visualize_game, [
  view_game/2,
  view_game/3,
  export_game/2,
  export_game/3
]).
:- use_module(library(gv)).
%% ascii_id gives a unique id to a ascii string
% this id is used as the id for the node by the
% gv library!
:- use_module(library(term_ext)).
:- use_module(location).
:- use_module(observation).
:- use_module(dot).

%% view a game 
view_game(Game, Expansion) :-
  view_game(Game, Expansion, standard).
view_game(Game, Expansion, Mode) :-
  gv_view(
    {Game, Expansion, Mode}/[Out]>>game_to_dot(Out, Game, Expansion, Mode),
    [directed(true), method(dot)]
  ).

export_game(Game, Expansion) :-
  export_game(Game, Expansion, standard).
export_game(Game, Expansion, Mode) :-
  format(atom(Filename), 'images/~a_K~a.png', [Game, Expansion]),
  gv_export(
    Filename,
    {Game, Expansion, Mode}/[Out]>>game_to_dot(Out, Game, Expansion, Mode),
    [directed(true), method(dot)]
  ).

game_to_dot(Out, Game, Expansion, Mode) :-
  locations_to_dot(Out, Game, Expansion, Mode),
  transitions_to_dot(Out, Game, Expansion),
  observations_to_dot(Out, Game, Expansion).

transitions_to_dot(Out, Game, Expansion) :-
  forall(game(Game, Expansion, location(From)), (
    forall(game(Game, Expansion, location(To)), (
      % we only want to display transitions that exist
      game(Game, Expansion, transition(From, _, To)),
      findall(JointAction, game(Game, Expansion, transition(From, JointAction, To)), JointActions),
      dot_arc_id(Out, From, To, [label(JointActions)]);
      % when there is no transition between two locations
      % we write nothing to the stream
      format(Out, '', [])
      )
    )
  )
).
