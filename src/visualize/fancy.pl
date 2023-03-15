:- use_module(library(gv)).
%% ascii_id gives a unique id to a ascii string
% this id is used as the id for the node by the
% gv library!
:- use_module(library(term_ext)).

%% view a game but name the nodes according to the
% expansion that they are 'created' in (see location_creation_number/4)
% It takes a third argument Cutoff that 
% makes it so that the nodes up to this expansion
% are given the name they had in the first expansion
% they occured and the rest are numbered
view_game(Game, Expansion, Cutoff) :-
  gv_view(
    {Game, Expansion, Cutoff}/[Out]>>game_to_dot(Out, Game, Expansion, Cutoff),
    [directed(true), method(dot)]
  ).

export_game(Game, Expansion, Cutoff) :-
  format(atom(Filename), 'images/~a_K~a.png', [Game, Expansion]),
  gv_export(
    Filename,
    {Game, Expansion, Cutoff}/[Out]>>game_to_dot(Out, Game, Expansion, Cutoff),
    [directed(true), method(dot)]
  ).

game_to_dot(Out, Game, Expansion, Cutoff) :-
  locations_to_dot(Out, Game, Expansion, Cutoff),
  transitions_to_dot(Out, Game, Expansion),
  observations_to_dot(Out, Game, Expansion).

locations_to_dot(Out, Game, Expansion, Cutoff) :-
  forall(
    game(Game, Expansion, location(Location)), 
    (
      location_born(Game, Expansion, Location, Number),
      (
        % show the name
        Number =< Cutoff,
        first_location_name(Game, Expansion, Location, Name), !;
        % show the number
        Name = Number
      ),
      dot_node_id(Out, Location, [label(Name)])
    )
  ).
