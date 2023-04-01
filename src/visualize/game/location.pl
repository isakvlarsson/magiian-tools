:- module(visualize_location, [
  locations_to_dot/4,
  agent_locations_to_dot/6
]).

:- use_module(library(gv)).
:- use_module(dot).

% ############### multi-agent ####################

locations_to_dot(Out, Game, Expansion, Mode) :-
  (
    Mode = standard ->
      locations_to_dot_standard(Out, Game, Expansion), !;
    Mode = cutoff(N) ->
      locations_to_dot_numbered(Out, Game, Expansion, N), !;
    Mode = actual ->
      locations_to_dot_actual(Out, Game, Expansion)
  ).


%% view locaitons with their full name
% this quickly gets unfeasible to use
locations_to_dot_standard(Out, Game, Expansion) :-
  forall(
    game(Game, Expansion, location(Location)), 
    (
      unfold_location_pointer(Game, Name, Location),
      dot_node_id(Out, Location, [label(Name)])
    )
  ).

%% View locations with names according to the
% expansion that they were 'created' in (see location_creation_number/4)
% It takes a third argument Cutoff that 
% makes it so that the nodes up to this expansion
% are given the name they had in the first expansion
% they occured and the rest are numbered
locations_to_dot_numbered(Out, Game, Expansion, Cutoff) :-
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

%% View locations with the name of their actual location
% in the original game.
locations_to_dot_actual(Out, Game, Expansion) :-
  forall(
    game(Game, Expansion, location(Location)), 
    (
      actual_location(Game, Location, Name),
      dot_node_id(Out, Location, [label(Name)])
    )
  ).


% ############## single-agent / projection ############

agent_locations_to_dot(Out, G, K, Agt, 0, Mode) :-
  forall(
    projection(G, K, Agt, 0, location(L)),
    (
      unfold_location_pointer(G, Name, L),
      my_dot_node_id(Out, L, Name)
    )
  ),
  !.

agent_locations_to_dot(Out, G, K, Agt, 1, Mode) :-
  forall(
    projection(G, K, Agt, 1, location(L)),
    my_dot_node(Out, L)
  ).

