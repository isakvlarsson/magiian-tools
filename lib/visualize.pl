:- use_module(library(gv)).
%% ascii_id gives a unique id to a ascii string
% this id is used as the id for the node by the
% gv library!
:- use_module(library(term_ext)).

%% view a game 
view_game(Game) :-
  gv_view(
    {Transitions}/[Out0]>>game_to_dot(Out0, Game),
    [directed(true), method(dot)]
  ).

export_game(Game) :-
  gv_export(
    'game.png',
    {Game}/[Out0]>>game_to_dot(Out0, Game),
    [directed(true)]
  ).

game_to_dot(Out, Game) :-
  locations_to_dot(Out, Game),
  transitions_to_dot(Out, Game),
  observations_to_dot(Out, Game).

locations_to_dot(Out, Game) :-
  forall(game(Game, location(Location)), 
         dot_node(Out, Location)).

transitions_to_dot(Out, Game) :-
  forall(game(Game, location(From)), (
    forall(game(Game, location(To)), (
      % we only want to display transitions that exist
      game(Game, transition(From, _, To)),
      findall(JointAction, game(Game, transition(From, JointAction, To)), JointActions),
      dot_arc(Out, From, To, [label(JointActions)]);
      % when there is no transition between two locations
      % we write nothing to the stream
      format(Out, '', [])
      )
    )
  )
).

observations_to_dot(Out, Game) :-
  forall(game(Game, agent(Agent)), (
  forall(game(Game, observation(Agent, Observation)), (
  % if there are more than one location in an observation
  [A, B] = Observation, my_dot_arc(Out, A, B, Agent);
  [A] = Observation, dot_node(Out, A)
)))).

my_dot_arc(Out, From, To, Agent) :-
  ascii_id(From, FromId),
  ascii_id(To, ToId),
  format(Out, '~a -> ~a [color="red", dir="none", label="~~ ~a"];\n', [FromId, ToId, Agent]).


%% view an expanded game
view_expanded_game(Game, Expansion) :-
  gv_view(
    {Transitions}/[Out0]>>expanded_game_to_dot(Out0, Game, Expansion),
    [directed(true), method(dot)]
  ).

expanded_game_to_dot(Out, Game, Expansion) :-
  locations_to_dot(Out, Game, Expansion),
  transitions_to_dot(Out, Game, Expansion),
  observations_to_dot(Out, Game, Expansion).

locations_to_dot(Out, Game, Expansion) :-
  forall(expanded_game(Game, Expansion, location(Location)), 
         dot_node(Out, Location)).

transitions_to_dot(Out, Game, Expansion) :-
  forall(expanded_game(Game, Expansion, location(From)), (
    forall(expanded_game(Game, Expansion, location(To)), (
      % we only want to display transitions that exist
      expanded_game(Game, Expansion, transition(From, _, To)),
      findall(JointAction, expanded_game(Game, Expansion, transition(From, JointAction, To)), JointActions),
      dot_arc(Out, From, To, [label(JointActions)]);
      % when there is no transition between two locations
      % we write nothing to the stream
      format(Out, '', [])
      )
    )
  )
).

observations_to_dot(Out, Game, Expansion) :-
  forall(expanded_game(Game, Expansion, agent(Agent)), (
  forall(expanded_game(Game, Expansion, observation(Agent, Observation)), (
  % if there are more than one location in an observation
  [A, B] = Observation, my_dot_arc(Out, A, B, Agent);
  % for now, write nothing
  format(Out, '', [])
)))).


