:- use_module(library(gv)).
%% ascii_id gives a unique id to a ascii string
% this id is used as the id for the node by the
% gv library!
:- use_module(library(term_ext)).

%% view a game 
view_game(Game, Expansion) :-
  gv_view(
    {Game, Expansion}/[Out0]>>game_to_dot(Out0, Game, Expansion),
    [directed(true), method(dot)]
  ).

export_game(Game, Expansion) :-
  gv_export(
    'game.png',
    {Game}/[Out0]>>game_to_dot(Out0, Game, Expansion),
    [directed(true)]
  ).

game_to_dot(Out, Game, Expansion) :-
  locations_to_dot(Out, Game, Expansion),
  transitions_to_dot(Out, Game, Expansion),
  observations_to_dot(Out, Game, Expansion).

locations_to_dot(Out, Game, Expansion) :-
  forall(
    game(Game, Expansion, location(Location)), 
    (
      dot_node(Out, Location)
    )
  ).

transitions_to_dot(Out, Game, Expansion) :-
  forall(game(Game, Expansion, location(From)), (
    forall(game(Game, Expansion, location(To)), (
      % we only want to display transitions that exist
      game(Game, Expansion, transition(From, _, To)),
      findall(JointAction, game(Game, Expansion, transition(From, JointAction, To)), JointActions),
      dot_arc(Out, From, To, [label(JointActions)]);
      % when there is no transition between two locations
      % we write nothing to the stream
      format(Out, '', [])
      )
    )
  )
).

observations_to_dot(Out, Game, Expansion) :-
  forall(
    game(Game, agent(Agent)),
    (
      forall(
        game(Game, Expansion, observation(Agent, Observation)),
        (
          % if there are more than one location in an observation
          Observation \== [_],
          % link it to all the other observaitons
          forall(
            (
              member(L1, Observation),
              member(L2, Observation),
              L1 \== L2
            ),
            (
              dot_observation_arc(Out, Game, L1, L2, Agent)
            )
          )
        )
      )
    )
  ).

dot_observation_arc(Out, Game, From, To, Agent) :-
  ascii_id(From, FromId),
  ascii_id(To, ToId),
  agent_color(Game, Agent, Color),
  format(Out, '~a -> ~a [color="~a", dir="none", style="dashed", label="~~ ~a"];\n', [FromId, ToId, Color, Agent]).


%% specifies the color of the agent's observations
agents_colors(['red', 'blue', 'green']).
agent_color(Game, Agent, Color) :-
  agent_index(Game, Agent, Index),
  agents_colors(Colors),
  nth0(Index, Colors, Color).
