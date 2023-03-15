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
  format(atom(Filename), 'images/~a_K~a.png', [Game, Expansion]),
  gv_export(
    Filename,
    {Game, Expansion}/[Out0]>>game_to_dot(Out0, Game, Expansion),
    [directed(true), method(dot)]
  ).

game_to_dot(Out, Game, Expansion) :-
  locations_to_dot(Out, Game, Expansion),
  transitions_to_dot(Out, Game, Expansion),
  observations_to_dot(Out, Game, Expansion).

locations_to_dot(Out, Game, Expansion) :-
  forall(
    game(Game, Expansion, location(Location)), 
    (
      unfold_location_pointer(Game, Name, Location),
      dot_node_id(Out, Location, [label(Name)])
    )
  ).

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

observations_to_dot(Out, Game, Expansion) :-
  forall(
    game(Game, agent(Agent)),
    (
      forall(
        game(Game, Expansion, observation(Agent, Observation)),
        equivalence_relation_to_dot(Out, Game, Agent, Observation)
      )
    )
  ).


%% writes a dot_observation_arc between all members 
% of the observation
equivalence_relation_to_dot(Out, _, _, []) :-
  format(Out, '', []).
equivalence_relation_to_dot(Out, Game, Agent, [H|T]) :-
  forall(
    member(Obs, T),
    dot_observation_arc_id(Out, Game, H, Obs, Agent)
  ),
  equivalence_relation_to_dot(Out, Game, Agent, T).



%% Output an observation relation between two nodes
% to the dot stream
dot_observation_arc(Out, Game, From, To, Agent) :-
  ascii_id(From, FromId),
  ascii_id(To, ToId),
  agent_color(Game, Agent, Color),
  format(Out, '~a -> ~a [color="~a", dir="none", style="dashed", label="~~ ~a"];\n', [FromId, ToId, Color, Agent]).


%% same as dot observation_arc/5 but one supplies their
% own ids for the nodes involved
dot_observation_arc_id(Out, Game, FromId, ToId, Agent) :-
  agent_color(Game, Agent, Color),
  format(Out, '~a -> ~a [color="~a", dir="none", style="dashed", label="~~ ~a"];\n', [FromId, ToId, Color, Agent]).

%% specifies the color of the agent's observations
agents_colors(['red', 'blue', 'green']).
agent_color(Game, Agent, Color) :-
  agent_index(Game, Agent, Index),
  agents_colors(Colors),
  nth0(Index, Colors, Color).
