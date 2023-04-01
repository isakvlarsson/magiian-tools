:- module(game_dot, [
  my_dot_node/2,
  my_dot_node_id/3,
  my_dot_arc/4,
  my_dot_arc_no_id/4,
  dot_observation_arc_id/5
]).

:- use_module(library(term_ext)).

my_dot_node(Out, Name) :-
  ascii_id(Name, Id),
  my_dot_node_id(Out, Id, Name).

my_dot_node_id(Out, Id, Name) :-
  format(Out, '~w [label="~w"];\n', [Id, Name]).

my_dot_arc(Out, From, To, Label) :-
  format(Out, '~w -> ~w [label="~w"];\n', [From, To, Label]).

my_dot_arc_no_id(Out, From, To, Label) :-
  ascii_id(From, FromId),
  ascii_id(To, ToId),
  format(Out, '~w -> ~w [label="~w"];\n', [FromId, ToId, Label]).

%% same as dot observation_arc/5 but one supplies their
% own ids for the nodes involved
dot_observation_arc_id(Out, Game, FromId, ToId, Agent) :-
  agent_color(Game, Agent, Color),
  format(Out, '~w -> ~w [color="~w", dir="none", style="dashed", label="~~ ~w"];\n', [FromId, ToId, Color, Agent]).

%% specifies the color of the agent's observations
agents_colors(['red', 'blue', 'green']).
agent_color(Game, Agent, Color) :-
  agent_index(Game, Agent, Index),
  agents_colors(Colors),
  nth0(Index, Colors, Color).
