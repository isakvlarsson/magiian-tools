:- module(visualize_observation, [
  observations_to_dot/3,
  agent_observations_to_dot/4
]).

:- use_module(library(gv)).
:- use_module(dot).

observations_to_dot(Out, G, K) :-
  forall(
    game(G, agent(Agt)),
    agent_observations_to_dot(Out, G, K, Agt)
  ).

agent_observations_to_dot(Out, G, K, Agt) :-
  forall(
    game(G, K, observation(Agt, Observation)),
    equivalence_relation_to_dot(Out, G, Agt, Observation)
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


