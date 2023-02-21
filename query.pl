% helpers
query_agents(Agents) :-
  findall(Agt, agent(Agt), Agents).

query_agent_index(Agent, Index) :-
  query_agents(Agents),
  nth0(Index, Agents, Agent), !.

query_agent_observations(Agent, Observations) :-
  findall(Obs, observation(Agent, Obs), Observations).

query_agent_observation(Agent, Observation) :-
  query_agent_observations(Agent, Observations),
  member(Observation, Observations).

query_agent_observation_actions(Agent, Observation, Actions) :-
  observation(Agent, Observation),
  % uses the fact that all locations in an observation should have the
  % same actions available
  [Location|_] = Observation,
  query_agent_location_actions(Agent, Location, Actions), !.

query_agent_observation_action(Agent, Observation, Action) :-
  query_agent_observation_actions(Agent, Observation, Actions), 
  member(Action, Actions).

% this gives all actions of a single agent at a location
query_agent_location_actions(Agent, Location, Actions) :-
  query_agent_index(Agent, Index),
  query_location_all_actions(Location, AllActions),
  maplist(nth0(Index), AllActions, ActionsWithDups),
  list_to_set(ActionsWithDups, Actions).

% this gives all agent's actions att this location
query_location_all_actions(Location, AllActions) :-
  findall(Actions, transition(Location, Actions, _), AllActions).
