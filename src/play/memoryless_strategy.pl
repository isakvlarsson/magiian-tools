/*
 * ####################### memoryless strategy ###########################
 * A memoryless strategy is a mapping from observations to actions
 * We use the pair construct in prolog for this. It is weird but works.
 * The memoryless strategy is represented with a list pairs for between
 * locations and action profiles.
 *
 * For the individual agent's strategies we represent a strategy as a
 * list of pairs between observations and actions
 * */

% finds a strategy profile for all locations in the game
memoryless_strategy(Game, Expansion, Strategy) :-
  game(Game, agents(Agents)),
  maplist(agent_memoryless_strategy(Game, Expansion), Agents, AgentStrategies),
  findall(Location, game(Game, Expansion, location(Location)), Locations),
  maplist(location_action_profile(Game, Expansion, AgentStrategies), Locations, ActionProfiles),
  zip_pair(Locations, ActionProfiles, Strategy).

  
  
%% converts a list of agent observation based strategies to 
% a location-actionprofile pair for each location in the game
location_action_profile(Game, Expansion, AgentStrategies, Location, ActionProfile) :-
  maplist(agent_location_action(Game, Expansion, Location), AgentStrategies, ActionProfile).

% helper for maplist in the above predicate
agent_location_action(Game, Expansion, Location, Strategy, Action) :-
  member(Obs-Action, Strategy), 
  member(Location, Obs). 

%% finds a memoryless strategy for an individual agent
agent_memoryless_strategy(Game, Expansion, Agent, Strategy) :-
  findall(Observation, game(Game, Expansion, observation(Agent, Observation)), Observations),
  maplist(action_at_observation(Game, Expansion, Agent), Observations, Actions),
  zip_pair(Observations, Actions, Strategy).
  

%% Get all actions available at an observation for an agent
% Here I assume that all locations in an observation have
% the same actions avilable for each of those locations
% (as they should).
action_at_observation(Game, Expansion, Agent, [Location|_], Action) :-
  setofall(JointAction, game(Game, Expansion, transition(Location, JointAction, _)), JointActions),
  agent_index(Game, Agent, Index),
  maplist(nth0(Index), JointActions, Actions),
  sort(Actions, UniqueActions),
  member(Action, UniqueActions).


