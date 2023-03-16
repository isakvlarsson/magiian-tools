/*
 * ####################### memoryless strategy ###########################
 * A memoryless strategy is a mapping from observations to actions for each
 * agent. This means that the agent must take the same action in every location
 * that is in the same observation. For the individual agent's strategies we 
 * represent a strategy as a list of pairs between observations and actions.
 *
 * A strategy for the 'whole' multi-agent game is a mapping between
 * locations and actionprofiles. We use the association list from the
 * swipl standard library to represent the strategies int the multi-agent
 * game to make querying fast.
 * (https://eu.swi-prolog.org/pldoc/man?section=assoc)
 * */

% finds a strategy profile for all locations in the game
memoryless_strategy(Game, Expansion, Strategy) :-
  game(Game, agents(Agents)),
  maplist(agent_memoryless_strategy(Game, Expansion), Agents, AgentStrategies),
  findall(Location, game(Game, Expansion, location(Location)), Locations),
  maplist(location_action_profile(Game, Expansion, AgentStrategies), Locations, ActionProfiles),
  zip_pair(Locations, ActionProfiles, KeyVals), 
  list_to_assoc(KeyVals, Strategy).

  
  
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


