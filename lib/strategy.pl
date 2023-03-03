
% generate all possible strategies on the form [location-[agent1_action, agent2_action, ...], ...]
global_strategy(Strategy) :-
  query_agents(Agents), 
  maplist(agent_strategy, Agents, StrategySeparate),
  flatten(StrategySeparate, FlatStrategySeparate),
  % It's very important that this sorting algorithm is stable
  % (It says that it is in the documentation)
  % This code assumes that the actions are laid out
  % in the same order as the corresponding agent
  keysort(FlatStrategySeparate, SortedFlatStrategySeparate),
  group_pairs_by_key(SortedFlatStrategySeparate, Strategy).

% I make some assumptions here that the ordering of locations will
% remain and be equal for all agents. May need to be changed later.
global_strategies(Strategies) :-
  findall(Strategy, global_strategy(Strategy), Strategies).

% A strategy for one agent is a mapping between it's observations and the possible actions
% at those observations (every location in an observation should have the same actions
% available)
% This gives every possible strategy for one agent
agent_strategy(Agent, Strategy) :-
  query_agent_observations(Agent, Observations),
  maplist(query_agent_observation_action(Agent), Observations, Actions),
  % convert to (Locations Action)
  maplist(pair_construct, Observations, Actions, ObservationPairs),
  % convert to [(Location Action), ...]
  maplist(obs_pair_to_loc_pairs, ObservationPairs, NestedLocationPairs),
  flatten(NestedLocationPairs, Strategy).

% a structure to hold mappings between Observations and actions
% there is a strange prolog structure called a pair, that can
% be used for key-value lists. It is perfect here. The syntax is
% `A-B` meaning Key=A and Value = B.
pair_construct(A, B, A-B).
reverse_pair_construct(B, A, A-B).
% convert one observation pair into a list of location
obs_pair_to_loc_pairs(ObsPair, LocPairs) :-
  Locations-Action = ObsPair,
  maplist(reverse_pair_construct(Action), Locations, LocPairs).

