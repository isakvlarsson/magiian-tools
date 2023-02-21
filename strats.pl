list_agents(Agents) :-
  findall(A, agent(A), Agents).

list_observations(Observations) :-
  findall((Agt, Obs), observation(Agt, Obs), Observations).

list_locations(Loc) :-
  findall(L, location(L), Loc).

observations_powerset_member(Observations) :-
  list_agents(Agents), 
  maplist(observation, Agents, Observations).

observations_powerset(Powerset) :-
  findall(J, observations_powerset_member(J), Powerset).

% a joint observation need to have common elements
% or more formally, the intersection cannot be
% the empty set
intersection_all([], Acc,  I) :- I = Acc.
intersection_all([Observation|Tail], Acc, I) :-
  intersection(Observation, Acc, NewAcc),
  intersection_all(Tail, NewAcc, I).

valid_joint_observation(Observation) :-
  list_locations(Loc),
  intersection_all(Observation, Loc, Intersection),
  length(Intersection, Len), Len =\= 0.


% A joint observation is a combinaition of observations
% for all agents that have an intersection (not the empty set)
joint_observations(Joint) :-
  observations_powerset(Powerset),
  % only save those that have an intersection
  include(valid_joint_observation, Powerset, Joint).


% associate a joint observation with a move
possible_action(FromObs, Action) :-
  % all locations in an observation should have the same
  % possible actions, this simplifies the implementation
  % because we can just get the actions available at on
  % of the locations in the observation
  [[Elem|_]|_] = FromObs,
  transition(Elem, AgentActions, _),
  member(Action, AgentActions).

obs_action(Obs, ObsAct):-
  possible_action(Obs, Act), ObsAct = [Obs, Act].

strategy(Strat) :-
  joint_observations(Joint),
  maplist(obs_action, Joint, Strat).


enumerate_strategies(Strategies) :-
  findall(Strategy, strategy(Strategy), StrategiesNotDistinct),
  % this implementation produces MANY duplicate (needs to be improved)
  % but for now I just remove them
  list_to_set(StrategiesNotDistinct, Strategies).


print_strat([]).
print_strat([Strategy|Rest]) :-
  write("One possible strategy"), nl,
  write(Strategy), nl,nl,
  print_strat(Rest).
