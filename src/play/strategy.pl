:- module(strategy, 
  [
    empty_strategy/3,
    put_strategy/4,
    get_strategy/3
  ]).

/*
 * A strategy maps locations to action profiles.
 *
 * It is a composition of individual strategies for
 * the agents. The agents have strategies that map
 * observations to actions. To make the composed
 * strategy simpler we map locations to observations 
 * to actions.
 * */

%% Construct an empty strategy
empty_strategy(G, K, Strats) :-
  game(G, agents(Agents)),
  maplist(empty_agent_strategy(G, K), Agents, Strats),
  !.

%% Create a new strategy with
% this action profile for this locaiton.
put_strategy(L, Strategy0, Act, Strategy1) :-
  maplist(put_agent_strategy(L), Strategy0, Act, Strategy1).

%% get the actions taken by the agents
% on this location (the individual
% agents may have taken their action on
% som other location in their observaiton)
get_strategy(L, Strategy, Acts) :-
  maplist(get_agent_strategy(L), Strategy, Acts).

/*
 * Predicates for the individual strategies.
 * */
empty_agent_strategy(G, K, Agt, [LObAssoc, ObsActAssoc]) :-
  % map locations to observations
  findall(L, game(G, K, location(L)), Ls),
  maplist(location_to_observation(G, K, Agt), Ls, LOb),
  list_to_assoc(LOb, LObAssoc),
  % map observations to actions
  findall(Ob, game(G, K, observation(Agt, Ob)), Obs),
  maplist(pair_default([]), Obs, ObsAct),
  list_to_assoc(ObsAct, ObsActAssoc), 
  !. 

location_to_observation(G, K, Agt, L, L-Obs) :-
  game(G, K, observation(Agt, Obs)),
  memberchk(L, Obs).

%% Associate an action profile with a location
% if the last action taken is the same as
% Act, no update is performed because this
% would be unnecesary
put_agent_strategy(L, Strategy, Act, Strategy) :-
  get_agent_strategy(L, Strategy, [Act|_]).
put_agent_strategy(L, [LO, OA], Act, [LO, OA1]) :-
  get_agent_strategy(L, [LO, OA], PrevActs),
  put_assoc(O, OA, [Act|PrevActs], OA1).

%% get the actions taken in a strategy
get_agent_strategy(L, [LO, OA], Acts) :-
  get_assoc(L, LO, O),
  get_assoc(O, OA, Acts).
