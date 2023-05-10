:- module(strategy, 
  [
    empty_strategy/3,
    put_strategy/4,
    get_agents_strategy/3,
    get_agent_strategy/3,
    max_acts/3,
    get_strategy/3
  ]).

:- use_module('../../utils').

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
get_agents_strategy(L, Strategy, Acts) :-
  maplist(get_agent_strategy(L), Strategy, Acts).

%% true if no agent has more than Num
% elements in their acts at Loc
max_acts(Loc, S, Num) :-
  get_agents_strategy(Loc, S, Acts),
  maplist(length, Acts, ActsLen),
  forall(member(X, ActsLen), X =< Num).

%% get the action-profile for a location
get_strategy(Loc, S, Act) :-
  get_agents_strategy(Loc, S, AgentsActs),
  maplist(custom_head, AgentsActs, Act).

custom_head([H|_], H).
custom_head([], noact).

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
  get_assoc(L, LO, O),
  put_assoc(O, OA, [Act|PrevActs], OA1).

%% get the actions taken in a strategy
get_agent_strategy(L, [LO, OA], Acts) :-
  get_assoc(L, LO, O),
  get_assoc(O, OA, Acts).


