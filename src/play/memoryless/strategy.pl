:- module(memoryless_strategy, [
  memoryless_strategy/3,
  get_memoryless_strategy/3
]).
/* ####################### memoryless strategy ###########################
 * A memoryless strategy is a mapping from observations to actions for each
 * agent. This means that the agent must take the same action in every location
 * that is in the same observation. We represent the agent's stategies with
 * an association-list with observations as keys and actions as values.
 *
 * A strategy for the 'whole' multi-agent game is a mapping between
 * locations and actionprofiles. We use the association list from the
 * swipl standard library to represent the strategies int the multi-agent
 * game to make querying fast.
 * (https://eu.swi-prolog.org/pldoc/man?section=assoc)
 * */
  
%% Construct a memoryless strategy
memoryless_strategy(G, K, Strategy) :-
  game(G, agents(Agents)),
  maplist(memoryless_agent_strategy(G, K), Agents, Strategy).

%% get the actions taken by the agents
% on this location (the individual
% agents may have taken their action on
% som other location in their observaiton)
get_memoryless_strategy(L, Strategy, Acts) :-
  maplist(get_agent_strategy(L), Strategy, Acts).

/* Predicates for the individual strategies.
 * */
memoryless_agent_strategy(G, K, Agt, [LObAssoc, ObsActAssoc]) :-
  % map locations to observations
  findall(L, game(G, K, location(L)), Ls),
  maplist(location_to_observation(G, K, Agt), Ls, LOb),
  list_to_assoc(LOb, LObAssoc),
  % map observations to actions
  findall(Ob, game(G, K, observation(Agt, Ob)), Obs), !,
  maplist(agent_action_at_observation(G, K, Agt), Obs, ObsAct),
  list_to_assoc(ObsAct, ObsActAssoc).

location_to_observation(G, K, Agt, L, L-Obs) :-
  game(G, K, observation(Agt, Obs)),
  memberchk(L, Obs).

%% get the actions taken in a strategy
get_agent_strategy(L, [LO, OA], Acts) :-
  get_assoc(L, LO, O),
  get_assoc(O, OA, Acts).

%% Get an action at an observation
% uses the fact that all locations
% in an observation has the same 
% actions
agent_action_at_observation(G, K, Agt, Obs, Obs-Act) :-
  Obs = [L|_],
  setofall(
    A,
    (
      game(G, K, transition(L, AP, _)),
      agent_index(G, Agt, Index),
      nth0(Index, AP, A)
    ),
    APs
  ),
  member(Act, APs).
