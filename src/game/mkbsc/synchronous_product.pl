:- module(synchronous_product, [
  create_expanded_game/2,
  unload_expanded_game/2
]).
:- use_module('../parse').
:- use_module(kbsc).
:- use_module(location_pointer).

/* ####################### MKBSC ##############################
 * These predicates handle expansion of a game to a higher order
 * of knowledge with the MKBSC algorithm.
 *
 * The MKBSC algorithm works as follows:
 * 1. project the game to all agents and run the
 *    KBSC algorithm on these individual games.
 * 2. Combine the individual expansions into a multiagent
 *    game with the "synchronous product" (basically playing
 *    the games concurently)
 * 3. The observation-partitioning in the expanded game is defined
 *    so that if the individual agent's knowledge is the same in
 *    multiple joint-knowledge states, then that agent cannot distinguish 
 *    between these joint-knowledge states.
 *
 * A good description of the algorithm and its implementation is given 
 * here: https://kth.diva-portal.org/smash/get/diva2:1221520/FULLTEXT01.pdf
 *
 * An important optimization is to not copy the names when creating the locations
 * in the expanded games, and instead store them with 'location pointers' (see
 * location_pointer.pl). Otherwise the name of each location would grow exponentially.
 * */


%% get the transitions from a location in an expanded game
transitions_in_expansion_from(Game, Expansion, JointKnowledge, Transitions) :-
  setofall(
    transition(JointKnowledge, Act, K),
    (
      game(Game, agents(Agents)),
      maplist(projection(Game, Expansion), Agents, JointKnowledge, Act, K),
      \+intersection_all(K, [])
    ),
    Transitions
  ).


% this is a helper to allow us to work with this predicate with maplist
projection(Game, Expansion, Agent, S1, Action, S2) :-
  projection(Game, Expansion, Agent, 1, transition(S1, Action, S2)).


%% The synchronous product combines single-agent games into a multi-agent game
% by playing them concurently
synchronous_product(Game, Expansion) :-
  NextExpansion is Expansion + 1,
  % add the initial state
  findall(I, projection(Game, Expansion, _, 1, initial(I)), Initial),
  create_location_pointer(Game, Initial, InitialPointer),
  assertz(game(Game, NextExpansion, initial(InitialPointer))),
  assertz(game(Game, NextExpansion, location(InitialPointer))),
  synchronous_product(Game, Expansion, [Initial]).
  
synchronous_product(_, _, []) :- !.
synchronous_product(Game, Expansion, [JointKnowledge|Queue]) :-
  transitions_in_expansion_from(Game, Expansion, JointKnowledge, Transitions),
  % add all transitions
  NextExpansion is Expansion + 1,
  forall(
    member(transition(F, Act, T), Transitions),
    (
      create_location_pointer(Game, F, FP),
      create_location_pointer(Game, T, TP),
      assertz(game(Game, NextExpansion, transition(FP, Act, TP)))
    )
  ),
  % add the unvisited locations to the queue
  setofall(
    K2,
    (
      member(transition(JointKnowledge, _JointAction, K2), Transitions),
      location_pointer(Game, K2, K2P),
      \+game(Game, NextExpansion, location(K2P)),
      % save the location (-pointer)
      assertz(game(Game, NextExpansion, location(K2P)))
    ),
    Unvisited
  ),
  append(Unvisited, Queue, NewQueue),
  synchronous_product(Game, Expansion, NewQueue).


%% Find all observations of an agent in a game
% using the definition of what the observation
% partitioning should be in the expanded game.
% (The games locations needs to be calculated first)
agent_observations(Game, Expansion, Agent, Observations) :-
  setofall(
    Obs,
    (
      game(Game, Expansion, location(JointKnowledgeP)),
      location_pointer(Game, JointKnowledge, JointKnowledgeP),
      agent_index(Game, Agent, Index),
      nth0(Index, JointKnowledge, Knowledge),
      setofall(
        AnotherJointKnowledgeP,
        (
          game(Game, Expansion, location(AnotherJointKnowledgeP)),
          location_pointer(Game, AnotherJointKnowledge, AnotherJointKnowledgeP),
          nth0(Index, AnotherJointKnowledge, Knowledge)
        ),
        Obs
      )
    ),
    Observations
  ).


%% create an expanded game with mkbsc
create_expanded_game(_, 0) :- !.
create_expanded_game(Game, Expansion) :-
  unload_expanded_game(Game, Expansion),
  PreviousExpansion is Expansion - 1,
  % make sure the previous expansion is loaded or load it
  (
    loaded(Game, PreviousExpansion), !;
    create_expanded_game(Game, PreviousExpansion)
  ),
  forall(
    game(Game, agent(Agent)),
    (
      create_projection(Game, PreviousExpansion, Agent),
      create_expanded_projection(Game, PreviousExpansion, Agent)
    )
  ),
  synchronous_product(Game, PreviousExpansion),
  % create the observation partitioning
  forall(
    game(Game, agent(Agent)),
    (
      forall(
        (
          agent_observations(Game, Expansion, Agent, Observations),
          member(Observation, Observations)
        ),
        (
          assertz(game(Game, Expansion, observation(Agent, Observation)))
        )
      )
    )
  ),
  assertz(loaded(Game, Expansion)).


unload_expanded_game(Game, Expansion) :-
  Expansion > 0,
  retractall(game(Game, Expansion, _)),
  retract(loaded(Game, Expansion)), !;
  true.
 

