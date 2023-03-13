/*
 * ############# Location pointers #########################
 * */
/*
 * The names of the locations/knowledge-states in the expanded games grows
 * exponentially. The locations should instead be stored 
 * as pointers.
 *
 * These pointers are keys in a key value store
 * of the locations/knowledge-states, and for now
 * they are generated with the `ascii_id/2` predicate
 * which generates a hash for a term. These are not
 * guaranteed to be unique but the chance for an overlap
 * is probably microscopical.
 * */

%% store the id of a location with its real name
% only stores a pointer once
:- dynamic location_pointer/3.
create_location_pointer(Game, Location, Pointer) :-
  location_pointer(Game, Location, Pointer), !;
  ascii_id(Location, Pointer),
  assert(location_pointer(Game, Location, Pointer)).


%% extracts the real name of a knowledge-state
% used for visualization purposes
unfold_location_pointer(Game, Location, Pointer) :-
  \+is_list(Pointer),
  \+location_pointer(Game, _, Pointer),
  Location = Pointer, !;
  location_pointer(Game, List, Pointer),
  maplist(unfold_location_pointer(Game), Location, List), !.
unfold_location_pointer(Game, Location, Pointers) :-
  maplist(unfold_location_pointer(Game), Location, Pointers), !.

/*
 * ####################### MKBSC ##############################
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
 * */


%% the post function for a multi-agent game
% it gives all locations that can be reached
% by takin a JointAction in a list of locations
post(Game, Expansion, S1, JointAction, S2) :-
  setofall(
    S2member,
    (
      member(S1member, S1),
      game(Game, Expansion, transition(S1member, JointAction, S2member))
    ),
    S2
  ).


%% generates the transitions in a game that goes out from
% a knowledge-state
transitions_in_expansion_from(Game, Expansion, JointKnowledge, T) :-
  % we first generate all possible transitions
  intersection_all(JointKnowledge, CommonKnowledge),
  setofall(
    ActionTransitions,
    (
      % we want to do this for all actions (that exist in the game)
      joint_action(Game, JointAction),
      % generete s
      post(Game, Expansion, CommonKnowledge, JointAction, S), S \== [],
      % generate s_i for all agents
      findall(Agent, game(Game, agent(Agent)), Agents),
      findall(
        transition(JointKnowledge, JointAction, K),
        (
          maplist(projection_expansion(Game, Expansion), Agents, JointKnowledge, JointAction, K),
          maplist(intersection(S), K, I),
          \+intersection_all(I, [])
        ),
        ActionTransitions
      )
    ),
    Transitions
  ),
  flatten(Transitions, T).

% this is a helper to allow us to work with this predicate with maplist
projection_expansion(Game, Expansion, Agent, S1, Action, S2) :-
  projection_expansion(Game, Expansion, Agent, transition(S1, Action, S2)).



%% The synchronous product combines single-agent games into a multi-agent game
% by essentially playing them concurently and only saving the transitions that
% make sense from the multi-agent perspective (the agents should have overlapping
% knowledge etc.)
synchronous_product(Game, Expansion) :-
  NextExpansion is Expansion + 1,
  % add the initial state
  findall(I, projection_expansion(Game, Expansion, _, initial(I)), Initial),
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
      create_projection_expansion(Game, PreviousExpansion, Agent)
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
  retractall(game(Game, Expansion, _)),
  retract(loaded(Game, Expansion)), !;
  true.
 

