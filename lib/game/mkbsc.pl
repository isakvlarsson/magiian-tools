/*
 * ####################### MKBSC ##############################
 * These predicates handle expansion of a game to a higher order
 * of knowledge with the MKBSC algorithm.
 *
 * The MKBSC algorithm works as folows:
 * 1. project the game to all agents and run the
 *    KBSC algorithm on these individual games.
 * 2. Combine the individual expansions into a multilayer
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
post(Game, Expansion, S1, JointAction, S2) :-
  setofall(
    S2member,
    (
      member(S1member, S1),
      game(Game, Expansion, transition(S1member, JointAction, S2member))
    ),
    S2
  ).


neighbor_transitions(Game, Expansion, JointKnowledge, T) :-
  % we first generate all possible transitions
  intersection_all(JointKnowledge, CommonKnowledge),
  setofall(
    Transition,
    (
      % we want to do this for all actions (that exist in the game)
      joint_action(Game, JointAction),
      % generete s
      post(Game, Expansion, CommonKnowledge, JointAction, S), S \== [],
      % generate s_i for all agents
      findall(Agent, game(Game, Expansion, agent(Agent)), Agents),
      findall(
        transition(JointKnowledge, JointAction, I),
        (
          maplist(projection_expansion(Game, Expansion), Agents, JointKnowledge, JointAction, K),
          maplist(intersection(S), K, I),
          \+intersection_all(I, [])
        ),
        Transition
      )
    ),
    Transitions
  ),
  flatten(Transitions, T).

% this is a helper to allow us to work with this predicate with maplist
projection_expansion(Game, Expansion, Agent, S1, Action, S2) :-
  projection_expansion(Game, Expansion, Agent, transition(S1, Action, S2)).

% these are helper predicates to allow us to get all possible jointactions
agent_action(Game, _, Action) :- game(Game, action(Action)).
joint_action(Game, JointAction) :-
  findall(Agent, game(Game, agent(Agent)), Agents),
  maplist(agent_action(Game), Agents, JointAction).




%% The synchronous product combines single-agent games into a multi-agent game
% by essentially playing them concurently and only saving the transitions that
% make sense from the multi-agent perspective (the agents should have overlapping
% knowledge etc.)
synchronous_product(Game, Expansion) :-
  NextExpansion is Expansion + 1,
  % add the initial state
  findall(I, projection_expansion(Game, Expansion, _, initial(I)), Initial),
  assertz(game(Game, NextExpansion, initial(Initial))),
  assertz(game(Game, NextExpansion, location(Initial))),
  synchronous_product(Game, Expansion, [Initial]).
  
synchronous_product(_, _, []) :- !.
synchronous_product(Game, Expansion, [JointKnowledge|Queue]) :-
  neighbor_transitions(Game, Expansion, JointKnowledge, Transitions),
  % add all transitions
  NextExpansion is Expansion + 1,
  forall(
    member(Transition, Transitions),
    assertz(game(Game, NextExpansion, Transition))
  ),
  % add the unvisited locations to the queue
  setofall(
    K2,
    (
      member(transition(JointKnowledge, JointAction, K2), Transitions),
      \+game(Game, NextExpansion, location(K2)),
      % save the location
      assertz(game(Game, NextExpansion, location(K2)))
    ),
    Unvisited
  ),
  append(Unvisited, Queue, NewQueue),
  synchronous_product(Game, Expansion, NewQueue).


%% create an expanded game with mkbsc
create_expanded_game(Game, Expansion) :-
  unload_expanded_game(Game, Expansion),
  PreviousExpansion is Expansion - 1,
  forall(
    game(Game, agent(Agent)),
    create_projection(Game, PreviousExpansion, Agent)
  ),
  synchronous_product(Game, PreviousExpansion),
  % create the observation partitioning
  forall(
    game(Game, agent(Agent)),
    (
      setofall(
        Equal,
        (
          game(Game, Expansion, location(Location)), 
          Equal = [Location]
        ),
        Observations
      ),
      forall(
        member(Observation, Observations),
        assertz(game(Game, Expansion, observation(Agent, Observation)))
      )
    )
  ).



unload_expanded_game(Game, Expansion) :-
  retractall(game(Game, Expansion, _)), !;
  true.
 

