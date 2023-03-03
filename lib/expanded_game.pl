% An MKBSC expansion of a game
% The 0 expansion is a special case
% This is probably not correct yet
create_expanded_game(Game, 0) :-
  unload_expanded_game(Game, 0),
  % add the knowledge states
  forall(
    game(Game, location(Location)),
    (
      game_knowledge_state(Game, Location, KnowledgeState),
      assertz(expanded_game(Game, 0, knowledge_state(KnowledgeState)))
    )
  ),
  % add initial knowledge state
  game(Game, initial(Initial)),
  game_knowledge_state(Game, Initial, KnowledgeState),
  assertz(expanded_game(Game, 0, initial_knowledge_state(KnowledgeState))),
  % add transitions, but make locations to singletons
  forall(
    game(Game, transition(From, JointActions, To)), 
    (
      game_knowledge_state(Game, From, FromKS),
      game_knowledge_state(Game, To, ToKS),
      assertz(expanded_game(Game, 0, transition(FromKS, JointActions, ToKS)))
    )
  ),
  % add observations as is
  forall(
    game(Game, observation(Agent, Observation)), 
    (
      maplist(game_knowledge_state(Game, Agent), Observation, KSObservation),
      assertz(expanded_game(Game, 0, observation(Agent, KSObservation)))
    )
  ),
  % set this expansion as loaded
  assertz(loaded(Game, 0)), !.
create_expanded_game(Game, Expansion) :-
  Expansion > 0,
  writeln('not implemented').

% unloads a game
% is allways true
unload_expanded_game(Game, Expansion) :-
  retractall(expanded_game(Game, Expansion, _)),
  retract(loaded(Game, Expansion)), !;
  true.

%%%% create a projection of an expanded game onto an agent
create_expanded_game_projection(Game, Expansion, Agent) :-
  agent_index(Game, Agent, Index),
  % the agent should only see its own action for the transitions
  forall(
    expanded_game(Game, Expansion, transition(From, JointActions, To))
  )
  % it should only see it's own knowledge state
  % it should only see it's own observation partitionings
  findall(
    transition(From, Action, To), 
    (
      expanded_game(Game, Expansion, transition(From, JointActions, To)),
      nth0(Index, JointActions, Action)
    ),
    Transitions
  ), 
  forall(
    member(Transition, Transitions),
    assertz(expanded_game_projection(Game, Expansion, Agent, Transition))
  ).

