% An MKBSC expansion of a game
% The 0 expansion is a special case
% This is probably not correct yet
create_expanded_game(Game, 0) :-
  unload_expanded_game(Game, 0),
  % add agents as is
  forall(game(Game, agent(A)), (
      wrap_expanded_game(Game, 0, agent(A), WrappedAgents),
      assertz(WrappedAgents)
    )
  ),
  % add locations as singleton lists
  forall((game(Game, location(Location)), singleton(Location, Location0)), (
    wrap_expanded_game(Game, 0, location(Location0), WrappedLocation0),
    assertz(WrappedLocation0)
  )),
  % add initial location as a singleton
  game(Game, initial(Initial)), singleton(Initial, Initial0),
  wrap_expanded_game(Game, 0, initial(Initial0), WrappedInitial0),
  assertz(WrappedInitial0),
  % add actions as is
  forall((game(Game, action(Action)), wrap_expanded_game(Game, 0, action(Action), WrappedAction)),
    assertz(WrappedAction)
  ),
  % add transitions, but make locations to singletons
  forall(
    (game(Game, transition(From, JointActions, To)), 
    singleton(From, From0),
    singleton(To, To0), 
    wrap_expanded_game(Game, 0, transition(From0, JointActions, To0), WrappedTransition)),
    assertz(WrappedTransition)),
  % add observations as is
  forall(
    (game(Game, observation(Agent, Observation)), 
     wrap_expanded_game(Game, 0, observation(Agent, Observation), WrappedObservation)),
    (assertz(WrappedObservation))
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

wrap_expanded_game(Game, Expansion, Term, expanded_game(Game, Expansion, Term)).

%%%% project an expanded game onto a agent
projection(Game, Expansion, Agent, Projection) :-
  % find the index of the Agent
  findall(Agent, expanded_game(Game, Expansion, agent(Agent)), Agents),
  expanded_game(Game, Expansion, agent(Agent)), nth0(Index, Agents, Agent),
  % project all transitions to that player
  findall(transition(From, Action, To), 
          (expanded_game(Game, Expansion, transition(From, JointActions, To)),
           nth0(Index, JointActions, Action)),
          Transitions), 
  Projection = Transitions.

