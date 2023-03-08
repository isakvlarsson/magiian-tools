% Load a game into the database
load_game(Game) :-
  unload_game(Game),
  atom_concat('games/', Game, F0), atom_concat(F0, '.pl', Filename),
  see(Filename), 
  repeat, 
  read(Term),
  (
    Term == end_of_file -> true, !;
    assertz(game(Game, Term)),
    fail % in order to backtrack to read from here
  ),
  assertz(loaded(Game)),
  seen.

% unloads the loaded game
% is allways true
unload_game(Game) :-
  retractall(game(Game, _)),
  retract(loaded(Game)), !;
  true.

% helper functions for interacting with the game
% 0 indexed
agent_index(Game, Agent, Index) :-
  findall(Agt, game(Game, agent(Agt)), Agents),
  nth0(Index, Agents, Agent), !.

% create the projection of the game of an agent
% This only creates new transitions because the
% rest of the game stays the same.
create_game_projection(Game, Agent) :-
  unload_game_projection(Game, Agent),
  agent_index(Game, Agent, Index),
  forall(
    game(Game, transition(From, JointAction, To)),
    (
      nth0(Index, JointAction, Action),
      assertz(game_projection(Game, Agent, transition(From, Action, To)))
    )
  ).

unload_game_projection(Game, Agent) :-
  retractall(game_projection(Game, Agent, _)), !;
  true.

% Compute the individual and joint knowledge states of a game
game_knowledge_state(Game, Location, KnowledgeState) :-
  findall(
    AgentKnowledgeState,
    game_knowledge_state(Game, Agent, Location, AgentKnowledgeState),
    KnowledgeState
  ).
game_knowledge_state(Game, Agent, Location, KnowledgeState) :-
  game(Game, location(Location)),
  game(Game, observation(Agent, Observation)),
  member(Location, Observation),
  KnowledgeState = Observation.
