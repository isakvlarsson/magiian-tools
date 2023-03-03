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
  findall(Agent, game(Game, Agent), Agents),
  nth0(Index, Agents, Agent).

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
