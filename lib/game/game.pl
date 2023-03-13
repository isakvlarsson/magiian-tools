:- use_module(library(term_ext)).
:- dynamic game/3.
/*
 * ################## Load game ###################
 * */
% Load a game into the database
load_game(Game) :-
  unload_game(Game),
  atom_concat('games/', Game, F0), atom_concat(F0, '.pl', Filename),
  see(Filename), 
  repeat, 
  read(Term),
  (
    Term == end_of_file -> true, !;
    assertz(game(Game, 0, Term)),
    fail % in order to backtrack to read from here
  ),
  assertz(loaded(Game, 0)),
  seen.
  
% unloads the loaded game
% is allways true
% unloads all expansions
unload_game(Game) :-
  retractall(game(Game, _, _)),
  retract(loaded(Game, _)), !;
  true.

/*
 * ####################### Helpers #########################
 * */

 % get the 0-indexed index of an agent in a game
agent_index(Game, Agent, Index) :-
  findall(Agt, game(Game, agent(Agt)), Agents),
  nth0(Index, Agents, Agent), !.

% To refer to the original game without the 0-expansion
game(Game, Term) :-
  game(Game, 0, Term).

% these allow us to get all possible jointactions in a game
agent_action(Game, _, Action) :- game(Game, action(Action)).
joint_action(Game, JointAction) :-
  findall(Agent, game(Game, agent(Agent)), Agents),
  maplist(agent_action(Game), Agents, JointAction).

