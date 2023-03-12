:- use_module(library(term_ext)).
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


/*
 * The names of the locations in the expanded games grows
 * exponentially so we need to give the locations an Id
 * instead.
 * */
%% store the id of a location with its real name
create_location_id(Location, Id) :-
  ascii_id(Location, Id),
  assert(location_id(Location, Id)).

%% get the real name associated with an id
expand_location_id(Id, Knowledge) :-
  location_id(Expanded, Id).






