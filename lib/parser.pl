/*
  This file takes care of reading games, modifying them slightly (adding name etc)
  and storing them in the prolog database

  ## structure of a gamefile
  A game file `Game.pl` should look like this:
  ```prolog
  % Agents
  agent(a1).
  agent(a2).
  % ...

  % Locations
  location(start).
  location(left).
  % ...
  
  % Initial location
  initial(start).

  % Actions
  action(init).
  action(push).
  % ...

  % Transitions
  transitions(start, [init, init], left).
  % ...

  % Observations
  observation(a1, [start]).
  observation(a2, [middle, right]).
  % ...
  ```

  ## Added information
  When parsing we add the terms `T` to the prolog database as
  `game(Game, T)` so that each term is associated to a certain
  game. `Game` is the name of the file without its `.pl` 
  extensiton.
*/

% Load a game into the database
load_game(Game) :-
  unload_game(Game),
  atom_concat('games/', Game, F0), atom_concat(F0, '.pl', Filename),
  see(Filename), 
  repeat, 
  read(Term),
  (
    Term == end_of_file -> true, !;
    wrapped(Game, Term, Wrapped),
    assertz(Wrapped),
    fail
  ),
  assertz(loaded(Game)),
  seen.

% unloads the loaded game
% is allways true
unload_game(Game) :-
  retractall(game(Game, _)),
  retract(loaded(Game)), !;
  true.

% wraps a term in a game(Game, Term)
% so that we know what game we are working
% on
wrapped(Game, Term, Wrapped) :-
  Wrapped = game(Game, Term).

