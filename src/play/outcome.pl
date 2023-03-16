%% Takes a stap in the game
% based on a memoryless strategy
% Visited is an assoc
step(_, _, _, Visited, Location, goto(Location)) :-
  % We have been here before
  get_assoc(Location, Visited, true), !.
step(Game, Expansion, Strategy, _, Location, fork(Nexts)) :-
  % multiple transitions, non-determinism
  % I call this a fork
  get_assoc(Location, Strategy, ActionProfile),
  findall(N, game(Game, Expansion, transition(Location, ActionProfile, N)), Nexts),
  length(Nexts, Len), 
  Len > 1, !.
step(Game, Expansion, Strategy, _, Location, Next) :-
  get_assoc(Location, Strategy, ActionProfile),
  game(Game, Expansion, transition(Location, ActionProfile, Next)), !.

%% a play of a game
% represented in i finite way
play(Game, Expansion, Strategy, Visited, Location, History, [goto(LoopStart)|History]) :-
  % we came back to the start of a loop
  step(Game, Expansion, Strategy, Visited, Location, goto(LoopStart)), !.
play(Game, Expansion, Strategy, Visited, Location, History, [fork(Forks)|History]) :-
  % there is non-determinism, start multiple new plays from here
  step(Game, Expansion, Strategy, Visited, Location, fork(Nexts)),
  list_to_assoc([Location-true], NewVisited),
  maplist(outcome_from(Game, Expansion, Strategy, NewVisited, [Location]), Nexts, Forks), !.
play(Game, Expansion, Strategy, Visited, Location, History, Outcome) :-
    % this is just a normal step, kepp playing
    step(Game, Expansion, Strategy, Visited, Location, Next),
    put_assoc(Location, Visited, true, NewVisited),
    play(Game, Expansion, Strategy, NewVisited, Next, [Location|History], Outcome), !.
    

%% Generate the outcome of a strategy on a game
% with the locations from the original game
outcome(Game, Expansion, Strategy, Outcome) :-
  game(Game, Expansion, initial(Initial)),
  empty_assoc(Visited),
  outcome_from(Game, Expansion, Strategy, Visited, [], Initial, Outcome).
outcome_from(Game, Expansion, Strategy, Visited, History, Location, Outcome) :-
  % this version is also used as a helper in `play/7`
  play(Game, Expansion, Strategy, Visited, Location, History, O),
  reverse(O, Outcome).
  
