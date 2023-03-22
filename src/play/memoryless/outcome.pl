:- module(memoryless_outcome, [
  outcome/4
]).

:- use_module('strategy').
% ####################### Outcome ##################################
%% Generate the outcome of a strategy on a game
% with the locations from the original game
outcome(Game, Expansion, Strategy, Outcome) :-
  game(Game, Expansion, initial(Initial)),
  empty_assoc(Visited),
  outcome_from(Game, Expansion, Strategy, Visited, [], Initial, Outcome).
outcome_from(Game, Expansion, Strategy, Visited, History, Location, Outcome) :-
  % this version is also used as a helper in `play/7`
  play(Game, Expansion, Strategy, Visited, Location, History, O),
  parse_history(O, Parsed),
  reverse(Parsed, Reversed),
  to_actual_locations(Game, Reversed, Outcome).


%% ############################## Helpers ###############################
%% Takes a stap in the game
% based on a memoryless strategy
% Visited is an assoc
step(_, _, _, Visited, Location, goto(Location)) :-
  % We have been here before
  get_assoc(Location, Visited, true), !.
step(Game, Expansion, Strategy, _, Location, fork(Nexts)) :-
  % multiple transitions, non-determinism
  % I call this a fork
  get_memoryless_strategy(Location, Strategy, ActionProfile),
  findall(N, game(Game, Expansion, transition(Location, ActionProfile, N)), Nexts),
  length(Nexts, Len), 
  Len > 1, !.
step(Game, Expansion, Strategy, _, Location, Next) :-
  get_memoryless_strategy(Location, Strategy, ActionProfile),
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
    


%% this converts the history to a
% better format. `goto`s are replaced
% with `loop`s
parse_history(History, Parsed) :-
  parse_history(History, [], Parsed).
parse_history([goto(L)|History], Acc, Parsed) :-
  parse_history_loop(L, History, Loop, Rest),
  parse_history(Rest, [loop(Loop)|Acc], Parsed), !.
parse_history([fork(Forks)|T], Acc, Parsed) :-
  maplist(parse_history, Forks, ParsedForks),
  parse_history(T, [fork(ParsedForks)|Acc], Parsed), !.
parse_history([L|History], Acc, Parsed) :-
  parse_history(History, [L|Acc], Parsed), !.
parse_history([], P, P).


parse_history_loop(Start, History, Loop, Rest) :-
  parse_history_loop(Start, History, [], Loop, Rest).
parse_history_loop(Start, [Start|History], Acc, [Start|Acc], History) :-
  !.
parse_history_loop(Start, [L|History], Acc, _, Rest) :-
  parse_history_loop(Start, History, [L|Acc], _, Rest).
  
%% Converts the locations in expanded
% games into their `actual` names
% in the original game
to_actual_locations(Game, History, Renamed) :-
  to_actual_locations(Game, History, [], Renamed).

to_actual_locations(_, [], Renamed, Renamed).
to_actual_locations(Game, [loop(Loop)|T], Acc, R) :-
  to_actual_locations(Game, Loop, [], Inner),
  to_actual_locations(Game, T, [loop(Inner)|Acc], R), !.
to_actual_locations(Game, [fork(Forks)|T], Acc, R) :-
  maplist(to_actual_locations(Game), Forks, Inner),
  to_actual_locations(Game, T, [fork(Inner)|Acc], R), !.
to_actual_locations(Game, [L|T], Acc, R) :-
  actual_location(Game, L, Actual),
  to_actual_locations(Game, T, [Actual|Acc], R).

