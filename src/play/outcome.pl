play_round(Game, Expansion, Strategy, Location, Next) :-
  % search for the action
  member(Location-Action, Strategy), 
  %take the action
  game(Game, Expansion, transition(Location, Action, Next)).

play(Game, Expansion, Strategy, Location, History, Outcome) :-
  play_round(Game, Expansion, Strategy, Location, Next),
  \+memberchk(Next, History),
  play(Game, Expansion, Strategy, Next, [Location|History], Outcome).
play(_, _, _, _, O, O).

%% Generate the outcome of a strategy on a game
% with the locations from the original game
outcome(Game, Expansion, Strategy, Outcome) :-
  game(Game, Expansion, initial(Initial)),
  play(Game, Expansion, Strategy, Initial, [], O),
  reverse(O, Outcome).
  

%% is the location the start of a cycle?
is_cycle_start(Game, Expansion, Strategy, Location) :-


%% Is this a 'fork'?
% (Does this location have multiple
% transitions with respect to the action
% taken in the strategy)
is_fork(Game, Expansion, Strategy, Location).
