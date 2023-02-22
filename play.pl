:- [strategy].

move(Location, Strategy, NextLocation) :-
  % get the joint_action associated with a Location
  member(Location-JointAction, Strategy),
  transition(Location, JointAction, NextLocation).

play(_, _, Limit, Limit, AccHistory, History):- reverse(AccHistory, History).
play(Location, Strategy, Iteration, Limit, AccHistory, _History) :-
  Iteration < Limit,
  move(Location, Strategy, NextLocation),
  NextIteration is Iteration + 1, 
  play(NextLocation, Strategy, NextIteration, Limit, [Location|AccHistory], _History).

play(InitialLocation, Strategy, Limit, History) :-
  play(InitialLocation, Strategy, 0, Limit, [], History).

main:-
  initial(Initial),
  global_strategy(Strategy),
  findall(History, play(Initial, Strategy, 100, History), AllHistories),
  nl, write('new game!'), nl, write(AllHistories), nl.


