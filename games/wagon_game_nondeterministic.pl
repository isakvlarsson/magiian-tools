% a list of the agents that are playing
agents([simon, sam]).

% The initial location
initial(start).

% delta - these implicitly define
% * locations in the game
% * actions in the game
% * actions for each agent
% from start
transition(start, [init, init], left).
transition(start, [init, init], middle).
transition(start, [init, init], right).
% from left
transition(left, [wait, wait], left).
transition(left, [wait, push], left).
transition(left, [push, push], left).
transition(left, [push, wait], middle).
transition(left, [push, wait], left).
% from middle
transition(middle, [wait, wait], middle).
transition(middle, [wait, push], left).
transition(middle, [push, push], middle).
transition(middle, [push, wait], right).
% from right
transition(right, [wait, wait], right).
transition(right, [wait, push], middle).
transition(right, [push, push], right).
transition(right, [push, wait], right).

%% equivalence relations, signifies that the locations
% here are equal regarding to an agent
equal(simon, [left, middle]).
equal(sam, [middle, right]).
