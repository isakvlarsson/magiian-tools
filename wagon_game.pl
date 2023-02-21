% Agt
agent(p1).
agent(p2).

% L
location(start).
location(left).
location(middle).
location(right).

% l_0
initial(start).

% Act
action(init).
action(push).
action(wait).

% delta
% from start
transition(start, [init, init], left).
transition(start, [init, init], middle).
transition(start, [init, init], right).
% from left
transition(left, [wait, wait], left).
transition(left, [wait, push], left).
transition(left, [push, push], left).
transition(left, [push, wait], middle).
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

% Obs_i for each player
observation(p1, [start]).
observation(p1, [left, middle]).
observation(p1, [right]).

observation(p2, [start]).
observation(p2, [left]).
observation(p2, [middle, right]).
