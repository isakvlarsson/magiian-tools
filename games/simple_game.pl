% Agt
agent(p1).
agent(p2).

% L
location(left).
location(right).

% l_0
initial(left).

% Act
action(push).
action(wait).

% delta
% from left
transition(left, [wait, wait], left).
transition(left, [wait, push], left).
transition(left, [push, push], left).
transition(left, [push, wait], right).
% from right
transition(right, [wait, wait], right).
transition(right, [wait, push], left).
transition(right, [push, push], right).
transition(right, [push, wait], right).

% Obs_i for each player
observation(p1, [left]).
observation(p1, [right]).

observation(p2, [right]).
observation(p2, [left]).
