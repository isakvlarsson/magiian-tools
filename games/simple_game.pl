% Agt
agents([p1, p2]).

% l_0
initial(left).

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
