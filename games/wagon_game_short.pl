% this is basically the wagon game
agents([a, b]).

% The initial location
initial(s).

% from start
transition(s, [init, init], l).
transition(s, [init, init], m).
transition(s, [init, init], r).
% from left
transition(l, [wait, wait], l).
transition(l, [wait, push], l).
transition(l, [push, push], l).
transition(l, [push, wait], m).
% from middle
transition(m, [wait, wait], m).
transition(m, [wait, push], l).
transition(m, [push, push], m).
transition(m, [push, wait], r).
% from right
transition(r, [wait, wait], r).
transition(r, [wait, push], m).
transition(r, [push, push], r).
transition(r, [push, wait], r).

%% equivalence relations, signifies that the locations
% here are equal regarding to an agent
equal(a, [l, m]).
equal(b, [m, r]).
