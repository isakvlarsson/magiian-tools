agents([1, 2]).
initial(start).

transition(start, [init, init], left).
transition(start, [init, init], right).


transition(left, [push, pull], right).
transition(left, [pull, pull], lose).
transition(left, [push, push], win).

transition(win, [push, push], win).
transition(lose, [pull, pull], lose).

transition(right, [pull, push], left).
transition(right, [push, push], win).
transition(right, [push, pull], lose).

equal(1, [left, right]).







