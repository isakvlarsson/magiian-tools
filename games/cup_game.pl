agents([p1, p2]).

initial(start).

transition(start, [grab, grab], bad).
transition(start, [grab, grab], good).

transition(bad, [squeeze, squeeze], good).
transition(bad, [lift, lift], lose).
transition(bad, [lift, squeeze], lose).
transition(bad, [squeeze, lift], lose).

transition(good, [squeeze, squeeze], good).
transition(good, [lift, lift], win).
transition(good, [lift, squeeze], lose).
transition(good, [squeeze, lift], lose).

transition(win, [lift, lift], win).

transition(lose, [lift, lift], lose).

equal(p2, [bad, good]).

