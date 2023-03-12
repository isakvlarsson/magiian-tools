agent(p1).
agent(p2).

location(start).
location(bad).
location(good).
location(lose).
location(win).

initial(start).

action(grab).
action(squeeze).
action(lift).

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

observation(p1, [start]).
observation(p1, [bad]).
observation(p1, [good]).
observation(p1, [lose]).
observation(p1, [win]).

observation(p2, [start]).
observation(p2, [bad, good]).
observation(p2, [lose]).
observation(p2, [win]).

