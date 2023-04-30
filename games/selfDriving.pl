agents([p1, p2]).

initial(start).

transition(start, [init, init], road).
transition(start, [init, init], parking).

transition(road , [w, b], parking).
transition(road , [b, w], parking).

transition(road , [g, w], road).
transition(road , [w, g], road).

transition(road , [b, b], accident).
transition(road , [w, w], accident).
transition(road , [b, g], accident).
transition(road , [g, b], accident).
transition(road , [g, g], accident).


transition(parking , [g, w], road).
transition(parking , [w, g], road).

transition(parking , [b, w], parking).
transition(parking , [w, b], parking).

transition(parking , [g, b], accident).
transition(parking , [b, g], accident).
transition(parking , [g, g], accident).
transition(parking , [w, w], accident).
transition(parking , [b, b], accident).

transition(accident , [w, w], accident).
transition(accident , [w, b], accident).
transition(accident , [w, g], accident).

equal(p2, [road, parking]).
equal(p2, [parking, accident]).


