/*
 * Cocktail Shaker Game
 * 
 * By the way, another group came up with a game that
 * is very similar to the Cart Pushing Game, but has
 * a good "story" and a meaningful objective. It is
 * called the Cocktail Shaker Game.

 * The idea is that we have two robots with one arm
 * each, that want to shake a cocktail. So they need
 * 
 * to take the glass, and then oscillate left-tight.
 * Each robot has just three actions: 'push', 'pull'
 * and 'wait'. If they both push, the glass will break.
 * If one of them pulls, but the other doesn't push,
 * they drop the glass on the floor.

 * Nice, isn't it?

 * Notice that the objective of shaking the glass is
 * a parity objective, as we discussed earlier.
 * */ 
agents([robot1, robot2]).

initial(start).

transition(start, [init, init], left).
transition(start, [init, init], right).

transition(left, [push, pull], right).
transition(left, [pull, push], left).
transition(left, [push, push], lose).
transition(left, [pull, pull], lose).

transition(right, [pull, push], left).
transition(right, [push, pull], right).
transition(right, [push, push], lose).
transition(right, [pull, pull], lose).

transition(lose, [pull, pull], lose).

equal(robot1, [left, right]).
equal(robot2, [left, right]).
