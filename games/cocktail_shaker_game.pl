/*
 * By the way, another group came up with a game that
 * is very similar to the Cart Pushing Game, but has
 * a good "story" and a meaningful objective. It is
 * called the Cocktail Shaker Game.

 * The idea is that we have two robots with one arm
 * each, that want to shake a cocktail. So they need
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

 transitions(start, [init, init], left).
 transitions(start, [init, init], right).

 transition(left, [wait, wait], left).
 transition(left, [wait, push], left).
 transition(left, [push, wait], left).
 transition(left, [push, pull], right).
 transition(left, [pull, push], right).
