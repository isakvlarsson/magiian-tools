# MAGIIAN tools

## Game representation
A game is representated as

```prolog
% Here is an example of how "the wagon game" would be representated

% Agt: the agents (write them in the same order as they appear in the
% transitions)
agent(p1).
agent(p2).

% L: the locations
location(start).
location(left).
location(middle).
location(right).

% l_0: the initial location
initial(start).

% Act: the possible actions
% (generally all of these are available in every
% location, but sometimes this rule is broken (for example with
% the init action))
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
```

## dependencies

* [swipl](https://www.swi-prolog.org/)
* [Graphviz](https://graphviz.org/)
* [prolog_graphviz](https://github.com/wouterbeek/prolog_graphviz)

## some useful commands / predicates

* Start the program with `swipl main.pl`.
* Load a game from the `games` directory with `load_game/1`. For example `?-
  load_game(wagon_game)`.
* Create an mkbsc-expansion of a game with `create_expanded_game/2`. For example
  `?- create_expanded_game(wagon_game, 3)` (this would create all expansions of
  that game up to 3).
* View a game with `view_game/2`. For example `?- view_game(wagon_game, 0)` to view
  the original version of that game, or `?- view_game(wagon_game, 2)` to view that
  game expanded 2 times with mkbsc. (I have only tested this on linux).
* Export an image of a game with `export_game/2`. This works the same as
  `view_game/2` but instead of opening a window with a picure of that game, an
  image is created.
