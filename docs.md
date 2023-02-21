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

## Memoryless strategy representation
We represent strategies as a new game graph where the actions for each location
and player have been picked already. So a memoryless strategy is represented as
a set of transitions:

```prolog
[(start, [init, init], left), (start, [init, init], middle), ...,
(middle, (push, wait), right), ...]
```

Meaning that when at a specifik location a specified action is taken. When the
action is nondeterministic, it's outcome can vary.

### Generating strategies

1. Map observations to actions for each player.
2. Map each location in the same observation to the action of the observation.
3. Merge the independent strategies to a "global strategy".
