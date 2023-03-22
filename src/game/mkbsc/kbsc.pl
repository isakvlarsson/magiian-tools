:- module(kbsc, [
  create_projection/3,
  create_projection_expansion/3,
  unload_projection_expansion/3,
  projection/4,
  projection_expansion/4
]).
:- dynamic projection/4.
:- dynamic projection_expansion/4.

:- use_module('../parse').
:- use_module('../../utils').
/*
 * ########################## KBSC ######################
 * these predicates handle projeciton of a multi-agent game
 * onto a single agent, and the expansion of that single-agent 
 * game with KBSC.
 *
 * When creating projections we only need to create new
 * transitions (that only uses the agents action), because
 * everything else is redundant.
 * */

%% create the projection of the game of an agent
create_projection(Game, Expansion, Agent) :-
  unload_projection(Game, Expansion, Agent),
  agent_index(Game, Agent, Index),
  forall(
    game(Game, Expansion, transition(From, JointAction, To)),
    (
      nth0(Index, JointAction, Action),
      assertz(projection(Game, Expansion, Agent, transition(From, Action, To)))
    )
  ).

unload_projection(Game, Expansion, Agent) :-
  retractall(projection(Game, Expansion, Agent, _)), !;
  true.


%% the post function
% used to get all locations S2 that are reachable
% from the locations S1 when taking an action Action
post(Game, Expansion, Agent, S1, Action, S2) :-
  setofall(
    S2member,
    (
      member(S1member, S1),
      projection(Game, Expansion, Agent, transition(S1member, Action, S2member))
    ),
    S2
  ).

%% finds a set of transitions in the expanded game
% that goes out from the location Si
% this is a useful helper when implementing kbsc
expanded_neighbor_transitions(Game, Expansion, Agent, Si, Transitions) :-
  setofall(
    transition(Si, Action, Intersection),
    (
      game(Game, action(Action)),
      post(Game, Expansion, Agent, Si, Action, Sj),
      game(Game, Expansion, observation(Agent, Observation)),
      intersection(Sj, Observation, Intersection),
      Intersection \== []
    ),
    Transitions
  ).

%% all transitions of the new game will be located in Transitions
create_projection_expansion(Game, Expansion, Agent) :-
  unload_projection_expansion(Game, Expansion, Agent),
  game(Game, Expansion, initial(Initial)),
  % save the initial cell
  assertz(projection_expansion(Game, Expansion, Agent, initial([Initial]))),
  assertz(projection_expansion(Game, Expansion, Agent, location([Initial]))),
  create_projection_expansion(Game, Expansion, Agent, [[Initial]]).

create_projection_expansion(_, _, _, []) :- !.
create_projection_expansion(Game, Expansion, Agent, [Si|Queue]) :-
  % take the transitions that go out from this observation
  expanded_neighbor_transitions(Game, Expansion, Agent, Si, NT),
  % save the transitions
  forall(
    member(T, NT),
    (
      assertz(projection_expansion(Game, Expansion, Agent, T))
    )
  ),
  % take the cells in NT that we have not visited yet
  findall(
    S,
    (
      member(transition(_, _, S), NT),
      \+projection_expansion(Game, Expansion, Agent, transition(S, _, _)),
      \+memberchk(S, [Si|Queue])
    ),
    Ss
  ),
  % save those cells
  forall(
    member(Sj, Ss),
    assertz(projection_expansion(Game, Expansion, Agent, location(Sj)))
  ),
  % recurse for the new locations
  append(Queue, Ss, NewQueue),
  create_projection_expansion(Game, Expansion, Agent, NewQueue).

unload_projection_expansion(Game, Expansion, Agent) :-
  retractall(projection_expansion(Game, Expansion, Agent, _)), !;
  true.


