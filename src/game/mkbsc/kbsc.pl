:- module(kbsc, [
  create_projection/3,
  create_expanded_projection/3,
  unload_expanded_projection/3,
  projection/5
]).
:- dynamic projection/5.

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
create_projection(G, K, Agt) :-
  unload_projection(G, K, Agt),
  agent_index(G, Agt, Index),
  forall(
    game(G, K, transition(From, JointAction, To)),
    (
      nth0(Index, JointAction, Action),
      assertz(projection(G, K, Agt, 0, transition(From, Action, To)))
    )
  ).

% a projection reuses everything from the original game
% except the transitions
projection(G, K, Agt, 0, initial(I)) :-
  game(G, K, initial(I)).
projection(G, K, Agt, 0, location(L)) :-
  game(G, K, location(L)).
projection(G, K, Agt, 0, observation(Obs)) :-
  game(G, K, observation(Agt, Obs)).

unload_projection(G, K, Agt) :-
  retractall(projection(G, K, Agt, _)), !;
  true.


%% the post function
% used to get all locations S2 that are reachable
% from the locations S1 when taking an action Action
post(G, K, Agt, S1, Action, S2) :-
  setofall(
    S2member,
    (
      member(S1member, S1),
      projection(G, K, Agt, 0, transition(S1member, Action, S2member))
    ),
    S2
  ).

%% finds a set of transitions in the expanded game
% that goes out from the location Si
% this is a useful helper when implementing kbsc
expanded_transitions(G, K, Agt, Si, Transitions) :-
  setofall(
    transition(Si, Action, Intersection),
    (
      game(G, action(Action)),
      post(G, K, Agt, Si, Action, Sj),
      game(G, K, observation(Agt, Observation)),
      intersection(Sj, Observation, Intersection),
      Intersection \== []
    ),
    Transitions
  ).

%% Create the expanded single-agent game
% with KBSC, unlike the multi-agent case
% this expansion does not have any 
% uncertainity (the observations are
% all singletons)
create_expanded_projection(G, K, Agt) :-
  unload_expanded_projection(G, K, Agt),
  game(G, K, initial(Initial)),
  % save the initial cell
  assertz(projection(G, K, Agt, 1, initial([Initial]))),
  assertz(projection(G, K, Agt, 1, location([Initial]))),
  create_expanded_projection(G, K, Agt, [[Initial]]).

create_expanded_projection(_, _, _, []) :- !.
create_expanded_projection(G, K, Agt, [Si|Queue]) :-
  % take the transitions that go out from this observation
  expanded_transitions(G, K, Agt, Si, NT),
  % save the transitions
  forall(
    member(T, NT),
    (
      assertz(projection(G, K, Agt, 1, T))
    )
  ),
  % take the cells in NT that we have not visited yet
  findall(
    S,
    (
      member(transition(_, _, S), NT),
      \+projection(G, K, Agt, 1, transition(S, _, _)),
      \+memberchk(S, [Si|Queue])
    ),
    Ss
  ),
  % save those cells
  forall(
    member(Sj, Ss),
    assertz(projection(G, K, Agt, 1, location(Sj)))
  ),
  % recurse for the new locations
  append(Queue, Ss, NewQueue),
  create_expanded_projection(G, K, Agt, NewQueue).

unload_expanded_projection(G, K, Agt) :-
  retractall(projection(G, K, Agt, 1, _)), !;
  true.


