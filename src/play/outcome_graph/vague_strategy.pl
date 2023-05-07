:- module(vague_strategy, [
  vague_put_strategy/4,
  possible_action_profiles/5
]).

:- use_module(library(clpfd)).
:- use_module(strategy).

%% Colective
vague_put_strategy(L, S0, Acts, S1) :-
  transpose(Acts, ActsT),
  maplist(vague_put_agent_strategy(L), S0, ActsT, S1).

possible_action_profiles(G, K, L, S, ActProfiles) :-
  get_strategy(L, S, Acts),
  setofall(
    ActProfile,
    (
      game(G, K, transition(L, ActProfile, _)),
      maplist(memberchk, ActProfile, Acts)
    ),
    ActProfiles
  ).

%% Individual
vague_put_agent_strategy(L, [LO, OA0], Acts, [LO, OA1]) :-
  % there is a vague strategy for this memorylevel
  get_agent_strategy(L, [LO, OA0], [Old|T]),
  sort(Acts, ActsS),
  intersection(ActsS, Old, New),
  % this is a legal 'narrowing'
  \+length(New, 0),
  get_assoc(L, LO, O),
  put_assoc(O, OA0, [New|T], OA1), 
  !.
vague_put_agent_strategy(L, [LO, OA0], Acts, [LO, OA1]) :-
  % it is not possible to make the
  % strategy more narrow here
  % so add more memory
  get_agent_strategy(L, [LO, OA0], Prevs),
  get_assoc(L, LO, O),
  sort(Acts, ActsS),
  put_assoc(O, OA0, [ActsS|Prevs], OA1).


