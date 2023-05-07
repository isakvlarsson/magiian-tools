%% this is a hardcoded query for
% the wagon game, it could be modified
% to analyze other games. The important
% thing is the number of non-deterministic
% branches
%
% to make it general one could traverse the
% outcome graph and pass strategies between 
% non-deterministic branches
:- module(outcome_graph_analyze, [
 compatible/3
]).

:- use_module(strategy).

diffing_outcomes(G, K0, K1, Diff) :-
  unique_outcomes(G, K0, O0),
  unique_outcomes(G, K1, O1),
  subtract(O1, O0, Diff).

unique_outcomes(G, K, Outcomes) :-
  setofall(
    O,
    an_outcome(G, K, O),
    Outcomes
  ).

an_outcome(G, K, [Branch1, Branch2, Branch3]) :-
  compatible(G, K, [Id1, Id2, Id3]),
  outcome_graph_goto_edge(G, K, _, LoopStart1, Id1, _, _, H1),
  outcome_graph_goto_edge(G, K, _, LoopStart2, Id2, _, _, H2),
  outcome_graph_goto_edge(G, K, _, LoopStart3, Id3, _, _, H3),
  parse_branch(G, H1, LoopStart1, Branch1),
  parse_branch(G, H2, LoopStart2, Branch2),
  parse_branch(G, H3, LoopStart3, Branch3).
  
parse_branch(G, History, LoopStart, [prefix(ActualPrefix), omega(ActualLoop)]) :-
  find_omega_part(History, LoopStart, Prefix, Loop),
  maplist(actual_location(G), Prefix, ActualPrefix),
  maplist(actual_location(G), Loop, ActualLoop),
  append(Prefix, [omega(Loop)], Branch).

find_omega_part(History, LoopStart, Prefix, Loop):-
  find_omega_part(History, LoopStart, Prefix, [], Loop).
find_omega_part([L-LoopStart|T], LoopStart, Prefix, LoopAcc, Loop) :-
  reverse(T, TRev), 
  maplist(history_loc, TRev, Prefix),
  Loop = [L|LoopAcc],
  !.
find_omega_part([L-_|T], LoopStart, Prefix, LoopAcc, Loop) :-
  find_omega_part(T, LoopStart, Prefix, [L|LoopAcc], Loop).
  
history_loc(L-_, L).

% traversing the histories of the nodes
% they have the same actions (or one has
% an action and another does not).
compatible(G, K, [Id1, Id2, Id3]) :-
  outcome_graph_goto_edge(G, K, _, _, Id1, _, S1, _),
  outcome_graph_goto_edge(G, K, _, _, Id2, _, S2, _),
  outcome_graph_goto_edge(G, K, _, _, Id3, _, S3, _),
  % a = a, remove these duplicates
  % (a, b) = (b, a), remove these duplicates
  sort([Id1, Id2, Id3], X),
  length(X, 3),
  X = [Id1, Id2, Id3],
  findall(L, game(G, K, location(L)), Ls),
  maplist(same_action(S1, S2, S3), Ls).


% they should have the same action or
% the no action taken
same_action(S1, S2, S3, L) :-
  get_strategy(L, S1, Act1),
  get_strategy(L, S2, Act2), 
  get_strategy(L, S3, Act3), 
  (
    (Act1 = Act2, Act2 = Act3), !;
    (Act1 = Act2, memberchk(noact, Act3)), !;
    (Act1 = Act3, memberchk(noact, Act2)), !;
    (Act2 = Act3, memberchk(noact, Act1)), !;
    (Act1 = Act2, memberchk(noact, Act3)), !;
    (memberchk(noact, Act1), memberchk(noact, Act2)), !;
    (memberchk(noact, Act1), memberchk(noact, Act3)), !;
    (memberchk(noact, Act2), memberchk(noact, Act3)), !;
    (memberchk(noact, Act1), memberchk(noact, Act2), memberchk(noact, Act3))
  ).

%% write out the full set of outcomes that are linked to
% a specific strategy
text_compatible(G, K, [Id1, Id2]) :-
  outcome_graph_goto_edge(G, K, _, Start1, Id1, _, S1, H1),
  outcome_graph_goto_edge(G, K, _, Start2, Id2, _, S2, H2),
  reverse(H1, H1Rev),
  reverse(H2, H2Rev),
  format(atom(Out), '(~w|~w)', [H1Rev, H2Rev]),
  writeln(Out).

%% visualize compatible sets of outcomes

:- use_module(library(gv)).

view_compatible(G, K, Compat) :-
  gv_view(
  {G, K, Compat}/[Out]>>compatible_to_dot(Out, G, K, Compat),
    [directed(true), method(dot)]
  ).

compatible_to_dot(Out, G, K, [Id1, Id2, Id3]) :-
  branch_to_dot(Out, G, K, Id1),
  branch_to_dot(Out, G, K, Id2),
  branch_to_dot(Out, G, K, Id3).
  
branch_to_dot(Out, G, K, Id) :-
  outcome_graph_goto_edge(G, K, End, Start, Id, _, S, H),
  history_to_dot(Out, start, H),
  % add the loop
  format(Out, '~w -> ~w \n', [End, Start]).

history_to_dot(Out, _, []) :-
  format(Out, '\n', []), 
  !.
history_to_dot(Out, start, [L-Node|T]) :-
  format(Out, '~w [label="~w"] \n', [Node, L]),
  history_to_dot(Out, Node, T),
  !.
history_to_dot(Out, PrevNode, [L-Node|T]) :-
  format(Out, '~w [label="~w"] \n', [Node, L]),
  format(Out, '~w -> ~w \n', [Node, PrevNode]),
  history_to_dot(Out, Node, T),
  !.

