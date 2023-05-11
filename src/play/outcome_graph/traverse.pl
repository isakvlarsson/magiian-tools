% here we test all memoryless strategies for the
% game, and reuse the outcome graph for simpler
% code.
:- module(outcome_graph_traverse, [
  traversal/5,
  strategy_traversal/5
]).

:- use_module('../memoryless/strategy').

%% the actual traversal

%% generate all traversals for a strategy
traversal(G, K, S, Hs, LSs) :-
  memoryless_strategy(G, K, S),
  setofall(H-LS, strategy_traversal(G, K, S, H, LS), HsLSs),
  maplist(unpair, HsLSs, Hs, LSs).

unpair(Key-Val, Key, Val).

%% traverse with a specific strategy
strategy_traversal(G, K, S, History, LoopStart) :-
  game(G, K, initial(Init)),
  outcome_graph_node(G, K, Init, InitNode),
  strategy_traversal(G, K, S, History, LoopStart, InitNode).

strategy_traversal(G, K, S, History, LoopStart, Node) :-
  % if we get to a loop, we have the full history
  outcome_graph_node(G, K, L, Node),
  get_memoryless_strategy(L, S, Act),
  outcome_graph_goto_edge(G, K, Node, LoopStart, _, Act, _, History).

strategy_traversal(G, K, S, History, LoopStart, Node) :-
  % normal step
  outcome_graph_node(G, K, L, Node),
  get_memoryless_strategy(L, S, Act),
  outcome_graph_edge(G, K, Node, Act, Next),
  strategy_traversal(G, K, S, History, LoopStart, Next).











%% human readable form
outcomes_to_latex(Os, Filename):-
  maplist(outcome_to_latex, Os, Regexes),
  sort(Regexes, RegexesU),
  open(Filename, write, Stream),
  forall(
    member(Regex, RegexesU),
    write(Stream, Regex),
    writeln(Stream, ', ')
  ),
  length(RegexesU, Total),
  format(Stream, 'total: ~', [Total]),
  close(Stream).

outcome_to_latex(Branches, Out) :-
  left_branch(Branches, Bl),
  middle_branch(Branches, Bm),
  right_branch(Branches, Br),
  maplist(branch_to_latex, [Bl, Bm, Br], BranchesOut),
  format(atom(Out), '$(~w + ~w + ~w)$', BranchesOut).


left_branch(Branches, B) :-
  member(B, Branches),
  what_branch(B, l).
middle_branch(Branches, B) :-
  member(B, Branches),
  what_branch(B, m).
right_branch(Branches, B) :-
  member(B, Branches),
  what_branch(B, r).
what_branch([prefix([_]), omega([X|_])], X).
what_branch([prefix([_|[X|_]]), _], X).

branch_to_latex([prefix(P), omega(O)], Out) :-
  atom_chars(Prefix, P),
  atom_chars(Omega, O),
  format(atom(Out), '~w(~w)^w', [Prefix, Omega]).

diffing_outcomes(G, K0, K1, Diff) :-
  unique_outcomes(G, K0, O0),
  unique_outcomes(G, K1, O1),
  subtract(O1, O0, Diff).

save_outcomes(Os, Filename) :-
  open(Filename, write, Stream),
  write(Stream, Os),
  writeln('.'),
  close(Stream).

unique_outcomes(G, K, Outcomes) :-
  setofall(
    O,
    (
      an_outcome(G, K, O)
    ),
    Outcomes
  ).

visited_in_branch(L, [prefix(P), omega(O)]) :-
  memberchk(L, P), 
  !;
  memberchk(L, O).
visited_in_all_branches(L, Bs) :-
  forall(
    member(B, Bs),
    visited_in_branch(L, B)
  ).
all_visited_in_all_branches(Ls, Bs) :-
  forall(
    member(L, Ls),
    visited_in_all_branches(L, Bs)
  ).

parse_outcome(G, Hs, LSs, Parsed) :-
  maplist(parse_branch(G), Hs, LSs, Parsed).

parse_branch(G, History, LoopStart, [prefix(ActualPrefix), omega(ActualLoop)]) :-
  find_omega_part(History, LoopStart, Prefix, Loop),
  maplist(actual_location(G), Prefix, ActualPrefix),
  maplist(actual_location(G), Loop, ActualLoop).

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


