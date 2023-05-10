:- use_module('src/play/outcome_graph/strategy').
%% this is a hardcoded query for
%
% the wagon game, it could be modified
% to analyze other games. The important
% thing is the number of non-deterministic
% branches
%
% to make it general one could traverse the
% outcome graph and pass strategies between 
% non-deterministic branches. That would also 
% probably make it faster

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
  format(atom(Out), '(~w + ~w + ~w)', BranchesOut).


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
  format(atom(Out), '$~w(~w)^w$', [Prefix, Omega]).

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
  outcome_graph_goto_edge(G, K, End, Start, Id, _, _, H),
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

