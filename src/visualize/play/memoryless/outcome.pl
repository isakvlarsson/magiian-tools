:- module(visualize_outcome, [
  view_outcome/2,
  export_outcome/4   
]).

:- use_module(library(gv)).
:- use_module(library(term_ext)).
:- use_module('../../utils').
:- use_module('dot').

view_outcome(Outcome, Strategy) :-
  gv_view(
    {Outcome, Strategy}/[Out]>>outcome_to_dot(Out, Outcome, _),
    [directed(true), method(dot)]
  ).

export_outcome(Game, Expansion, Outcome, Strategy) :-
  format(atom(Filename), 'images/~a_K~a_outcome.png', [Game, Expansion]),
  gv_export(
    Filename,
    {Outcome, Strategy}/[Out]>>outcome_to_dot(Out, Outcome, _),
    [directed(true), method(dot)]
  ).

outcome_to_dot(Out, [fork(Forks)], Id) :-
  maplist(split_path, Forks, Hs, Paths),
  same(Hs, Root),
  dot_id(Id),
  outcome_node_id(Out, Id, Root),
  maplist(outcome_to_dot(Out), Paths, Ids),
  maplist(outcome_edge_id(Out, Id), Ids),
  !.
outcome_to_dot(Out, [loop([H|T])], Id) :-
  dot_id(Id),
  outcome_node_id(Out, Id, H),
  loop_to_dot(Out, T, IdNext, Id),
  !.
outcome_to_dot(Out, [H|T], Id) :-
  dot_id(Id),
  outcome_node_id(Out, Id, H),
  outcome_to_dot(Out, T, IdNext),
  IdNext \== no,
  outcome_edge_id(Out, Id, IdNext);
  format(Out, '', []),
  !.
outcome_to_dot(Out, [], no).

loop_to_dot(Out, [], _, StartId) :-
  outcome_edge_id(Out, StartId, StartId).
loop_to_dot(Out, [X], Id, StartId) :-
  dot_id(Id),
  outcome_node_id(Out, Id, H),
  outcome_edge_id(Out, Id, StartId),
  !.
loop_to_dot(Out, [H|T], Id, StartId) :-
  dot_id(Id),
  outcome_node_id(Out, Id, H),
  loop_to_dot(Out, T, IdNext, StartId),
  outcome_edge_id(Out, Id, IdNext).

split_path(fork(Forks), Hs, Ts) :-
  maplist(split_path, Forks, Hs, Ts).
split_path(loop([H|T]), H, T) :- !.
split_path([H|T], H, T).

dot_id(Id) :-
  uuid(X),
  ascii_id(X, Id).
