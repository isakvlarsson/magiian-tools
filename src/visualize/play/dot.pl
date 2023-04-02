:- module(play_dot, [
  strategy_edge_id/4,
  outcome_node_id/3,
  outcome_edge_id/3
]).

strategy_edge_id(Out, FromId, ToId, Act) :-
  format(Out, '~a -> ~a [color="red", label="~w"];\n', [FromId, ToId, Act]).

outcome_node_id(Out, Id, L) :-
  format(Out, '~w [label="~w"]', [Id, L]).

outcome_edge_id(Out, From, To) :-
  format(Out, '~w -> ~w;\n', [From, To]).
