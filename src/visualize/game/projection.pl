:- module(visualize_projection, [
    view_projection/4,
    view_projection/5,
    export_projection/4,
    export_projection/5
]).

:- use_module(library(gv)).
:- use_module(location).
:- use_module(observation).
:- use_module(dot).

view_projection(G, K, Agt, InnerK) :-
  view_projection(G, K, Agt, InnerK, standard).
view_projection(G, K, Agt, InnerK, Mode) :-
  gv_view(
    {G, K, Agt, InnerK, Mode}/[Out]>>projection_to_dot(Out, G, K, Agt, InnerK, Mode),
    [directed(true), method(dot)]
  ).

export_projection(G, K, Agt, InnerK) :-
  export_projection(G, K, Agt, InnerK, standard).
export_projection(G, K, Agt, InnerK, Mode) :-
  format(atom(Filename), 'images/~a_K~a_~a.dot', [G, K, Agt]),
  gv_export(
    Filename,
    {G, K, Agt, InnerK, Mode}/[Out]>>projection_to_dot(Out, G, K, Agt, InnerK, Mode),
    [directed(true), method(dot)]
  ).

projection_to_dot(Out, G, K, Agt, 0, Mode) :-
  agent_locations_to_dot(Out, G, K, Agt, InnerK, Mode),
  agent_transitions_to_dot(Out, G, K, Agt, InnerK),
  agent_observations_to_dot(Out, G, K, Agt),
  !.
projection_to_dot(Out, G, K, Agt, 1, Mode) :-
  agent_locations_to_dot(Out, G, K, Agt, 1, Mode),
  agent_transitions_to_dot(Out, G, K, Agt, 1).

agent_transitions_to_dot(Out, G, K, Agt, InnerK) :-
  setofall([From, To]-Act, projection(G, K, Agt, InnerK, transition(From, Act, To)), Transitions),
  group_pairs_by_key(Transitions, LabeledTransitions),
  forall(
    member([From, To]-Act, LabeledTransitions),
    (
      InnerK == 0 -> my_dot_arc(Out, From, To, Act), !;
      InnerK == 1 -> my_dot_arc_no_id(Out, From, To, Act)
    )
  ).

