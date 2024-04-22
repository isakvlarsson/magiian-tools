:- dynamic visited_goto/2, unique_simple_outcome/3.

wagon_query(K_max):-
	G = wagon_game_short, 
	load_game(G), 
	create_expanded_game(G, K_max),  
	iterate_k_levels(G, 0, K_max).

generate_outcomes_as_locations(G, K_max):- 
	load_game(G), 
	create_expanded_game(G, K_max),  
	iterate_k_levels(G, 0, K_max).

iterate_k_levels(G, K, K_max):-
	K =:= K_max + 1,
	!.
iterate_k_levels(G, K, K_max):-
	create_outcome_graph(G, K),
	retractall(visited_goto(G, K, _, _)),
	retractall(unique_simple_outcome(G, K, _)),
	format(atom(Filename), 'outcomes/~a_K~a_outcomes.txt', [G, K]),
	open(Filename, write,Out),
	forall(unique_outcome(G, K, Outcome),
		(forall(outcome_as_locations(G, K, Outcome, Locations),
		(\+unique_simple_outcome(G, _, Locations) -> 
			(assertz(unique_simple_outcome(G, K, Locations)),format_outcome(Locations, String), writeln(Out, String))
			;
			true)
		))),
	close(Out),
	K1 is K + 1,
	iterate_k_levels(G, K1, K_max),
	!.
	
%% Is true if Outcome is a outcome (list of nodes in the outcome graph) of G at level K
outcome(G, K, Outcome):-
	find_start_node(G, K, outcome_graph_node(G, K, L, N)),
	outcome(G, K, Outcome, [outcome_graph_node(G, K, L, N)], outcome_graph_node(G, K, L, N)),
	!.
outcome(G, K, Outcome, Accumulator, Last):-
	outcome_graph_node(G, K, L, N) = Last,
	outcome_graph_edge(G, K, N, _, N2), % If there is a normal edge from this node
	outcome_graph_node(G, K, L2, N2),
	Last2 = outcome_graph_node(G, K, L2, N2),
	append(Accumulator, [Last2], Accumulator2),
	outcome(G,K, Outcome, Accumulator2,  Last2).
outcome(G, K, Outcome, Accumulator, Last):-
	outcome_graph_node(G, K, L, N) = Last,
	outcome_graph_goto_edge(G, K, N, Start, Id, Act, FinalS, History), % If there is a goto-edge from this node
	outcome_graph_goto(G, K, N, Start, Back, Id),
	Last2 = outcome_graph_goto(G, K, N, Start, Back, Id),
	append(Accumulator, [Last2], Accumulator2),
	Outcome = Accumulator2.

%% Is true if Outcome is a unique outcome (list of nodes in the outcome graph) of G at level K
unique_outcome(G, K, Outcome):-
	find_start_node(G, K, outcome_graph_node(G, K, L, N)),
	unique_outcome(G, K, Outcome, [outcome_graph_node(G, K, L, N)], outcome_graph_node(G, K, L, N)).
unique_outcome(G, K, Outcome, Accumulator, Last):-
	outcome_graph_node(G, K, L, N) = Last,
	outcome_graph_edge(G, K, N, _, N2), % If there is a normal edge from this node
	outcome_graph_node(G, K, L2, N2),
	Last2 = outcome_graph_node(G, K, L2, N2),
	append(Accumulator, [Last2], Accumulator2),
	unique_outcome(G,K, Outcome, Accumulator2,  Last2).
unique_outcome(G, K, Outcome, Accumulator, Last):-
	outcome_graph_node(G, K, L, N) = Last,
	outcome_graph_goto_edge(G, K, N, Start, Id, Act, FinalS, History), % If there is a goto-edge from this node
	outcome_graph_goto(G, K, N, Start, Back, Id),
	(visited_goto(G, K, Start, N) -> fail; assertz(visited_goto(G, K, Start, N))),
	Last2 = outcome_graph_goto(G, K, N, Start, Back, Id),
	append(Accumulator, [Last2], Accumulator2),
	Outcome = Accumulator2.


outcome_as_locations(G, K, Outcome, Locations):-
	outcome_as_locations(G, K, Outcome, Locations, []).
outcome_as_locations(G, K, [outcome_graph_goto(G, K, End, Start, Back, Id)], Locations, Acc):-
	append(Acc, [Back], Acc2),
	Locations = Acc2.
outcome_as_locations(G, K, [outcome_graph_node(G, K, L, N)|T], Locations, Acc):-
	actual_location(G, L, ActualLocation),
	append(Acc, [ActualLocation], Acc2),
	outcome_as_locations(G, K, T, Locations, Acc2).

%% Finds the start node of the outcome graphs and unifies it with `Node`
find_start_node(G, K, Node):-
	game(G, K, initial(Init)),
	Node = outcome_graph_node(G, K, Init, N),
	!.
%% Formats a list of locations (with a number et the end) 
%  as a string of comma-separated locations with the repeating part in parentheses
%  Ex. Outcome = [s, l, m, 1] String = "s,(l,m)" 
format_outcome(Outcome, String):-
	format_outcome(Outcome, String, 0, S),
	!.
format_outcome([H], ")", N, StepsToParenthesis):-
	StepsToParenthesis = N - H - 1,
	!.
format_outcome([H|T], String, 0, StepsToParenthesis):-
	format_outcome(T, String1, 0 + 1, StepsToParenthesis),
	format(atom(Str), "~a", [H]),
	string_concat(Str, String1, String),
	!.
format_outcome([H|T], String, N, StepsToParenthesis):-
	format_outcome(T, String1, N + 1, StepsToParenthesis),
	(N =:= StepsToParenthesis -> 
		format(atom(Str), ",~c~a", [0x28, H]); 
		format(atom(Str), ",~a", [H])),
	string_concat(Str, String1, String),
	!.


	