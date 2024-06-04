:- dynamic visited_goto/2, unique_simple_outcome/3.

%% Performs knowledge expansion 
% and exports every game graph up to K_max
% Enteirely just a convenience function
export_game_graphs(G, K_max):-
	load_game(G), 
	create_expanded_game(G, K_max),  
	export_game_graphs(G, K_max, K_max).
export_game_graphs(G, K_max, -1):-
	!.
export_game_graphs(G, K_max, K):-
	export_game(G, K),
	K1 is K-1,
	export_game_graphs(G, K_max, K1).

%% Performs knowledge expansion 
% and exports lists of outcomes for each knowledge level up to K_max
export_outcomes_as_locations(G, K_max):- 
	load_game(G), 
	create_expanded_game(G, K_max),  
	iterate_k_levels(G, 0, K_max).

%% Main loop for finding each unique outcome and printing it in a file
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
% IS NOT USED
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

%% Formats a list of outcome-graph nodes to a list of locations, 
% with the last element being an integer, indicating how many 
% nodes to loop from the last location. I.e [s, m, l, 1] would indicate that you go from l to m
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

%% Formats a list of locations (with a number at the end) 
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


	