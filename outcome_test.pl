
wagon_query(K_max):-
	G = wagon_game_short, 
	load_game(G), 
	create_expanded_game(G, K_max), 
	create_outcome_graph(G, K_max), 
	iterate_k_levels(G, 1, K_max).

iterate_k_levels(G, K, K_max):-
	% Stop when done
	K = K_max;
	outcome(G, K, Outcomes).

%% Is true if Outcome is a outcome (list of nodes in the outcome graph) of G at level K
outcome(G, K, Outcome):-
	actual_location(G,L, s),
	outcome_graph_node(G, K, L, N),
	outcome(G, K, Outcome, [outcome_graph_node(G, K, L, N)], outcome_graph_node(G, K, L, N)).
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
	

