
load_game_and_generate_outcomes(G, K):-
	load_game(G), 
	create_expanded_game(G, K), 
	create_outcome_graph(G, K).