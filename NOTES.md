# Outcomes
To generate outcomes for a game and view them: 

```prolog
load_game(wagon_game_short),
create_expanded_game(wagon_game_short, 5),
create_outcome_graph(wagon_game_short, 0).
```
## Outcome-graph
This creates and loads an outcome-graph into the database, with nodes and edges:

### Node
```prolog
outcome_graph_node(G, K, L, N).
```
Where N is the unique id for that node

### Edge

```prolog
outcome_graph_edge(G, K, N1, Act, N2).
```

Where the edge goes from `N1` to `N2`, with the joint action `Act`.

### Goto Node

```prolog
% End is the last node of the loop
% Start is the first node of the loop
% Back is the length of the loop
% Id is the unique id of the goto-node
create_outcome_graph_goto(G, K, End, Start, Back, Id).
```
