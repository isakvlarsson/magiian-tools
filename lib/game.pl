%% query the database for information abaout a loaded game
agents(Game, Agents) :-
  findall(agent(Agent), game(Game, agent(Agent)), Agents).

actions(Game, Actions) :-
  findall(action(Action),
          game(Game, action(Action)),
          Actions).

locations(Game, Locations) :-
  findall(location(Location),
          game(Game, location(Location)),
          Locations).

initial(Game, Initial) :-
  game(Game, initial(I)),
  Initial = initial(I).

transitions(Game, Transitions) :-
  findall(transition(From, JointTransition, To),
          game(Game, transition(From, JointTransition, To)),
          Transitions).

observations(Game, Observations) :-
  findall(observation(Agent, Observation),
          game(Game, observation(Agent, Observation)),
          Observations).

%% project a game onto an agent
projection(Agent, Projection) :-
  agents(Agents), nth0(Index, Agents, Agent),
  findall(transition(From, Action, To), 
          (transition(From, JointActions, To), nth0(Index, JointActions, Action)),
          Transitions), 
  Projection = Transitions.

