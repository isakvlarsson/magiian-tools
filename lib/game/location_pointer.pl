/*
 * ############# Location pointers #########################
 * */
/*
 * The names of the locations/knowledge-states in the expanded games grows
 * exponentially. The locations should instead be stored 
 * as pointers.
 *
 * These pointers are keys in a key value store
 * of the locations/knowledge-states, and for now
 * they are generated with the `ascii_id/2` predicate
 * which generates a hash for a term. These are not
 * guaranteed to be unique but the chance for an overlap
 * is probably microscopical.
 * */

%% store the id of a location with its real name
% only stores a pointer once
:- dynamic location_pointer/3.
create_location_pointer(Game, Location, Pointer) :-
  location_pointer(Game, Location, Pointer), !;
  ascii_id(Location, Pointer),
  assert(location_pointer(Game, Location, Pointer)).


%% extracts the real name of a knowledge-state
% used for visualization purposes
unfold_location_pointer(Game, Location, Pointer) :-
  \+is_list(Pointer),
  \+location_pointer(Game, _, Pointer),
  Location = Pointer, !;
  location_pointer(Game, List, Pointer),
  maplist(unfold_location_pointer(Game), Location, List), !.
unfold_location_pointer(Game, Location, Pointers) :-
  maplist(unfold_location_pointer(Game), Location, Pointers), !.


%% finds out if a locations is 'new' in this expansion
% of the game or if it existed in the previous expansion.
% We say that it existed if the JointKnowledge of the locaiton
% consists of observations that all agents have access to in the
% previous version
location_is_new(Game, 0, L) :-
  game(Game, 0, location(L)), !.
location_is_new(Game, Expansion, LP) :-
  game(Game, Expansion, location(LP)),
  location_pointer(Game, L, LP),
  game(Game, agents(Agents)),
  PreviousExpansion is Expansion - 1,
  \+maplist(agent_has_observation(Game, PreviousExpansion), Agents, L).

% helper to the above predicate
agent_has_observation(Game, Expansion, Agent, Observation) :-
  game(Game, Expansion, O), O = observation(Agent, Observation), !.


%% associates a number to each location related to
% in which expansion that location was 'created'.
location_born(Game, Expansion, Location, Expansion) :-
  % it is new in this expansion
  game(Game, Expansion, location(Location)),
  location_is_new(Game, Expansion, Location), !.
location_born(Game, Expansion, Location, Number) :-
  % it was created in an earlier expansion
  PreviousExpansion is Expansion - 1,
  location_pointer(Game, List, Location),
  intersection_all(List, [IsomorphicLocation]),
  location_born(Game, PreviousExpansion, IsomorphicLocation, Number).


%% Gives the first name (in the isomorphic sense) of a location.
first_location_name(Game, Expansion, Location, Name) :-
  % it is new in this expansion
  game(Game, Expansion, location(Location)),
  location_is_new(Game, Expansion, Location),
  unfold_location_pointer(Game, Name, Location), !.
first_location_name(Game, Expansion, Location, Name) :-
  % it was created in an earlier expansion
  PreviousExpansion is Expansion - 1,
  location_pointer(Game, List, Location),
  intersection_all(List, [IsomorphicLocation]),
  first_location_name(Game, PreviousExpansion, IsomorphicLocation, Name).
 
