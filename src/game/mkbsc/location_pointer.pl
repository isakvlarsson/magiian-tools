:- module(location_pointer, [
  create_location_pointer/3,
  unfold_location_pointer/3,
  location_born/4,
  first_location_name/4,
  actual_location/3,
  location_pointer/3
]).

:- use_module(library(term_ext)).
:- dynamic location_pointer/3.
:- table unfold_location_pointer/3, actual_location/3.
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
 


%% Gives the actual location that this knowledge-state
% corresponds to. This is just the location that is in
% all 'parts' of the knowledgestate, all the way down.
actual_location(Game, Location, Location) :-
  \+location_pointer(Game, _, Location), !.
actual_location(Game, LocationPointer, Actual) :-
  location_pointer(Game, Location, LocationPointer),
  intersection_all(Location, [I]),
  actual_location(Game, I, Actual),
  !.
actual_location(Game, LocationPointer, Actual):- 
  % Edge case added, when there is a node where both agents are unsure where they are // Isak Larsson 2024
  % This is a quickfix for a much bigger issue, where the program cant handle these kinds of cases
  location_pointer(Game, Location, LocationPointer),
  intersection_all(Location, [A, B]),
  member(I, [A, B]),
  actual_location(Game, I, Actual).

% ################# helpers #################

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



