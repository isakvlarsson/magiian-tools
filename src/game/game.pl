:- use_module(library(term_ext)).
:- dynamic game/3.
/*
 * ################## Load game ###################
 * */
%% Load a game from a file and parses it to the internal format
% then the facts about the game is saved to the prolog database
load_game(Game) :-
  unload_game(Game),
  atom_concat('games/', Game, F0), atom_concat(F0, '.pl', Filename),
  see(Filename), 
  repeat, 
  read(Term),
  (
    Term == end_of_file -> !;
    process_gameterm(Game, Term),
    fail
  ),
  % add all the singleton observations
  forall(
    (
      game(Game, agent(Agent)),
      findall(Obs, game(wagon_game, observation(Agent, Obs)), Obss),
      flatten(Obss, Partitioning)
    ),
    (
      forall(
        (
          game(Game, location(L)),
          \+ memberchk(L, Partitioning)
        ),
        (
          assert_gamefact_once(Game, observation(Agent, [L]))
        )
      )
    )
  ),
  % mark the game as loaded
  assertz(loaded(Game, 0)),
  seen.

%% loads a game and creates the expansion
load_game(Game, Expansion) :-
  load_game(Game),
  create_expanded_game(Game, Expansion).

%% helper for the load_gam/1
% to parse the input
process_gameterm(Game, Term) :-
  (
    Term = agents(Agents) -> 
    assert_gamefact_once(Game, Term),
    forall(
      member(Agent, Agents),
      assert_gamefact_once(Game, agent(Agent))
    );

    Term = initial(Initial) ->
    assert_gamefact_once(Game, initial(Initial));

    Term = transition(From, JointAction, To) ->
    (
      % add the transition
      assert_gamefact_once(Game, transition(From, JointAction, To)),
      % add the locations
      assert_gamefact_once(Game, location(From)),
      assert_gamefact_once(Game, location(To)),
      % add the actions
      forall(
        member(Action, JointAction),
        assert_gamefact_once(Game, action(Action))
      )
    );

    Term = equal(Agent, Locations) ->
    assert_gamefact_once(Game, observation(Agent, Locations))
  ).
 
%add_singleton_observations(Game) :-
  
  
% unloads the loaded game
% is allways true
% unloads all expansions
unload_game(Game) :-
  retractall(game(Game, _, _)),
  retract(loaded(Game, _)), !;
  true.

/*
 * ####################### Helpers #########################
 * */

assert_gamefact_once(Game, Term) :-
  \+game(Game, 0, Term),
  assertz(game(Game, 0, Term)), !;
  true.

 % get the 0-indexed index of an agent in a game
agent_index(Game, Agent, Index) :-
  findall(Agt, game(Game, agent(Agt)), Agents),
  nth0(Index, Agents, Agent), !.

% To refer to the original game without the 0-expansion
game(Game, Term) :-
  game(Game, 0, Term).

% these allow us to get all possible jointactions in a game
agent_action(Game, _, Action) :- game(Game, action(Action)).
joint_action(Game, JointAction) :-
  findall(Agent, game(Game, agent(Agent)), Agents),
  maplist(agent_action(Game), Agents, JointAction).

