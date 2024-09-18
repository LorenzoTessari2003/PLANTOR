% This file was automatically generated by the LLM system
%%%%%%%%%%%%%%%%%%%%%%%
% kb
%%%%%%%%%%%%%%%%%%%%%%%
% Positions
pos(1,1).
pos(2,2).
pos(3,3).

% Blocks
block(b1).
block(b2).
block(b3).

% Agents
agent(a1).
agent(a2).

% Resources
resources(agent(_)).

%%%%%%%%%%%%%%%%%%%%%%%
% init
%%%%%%%%%%%%%%%%%%%%%%%
init_state([
  ontable(b1), ontable(b2), ontable(b3),
  at(b1,1,1), at(b2,2,2), at(b3,3,3),
  clear(b1), clear(b2), clear(b3),
  available(a1), available(a2)
]).

%%%%%%%%%%%%%%%%%%%%%%%
% goal
%%%%%%%%%%%%%%%%%%%%%%%
goal_state([
  ontable(b1), ontable(b2), on(b3, b1),
  at(b1,1,1), at(b2,2,2), at(b3,1,1),
  clear(b2), clear(b3),
  available(a1), available(a2)
]).

:- ensure_loaded('prolog_planner/examples/blocks_world/actions.pl').
:- ensure_loaded('prolog_planner/examples/blocks_world/mappings.pl').
