% This file was automatically generated by the LLM system
%%%%%%%%%%%%%%%%%%%%%%%
% kb
%%%%%%%%%%%%%%%%%%%%%%%
% Define positions on the table
pos(1,1).
pos(2,2).
pos(3,3).

% Define blocks
block(b1).
block(b2).
block(b3).

% Define agents
agent(a1).
agent(a2).

% Define low-level predicates for the agents' arms
ll_arm(a1).
ll_arm(a2).

% Define low-level predicates for the agents' grippers
ll_gripper(a1).
ll_gripper(a2).

% Define resources
resources(agent(_)).
resources(ll_arm(_)).
resources(ll_gripper(_)).

%%%%%%%%%%%%%%%%%%%%%%%
% init
%%%%%%%%%%%%%%%%%%%%%%%
init_state([
  ll_arm_at(a1, 4, 4), ll_arm_at(a2, 5, 5),
  ll_gripper(a1, open), ll_gripper(a2, open)
]).

%%%%%%%%%%%%%%%%%%%%%%%
% goal
%%%%%%%%%%%%%%%%%%%%%%%
goal_state([
  ll_arm_at(a1, _, _), ll_arm_at(a2, _, _),
  ll_gripper(a1, _), ll_gripper(a2, _)
]).

%%%%%%%%%%%%%%%%%%%%%%%
% actions
%%%%%%%%%%%%%%%%%%%%%%%
% Move a block from a position on the table to another position on the table
action(move_block_table_to_table_start(Agent, Block, X1, Y1, X2, Y2),
  [available(Agent), ontable(Block), at(Block, X1, Y1), clear(Block), pos(X2, Y2)],
  [at(_, X2, Y2)],
  [],
  [block(Block), agent(Agent)],
  [del(available(Agent)), del(clear(Block)), del(at(Block, X1, Y1)), add(moving(Agent, Block))]
).

action(move_block_table_to_table_end(Agent, Block, X1, Y1, X2, Y2),
  [moving(Agent, Block)],
  [],
  [],
  [block(Block), agent(Agent)],
  [del(moving(Agent, Block)), add(available(Agent)), add(clear(Block)), add(at(Block, X2, Y2)), add(ontable(Block))]
).

% Move a block from a position on the table to the top of another block
action(move_block_table_to_block_start(Agent, Block1, Block2, X1, Y1, X2, Y2),
  [available(Agent), ontable(Block1), at(Block1, X1, Y1), clear(Block1), clear(Block2), at(Block2, X2, Y2)],
  [],
  [],
  [block(Block1), block(Block2), agent(Agent)],
  [del(available(Agent)), del(clear(Block1)), del(at(Block1, X1, Y1)), add(moving(Agent, Block1))]
).

action(move_block_table_to_block_end(Agent, Block1, Block2, X1, Y1, X2, Y2),
  [moving(Agent, Block1)],
  [],
  [],
  [block(Block1), block(Block2), agent(Agent)],
  [del(moving(Agent, Block1)), add(available(Agent)), add(clear(Block2)), add(on(Block1, Block2)), add(at(Block1, X2, Y2))]
).

% Move a block from the top of a block to a position on the table
action(move_block_block_to_table_start(Agent, Block1, Block2, X1, Y1, X2, Y2),
  [available(Agent), on(Block1, Block2), at(Block1, X1, Y1), clear(Block1), pos(X2, Y2)],
  [at(_, X2, Y2)],
  [],
  [block(Block1), block(Block2), agent(Agent)],
  [del(available(Agent)), del(clear(Block1)), del(on(Block1, Block2)), add(moving(Agent, Block1)), add(clear(Block2))]
).

action(move_block_block_to_table_end(Agent, Block1, Block2, X1, Y1, X2, Y2),
  [moving(Agent, Block1)],
  [],
  [],
  [block(Block1), block(Block2), agent(Agent)],
  [del(moving(Agent, Block1)), add(available(Agent)), add(clear(Block1)), add(at(Block1, X2, Y2)), add(ontable(Block1))]
).

% Move a block from the top of a block to the top of another block
action(move_block_block_to_block_start(Agent, Block1, Block2, Block3, X1, Y1, X2, Y2),
  [available(Agent), on(Block1, Block2), at(Block1, X1, Y1), clear(Block1), clear(Block3), at(Block3, X2, Y2)],
  [],
  [],
  [block(Block1), block(Block2), block(Block3), agent(Agent)],
  [del(available(Agent)), del(clear(Block1)), del(on(Block1, Block2)), add(moving(Agent, Block1)), add(clear(Block2))]
).

action(move_block_block_to_block_end(Agent, Block1, Block2, Block3, X1, Y1, X2, Y2),
  [moving(Agent, Block1)],
  [],
  [],
  [block(Block1), block(Block2), block(Block3), agent(Agent)],
  [del(moving(Agent, Block1)), add(available(Agent)), add(clear(Block3)), add(on(Block1, Block3)), add(at(Block1, X2, Y2))]
).


%%%%%%%%%%%%%%%%%%%%%%%
% ll_actions
%%%%%%%%%%%%%%%%%%%%%%%
% Move the arm from one position to another
ll_action(ll_move_arm_start(Agent, X1, Y1, X2, Y2),
  [ll_arm_at(Agent, X1, Y1)],
  [ll_moving_arm(Agent, _, _, _, _, _)],
  [],
  [ll_arm(Agent), pos(X2, Y2)],
  [del(ll_arm_at(Agent, X1, Y1)), add(ll_moving_arm(Agent, X1, Y1, X2, Y2))]
).

ll_action(ll_move_arm_end(Agent, X1, Y1, X2, Y2),
  [ll_moving_arm(Agent, X1, Y1, X2, Y2)],
  [],
  [],
  [ll_arm(Agent)],
  [del(ll_moving_arm(Agent, X1, Y1, X2, Y2)), add(ll_arm_at(Agent, X2, Y2))]
).

% Close the gripper of the agent
ll_action(ll_close_gripper(Agent),
  [ll_gripper(Agent, open)],
  [ll_gripping(Agent, _), ll_releasing(Agent)],
  [],
  [ll_gripper(Agent)],
  [del(ll_gripper(Agent, open)), add(ll_gripper(Agent, close))]
).

% Open the gripper of the agent
ll_action(ll_open_gripper(Agent),
  [ll_gripper(Agent, close)],
  [ll_gripping(Agent, _), ll_releasing(Agent)],
  [],
  [ll_gripper(Agent)],
  [del(ll_gripper(Agent, close)), add(ll_gripper(Agent, open))]
).

%%%%%%%%%%%%%%%%%%%%%%%
% mappings
%%%%%%%%%%%%%%%%%%%%%%%
mapping(move_block_table_to_table_start(Agent, Block, X1, Y1, X2, Y2),
  [
    ll_move_arm_start(Agent, X1, Y1, X1, Y1),
    ll_move_arm_end(Agent, X1, Y1, X1, Y1),
    ll_close_gripper(Agent),
    ll_move_arm_start(Agent, X1, Y1, X2, Y2),
    ll_move_arm_end(Agent, X1, Y1, X2, Y2),
    ll_open_gripper(Agent)
  ]
).

mapping(move_block_table_to_block_start(Agent, Block1, Block2, X1, Y1, X2, Y2),
  [
    ll_move_arm_start(Agent, X1, Y1, X1, Y1),
    ll_move_arm_end(Agent, X1, Y1, X1, Y1),
    ll_close_gripper(Agent),
    ll_move_arm_start(Agent, X1, Y1, X2, Y2),
    ll_move_arm_end(Agent, X1, Y1, X2, Y2),
    ll_open_gripper(Agent)
  ]
).

mapping(move_block_block_to_table_start(Agent, Block1, Block2, X1, Y1, X2, Y2),
  [
    ll_move_arm_start(Agent, X1, Y1, X1, Y1),
    ll_move_arm_end(Agent, X1, Y1, X1, Y1),
    ll_close_gripper(Agent),
    ll_move_arm_start(Agent, X1, Y1, X2, Y2),
    ll_move_arm_end(Agent, X1, Y1, X2, Y2),
    ll_open_gripper(Agent)
  ]
).

mapping(move_block_block_to_block_start(Agent, Block1, Block2, Block3, X1, Y1, X2, Y2),
  [
    ll_move_arm_start(Agent, X1, Y1, X1, Y1),
    ll_move_arm_end(Agent, X1, Y1, X1, Y1),
    ll_close_gripper(Agent),
    ll_move_arm_start(Agent, X1, Y1, X2, Y2),
    ll_move_arm_end(Agent, X1, Y1, X2, Y2),
    ll_open_gripper(Agent)
  ]
).
