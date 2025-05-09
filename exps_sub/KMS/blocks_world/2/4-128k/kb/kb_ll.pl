% This file was automatically generated by the LLM system
%%%%%%%%%%%%%%%%%%%%%%%
% kb
%%%%%%%%%%%%%%%%%%%%%%%
% Define positions on the table
pos(1,1).
pos(2,2).
pos(3,3).
pos(4,4).
pos(5,5).
pos(10,10).

% Define blocks
block(b1).
block(b2).
block(b3).
block(b4).
block(b5).
block(b6).

% Define agents
agent(a1).
agent(a2).

% Define arms as resources
ll_arm(a1).
ll_arm(a2).

% Define grippers as resources
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
  ll_ontable(b1), ll_ontable(b2), ll_ontable(b3),
  ll_on(b4, b1), ll_on(b5, b2), ll_on(b6, b3),
  ll_at(b1, 1, 1), ll_at(b2, 2, 2), ll_at(b3, 3, 3),
  ll_at(b4, 1, 1), ll_at(b5, 2, 2), ll_at(b6, 3, 3),
  ll_clear(b4), ll_clear(b5), ll_clear(b6),
  ll_available(a1), ll_available(a2),
  ll_arm_at(a1, 4, 4), ll_arm_at(a2, 5, 5),
  ll_gripper(a1, open), ll_gripper(a2, open)
]).

%%%%%%%%%%%%%%%%%%%%%%%
% goal
%%%%%%%%%%%%%%%%%%%%%%%
goal_state([
  ll_ontable(b1), ll_ontable(b2), ll_ontable(b3), ll_ontable(b4),
  ll_on(b5, b4),
  ll_at(b1, 1, 1), ll_at(b2, 2, 2), ll_at(b3, 3, 3),
  ll_at(b4, 10, 10), ll_at(b5, 10, 10), ll_at(b6, 3, 3),
  ll_clear(b5), ll_clear(b6),
  ll_available(a1), ll_available(a2),
  ll_arm_at(a1, _, _), ll_arm_at(a2, _, _),
  ll_gripper(a1, _), ll_gripper(a2, _)
]).

%%%%%%%%%%%%%%%%%%%%%%%
% actions
%%%%%%%%%%%%%%%%%%%%%%%
% Move a block from a position on the table to another position on the table
action(move_block_table_to_table_start(Agent, Block, X1, Y1, X2, Y2),
  [available(Agent), ontable(Block), at(Block, X1, Y1), clear(Block)],
  [at(_, X2, Y2)],
  [],
  [agent(Agent), pos(X1, Y1), pos(X2, Y2), block(Block)],
  [
    del(available(Agent)), del(clear(Block)), del(ontable(Block)), del(at(Block, X1, Y1)),
    add(moving(Agent, Block, X1, Y1, X2, Y2))
  ]
).
action(move_block_table_to_table_end(Agent, Block, X1, Y1, X2, Y2),
  [moving(Agent, Block, X1, Y1, X2, Y2)],
  [at(_, X2, Y2)],
  [],
  [agent(Agent), pos(X2, Y2)],
  [
    del(moving(Agent, Block, X1, Y1, X2, Y2)),
    add(available(Agent)), add(clear(Block)), add(ontable(Block)), add(at(Block, X2, Y2))
  ]
).

% Move a block from a position on the table to the top of another block
action(move_block_table_to_block_start(Agent, Block1, Block2, X1, Y1, X2, Y2),
  [available(Agent), ontable(Block1), at(Block1, X1, Y1), clear(Block1), at(Block2, X2, Y2), clear(Block2)],
  [],
  [],
  [agent(Agent), pos(X1, Y1), block(Block1), block(Block2)],
  [
    del(available(Agent)), del(clear(Block1)), del(ontable(Block1)), del(at(Block1, X1, Y1)),
    add(moving(Agent, Block1, X1, Y1, X2, Y2))
  ]
).
action(move_block_table_to_block_end(Agent, Block1, Block2, X1, Y1, X2, Y2),
  [moving(Agent, Block1, X1, Y1, X2, Y2), at(Block2, X2, Y2), clear(Block2)],
  [],
  [],
  [agent(Agent), block(Block1), block(Block2)],
  [
    del(moving(Agent, Block1, X1, Y1, X2, Y2)), del(clear(Block2)),
    add(available(Agent)), add(on(Block1, Block2)), add(at(Block1, X2, Y2))
  ]
).

% Move a block from the top of a block to a position on the table
action(move_block_block_to_table_start(Agent, Block1, Block2, X1, Y1, X2, Y2),
  [available(Agent), on(Block1, Block2), at(Block1, X1, Y1), clear(Block1)],
  [at(_, X2, Y2)],
  [],
  [agent(Agent), pos(X2, Y2), block(Block1), block(Block2)],
  [
    del(available(Agent)), del(clear(Block1)), del(on(Block1, Block2)),
    add(moving(Agent, Block1, X1, Y1, X2, Y2)), add(clear(Block2))
  ]
).
action(move_block_block_to_table_end(Agent, Block1, Block2, X1, Y1, X2, Y2),
  [moving(Agent, Block1, X1, Y1, X2, Y2)],
  [at(_, X2, Y2)],
  [],
  [agent(Agent), pos(X2, Y2), block(Block1)],
  [
    del(moving(Agent, Block1, X1, Y1, X2, Y2)),
    add(available(Agent)), add(clear(Block1)), add(ontable(Block1)), add(at(Block1, X2, Y2))
  ]
).

% Move a block from the top of a block to the top of another block
action(move_block_block_to_block_start(Agent, Block1, Block2, Block3, X1, Y1, X2, Y2),
  [available(Agent), on(Block1, Block2), at(Block1, X1, Y1), clear(Block1), at(Block3, X2, Y2), clear(Block3)],
  [],
  [],
  [agent(Agent), block(Block1), block(Block2), block(Block3)],
  [
    del(available(Agent)), del(clear(Block1)), del(on(Block1, Block2)),
    add(moving(Agent, Block1, X1, Y1, X2, Y2)), add(clear(Block2))
  ]
).
action(move_block_block_to_block_end(Agent, Block1, Block2, Block3, X1, Y1, X2, Y2),
  [moving(Agent, Block1, X1, Y1, X2, Y2), at(Block3, X2, Y2), clear(Block3)],
  [],
  [],
  [agent(Agent), block(Block1), block(Block3)],
  [
    del(moving(Agent, Block1, X1, Y1, X2, Y2)), del(clear(Block3)),
    add(available(Agent)), add(on(Block1, Block3)), add(at(Block1, X2, Y2))
  ]
).


%%%%%%%%%%%%%%%%%%%%%%%
% ll_actions
%%%%%%%%%%%%%%%%%%%%%%%
% Move the arm of the agent from one position to another
ll_action(ll_move_arm(Agent, X1, Y1, X2, Y2),
  [ll_arm_at(Agent, X1, Y1)],
  [ll_arm_moving(Agent, _, _, _, _)],
  [],
  [ll_arm(Agent), pos(X1, Y1), pos(X2, Y2)],
  [
    del(ll_arm_at(Agent, X1, Y1)),
    add(ll_arm_moving(Agent, X1, Y1, X2, Y2))
  ]
).
ll_action(ll_move_arm_end(Agent, X1, Y1, X2, Y2),
  [ll_arm_moving(Agent, X1, Y1, X2, Y2)],
  [],
  [],
  [ll_arm(Agent), pos(X2, Y2)],
  [
    del(ll_arm_moving(Agent, X1, Y1, X2, Y2)),
    add(ll_arm_at(Agent, X2, Y2))
  ]
).

% Close the gripper of the agent
ll_action(ll_close_gripper(Agent),
  [ll_gripper(Agent, open)],
  [ll_gripper(Agent, close)],
  [],
  [ll_gripper(Agent)],
  [
    del(ll_gripper(Agent, open)),
    add(ll_gripper(Agent, close))
  ]
).

% Open the gripper of the agent
ll_action(ll_open_gripper(Agent),
  [ll_gripper(Agent, close)],
  [ll_gripper(Agent, open)],
  [],
  [ll_gripper(Agent)],
  [
    del(ll_gripper(Agent, close)),
    add(ll_gripper(Agent, open))
  ]
).

%%%%%%%%%%%%%%%%%%%%%%%
% mappings
%%%%%%%%%%%%%%%%%%%%%%%
mapping(move_block_table_to_table_start(Agent, Block, X1, Y1, X2, Y2),
  [
    ll_move_arm(Agent, X1, Y1, X2, Y2),
    ll_close_gripper(Agent),
    ll_open_gripper(Agent)
  ]
).

mapping(move_block_table_to_block_start(Agent, Block1, Block2, X1, Y1, X2, Y2),
  [
    ll_move_arm(Agent, X1, Y1, X2, Y2),
    ll_close_gripper(Agent),
    ll_open_gripper(Agent)
  ]
).

mapping(move_block_block_to_table_start(Agent, Block1, Block2, X1, Y1, X2, Y2),
  [
    ll_move_arm(Agent, X1, Y1, X2, Y2),
    ll_close_gripper(Agent),
    ll_open_gripper(Agent)
  ]
).

mapping(move_block_block_to_block_start(Agent, Block1, Block2, Block3, X1, Y1, X2, Y2),
  [
    ll_move_arm(Agent, X1, Y1, X2, Y2),
    ll_close_gripper(Agent),
    ll_open_gripper(Agent)
  ]
).
