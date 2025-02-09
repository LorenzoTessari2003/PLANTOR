% This file was automatically generated by the LLM system
%%%%%%%%%%%%%%%%%%%%%%%
% kb
%%%%%%%%%%%%%%%%%%%%%%%
% Positions
pos(1,1).
pos(2,2).
pos(3,3).
pos(10,10).
pos(4,4).
pos(5,5).

% Blocks
block(b1).
block(b2).
block(b3).
block(b4).
block(b5).
block(b6).

% Agents
agent(a1).
agent(a2).

% Arms
ll_arm(a1).
ll_arm(a2).

% Grippers
ll_gripper(a1).
ll_gripper(a2).

% Resources
resources(agent(_)).
resources(ll_arm(_)).
resources(ll_gripper(_)).

%%%%%%%%%%%%%%%%%%%%%%%
% init
%%%%%%%%%%%%%%%%%%%%%%%
init_state([
  ontable(b1), ontable(b2), ontable(b3),
  on(b4, b1), on(b5, b2), on(b6, b3),
  at(b1,1,1), at(b2,2,2), at(b3,3,3), at(b4,1,1), at(b5,2,2), at(b6,3,3),
  clear(b4), clear(b5), clear(b6),
  available(a1), available(a2),
  ll_arm_at(a1,4,4), ll_arm_at(a2,5,5),
  ll_gripper(a1,open), ll_gripper(a2,open)
]).

%%%%%%%%%%%%%%%%%%%%%%%
% goal
%%%%%%%%%%%%%%%%%%%%%%%
goal_state([
  ontable(b1), ontable(b2), ontable(b3), ontable(b4),
  on(b5, b4), on(b6, b3),
  at(b1,1,1), at(b2,2,2), at(b3,3,3), at(b4,10,10), at(b5,10,10), at(b6,3,3),
  clear(b1), clear(b2), clear(b5), clear(b6),
  available(a1), available(a2),
  ll_arm_at(a1,_,_), ll_arm_at(a2,_,_),
  ll_gripper(a1,_), ll_gripper(a2,_)
]).

%%%%%%%%%%%%%%%%%%%%%%%
% actions
%%%%%%%%%%%%%%%%%%%%%%%
% Move a block from a position on the table to another position on the table
action(move_table_to_table_start(Agent, Block, X1, Y1, X2, Y2), 
  [ontable(Block), at(Block, X1, Y1), available(Agent), clear(Block)],
  [at(_, X2, Y2), on(Block, _), moving_table_to_table(_, Block, _, _, _, _), moving_table_to_block(_, Block, _, _, _, _, _)],
  [],
  [agent(Agent), pos(X1, Y1), pos(X2, Y2), block(Block)],
  [
    del(available(Agent)), del(clear(Block)), del(ontable(Block)), del(at(Block, X1, Y1)),
    add(moving_table_to_table(Agent, Block, X1, Y1, X2, Y2))
  ]
).
action(move_table_to_table_end(Agent, Block, X1, Y1, X2, Y2),
  [moving_table_to_table(Agent, Block, X1, Y1, X2, Y2)],
  [at(_, X2, Y2)],
  [],
  [agent(Agent)],
  [
    del(moving_table_to_table(Agent, Block, X1, Y1, X2, Y2)),
    add(ontable(Block)), add(at(Block, X2, Y2)), add(clear(Block)), add(available(Agent))
  ]
).

% Move a block from a position on the table to the top of another block
action(move_table_to_block_start(Agent, Block1, Block2, X1, Y1, X2, Y2),
  [available(Agent), ontable(Block1), at(Block1, X1, Y1), at(Block2, X2, Y2), clear(Block2), clear(Block1)],
  [on(_, Block1), on(Block1, _), moving_table_to_table(_, Block, _, _, _, _), moving_table_to_block(_, Block, _, _, _, _, _)],
  [],
  [agent(Agent), pos(X1, Y1), pos(X2, Y2), block(Block1), block(Block2), Block1 \= Block2],
  [
    del(available(Agent)), del(clear(Block1)), del(ontable(Block1)), del(at(Block1, X1, Y1)),
    add(moving_table_to_block(Agent, Block1, Block2, X1, Y1, X2, Y2))
  ]
).
action(move_table_to_block_end(Agent, Block1, Block2, X1, Y1, X2, Y2),
  [moving_table_to_block(Agent, Block1, Block2, X1, Y1, X2, Y2), clear(Block2)],
  [],
  [],
  [agent(Agent)],
  [
    del(clear(Block2)), del(moving_table_to_block(Agent, Block1, Block2, X1, Y1, X2, Y2)),
    add(on(Block1, Block2)), add(at(Block1, X2, Y2)), add(clear(Block1)), add(available(Agent))
  ]
).

% Move a block from the top of another block to a position on the table
action(move_onblock_to_table_start(Agent, Block1, X1, Y1, X2, Y2),
  [available(Agent), on(Block1, Block2), at(Block1, X1, Y1), at(Block2, X1, Y1), clear(Block1)],
  [moving_onblock_to_table(_, Block1, _, _, _, _), on(_, Block1), ontable(Block1), at(_, X2, Y2)],
  [],
  [agent(Agent), pos(X1, Y1), pos(X2, Y2), block(Block1), block(Block2), Block1 \= Block2],
  [
    del(available(Agent)), del(clear(Block1)), del(on(Block1, Block2)), del(at(Block1, X1, Y1)),
    add(moving_onblock_to_table(Agent, Block1, X1, Y1, X2, Y2)), add(clear(Block2))
  ]
).
action(move_onblock_to_table_end(Agent, Block1, X1, Y1, X2, Y2),
  [moving_onblock_to_table(Agent, Block1, X1, Y1, X2, Y2)],
  [at(_, X2, Y2)],
  [],
  [agent(Agent)],
  [
    del(moving_onblock_to_table(Agent, Block1, X1, Y1, X2, Y2)),
    add(ontable(Block1)), add(at(Block1, X2, Y2)), add(clear(Block1)), add(available(Agent))
  ]
).

% Move a block from the top of another block to the top of another block
action(move_onblock_to_block_start(Agent, Block1, Block2, X1, Y1, X2, Y2),
  [available(Agent), on(Block1, Block3), at(Block1, X1, Y1), at(Block2, X2, Y2), clear(Block2), clear(Block1)],
  [moving_onblock_to_block(_, Block1, _, _, _, _), on(_, Block1), ontable(Block1)],
  [],
  [agent(Agent), pos(X1, Y1), pos(X2, Y2), block(Block1), block(Block2), block(Block3), Block1 \= Block2, Block1 \= Block3, Block2 \= Block3],
  [
    del(available(Agent)), del(clear(Block1)), del(on(Block1, Block3)), del(at(Block1, X1, Y1)),
    add(moving_onblock_to_block(Agent, Block1, Block2, X1, Y1, X2, Y2)), add(clear(Block3))
  ]
).
action(move_onblock_to_block_end(Agent, Block1, Block2, X1, Y1, X2, Y2),
  [moving_onblock_to_block(Agent, Block1, Block2, X1, Y1, X2, Y2), clear(Block2)],
  [],
  [],
  [agent(Agent)],
  [
    del(clear(Block2)), del(moving_onblock_to_block(Agent, Block1, Block2, X1, Y1, X2, Y2)),
    add(on(Block1, Block2)), add(at(Block1, X2, Y2)), add(clear(Block1)), add(available(Agent))
  ]
).

%%%%%%%%%%%%%%%%%%%%%%%
% ll_actions
%%%%%%%%%%%%%%%%%%%%%%%
% Move the arm from one position to another
ll_action(ll_move_arm_start(Agent, X1, Y1, X2, Y2),
  [ll_arm_at(Agent, X1, Y1)],
  [ll_moving_arm(Agent, _, _, _, _), ll_gripping(Agent, _), ll_releasing(Agent)],
  [],
  [ll_arm(Agent), pos(X1, Y1), pos(X2, Y2)],
  [
    add(ll_moving_arm(Agent, X1, Y1, X2, Y2)),
    del(ll_arm_at(Agent, X1, Y1))
  ]
).
ll_action(ll_move_arm_end(Agent, X1, Y1, X2, Y2),
  [ll_moving_arm(Agent, X1, Y1, X2, Y2)],
  [],
  [],
  [],
  [
    del(ll_moving_arm(Agent, X1, Y1, X2, Y2)),
    add(ll_arm_at(Agent, X2, Y2))
  ]
).

% Close the gripper
ll_action(ll_close_gripper_start(Agent),
  [ll_gripper(Agent, open)],
  [ll_moving_arm(Agent, _, _, _, _), ll_gripping(Agent, _), ll_releasing(Agent)],
  [],
  [ll_gripper(Agent)],
  [
    del(ll_gripper(Agent, open)),
    add(ll_gripping(Agent, _))
  ]
).
ll_action(ll_close_gripper_end(Agent),
  [ll_gripping(Agent, _)],
  [],
  [],
  [ll_gripper(Agent)],
  [
    del(ll_gripping(Agent, _)),
    add(ll_gripper(Agent, close))
  ]
).

% Open the gripper
ll_action(ll_open_gripper_start(Agent),
  [ll_gripper(Agent, close)],
  [ll_moving_arm(Agent, _, _, _, _), ll_gripping(Agent, _), ll_releasing(Agent)],
  [],
  [ll_gripper(Agent)],
  [
    del(ll_gripper(Agent, close)),
    add(ll_releasing(Agent))
  ]
).
ll_action(ll_open_gripper_end(Agent),
  [ll_releasing(Agent)],
  [],
  [],
  [ll_gripper(Agent)],
  [
    del(ll_releasing(Agent)),
    add(ll_gripper(Agent, open))
  ]
).

%%%%%%%%%%%%%%%%%%%%%%%
% mappings
%%%%%%%%%%%%%%%%%%%%%%%
mapping(move_table_to_table_start(Agent, Block, X1, Y1, X2, Y2),
  [
    ll_move_arm_start(Agent, 4, 4, X1, Y1),
    ll_move_arm_end(Agent, 4, 4, X1, Y1),
    ll_close_gripper_start(Agent),
    ll_close_gripper_end(Agent),
    ll_move_arm_start(Agent, X1, Y1, X2, Y2),
    ll_move_arm_end(Agent, X1, Y1, X2, Y2),
    ll_open_gripper_start(Agent),
    ll_open_gripper_end(Agent)
  ]
).

mapping(move_table_to_block_start(Agent, Block1, Block2, X1, Y1, X2, Y2),
  [
    ll_move_arm_start(Agent, 4, 4, X1, Y1),
    ll_move_arm_end(Agent, 4, 4, X1, Y1),
    ll_close_gripper_start(Agent),
    ll_close_gripper_end(Agent),
    ll_move_arm_start(Agent, X1, Y1, X2, Y2),
    ll_move_arm_end(Agent, X1, Y1, X2, Y2),
    ll_open_gripper_start(Agent),
    ll_open_gripper_end(Agent)
  ]
).

mapping(move_onblock_to_table_start(Agent, Block1, X1, Y1, X2, Y2),
  [
    ll_move_arm_start(Agent, 4, 4, X1, Y1),
    ll_move_arm_end(Agent, 4, 4, X1, Y1),
    ll_close_gripper_start(Agent),
    ll_close_gripper_end(Agent),
    ll_move_arm_start(Agent, X1, Y1, X2, Y2),
    ll_move_arm_end(Agent, X1, Y1, X2, Y2),
    ll_open_gripper_start(Agent),
    ll_open_gripper_end(Agent)
  ]
).

mapping(move_onblock_to_block_start(Agent, Block1, Block2, X1, Y1, X2, Y2),
  [
    ll_move_arm_start(Agent, 4, 4, X1, Y1),
    ll_move_arm_end(Agent, 4, 4, X1, Y1),
    ll_close_gripper_start(Agent),
    ll_close_gripper_end(Agent),
    ll_move_arm_start(Agent, X1, Y1, X2, Y2),
    ll_move_arm_end(Agent, X1, Y1, X2, Y2),
    ll_open_gripper_start(Agent),
    ll_open_gripper_end(Agent)
  ]
).
