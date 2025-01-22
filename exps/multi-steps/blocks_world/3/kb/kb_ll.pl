% This file was automatically generated by the LLM system
%%%%%%%%%%%%%%%%%%%%%%%
% kb
%%%%%%%%%%%%%%%%%%%%%%%
% Positions
pos(1,1).
pos(2,1).
pos(3,1).

% Blocks
block(a).
block(b).
block(c).
block(d).

% Agents
agent(agent1).
agent(agent2).

% Low-level predicates for arms and grippers
ll_arm(agent1).
ll_arm(agent2).

ll_gripper(agent1).
ll_gripper(agent2).

% Resources
resources(agent(_)).
resources(ll_arm(_)).
resources(ll_gripper(_)).

%%%%%%%%%%%%%%%%%%%%%%%
% init
%%%%%%%%%%%%%%%%%%%%%%%
init_state([
  ontable(a),
  on(b, a), on(c, b), on(d, c),
  at(a, 1, 1), at(b, 1, 1), at(c, 1, 1), at(d, 1, 1),
  clear(d),
  available(agent1), available(agent2),
  ll_arm_at(agent1, 0, 0), ll_arm_at(agent2, 3, 3),
  ll_gripper(agent1, open), ll_gripper(agent2, open)
]).

%%%%%%%%%%%%%%%%%%%%%%%
% goal
%%%%%%%%%%%%%%%%%%%%%%%
goal_state([
  ontable(a), ontable(b), ontable(c),
  on(d, a),
  at(a, 1, 1), at(b, 2, 1), at(c, 3, 1), at(d, 1, 1),
  clear(b), clear(c), clear(d),
  available(agent1), available(agent2),
  ll_arm_at(agent1, _, _), ll_arm_at(agent2, _, _),
  ll_gripper(agent1, _), ll_gripper(agent2, _)
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
  [at(X2, Y2)],
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
ll_action(move_arm_start(Arm, X1, Y1, X2, Y2),
  [ll_arm_at(Arm, X1, Y1)],
  [ll_moving_arm(Arm, _, _, _, _), ll_gripping(Arm, _), ll_releasing(Arm)],
  [],
  [ll_arm(Arm), pos(X1, Y1), pos(X2, Y2)],
  [
    add(ll_moving_arm(Arm, X1, Y1, X2, Y2)),
    del(ll_arm_at(Arm, X1, Y1))
  ]
).

ll_action(move_arm_end(Arm, X1, Y1, X2, Y2),
  [ll_moving_arm(Arm, X1, Y1, X2, Y2)],
  [],
  [],
  [],
  [
    del(ll_moving_arm(Arm, X1, Y1, X2, Y2)),
    add(ll_arm_at(Arm, X2, Y2))
  ]
).

ll_action(close_start(Arm),
  [ll_gripper(Arm, open)],
  [ll_moving_arm(Arm, _, _, _, _), ll_gripping(Arm, _), ll_releasing(Arm), ll_gripped(Arm)],
  [],
  [ll_gripper(Arm)],
  [
    del(ll_gripper(Arm, open)),
    add(ll_gripping(Arm))
  ]
).

ll_action(close_end(Arm),
  [ll_gripping(Arm)],
  [],
  [],
  [ll_gripper(Arm)],
  [
    del(ll_gripping(Arm)),
    add(ll_gripped(Arm)), add(ll_gripper(Arm, close))
  ]
).

ll_action(open_start(Arm),
  [ll_gripper(Arm, close)],
  [ll_moving_arm(Arm, _, _, _, _), ll_gripping(Arm, _), ll_releasing(Arm)],
  [],
  [ll_gripper(Arm)],
  [
    del(ll_gripper(Arm, close)),
    add(ll_releasing(Arm))
  ]
).

ll_action(open_end(Arm),
  [ll_releasing(Arm)],
  [],
  [],
  [ll_gripper(Arm)],
  [
    del(ll_releasing(Arm)),
    add(ll_gripper(Arm, open))
  ]
).

%%%%%%%%%%%%%%%%%%%%%%%
% mappings
%%%%%%%%%%%%%%%%%%%%%%%
mapping(move_table_to_table_start(Agent, Block, X1, Y1, X2, Y2),
  [
    move_arm_start(Agent, X1, Y1, X1, Y1),
    move_arm_end(Agent, X1, Y1, X1, Y1),
    close_start(Agent),
    close_end(Agent),
    move_arm_start(Agent, X1, Y1, X2, Y2),
    move_arm_end(Agent, X1, Y1, X2, Y2),
    open_start(Agent),
    open_end(Agent)
  ]
).

mapping(move_table_to_block_start(Agent, Block1, Block2, X1, Y1, X2, Y2),
  [
    move_arm_start(Agent, X1, Y1, X1, Y1),
    move_arm_end(Agent, X1, Y1, X1, Y1),
    close_start(Agent),
    close_end(Agent),
    move_arm_start(Agent, X1, Y1, X2, Y2),
    move_arm_end(Agent, X1, Y1, X2, Y2),
    open_start(Agent),
    open_end(Agent)
  ]
).

mapping(move_onblock_to_table_start(Agent, Block1, X1, Y1, X2, Y2),
  [
    move_arm_start(Agent, X1, Y1, X1, Y1),
    move_arm_end(Agent, X1, Y1, X1, Y1),
    close_start(Agent),
    close_end(Agent),
    move_arm_start(Agent, X1, Y1, X2, Y2),
    move_arm_end(Agent, X1, Y1, X2, Y2),
    open_start(Agent),
    open_end(Agent)
  ]
).

mapping(move_onblock_to_block_start(Agent, Block1, Block2, X1, Y1, X2, Y2),
  [
    move_arm_start(Agent, X1, Y1, X1, Y1),
    move_arm_end(Agent, X1, Y1, X1, Y1),
    close_start(Agent),
    close_end(Agent),
    move_arm_start(Agent, X1, Y1, X2, Y2),
    move_arm_end(Agent, X1, Y1, X2, Y2),
    open_start(Agent),
    open_end(Agent)
  ]
).
