:- dynamic agent/1.
:- dynamic ll_arm/1.
:- dynamic ll_gripper/1.

% Positions
pos(32,60).
pos(113,83).
pos(44,40).
pos(86,52).
pos(72,52).
pos(79,52).
pos(4,4).
pos(5,5).

% Blocks
block(b1).
block(b2).
arch(b3).

% Agents
agent(a1).
agent(a2).

% Low-level predicates for arms and grippers
ll_arm(a1).
ll_arm(a2).

ll_gripper(a1).
ll_gripper(a2).

% Resources
resources(agent(_)).
resources(ll_arm(_)).
resources(ll_gripper(_)).


init_state([
  ontable(b1), ontable(b2), ontable(b3),
  at(b1,32,60), at(b2,113,83), at(b3,44,40),
  clear(b1), clear(b2), clear(b3),
  available(a1), available(a2),
  ll_arm_at(a1,4,4,_), ll_arm_at(a2,5,5,_),
  ll_gripper(a1,open), ll_gripper(a2,open)
]).

goal_state([
  ontable(b1), ontable(b2),
  on(b3, b1), on(b3, b2),
  at(b1,86,52), at(b2,72,52), at(b3,79,52),
  clear(b3),
  available(a1), available(a2),
  ll_arm_at(a1,_,_,_), ll_arm_at(a2,_,_,_),
  ll_gripper(a1,_), ll_gripper(a2,_)
]).

          
% Move a block from a position on the table to another position on the table
action(move_table_to_table_start(Agent, Block, X1, Y1, X2, Y2), 
  [ontable(Block), at(Block, X1, Y1), available(Agent), clear(Block)],
  [at(_, X2, Y2), on(Block, _), moving_table_to_table(_, Block, _, _, _, _), moving_table_to_block(_, Block, _, _, _, _, _)],
  [],
  [agent(Agent), pos(X1, Y1), pos(X2, Y2), block(Block), X1\=X2, Y1\=Y2],
  [
    del(available(Agent)), del(clear(Block)), del(ontable(Block)), del(at(Block, X1, Y1)),
    add(moving_table_to_table(Agent, Block, X1, Y1, X2, Y2))
  ]
).
action(move_table_to_table_end(Agent, Block, X1, Y1, X2, Y2),
  [moving_table_to_table(Agent, Block, X1, Y1, X2, Y2)],
  [at(_, X2, Y2)],
  [],
  [],
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

% Place the architrave on top of the two pillars
action(place_architrave_start(Agent, Arch, Block1, Block2, FromArchX, FromArchY, ToArchX, ToArchY, Block1X, Block1Y, Block2X, Block2Y),
  [available(Agent), ontable(Arch), at(Arch, FromArchX, FromArchY), at(Block1, Block1X, Block1Y), at(Block2, Block2X, Block2Y), clear(Block1), clear(Block2)],
  [moving_table_to_table(_, Arch, _, _, _, _), moving_table_to_block(_, Arch, _, _, _, _, _), moving_onblock_to_table(_, Arch, _, _, _, _), moving_onblock_to_block(_, Arch, _, _, _, _, _)],
  [],
  [ 
    agent(Agent), arch(Arch),
    pos(ToArchX, ToArchY), FromArchX \= ToArchX, FromArchY \= ToArchY
  ],
  [
    del(available(Agent)), del(clear(Arch)), del(ontable(Arch)), del(at(Arch, FromArchX, FromArchY)), del(clear(Block1)), del(clear(Block2)),
    add(placing_arch(Agent, Arch, Block1, Block2, FromArchX, FromArchY, ToArchX, ToArchY, Block1X, Block1Y, Block2X, Block2Y))
  ]
).
action(place_architrave_end(Agent, Arch, Block1, Block2, FromArchX, FromArchY, ToArchX, ToArchY, Block1X, Block1Y, Block2X, Block2Y),
  [placing_arch(Agent, Arch, Block1, Block2, FromArchX, FromArchY, ToArchX, ToArchY, Block1X, Block1Y, Block2X, Block2Y)],
  [at(_, ToArchX, ToArchY)],
  [],
  [],
  [
    del(placing_arch(Agent, Arch, Block1, Block2, FromArchX, FromArchY, ToArchX, ToArchY, Block1X, Block1Y, Block2X, Block2Y)),
    add(on(Arch, Block1)), add(on(Arch, Block2)), add(at(Arch, ToArchX, ToArchY)), add(clear(Arch)), add(available(Agent))
  ]
).

ll_action(ll_move_arm_start(Agent, X, Y, Z),
  [],
  [ll_arm_at(_, X, Y, Z)],
  [],
  [agent(Agent), pos(X, Y), ll_arm(Agent)],
  [
    add(ll_moving_arm(Agent, X, Y, Z))
  ]
).

ll_action(ll_move_arm_end(Agent, X, Y, Z),
  [ll_moving_arm(Agent, X, Y, Z)],
  [ll_arm_at(_, X, Y, Z)],
  [],
  [],
  [
    del(ll_moving_arm(Agent, X, Y, Z)),
    add(ll_arm_at(Agent, X, Y, Z))
  ]
).

ll_action(ll_close_gripper_start(Agent),
  [ll_gripper(Agent, open)],
  [],
  [],
  [agent(Agent), ll_gripper(Agent)],
  [
    del(ll_gripper(Agent, open)),
    add(ll_closing_gripper(Agent))
  ]
).

ll_action(ll_close_gripper_end(Agent),
  [ll_closing_gripper(Agent)],
  [],
  [],
  [],
  [
    del(ll_closing_gripper(Agent)),
    add(ll_gripper(Agent, closed))
  ]
).

ll_action(ll_open_gripper_start(Agent),
  [ll_gripper(Agent, closed)],
  [],
  [],
  [agent(Agent), ll_gripper(Agent)],
  [
    del(ll_gripper(Agent, closed)),
    add(ll_opening_gripper(Agent))
  ]
).

ll_action(ll_open_gripper_end(Agent),
  [ll_opening_gripper(Agent)],
  [],
  [],
  [],
  [
    del(ll_opening_gripper(Agent)),
    add(ll_gripper(Agent, open))
  ]
).


mapping(move_table_to_table_start(Agent, Block, X1, Y1, X2, Y2),
  [
    ll_move_arm_start(Agent, X1, Y1, 0),
    ll_move_arm_end(Agent, X1, Y1, 0),
    ll_close_gripper_start(Agent),
    ll_close_gripper_end(Agent),
    ll_move_arm_start(Agent, X2, Y2, 0),
    ll_move_arm_end(Agent, X2, Y2, 0),
    ll_open_gripper_start(Agent),
    ll_open_gripper_end(Agent)
  ]
).

mapping(move_table_to_block_start(Agent, _Block1, _Block2, X1, Y1, X2, Y2),
  [
    ll_move_arm_start(Agent, X1, Y1, 0),
    ll_move_arm_end(Agent, X1, Y1, 0),
    ll_close_gripper_start(Agent),
    ll_close_gripper_end(Agent),
    ll_move_arm_start(Agent, X2, Y2, 0),
    ll_move_arm_end(Agent, X2, Y2, 0),
    ll_open_gripper_start(Agent),
    ll_open_gripper_end(Agent)
  ]
).

mapping(move_onblock_to_table_start(Agent, _Block, X1, Y1, X2, Y2),
  [
    ll_move_arm_start(Agent, X1, Y1, 0),
    ll_move_arm_end(Agent, X1, Y1, 0),
    ll_close_gripper_start(Agent),
    ll_close_gripper_end(Agent),
    ll_move_arm_start(Agent, X2, Y2, 0),
    ll_move_arm_end(Agent, X2, Y2, 0),
    ll_open_gripper_start(Agent),
    ll_open_gripper_end(Agent)
  ]
).

mapping(move_onblock_to_block_start(Agent, _Block1, _Block2, X1, Y1, X2, Y2),
  [
    ll_move_arm_start(Agent, X1, Y1, 0),
    ll_move_arm_end(Agent, X1, Y1, 0),
    ll_close_gripper_start(Agent),
    ll_close_gripper_end(Agent),
    ll_move_arm_start(Agent, X2, Y2, 0),
    ll_move_arm_end(Agent, X2, Y2, 0),
    ll_open_gripper_start(Agent),
    ll_open_gripper_end(Agent)
  ]
).

mapping(place_architrave_start(Agent, _Arch, _Block1, _Block2, FromArchX, FromArchY, ToArchX, ToArchY, _Block1X, _Block1Y, _Block2X, _Block2Y),
  [
    ll_move_arm_start(Agent, FromArchX, FromArchY, 0),
    ll_move_arm_end(Agent, FromArchX, FromArchY, 0),
    ll_close_gripper_start(Agent),
    ll_close_gripper_end(Agent),
    ll_move_arm_start(Agent, ToArchX, ToArchY, 1),
    ll_move_arm_end(Agent, ToArchX, ToArchY, 1),
    ll_open_gripper_start(Agent),
    ll_open_gripper_end(Agent)
  ]
).

% :- dynamic agent/1.


          
% % Move a block from a position on the table to another position on the table
% action(move_table_to_table_start(Agent, Block, X1, Y1, X2, Y2), 
%   [ontable(Block), at(Block, X1, Y1), available(Agent), clear(Block)],
%   [at(_, X2, Y2), on(Block, _), moving_table_to_table(_, Block, _, _, _, _), moving_table_to_block(_, Block, _, _, _, _, _)],
%   [],
%   [agent(Agent), pos(X1, Y1), pos(X2, Y2), block(Block), X1\=X2, Y1\=Y2],
%   [
%     del(available(Agent)), del(clear(Block)), del(ontable(Block)), del(at(Block, X1, Y1)),
%     add(moving_table_to_table(Agent, Block, X1, Y1, X2, Y2))
%   ]
% ).
% action(move_table_to_table_end(Agent, Block, X1, Y1, X2, Y2),
%   [moving_table_to_table(Agent, Block, X1, Y1, X2, Y2)],
%   [at(_, X2, Y2)],
%   [],
%   [],
%   [
%     del(moving_table_to_table(Agent, Block, X1, Y1, X2, Y2)),
%     add(ontable(Block)), add(at(Block, X2, Y2)), add(clear(Block)), add(available(Agent))
%   ]
% ).

% % Move a block from a position on the table to the top of another block
% action(move_table_to_block_start(Agent, Block1, Block2, X1, Y1, X2, Y2),
%   [available(Agent), ontable(Block1), at(Block1, X1, Y1), at(Block2, X2, Y2), clear(Block2), clear(Block1)],
%   [on(_, Block1), on(Block1, _), moving_table_to_table(_, Block, _, _, _, _), moving_table_to_block(_, Block, _, _, _, _, _)],
%   [],
%   [agent(Agent), pos(X1, Y1), pos(X2, Y2), block(Block1), block(Block2), Block1 \= Block2],
%   [
%     del(available(Agent)), del(clear(Block1)), del(ontable(Block1)), del(at(Block1, X1, Y1)),
%     add(moving_table_to_block(Agent, Block1, Block2, X1, Y1, X2, Y2))
%   ]
% ).
% action(move_table_to_block_end(Agent, Block1, Block2, X1, Y1, X2, Y2),
%   [moving_table_to_block(Agent, Block1, Block2, X1, Y1, X2, Y2), clear(Block2)],
%   [],
%   [],
%   [agent(Agent)],
%   [
%     del(clear(Block2)), del(moving_table_to_block(Agent, Block1, Block2, X1, Y1, X2, Y2)),
%     add(on(Block1, Block2)), add(at(Block1, X2, Y2)), add(clear(Block1)), add(available(Agent))
%   ]
% ).

% % Move a block from the top of another block to a position on the table
% action(move_onblock_to_table_start(Agent, Block1, X1, Y1, X2, Y2),
%   [available(Agent), on(Block1, Block2), at(Block1, X1, Y1), at(Block2, X1, Y1), clear(Block1)],
%   [moving_onblock_to_table(_, Block1, _, _, _, _), on(_, Block1), ontable(Block1), at(_, X2, Y2)],
%   [],
%   [agent(Agent), pos(X1, Y1), pos(X2, Y2), block(Block1), block(Block2), Block1 \= Block2],
%   [
%     del(available(Agent)), del(clear(Block1)), del(on(Block1, Block2)), del(at(Block1, X1, Y1)),
%     add(moving_onblock_to_table(Agent, Block1, X1, Y1, X2, Y2)), add(clear(Block2))
%   ]
% ).
% action(move_onblock_to_table_end(Agent, Block1, X1, Y1, X2, Y2),
%   [moving_onblock_to_table(Agent, Block1, X1, Y1, X2, Y2)],
%   [at(_, X2, Y2)],
%   [],
%   [agent(Agent)],
%   [
%     del(moving_onblock_to_table(Agent, Block1, X1, Y1, X2, Y2)),
%     add(ontable(Block1)), add(at(Block1, X2, Y2)), add(clear(Block1)), add(available(Agent))
%   ]
% ).

% % Move a block from the top of another block to the top of another block
% action(move_onblock_to_block_start(Agent, Block1, Block2, X1, Y1, X2, Y2),
%   [available(Agent), on(Block1, Block3), at(Block1, X1, Y1), at(Block2, X2, Y2), clear(Block2), clear(Block1)],
%   [moving_onblock_to_block(_, Block1, _, _, _, _), on(_, Block1), ontable(Block1)],
%   [],
%   [agent(Agent), pos(X1, Y1), pos(X2, Y2), block(Block1), block(Block2), block(Block3), Block1 \= Block2, Block1 \= Block3, Block2 \= Block3],
%   [
%     del(available(Agent)), del(clear(Block1)), del(on(Block1, Block3)), del(at(Block1, X1, Y1)),
%     add(moving_onblock_to_block(Agent, Block1, Block2, X1, Y1, X2, Y2)), add(clear(Block3))
%   ]
% ).
% action(move_onblock_to_block_end(Agent, Block1, Block2, X1, Y1, X2, Y2),
%   [moving_onblock_to_block(Agent, Block1, Block2, X1, Y1, X2, Y2), clear(Block2)],
%   [],
%   [],
%   [agent(Agent)],
%   [
%     del(clear(Block2)), del(moving_onblock_to_block(Agent, Block1, Block2, X1, Y1, X2, Y2)),
%     add(on(Block1, Block2)), add(at(Block1, X2, Y2)), add(clear(Block1)), add(available(Agent))
%   ]
% ).

% % Place the architrave on top of the two pillars
% action(place_architrave_start(Agent, Arch, Block1, Block2, FromArchX, FromArchY, ToArchX, ToArchY, Block1X, Block1Y, Block2X, Block2Y),
%   [available(Agent), ontable(Arch), at(Arch, FromArchX, FromArchY), at(Block1, Block1X, Block1Y), at(Block2, Block2X, Block2Y), clear(Block1), clear(Block2)],
%   [moving_table_to_table(_, Arch, _, _, _, _), moving_table_to_block(_, Arch, _, _, _, _, _), moving_onblock_to_table(_, Arch, _, _, _, _), moving_onblock_to_block(_, Arch, _, _, _, _, _)],
%   [],
%   [ 
%     agent(Agent), arch(Arch),
%     pos(ToArchX, ToArchY), FromArchX \= ToArchX, FromArchY \= ToArchY
%   ],
%   [
%     del(available(Agent)), del(clear(Arch)), del(ontable(Arch)), del(at(Arch, FromArchX, FromArchY)), del(clear(Block1)), del(clear(Block2)),
%     add(placing_arch(Agent, Arch, Block1, Block2, FromArchX, FromArchY, ToArchX, ToArchY, Block1X, Block1Y, Block2X, Block2Y))
%   ]
% ).
% action(place_architrave_end(Agent, Arch, Block1, Block2, FromArchX, FromArchY, ToArchX, ToArchY, Block1X, Block1Y, Block2X, Block2Y),
%   [placing_arch(Agent, Arch, Block1, Block2, FromArchX, FromArchY, ToArchX, ToArchY, Block1X, Block1Y, Block2X, Block2Y)],
%   [at(_, ToArchX, ToArchY)],
%   [],
%   [],
%   [
%     del(placing_arch(Agent, Arch, Block1, Block2, FromArchX, FromArchY, ToArchX, ToArchY, Block1X, Block1Y, Block2X, Block2Y)),
%     add(on(Arch, Block1)), add(on(Arch, Block2)), add(at(Arch, ToArchX, ToArchY)), add(clear(Arch)), add(available(Agent))
%   ]
% ).