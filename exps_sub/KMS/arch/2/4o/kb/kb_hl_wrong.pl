% This file was automatically generated by the LLM system
%%%%%%%%%%%%%%%%%%%%%%%
% kb
%%%%%%%%%%%%%%%%%%%%%%%
% Positions
pos(1,1).
pos(2,2).
pos(3,3).
pos(6,6).
pos(9,6).

% Blocks
block(b1).
block(b2).
architrave(b3).

% Agents
agent(arm).

% Resources
resources(agent(_)).

%%%%%%%%%%%%%%%%%%%%%%%
% init
%%%%%%%%%%%%%%%%%%%%%%%
init_state([
  ontable(b1), ontable(b2), ontable(b3),
  at(b1,1,1), at(b2,2,2), at(b3,3,3),
  clear(b1), clear(b2), clear(b3),
  available(arm)
]).

%%%%%%%%%%%%%%%%%%%%%%%
% goal
%%%%%%%%%%%%%%%%%%%%%%%
goal_state([
  ontable(b1), ontable(b2),
  on(b3, b1), on(b3, b2),
  at(b1,6,6), at(b2,9,6), at(b3,6,6),
  clear(b3),
  available(arm)
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

% Place the architrave on top of the two pillars
action(place_architrave_start(Agent, Arch, Pillar1, Pillar2, X1, Y1, X2, Y2),
  [available(Agent), ontable(Arch), at(Arch, X1, Y1), at(Pillar1, X2, Y2), at(Pillar2, X3, Y3), clear(Pillar1), clear(Pillar2)],
  [moving_architrave(_, Arch, _, _, _, _, _, _), on(_, Arch), ontable(Arch)],
  [],
  [agent(Agent), pos(X1, Y1), pos(X2, Y2), pos(X3, Y3), architrave(Arch), block(Pillar1), block(Pillar2), Pillar1 \= Pillar2],
  [
    del(available(Agent)), del(clear(Arch)), del(ontable(Arch)), del(at(Arch, X1, Y1)),
    add(moving_architrave(Agent, Arch, Pillar1, Pillar2, X1, Y1, X2, Y2, X3, Y3))
  ]
).
action(place_architrave_end(Agent, Arch, Pillar1, Pillar2, X1, Y1, X2, Y2, X3, Y3),
  [moving_architrave(Agent, Arch, Pillar1, Pillar2, X1, Y1, X2, Y2, X3, Y3), clear(Pillar1), clear(Pillar2)],
  [],
  [],
  [agent(Agent)],
  [
    del(clear(Pillar1)), del(clear(Pillar2)), del(moving_architrave(Agent, Arch, Pillar1, Pillar2, X1, Y1, X2, Y2, X3, Y3)),
    add(on(Arch, Pillar1)), add(on(Arch, Pillar2)), add(at(Arch, X2, Y2)), add(clear(Arch)), add(available(Agent))
  ]
).
