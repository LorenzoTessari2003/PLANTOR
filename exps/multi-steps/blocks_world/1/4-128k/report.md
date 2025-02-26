# Report Experiment BW - 4 128K - 1

## Consistency Check

The LLM was not able to correctly assert that the query is consistent. It provides the following 
description of the inconsistency:

```
However, the low-level description introduces a problem. The end-effectors of the agents are 
initially in positions (4,4) and (5,5), which are not the initial positions of any of the blocks. 
The low-level actions provided (move_arm, close_gripper, open_gripper) do not include an action to 
pick up a block, which is necessary to move block b3 on top of block b1. The APIs provided are 
insufficient to complete the task because they lack a specific action to attach or detach the block 
to the gripper. Without this, the agents cannot move the blocks as required by the high-level plan.
```

## High-Level KB Generation

While GPT4-128K was able to generate the 4 required pieces, the predicates were wrong. The scenario 
consisted on moving one block from the table to the top of another block. By only focusing on the 
action required for this movement, the LLM produced the following start and end actions:

```prolog
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
```

This will not plan correctly because:

- The predicate `moving` does not account for:
  - the name of the second block `Block2`;
  - the initial position of `Block1`-the block that must be put on top of the other-and the final 
  position
  - the same predicate is shared between a number of different actions, meaning that one start
  action may be closed by another end action. 
- The action is not checking to make sure that `pos(X1,Y1)` and `pos(X2, Y2)` are present in the 
  general knowledge-base.

## Low-Level KB Generation

The generation of the LL KB was not successful. Multiple errors are present:

- The same errors present in the HL KB have been carried out also on the low-level part.
- Mappings are wrong
- Missing division in temporal actions for `open_gripper` and `close_gripper`