# Report Experiment BW - 4o - 3 

## Consistency Check

The LLM did not detect (correctly) any inconsistency in the descriptions.

## High-Level Generation

The LLM produced all the 4 parts required for the HL KB. 

The general KB seems to correctly capture all the necessary predicates and also the resources. 

The initial state correctly captures the position of the blocks on the table and the fact that both
agents are available.

The goal state correctly describe the movement of the block from the table to the top of another 
block.

The LLM generated all the actions dividing them in start and final actions. By running the HL
planner there seems to not be any error as it finds the correct solution in 1.5s. It is worth
mentioning that the plan is not optimal, indeed, in order to place D on top of A the planner moves 
first the block in a position and then moves the block D back on top of A. This is because the 
`move_onblock_to_block_start` action require A to be free, which is not at the beginning of the
first action that moves D.

The solution was found in 27.462s.

## Low-Level Generation

When adding the low-level description, the LLM made two mistakes:

- The mapping for `move_*` is not completely correct. For example, the HL action
  `move_table_to_table_start` was initially mapped to 
  ```prolog
  mapping(move_table_to_table_start(Agent, Block, X1, Y1, X2, Y2),
    [
        move_arm_start(Agent, X1, Y1, X1, Y1),
        move_arm_end(Agent, X1, Y1, X1, Y1),
        close_gripper_start(Agent, Block),
        close_gripper_end(Agent, Block),
        move_arm_start(Agent, X1, Y1, X2, Y2),
        move_arm_end(Agent, X1, Y1, X2, Y2),
        open_gripper_start(Agent),
        open_gripper_end(Agent)
    ]
  ).
  ``` 
- It used an additional predicate `ll_gripped(Arm)`, which is redundant since 
  `ll_gripper(Arm, close)` and `ll_gripper(Arm, open)` already take care of this. Also, it was added
  in one action, required as a precondition in another, but never removed. Hence there are two
  possible solutions:

    - either remove `ll_gripped(Arm)`, or
    - add also the predicates to remove the predicate once the gripper is opened.

The solution was found in 22.486s.
