# Report Experiment 1 Arch - 4o - 1

## Consistency Check

The LLM did not detect (correctly) any inconsistency in the descriptions.

## High-Level Generation

The LLM produced all the 4 parts required for the HL KB. 

The general KB seems to correctly capture all the necessary predicates and also the resources. 

The initial state correctly captures the position of the blocks on the table and the fact that both
agents are available.

The goal state correctly describe the movement of the block from the table to the top of another 
block.

The LLM generated all the actions dividing them in start and final actions. By running the HL planner
there seems to not be any error as it finds the correct solution in 1.457s. Also when changing the 
order of the actions, we are still obtaining the correct result. 


## Low-Level Generation

The LLM did not update the HL KB correctly. There are two main errors:

- The mapping for `move_*` is not completely correct. For example, the HL action
  `move_table_to_table_start` was initially mapped to 
  ```prolog
  mapping(move_table_to_table_start(Agent, Block, X1, Y1, X2, Y2),
    [
        move_arm_start(Agent, 4, 4, X1, Y1),
        move_arm_end(Agent, 4, 4, X1, Y1),
        close_gripper_start(Agent, Block),
        close_gripper_end(Agent, Block),
        move_arm_start(Agent, X1, Y1, X2, Y2),
        move_arm_end(Agent, X1, Y1, X2, Y2),
        open_gripper_start(Agent),
        open_gripper_end(Agent)
    ]
  ).
  ``` 
  assuming that the robotic arm would always start from (4, 4), which instead is not the case. 

The plan was found in 2.544s.