# Report Experiment 3 

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
there seems to not be any error as it finds the correct solution in 8ms. Is worth to mention that the plan is not optimal, 
in order to place D on top of A the planner move at first the block in a position and then move it on top of A. This 
because the move_onblock_to_block_start action require the two blocks to be clear.  
After testing with the planner, we were able to extract a plan in 9ms. 

## Low-Level Generation

Find HL plan but could not apply mappings since there was an error in every mapping
move_arm_*(Agent, X1, Y1, X1, Y1), has to be move_arm_*(Agent, _, _, X1, Y1),
Changing this solves the problem
