# Report Experiment 1 

## Consistency Check

The LLM did not detect (correctly) any inconsistency in the descriptions.

## High-Level Generation

The LLM produced all the 4 parts required for the HL KB. 

The general KB seems to correctly capture all the necessary predicates and also the resources. 

The initial and final states correctly captures the position of the blocks on the table and the fact
that both agents are available.

The LLM generated all the actions dividing them in start and final actions. By running the HL planner
there seems to not be any error as it finds the correct solution in 200ms. Also when changing the 
order of the actions, we are still obtaining the correct result. 


## Low-Level Generation

The LLM updated the HL KB correctly, capturing the new predicates for the arm and the gripper. It 
was also able to correctly write the actions and the mappings. 

After testing with the planner, we were able to extract a plan in 9ms. 


## Variations

When the HL query is not correct, e.g.:

There are 6 blocks on a table. In the initial state of the simulation, block b1 is in position
(1,1) on the table, block b2 is in position (2,2) on the table, block b3 is in position (3,3) on the
table, whereas blocks b4, b5 and b6 are on top of blocks b1, b2 and b3, respectively. After the 
execution of the plan, only blocks b4 and b5 have been moved. In particular, b4 is not on the table 
in position (10,10) and block b5 has been moved on top of it. There are two available agents that 
can carry out the task. They are available at the beginning and will be available at the end. 
Each agent can move a block:
- from a position on the table to another position on the table;
- from a position on the table, to the top of another block;
- from the top of a block, to a position on the table;
- from the top of a block, to the top of another block.

The LLM is able to understand it and reports the following error during the consistency check:


PROBLEM. The description is inconsistent. The final state mentions that block b4 is "not on the
table in position (10,10)" and that block b5 has been moved on top of it. This is contradictory
because if block b4 is not on the table, it cannot be in position (10,10) and block b5 cannot be on
top of it. Additionally, the initial state does not provide a clear indication of how blocks b4 and
b5 can be moved to a position that is not on the table. The description of the agents' capabilities
also does not support moving blocks to a position that is not on the table. 