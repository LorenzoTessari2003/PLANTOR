# Report Experiment BW - 4o - 2

## Consistency Check

The LLM did detected correctly an inconsistency in the HL descriptions. Indeed, the HL query states:
"b2 is not on the table in position (10,10)", which is at best confusing. After correcting the 
mistake, the LLM did not detect any inconsistency correctly. 

## High-Level Generation

The LLM produced all the 4 parts required for the HL KB. 

The general KB seems to correctly capture all the necessary predicates and also the resources. 

The initial and final states correctly captures the position of the blocks on the table and the fact
that both agents are available.

The LLM generated all the actions dividing them in start and final actions. By running the HL planner
there seems to not be any error as it finds the correct solution in 1.395s. Also when changing the 
order of the actions, we are still obtaining the correct result. 


## Low-Level Generation

The LLM did not update the HL KB correctly. There main error is:

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

After fixing this mistake, the planner managed to find a solution in 1.643s.


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