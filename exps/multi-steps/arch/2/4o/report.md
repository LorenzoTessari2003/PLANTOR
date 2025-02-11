# Report Experiment 1 Arch - 4o - 1


## Consistency Check

The LLM did detected correctly an inconsistency in the HL descriptions. Indeed, the HL query states:
"b2 is not on the table in position (10,10)", which is at best confusing. After correcting the 
mistake, the LLM did not detect any inconsistency correctly. 


## High-Level Generation

The LLM produced all the 4 parts required for the HL KB. 

The general KB seems to correctly capture all the necessary predicates and also the resources. 

The initial and final states correctly captures the position of the blocks on the table and the fact
that both agents are available.

The LLM generated all the actions dividing them in start and final actions. The action 
`place_architrave_start(Agent, Arch, Pillar1, Pillar2, X1, Y1, X2, Y2)` is wrong because it requires
that the predicate `ontable(Arch)` is not on the table, but in some cases it may be on the table.

By removing the predicate, the plan was correctly found in 8ms.


## Low-Level Generation

The LLM did not update the HL KB correctly. There are several errors:

- The LLM did not account for the arms starting positions in the general knowledge. `pos(4,4)` and
  `pos(5,5)` must be added in order to correctly plan.

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

- It used an additional predicate `ll_gripped(Arm)`, which is redundant since 
  `ll_gripper(Arm, close)` and `ll_gripper(Arm, open)` already take care of this. Also, it was added
  in one action, required as a precondition in another, but never removed. Hence there are two
  possible solutions:

    - either remove `ll_gripped(Arm)`, or
    - add also the predicates to remove the predicate once the gripper is opened.
  
- The mappings for the `place_architrave_start` is missing a pair of coordinates. One must add 
  `(X3,Y3)` and correctly fix the mapping.

After fixing these mistakes, the planner managed to find a solution in 337ms.

