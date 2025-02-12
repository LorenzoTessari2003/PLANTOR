# Report Experiment BW - 4 128K - 3

## Consistency Check

The LLM did not detect any inconsistency, which is correct.

## High-Level KB Generation

The LLM correctly generated the 4 section of the knowledge-base.

The generate KB was correct except for the actions checking the predicate `pos(X,Y)` inside the 
state when it actually is part of the general knowledge.

We could also highlight the fact that the `move_block` action is not very descriptive w.r.t. the 
other actions, it still could provide a correct plan when asked for a different plan.

## Low-Level KB Generation

The LL KB had more errors:

- The low-level actions were looking for the predicate `ll_arm`, but they wouldn't find it as there 
  was none.
- The LLM mixed two different predicates to indicate the position of the arm: `ll_arm_at` and 
  `ll_arm_pos`, which did not enable the planner to find a correct plan.
- The mappings had the following structure:
    ```prolog
    mapping(move_block_start(Agent, Block, X1, Y1, X2, Y2),
    [
        move_arm_start(Agent, X1, Y1, X1, Y1),
        close_start(Agent),
        close_end(Agent),
        move_arm_start(Agent, X1, Y1, X2, Y2),
        move_arm_end(Agent, X1, Y1, X2, Y2),
        open_start(Agent),
        open_end(Agent)
    ]
    ).
    ```
    This has two errors:
    
    - The first action has no ending counterpart;
    - The first action always moves the arm to the same position.
    
    The correct mapping would be:
    ```prolog
    mapping(move_block_start(Agent, Block, X1, Y1, X2, Y2),
    [
        move_arm_start(Agent, _, _, X1, Y1),
        move_arm_end(Agent, _, _, X1, Y1),
        close_start(Agent),
        close_end(Agent),
        move_arm_start(Agent, X1, Y1, X2, Y2),
        move_arm_end(Agent, X1, Y1, X2, Y2),
        open_start(Agent),
        open_end(Agent)
    ]
    ).
    ```
