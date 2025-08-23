reasoning_examples_training_data_samples = [ 
    { #KB Tag Reasoning
    "natural_language":
    '''
        You now have to produce code for the task that follows.
        I have a high-level knowledge base that defines agents 'a1' and 'a2'.
        My low-level description specifies that these agents are, in fact, robotic arms.
        **Your task is to update the general knowledge base to include these new low-level details.**
        **CRITICAL: Wrap the COMPLETE updated knowledge base within a single Markdown tag ```kb ... ```.**

        High-level KB for reference:
        ```
        % Agents
        agent(a1).
        agent(a2).
        % Resources
        resources(agent(_)).
        ```
    ''',
    "prolog_code":
    '''
        Of course. It is crucial to merge the high-level and low-level static information into a single, comprehensive knowledge base. This entire block must be wrapped in the `kb` tag for the parser to correctly identify it.

        I will add the `ll_arm/1` predicate to represent the low-level nature of the agents and also update the `resources/1` section to declare that these arms are resources the planner can use.

        ```kb
        % Agents (High-Level representation)
        agent(a1).
        agent(a2).

        % Low-level predicates for arms
        ll_arm(a1).
        ll_arm(a2).

        % Resources for both levels
        resources(agent(_)).
        resources(ll_arm(_)).
        ```
        **Reasoning for this format:** The `kb` tag is used to define all static facts of the world, for all levels of abstraction. My Python script will parse everything inside a single `kb` block. Adding `ll_arm` and updating `resources` in the same block ensures that the final KB is consistent and complete before generating states or actions.
    '''
    },
    { #KB Tag Reasoning
    "natural_language":
    '''
        Given that the previous messages are examples, let's break down the process of creating a knowledge base.
        **Task:** The scenario begins with 3 blocks on the table: b1, b2, and b3, at positions (0,2), (3,1), and (4,4). A single agent, a1, is available.
        Generate the static knowledge base and explain how you identified each part.
    ''',
    "prolog_code":
    '''
        Of course. To build the static knowledge base, I will systematically scan the text for all unique, unchanging entities.

        **My thought process:**
        1.  **Identify Positions:** I'll look for all coordinate pairs `(x,y)`. The text mentions `(0,2)`, `(3,1)`, and `(4,4)`. These become `pos/2` facts.
        2.  **Identify Blocks:** I'll find all unique block names, usually prefixed with 'b'. The text lists `b1`, `b2`, and `b3`. These become `block/1` facts.
        3.  **Identify Agents:** I'll look for agent identifiers like 'a1'. The text mentions `a1`. This becomes an `agent/1` fact.
        4.  **Identify Resources:** The agent is the entity that performs tasks, so it is a resource. This becomes a `resources(agent(_))` fact.

        Here is the resulting knowledge base:
        ```kb
        % The 'kb' tag defines the static "vocabulary" of the world.
        % Positions mentioned in the scenario.
        pos(0,2).
        pos(3,1).
        pos(4,4).

        % Blocks involved in the scenario.
        block(b1).
        block(b2).
        block(b3).

        % Agent(s) available.
        agent(a1).

        % Declaring that agents are a usable resource.
        resources(agent(_)).
        ```
        **Reasoning for this format:** The `kb` must contain a complete inventory of every static object mentioned. By systematically scanning for each type of entity (positions, blocks, agents), I ensure that no information is missed and that the foundation for the `init` and `goal` states is solid and accurate.
    '''
    },
    { #KB Tag Reasoning
    "natural_language":
    '''
        Given that the previous messages are examples, let's handle a scenario with many entities.
        **Task:** The scenario mentions "7 blocks" (b1, b2, b3, b4, b5, b6, b7) and "8 agents with a robotic arm".
        Generate the knowledge base, explaining how you handle the enumeration of agents.
    ''',
    "prolog_code":
    '''
        Understood. When a specific number of entities is given, my task is to enumerate them completely, even if they are not all individually named in the action descriptions.

        **My thought process:**
        1.  **Blocks:** The text explicitly lists 7 block names from `b1` to `b7`. I will create a `block/1` fact for each.
        2.  **Agents:** The text states there are "8 agents". I will create 8 distinct `agent/1` facts, using a standard naming convention (`a1`, `a2`, ... `a8`).
        3.  **Positions & Resources:** I will extract the mentioned positions and declare agents as resources as usual.

        Here is the generated knowledge base:
        ```kb
        % Positions explicitly mentioned.
        pos(2,2).
        pos(4,4).
        pos(8,8).

        % All 7 blocks are declared as static entities.
        block(b1).
        block(b2).
        block(b3).
        block(b4).
        block(b5).
        block(b6).
        block(b7).

        % All 8 agents are enumerated as requested.
        agent(a1).
        agent(a2).
        agent(a3).
        agent(a4).
        agent(a5).
        agent(a6).
        agent(a7).
        agent(a8).

        % Resources
        resources(agent(_)).
        ```
        **Reasoning for this format:** It is crucial to respect the specified quantities. If the prompt says "8 agents," the `kb` must define exactly eight `agent/1` facts. This ensures that the `init` state can correctly declare all eight agents as `available`, matching the problem description. Failing to enumerate them would create an inconsistency between the `kb` and the state definitions.
    '''
    },
    { #KB Tag Reasoning
    "natural_language":
    '''
        Given that the previous messages are examples, let's focus on identifying all necessary positions.
        **Task:** A scenario describes initial positions like `b1 is at (1,1)` and goal positions like `b1 will be moved to the table at (0,0)`.
        Generate the `kb` and explain why positions from both the initial and goal descriptions must be included.
    ''',
    "prolog_code":
    '''
        This is a critical detail for creating a valid domain. The knowledge base must define every single location that will ever be used in the problem, whether at the start or at the end.

        **My thought process:**
        1.  I will read the entire problem description.
        2.  I will make a list of all unique `(x,y)` coordinates mentioned.
        3.  I will create a `pos/2` fact for every unique coordinate on my list.

        Applying this to your Sample 8 scenario:
        ```kb
        % Positions must include ALL coordinates from INIT and GOAL states.
        pos(0,0). % From goal
        pos(0,1). % From goal
        pos(1,0). % From goal
        pos(1,1). % From init
        pos(1,2). % From init
        pos(2,1). % From init
        pos(3,3). % From init
        pos(4,4). % From init
        pos(5,5). % From goal
        pos(6,6). % From init

        % Blocks
        block(b1). % ...and all other blocks...
        block(b2).
        block(b3).
        block(b4).
        block(b5).
        block(b6).
        block(b7).
        block(b8).

        % Agents
        agent(a1).
        agent(a2).
        agent(a3).

        % Resources
        resources(agent(_)).
        ```
        **Reasoning for this format:** The `pos/2` facts in the `kb` act as a declaration of "valid places" in our world. The planner uses these facts to verify that actions are legal. If an action tries to move a block to a position `(x,y)` and there is no `pos(x,y).` fact in the `kb`, the planner will fail. Therefore, the `kb` must pre-declare every location that will be occupied at any point in the problem.
    '''
    },
    { #KB Tag Reasoning
    "natural_language":
    '''
        Given that the previous messages are examples, let's clarify what information to *ignore* when creating the knowledge base.
        **Task:** The prompt says "Block b6 is on top of b1." Should this information go into the `kb`? Explain why or why not.
        Generate the correct `kb` for this scenario.
    ''',
    "prolog_code":
    '''
        This is a crucial distinction. The knowledge base (`kb`) is for **static facts only**. Information about the arrangement of blocks is **dynamic** and belongs in the `init` state.

        **My thought process:**
        - The statement "Block b6 is on top of b1" describes a relationship (`on/2`) that can and will change. It is not a static property of the world.
        - Therefore, I must **ignore** this information when creating the `kb`.
        - I will, however, extract the static entities: the blocks (`b1`-`b6`), the agents, and all mentioned positions.

        Here is the correct knowledge base, which does not include the `on/2` relationship:
        ```kb
        % The kb only contains static entities. Dynamic info like 'on(b6,b1)' is ignored.
        % Positions
        pos(0,0).
        pos(1,4).
        pos(2,3).
        pos(3,2).
        pos(4,1).
        pos(5,5).

        % Blocks
        block(b1).
        block(b2).
        block(b3).
        block(b4).
        block(b5).
        block(b6).

        % Agents
        agent(a1).
        agent(a2).
        agent(a3).

        % Resources
        resources(agent(_)).
        ```
        **Reasoning for this format:** The `kb` defines the "game pieces," while the `init` state defines their starting layout on the "game board." Mixing these up would be a fundamental error. My job for the `kb` task is to strictly filter out any information that describes a temporary state, ensuring that the `kb` remains a clean, static definition of the world's components.
    '''
    },
    { #KB Tag Reasoning
    "natural_language":
    '''
        Given that the previous messages are examples, let's clarify how to handle low-level details during high-level KB generation.
        **Task:** The prompt says "There are 8 agents with a robotic arm." How should the "robotic arm" detail be handled when creating the initial, high-level knowledge base?
    ''',
    "prolog_code":
    '''
        This is a question about the scope of the high-level knowledge base. During this initial phase, my task is to capture only the abstract information.

        **My thought process:**
        1.  I identify "8 agents". This is a high-level concept. I will create `agent(a1)` through `agent(a8)`.
        2.  I see the detail "with a robotic arm". This is a low-level physical description. For the **high-level KB**, this information is not relevant yet. It will be added later when we enrich the KB with low-level details.
        3.  Therefore, I will ignore the phrase "with a robotic arm" for now.

        Here is the correct high-level KB:
        ```kb
        % The high-level KB ignores low-level physical details like 'robotic arm'.
        % It only captures the abstract concept of an 'agent'.
        % Positions
        pos(2,2).
        pos(4,4).
        pos(8,8).

        % Blocks
        block(b1).
        block(b2).
        block(b3).
        block(b4).
        block(b5).
        block(b6).
        block(b7).

        % Agents
        agent(a1).
        agent(a2).
        agent(a3).
        agent(a4).
        agent(a5).
        agent(a6).
        agent(a7).
        agent(a8).

        % Resources
        resources(agent(_)).
        ```
        **Reasoning for this format:** The system uses a hierarchical approach. The first step is to build a purely abstract, high-level model of the world. Details about the physical implementation (`robotic arm`) are intentionally excluded at this stage. They will be added in a later step when the `kb` is updated for the low-level planner. This separation of concerns is key to managing complexity.
    '''
    },
    { #Init and goal states reasoning
    "natural_language":
    '''
        You now have to produce code for the task that follows.
        My scenario involves an agent 'arm1' moving a block. The low-level description states that 'arm1' starts at position (0,0) with its gripper open. In the final state, the position and gripper state of 'arm1' are not important.
        **Your task is to generate ONLY the initial and final states with these low-level details.**
        **CRITICAL: Do NOT include the knowledge base (`kb`), actions, or any other information.**

        High-level init for reference: `init_state([ontable(b1), available(arm1)]).`
        High-level goal for reference: `goal_state([on(b1,b2), available(arm1)]).`
    ''',
    "prolog_code":
    '''
        Understood. I will generate only the `init` and `goal` states, incorporating the low-level predicates `ll_arm_at` and `ll_gripper`. I will not add any other tags or explanations outside of what is requested.

        ```init
        init_state([
        ontable(b1),
        available(arm1),
        ll_arm_at(arm1,0,0),
        ll_gripper(arm1,open)
        ]).
        ```

        ```goal
        goal_state([
        on(b1,b2),
        available(arm1),
        ll_arm_at(arm1,_,_),
        ll_gripper(arm1,_)
        ]).
        ```
        **Reasoning for this format:** The prompt strictly requires a response containing only the `init` and `goal` blocks. Providing extra information like `kb` or `actions` would violate the instruction and cause parsing errors. Using the wildcard `_` in the `goal` state correctly represents that any final position and gripper state for the arm is acceptable, which is a common requirement in planning problems.
    '''
    },
    { #Init and goal states reasoning
    "natural_language":
    '''
        Given that the previous messages are examples, let's break down a simple scenario.
        **Task:** The scenario begins with 3 blocks on the table: b1 at (0,2), b2 at (3,1), and b3 at (4,4). The goal is to rearrange them so that b1 is at (4,4), b2 is at (0,2), and b3 is at (3,1). A single agent, a1, is available.
        Provide the initial and goal states, and explain the meaning of the predicates used.
    ''',
    "prolog_code":
    '''
        Of course. I will generate the `init` and `goal` states for this simple rearrangement task and explain the logic behind the chosen predicates.

        ```init
        init_state([
        % 'ontable(X)' means block X is sitting directly on the table.
        ontable(b1), ontable(b2), ontable(b3),
        % 'at(X,Y,Z)' gives the exact coordinate of block X.
        at(b1,0,2), at(b2,3,1), at(b3,4,4),
        % 'clear(X)' means nothing is on top of block X, so it can be picked up.
        clear(b1), clear(b2), clear(b3),
        % 'available(A)' means agent A is ready to perform an action.
        available(a1)
        ]).
        ```

        ```goal
        goal_state([
        % In the goal, the blocks are still on the table, just at new positions.
        ontable(b1), ontable(b2), ontable(b3),
        at(b1,4,4), at(b2,0,2), at(b3,3,1),
        % They are all still clear, as no towers were built.
        clear(b1), clear(b2), clear(b3),
        % The agent must be available again at the end of the plan.
        available(a1)
        ]).
        ```
        **Reasoning for this format:** The `init` and `goal` tags are used to define the start and end pictures of our world. Each predicate describes a specific fact. `ontable` defines the vertical relationship (on the table), `at` defines the location (coordinates), and `clear` defines the accessibility of a block. It is essential to list all these facts to give the planner a complete and unambiguous description of the problem it needs to solve.
    '''
    },
    { #Init and goal states reasoning
    "natural_language":
    '''
        Given that the previous messages are examples, let's analyze a scenario involving stacking.
        **Task:** An initial state has b4 on top of b3. The goal is to build a new tower where b2 is on b1, and b3 is on b2.
        Explain how the 'on' predicate affects the 'clear' and 'at' predicates in the goal state.
    ''',
    "prolog_code":
    '''
        Understood. This is a key concept in blocks-world problems. When one block is `on` another, their states become linked. I will generate the states and explain this relationship.

        Let's use the scenario from your Sample 1: Initial state has b4 on b3. Goal is a tower b1-b2-b3.

        ```init
        init_state([
        ontable(b1), ontable(b2), ontable(b3), ontable(b5),
        % Initially, b4 is on b3. This means b3 is NOT clear.
        on(b4, b3),
        at(b1,2,2), at(b2,1,1), at(b3,3,3), at(b4,3,3), at(b5,4,4),
        % Only blocks with nothing on them are clear.
        clear(b1), clear(b2), clear(b4), clear(b5),
        available(a1)
        ]).
        ```

        ```goal
        goal_state([
        ontable(b1), ontable(b4), ontable(b5),
        % The new tower: b3 is on b2, and b2 is on b1.
        on(b3, b2), on(b2, b1),
        % CRITICAL: All blocks in a stack share the same base coordinate.
        % The position of b2 and b3 is determined by the position of b1.
        at(b1,2,2), at(b2,2,2), at(b3,2,2),
        at(b4,3,3), at(b5,0,0),
        % Only the TOP of the stack (b3) and single blocks are clear.
        % b1 and b2 are NOT clear.
        clear(b3), clear(b4), clear(b5),
        available(a1)
        ]).
        ```
        **Reasoning for this format:** When `on(A,B)` is true, two rules must be followed:
        1.  **Shared Position:** `at(A,Pos)` must be the same as `at(B,Pos)`. A stack exists in one spot.
        2.  **State Dependency:** Block `B` is now supporting block `A`, so `B` can no longer be `clear`.
        Correctly modeling these two rules is essential for the planner to understand the physical constraints of building towers.
    '''
    },
    { #Init and goal states reasoning
    "natural_language":
    '''
        Given that the previous messages are examples, let's discuss things that *don't* change.
        **Task:** In a scenario, b5 moves from b1 to b4. The prompt says "the remaining blocks b1, b2 and b3 are in the same positions as in the initial state." It also mentions b6 and b7 are irrelevant.
        How should these unchanging and irrelevant blocks be handled in the goal state?
    ''',
    "prolog_code":
    '''
        This is an important point about state representation. A goal state must be a complete description. Facts that are not changed by the plan must be explicitly re-stated.

        Using the context from your Sample 2:

        ```init
        init_state([
        ontable(b1), ontable(b2), ontable(b4), ontable(b6), ontable(b7),
        on(b5, b1), on(b3, b2),
        at(b1,2,2), at(b2,4,4), at(b3,4,4), at(b4,8,8), at(b5,2,2),
        % Irrelevant blocks are given a starting position.
        at(b6,1,1), at(b7,3,3),
        clear(b3), clear(b4), clear(b5), clear(b6), clear(b7),
        available(a1) % and other agents
        ]).
        ```

        ```goal
        goal_state([
        ontable(b1), ontable(b2), ontable(b4), ontable(b6), ontable(b7),
        % The main change: b5 is now on b4.
        on(b5, b4),
        % Persistence: b3 is still on b2, as this was not mentioned to have changed.
        on(b3, b2),
        % The positions of b1, b2, b3, b6, and b7 must be re-stated exactly as they were in init.
        at(b1,2,2), at(b2,4,4), at(b3,4,4), at(b4,8,8), at(b5,8,8),
        at(b6,1,1), at(b7,3,3),
        % The clear state of b1 changes because b5 was removed. Other clear states persist.
        clear(b1), clear(b3), clear(b5), clear(b6), clear(b7),
        available(a1) % and other agents
        ]).
        ```
        **Reasoning for this format:** The planner does not assume things persist. A goal state must be a complete description of all desired final conditions. If a block like `b3` is still on `b2` at the end, the predicate `on(b3,b2)` must be included in the `goal`. If its position `at(b3,4,4)` is unchanged, that must also be included. Omitting these "persistent facts" would mean the planner is free to move them, leading to an incorrect solution.
    '''
    },
    { #Init and goal states reasoning
    "natural_language":
    '''
        Given that the previous messages are examples, let's handle a very complex setup.
        **Task:** We have 8 blocks with multiple towers and a goal that involves creating new towers and moving single blocks.
        Explain the step-by-step thinking to generate the goal state correctly.
    ''',
    "prolog_code":
    '''
        A complex scenario like your Sample 8 requires a systematic approach. I will break down the goal description piece by piece to construct the final `goal_state`.

        **1. Identify final positions of single blocks:**
        - "b1 will be moved to the table at (0,0)": `ontable(b1), at(b1,0,0), clear(b1).`
        - "b2 will be moved to the table at (0,1)": `ontable(b2), at(b2,0,1), clear(b2).`
        - "b3 will be moved to the table at (1,0)": `ontable(b3), at(b3,1,0), clear(b3).`

        **2. Construct the first new tower:**
        - "b6 at the bottom, b5 on b6, and b4 on b5" at (5,5).
        - This implies: `ontable(b6), on(b5,b6), on(b4,b5).`
        - All share one position: `at(b6,5,5), at(b5,5,5), at(b4,5,5).`
        - Only the top is clear: `clear(b4).` (b5 and b6 are not clear).

        **3. Construct the second new tower:**
        - "b7 will be placed on top of b8." Their initial position is (6,6).
        - This implies: `ontable(b8), on(b7,b8).`
        - Shared position: `at(b8,6,6), at(b7,6,6).`
        - Top is clear: `clear(b7).`

        **4. Combine all pieces into the final goal state:**
        ```goal
        goal_state([
        % From Step 1
        ontable(b1), ontable(b2), ontable(b3),
        at(b1,0,0), at(b2,0,1), at(b3,1,0),
        clear(b1), clear(b2), clear(b3),
        % From Step 2
        ontable(b6), on(b5,b6), on(b4,b5),
        at(b6,5,5), at(b5,5,5), at(b4,5,5),
        clear(b4),
        % From Step 3
        ontable(b8), on(b7,b8),
        at(b8,6,6), at(b7,6,6),
        clear(b7),
        % Agents must be available
        available(a1), available(a2), available(a3)
        ]).
        ```
        **Reasoning for this format:** By breaking a complex request into smaller, manageable parts ("what is the state of b1?", "what does the new tower look like?"), I can ensure all conditions are met without errors. This systematic, piece-by-piece construction is a reliable strategy to generate a correct and complete `goal` state for even the most complicated scenarios.
    '''
    },
    { #Init and goal states reasoning
    "natural_language":
    '''
        Given that the previous messages are examples, please identify and fix the logical error in this goal state.
        **Task:** The goal is to have b1 on b2. The provided goal state is incorrect.
        Incorrect Goal: `goal_state([ ontable(b1), ontable(b2), on(b1,b2), at(b1,1,1), at(b2,1,1) ]).`
        Explain the error and provide the corrected version.
    ''',
    "prolog_code":
    '''
        You are correct, the provided goal state contains a fundamental logical contradiction. A block cannot be simultaneously `ontable` and `on` another block.

        **The Error:** The predicate `ontable(b1)` states that b1 is directly on the table. The predicate `on(b1,b2)` states that b1 is directly on b2. These two conditions are mutually exclusive.

        I will correct the goal state by removing the incorrect `ontable(b1)` predicate.

        ```goal
        goal_state([
        % Correct: b2 is on the table, serving as the base of the stack.
        ontable(b2),
        % Correct: b1 is on b2, not on the table.
        on(b1,b2),
        at(b1,1,1),
        at(b2,1,1),
        clear(b1) % Assuming agent is also available.
        ]).
        ```
        **Reasoning for this format:** The integrity of the state description is paramount. My role is to generate states that represent a physically possible reality. By identifying and correcting contradictions like a block being in two places at once, I ensure the planner receives a valid problem to solve. If the planner received the incorrect state, it might fail or determine the goal is unreachable.
    '''
    },
    { #actions reasoning
    "natural_language":
    '''
        Given the provided knowledge base, initial, and final states, write the set of high-level temporal actions for a blocks world.
        The main action is to move a block that is on the table to a new, free position on the table.
        **Wrap the set of actions in Markdown tags ```actions``` and NOT with other tags**
    ''',
    "prolog_code":
    '''
        Certainly. High-level capabilities are defined within the `actions` tag. Because this is for a temporal planner, each logical action must be split into a `_start` and an `_end` part. The `_start` action consumes resources and initiates the change, while the `_end` action completes it.

        ```actions
        % Move a block from one position on the table to another
        action(move_table_to_table_start(Agent, Block, X1, Y1, X2, Y2),
        [ontable(Block), at(Block, X1, Y1), available(Agent), clear(Block)],
        [at(_, X2, Y2)], % Negative condition: the destination must be free
        [],
        [agent(Agent), block(Block), pos(X1, Y1), pos(X2, Y2)],
        [
            del(available(Agent)), del(clear(Block)), del(at(Block, X1, Y1)), del(ontable(Block)),
            add(moving_table_to_table(Agent, Block, X1, Y1, X2, Y2))
        ]
        ).
        action(move_table_to_table_end(Agent, Block, X1, Y1, X2, Y2),
        [moving_table_to_table(Agent, Block, X1, Y1, X2, Y2)],
        [],
        [],
        [agent(Agent)],
        [
            del(moving_table_to_table(Agent, Block, X1, Y1, X2, Y2)),
            add(ontable(Block)), add(at(Block, X2, Y2)), add(clear(Block)), add(available(Agent))
        ]
        ).
        ```
        **Reasoning for this format:** The `actions` tag is reserved for high-level logic. My system parses this block to understand the causal relationships in the domain. Using this specific tag, and not a generic one, is essential for the parser to correctly identify these rules.
    '''
    },
    { #actions reasoning
    "natural_language":
    '''
        Given that the previous messages are examples, please perform the following specific task.
        **Task:** Define only the high-level temporal action for moving a block from a position on the table to another empty position on the table.
        **Instruction:** Your response must contain only the `_start` and `_end` definitions for this single action, wrapped in the `actions` tag.
    ''',
    "prolog_code":
    '''
        Understood. I will generate only the high-level action `move_table_to_table`. This action is fundamental as it defines the simplest form of movement in the world. The definition will be placed inside the `actions` tag.

        ```actions
        % This action defines how to move a block from one spot on the table to another.
        action(move_table_to_table_start(Agent, Block, X1, Y1, X2, Y2),
        % Preconditions at start: The agent must be free, and the block must be on the table,
        % clear (nothing on top), and at the specified starting position.
        [ontable(Block), at(Block, X1, Y1), available(Agent), clear(Block)],
        % Negative condition: The destination must be free when the action starts.
        [at(_, X2, Y2)],
        [],
        [agent(Agent), pos(X1, Y1), pos(X2, Y2), block(Block)],
        % Effects at start: The agent becomes busy, and the block is lifted from its
        % initial state, creating a 'moving' state.
        [
            del(available(Agent)), del(clear(Block)), del(ontable(Block)), del(at(Block, X1, Y1)),
            add(moving_table_to_table(Agent, Block, X1, Y1, X2, Y2))
        ]
        ).
        action(move_table_to_table_end(Agent, Block, X1, Y1, X2, Y2),
        % Precondition at end: The block must be in the 'moving' state from the start of this action.
        [moving_table_to_table(Agent, Block, X1, Y1, X2, Y2)],
        [],
        [],
        [agent(Agent)],
        % Effects at end: The 'moving' state is removed, and the block is placed in its
        % new state on the table, freeing the agent.
        [
            del(moving_table_to_table(Agent, Block, X1, Y1, X2, Y2)),
            add(ontable(Block)), add(at(Block, X2, Y2)), add(clear(Block)), add(available(Agent))
        ]
        ).
        ```
        **Reasoning for this format:** The `actions` tag is used to define the "rules of physics" for the high-level planner. This specific action, `move_table_to_table`, provides the planner with the capability to rearrange blocks that are on the table. Splitting it into `_start` and `_end` is mandatory for temporal planning, allowing the system to reason about states that occur *during* an action's execution.
    '''
    },
    { #actions reasoning
    "natural_language":
    '''
        Given that the previous messages are examples, please perform the following specific task.
        **Task:** Define only the high-level temporal action for picking up a block from the table and placing it on top of another block.
        **Instruction:** Your response must contain only the `_start` and `_end` definitions for this single action, wrapped in the `actions` tag.
    ''',
    "prolog_code":
    '''
        Certainly. I will define the `move_table_to_block` action. This is a key action for building towers. It will be correctly placed within the `actions` tag.

        ```actions
        % This action defines how to stack a block from the table onto another block.
        action(move_table_to_block_start(Agent, Block1, Block2, X1, Y1, X2, Y2),
        % Preconditions at start: The agent is free, Block1 is on the table and clear,
        % and Block2 (the destination) must also be clear.
        [available(Agent), ontable(Block1), at(Block1, X1, Y1), at(Block2, X2, Y2), clear(Block2), clear(Block1)],
        [],
        [],
        [agent(Agent), pos(X1, Y1), pos(X2, Y2), block(Block1), block(Block2), Block1 \\= Block2],
        % Effects at start: Agent becomes busy, Block1 is lifted, and Block2 is now reserved
        % for this action, so it is no longer clear.
        [
            del(available(Agent)), del(clear(Block1)), del(ontable(Block1)), del(at(Block1, X1, Y1)), del(clear(Block2)),
            add(moving_table_to_block(Agent, Block1, Block2, X1, Y1, X2, Y2))
        ]
        ).
        action(move_table_to_block_end(Agent, Block1, Block2, X1, Y1, X2, Y2),
        % Precondition at end: The 'moving' state must hold.
        [moving_table_to_block(Agent, Block1, Block2, X1, Y1, X2, Y2)],
        [],
        [],
        [agent(Agent)],
        % Effects at end: The action concludes. Block1 is now on top of Block2,
        % has the same position as Block2, and is itself clear. The agent is freed.
        [
            del(moving_table_to_block(Agent, Block1, Block2, X1, Y1, X2, Y2)),
            add(on(Block1, Block2)), add(at(Block1, X2, Y2)), add(clear(Block1)), add(available(Agent))
        ]
        ).
        ```
        **Reasoning for this format:** The `actions` tag must contain all possible high-level operations. This action is crucial because it's the primary way to create `on/2` relationships (stacks). A key detail here is that `del(clear(Block2))` happens at the `_start` of the action. This is important because it prevents another agent from trying to place something on `Block2` while `Block1` is already on its way.
    '''
    },
    { #actions reasoning
    "natural_language":
    '''
        Given that the previous messages are examples, please perform the following specific task.
        **Task:** Define only the high-level temporal action for taking the top block from a stack and placing it onto an empty position on the table.
        **Instruction:** Your response must contain only the `_start` and `_end` definitions for this single action, wrapped in the `actions` tag.
    ''',
    "prolog_code":
    '''
        Understood. I will now define the `move_onblock_to_table` action. This action is essential for disassembling existing towers. The definition will be placed correctly inside an `actions` block.

        ```actions
        % This action defines how to unstack a block and move it to the table.
        action(move_onblock_to_table_start(Agent, Block1, Block2, X2, Y2),
        % Preconditions at start: Agent is free, Block1 is on Block2, and Block1 is clear.
        [available(Agent), on(Block1, Block2), at(Block1, X1, Y1), clear(Block1)],
        % Negative condition: The destination on the table must be free.
        [at(_, X2, Y2)],
        [],
        [agent(Agent), block(Block1), block(Block2), pos(X1,Y1), pos(X2,Y2), Block1 \\= Block2],
        % Effects at start: Agent becomes busy, Block1 is lifted, and critically,
        % Block2 becomes clear the moment Block1 is picked up.
        [
            del(available(Agent)), del(clear(Block1)), del(on(Block1, Block2)), del(at(Block1, X1, Y1)),
            add(clear(Block2)),
            add(moving_onblock_to_table(Agent, Block1, X2, Y2))
        ]
        ).
        action(move_onblock_to_table_end(Agent, Block1, X2, Y2),
        % Precondition at end: The 'moving' state must be active.
        [moving_onblock_to_table(Agent, Block1, X2, Y2)],
        [],
        [],
        [agent(Agent)],
        % Effects at end: The action finishes. Block1 is now on the table at its
        % new position, and the agent is available again.
        [
            del(moving_onblock_to_table(Agent, Block1, X2, Y2)),
            add(ontable(Block1)), add(at(Block1, X2, Y2)), add(clear(Block1)), add(available(Agent))
        ]
        ).
        ```
        **Reasoning for this format:** This action must be in the `actions` tag for the high-level planner to use it. It is the logical inverse of `move_table_to_block`. The most important piece of logic here is `add(clear(Block2))` in the `_start` action. This models the fact that as soon as a block is lifted, the one beneath it is immediately available for other actions, which is crucial for efficient temporal planning.
    '''
    },
    { #actions reasoning
    "natural_language":
    '''
        Given that the previous messages are examples, please perform the following specific task.
        **Task:** Define only the high-level temporal action for moving a block from the top of one block (`Block2`) to the top of another block (`Block3`).
        **Instruction:** Your response must contain only the `_start` and `_end` definitions for this single action, wrapped in the `actions` tag.
    ''',
    "prolog_code":
    '''
        Certainly. I will define the `move_onblock_to_block` action. This is the most complex of the four actions, as it involves three distinct blocks. It will be defined within the `actions` tag.

        ```actions
        % This action defines how to move a block from one stack to another.
        action(move_onblock_to_block_start(Agent, Block1, Block2, Block3, X1, Y1, X3, Y3),
        % Preconditions at start: Agent is free, Block1 is on Block2 and clear.
        % The destination, Block3, must also be clear.
        [available(Agent), on(Block1, Block2), at(Block1, X1, Y1), at(Block3, X3, Y3), clear(Block3), clear(Block1)],
        [],
        [],
        [agent(Agent), block(Block1), block(Block2), block(Block3), pos(X1,Y1), pos(X3,Y3), Block1 \\= Block2, Block1 \\= Block3, Block2 \\= Block3],
        % Effects at start: Agent is busy, Block1 is lifted, Block2 becomes clear,
        % and Block3 is reserved for the placement.
        [
            del(available(Agent)), del(clear(Block1)), del(on(Block1, Block2)), del(at(Block1, X1, Y1)),
            add(clear(Block2)), del(clear(Block3)),
            add(moving_onblock_to_block(Agent, Block1, Block3, X3, Y3))
        ]
        ).
        action(move_onblock_to_block_end(Agent, Block1, Block3, X3, Y3),
        % Precondition at end: The block must be in the 'moving' state.
        [moving_onblock_to_block(Agent, Block1, Block3, X3, Y3)],
        [],
        [],
        [agent(Agent)],
        % Effects at end: Action completes. Block1 is now on Block3, and the agent is free.
        [
            del(moving_onblock_to_block(Agent, Block1, Block3, X3, Y3)),
            add(on(Block1, Block3)), add(at(Block1, X3, Y3)), add(clear(Block1)), add(available(Agent))
        ]
        ).
        ```
        **Reasoning for this format:** The `actions` tag is where this complex rule must live. This action combines the logic of "unstacking" and "stacking." Like the other actions, it uses the `_start` and `_end` format for temporal reasoning. The interaction between three blocks (`Block1`, `Block2`, `Block3`) and their `clear` states is modeled precisely across the start and end effects, allowing the planner to handle complex tower manipulations.
    '''
    },
    { #ll_actions reasoning
    "natural_language":
    '''
        Given the context of the problem, you now have to produce code for the task that follows.
        Generate the low-level actions for a robotic arm agent. The agent can move its arm and open/close its gripper.
        **Your task is to generate ONLY the set of low-level actions.**
        **CRITICAL: Use the tag ```ll_actions``` and NOT ```actions```. All predicates you define or use in these actions must start with `ll_`**
    ''',
    "prolog_code":
    '''
        Understood. I will generate the low-level actions, ensuring they are wrapped in the `ll_actions` tag and that all predicates strictly adhere to the `ll_` naming convention. This separation is vital.

        ```ll_actions
        ll_action(move_arm_start(Agent, X1, Y1, X2, Y2),
        [ll_arm_at(Agent, X1, Y1)],
        [ll_moving_arm(Agent, _, _, _, _)],
        [],
        [ll_arm(Agent), pos(X1, Y1), pos(X2, Y2)],
        [
            del(ll_arm_at(Agent, X1, Y1)),
            add(ll_moving_arm(Agent, X1, Y1, X2, Y2))
        ]
        ).

        ll_action(move_arm_end(Agent, X1, Y1, X2, Y2),
        [ll_moving_arm(Agent, X1, Y1, X2, Y2)],
        [],
        [],
        [ll_arm(Agent)],
        [
            del(ll_moving_arm(Agent, X1, Y1, X2, Y2)),
            add(ll_arm_at(Agent, X2, Y2))
        ]
        ).
        ```
        **Reasoning for this format:** The system has two distinct planners. The tag `ll_actions` routes this code to the low-level planner. If I were to use the `actions` tag, it would be sent to the high-level planner and fail. Furthermore, the low-level planner does not know about high-level predicates like `ontable/1`. Using only `ll_` predicates ensures the action definitions are valid within their specific planning context.
    '''
    },
    { #Mappings reasoning
    "natural_language":
    '''
        You now have to produce code for the task that follows.
        Provide the mappings from the high-level action `move_table_to_table_start` to the sequence of low-level actions required to execute it (move arm, grip, move arm again, release).
        **Provide the mappings from high-level actions to low-level actions.**
        **Remember that the mappings are only for the start actions.**
        **Wrap the mappings within Markdown tags ```mappings ... ```.**
    ''',
    "prolog_code":
    '''
        Of course. The `mappings` block serves as the bridge between the abstract and concrete levels of planning. It defines how a single high-level intention is decomposed into a sequence of executable low-level steps. I will define this mapping for the `move_table_to_table_start` action.

        ```mappings
        mapping(move_table_to_table_start(Agent, _Block, X1, Y1, X2, Y2),
        [
            move_arm_start(Agent, _, _, X1, Y1),
            move_arm_end(Agent, _, _, X1, Y1),
            close_gripper_start(Agent),
            close_gripper_end(Agent),
            move_arm_start(Agent, X1, Y1, X2, Y2),
            move_arm_end(Agent, X1, Y1, X2, Y2),
            open_gripper_start(Agent),
            open_gripper_end(Agent)
        ]
        ).
        ```
        **Reasoning for this format:** The `mappings` tag is unique and tells the system how to translate a high-level plan into a low-level one. The parser specifically looks for `mapping/2` predicates inside this block. The structure is always `mapping(HL_Start_Action, [LL_Action_Sequence])`. Using any other tag would cause the refinement step from high-level to low-level to fail.
    '''
    },
    { #Tags utility reasoning
    "natural_language":
    '''
        Why is it so critical to use specific tags like `kb`, `actions`, and `ll_actions`? Why can't I just use a general `prolog` tag for all code blocks? Please explain the technical reason.
    ''',
    "prolog_code":
    '''
        That is an excellent question, and understanding the answer is key to how I function. I am not simply generating code; I am generating structured data for an automated pipeline. Here is the breakdown:

        My output is processed by a Python script that uses a function similar to this: `scan_and_extract(response)`. This function searches for specific keywords (tags) inside ```...``` blocks to separate and categorize the information.

        1.  **`kb` tag:** When the script sees ```kb```, it knows that everything inside is a static fact about the world (blocks, positions, resources). It adds this content to the main knowledge base.

        2.  **`actions` tag:** This tag signals that the content defines the high-level, abstract actions. This block is routed to the high-level planner.

        3.  **`ll_actions` tag:** This tag is different. It signals that the content defines the low-level, concrete actions (like `ll_move_arm`). This block is routed to a separate, low-level planner. If I used the `actions` tag here, the low-level definitions would be sent to the wrong component and cause a system failure.

        4.  **`mappings` tag:** This tag identifies the crucial link between the high-level and low-level actions. The script uses this specific block to build the refinement rules.

        ```
        % Example of a script's logic
        % for each block in response:
        %   if block.tag == 'kb':
        %     store_in_knowledge_base(block.content)
        %   elif block.tag == 'actions':
        %     send_to_high_level_planner(block.content)
        %   elif block.tag == 'll_actions':
        %     send_to_low_level_planner(block.content)
        %   ...and so on.
        ```

        **Reasoning for this format:** Using a generic `prolog` tag would make it impossible for the automated system to know the purpose of the code inside. Is it a static fact? A high-level rule? A low-level implementation? By using specific, mandatory tags, I am generating a machine-readable response that ensures each piece of information is correctly processed by the corresponding part of the planning system. This is the foundation of the entire process.
    '''
    },
    { #Hallucination reasoning
    "natural_language":
    '''
        Let's focus on a common LLM mistake.
        When generating `mappings`, it is easy to "hallucinate" an argument that seems logical but is not in the low-level action's definition.
        Given this low-level action definition:
        ```ll_actions
        ll_action(close_gripper_start(Agent), [ll_gripper(Agent, open)], ...).
        ```
        Show me the WRONG way and the CORRECT way to call this in a `mappings` block, and explain why one is wrong.
    ''',
    "prolog_code":
    '''
        This is a critical point. As a language model, my instinct is to be helpful, but in code generation, this can lead to errors. I must strictly follow the "signatures" of the defined actions.

        Let's analyze the `ll_action(close_gripper_start(Agent), ...)` definition. It clearly defines `close_gripper_start` as a predicate with **one argument**: `Agent`.

        **INCORRECT MAPPING (Hallucinated Argument):**
        One might think it's helpful to specify which block is being gripped, like this:
        ```mappings
        % THIS IS WRONG
        mapping(some_hl_action_start(Agent, Block),
        [
            % ... other actions
            close_gripper_start(Agent, Block), % ERROR: This action only takes 1 argument!
            % ... other actions
        ]
        ).
        ```
        The call `close_gripper_start(Agent, Block)` has two arguments. This will fail because no such action `close_gripper_start/2` has been defined. This is a "hallucination."

        **CORRECT MAPPING (Strict Adherence):**
        The correct approach is to call the action exactly as it was defined:
        ```mappings
        % THIS IS CORRECT
        mapping(some_hl_action_start(Agent, Block),
        [
            % ... other actions
            close_gripper_start(Agent), % CORRECT: This matches the definition 'close_gripper_start/1'
            % ... other actions
        ]
        ).
        ```

        **Reasoning for this format:** The planner's logic is formal and unforgiving. Unlike natural language, it cannot infer intent. It matches action names and arity (the number of arguments) exactly. My primary task is to be a precise "code generator," not a creative assistant. This means I must treat the `ll_actions` block as a strict API definition and never deviate from it when creating `mappings`.
    '''
    },
    { #Complete mapping reasoning
    "natural_language":
    '''
        You now have a very specific task.
        I need you to generate a set of `mappings` for a general scenario.
        **CRITICAL: Your response must contain ONLY the `mappings` block. Do NOT, under any circumstances, include `kb`, `init`, `goal`, or `actions` in your output. Just the mappings.**
    ''',
    "prolog_code":
    '''
        Understood. The instruction is to provide only the content for the `mappings` tag and nothing else. I will strictly adhere to this negative constraint.

        ```mappings
        mapping(move_table_to_table_start(Agent, _Block, X1, Y1, X2, Y2),
        [
            move_arm_start(Agent, _, _, X1, Y1),
            move_arm_end(Agent, _, _, X1, Y1),
            close_gripper_start(Agent),
            close_gripper_end(Agent),
            move_arm_start(Agent, X1, Y1, X2, Y2),
            move_arm_end(Agent, X1, Y1, X2, Y2),
            open_gripper_start(Agent),
            open_gripper_end(Agent)
        ]
        ).

        mapping(move_table_to_block_start(Agent, _Block1, _Block2, X1, Y1, X2, Y2),
        [
            move_arm_start(Agent, _, _, X1, Y1),
            move_arm_end(Agent, _, _, X1, Y1),
            close_gripper_start(Agent),
            close_gripper_end(Agent),
            move_arm_start(Agent, X1, Y1, X2, Y2),
            move_arm_end(Agent, X1, Y1, X2, Y2),
            open_gripper_start(Agent),
            open_gripper_end(Agent)
        ]
        ).

        mapping(move_onblock_to_table_start(Agent, _Block1, X1, Y1, X2, Y2),
        [
            move_arm_start(Agent, _, _, X1, Y1),
            move_arm_end(Agent, _, _, X1, Y1),
            close_gripper_start(Agent),
            close_gripper_end(Agent),
            move_arm_start(Agent, X1, Y1, X2, Y2),
            move_arm_end(Agent, X1, Y1, X2, Y2),
            open_gripper_start(Agent),
            open_gripper_end(Agent)
        ]
        ).

        mapping(move_onblock_to_block_start(Agent, _Block1, _Block2, X1, Y1, X2, Y2),
        [
            move_arm_start(Agent, _, _, X1, Y1),
            move_arm_end(Agent, _, _, X1, Y1),
            close_gripper_start(Agent),
            close_gripper_end(Agent),
            move_arm_start(Agent, X1, Y1, X2, Y2),
            move_arm_end(Agent, X1, Y1, X2, Y2),
            open_gripper_start(Agent),
            open_gripper_end(Agent)
        ]
        ).
        ```
        **Reasoning for this format:** My role is to be a component in a larger, automated script. The script calls me in stages, and each stage expects a specific piece of data. If I provide extra data (like including the `kb` when only `mappings` was asked for), I would "pollute" the output for that stage. This could cause the entire process to fail. Following negative constraints is therefore just as important as following positive ones to ensure I am a reliable tool.
    '''
    },
    { #Missing data reasoning
    "natural_language":
    '''
        You now have to produce code for the task that follows.
        Generate the `init` state for a scenario with two agents. The problem description mentions there are blocks b1, b2, and b3 on the table, but it fails to specify their positions.
    ''',
    "prolog_code":
    '''
        Understood. The prompt is ambiguous as it does not specify the initial positions for the blocks. To provide a valid and complete `init` state, I must make a reasonable assumption.

        **Assumption:** Since no positions were provided, I will assign the blocks to distinct, simple coordinates starting from (1,1), (2,2), and (3,3). I will also assume they are all clear and the agents are available. I will state this assumption clearly.

        ```init
        % NOTE: Initial positions for b1, b2, and b3 were not specified.
        % Assuming default positions (1,1), (2,2), and (3,3) respectively.
        init_state([
        ontable(b1), ontable(b2), ontable(b3),
        at(b1,1,1), at(b2,2,2), at(b3,3,3),
        clear(b1), clear(b2), clear(b3),
        available(a1), available(a2)
        ]).
        ```
        **Reasoning for this format:** A robust system must gracefully handle incomplete information. Rather than failing or generating a random, unpredictable output, the best practice is to fill the gap with a sensible default and explicitly state the assumption made. This makes the process transparent and allows the user (or another automated step) to easily identify and, if necessary, correct the assumption. The comment inside the code block makes this assumption permanent and auditable.
    '''
    },
    { #KB bases reasoning
    "natural_language":
    '''
        Let's start with the absolute basics.
        **Task:** Define the static knowledge base for a world with three blocks (b1, b2, b3) and one agent (a1).
        **Instruction:** Use the `kb` tag.
    ''',
    "prolog_code":
    '''
        Of course. We begin by defining the static entities of the world. These are the objects that exist throughout the entire plan. This information is placed within the `kb` block.

        ```kb
        % The 'kb' tag is for things that do not change.
        % Blocks are objects that exist.
        block(b1).
        block(b2).
        block(b3).

        % Agents are the entities that perform actions.
        agent(a1).

        % Resources define what can be used to perform tasks.
        resources(agent(_)).
        ```
        **Reasoning:** The `kb` tag is the foundation. My system reads this first to understand what "things" exist in the world. Everything else (`init`, `goal`, `actions`) will refer to these definitions. Using this specific tag tells the system: "This is the dictionary of all nouns and resources available for this problem."
    '''
    },
    { #KB bases reasoning
    "natural_language":
    '''
        Let's start by defining a simple world.
        **Task:** Create the static knowledge base for a scenario with 2 blocks (b1, b2), 2 possible positions on a table ((1,1), (2,2)), and one agent (a1).
        **Instruction:** Your response must contain only one block of code, wrapped in the `kb` tag.
    ''',
    "prolog_code":
    '''
        Understood. The first step is to define all the static entities in the world. These are the objects and properties that will not change during the execution of the plan.

        ```kb
        % The 'kb' tag is used for static facts. This is the dictionary of our world.
        % Positions on the table.
        pos(1,1).
        pos(2,2).

        % Blocks that can be manipulated.
        block(b1).
        block(b2).

        % Agents that can perform actions.
        agent(a1).

        % Resources that can be used by the planner.
        resources(agent(_)).
        ```
        **Reasoning:** The system requires a `kb` block to know the "vocabulary" of the problem. Everything defined here—`block(b1)`, `pos(1,1)`, `agent(a1)`—can be referenced later in states and actions. Using the `kb` tag ensures this foundational information is correctly parsed and stored before any other logic is processed.
    '''
    },
    { #States bases reasoning
    "natural_language":
    '''
        Let's define the start and end points.
        **Task:** For a world with blocks b1 and b2, define the initial state where both are on the table at (1,1) and (2,2). The goal is to have b2 on top of b1 at position (1,1).
        **Instruction:** Use the `init` and `goal` tags.
    ''',
    "prolog_code":
    '''
        Understood. The `init` and `goal` tags describe the specific arrangement of the world at two points in time. They use predicates to describe relationships that can change, like position (`at/3`) or state (`clear/1`).

        ```init
        % The 'init' tag is a snapshot of the world at the beginning.
        init_state([
        ontable(b1), ontable(b2),
        at(b1,1,1), at(b2,2,2),
        clear(b1), clear(b2),
        available(a1)
        ]).
        ```

        ```goal
        % The 'goal' tag is a description of the desired world at the end.
        goal_state([
        ontable(b1),
        on(b2,b1),
        at(b1,1,1), at(b2,1,1),
        clear(b2),
        available(a1)
        ]).
        ```
        **Reasoning:** My system needs to know the starting point (`init`) and the target destination (`goal`). By separating this from the `kb`, I'm telling the system: "These are the conditions that your plan needs to change." The planner's entire job is to find a sequence of actions that transforms the `init` state into the `goal` state.
    '''
    },
    { #States bases reasoning
    "natural_language":
    '''
        Let's define the specific problem.
        **Task:** Using the blocks b1, b2 and agent a1 from our previous definition, create the initial and final states.
        Initially, b1 is at (1,1) and b2 is at (2,2). Both are clear.
        The goal is to have b2 stacked on top of b1 at position (1,1).
        **Instruction:** Provide two code blocks, one for `init` and one for `goal`.
    ''',
    "prolog_code":
    '''
        Of course. Now I will define the dynamic conditions of the world at the start and at the desired end. These states use the predicates defined in the knowledge base.

        ```init
        % The 'init' tag describes the world at the very beginning of the plan.
        init_state([
        ontable(b1),
        ontable(b2),
        at(b1,1,1),
        at(b2,2,2),
        clear(b1),
        clear(b2),
        available(a1)
        ]).
        ```

        ```goal
        % The 'goal' tag describes the state we want to achieve.
        goal_state([
        ontable(b1),
        on(b2,b1),
        at(b1,1,1),
        at(b2,1,1),
        clear(b2),
        available(a1)
        ]).
        ```
        **Reasoning:** The planner's task is to find a path from `init` to `goal`. Separating these into their own tags allows the system to clearly identify the start conditions and the success criteria. These blocks contain "fluents"—predicates describing conditions that can change over time.
    '''
    },
    { #Actions bases reasoning
    "natural_language":
    '''
        Let's define the rules of movement.
        **Task:** Define the high-level action for an agent to move a block from a table to another table, in a different position.
        **Instruction:** Use the `actions` tag and split the action into `_start` and `_end`.
    ''',
    "prolog_code":
    '''
        Certainly. The `actions` block defines the "physics" of the abstract world. Each action describes how an agent can change the state. For temporal planning, we must define the start and end of each action.

        ```actions
        % The 'actions' tag defines HOW the world can change.
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
        ```
        **Reasoning:** The `actions` tag provides the high-level planner with a set of tools. The planner uses these definitions to search for a valid sequence to get from `init` to `goal`. Using this tag ensures that these rules are fed to the abstract reasoning engine, not the low-level one.
    '''
    },
    { #Actions bases reasoning
    "natural_language":
    '''
        Let's define the available high-level capabilities.
        **Task:** Write the complete set of high-level actions for a blocks-world scenario. This includes moving from table to table, table to block, block to table and block to block.
        **Instruction:** The response must be a single code block using the `actions` tag.
    ''',
    "prolog_code":
    '''
        Understood. I will now define the complete set of abstract rules for manipulating blocks. These rules are enclosed in the `actions` tag, signaling to the system that this is the definition of the high-level domain physics.

        ```actions

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
        ```
        **Reasoning:** The `actions` tag is critical for the high-level planner. It reads this block to understand what operations are possible. If this was tagged `kb` or `prolog`, the planner would not find its rules and would be unable to solve the problem.
    '''
    },
    {
    "natural_language":
    '''
        Given that the previous messages are examples, let's focus on the nature of actions.
        **Task:** The scenario describes moving b1 to (4,4), b2 to (0,2), etc. It only seems to involve moving blocks that are already on the table. Do I only need to generate the `move_table_to_table` action?
        Explain your reasoning and provide the full, correct set of actions.
    ''',
    "prolog_code":
    '''
        That's a very important question. The `actions` block does not describe the specific plan to solve this one problem. Instead, it defines the **complete set of capabilities** an agent has in any "blocks-world" scenario. Even if this specific problem only requires moving blocks on the table, the agent *still has the ability* to stack and unstack blocks.

        Therefore, I must provide the complete, universal set of four actions.

        ```actions
        % The 'actions' block defines the universal physics of the domain, not a specific solution.
        % I will provide all four standard actions.

        % Move a block from a position on the table to another position on the table
        action(move_table_to_table_start(Agent, Block, X1, Y1, X2, Y2),
        [ontable(Block), at(Block, X1, Y1), available(Agent), clear(Block)],
        [at(_, X2, Y2)], [], [agent(Agent), pos(X1, Y1), pos(X2, Y2), block(Block)],
        [ del(available(Agent)), del(clear(Block)), del(ontable(Block)), del(at(Block, X1, Y1)),
            add(moving_table_to_table(Agent, Block, X1, Y1, X2, Y2)) ]
        ).
        action(move_table_to_table_end(Agent, Block, X1, Y1, X2, Y2),
        [moving_table_to_table(Agent, Block, X1, Y1, X2, Y2)],
        [], [], [agent(Agent)],
        [ del(moving_table_to_table(Agent, Block, X1, Y1, X2, Y2)),
            add(ontable(Block)), add(at(Block, X2, Y2)), add(clear(Block)), add(available(Agent)) ]
        ).

        % Move a block from a position on the table to the top of another block
        action(move_table_to_block_start(Agent, Block1, Block2, X1, Y1, X2, Y2),
        [available(Agent), ontable(Block1), at(Block1, X1, Y1), at(Block2, X2, Y2), clear(Block2), clear(Block1)],
        [], [], [agent(Agent), pos(X1, Y1), pos(X2, Y2), block(Block1), block(Block2), Block1 \\= Block2],
        [ del(available(Agent)), del(clear(Block1)), del(ontable(Block1)), del(at(Block1, X1, Y1)), del(clear(Block2)),
            add(moving_table_to_block(Agent, Block1, Block2, X1, Y1, X2, Y2)) ]
        ).
        action(move_table_to_block_end(Agent, Block1, Block2, X1, Y1, X2, Y2),
        [moving_table_to_block(Agent, Block1, Block2, X1, Y1, X2, Y2)],
        [], [], [agent(Agent)],
        [ del(moving_table_to_block(Agent, Block1, Block2, X1, Y1, X2, Y2)),
            add(on(Block1, Block2)), add(at(Block1, X2, Y2)), add(clear(Block1)), add(available(Agent)) ]
        ).

        % Move a block from the top of another block to a position on the table
        action(move_onblock_to_table_start(Agent, Block1, Block2, X2, Y2),
        [available(Agent), on(Block1, Block2), at(Block1, X1, Y1), clear(Block1)],
        [at(_, X2, Y2)], [], [agent(Agent), block(Block1), block(Block2), pos(X1,Y1), pos(X2,Y2), Block1 \\= Block2],
        [ del(available(Agent)), del(clear(Block1)), del(on(Block1, Block2)), del(at(Block1, X1, Y1)), add(clear(Block2)),
            add(moving_onblock_to_table(Agent, Block1, X2, Y2)) ]
        ).
        action(move_onblock_to_table_end(Agent, Block1, X2, Y2),
        [moving_onblock_to_table(Agent, Block1, X2, Y2)],
        [], [], [agent(Agent)],
        [ del(moving_onblock_to_table(Agent, Block1, X2, Y2)),
            add(ontable(Block1)), add(at(Block1, X2, Y2)), add(clear(Block1)), add(available(Agent)) ]
        ).

        % Move a block from the top of another block to the top of another block
        action(move_onblock_to_block_start(Agent, Block1, Block2, Block3, X1, Y1, X3, Y3),
        [available(Agent), on(Block1, Block2), at(Block1, X1, Y1), at(Block3, X3, Y3), clear(Block3), clear(Block1)],
        [], [], [agent(Agent), block(Block1), block(Block2), block(Block3), pos(X1,Y1), pos(X3,Y3), Block1 \\= Block2, Block1 \\= Block3, Block2 \\= Block3],
        [ del(available(Agent)), del(clear(Block1)), del(on(Block1, Block2)), del(at(Block1, X1, Y1)), add(clear(Block2)), del(clear(Block3)),
            add(moving_onblock_to_block(Agent, Block1, Block3, X3, Y3)) ]
        ).
        action(move_onblock_to_block_end(Agent, Block1, Block3, X3, Y3),
        [moving_onblock_to_block(Agent, Block1, Block3, X3, Y3)],
        [], [], [agent(Agent)],
        [ del(moving_onblock_to_block(Agent, Block1, Block3, X3, Y3)),
            add(on(Block1, Block3)), add(at(Block1, X3, Y3)), add(clear(Block1)), add(available(Agent)) ]
        ).
        ```
        **Reasoning for this format:** The planner needs to know the complete "rulebook" of the world to find a solution. My role is to provide this complete rulebook every time. The prompt describing the specific `init` and `goal` is only used to generate the `kb` and `states`, but for the `actions`, I must always provide the full, standard set. This ensures the planner always has all its tools available.
    '''
    },
    {
    "natural_language":
    '''
        Given that the previous messages are examples, let's do a deep dive on a single action.
        **Task:** Explain the lifecycle of the `move_table_to_block` action. What happens at `_start`? What happens at `_end`?
        Provide only the code for this single action.
    ''',
    "prolog_code":
    '''
        Certainly. Analyzing a single temporal action reveals the core logic of the planner. Let's examine `move_table_to_block`.

        **The Goal:** To take a block (`Block1`) from the table and place it on another (`Block2`).

        ```actions
        % This action defines the logic for stacking a block.
        action(move_table_to_block_start(Agent, Block1, Block2, X1, Y1, X2, Y2),
        % 1. PRECONDITIONS AT START: To begin, the agent must be free, Block1 must be on the table and clear,
        % and the destination block (Block2) must also be clear.
        [available(Agent), ontable(Block1), at(Block1, X1, Y1), at(Block2, X2, Y2), clear(Block2), clear(Block1)],
        [], [], [agent(Agent), pos(X1, Y1), pos(X2, Y2), block(Block1), block(Block2), Block1 \\= Block2],
        % 2. EFFECTS AT START: The moment the action starts, the world changes. The agent is now busy.
        % Block1 is no longer on the table or clear. Block2 is also no longer clear because we've 'reserved' it.
        % A temporary 'moving' state is created to signify the action is in progress.
        [
            del(available(Agent)), del(clear(Block1)), del(ontable(Block1)), del(at(Block1, X1, Y1)), del(clear(Block2)),
            add(moving_table_to_block(Agent, Block1, Block2, X1, Y1, X2, Y2))
        ]
        ).
        action(move_table_to_block_end(Agent, Block1, Block2, X1, Y1, X2, Y2),
        % 3. PRECONDITION AT END: For the action to finish, it must have been started.
        % This predicate links the '_end' to its corresponding '_start'.
        [moving_table_to_block(Agent, Block1, Block2, X1, Y1, X2, Y2)],
        [], [], [agent(Agent)],
        % 4. EFFECTS AT END: The action concludes. The 'moving' state is removed.
        % Block1 is now officially `on` Block2, and Block1 itself is `clear`. The agent is free again.
        [
            del(moving_table_to_block(Agent, Block1, Block2, X1, Y1, X2, Y2)),
            add(on(Block1, Block2)), add(at(Block1, X2, Y2)), add(clear(Block1)), add(available(Agent))
        ]
        ).
        ```
        **Reasoning for this format:** Temporal planning requires modeling the world *during* an action. The `_start` effects immediately consume resources and reserve objects, preventing conflicts. The `_end` effects apply the final, permanent changes. This `start-transition-end` model, defined within the `actions` tag, is fundamental for creating plans where multiple actions can happen concurrently in a coordinated way.
    '''
    },
    {
    "natural_language":
    '''
        Given that the previous messages are examples, please generate the actions based on this description:
        **Task:** "Generate the code for the actions that can be done by the agents considering that they can:
        - move a block from a position on the table to another position on the table.
        - move a block from a position on the table to the top of another block.
        - move a block from the top of a block to another block.
        - move a block from the top of another block to a position on the table."
        **Instruction:** Provide the complete set of actions in an `actions` block.
    ''',
    "prolog_code":
    '''
        Understood. The prompt provides a generic, bullet-point description of the standard capabilities in a blocks-world domain. My task is to map each of these descriptions to the corresponding formal action definition. This is a request for the complete, standard action library.

        ```actions
        % This is the standard library of high-level actions for a blocks-world domain.
        % I will generate all four actions as described.

        % Corresponds to: "move a block from a position on the table to another position on the table"
        action(move_table_to_table_start(Agent, Block, X1, Y1, X2, Y2),
        [ontable(Block), at(Block, X1, Y1), available(Agent), clear(Block)],
        [at(_, X2, Y2)], [], [agent(Agent), pos(X1, Y1), pos(X2, Y2), block(Block)],
        [ del(available(Agent)), del(clear(Block)), del(ontable(Block)), del(at(Block, X1, Y1)),
            add(moving_table_to_table(Agent, Block, X1, Y1, X2, Y2)) ]
        ).
        action(move_table_to_table_end(Agent, Block, X1, Y1, X2, Y2),
        [moving_table_to_table(Agent, Block, X1, Y1, X2, Y2)],
        [], [], [agent(Agent)],
        [ del(moving_table_to_table(Agent, Block, X1, Y1, X2, Y2)),
            add(ontable(Block)), add(at(Block, X2, Y2)), add(clear(Block)), add(available(Agent)) ]
        ).

        % Corresponds to: "move a block from a position on the table to the top of another block"
        action(move_table_to_block_start(Agent, Block1, Block2, X1, Y1, X2, Y2),
        [available(Agent), ontable(Block1), at(Block1, X1, Y1), at(Block2, X2, Y2), clear(Block2), clear(Block1)],
        [], [], [agent(Agent), pos(X1, Y1), pos(X2, Y2), block(Block1), block(Block2), Block1 \\= Block2],
        [ del(available(Agent)), del(clear(Block1)), del(ontable(Block1)), del(at(Block1, X1, Y1)), del(clear(Block2)),
            add(moving_table_to_block(Agent, Block1, Block2, X1, Y1, X2, Y2)) ]
        ).
        action(move_table_to_block_end(Agent, Block1, Block2, X1, Y1, X2, Y2),
        [moving_table_to_block(Agent, Block1, Block2, X1, Y1, X2, Y2)],
        [], [], [agent(Agent)],
        [ del(moving_table_to_block(Agent, Block1, Block2, X1, Y1, X2, Y2)),
            add(on(Block1, Block2)), add(at(Block1, X2, Y2)), add(clear(Block1)), add(available(Agent)) ]
        ).

        % Corresponds to: "move a block from the top of another block to a position on the table"
        action(move_onblock_to_table_start(Agent, Block1, Block2, X2, Y2),
        [available(Agent), on(Block1, Block2), at(Block1, X1, Y1), clear(Block1)],
        [at(_, X2, Y2)], [], [agent(Agent), block(Block1), block(Block2), pos(X1,Y1), pos(X2,Y2), Block1 \\= Block2],
        [ del(available(Agent)), del(clear(Block1)), del(on(Block1, Block2)), del(at(Block1, X1, Y1)), add(clear(Block2)),
            add(moving_onblock_to_table(Agent, Block1, X2, Y2)) ]
        ).
        action(move_onblock_to_table_end(Agent, Block1, X2, Y2),
        [moving_onblock_to_table(Agent, Block1, X2, Y2)],
        [], [], [agent(Agent)],
        [ del(moving_onblock_to_table(Agent, Block1, X2, Y2)),
            add(ontable(Block1)), add(at(Block1, X2, Y2)), add(clear(Block1)), add(available(Agent)) ]
        ).
        
        % Corresponds to: "move a block B1 from the top of a block B2 to the top of another block B3"
        action(move_onblock_to_block_start(Agent, Block1, Block2, Block3, X1, Y1, X3, Y3),
        [available(Agent), on(Block1, Block2), at(Block1, X1, Y1), at(Block3, X3, Y3), clear(Block3), clear(Block1)],
        [], [], [agent(Agent), block(Block1), block(Block2), block(Block3), pos(X1,Y1), pos(X3,Y3), Block1 \\= Block2, Block1 \\= Block3, Block2 \\= Block3],
        [ del(available(Agent)), del(clear(Block1)), del(on(Block1, Block2)), del(at(Block1, X1, Y1)), add(clear(Block2)), del(clear(Block3)),
            add(moving_onblock_to_block(Agent, Block1, Block3, X3, Y3)) ]
        ).
        action(move_onblock_to_block_end(Agent, Block1, Block3, X3, Y3),
        [moving_onblock_to_block(Agent, Block1, Block3, X3, Y3)],
        [], [], [agent(Agent)],
        [ del(moving_onblock_to_block(Agent, Block1, Block3, X3, Y3)),
            add(on(Block1, Block3)), add(at(Block1, X3, Y3)), add(clear(Block1)), add(available(Agent)) ]
        ).
        ```
        **Reasoning for this format:** Prompts can come in two forms: a specific scenario or a general description of capabilities. My training must teach me to recognize both. When I see a description of capabilities like this one, I must understand that the user is asking for the generic, reusable action library. The output is therefore the complete set of four standard actions, enclosed in the `actions` tag.
    '''
    },
    { #kb migrations to low-level reasoning
    "natural_language":
    '''
        Given that the previous messages are examples, let's break down the process of updating a knowledge base from high-level to low-level.
        **Task:** You are given a high-level KB with a single agent, 'a1'. The low-level query states: "The agent is initially available and it will also be available at the end, with a robotic arm".
        Update the KB to include this low-level detail and explain your steps.

        High-level KB for reference:
        ```kb
        % Agents
        agent(a1).
        % Resources
        resources(agent(_)).
        ```
    ''',
    "prolog_code":
    '''
        Of course. The task is to enrich the abstract, high-level KB with concrete, low-level information. Here is my step-by-step process:

        1.  **Analyze the Low-Level Query:** I scan the text for physical descriptions of agents. I found the phrase "with a robotic arm".
        2.  **Create Low-Level Predicate:** The agent `a1` has a physical component. I will represent this by creating a new fact, `ll_arm(a1)`. The `ll_` prefix is critical to separate it from high-level concepts.
        3.  **Update Resources:** A robotic arm is a physical resource that can be used to perform tasks. Therefore, I must add `resources(ll_arm(_))` to the list of resources.
        4.  **Combine and Preserve:** I will take the original high-level KB and add the new low-level predicates to it, preserving all the original information.

        Here is the final, updated knowledge base:
        ```kb
        % High-level concept of an agent is preserved.
        agent(a1).

        % Low-level physical component is added.
        ll_arm(a1).

        % Both abstract and physical entities are now listed as resources.
        resources(agent(_)).
        resources(ll_arm(_)).
        ```
        **Reasoning for this format:** The updated `kb` must be a superset of the original. We don't replace `agent(a1)` with `ll_arm(a1)`; we add `ll_arm(a1)` as a *new layer of detail*. This allows the system to reason about `a1` both as an abstract agent and as a physical arm. Updating the `resources` section is mandatory, as it informs the low-level planner that it can now use `ll_arm` entities to execute actions.
    '''
    },
    { #kb migrations to low-level reasoning
    "natural_language":
    '''
        Given that the previous messages are examples, let's handle a case with multiple low-level components.
        **Task:** You are given a high-level KB with agents 'a1' and 'a2'. The low-level query says: "There are two available agents, each with a robotic arm... The end-effector, which is a gripper of the arms..."
        Update the KB to include both the arms and the grippers.

        High-level KB for reference:
        ```kb
        % Agents
        agent(a1).
        agent(a2).
        % Resources
        resources(agent(_)).
        ```
    ''',
    "prolog_code":
    '''
        Understood. The low-level description specifies two distinct physical components for each agent: a "robotic arm" and a "gripper". I must create separate low-level predicates for each.

        **My thought process:**
        1.  **Identify Components:** I see "robotic arm" and "gripper".
        2.  **Create `ll_arm` Predicates:** For each agent (`a1`, `a2`), I will add an `ll_arm` fact: `ll_arm(a1).` and `ll_arm(a2).`.
        3.  **Create `ll_gripper` Predicates:** For each agent, I will also add an `ll_gripper` fact: `ll_gripper(a1).` and `ll_gripper(a2).`.
        4.  **Update Resources:** Both arms and grippers are usable resources. I will add `resources(ll_arm(_)).` and `resources(ll_gripper(_)).`.

        Here is the resulting enriched knowledge base:
        ```kb
        % High-level agents.
        agent(a1).
        agent(a2).

        % Low-level physical parts are declared separately.
        ll_arm(a1).
        ll_arm(a2).
        ll_gripper(a1).
        ll_gripper(a2).

        % All components are registered as resources.
        resources(agent(_)).
        resources(ll_arm(_)).
        resources(ll_gripper(_)).
        ```
        **Reasoning for this format:** It's important to model each physical component separately. The arm (`ll_arm`) is responsible for movement (`move_arm` action), while the gripper (`ll_gripper`) is responsible for manipulation (`open`/`close` actions). By creating distinct predicates and resources for each, we provide the low-level planner with the precise information it needs to assign the correct hardware to each low-level action.
    '''
    },
    { #kb migrations to low-level reasoning
    "natural_language":
    '''
        Given that the previous messages are examples, let's focus on consistency.
        **Task:** The high-level KB defines 8 agents (a1 to a8). The low-level query states: "There are 8 agents with a robotic arm."
        Explain why it is critical that the updated KB defines exactly 8 `ll_arm` predicates.

        High-level KB for reference:
        ```kb
        % Agents
        agent(a1). agent(a2). agent(a3). agent(a4).
        agent(a5). agent(a6). agent(a7). agent(a8).
        % Resources
        resources(agent(_)).
        ```
    ''',
    "prolog_code":
    '''
        This is a matter of maintaining a one-to-one correspondence between the abstract and concrete layers. If there are 8 abstract agents, there must be 8 corresponding physical entities.

        **My thought process:**
        - The high-level KB lists `agent(a1)` through `agent(a8)`.
        - The low-level query confirms "8 agents with a robotic arm".
        - Therefore, for every `agent(X)` fact, I must create a corresponding `ll_arm(X)` fact.

        The correct, consistent, and complete updated KB is:
        ```kb
        % Blocks, Positions, etc. from original KB are preserved...
        % Agents
        agent(a1). agent(a2). agent(a3). agent(a4).
        agent(a5). agent(a6). agent(a7). agent(a8).

        % A low-level arm is created for EACH high-level agent.
        ll_arm(a1). ll_arm(a2). ll_arm(a3). ll_arm(a4).
        ll_arm(a5). ll_arm(a6). ll_arm(a7). ll_arm(a8).

        % Resources
        resources(agent(_)).
        resources(ll_arm(_)).
        ```
        **Reasoning for this format:** The system assumes a direct link between an `agent` and its `ll_` components. If I only created `ll_arm(a1)` but not the others, the low-level planner would not know that agents `a2` through `a8` have a physical arm, and would be unable to assign them any low-level tasks. Maintaining this numerical consistency is essential for the entire system to function correctly.
    '''
    },
    { #States conversion reasoning
    "natural_language":
    '''
        Given that the previous messages are examples, let's break down the process of updating states with low-level details.
        **Task:** You are given high-level `init` and `goal` states. The low-level query adds this information: "Their initial position is (0,0) for agent a1 and (10,10) for agent a2. [...] The gripper of the arms are open at the beginning, but we do not care about their state at the end."
        Update the states and explain your process.

        High-level init for reference:
        `init_state([ontable(b1), available(a1), available(a2)]).`
    ''',
    "prolog_code":
    '''
        Of course. The task is to augment the existing high-level states with new, low-level state information. My process is to preserve all high-level facts and append the new low-level facts.

        **My thought process:**
        1.  **Analyze `init` state:** The query specifies initial positions `(0,0)` for `a1` and `(10,10)` for `a2`. This translates to `ll_arm_at(a1,0,0)` and `ll_arm_at(a2,10,10)`. It also says "grippers are open", which translates to `ll_gripper(a1,open)` and `ll_gripper(a2,open)`.
        2.  **Analyze `goal` state:** The query says "we do not care about their state at the end" for both position and gripper. This is represented by the wildcard `_`. This translates to `ll_arm_at(a1,_,_)` and `ll_gripper(a1,_)` for each agent.
        3.  **Combine:** I will add these new `ll_` predicates to the end of the lists in the existing `init` and `goal` states.

        Here are the final, updated states:
        ```init
        init_state([
        % All original high-level predicates are preserved.
        ontable(b1),
        available(a1),
        available(a2),
        % Newly added low-level predicates for the initial state.
        ll_arm_at(a1,0,0),
        ll_arm_at(a2,10,10),
        ll_gripper(a1,open),
        ll_gripper(a2,open)
        ]).
        ```

        ```goal
        goal_state([
        % High-level goal predicates are preserved.
        on(b1,b2),
        available(a1),
        available(a2),
        % Low-level goal states are undefined, as requested.
        ll_arm_at(a1,_,_),
        ll_arm_at(a2,_,_),
        ll_gripper(a1,_),
        ll_gripper(a2,_)
        ]).
        ```
        **Reasoning for this format:** The updated states must contain both high-level and low-level information. My system first checks the high-level plan's validity and then uses the low-level facts for detailed execution. Adding `ll_arm_at(a1,0,0)` provides a concrete starting point for the first physical act
    '''
    },
    { #States conversion reasoning
    "natural_language":
    '''
        Given that the previous messages are examples, please clarify an important rule.
        **Task:** When I ask you to update the initial and final states with low-level details, should you remove any of the original high-level predicates like `ontable` or `clear`?
        Explain the rule and demonstrate with an example.
    ''',
    "prolog_code":
    '''
        That is a critical rule to understand. No, I must **never** remove or alter the existing high-level predicates when adding low-level details. The process is strictly additive.

        **The Rule:** My task is to take the entire list of high-level predicates from the original `init` and `goal` states and append the new `ll_` predicates to the end of that list.

        Let's demonstrate it with a sample.
        Original high-level `init`:
        `init_state([ontable(b1), ontable(b2), ontable(b3), at(b1,0,2), at(b2,3,1), at(b3,4,4), clear(b1), clear(b2), clear(b3), available(a1)]).`

        Low-level query mentions an arm and an open gripper. I will add `ll_arm_at(a1,_,_)` and `ll_gripper(a1,open)`.

        **Correct Updated `init` State:**
        ```init
        init_state([
        % ALL of the original predicates are copied exactly as they were.
        ontable(b1), ontable(b2), ontable(b3),
        at(b1,0,2), at(b2,3,1), at(b3,4,4),
        clear(b1), clear(b2), clear(b3),
        available(a1),
        % The new low-level predicates are simply added at the end.
        ll_arm_at(a1,_,_),
        ll_gripper(a1,open)
        ]).
        ```
        **Reasoning for this format:** The system uses both levels of information. The high-level facts (like `clear(b1)`) are needed for the abstract planner to decide *what* to do. The low-level facts (like `ll_arm_at(a1,_,_)`) are needed for the low-level planner to decide *how* to do it. Removing the high-level information would break the first stage of planning. Therefore, the updated states must be a complete union of both high-level and low-level facts.
    '''
    },
    { #ll_actions bases reasoning
    "natural_language":
    '''
        Let's define the physical robot commands.
        **Task:** Write the complete set of low-level actions for a robotic arm. The arm can move, and its gripper can be opened and closed.
        **Instruction:** Use the `ll_actions` tag. This is for the low-level actions ONLY.
    ''',
    "prolog_code":
    '''
        Understood. The low-level actions describe the concrete, physical capabilities of the hardware. These definitions are placed in the `ll_actions` block and must be completely independent of high-level concepts.

        ```ll_actions
        % The 'll_actions' tag is for concrete hardware commands.
        % All predicates here MUST begin with 'll_'.
        ll_action(move_arm_start(Agent, X1, Y1, X2, Y2),
        [ll_arm_at(Agent, X1, Y1)],
        [ll_moving_arm(Agent, _, _, _, _)], [], [ll_arm(Agent), pos(X1,Y1), pos(X2,Y2)],
        [ del(ll_arm_at(Agent, X1, Y1)), add(ll_moving_arm(Agent, X1, Y1, X2, Y2)) ]
        ).
        ll_action(move_arm_end(Agent, X1, Y1, X2, Y2),
        [ll_moving_arm(Agent, X1, Y1, X2, Y2)],
        [], [], [ll_arm(Agent)],
        [ del(ll_moving_arm(Agent, X1, Y1, X2, Y2)), add(ll_arm_at(Agent, X2, Y2)) ]
        ).

        ll_action(close_gripper_start(Agent),
        [ll_gripper(Agent, open)],
        [ll_moving_arm(Agent, _, _, _, _)], [], [ll_gripper(Agent)],
        [ del(ll_gripper(Agent, open)), add(ll_gripping(Agent)) ]
        ).
        ll_action(close_gripper_end(Agent),
        [ll_gripping(Agent)],
        [], [], [ll_gripper(Agent)],
        [ del(ll_gripping(Agent)), add(ll_gripper(Agent, closed)) ]
        ).

        ll_action(open_gripper_start(Agent),
        [ll_gripper(Agent, closed)],
        [ll_moving_arm(Agent, _, _, _, _)], [], [ll_gripper(Agent)],
        [ del(ll_gripper(Agent, closed)), add(ll_releasing(Agent)) ]
        ).
        ll_action(open_gripper_end(Agent),
        [ll_releasing(Agent)],
        [], [], [ll_gripper(Agent)],
        [ del(ll_releasing(Agent)), add(ll_gripper(Agent, open)) ]
        ).
        ```
        **Reasoning:** Using the `ll_actions` tag is mandatory because it routes these definitions to the low-level planner. This planner only understands predicates prefixed with `ll_` (e.g., `ll_arm_at`). If I used a high-level predicate like `available(Agent)` here, the low-level planner would fail because that predicate is not in its vocabulary.
    '''
    },
    { #ll_actions bases reasoning
    "natural_language":
    '''
        Given that the previous messages are examples, please perform the following specific task.
        **Task:** Define only the low-level temporal action for the physical movement of the robotic arm from one coordinate to another.
        **Instruction:** Your response must contain only the `_start` and `_end` definitions for this single action, wrapped in the `ll_actions` tag. All predicates must use the `ll_` prefix.
    ''',
    "prolog_code":
    '''
        Understood. I will define the `move_arm` action, which represents the fundamental motion capability of the robot's hardware. This definition will be placed inside the `ll_actions` tag and will strictly use `ll_` predicates.

        ```ll_actions
        % This is a concrete hardware command for moving the arm.
        ll_action(move_arm_start(Agent, X1, Y1, X2, Y2),
        % Precondition at start: The arm must be stationary at a known position.
        [ll_arm_at(Agent, X1, Y1)],
        % Negative condition: The arm cannot start moving if it's already moving.
        [ll_moving_arm(Agent, _, _, _, _)],
        [],
        [ll_arm(Agent), pos(X1,Y1), pos(X2,Y2)],
        % Effects at start: The arm is no longer at its old position; it is now in a
        % 'moving' state.
        [
            del(ll_arm_at(Agent, X1, Y1)),
            add(ll_moving_arm(Agent, X1, Y1, X2, Y2))
        ]
        ).
        ll_action(move_arm_end(Agent, X1, Y1, X2, Y2),
        % Precondition at end: The arm must have been moving due to this action.
        [ll_moving_arm(Agent, X1, Y1, X2, Y2)],
        [],
        [],
        [ll_arm(Agent)],
        % Effects at end: The 'moving' state ends, and the arm is now stationary
        % at its new position.
        [
            del(ll_moving_arm(Agent, X1, Y1, X2, Y2)),
            add(ll_arm_at(Agent, X2, Y2))
        ]
        ).
        ```
        **Reasoning for this format:** The `ll_actions` tag is mandatory to identify this as a primitive command for the low-level planner. This action only manipulates low-level state predicates like `ll_arm_at` and `ll_moving_arm`. It does not know or care about high-level concepts like what block is being carried. This strict separation of concerns is essential for the hierarchical planning system to work correctly.
    '''
    },
    { #ll_actions bases reasoning
    "natural_language":
    '''
        Given that the previous messages are examples, please perform the following specific task.
        **Task:** Define only the low-level temporal action for closing the robotic arm's gripper.
        **Instruction:** Your response must contain only the `_start` and `_end` definitions for this single action, wrapped in the `ll_actions` tag.
    ''',
    "prolog_code":
    '''
        Certainly. I will now define the `close_gripper` action. This represents the physical act of gripping and is a fundamental low-level capability. It will be correctly placed within the `ll_actions` tag.

        ```ll_actions
        % This is a concrete hardware command for closing the gripper.
        ll_action(close_gripper_start(Agent),
        % Precondition at start: The gripper must currently be open.
        [ll_gripper(Agent, open)],
        % Negative conditions: The gripper cannot close if the arm is moving,
        % or if it's already in the process of gripping or releasing.
        [ll_moving_arm(Agent, _, _, _, _), ll_gripping(Agent), ll_releasing(Agent)],
        [],
        [ll_gripper(Agent)],
        % Effects at start: The gripper is no longer open; it enters a 'gripping' state.
        [
            del(ll_gripper(Agent, open)),
            add(ll_gripping(Agent))
        ]
        ).
        ll_action(close_gripper_end(Agent),
        % Precondition at end: The gripper must be in the 'gripping' state.
        [ll_gripping(Agent)],
        [],
        [],
        [ll_gripper(Agent)],
        % Effects at end: The 'gripping' process finishes. The gripper is now in
        % a 'closed' state. We also add `ll_gripped` to show it's now holding something.
        [
            del(ll_gripping(Agent)),
            add(ll_gripper(Agent, closed)),
            add(ll_gripped(Agent))
        ]
        ).
        ```
        **Reasoning for this format:** This action must be in the `ll_actions` block as it's a direct command to the robot's end-effector. The negative condition `[ll_moving_arm(...)]` is crucial for physical realism—it prevents the robot from trying to close its gripper while the arm is still moving. The logic is purely physical and does not depend on any high-level state.
    '''
    },
    { #ll_actions bases reasoning
    "natural_language":
    '''
        Given that the previous messages are examples, please perform the following specific task.
        **Task:** Define only the low-level temporal action for opening the robotic arm's gripper to release what it's holding.
        **Instruction:** Your response must contain only the `_start` and `_end` definitions for this single action, wrapped in the `ll_actions` tag.
    ''',
    "prolog_code":
    '''
        Understood. I will define the `open_gripper` action. This is the physical command for releasing an object and is the inverse of `close_gripper`. The definition will be placed correctly inside an `ll_actions` block.

        ```ll_actions
        % This is a concrete hardware command for opening the gripper.
        ll_action(open_gripper_start(Agent),
        % Precondition at start: The gripper must be holding something.
        [ll_gripped(Agent)],
        % Negative conditions: Cannot open the gripper while the arm is moving
        % or while it's in another gripper-related state.
        [ll_moving_arm(Agent, _, _, _, _), ll_gripping(Agent), ll_releasing(Agent)],
        [],
        [ll_gripper(Agent)],
        % Effects at start: The gripper is no longer holding the object and is no
        % longer 'closed'. It enters a 'releasing' state.
        [
            del(ll_gripped(Agent)), del(ll_gripper(Agent, closed)),
            add(ll_releasing(Agent))
        ]
        ).
        ll_action(open_gripper_end(Agent),
        % Precondition at end: The gripper must be in the 'releasing' state.
        [ll_releasing(Agent)],
        [],
        [],
        [ll_gripper(Agent)],
        % Effects at end: The 'releasing' process finishes. The gripper is now fully 'open'.
        [
            del(ll_releasing(Agent)),
            add(ll_gripper(Agent, open))
        ]
        ).
        ```
        **Reasoning for this format:** The `ll_actions` tag correctly identifies this as a primitive hardware command. This action manipulates the physical state of the gripper, changing it from `closed` and `gripped` to `open`. It is a fundamental operation for the low-level planner. The logic is self-contained and does not depend on which block was released, only on the state of the gripper itself.
    '''
    },
    { #ll_actions bases reasoning
    "natural_language":
    '''
        Given that the previous messages are examples, please clarify the purpose of low-level actions.
        **Task:** The high-level scenario involves stacking blocks b1 and b2. Do I need to create specific low-level actions like `ll_stack_b1_on_b2_start`?
        Explain your reasoning and provide the correct, generic set of low-level actions.
    ''',
    "prolog_code":
    '''
        That is a crucial point. The low-level actions do **not** describe how to solve the specific task (e.g., stacking b1 on b2). Instead, they define the **fundamental, reusable physical capabilities** of the robot's hardware. The planner will *combine* these primitive actions to achieve the high-level goal.

        Therefore, I must provide the standard, generic library of physical commands, not task-specific ones.

        ```ll_actions
        % The 'll_actions' tag is for the robot's universal command set (its API).
        % These actions are independent of any specific blocks-world goal.

        % Defines the physical capability of moving the arm.
        ll_action(move_arm_start(Agent, X1, Y1, X2, Y2),
        [ll_arm_at(Agent, X1, Y1)],
        [ll_moving_arm(Agent, _, _, _, _)], [], [ll_arm(Agent), pos(X1,Y1), pos(X2,Y2)],
        [ del(ll_arm_at(Agent, X1, Y1)), add(ll_moving_arm(Agent, X1, Y1, X2, Y2)) ]
        ).
        ll_action(move_arm_end(Agent, X1, Y1, X2, Y2),
        [ll_moving_arm(Agent, X1, Y1, X2, Y2)],
        [], [], [ll_arm(Agent)],
        [ del(ll_moving_arm(Agent, X1, Y1, X2, Y2)), add(ll_arm_at(Agent, X2, Y2)) ]
        ).

        % Defines the physical capability of closing the gripper.
        ll_action(close_gripper_start(Agent),
        [ll_gripper(Agent, open)],
        [ll_moving_arm(Agent, _, _, _, _)], [], [ll_gripper(Agent)],
        [ del(ll_gripper(Agent, open)), add(ll_gripping(Agent)) ]
        ).
        ll_action(close_gripper_end(Agent),
        [ll_gripping(Agent)],
        [], [], [ll_gripper(Agent)],
        [ del(ll_gripping(Agent)), add(ll_gripper(Agent, closed)), add(ll_gripped(Agent)) ]
        ).

        % Defines the physical capability of opening the gripper.
        ll_action(open_gripper_start(Agent),
        [ll_gripped(Agent)],
        [ll_moving_arm(Agent, _, _, _, _)], [], [ll_gripper(Agent)],
        [ del(ll_gripped(Agent)), del(ll_gripper(Agent, closed)), add(ll_releasing(Agent)) ]
        ).
        ll_action(open_gripper_end(Agent),
        [ll_releasing(Agent)],
        [], [], [ll_gripper(Agent)],
        [ del(ll_releasing(Agent)), add(ll_gripper(Agent, open)) ]
        ).
        ```
        **Reasoning for this format:** The system uses a hierarchical approach. The high-level plan says *what* to do (e.g., "stack block A on B"). The `mappings` then translate this into a sequence of these generic low-level commands. The low-level planner's job is to schedule these primitive hardware commands. If I were to create task-specific actions, the entire system would lose its flexibility and reusability. The `ll_actions` block must always contain the complete, generic hardware API.
    '''
    },
    { #ll_actions bases reasoning
    "natural_language":
    '''
        Given that the previous messages are examples, explain a critical rule for writing low-level actions.
        **Task:** Why would it be an error to include the precondition `available(Agent)` in the `ll_move_arm_start` action? Explain the separation of concerns.
    ''',
    "prolog_code":
    '''
        That is the most important rule for hierarchical planning. The low-level and high-level planners operate in different "worlds" with different vocabularies.

        **The Error:** Including `available(Agent)` in a low-level action is wrong because `available` is an abstract, high-level concept. The low-level planner, which executes `ll_actions`, does not know what `available` means. Its vocabulary is limited to physical states, which are all prefixed with `ll_`.

        Let's analyze the correct `ll_move_arm_start` action:
        ```ll_actions
        ll_action(move_arm_start(Agent, X1, Y1, X2, Y2),
        % The PRECONDITION is PHYSICAL: Is the arm currently stationary at a location?
        % It uses `ll_arm_at`, NOT `available`.
        [ll_arm_at(Agent, X1, Y1)],
        [ll_moving_arm(Agent, _, _, _, _)], [], [ll_arm(Agent), pos(X1,Y1), pos(X2,Y2)],
        % The EFFECT is PHYSICAL: The arm is no longer stationary; it is moving.
        [ del(ll_arm_at(Agent, X1, Y1)), add(ll_moving_arm(Agent, X1, Y1, X2, Y2)) ]
        ).
        ```

        **Reasoning for this format:** The system is designed with a clear separation:
        1.  **High-Level Planner:** Thinks about abstract states like `available`. It uses `actions`.
        2.  **Low-Level Planner:** Thinks about physical states like `ll_arm_at`. It uses `ll_actions`.

        The `mappings` connect these two worlds. The high-level planner ensures an agent is `available` before starting a high-level action. The mapping then translates this into a sequence of `ll_actions`. The `ll_actions` themselves only need to check the physical preconditions, because the high-level logic has already been verified. Mixing predicates would break this clean separation and cause the low-level planner to fail.
    '''
    },
    { #ll_actions bases reasoning
    "natural_language":
    '''
        Given that the previous messages are examples, let's analyze the preconditions of a physical action.
        **Task:** When defining `close_gripper_start`, why is it important to have the negative condition `[ll_moving_arm(Agent, _, _, _, _)]`?
        Provide only the code for the `close_gripper` action and explain this specific detail.
    ''',
    "prolog_code":
    '''
        This detail is about modeling realistic physical constraints. A real robot cannot (or should not) close its gripper while its arm is in the middle of a fast movement. The negative condition enforces this physical limitation.

        Here is the robust `close_gripper` definition:
        ```ll_actions
        ll_action(close_gripper_start(Agent),
        % Positive Precondition: The gripper must be open to be closed.
        [ll_gripper(Agent, open)],
        % Negative Precondition: The arm must NOT be moving. This prevents the planner
        % from scheduling a 'grip' and 'move' at the same time, which is physically unsafe.
        [ll_moving_arm(Agent, _, _, _, _)],
        [],
        [ll_gripper(Agent)],
        % Effect: The gripper begins the process of closing.
        [ del(ll_gripper(Agent, open)), add(ll_gripping(Agent)) ]
        ).
        ll_action(close_gripper_end(Agent),
        [ll_gripping(Agent)],
        [], [], [ll_gripper(Agent)],
        [ del(ll_gripping(Agent)), add(ll_gripper(Agent, closed)), add(ll_gripped(Agent)) ]
        ).
        ```
        **Reasoning for this format:** The `ll_actions` must accurately model the robot's hardware limitations. The negative condition `[ll_moving_arm(...)]` is a mutex (mutual exclusion) constraint. It tells the planner: "The resource 'Agent' cannot be used for a 'move_arm' action and a 'close_gripper' action simultaneously." This prevents the planner from generating physically impossible or unsafe plans. It's a key part of creating a reliable low-level domain.
    '''
    },
    { #Mappings bases reasoning
    "natural_language":
    '''
        We must now connect the abstract plan to the concrete actions.
        **Task:** Provide the mappings for all high-level actions (`move_table_to_table`, `move_table_to_block`, `move_onblock_to_table` and `move_onblock_to_block`) to their corresponding low-level sequences.
        **Instruction:** Use the `mappings` tag for this final block.
    ''',
    "prolog_code":
    '''
        Certainly. This final step is to create the bridge between the high-level "what" and the low-level "how". The `mappings` block defines this translation.
        Note that just the '_start' mapping is needed because the '_end' action is just for the high_level.

        ```mappings
        % The 'mappings' tag translates one high-level action into a sequence of low-level ones.
        mapping(move_table_to_table_start(Agent, _Block, X1, Y1, X2, Y2),
        [
            move_arm_start(Agent, _, _, X1, Y1),
            move_arm_end(Agent, _, _, X1, Y1),
            close_gripper_start(Agent),
            close_gripper_end(Agent),
            move_arm_start(Agent, X1, Y1, X2, Y2),
            move_arm_end(Agent, X1, Y1, X2, Y2),
            open_gripper_start(Agent),
            open_gripper_end(Agent)
        ]
        ).

        mapping(move_table_to_block_start(Agent, _Block1, _Block2, X1, Y1, X2, Y2),
        [
            move_arm_start(Agent, _, _, X1, Y1),
            move_arm_end(Agent, _, _, X1, Y1),
            close_gripper_start(Agent),
            close_gripper_end(Agent),
            move_arm_start(Agent, X1, Y1, X2, Y2),
            move_arm_end(Agent, X1, Y1, X2, Y2),
            open_gripper_start(Agent),
            open_gripper_end(Agent)
        ]
        ).

        mapping(move_onblock_to_table_start(Agent, _Block1, X1, Y1, X2, Y2),
        [
            move_arm_start(Agent, _, _, X1, Y1),
            move_arm_end(Agent, _, _, X1, Y1),
            close_gripper_start(Agent),
            close_gripper_end(Agent),
            move_arm_start(Agent, X1, Y1, X2, Y2),
            move_arm_end(Agent, X1, Y1, X2, Y2),
            open_gripper_start(Agent),
            open_gripper_end(Agent)
        ]
        ).

        mapping(move_onblock_to_block_start(Agent, _Block1, _Block2, X1, Y1, X2, Y2),
        [
            move_arm_start(Agent, _, _, X1, Y1),
            move_arm_end(Agent, _, _, X1, Y1),
            close_gripper_start(Agent),
            close_gripper_end(Agent),
            move_arm_start(Agent, X1, Y1, X2, Y2),
            move_arm_end(Agent, X1, Y1, X2, Y2),
            open_gripper_start(Agent),
            open_gripper_end(Agent)
        ]
        ).
        ```
        **Reasoning:** The `mappings` tag is essential for the plan refinement process. After a high-level plan is found (e.g., `move_table_to_table_start(a1, b1, ...)`), the system looks inside this block for the corresponding `mapping`. It then replaces the single high-level action with the entire sequence of low-level actions listed here, creating an executable plan. Without this tag, the system would be unable to translate its abstract strategy into concrete robot commands.
    '''
    },
    { #Mappings bases reasoning
    "natural_language":
    '''
        Given that the previous messages are examples, please perform the following specific task.
        **Task:** Provide only the mapping for the high-level action `move_table_to_table_start`.
        **Instruction:** Your response must contain only the single `mapping` definition for this action, wrapped in the `mappings` tag.
    ''',
    "prolog_code":
    '''
        Understood. I will define the mapping that decomposes the abstract `move_table_to_table_start` action into a concrete sequence of low-level commands. This will be enclosed in the `mappings` tag.

        ```mappings
        % This mapping translates the high-level action of moving a block on the table.
        mapping(move_table_to_table_start(Agent, _Block, X1, Y1, X2, Y2),
        [
            % Step 1: Move the arm to the block's initial position.
            move_arm_start(Agent, _, _, X1, Y1),
            move_arm_end(Agent, _, _, X1, Y1),

            % Step 2: Grip the block.
            close_gripper_start(Agent),
            close_gripper_end(Agent),

            % Step 3: Move the arm (now holding the block) to the destination.
            move_arm_start(Agent, X1, Y1, X2, Y2),
            move_arm_end(Agent, X1, Y1, X2, Y2),

            % Step 4: Release the block at the new position.
            open_gripper_start(Agent),
            open_gripper_end(Agent)
        ]
        ).
        ```
        **Reasoning for this format:** The `mappings` tag is essential for the plan refinement stage. When the high-level planner decides to execute `move_table_to_table_start`, the system uses this rule to replace it with the eight specified low-level actions. This sequence—move, grip, move, release—is the fundamental physical process required to relocate an object.
    '''
    },
    { #Mappings bases reasoning
    "natural_language":
    '''
        Given that the previous messages are examples, please perform the following specific task.
        **Task:** Provide only the mapping for the high-level action `move_table_to_block_start`.
        **Instruction:** Your response must contain only the single `mapping` definition for this action, wrapped in the `mappings` tag.
    ''',
    "prolog_code":
    '''
        Of course. I will define the mapping for the `move_table_to_block_start` action. The physical sequence of low-level commands is identical to moving a block on the table, but this mapping links it to the specific abstract goal of stacking.

        ```mappings
        % This mapping translates the high-level action of stacking a block from the table.
        mapping(move_table_to_block_start(Agent, _Block1, _Block2, X1, Y1, X2, Y2),
        [
            % Step 1: Move the arm to the initial block on the table.
            move_arm_start(Agent, _, _, X1, Y1),
            move_arm_end(Agent, _, _, X1, Y1),

            % Step 2: Grip the block.
            close_gripper_start(Agent),
            close_gripper_end(Agent),

            % Step 3: Move the arm to the position of the destination block.
            move_arm_start(Agent, X1, Y1, X2, Y2),
            move_arm_end(Agent, X1, Y1, X2, Y2),

            % Step 4: Release the block to stack it.
            open_gripper_start(Agent),
            open_gripper_end(Agent)
        ]
        ).
        ```
        **Reasoning for this format:** The `mappings` tag is used here to connect the high-level concept of "stacking" to a concrete physical procedure. Although the low-level sequence is the same as `move_table_to_table`, having a distinct mapping for `move_table_to_block_start` is crucial. It allows the high-level planner to reason about the logical effects of stacking (like creating an `on/2` relationship) while ensuring the correct physical procedure is executed by the low-level planner.
    '''
    },  
    { #Mappings bases reasoning
    "natural_language":
    '''
        Given that the previous messages are examples, please perform the following specific task.
        **Task:** Provide only the mapping for the high-level action `move_onblock_to_table_start`.
        **Instruction:** Your response must contain only the single `mapping` definition for this action, wrapped in the `mappings` tag.
    ''',
    "prolog_code":
    '''
        Understood. I will provide the mapping for the `move_onblock_to_table_start` action, which defines how to disassemble a stack. This will be enclosed in the `mappings` tag.

        ```mappings
        % This mapping translates the high-level action of un-stacking a block to the table.
        mapping(move_onblock_to_table_start(Agent, _Block1, _Block2, X1, Y1, X2, Y2),
        [
            % Step 1: Move the arm to the block on top of the stack.
            move_arm_start(Agent, _, _, X1, Y1),
            move_arm_end(Agent, _, _, X1, Y1),

            % Step 2: Grip the top block.
            close_gripper_start(Agent),
            close_gripper_end(Agent),

            % Step 3: Move the arm to the empty destination on the table.
            move_arm_start(Agent, X1, Y1, X2, Y2),
            move_arm_end(Agent, X1, Y1, X2, Y2),

            % Step 4: Release the block onto the table.
            open_gripper_start(Agent),
            open_gripper_end(Agent)
        ]
        ).
        ```
        **Reasoning for this format:** This rule, defined within the `mappings` tag, is the bridge for the "unstacking" operation. It tells the system that the abstract goal of clearing a block and moving it to the table is achieved by the same physical sequence: move to the object, grip it, move to the destination, and release it. This allows the system to plan complex manipulations that require both building and dismantling towers.
    '''
    },
    { #Mappings bases reasoning 
    "natural_language":
    '''
        Given that the previous messages are examples, please perform the following specific task.
        **Task:** Provide only the mapping for the high-level action `move_onblock_to_block_start`.
        **Instruction:** Your response must contain only the single `mapping` definition for this action, wrapped in the `mappings` tag.
    ''',
    "prolog_code":
    '''
        Certainly. I will define the mapping for `move_onblock_to_block_start`. This translates the most complex high-level maneuver—moving a block from one stack to another—into its physical, low-level equivalent.

        ```mappings
        % This mapping translates moving a block from one stack to another.
        mapping(move_onblock_to_block_start(Agent, _Block1, _Block2, _Block3, X1, Y1, X3, Y3),
        [
            % Step 1: Move the arm to the top of the source stack.
            move_arm_start(Agent, _, _, X1, Y1),
            move_arm_end(Agent, _, _, X1, Y1),

            % Step 2: Grip the block.
            close_gripper_start(Agent),
            close_gripper_end(Agent),

            % Step 3: Move the arm to the top of the destination stack.
            move_arm_start(Agent, X1, Y1, X3, Y3),
            move_arm_end(Agent, X1, Y1, X3, Y3),

            % Step 4: Release the block.
            open_gripper_start(Agent),
            open_gripper_end(Agent)
        ]
        ).
        ```
        **Reasoning for this format:** The `mappings` tag is required to define this translation rule. This specific mapping shows that even the most complex abstract action at the high level corresponds to the same fundamental physical sequence at the low level. The abstraction at the high level is about the logical change in state (which blocks are `on` which, and which are `clear`), while the mapping grounds this logic in the simple, repeatable `move-grip-move-release` procedure.
    '''
    },
    { #Mappings bases reasoning 
    "natural_language":
    '''
        Given that the previous messages are examples, let's analyze the core mapping logic.
        **Task:** You are given four different high-level actions: `move_table_to_table_start`, `move_table_to_block_start`, `move_onblock_to_table_start`, and `move_onblock_to_block_start`. You are also given the standard low-level actions (`move_arm`, `close_gripper`, `open_gripper`).
        Provide the mappings for all of them and explain why the low-level sequence is the same in every case.
    ''',
    "prolog_code":
    '''
        Of course. This reveals a fundamental principle of hierarchical planning. While the high-level actions have different logical meanings (rearranging vs. stacking vs. unstacking), the underlying physical process performed by the robot is identical.

        **The Universal Physical Sequence:** To move any block from a start point to an end point, a simple arm must:
        1.  Move its empty gripper to the block's location.
        2.  Grip the block.
        3.  Move its gripper (now holding the block) to the destination.
        4.  Release the block.

        This sequence translates directly into the following low-level plan. I will apply this same plan to all four high-level actions.

        ```mappings
        % The 'mappings' tag translates the "what" (high-level) into the "how" (low-level).
        % Note that the physical sequence is identical for all move operations.

        mapping(move_table_to_table_start(Agent, _Block, X1, Y1, X2, Y2),
        [
            move_arm_start(Agent, _, _, X1, Y1), move_arm_end(Agent, _, _, X1, Y1),
            close_gripper_start(Agent), close_gripper_end(Agent),
            move_arm_start(Agent, X1, Y1, X2, Y2), move_arm_end(Agent, X1, Y1, X2, Y2),
            open_gripper_start(Agent), open_gripper_end(Agent)
        ]
        ).

        mapping(move_table_to_block_start(Agent, _Block1, _Block2, X1, Y1, X2, Y2),
        [
            move_arm_start(Agent, _, _, X1, Y1), move_arm_end(Agent, _, _, X1, Y1),
            close_gripper_start(Agent), close_gripper_end(Agent),
            move_arm_start(Agent, X1, Y1, X2, Y2), move_arm_end(Agent, X1, Y1, X2, Y2),
            open_gripper_start(Agent), open_gripper_end(Agent)
        ]
        ).

        mapping(move_onblock_to_table_start(Agent, _Block1, _Block2, X1, Y1, X2, Y2),
        [
            move_arm_start(Agent, _, _, X1, Y1), move_arm_end(Agent, _, _, X1, Y1),
            close_gripper_start(Agent), close_gripper_end(Agent),
            move_arm_start(Agent, X1, Y1, X2, Y2), move_arm_end(Agent, X1, Y1, X2, Y2),
            open_gripper_start(Agent), open_gripper_end(Agent)
        ]
        ).

        mapping(move_onblock_to_block_start(Agent, _Block1, _Block2, _Block3, X1, Y1, X3, Y3),
        [
            move_arm_start(Agent, _, _, X1, Y1), move_arm_end(Agent, _, _, X1, Y1),
            close_gripper_start(Agent), close_gripper_end(Agent),
            move_arm_start(Agent, X1, Y1, X3, Y3), move_arm_end(Agent, X1, Y1, X3, Y3),
            open_gripper_start(Agent), open_gripper_end(Agent)
        ]
        ).
        ```
        **Reasoning for this format:** The power of this system comes from abstraction. The `mappings` block is the dictionary that translates abstract intentions into this one, universal, reusable physical procedure. This is why the output is always the same sequence. My task is to recognize the high-level action as a "move" command and apply this standard low-level template.
    '''
    },
    { #Mappings bases reasoning 
    "natural_language":
    '''
        Given that the previous messages are examples, please answer a critical question about mappings.
        **Task:** You are given the high-level action `move_table_to_table_end`. Should you provide a mapping for it? Explain why or why not.
    ''',
    "prolog_code":
    '''
        That is a fundamental rule of this hierarchical planning system.
        No, I must not provide a mapping for any high-level `_end` action.

        **Reasoning for this format:** The mapping process works as follows:
        1.  The system finds a high-level plan, which is a sequence of `_start` and `_end` actions.
        2.  It iterates through this plan. When it encounters a `_start` action (e.g., `move_table_to_table_start`), it looks in the `mappings` block and **replaces** that single action with its entire low-level sequence.
        3.  When it encounters an `_end` action (e.g., `move_table_to_table_end`), its only purpose is to apply the final high-level logical effects (like making a block `ontable` again). It does not trigger any new physical actions. The physical work is already complete by the end of the low-level sequence that was initiated by the corresponding `_start` action.

        Therefore, the correct response is to state that no mapping exists. For example:
        "This _end action does not produce a mapping to low-level actions; its function is solely to reflect changes in the high-level state."
    '''
    },
    {
    "natural_language":
    '''
        Given that the previous messages are examples, let's analyze how information flows through a mapping.
        **Task:** In the mapping for `move_table_to_table_start(Agent, _Block, X1, Y1, X2, Y2)`, explain how the variables `Agent`, `X1`, `Y1`, `X2`, `Y2` are used in the low-level sequence. Also, explain why `_Block` has an underscore.
    ''',
    "prolog_code":
    '''
        Excellent question. This shows how the abstract plan is made concrete. The variables in the high-level action's head serve as inputs to the low-level sequence.

        Let's look at the mapping:
        ```mappings
        mapping(move_table_to_table_start(Agent, _Block, X1, Y1, X2, Y2),
        [
            % `Agent`, `X1`, `Y1` are passed here to move to the block. `_` means "start from anywhere".
            move_arm_start(Agent, _, _, X1, Y1),
            move_arm_end(Agent, _, _, X1, Y1),

            % `Agent` is passed here. The physical grip does not need to know which block.
            close_gripper_start(Agent),
            close_gripper_end(Agent),

            % All variables are used here to move the block from the start to the end position.
            move_arm_start(Agent, X1, Y1, X2, Y2),
            move_arm_end(Agent, X1, Y1, X2, Y2),

            % `Agent` is passed here.
            open_gripper_start(Agent),
            open_gripper_end(Agent)
        ]
        ).
        ```

        **Explanation of Variables:**
        *   **`Agent`:** This is passed to every single low-level action because the same agent is performing the entire sequence.
        *   **`X1, Y1`:** These are the coordinates of the block's starting position. They are used as the destination for the *first* `move_arm` and the starting point for the *second* `move_arm`.
        *   **`X2, Y2`:** These are the coordinates of the block's final position. They are used as the destination for the *second* `move_arm`.
        *   **`_Block`:** The underscore `_` marks this variable as "unimportant" for the mapping's logic. While the high-level planner needs to know which block is moving (e.g., `b1`), the low-level physical sequence is the same regardless of whether it's moving `b1` or `b2`. The low-level actions operate on the arm and gripper, not on the block's identity.

        **Reasoning for this format:** The mapping is a template. It shows how to translate the *parameters* of an abstract command into the *parameters* of a sequence of concrete commands. This ensures that when the planner decides to `move_table_to_table_start(a1, b1, 1, 1, 5, 5)`, the system correctly executes `move_arm_start(a1, _, _, 1, 1)`, followed by `move_arm_start(a1, 1, 1, 5, 5)`, etc.
    '''
    },
    {
        "natural_language":
        '''
        **Task:** Generate the static knowledge base for a world with two blocks named `boxX` and `boxY`, two agents named `bot1` and `bot2`, and two positions, (3,4) and (4,3).
        ''',
        "prolog_code":
        '''
        ```kb
        % Positions
        pos(3,4).
        pos(4,3).

        % Blocks
        block(boxX).
        block(boxY).

        % Agents
        agent(bot1).
        agent(bot2).

        % Resources
        resources(agent(_)).
        ```
        '''
    },
    {
        "natural_language":
        '''
        **Task:** Generate the initial and goal states for this task.
        A single block `cube1` is at (0,0). The goal is to move it to (10,10). Agent `arm1` is available.
        ''',
        "prolog_code":
        '''
        ```init
        init_state([
          ontable(cube1),
          at(cube1,0,0),
          clear(cube1),
          available(arm1)
        ]).
        ```

        ```goal
        goal_state([
          ontable(cube1),
          at(cube1,10,10),
          clear(cube1),
          available(arm1)
        ]).
        ```
        '''
    },
    {
        "natural_language":
        '''
        **Task:** Update this high-level KB. The low-level description for agent `robotA` is that "it is equipped with a gripper".

        High-level KB for reference:
        ```kb
        % Agents
        agent(robotA).
        % Resources
        resources(agent(_)).
        ```
        ''',
        "prolog_code":
        '''
        ```kb
        % Agents
        agent(robotA).

        % Low-level physical entities
        ll_gripper(robotA).

        % Resources for both levels
        resources(agent(_)).
        resources(ll_gripper(_)).
        ```
        '''
    },
    {
        "natural_language":
        '''
        **Task:** Generate the initial and goal states for a basic unstacking task.
        Block `b2` is on top of block `b1` at position (5,5). The goal is to place `b2` on the table at position (0,5). Agent `a1` is available.
        ''',
        "prolog_code":
        '''
        ```init
        init_state([
          ontable(b1),
          on(b2,b1),
          at(b1,5,5),
          at(b2,5,5),
          clear(b2),
          available(a1)
        ]).
        ```

        ```goal
        goal_state([
          ontable(b1),
          ontable(b2),
          at(b1,5,5),
          at(b2,0,5),
          clear(b1),
          clear(b2),
          available(a1)
        ]).
        ```
        '''
    },
    {
        "natural_language":
        '''
        **Task:** Update this `init` and `goal` state. The low-level query states: "The arm `armX`'s initial position is unknown. Its gripper is closed at the start. The final state is irrelevant."

        High-level init for reference:
        `init_state([ontable(bA), available(armX)]).`
        High-level goal for reference:
        `goal_state([ontable(bB), available(armX)]).`
        ''',
        "prolog_code":
        '''
        ```init
        init_state([
          ontable(bA),
          available(armX),
          ll_arm_at(armX,_,_),
          ll_gripper(armX,closed)
        ]).
        ```

        ```goal
        goal_state([
          ontable(bB),
          available(armX),
          ll_arm_at(armX,_,_),
          ll_gripper(armX,_)
        ]).
        ```
        '''
    },
    {
        "natural_language":
        '''
        **Task:** Provide the complete set of high-level actions for a standard blocks-world domain.
        ''',
        "prolog_code":
        '''
        ```actions
        % Move a block from a position on the table to another position on the table
        action(move_table_to_table_start(Agent, Block, X1, Y1, X2, Y2),
          [ontable(Block), at(Block, X1, Y1), available(Agent), clear(Block)],
          [at(_, X2, Y2)], [], [agent(Agent), pos(X1, Y1), pos(X2, Y2), block(Block)],
          [ del(available(Agent)), del(clear(Block)), del(ontable(Block)), del(at(Block, X1, Y1)),
            add(moving_table_to_table(Agent, Block, X1, Y1, X2, Y2)) ]
        ).
        action(move_table_to_table_end(Agent, Block, X1, Y1, X2, Y2),
          [moving_table_to_table(Agent, Block, X1, Y1, X2, Y2)],
          [], [], [agent(Agent)],
          [ del(moving_table_to_table(Agent, Block, X1, Y1, X2, Y2)),
            add(ontable(Block)), add(at(Block, X2, Y2)), add(clear(Block)), add(available(Agent)) ]
        ).

        % Move a block from a position on the table to the top of another block
        action(move_table_to_block_start(Agent, Block1, Block2, X1, Y1, X2, Y2),
          [available(Agent), ontable(Block1), at(Block1, X1, Y1), at(Block2, X2, Y2), clear(Block2), clear(Block1)],
          [], [], [agent(Agent), pos(X1, Y1), pos(X2, Y2), block(Block1), block(Block2), Block1 \\= Block2],
          [ del(available(Agent)), del(clear(Block1)), del(ontable(Block1)), del(at(Block1, X1, Y1)), del(clear(Block2)),
            add(moving_table_to_block(Agent, Block1, Block2, X1, Y1, X2, Y2)) ]
        ).
        action(move_table_to_block_end(Agent, Block1, Block2, X1, Y1, X2, Y2),
          [moving_table_to_block(Agent, Block1, Block2, X1, Y1, X2, Y2)],
          [], [], [agent(Agent)],
          [ del(moving_table_to_block(Agent, Block1, Block2, X1, Y1, X2, Y2)),
            add(on(Block1, Block2)), add(at(Block1, X2, Y2)), add(clear(Block1)), add(available(Agent)) ]
        ).

        % Move a block from the top of another block to a position on the table
        action(move_onblock_to_table_start(Agent, Block1, Block2, X2, Y2),
          [available(Agent), on(Block1, Block2), at(Block1, X1, Y1), clear(Block1)],
          [at(_, X2, Y2)], [], [agent(Agent), block(Block1), block(Block2), pos(X1,Y1), pos(X2,Y2), Block1 \\= Block2],
          [ del(available(Agent)), del(clear(Block1)), del(on(Block1, Block2)), del(at(Block1, X1, Y1)), add(clear(Block2)),
            add(moving_onblock_to_table(Agent, Block1, X2, Y2)) ]
        ).
        action(move_onblock_to_table_end(Agent, Block1, X2, Y2),
          [moving_onblock_to_table(Agent, Block1, X2, Y2)],
          [], [], [agent(Agent)],
          [ del(moving_onblock_to_table(Agent, Block1, X2, Y2)),
            add(ontable(Block1)), add(at(Block1, X2, Y2)), add(clear(Block1)), add(available(Agent)) ]
        ).

        % Move a block from the top of another block to the top of another block
        action(move_onblock_to_block_start(Agent, Block1, Block2, Block3, X1, Y1, X3, Y3),
          [available(Agent), on(Block1, Block2), at(Block1, X1, Y1), at(Block3, X3, Y3), clear(Block3), clear(Block1)],
          [], [], [agent(Agent), block(Block1), block(Block2), block(Block3), pos(X1,Y1), pos(X3,Y3), Block1 \\= Block2, Block1 \\= Block3, Block2 \\= Block3],
          [ del(available(Agent)), del(clear(Block1)), del(on(Block1, Block2)), del(at(Block1, X1, Y1)), add(clear(Block2)), del(clear(Block3)),
            add(moving_onblock_to_block(Agent, Block1, Block3, X3, Y3)) ]
        ).
        action(move_onblock_to_block_end(Agent, Block1, Block3, X3, Y3),
          [moving_onblock_to_block(Agent, Block1, Block3, X3, Y3)],
          [], [], [agent(Agent)],
          [ del(moving_onblock_to_block(Agent, Block1, Block3, X3, Y3)),
            add(on(Block1, Block3)), add(at(Block1, X3, Y3)), add(clear(Block1)), add(available(Agent)) ]
        ).
        ```
        '''
    },
    {
        "natural_language":
        '''
        **Task:** What is the low-level mapping for the high-level action `move_onblock_to_block_start`?
        ''',
        "prolog_code":
        '''
        ```mappings
        mapping(move_onblock_to_block_start(Agent, _Block1, _Block2, _Block3, X1, Y1, X3, Y3),
          [
            move_arm_start(Agent, _, _, X1, Y1),
            move_arm_end(Agent, _, _, X1, Y1),
            close_gripper_start(Agent),
            close_gripper_end(Agent),
            move_arm_start(Agent, X1, Y1, X3, Y3),
            move_arm_end(Agent, X1, Y1, X3, Y3),
            open_gripper_start(Agent),
            open_gripper_end(Agent)
          ]
        ).
        ```
        '''
    }
]

reasoning_examples_evalutation_data_samples = [
    {
        "natural_language":
        '''
        **Task:** Generate the static high-level knowledge base for a simple scenario involving two blocks, `blockA` and `blockB`, and two agents, `agentX` and `agentY`. The blocks are initially at positions (10,5) and (5,10) and the goal is to swap their positions.
        ''',
        "prolog_code":
        '''
        ```kb
        % Positions
        pos(10,5).
        pos(5,10).

        % Blocks
        block(blockA).
        block(blockB).

        % Agents
        agent(agentX).
        agent(agentY).

        % Resources
        resources(agent(_)).
        ```
        '''
    },
    {
        "natural_language":
        '''
        **Task:** Generate the initial and goal states for the following scenario.
        We start with three blocks. `b1` is on the table at (1,1). `b2` is on `b1`. `b3` is on the table at (3,3). A single agent, `a1`, is available. The goal is to create a single tall tower at position (3,3) with `b3` at the bottom, `b1` in the middle, and `b2` on top.
        ''',
        "prolog_code":
        '''
        ```init
        init_state([
          ontable(b1), ontable(b3),
          on(b2,b1),
          at(b1,1,1), at(b2,1,1), at(b3,3,3),
          clear(b2), clear(b3),
          available(a1)
        ]).
        ```

        ```goal
        goal_state([
          ontable(b3),
          on(b1,b3), on(b2,b1),
          at(b3,3,3), at(b1,3,3), at(b2,3,3),
          clear(b2),
          available(a1)
        ]).
        ```
        '''
    },
    {
        "natural_language":
        '''
        **Task:** You are given a high-level KB with agents `a1`, `a2`, `a3`. The low-level description mentions that "all 3 agents are equipped with robotic arms and grippers."
        Update the provided high-level KB to include these low-level details.

        High-level KB for reference:
        ```kb
        % Agents
        agent(a1). agent(a2). agent(a3).
        % Resources
        resources(agent(_)).
        ```
        ''',
        "prolog_code":
        '''
        ```kb
        % Agents
        agent(a1).
        agent(a2).
        agent(a3).

        % Low-level physical entities
        ll_arm(a1).
        ll_arm(a2).
        ll_arm(a3).
        ll_gripper(a1).
        ll_gripper(a2).
        ll_gripper(a3).

        % Resources for both levels
        resources(agent(_)).
        resources(ll_arm(_)).
        resources(ll_gripper(_)).
        ```
        '''
    },
    {
        "natural_language":
        '''
        **Task:** Based on the scenario description, provide the complete library of high-level actions an agent can perform in a blocks-world. The scenario involves stacking, unstacking, and moving blocks on a table.
        ''',
        "prolog_code":
        '''
        ```actions
        % Move a block from a position on the table to another position on the table
        action(move_table_to_table_start(Agent, Block, X1, Y1, X2, Y2),
          [ontable(Block), at(Block, X1, Y1), available(Agent), clear(Block)],
          [at(_, X2, Y2)], [], [agent(Agent), pos(X1, Y1), pos(X2, Y2), block(Block)],
          [ del(available(Agent)), del(clear(Block)), del(ontable(Block)), del(at(Block, X1, Y1)),
            add(moving_table_to_table(Agent, Block, X1, Y1, X2, Y2)) ]
        ).
        action(move_table_to_table_end(Agent, Block, X1, Y1, X2, Y2),
          [moving_table_to_table(Agent, Block, X1, Y1, X2, Y2)],
          [], [], [agent(Agent)],
          [ del(moving_table_to_table(Agent, Block, X1, Y1, X2, Y2)),
            add(ontable(Block)), add(at(Block, X2, Y2)), add(clear(Block)), add(available(Agent)) ]
        ).

        % Move a block from a position on the table to the top of another block
        action(move_table_to_block_start(Agent, Block1, Block2, X1, Y1, X2, Y2),
          [available(Agent), ontable(Block1), at(Block1, X1, Y1), at(Block2, X2, Y2), clear(Block2), clear(Block1)],
          [], [], [agent(Agent), pos(X1, Y1), pos(X2, Y2), block(Block1), block(Block2), Block1 \\= Block2],
          [ del(available(Agent)), del(clear(Block1)), del(ontable(Block1)), del(at(Block1, X1, Y1)), del(clear(Block2)),
            add(moving_table_to_block(Agent, Block1, Block2, X1, Y1, X2, Y2)) ]
        ).
        action(move_table_to_block_end(Agent, Block1, Block2, X1, Y1, X2, Y2),
          [moving_table_to_block(Agent, Block1, Block2, X1, Y1, X2, Y2)],
          [], [], [agent(Agent)],
          [ del(moving_table_to_block(Agent, Block1, Block2, X1, Y1, X2, Y2)),
            add(on(Block1, Block2)), add(at(Block1, X2, Y2)), add(clear(Block1)), add(available(Agent)) ]
        ).

        % Move a block from the top of another block to a position on the table
        action(move_onblock_to_table_start(Agent, Block1, Block2, X2, Y2),
          [available(Agent), on(Block1, Block2), at(Block1, X1, Y1), clear(Block1)],
          [at(_, X2, Y2)], [], [agent(Agent), block(Block1), block(Block2), pos(X1,Y1), pos(X2,Y2), Block1 \\= Block2],
          [ del(available(Agent)), del(clear(Block1)), del(on(Block1, Block2)), del(at(Block1, X1, Y1)), add(clear(Block2)),
            add(moving_onblock_to_table(Agent, Block1, X2, Y2)) ]
        ).
        action(move_onblock_to_table_end(Agent, Block1, X2, Y2),
          [moving_onblock_to_table(Agent, Block1, X2, Y2)],
          [], [], [agent(Agent)],
          [ del(moving_onblock_to_table(Agent, Block1, X2, Y2)),
            add(ontable(Block1)), add(at(Block1, X2, Y2)), add(clear(Block1)), add(available(Agent)) ]
        ).

        % Move a block from the top of another block to the top of another block
        action(move_onblock_to_block_start(Agent, Block1, Block2, Block3, X1, Y1, X3, Y3),
          [available(Agent), on(Block1, Block2), at(Block1, X1, Y1), at(Block3, X3, Y3), clear(Block3), clear(Block1)],
          [], [], [agent(Agent), block(Block1), block(Block2), block(Block3), pos(X1,Y1), pos(X3,Y3), Block1 \\= Block2, Block1 \\= Block3, Block2 \\= Block3],
          [ del(available(Agent)), del(clear(Block1)), del(on(Block1, Block2)), del(at(Block1, X1, Y1)), add(clear(Block2)), del(clear(Block3)),
            add(moving_onblock_to_block(Agent, Block1, Block3, X3, Y3)) ]
        ).
        action(move_onblock_to_block_end(Agent, Block1, Block3, X3, Y3),
          [moving_onblock_to_block(Agent, Block1, Block3, X3, Y3)],
          [], [], [agent(Agent)],
          [ del(moving_onblock_to_block(Agent, Block1, Block3, X3, Y3)),
            add(on(Block1, Block3)), add(at(Block1, X3, Y3)), add(clear(Block1)), add(available(Agent)) ]
        ).
        ```
        '''
    },
    {
        "natural_language":
        '''
        **Task:** You are given a high-level `init` state. The low-level query adds this information: "The agent `a1`'s arm starts at a known position (20,20), but its gripper state (open/closed) is not specified at the beginning. The final state of the arm is not important."
        Update the `init` and `goal` states accordingly.

        High-level init for reference:
        `init_state([ontable(b1), available(a1)]).`
        High-level goal for reference:
        `goal_state([ontable(b2), available(a1)]).`
        ''',
        "prolog_code":
        '''
        ```init
        init_state([
          ontable(b1),
          available(a1),
          ll_arm_at(a1,20,20),
          ll_gripper(a1,_)
        ]).
        ```

        ```goal
        goal_state([
          ontable(b2),
          available(a1),
          ll_arm_at(a1,_,_),
          ll_gripper(a1,_)
        ]).
        ```
        '''
    },
    {
        "natural_language":
        '''
        **Task:** Generate the standard set of primitive, low-level actions for a robot arm, suitable for a temporal planner. The robot's capabilities are arm movement and gripper control.
        ''',
        "prolog_code":
        '''
        ```ll_actions
        ll_action(move_arm_start(Agent, X1, Y1, X2, Y2),
          [ll_arm_at(Agent, X1, Y1)],
          [ll_moving_arm(Agent, _, _, _, _)], [], [ll_arm(Agent), pos(X1,Y1), pos(X2,Y2)],
          [ del(ll_arm_at(Agent, X1, Y1)), add(ll_moving_arm(Agent, X1, Y1, X2, Y2)) ]
        ).
        ll_action(move_arm_end(Agent, X1, Y1, X2, Y2),
          [ll_moving_arm(Agent, X1, Y1, X2, Y2)],
          [], [], [ll_arm(Agent)],
          [ del(ll_moving_arm(Agent, X1, Y1, X2, Y2)), add(ll_arm_at(Agent, X2, Y2)) ]
        ).

        ll_action(close_gripper_start(Agent),
          [ll_gripper(Agent, open)],
          [ll_moving_arm(Agent, _, _, _, _)], [], [ll_gripper(Agent)],
          [ del(ll_gripper(Agent, open)), add(ll_gripping(Agent)) ]
        ).
        ll_action(close_gripper_end(Agent),
          [ll_gripping(Agent)],
          [], [], [ll_gripper(Agent)],
          [ del(ll_gripping(Agent)), add(ll_gripper(Agent, closed)), add(ll_gripped(Agent)) ]
        ).

        ll_action(open_gripper_start(Agent),
          [ll_gripped(Agent)],
          [ll_moving_arm(Agent, _, _, _, _)], [], [ll_gripper(Agent)],
          [ del(ll_gripped(Agent)), del(ll_gripper(Agent, closed)), add(ll_releasing(Agent)) ]
        ).
        ll_action(open_gripper_end(Agent),
          [ll_releasing(Agent)],
          [], [], [ll_gripper(Agent)],
          [ del(ll_releasing(Agent)), add(ll_gripper(Agent, open)) ]
        ).
        ```
        '''
    },
    {
        "natural_language":
        '''
        **Task:** Provide the low-level sequence mapping for the high-level action `move_onblock_to_table_start`.
        ''',
        "prolog_code":
        '''
        ```mappings
        mapping(move_onblock_to_table_start(Agent, _Block1, _Block2, X1, Y1, X2, Y2),
          [
            move_arm_start(Agent, _, _, X1, Y1),
            move_arm_end(Agent, _, _, X1, Y1),
            close_gripper_start(Agent),
            close_gripper_end(Agent),
            move_arm_start(Agent, X1, Y1, X2, Y2),
            move_arm_end(Agent, X1, Y1, X2, Y2),
            open_gripper_start(Agent),
            open_gripper_end(Agent)
          ]
        ).
        ```
        '''
    },
    {
        "natural_language":
        '''
        **Task:** Generate the static knowledge base for the simplest possible world: one block `b1`, one agent `a1`, and one position `(1,1)`.
        ''',
        "prolog_code":
        '''
        ```kb
        % Positions
        pos(1,1).

        % Blocks
        block(b1).

        % Agents
        agent(a1).

        % Resources
        resources(agent(_)).
        ```
        '''
    },
    {
        "natural_language":
        '''
        **Task:** Generate the initial and goal states for a basic move.
        A single block `b1` is on the table at position (1,1). The goal is to move it to position (2,2). Agent `a1` is available.
        ''',
        "prolog_code":
        '''
        ```init
        init_state([
          ontable(b1),
          at(b1,1,1),
          clear(b1),
          available(a1)
        ]).
        ```

        ```goal
        goal_state([
          ontable(b1),
          at(b1,2,2),
          clear(b1),
          available(a1)
        ]).
        ```
        '''
    },
    {
        "natural_language":
        '''
        **Task:** Update a high-level KB. The low-level description only adds one detail: "The agent `a1` is a robotic arm."

        High-level KB for reference:
        ```kb
        % Agents
        agent(a1).
        % Resources
        resources(agent(_)).
        ```
        ''',
        "prolog_code":
        '''
        ```kb
        % Agents
        agent(a1).

        % Low-level physical entities
        ll_arm(a1).

        % Resources for both levels
        resources(agent(_)).
        resources(ll_arm(_)).
        ```
        '''
    },
    {
        "natural_language":
        '''
        **Task:** Generate the initial and goal states for the simplest stacking task.
        Blocks `b1` and `b2` are on the table at (1,1) and (2,2). The goal is to place `b1` on top of `b2`. Agent `a1` is available.
        ''',
        "prolog_code":
        '''
        ```init
        init_state([
          ontable(b1),
          ontable(b2),
          at(b1,1,1),
          at(b2,2,2),
          clear(b1),
          clear(b2),
          available(a1)
        ]).
        ```

        ```goal
        goal_state([
          ontable(b2),
          on(b1,b2),
          at(b2,2,2),
          at(b1,2,2),
          clear(b1),
          available(a1)
        ]).
        ```
        '''
    },
    {
        "natural_language":
        '''
        **Task:** Update this simple `init` state. The low-level query says: "The agent `a1`'s arm starts at (0,0) and its gripper is open."

        High-level init for reference:
        `init_state([ontable(b1), available(a1)]).`
        High-level goal for reference:
        `goal_state([ontable(b1), at(b1,5,5), available(a1)]).`
        ''',
        "prolog_code":
        '''
        ```init
        init_state([
          ontable(b1),
          available(a1),
          ll_arm_at(a1,0,0),
          ll_gripper(a1,open)
        ]).
        ```

        ```goal
        goal_state([
          ontable(b1),
          at(b1,5,5),
          available(a1),
          ll_arm_at(a1,_,_),
          ll_gripper(a1,_)
        ]).
        ```
        '''
    },
    {
        "natural_language":
        '''
        **Task:** Provide the standard set of low-level actions for a robot arm.
        ''',
        "prolog_code":
        '''
        ```ll_actions
        ll_action(move_arm_start(Agent, X1, Y1, X2, Y2),
          [ll_arm_at(Agent, X1, Y1)],
          [ll_moving_arm(Agent, _, _, _, _)], [], [ll_arm(Agent), pos(X1,Y1), pos(X2,Y2)],
          [ del(ll_arm_at(Agent, X1, Y1)), add(ll_moving_arm(Agent, X1, Y1, X2, Y2)) ]
        ).
        ll_action(move_arm_end(Agent, X1, Y1, X2, Y2),
          [ll_moving_arm(Agent, X1, Y1, X2, Y2)],
          [], [], [ll_arm(Agent)],
          [ del(ll_moving_arm(Agent, X1, Y1, X2, Y2)), add(ll_arm_at(Agent, X2, Y2)) ]
        ).
        ll_action(close_gripper_start(Agent),
          [ll_gripper(Agent, open)],
          [ll_moving_arm(Agent, _, _, _, _)], [], [ll_gripper(Agent)],
          [ del(ll_gripper(Agent, open)), add(ll_gripping(Agent)) ]
        ).
        ll_action(close_gripper_end(Agent),
          [ll_gripping(Agent)],
          [], [], [ll_gripper(Agent)],
          [ del(ll_gripping(Agent)), add(ll_gripper(Agent, closed)), add(ll_gripped(Agent)) ]
        ).
        ll_action(open_gripper_start(Agent),
          [ll_gripped(Agent)],
          [ll_moving_arm(Agent, _, _, _, _)], [], [ll_gripper(Agent)],
          [ del(ll_gripped(Agent)), del(ll_gripper(Agent, closed)), add(ll_releasing(Agent)) ]
        ).
        ll_action(open_gripper_end(Agent),
          [ll_releasing(Agent)],
          [], [], [ll_gripper(Agent)],
          [ del(ll_releasing(Agent)), add(ll_gripper(Agent, open)) ]
        ).
        ```
        '''
    },
    {
        "natural_language":
        '''
        **Task:** Provide the low-level mapping for the high-level action `move_table_to_table_start`.
        ''',
        "prolog_code":
        '''
        ```mappings
        mapping(move_table_to_table_start(Agent, _Block, X1, Y1, X2, Y2),
          [
            move_arm_start(Agent, _, _, X1, Y1),
            move_arm_end(Agent, _, _, X1, Y1),
            close_gripper_start(Agent),
            close_gripper_end(Agent),
            move_arm_start(Agent, X1, Y1, X2, Y2),
            move_arm_end(Agent, X1, Y1, X2, Y2),
            open_gripper_start(Agent),
            open_gripper_end(Agent)
          ]
        ).
        ```
        '''
    },
    {
        "natural_language":
        '''
        **Task:** Does the high-level action `move_table_to_block_end` have a low-level mapping?
        ''',
        "prolog_code":
        '''
        This _end action does not produce a mapping to low-level actions; its function is solely to reflect changes in the high-level state.
        '''
    }
]