states_training_data_samples = [ 
    { #Sample 1
    "natural_language":
    '''
        Provide the initial and goal states for the following scenario:
        Given an initial state in which there are four blocks b1, b2, b3, b5 on the table in
        positions (2,2), (1,1), (3,3), (4,4), respectively, and a fifth block b4 on top of b3 in
        position (3,3), move the block b2 on top of b1, b3 on top of b2 and place b5 on the table
        in position (0,0) and b4 on the table in position (3,3). The agent is initially available
        and it will also be available at the end.
    ''',
    "prolog_code":
    '''
        ```init
        init_state([
        ontable(b1), ontable(b2), ontable(b3), ontable(b5),
        on(b4, b3),
        at(b1,2,2), at(b2,1,1), at(b3,3,3), at(b4,3,3), at(b5,4,4),
        clear(b1), clear(b2), clear(b4), clear(b5),
        available(a1)
        ]).
        ```

        ```goal
        goal_state([
        ontable(b1), ontable(b4), ontable(b5),
        on(b3, b2), on(b2, b1),
        at(b1,2,2), at(b2,2,2), at(b3,2,2), at(b4,3,3), at(b5,0,0),
        clear(b3), clear(b4), clear(b5),
        available(a1)
        ]).
        ```
    ''',
    },
    { #Sample 2
    "natural_language":
    '''
        Provide the initial and goal states for the following scenario:
        There are 7 blocks on a table. In the initial state of the simulation, block b1 is in
        position (2,2), block b2 is in position (4,4), block b4 is in position (8,8). Block b5 is
        on top of block b1 in position (2,2) and block b3 is on top of block b2 in position
        (4,4). After moving the blocks around during the simulation, at the end, we have b5 on top
        of block b4, both in position (8,8), and the remaining blocks b1, b2 and b3 are in the
        same positions as in the initial state. There are other blocks which are not relevant for
        the simulation, such as b6, b7. We do not care about their position.
        There are 8 agents with a robotic arm.
    ''',
    "prolog_code":
    '''
        ```init
        init_state([
        ontable(b1), ontable(b2), ontable(b4),
        on(b5, b1), on(b3, b2),
        at(b1,2,2), at(b2,4,4), at(b3,4,4), at(b4,8,8), at(b5,2,2),
        clear(b3), clear(b4), clear(b5),
        available(a1), available(a2), available(a3), available(a4), available(a5), available(a6), available(a7), available(a8)
        ]).
        ```

        ```goal
        goal_state([
        ontable(b1), ontable(b2), ontable(b4),
        on(b5, b4), on(b3, b2),
        at(b1,2,2), at(b2,4,4), at(b3,4,4), at(b4,8,8), at(b5,8,8),
        clear(b1), clear(b3), clear(b5),
        available(a1), available(a2), available(a3), available(a4), available(a5), available(a6), available(a7), available(a8)
        ]).
        ```
    ''',
    },
    { #Sample 3
    "natural_language":
    '''
        Provide the initial and goal states for the following scenario:
        There are 5 blocks on a table. In the initial state of the simulation, block b1 is in
        position (1,1), block b2 is in position (2,2), block b3 is in position (3,3), b4 is in
        position (4,4) and b5 is in position (5,5). After the execution of the plan, the blocks
        are still on the table, but their positions are b1: (5,5), b2: (4,4), b3: (3,3), b4: (2,2)
        and b5: (1,1). There are two available agents that can carry out the task. They are
        available at the beginning and will be available at the end.
    ''',
    "prolog_code":
    '''
        ```init
        init_state([
        ontable(b1), ontable(b2), ontable(b3), ontable(b4), ontable(b5),
        at(b1,1,1), at(b2,2,2), at(b3,3,3), at(b4,4,4), at(b5,5,5),
        clear(b1), clear(b2), clear(b3), clear(b4), clear(b5),
        available(a1), available(a2)
        ]).
        ```

        ```goal
        goal_state([
        ontable(b1), ontable(b2), ontable(b3), ontable(b4), ontable(b5),
        at(b1,5,5), at(b2,4,4), at(b3,3,3), at(b4,2,2), at(b5,1,1),
        clear(b1), clear(b2), clear(b3), clear(b4), clear(b5),
        available(a1), available(a2)
        ]).
        ```
    ''',
    },
    { #Sample 4
    "natural_language":
    '''
        Provide the initial and goal states for the following scenario:
        The scenario begins with 3 blocks on the table: b1, b2, and b3. Initially, block b1 is
        at (0,2), block b2 is at (3,1), and block b3 can be found at (4,4). After a sequence of
        operations, the blocks remain on the table but their final arrangement is b1 at (4,4),
        b2 at (0,2), and b3 at (3,1). A single agent, a1, is available for all necessary tasks
        and remains available.
    ''',
    "prolog_code":
    '''
        ```init
        init_state([
        ontable(b1), ontable(b2), ontable(b3),
        at(b1,0,2), at(b2,3,1), at(b3,4,4),
        clear(b1), clear(b2), clear(b3),
        available(a1)
        ]).
        ```

        ```goal
        goal_state([
        ontable(b1), ontable(b2), ontable(b3),
        at(b1,4,4), at(b2,0,2), at(b3,3,1),
        clear(b1), clear(b2), clear(b3),
        available(a1)
        ]).
        ```
    ''',
    },
    { #Sample 5
    "natural_language":
    '''
        Provide the initial and goal states for the following scenario:
        The problem begins with 6 blocks on the workspace: b1, b2, b3, b4, b5, and b6.
        Block b1 is at (1,4), b2 is at (4,1), b3 is at (0,0), b4 is at (2,3), and b5 is at (3,2).
        Block b6 is on top of b1. The planned actions include stacking b2 on b3, b4 on b5,
        and moving b6 to position (5,5). There are 3 agents (a1, a2, a3) assigned to this task.
    ''',
    "prolog_code":
    '''
        ```init
        init_state([
        ontable(b1), ontable(b2), ontable(b3), ontable(b4), ontable(b5),
        on(b6,b1),
        at(b1,1,4), at(b2,4,1), at(b3,0,0), at(b4,2,3), at(b5,3,2), at(b6,1,4),
        clear(b2), clear(b3), clear(b4), clear(b5), clear(b6),
        available(a1), available(a2), available(a3)
        ]).
        ```

        ```goal
        goal_state([
        ontable(b1), ontable(b3), ontable(b5), ontable(b6),
        on(b2,b3), on(b4,b5),
        at(b1,1,4), at(b2,0,0), at(b3,0,0), at(b4,3,2), at(b5,3,2), at(b6,5,5),
        clear(b1), clear(b2), clear(b4), clear(b6),
        available(a1), available(a2), available(a3)
        ]).
        ```
    ''',
    },
    { #Sample 6
    "natural_language":
    '''
        Provide the initial and goal states for the following scenario:
        Consider an environment with blocks b1, b2, and b3. In their initial positions, b1 is at (2,0),
        b2 is at (0,2), and b3 is at (3,3). Block b1 will be moved to (3,3) to be on top of b3, and
        block b2 will be moved to position (1,1). 3 agents, involved in these operations, are available
        from start to finish.
    ''',
    "prolog_code":
    '''
        ```init
        init_state([
        ontable(b1), ontable(b2), ontable(b3),
        at(b1,2,0), at(b2,0,2), at(b3,3,3),
        clear(b1), clear(b2), clear(b3),
        available(a1), available(a2), available(a3)
        ]).
        ```

        ```goal
        goal_state([
        ontable(b2), ontable(b3),
        on(b1,b3),
        at(b1,3,3), at(b2,1,1), at(b3,3,3),
        clear(b1), clear(b2),
        available(a1), available(a2), available(a3)
        ]).
        ```
    ''',
    },
    { #Sample 7
    "natural_language":
    '''
        Provide the initial and goal states for the following scenario:
        The initial state shows blocks b1, b2, b3, b4, and b5. Block b1 is at (0,4),
        b2 is at (4,0), b3 is at (1,3), b4 is at (3,1), and b5 is at (2,2).
        The tasks involve moving b1 on top of b2, b3 on top of b4, and placing
        b5 at table position (0,0). A team of 2 agents will carry out these moves.
    ''',
    "prolog_code":
    '''
        ```init
        init_state([
        ontable(b1), ontable(b2), ontable(b3), ontable(b4), ontable(b5),
        at(b1,0,4), at(b2,4,0), at(b3,1,3), at(b4,3,1), at(b5,2,2),
        clear(b1), clear(b2), clear(b3), clear(b4), clear(b5),
        available(a1), available(a2)
        ]).
        ```

        ```goal
        goal_state([
        ontable(b2), ontable(b4), ontable(b5),
        on(b1,b2), on(b3,b4),
        at(b1,4,0), at(b2,4,0), at(b3,3,1), at(b4,3,1), at(b5,0,0),
        clear(b1), clear(b3), clear(b5),
        available(a1), available(a2)
        ]).
        ```
    ''',
    },
    { #Sample 8
    "natural_language":
    '''
        Provide the initial and goal states for the following scenario:
        We have a complex setup involving eight blocks: b1, b2, b3, b4, b5, b6, b7, and b8.
        Initially, b1 is at (1,1), b2 is at (1,2), and b3 is at (2,1) on the table.
        Block b4 is stacked on top of b1, and block b5 is on top of b2.
        Blocks b6 and b7 are also on the table at positions (3,3) and (4,4) respectively,
        while b8 is located at (6,6).
        The goal is to rearrange them significantly:
        b1 will be moved to the table at (0,0).
        b2 will be moved to the table at (0,1).
        b3 will be moved to the table at (1,0).
        A new tower will be constructed at (5,5) with b6 at the bottom, b5 on b6, and b4 on b5.
        Finally, b7 will be placed on top of b8.
        Three agents, a1, a2, and a3, are available to perform these manipulations.
    ''',
    "prolog_code":
    '''
        ```init
        init_state([
        ontable(b1), ontable(b2), ontable(b3), ontable(b6), ontable(b7), ontable(b8),
        on(b4,b1), on(b5,b2),
        at(b1,1,1), at(b2,1,2), at(b3,2,1), at(b4,1,1), at(b5,1,2), at(b6,3,3), at(b7,4,4), at(b8,6,6),
        clear(b3), clear(b4), clear(b5), clear(b6), clear(b7), clear(b8),
        available(a1), available(a2), available(a3)
        ]).
        ```

        ```goal
        goal_state([
        ontable(b1), ontable(b2), ontable(b3), ontable(b6), ontable(b8),
        on(b5,b6), on(b4,b5), on(b7,b8),
        at(b1,0,0), at(b2,0,1), at(b3,1,0), at(b4,5,5), at(b5,5,5), at(b6,5,5), at(b7,6,6), at(b8,6,6),
        clear(b1), clear(b2), clear(b3), clear(b4), clear(b7),
        available(a1), available(a2), available(a3)
        ]).
        ```
    ''',
    },
]

states_evaluation_data_samples = [ 
    { #Sample 1
    "natural_language":
    '''
        Provide the initial and goal states for the following scenario:
        This scenario features 5 blocks on the table. Initially, block b1 is located at (0,3),
        block b2 is at (3,0), and block b3 is at (1,1). Block b4 is found on top of b1 at (0,3),
        and block b5 is on top of b2 at (3,0). The plan involves moving b4 onto b3, and then placing
        b5 at position (4,4). There are 2 agents, a1 and a2, available to perform these operations.
    ''',
    "prolog_code":
    '''
        ```init
        init_state([
        ontable(b1), ontable(b2), ontable(b3),
        on(b4,b1), on(b5,b2),
        at(b1,0,3), at(b2,3,0), at(b3,1,1), at(b4,0,3), at(b5,3,0),
        clear(b3), clear(b4), clear(b5),
        available(a1), available(a2)
        ]).
        ```

        ```goal
        goal_state([
        ontable(b1), ontable(b2), ontable(b3), ontable(b5),
        on(b4,b3),
        at(b1,0,3), at(b2,3,0), at(b3,1,1), at(b4,1,1), at(b5,4,4),
        clear(b1), clear(b2), clear(b4), clear(b5),
        available(a1), available(a2)
        ]).
        ```
    ''',
    },
    { #Sample 2
    "natural_language":
    '''
        Provide the initial and goal states for the following scenario:
        Given the starting configuration with four blocks: b1, b2, b3, and b4. Block b1 is at (5,5),
        b2 is at (1,2), b3 is at (2,1). Block b4 is initially stacked upon b3. The objective is to move b1
        to position (0,3), place b2 on top of b4, and move b3 to (3,0). Four agents are available and will
        be utilized for these manipulations.
    ''',
    "prolog_code":
    '''
        ```init
        init_state([
        ontable(b1), ontable(b2), ontable(b3),
        on(b4,b3),
        at(b1,5,5), at(b2,1,2), at(b3,2,1), at(b4,2,1),
        clear(b1), clear(b2), clear(b4),
        available(a1), available(a2), available(a3), available(a4)
        ]).
        ```
        
        ```goal
        goal_state([
        ontable(b1), ontable(b3), ontable(b4),
        on(b2,b4),
        at(b1,0,3), at(b2,2,1), at(b3,3,0), at(b4,2,1),
        clear(b1), clear(b2), clear(b3),
        available(a1), available(a2), available(a3), available(a4)
        ]).
        ```
    ''',
    }
]
