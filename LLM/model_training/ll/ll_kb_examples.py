ll_kb_training_data_samples = [
    { #Sample 1
    "natural_language":
    '''
        General knowledge base:
            ```kb
            % Positions
            pos(1,1).
            pos(2,2).
            pos(3,3).
            pos(4,4).

            % Blocks
            block(b1).
            block(b2).
            block(b3).
            block(b4).
            block(b5).

            % Agents
            agent(a1).

            % Resources
            resources(agent(_)).
            ```
        Update the general knowledge base, provided by the high-level to reflect the changes
        in the description for the low-level, and identify new possible resources to be used, using
        the prefix "ll_" for low-level predicates to avoid conflicts with the high-level predicates,
        using the content of the following low-level query:
            "Given an initial state in which there are four blocks b1, b2, b3, b5 on the table in
            positions (2,2), (1,1), (3,3), (4,4), respectively, and a fifth block b4 on top of b3 in
            position (3,3), move the block b2 on top of b1, b3 on top of b2 and place b5 on the table
            in position (0,0) and b4 on the table in position (3,3). The agent is initially available
            and it will also be available at the end, with a robotic arm that is available at the beginning 
            and will be available at the end, which can be used to pick objects. The end-effector, which is a gripper
            of the arms are open at the beginning, but we do not care about their state at the end."
    ''',
    "prolog_code":
    '''
        ```kb
        % Positions
        pos(1,1).
        pos(2,2).
        pos(3,3).
        pos(4,4).

        % Blocks
        block(b1).
        block(b2).
        block(b3).
        block(b4).
        block(b5).

        % Agents
        agent(a1).

        % Arms
        ll_arm(a1).

        % Resources
        resources(agent(_)).
        resources(ll_arm(_)).
        ```
    ''',
    },
    { #Sample 2
    "natural_language":
    f'''
        General knowledge base:
            ```kb
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
        Update the general knowledge base, provided by the high-level to reflect the changes
        in the description for the low-level, and identify new possible resources to be used, using
        the prefix "ll_" for low-level predicates to avoid conflicts with the high-level predicates,
        using the content of the following low-level query:
            "There are 7 blocks on a table. In the initial state of the simulation, block b1 is in
            position (2,2), block b2 is in position (4,4), block b4 is in position (8,8). Block b5 is
            on top of block b1 in position (2,2) and block b3 is on top of block b2 in position
            (4,4). After moving the blocks around during the simulation, at the end, we have b5 on top
            of block b4, both in position (8,8), and the remaining blocks b1, b2 and b3 are in the
            same positions as in the initial state. There are other blocks which are not relevant for
            the simulation, such as b6, b7. We do not care about their position.
            There are 8 agents with a robotic arm. The end-effector, which is a gripper of the arms 
            are open at the beginning, but we do not care about their state at the end."
    ''',
    "prolog_code":
    '''
        ```kb
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

        % Arms
        ll_arm(a1).
        ll_arm(a2).
        ll_arm(a3).
        ll_arm(a4).
        ll_arm(a5).
        ll_arm(a6).
        ll_arm(a7).
        ll_arm(a8).

        % Resources
        resources(agent(_)).
        resources(ll_arm(_)).
        ```
    ''',
    },
    { #Sample 3
    "natural_language":
    f'''
        General knowledge base:
            ```kb
            % Positions
            pos(1,1).
            pos(2,2).
            pos(3,3).
            pos(4,4).
            pos(5,5).

            % Blocks
            block(b1).
            block(b2).
            block(b3).
            block(b4).
            block(b5).

            % Agents
            agent(a1).
            agent(a2).

            % Resources
            resources(agent(_)).
            ```
        Update the general knowledge base, provided by the high-level to reflect the changes
        in the description for the low-level, and identify new possible resources to be used, using
        the prefix "ll_" for low-level predicates to avoid conflicts with the high-level predicates,
        using the content of the following low-level query:
            "There are 5 blocks on a table. In the initial state of the simulation, block b1 is in
            position (1,1), block b2 is in position (2,2), block b3 is in position (3,3), b4 is in
            position (4,4) and b5 is in position (5,5). After the execution of the plan, the blocks
            are still on the table, but their positions are b1: (5,5), b2: (4,4), b3: (3,3), b4: (2,2)
            and b5: (1,1). There are two available agents, each with a robotic arm, that can carry out the task. They are
            available at the beginning and will be available at the end. The end-effector, which is a gripper of the
            arms are open at the beginning, but we do not care about their state at the end."
    ''',
    "prolog_code":
    '''
        ```kb
        % Positions
        pos(1,1).
        pos(2,2).
        pos(3,3).
        pos(4,4).
        pos(5,5).

        % Blocks
        block(b1).
        block(b2).
        block(b3).
        block(b4).
        block(b5).

        % Agents
        agent(a1).
        agent(a2).

        % Arms
        ll_arm(a1).
        ll_arm(a2).

        % Resources
        resources(agent(_)).
        resources(ll_arm(_)).
        ```
    ''',
    },
    { #Sample 4
    "natural_language":
    f'''
        General knowledge base:
            ```kb
            % Positions
            pos(0,2).
            pos(3,1).
            pos(4,4).

            % Blocks
            block(b1).
            block(b2).
            block(b3).

            % Agents
            agent(a1).

            % Resources
            resources(agent(_)).
            ```
        Update the general knowledge base, provided by the high-level to reflect the changes
        in the description for the low-level, and identify new possible resources to be used, using
        the prefix "ll_" for low-level predicates to avoid conflicts with the high-level predicates,
        using the content of the following low-level query:
            "The scenario begins with 3 blocks on the table: b1, b2, and b3. Initially, block b1 is
            at (0,2), block b2 is at (3,1), and block b3 can be found at (4,4). After a sequence of
            operations, the blocks remain on the table but their final arrangement is b1 at (4,4),
            b2 at (0,2), and b3 at (3,1). A single agent, a1, with a robotic arm, is available for all 
            necessary tasks and remains available. The end-effector, which is a gripper of the arms 
            are open at the beginning, but we do not care about their state at the end."
    ''',
    "prolog_code":
    '''
        ```kb
        % Positions
        pos(0,2).
        pos(3,1).
        pos(4,4).

        % Blocks
        block(b1).
        block(b2).
        block(b3).

        % Agents
        agent(a1).

        % Arms
        ll_arm(a1).

        % Resources
        resources(agent(_)).
        resources(ll_arm(_)).
        ```
    ''',
    },
    { #Sample 5
    "natural_language":
    f'''
        General knowledge base:
            ```kb
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
        Update the general knowledge base, provided by the high-level to reflect the changes
        in the description for the low-level, and identify new possible resources to be used, using
        the prefix "ll_" for low-level predicates to avoid conflicts with the high-level predicates,
        using the content of the following low-level query:
            "The problem begins with 6 blocks on the workspace: b1, b2, b3, b4, b5, and b6.
            Block b1 is at (1,4), b2 is at (4,1), b3 is at (0,0), b4 is at (2,3), and b5 is at (3,2).
            Block b6 is on top of b1. The planned actions include stacking b2 on b3, b4 on b5,
            and moving b6 to position (5,5). There are 3 agents (a1, a2, a3), each with a robotic arm, 
            assigned to this task. The end-effector, which is a gripper of the arms are open at the beginning, 
            but we do not care about their state at the end."
    ''',
    "prolog_code":
    '''
        ```kb
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

        % Arms
        ll_arm(a1).
        ll_arm(a2).
        ll_arm(a3).

        % Resources
        resources(agent(_)).
        resources(ll_arm(_)).
        ```
    ''',
    },
    { #Sample 6
    "natural_language":
    f'''
        General knowledge base:
            ```kb
            % Positions
            pos(0,2).
            pos(1,1).
            pos(2,0).
            pos(3,3).

            % Blocks
            block(b1).
            block(b2).
            block(b3).

            % Agents
            agent(a1).
            agent(a2).
            agent(a3).

            % Resources
            resources(agent(_)).
            ```
        Update the general knowledge base, provided by the high-level to reflect the changes
        in the description for the low-level, and identify new possible resources to be used, using
        the prefix "ll_" for low-level predicates to avoid conflicts with the high-level predicates,
        using the content of the following low-level query:
            "Consider an environment with blocks b1, b2, and b3. In their initial positions, b1 is at (2,0),
            b2 is at (0,2), and b3 is at (3,3). Block b1 will be moved to (3,3) to be on top of b3, and
            block b2 will be moved to position (1,1). 3 agents, each with a robotic arm, involved 
            in these operations, are available from start to finish. The end-effector, which is a gripper 
            of the arms are open at the beginning, but we do not care about their state at the end."
    ''',
    "prolog_code":
    '''
        ```kb
        % Positions
        pos(0,2).
        pos(1,1).
        pos(2,0).
        pos(3,3).

        % Blocks
        block(b1).
        block(b2).
        block(b3).

        % Agents
        agent(a1).
        agent(a2).
        agent(a3).

        % Arms
        ll_arm(a1).
        ll_arm(a2).
        ll_arm(a3).

        % Resources
        resources(agent(_)).
        resources(ll_arm(_)).
        ```
    ''',
    },
    { #Sample 7
    "natural_language":
    f'''
        General knowledge base:
            ```kb
            % Positions
            pos(0,0).
            pos(0,4).
            pos(1,3).
            pos(2,2).
            pos(3,1).
            pos(4,0).

            % Blocks
            block(b1).
            block(b2).
            block(b3).
            block(b4).
            block(b5).

            % Agents
            agent(a1).
            agent(a2).

            % Resources
            resources(agent(_)).
            ```
        Update the general knowledge base, provided by the high-level to reflect the changes
        in the description for the low-level, and identify new possible resources to be used, using
        the prefix "ll_" for low-level predicates to avoid conflicts with the high-level predicates,
        using the content of the following low-level query:
            "The initial state shows blocks b1, b2, b3, b4, and b5. Block b1 is at (0,4),
            b2 is at (4,0), b3 is at (1,3), b4 is at (3,1), and b5 is at (2,2).
            The tasks involve moving b1 on top of b2, b3 on top of b4, and placing
            b5 at table position (0,0). A team of 2 agents, each with a robotic arm, will carry out these 
            moves. The end-effector, which is a gripper of the arms are open at the beginning, but we do 
            not care about their state at the end."
    ''',
    "prolog_code":
    '''
        ```kb
        % Positions
        pos(0,0).
        pos(0,4).
        pos(1,3).
        pos(2,2).
        pos(3,1).
        pos(4,0).

        % Blocks
        block(b1).
        block(b2).
        block(b3).
        block(b4).
        block(b5).

        % Agents
        agent(a1).
        agent(a2).

        % Arms
        ll_arm(a1).
        ll_arm(a2).

        % Resources
        resources(agent(_)).
        resources(ll_arm(_)).
        ```
    ''',
    },
    { #Sample 8
    "natural_language":
    f'''
        General knowledge base:
            ```kb
            % Positions
            pos(0,0).
            pos(0,1).
            pos(1,0).
            pos(1,1).
            pos(1,2).
            pos(2,1).
            pos(3,3).
            pos(4,4).
            pos(5,5).
            pos(6,6).

            % Blocks
            block(b1).
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
        Update the general knowledge base, provided by the high-level to reflect the changes
        in the description for the low-level, and identify new possible resources to be used, using
        the prefix "ll_" for low-level predicates to avoid conflicts with the high-level predicates,
        using the content of the following low-level query:
            "We have a complex setup involving eight blocks: b1, b2, b3, b4, b5, b6, b7, and b8.
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
            Three agents, a1, a2, and a3, each with a robotic arm, are available to perform these 
            manipulations. The end-effector, which is a gripper of the arms are open at the beginning, 
            but we do not care about their state at the end."
    ''',
    "prolog_code":
    '''
        ```kb
        % Positions
        pos(0,0).
        pos(0,1).
        pos(1,0).
        pos(1,1).
        pos(1,2).
        pos(2,1).
        pos(3,3).
        pos(4,4).
        pos(5,5).
        pos(6,6).

        % Blocks
        block(b1).
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

        % Arms
        ll_arm(a1).
        ll_arm(a2).
        ll_arm(a3).

        % Resources
        resources(agent(_)).
        resources(ll_arm(_)).
        ```
    ''',
    }
]

ll_kb_evaluation_data_samples = [
    { #Sample 1
    "natural_language":
    f'''
        General knowledge base:
            ```kb
            % Positions
            pos(0,3).
            pos(1,1).
            pos(3,0).
            pos(4,4).

            % Blocks
            block(b1).
            block(b2).
            block(b3).
            block(b4).
            block(b5).

            % Agents
            agent(a1).
            agent(a2).

            % Resources
            resources(agent(_)).
            ```
        Update the general knowledge base, provided by the high-level to reflect the changes
        in the description for the low-level, and identify new possible resources to be used, using
        the prefix "ll_" for low-level predicates to avoid conflicts with the high-level predicates,
        using the content of the following low-level query:
            "This scenario features 5 blocks on the table. Initially, block b1 is located at (0,3),
            block b2 is at (3,0), and block b3 is at (1,1). Block b4 is found on top of b1 at (0,3),
            and block b5 is on top of b2 at (3,0). The plan involves moving b4 onto b3, and then placing
            b5 at position (4,4). There are 2 agents, a1 and a2, each with a robotic arm, available to 
            perform these operations. The end-effector, which is a gripper of the arms are open at the 
            beginning, but we do not care about their state at the end."
    ''',
    "prolog_code":
    '''
        ```kb
        % Positions
        pos(0,3).
        pos(1,1).
        pos(3,0).
        pos(4,4).

        % Blocks
        block(b1).
        block(b2).
        block(b3).
        block(b4).
        block(b5).

        % Agents
        agent(a1).
        agent(a2).

        % Arms
        ll_arm(a1).
        ll_arm(a2).

        % Resources
        resources(agent(_)).
        resources(ll_arm(_)).
        ```
    ''',
    },
    { #Sample 2
    "natural_language":
    f'''
        General knowledge base:
            ```kb
            % Positions
            pos(0,3).
            pos(1,2).
            pos(2,1).
            pos(3,0).
            pos(5,5).

            % Blocks
            block(b1).
            block(b2).
            block(b3).
            block(b4).

            % Agents
            agent(a1).
            agent(a2).
            agent(a3).
            agent(a4).

            % Resources
            resources(agent(_)).
            ```
        Update the general knowledge base, provided by the high-level to reflect the changes
        in the description for the low-level, and identify new possible resources to be used, using
        the prefix "ll_" for low-level predicates to avoid conflicts with the high-level predicates,
        using the content of the following low-level query:
            "Given the starting configuration with four blocks: b1, b2, b3, and b4. Block b1 is at (5,5),
            b2 is at (1,2), b3 is at (2,1). Block b4 is initially stacked upon b3. The objective is to move b1
            to position (0,3), place b2 on top of b4, and move b3 to (3,0). Four agents, each with a robotic arm, 
            are available and will be utilized for these manipulations. The end-effector, which is a gripper 
            of the arms are open at the beginning, but we do not care about their state at the end."
    ''',
    "prolog_code":
    '''
        ```kb
        % Positions
        pos(0,3).
        pos(1,2).
        pos(2,1).
        pos(3,0).
        pos(5,5).

        % Blocks
        block(b1).
        block(b2).
        block(b3).
        block(b4).

        % Agents
        agent(a1).
        agent(a2).
        agent(a3).
        agent(a4).

        % Arms
        ll_arm(a1).
        ll_arm(a2).
        ll_arm(a3).
        ll_arm(a4).

        % Resources
        resources(agent(_)).
        resources(ll_arm(_)).
        ```
    ''',
    }
]
