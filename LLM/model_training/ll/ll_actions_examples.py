ll_actions_training_data_samples = [
    { #Sample 1
    "natural_language":
    '''
        Generate the Prolog definitions for a set of low-level robotic actions.
        The available fundamental operations are:
        - move_arm(Arm, X1, Y1, X2, Y2): Moves the robotic Arm from position (X1,Y1) to (X2,Y2).
        - grip(Arm): Closes the Arm's gripper.
        - release(Arm): Opens the Arm's gripper.

        Each fundamental operation must be decomposed into two instantaneous events for planning:
        1. An '_start' event (e.g., move_arm_start): Initiates the operation and typically marks 
        the involved resource (Arm) as busy or in an intermediate state.
        2. An '_end' event (e.g., move_arm_end): Completes the operation, achieves its primary effect, 
        and updates the resource's state accordingly (e.g., making it available or reflecting its new status).
    ''',
    "prolog_code":
    '''
        ```ll_actions
        ll_action(grip_start(A),
        [ll_gripper(A, open)],
        [ll_arm_gripping(A), ll_gripped(A)],
        [],
        [ll_arm(A)],
        [
            del(ll_gripper(A, open)),
            add(ll_arm_gripping(A))
        ]
        ).

        ll_action(grip_end(A),
        [ll_arm_gripping(A)],
        [],
        [],
        [ll_arm(A)],
        [
            del(ll_arm_gripping(A)),
            add(ll_gripper(A, closed))
        ]
        ).

        ll_action(release_start(A),
        [ll_gripper(A, closed)],
        [],
        [],
        [ll_arm(A)],
        [
            del(ll_gripper(A, closed)),
            add(ll_arm_releasing(A))
        ]
        ).

        ll_action(release_end(A),
        [ll_arm_releasing(A)],
        [],
        [],
        [ll_arm(A)],
        [
            del(ll_arm_releasing(A)),
            add(ll_gripper(A, open))
        ]
        ).

        ll_action(move_arm_start(A, X1, Y1, X2, Y2),
        [ll_arm_at(A, X1, Y1)],
        [ll_moving(_, _, _, X2, Y2), ll_moving(A, _, _, _, _)],
        [],
        [ll_arm(A), pos(X1, Y1), pos(X2, Y2)],
        [
            del(ll_arm_at(A, X1, Y1)),
            add(ll_moving(A, X1, Y1, X2, Y2))
        ]
        ).

        ll_action(move_arm_end(A, X1, Y1, X2, Y2),
        [ll_moving(A, X1, Y1, X2, Y2)],
        [],
        [],
        [ll_arm(A)],
        [
            del(ll_moving(A, X1, Y1, X2, Y2)),
            add(ll_arm_at(A, X2, Y2))
        ]
        ).
        ```
    ''',
    },
    { #Sample 2
    "natural_language":
    '''
        Define the fundamental low-level operations for a robotic arm. These operations include:
        - Moving the arm between two specified (x,y) coordinates.
        - Activating the arm's gripper to grasp.
        - Deactivating the arm's gripper to release.

        Each operation must be modeled with distinct start and end events for planning purposes. 
        The start event should initiate the action and mark the arm as busy, while the end event 
        should complete the action and update the arm's state.
    ''',
    "prolog_code":
    '''
        ```ll_actions
        ll_action(grip_start(A),
        [ll_gripper(A, open)],
        [ll_arm_gripping(A), ll_gripped(A)],
        [],
        [ll_arm(A)],
        [
            del(ll_gripper(A, open)),
            add(ll_arm_gripping(A))
        ]
        ).

        ll_action(grip_end(A),
        [ll_arm_gripping(A)],
        [],
        [],
        [ll_arm(A)],
        [
            del(ll_arm_gripping(A)),
            add(ll_gripper(A, closed))
        ]
        ).

        ll_action(release_start(A),
        [ll_gripper(A, closed)],
        [],
        [],
        [ll_arm(A)],
        [
            del(ll_gripper(A, closed)),
            add(ll_arm_releasing(A))
        ]
        ).

        ll_action(release_end(A),
        [ll_arm_releasing(A)],
        [],
        [],
        [ll_arm(A)],
        [
            del(ll_arm_releasing(A)),
            add(ll_gripper(A, open))
        ]
        ).

        ll_action(move_arm_start(A, X1, Y1, X2, Y2),
        [ll_arm_at(A, X1, Y1)],
        [ll_moving(_, _, _, X2, Y2), ll_moving(A, _, _, _, _)],
        [],
        [ll_arm(A), pos(X1, Y1), pos(X2, Y2)],
        [
            del(ll_arm_at(A, X1, Y1)),
            add(ll_moving(A, X1, Y1, X2, Y2))
        ]
        ).

        ll_action(move_arm_end(A, X1, Y1, X2, Y2),
        [ll_moving(A, X1, Y1, X2, Y2)],
        [],
        [],
        [ll_arm(A)],
        [
            del(ll_moving(A, X1, Y1, X2, Y2)),
            add(ll_arm_at(A, X2, Y2))
        ]
        ).
        ```
    ''',
    },
    { #Sample 3
    "natural_language":
    '''
        Generate Prolog schemata for primitive robotic manipulator actions. The required actions are:
        1. Gripper closure (grip(Arm)).
        2. Gripper opening (release(Arm)).
        3. Arm repositioning (move_arm(Arm, FromX, FromY, ToX, ToY)).

        For each primitive, provide two instantaneous action definitions: an '_start' 
        variant to begin the process and allocate resources, and an '_end' 
        variant to finalize the process and reflect its outcome.
    ''',
    "prolog_code":
    '''
        ```ll_actions
        ll_action(grip_start(A),
        [ll_gripper(A, open)],
        [ll_arm_gripping(A), ll_gripped(A)],
        [],
        [ll_arm(A)],
        [
            del(ll_gripper(A, open)),
            add(ll_arm_gripping(A))
        ]
        ).

        ll_action(grip_end(A),
        [ll_arm_gripping(A)],
        [],
        [],
        [ll_arm(A)],
        [
            del(ll_arm_gripping(A)),
            add(ll_gripper(A, closed))
        ]
        ).

        ll_action(release_start(A),
        [ll_gripper(A, closed)],
        [],
        [],
        [ll_arm(A)],
        [
            del(ll_gripper(A, closed)),
            add(ll_arm_releasing(A))
        ]
        ).

        ll_action(release_end(A),
        [ll_arm_releasing(A)],
        [],
        [],
        [ll_arm(A)],
        [
            del(ll_arm_releasing(A)),
            add(ll_gripper(A, open))
        ]
        ).

        ll_action(move_arm_start(A, X1, Y1, X2, Y2),
        [ll_arm_at(A, X1, Y1)],
        [ll_moving(_, _, _, X2, Y2), ll_moving(A, _, _, _, _)],
        [],
        [ll_arm(A), pos(X1, Y1), pos(X2, Y2)],
        [
            del(ll_arm_at(A, X1, Y1)),
            add(ll_moving(A, X1, Y1, X2, Y2))
        ]
        ).

        ll_action(move_arm_end(A, X1, Y1, X2, Y2),
        [ll_moving(A, X1, Y1, X2, Y2)],
        [],
        [],
        [ll_arm(A)],
        [
            del(ll_moving(A, X1, Y1, X2, Y2)),
            add(ll_arm_at(A, X2, Y2))
        ]
        ).
        ```
    ''',
    },
    { #Sample 4
    "natural_language":
    '''
        Provide the Prolog definitions for a set of low-level robotic actions, specifically focusing on 
        their decomposition into start and end phases. The actions to define are:
        - grip(A): Causes the gripper of arm A to close.
        - release(A): Causes the gripper of arm A to open.
        - move_arm(A, X1, Y1, X2, Y2): Moves arm A from (X1,Y1) to (X2,Y2).

        The '_start' phase of each action should handle preconditions and initiate the arm's activity. 
        The '_end' phase should verify completion and apply the final effects.
    ''',
    "prolog_code":
    '''
        ```ll_actions
        ll_action(grip_start(A),
        [ll_gripper(A, open)],
        [ll_arm_gripping(A), ll_gripped(A)],
        [],
        [ll_arm(A)],
        [
            del(ll_gripper(A, open)),
            add(ll_arm_gripping(A))
        ]
        ).

        ll_action(grip_end(A),
        [ll_arm_gripping(A)],
        [],
        [],
        [ll_arm(A)],
        [
            del(ll_arm_gripping(A)),
            add(ll_gripper(A, closed))
        ]
        ).

        ll_action(release_start(A),
        [ll_gripper(A, closed)],
        [],
        [],
        [ll_arm(A)],
        [
            del(ll_gripper(A, closed)),
            add(ll_arm_releasing(A))
        ]
        ).

        ll_action(release_end(A),
        [ll_arm_releasing(A)],
        [],
        [],
        [ll_arm(A)],
        [
            del(ll_arm_releasing(A)),
            add(ll_gripper(A, open))
        ]
        ).

        ll_action(move_arm_start(A, X1, Y1, X2, Y2),
        [ll_arm_at(A, X1, Y1)],
        [ll_moving(_, _, _, X2, Y2), ll_moving(A, _, _, _, _)],
        [],
        [ll_arm(A), pos(X1, Y1), pos(X2, Y2)],
        [
            del(ll_arm_at(A, X1, Y1)),
            add(ll_moving(A, X1, Y1, X2, Y2))
        ]
        ).

        ll_action(move_arm_end(A, X1, Y1, X2, Y2),
        [ll_moving(A, X1, Y1, X2, Y2)],
        [],
        [],
        [ll_arm(A)],
        [
            del(ll_moving(A, X1, Y1, X2, Y2)),
            add(ll_arm_at(A, X2, Y2))
        ]
        ).
        ```
    ''',
    },
    { #Sample 5
    "natural_language":
    '''
        In a typical blocks world scenario involving stacking blocks like b1, b2, and b3 on a table using an agent a1, 
        we need to define the underlying low-level robotic arm capabilities.
        Please provide the Prolog definitions for the following actions:
        - move_arm(Arm, X1, Y1, X2, Y2)
        - grip(Arm)
        - release(Arm)

        Remember to split each into '_start' and '_end' events, managing the arm's state (e.g., busy, at_location, gripper_state).
    ''',
    "prolog_code":
    '''
        ```ll_actions
        ll_action(grip_start(A),
        [ll_gripper(A, open)],
        [ll_arm_gripping(A), ll_gripped(A)],
        [],
        [ll_arm(A)],
        [
            del(ll_gripper(A, open)),
            add(ll_arm_gripping(A))
        ]
        ).

        ll_action(grip_end(A),
        [ll_arm_gripping(A)],
        [],
        [],
        [ll_arm(A)],
        [
            del(ll_arm_gripping(A)),
            add(ll_gripper(A, closed))
        ]
        ).

        ll_action(release_start(A),
        [ll_gripper(A, closed)],
        [],
        [],
        [ll_arm(A)],
        [
            del(ll_gripper(A, closed)),
            add(ll_arm_releasing(A))
        ]
        ).

        ll_action(release_end(A),
        [ll_arm_releasing(A)],
        [],
        [],
        [ll_arm(A)],
        [
            del(ll_arm_releasing(A)),
            add(ll_gripper(A, open))
        ]
        ).

        ll_action(move_arm_start(A, X1, Y1, X2, Y2),
        [ll_arm_at(A, X1, Y1)],
        [ll_moving(_, _, _, X2, Y2), ll_moving(A, _, _, _, _)],
        [],
        [ll_arm(A), pos(X1, Y1), pos(X2, Y2)],
        [
            del(ll_arm_at(A, X1, Y1)),
            add(ll_moving(A, X1, Y1, X2, Y2))
        ]
        ).

        ll_action(move_arm_end(A, X1, Y1, X2, Y2),
        [ll_moving(A, X1, Y1, X2, Y2)],
        [],
        [],
        [ll_arm(A)],
        [
            del(ll_moving(A, X1, Y1, X2, Y2)),
            add(ll_arm_at(A, X2, Y2))
        ]
        ).
        ```
    ''',
    },
    { #Sample 6
    "natural_language":
    '''
        Our robotic system uses ROS (Robot Operating System) and communicates over a TCP/IP network. The arms 
        have 6 degrees of freedom. For the high-level planner, we need abstract low-level action definitions 
        in Prolog. Please define:
        - move_arm(Identifier, StartCoordX, StartCoordY, EndCoordX, EndCoordY)
        - grip(Identifier)
        - release(Identifier)

        Each action should be broken down into a start and end component, detailing preconditions, 
        effects, and resource usage (e.g., arm 'Identifier').
    ''',
    "prolog_code":
    '''
        ```ll_actions
        ll_action(grip_start(A),
        [ll_gripper(A, open)],
        [ll_arm_gripping(A), ll_gripped(A)],
        [],
        [ll_arm(A)],
        [
            del(ll_gripper(A, open)),
            add(ll_arm_gripping(A))
        ]
        ).

        ll_action(grip_end(A),
        [ll_arm_gripping(A)],
        [],
        [],
        [ll_arm(A)],
        [
            del(ll_arm_gripping(A)),
            add(ll_gripper(A, closed))
        ]
        ).

        ll_action(release_start(A),
        [ll_gripper(A, closed)],
        [],
        [],
        [ll_arm(A)],
        [
            del(ll_gripper(A, closed)),
            add(ll_arm_releasing(A))
        ]
        ).

        ll_action(release_end(A),
        [ll_arm_releasing(A)],
        [],
        [],
        [ll_arm(A)],
        [
            del(ll_arm_releasing(A)),
            add(ll_gripper(A, open))
        ]
        ).

        ll_action(move_arm_start(A, X1, Y1, X2, Y2),
        [ll_arm_at(A, X1, Y1)],
        [ll_moving(_, _, _, X2, Y2), ll_moving(A, _, _, _, _)],
        [],
        [ll_arm(A), pos(X1, Y1), pos(X2, Y2)],
        [
            del(ll_arm_at(A, X1, Y1)),
            add(ll_moving(A, X1, Y1, X2, Y2))
        ]
        ).

        ll_action(move_arm_end(A, X1, Y1, X2, Y2),
        [ll_moving(A, X1, Y1, X2, Y2)],
        [],
        [],
        [ll_arm(A)],
        [
            del(ll_moving(A, X1, Y1, X2, Y2)),
            add(ll_arm_at(A, X2, Y2))
        ]
        ).
        ```
    ''',
    }
]

ll_actions_evaluation_data_samples = [
    { #Sample 1
    "natural_language":
    '''
        Provide the code of low-level actions, considering the list of low-level actions that can be performed by the agents:
        - move_arm_start(arm, x1, y1, x2, y2), which makes the robotic arm starting to move from position (x1,y1) to position (x2,y2).
        - move_arm_end(arm, x1, y1, x2, y2), which completes the movement of the robotic arm from position (x1,y1) to position (x2,y2).
        - grip_start(arm), which makes the gripper starting to close.
        - grip_end(arm), which indicates the gripper has closed.
        - release_start(arm), which makes the gripper starting to open.
        - release_end(arm), which indicates the gripper has opened.
    ''',
    "prolog_code":
    '''
        ```ll_actions
        ll_action(grip_start(A),
        [ll_gripper(A, open)],
        [ll_arm_gripping(A), ll_gripped(A)],
        [],
        [ll_arm(A)],
        [
            del(ll_gripper(A, open)),
            add(ll_arm_gripping(A))
        ]
        ).

        ll_action(grip_end(A),
        [ll_arm_gripping(A)],
        [],
        [],
        [ll_arm(A)],
        [
            del(ll_arm_gripping(A)),
            add(ll_gripper(A, closed))
        ]
        ).

        ll_action(release_start(A),
        [ll_gripper(A, closed)],
        [],
        [],
        [ll_arm(A)],
        [
            del(ll_gripper(A, closed)),
            add(ll_arm_releasing(A))
        ]
        ).

        ll_action(release_end(A),
        [ll_arm_releasing(A)],
        [],
        [],
        [ll_arm(A)],
        [
            del(ll_arm_releasing(A)),
            add(ll_gripper(A, open))
        ]
        ).

        ll_action(move_arm_start(A, X1, Y1, X2, Y2),
        [ll_arm_at(A, X1, Y1)],
        [ll_moving(_, _, _, X2, Y2), ll_moving(A, _, _, _, _)],
        [],
        [ll_arm(A), pos(X1, Y1), pos(X2, Y2)],
        [
            del(ll_arm_at(A, X1, Y1)),
            add(ll_moving(A, X1, Y1, X2, Y2))
        ]
        ).

        ll_action(move_arm_end(A, X1, Y1, X2, Y2),
        [ll_moving(A, X1, Y1, X2, Y2)],
        [],
        [],
        [ll_arm(A)],
        [
            del(ll_moving(A, X1, Y1, X2, Y2)),
            add(ll_arm_at(A, X2, Y2))
        ]
        ).
        ```
    ''',
    },
    { #Sample 2
    "natural_language":
    '''
        Define the low-level actions for a robot arm: move_arm, grip, and release.
        These actions are not instantaneous and need to be represented in a way suitable for a planner that handles durative or event-based actions.
        Consider how resources like the arm itself are managed during these operations.
    ''',
    "prolog_code":
    '''
        ```ll_actions
        ll_action(grip_start(A),
        [ll_gripper(A, open)],
        [ll_arm_gripping(A), ll_gripped(A)],
        [],
        [ll_arm(A)],
        [
            del(ll_gripper(A, open)),
            add(ll_arm_gripping(A))
        ]
        ).

        ll_action(grip_end(A),
        [ll_arm_gripping(A)],
        [],
        [],
        [ll_arm(A)],
        [
            del(ll_arm_gripping(A)),
            add(ll_gripper(A, closed))
        ]
        ).

        ll_action(release_start(A),
        [ll_gripper(A, closed)],
        [],
        [],
        [ll_arm(A)],
        [
            del(ll_gripper(A, closed)),
            add(ll_arm_releasing(A))
        ]
        ).

        ll_action(release_end(A),
        [ll_arm_releasing(A)],
        [],
        [],
        [ll_arm(A)],
        [
            del(ll_arm_releasing(A)),
            add(ll_gripper(A, open))
        ]
        ).

        ll_action(move_arm_start(A, X1, Y1, X2, Y2),
        [ll_arm_at(A, X1, Y1)],
        [ll_moving(_, _, _, X2, Y2), ll_moving(A, _, _, _, _)],
        [],
        [ll_arm(A), pos(X1, Y1), pos(X2, Y2)],
        [
            del(ll_arm_at(A, X1, Y1)),
            add(ll_moving(A, X1, Y1, X2, Y2))
        ]
        ).

        ll_action(move_arm_end(A, X1, Y1, X2, Y2),
        [ll_moving(A, X1, Y1, X2, Y2)],
        [],
        [],
        [ll_arm(A)],
        [
            del(ll_moving(A, X1, Y1, X2, Y2)),
            add(ll_arm_at(A, X2, Y2))
        ]
        ).
        ```
    ''',
    }
]