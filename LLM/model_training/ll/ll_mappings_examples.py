ll_mappings_training_data_samples = [
    { #Sample 1
    "natural_language":
    '''
        Consider the following high-level action:
            ```actions
            % Move Block1 from (X1,Y1) on the table to the top of block Block2 in (X2,Y2). Notice that we are not removing the predicates for Block2 yet as the action is not concluded yet.
            action(move_table_to_block_start(Agent, Block1, Block2, X1, Y1, X2, Y2),
            [available(Agent), ontable(Block1), at(Block1, X1, Y1), at(Block2, X2, Y2), clear(Block2), clear(Block1)],
            [on(_, Block1), on(Block1, _), moving_table_to_table(_, Block, _, _, _, _), moving_table_to_block(_, Block, _, _, _, _, _)],
            [],
            [agent(Agent), pos(X1, Y1), pos(X2, Y2), block(Block1), block(Block2), Block1 != Block2],
            [
                del(available(Agent)), del(clear(Block1)), del(ontable(Block1)), del(at(Block1, X1, Y1)),
                add(moving_table_to_block(Agent, Block1, Block2, X1, Y1, X2, Y2))
            ]
            ).
            ```
        And consider the following low-level actions:
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
        Provide a mapping between the high-level action and the low-level actions.
    ''',
    "prolog_code":
    '''
        ```mappings
        mapping(move_table_to_block_start(Agent, _Block1, _Block2, X1, Y1, X2, Y2),
        [
            move_arm_start(Agent, _, _, X1, Y1),
            move_arm_end(Agent, _, _, X1, Y1),
            grip_start(Agent),
            grip_end(Agent),
            move_arm_start(Agent, X1, Y1, X2, Y2),
            move_arm_end(Agent, X1, Y1, X2, Y2),
            release_start(Agent),
            release_end(Agent)
        ]
        ).
        ```
    ''',
    },
    { #Sample 2
    "natural_language":
    f'''
        Consider the following high-level action:
            ```actions
            % Move a block from a position (X1,Y1) to another position (X2,Y2) on the table. 
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
            ```
        And consider the following low-level actions:
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
        Provide a mapping between the high-level action and the low-level actions.
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
    ''',
    },
    { #Sample 3
    "natural_language":
    f'''
        Consider the following high-level action:
            ```actions
            % Move Block1 from (X1,Y1) on top of another block to the table in (X2,Y2).
            action(move_onblock_to_table_start(Agent, Block1, X1, Y1, X2, Y2),
            [available(Agent), on(Block1, Block2), at(Block1, X1, Y1), at(Block2, X1, Y1), clear(Block1)],
            [moving_onblock_to_table(_, Block1, _, _, _, _), on(_, Block1), ontable(Block1), at(_, X2, Y2)],
            [],
            [agent(Agent), pos(X1, Y1), pos(X2, Y2), block(Block1), block(Block2), Block1 != Block2],
            [
                del(available(Agent)), del(clear(Block1)), del(on(Block1, Block2)), del(at(Block1, X1, Y1)),
                add(moving_onblock_to_table(Agent, Block1, X1, Y1, X2, Y2)), add(clear(Block2))
            ]
            ).
            ```
        And consider the following low-level actions:
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
        Provide a mapping between the high-level action and the low-level actions.
    ''',
    "prolog_code":
    '''
        ```mappings
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
        ```
    ''',
    },
    { #Sample 4
    "natural_language":
    '''
        Consider the following high-level action:
            ```actions
            % Move Block1 from (X1,Y1) on top of another block to the top of block Block2 in (X2,Y2). Notice that we are not removing the predicates for Block2 yet as the action is not concluded yet.
            action(move_onblock_to_block_start(Agent, Block1, Block2, X1, Y1, X2, Y2),
            [available(Agent), on(Block1, Block3), at(Block1, X1, Y1), at(Block2, X2, Y2), clear(Block2), clear(Block1)],
            [moving_onblock_to_block(_, Block1, _, _, _, _), on(_, Block1), ontable(Block1)],
            [],
            [agent(Agent), pos(X1, Y1), pos(X2, Y2), block(Block1), block(Block2), block(Block3), Block1 != Block2, Block1 != Block3, Block2 != Block3],
            [
                del(available(Agent)), del(clear(Block1)), del(on(Block1, Block3)), del(at(Block1, X1, Y1)),
                add(moving_onblock_to_block(Agent, Block1, Block2, X1, Y1, X2, Y2)), add(clear(Block3))
            ]
            ).
            ```
        And consider the following low-level actions:
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
        Provide a mapping between the high-level action and the low-level actions.
    ''',
    "prolog_code":
    '''
        ```mappings
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
    ''',
    },
    { #Sample 5
    "natural_language":
    '''
        Consider the following high-level action:
            ```actions
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
            ```
        And consider the following low-level actions:
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
        Provide a mapping between the high-level action and the low-level actions.
    ''',
    "prolog_code":
    '''
        This _end action does not produce a mapping to low-level actions;
        its function is solely to reflect changes in the high-level state.
    ''',
    },
    { #Sample 6
    "natural_language":
    '''
        Consider the following high-level action:
            ```actions
            action(move_table_to_table_end(Agent, Block, X1, Y1, X2, Y2),
            [moving_table_to_table(Agent, Block, X1, Y1, X2, Y2)],
            [at(X2, Y2)],
            [],
            [agent(Agent)],
            [
                del(moving_table_to_table(Agent, Block, X1, Y1, X2, Y2)),
                add(ontable(Block)), add(at(Block, X2, Y2)), add(clear(Block)), add(available(Agent))
            ]
            ).
            ```
        And consider the following low-level actions:
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
        Provide a mapping between the high-level action and the low-level actions.
    ''',
    "prolog_code":
    '''
        This _end action does not produce a mapping to low-level actions;
        its function is solely to reflect changes in the high-level state.
    ''',
    },
    { #Sample 7
    "natural_language":
    '''
        Consider the following high-level action:
            ```actions
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
        And consider the following low-level actions:
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
        Provide a mapping between the high-level action and the low-level actions.
    ''',
    "prolog_code":
    '''
        This _end action does not produce a mapping to low-level actions;
        its function is solely to reflect changes in the high-level state.
    ''',
    },
    { #Sample 8
    "natural_language":
    '''
        Consider the following high-level action:
            ```actions
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
            ```
        And consider the following low-level actions:
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
        Provide a mapping between the high-level action and the low-level actions.
    ''',
    "prolog_code":
    '''
        This _end action does not produce a mapping to low-level actions;
        its function is solely to reflect changes in the high-level state.
    ''',
    },
    { #Sample 9
    "natural_language":
    '''
        Consider the following high-level actions:
            ```actions
            % Move Block1 from (X1,Y1) on the table to the top of block Block2 in (X2,Y2). Notice that we are not removing the predicates for Block2 yet as the action is not concluded yet.
            action(move_table_to_block_start(Agent, Block1, Block2, X1, Y1, X2, Y2),
            [available(Agent), ontable(Block1), at(Block1, X1, Y1), at(Block2, X2, Y2), clear(Block2), clear(Block1)],
            [on(_, Block1), on(Block1, _), moving_table_to_table(_, Block, _, _, _, _), moving_table_to_block(_, Block, _, _, _, _, _)],
            [],
            [agent(Agent), pos(X1, Y1), pos(X2, Y2), block(Block1), block(Block2), Block1 != Block2],
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
            % Move a block from a position (X1,Y1) to another position (X2,Y2) on the table. 
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
            [at(X2, Y2)],
            [],
            [agent(Agent)],
            [
                del(moving_table_to_table(Agent, Block, X1, Y1, X2, Y2)),
                add(ontable(Block)), add(at(Block, X2, Y2)), add(clear(Block)), add(available(Agent))
            ]
            ).
            % Move Block1 from (X1,Y1) on top of another block to the table in (X2,Y2).
            action(move_onblock_to_table_start(Agent, Block1, X1, Y1, X2, Y2),
            [available(Agent), on(Block1, Block2), at(Block1, X1, Y1), at(Block2, X1, Y1), clear(Block1)],
            [moving_onblock_to_table(_, Block1, _, _, _, _), on(_, Block1), ontable(Block1), at(_, X2, Y2)],
            [],
            [agent(Agent), pos(X1, Y1), pos(X2, Y2), block(Block1), block(Block2), Block1 != Block2],
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
            % Move Block1 from (X1,Y1) on top of another block to the top of block Block2 in (X2,Y2). Notice that we are not removing the predicates for Block2 yet as the action is not concluded yet.
            action(move_onblock_to_block_start(Agent, Block1, Block2, X1, Y1, X2, Y2),
            [available(Agent), on(Block1, Block3), at(Block1, X1, Y1), at(Block2, X2, Y2), clear(Block2), clear(Block1)],
            [moving_onblock_to_block(_, Block1, _, _, _, _), on(_, Block1), ontable(Block1)],
            [],
            [agent(Agent), pos(X1, Y1), pos(X2, Y2), block(Block1), block(Block2), block(Block3), Block1 != Block2, Block1 != Block3, Block2 != Block3],
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
        And consider the following low-level actions:
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
        Provide a set of mappings between the high-level actions and the low-level actions.
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
        The _end actions do not produce a mapping to low-level actions;
        their function is solely to reflect changes in the high-level state.
    ''',
    },
]

ll_mappings_evaluation_data_samples = [
    { #Sample 1
    "natural_language":
    '''
        Consider the following high-level actions:
            ```actions
            % Move Block1 from (X1,Y1) on the table to the top of block Block2 in (X2,Y2). Notice that we are not removing the predicates for Block2 yet as the action is not concluded yet.
            action(move_table_to_block_start(Agent, Block1, Block2, X1, Y1, X2, Y2),
            [available(Agent), ontable(Block1), at(Block1, X1, Y1), at(Block2, X2, Y2), clear(Block2), clear(Block1)],
            [on(_, Block1), on(Block1, _), moving_table_to_table(_, Block, _, _, _, _), moving_table_to_block(_, Block, _, _, _, _, _)],
            [],
            [agent(Agent), pos(X1, Y1), pos(X2, Y2), block(Block1), block(Block2), Block1 != Block2],
            [
                del(available(Agent)), del(clear(Block1)), del(ontable(Block1)), del(at(Block1, X1, Y1)),
                add(moving_table_to_block(Agent, Block1, Block2, X1, Y1, X2, Y2))
            ]
            ).
            % Move a block from a position (X1,Y1) to another position (X2,Y2) on the table. 
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
            % Move Block1 from (X1,Y1) on top of another block to the table in (X2,Y2).
            action(move_onblock_to_table_start(Agent, Block1, X1, Y1, X2, Y2),
            [available(Agent), on(Block1, Block2), at(Block1, X1, Y1), at(Block2, X1, Y1), clear(Block1)],
            [moving_onblock_to_table(_, Block1, _, _, _, _), on(_, Block1), ontable(Block1), at(_, X2, Y2)],
            [],
            [agent(Agent), pos(X1, Y1), pos(X2, Y2), block(Block1), block(Block2), Block1 != Block2],
            [
                del(available(Agent)), del(clear(Block1)), del(on(Block1, Block2)), del(at(Block1, X1, Y1)),
                add(moving_onblock_to_table(Agent, Block1, X1, Y1, X2, Y2)), add(clear(Block2))
            ]
            ).
            % Move Block1 from (X1,Y1) on top of another block to the top of block Block2 in (X2,Y2). Notice that we are not removing the predicates for Block2 yet as the action is not concluded yet.
            action(move_onblock_to_block_start(Agent, Block1, Block2, X1, Y1, X2, Y2),
            [available(Agent), on(Block1, Block3), at(Block1, X1, Y1), at(Block2, X2, Y2), clear(Block2), clear(Block1)],
            [moving_onblock_to_block(_, Block1, _, _, _, _), on(_, Block1), ontable(Block1)],
            [],
            [agent(Agent), pos(X1, Y1), pos(X2, Y2), block(Block1), block(Block2), block(Block3), Block1 != Block2, Block1 != Block3, Block2 != Block3],
            [
                del(available(Agent)), del(clear(Block1)), del(on(Block1, Block3)), del(at(Block1, X1, Y1)),
                add(moving_onblock_to_block(Agent, Block1, Block2, X1, Y1, X2, Y2)), add(clear(Block3))
            ]
            ).
            ```
        And consider the following low-level actions:
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
        Provide a set of mappings between the high-level actions and the low-level actions.
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
    ''',
    },
    { #Sample 2
    "natural_language":
    '''
        Consider the following high-level actions:
            ```actions
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
            action(move_table_to_table_end(Agent, Block, X1, Y1, X2, Y2),
            [moving_table_to_table(Agent, Block, X1, Y1, X2, Y2)],
            [at(X2, Y2)],
            [],
            [agent(Agent)],
            [
                del(moving_table_to_table(Agent, Block, X1, Y1, X2, Y2)),
                add(ontable(Block)), add(at(Block, X2, Y2)), add(clear(Block)), add(available(Agent))
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
        And consider the following low-level actions:
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
        Provide a set of mappings between the high-level actions and the low-level actions.
    ''',
    "prolog_code":
    '''
        The _end actions do not produce a mapping to low-level actions;
        their function is solely to reflect changes in the high-level state.
    ''',
    },
]