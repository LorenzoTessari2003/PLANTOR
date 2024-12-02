:- ensure_loaded('../conditions.pl').

:- dynamic planner_debug/1.
planner_debug(true).

% :-ensure_loaded('conditions_tests_kb.pl').
:- ensure_loaded('../../../output/kb.pl').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_last_achievers_ids_now :-
  last_achievers_ids(
    [ontable(b6),at(b6,1,1),available(a2),clear(b6)],
    [
      moving_block_table_to_table(_25918,b6,_25922,_25924), moving_block_table_to_block(_25934,b6,_25938,_25940,_25942,_25944,_25946),
      moving_block_on_to_table(_25956,b6,_25960,_25962,_25964,_25966), moving_block_on_to_block(_25976,b6,_25980,_25982,_25984,_25986,_25988)
    ],
    [pos(1,1), pos(2,2), block(b6), agent(a2)],
    [0-move_block_table_to_table_start(a1,b4,8,8,2,2)],
    B
  ),
  format('Finished ~w\n', [B]),
  true.

test(L):-
  length(L, Len),
  format('Maybe I need to use Len ~w\n', [Len]),
  append([], [Len], L1),
  (
    format('Trying to understand why Prolog sucks so much\n'),
    Len > 3 
    ->(
      format('List is longer than 3 ~w\n', [Len])
    );(
      format('List is not longer than 3 ~w\n', [Len])
    )
  ),
  format(L1).

% test_achievers_4 :-
%   achiever(
%     [av(a1),pillar(a,block1),pillar(b,block2),free(arch1)], 
%     [placing_arch(_19930,a,b,_19936),placed_arch(a,b,_19950)], 
%     [del(pillaring(a1,b,block2)),add(av(a1)),add(pillar(b,block2))], 
%     []
%   ),
%   true.

% test_achievers_3 :-
%   % trace,
%   achiever(
%     [av(a1),pillar(a,block1),pillar(b,block2),free(arch1)], 
%     [placing_arch(_19930,a,b,_19936),placed_arch(a,b,_19950)], 
%     build_pillar_end(a1, b, block2)
%   ),
%   true.

% test_achievers_5 :-
%   last_achievers_ids(
%     [av(a1),pillar(a,block1),pillar(b,block2),free(arch1)],
%     [],
%     [[1-build_pillar_end(a1, a, block1)], [2-build_pillar_end(a1, b, block2)]],
%     A
%   ),
%   format('A ~w~n', [A]).


% test_check_in_verify :- 
%   format('Running\n'),
%   check_in_verify(a1, [agent(a1),agent(a2)], agent(a1)),
%   format('1~n'),
%   check_in_verify(a2, [agent(a1),agent(a2)], agent(a2)),
%   format('2~n'),
%   \+check_in_verify(b, [agent(a2)], _),
%   format('3~n'),
%   \+check_in_verify(b, [block(b1), agent(a3), agent(a2)], _),
%   format('5~n'),
%   \+check_in_verify(a1, [], _),
%   format('6~n'),
%   check_in_verify(a1, [agent(a4), block(b1, a1)], block(b1, a1)),
%   format('7~n'),
%   check_in_verify(b2, [block(b2), pos(pos1, 1, 1, 1)], block(b2)),
%   format('8~n'),
%   true.

% test_not_in_resources :-
%   format('Running\n'),
%   \+not_in_resources([av(a1)], [agent(a1)], _),
%   format('1~n'), !,
%   \+not_in_resources([], [], _),
%   format('2~n'), !,
%   not_in_resources([block(b1)], [agent(a1)], _),
%   format('3~n'), !,
%   \+not_in_resources([av(a1), av(a2)], [agent(a1), agent(a2)], _),
%   format('4~n'), !,
%   \+not_in_resources([av(a2), av(a1)], [agent(a1), agent(a2)], _),
%   format('5~n'), !,
%   not_in_resources([tree(t2), av(a2)], [point(p2), agent(a2)], _),
%   format('6~n'), !,
%   not_in_resources([tree(t2)], [tree(t2)], _),
%   format('7~n'), !,
%   not_in_resources([av(a1), free(block2)], [agent(a1)], _),
%   format('8~n'), !,
%   % There is b1 which is not in the resources
%   not_in_resources([block(pos1, b1), free(block2)], [block(b2), pos(pos1, 1, 1, 1)], _),
%   format('9~n'),
%   % trace(not_in_resources),
%   \+not_in_resources([block(pos1, b2)], [block(b2), pos(pos1, 1, 1, 1)], _),
%   format('10~n'),
%   \+not_in_resources([av(a1)], [block(block2),agent(a1),pos(b,_8786,_8788,_8790)], agent(a1)),
%   format('11~n'),
%   true.

% test_last_achievers_ids :-
%   last_achievers_ids(
%     [av(a1),pillar(a,block1),free(arch1)],
%     [],
%     [agent(a1), block(b), block(a), arch(arch1)],
%     [[1-build_pillar_end(a1, a, block1)], [2-build_pillar_end(a1, b, block2)]],
%     [1]
%   ),
%   format('1~n'),

%   last_achievers_ids(
%     [av(a1),pillar(a1,block1),free(arch2)],
%     [],
%     [agent(a1), block(b), block(a), arch(arch1)],
%     [[1-build_pillar_end(a1, a, block1)], [2-build_pillar_end(a1, b, block2)]],
%     A
%   ),
%   format('2 ~w~n', [A]), 
%   true.

% test_achievers :- 
%   \+achiever(
%    [av(a1),free(block2)],
%    [pillar(b,_10558),pillaring(_10568,b,_10572)],
%    [block(block2),agent(a1),pos(b,_10616,_10618,_10620)],
%    [del(pillaring(a1,a,block1)),add(av(a1)),add(pillar(a,block1))],
%    []
%   ),
%   format('1~n'),
  
%   achiever(
%     [moving_arm(a1,a,0,0,0,1,1,0)],
%     [],
%     [arm(a1)],
%     [add(moving_arm(a1,a,0,0,0,1,1,0)),del(arm_at(a1,0,0,0))],
%     []
%   ),
%   format('2~n'),

%   \+achiever(
%     [av(a1),free(block2)],
%     [pillar(b,_6458),pillaring(_6468,b,_6472)],
%     [block(block2),agent(a1),pos(b,_6516,_6518,_6520)],
%     [add(av(a1)),add(pillar(a,block1))],
%     []
%   ),
%   format('3~n'),

% % Adding constraint that 12_move_arm_start(a1, a, 0, 0, 0, 1, 1, 0) starts after 5_grip_end(a1, block1)

%   \+achiever(
%     [arm_at(a1,0,0,0)],
%     [moving_arm(a1, _, _, _, _, _, _), gripping(a1, _), releasing(a1)],
%     [arm(a1),pos(a,1,1,0)],
%     [add(gripped(a1)),add(gripper(a1,close)),del(gripping(a1,block1))],
%     []
%   ),
%   format('4~n'),
%   true.

% test_apply_action :-
%   \+is_applicable(
%     [available(a1),available(a2),ontable(b1,2,2),ontable(b2,4,4),ontable(b4,8,8),on(b5,b1,2,2),on(b3,b2,4,4),clear(b5),clear(b3),clear(b4)],
%     [ontable(B, X, Y), available(A), clear(B)],
%     [gripped(_, B), gripping(_, B)],
%     [ontable(B, X, Y)],
%     [agent(A)]
%   ),
%   debug_format('B ~w X ~w Y ~w A ~w~n', [B, X, Y, A]),
  
%   is_applicable(
%     [available(a1),available(a2),ontable(b1,2,2),ontable(b2,4,4),ontable(b4,8,8),on(b5,b1,2,2),on(b3,b2,4,4),clear(b5),clear(b3),clear(b4)],
%     [on(B1, B2, X1, Y1), available(A1), clear(B1)],
%     [gripped(_, B1), gripping(_, B1)],
%     [on(B1, B2, X, Y)],
%     [agent(A1)]
%   ),
%   debug_format('B1 ~w B2 ~w X1 ~w Y1 ~w A1 ~w~n', [B1, B2, X1, Y1, A1]),
%   true.

% test_conditions_met :-
%   conditions_met(
%     [av(Agent),free(Arch)],
%     [av(a2),free(b1)]
%   ),
%   debug_format('Agent ~w Arch ~w~n', [Agent, Arch]),
%   \+conditions_met(
%     [av(Agent1),free(Arch1)],
%     [av(a2),block(b1)]
%   ),
%   which_conditions_not_met(
%     [av(Agent1),free(Arch1)],
%     [av(a2),block(b1)],
%     R
%   ),
%   debug_format('R ~w~n', [R]),
%   true.
