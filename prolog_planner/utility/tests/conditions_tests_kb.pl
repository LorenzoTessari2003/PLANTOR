% This is just the kb for the conditions_tests.pl file

action(build_pillar_start(A, Pos, Block1),
  [av(A), free(Block1)],
  [pillar(Pos, _), pillaring(_, Pos, _)],
  [pillar(Pos, Block1)],
  [block(Block1), agent(A), pos(Pos,_,_,_)],
  [
  del(av(A)), del(free(Block1)),
  add(pillaring(A, Pos, Block1))
  ]
).
action(build_pillar_end(A, Pos, Block1),
  [pillaring(A, Pos, Block1)],
  [pillar(Pos, _)],
  [],
  [agent(A), block(Block1), pos(Pos,_,_,_)],
  [
    del(pillaring(A, Pos, Block1)),
    add(av(A)), add(pillar(Pos, Block1))
  ]
).

ll_action(move_arm_start(A, To, X1, Y1, Z1, X2, Y2, Z2),
  [arm_at(A, X1, Y1, Z1)],
  [moving_arm(A, _, _, _, _, _, _), gripping(A, _), releasing(A)],
  [],
  [arm(A), pos(To, X2, Y2, Z2)],
  [
    add(moving_arm(A, To, X1, Y1, Z1, X2, Y2, Z2)),
    del(arm_at(A, X1, Y1, Z1))
  ]
).
ll_action(move_arm_end(A, To, X1, Y1, Z1, X2, Y2, Z2),
  [moving_arm(A, To, X1, Y1, Z1, X2, Y2, Z2)],
  [],
  [],
  [arm(A)],
  [
    del(moving_arm(A, To, X1, Y1, Z1, X2, Y2, Z2)),
    add(arm_at(A, X2, Y2, Z2))
  ]
).

planner_debug(true).

agent(a1).
agent(a2).

arm(a1, 10, 10, 10).
arm(a2, 20, 20, 20).

arm(a1).

gripper(a1).
gripper(a2).

new_agent(a1, a2, a3).
new_agent(a2, a3, a7).

pos(pos1, 1, 2, 3).

block(b2).

resources :- resources(X), X, functor(X, Y, _), write(X), write(' '), write(Y), nl.
resources(new_agent(_, _, _)).
resources(agent(_)).
resources(arm(_, _, _, _)).
resources(gripper(_)).
resources(pos(_, _, _, _)).
resources(block(_)).
resources(arm(_)).
