%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                     HL                                     %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
action(build_pillar_start(A, Pos, Block1),
  [av(A), free(Block1)],
  [pillar(Pos, _), pillaring(_, Pos, _), arch(_, _, _, Pos)],
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
action(place_arch_start(A, Pos1, Pos2, Arch, To),
  [av(A), pillar(Pos1, _), pillar(Pos2, _), free(Arch)],
  [
    pillar(To, _), 
    placing_arch(_, Pos1, _, _), placing_arch(_, _, Pos1, _), placing_arch(_, _, _, Pos1), 
    placing_arch(_, Pos2, _, _), placing_arch(_, _, Pos2, _), placing_arch(_, _, _, Pos2), 
    placing_arch(_, To, _, _), placing_arch(_, _, To, _), placing_arch(_, _, _, To), 
    arch(Pos1, _, _, _), arch(_, Pos1, _, _), arch(_, _, _, Pos1),
    arch(Pos2, _, _, _), arch(_, Pos2, _, _), arch(_, _, _, Pos2),
    arch(To, _, _, _), arch(_, To, _, _), arch(_, _, _, To)
  ],
  [arch(Pos1, Pos2, Arch, To)],
  [arch(Arch), agent(A), pos(Pos1,_,_,_), pos(Pos2,_,_,_), Pos1\=Pos2, pos(To,_,_,_)],
  [
    del(av(A)), del(free(Arch)),
    add(placing_arch(A, Pos1, Pos2, Arch, To))
  ]
).
action(place_arch_end(A, Pos1, Pos2, Arch, To),
  [placing_arch(A, Pos1, Pos2, Arch, To)],
  [arch(_, Pos1, Pos2)],
  [],
  [],
  [
    del(placing_arch(A, Pos1, Pos2, Arch, To)),
    add(arch(Pos1, Pos2, Arch, To)), add(av(A))
  ]
).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                    INT                                     %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ll_action(move_block_start(A, Block, From, To),
%   [at(From, Block)],
%   [at(To, _)],
%   [at(To, Block)],
%   [],
%   [
%     del(at(From, Block)),
%     add(moving_block(A, Block, From, To))
%   ]
% ).
% ll_action(move_block_end(A, Block, From, To),
%   [moving_block(A, Block, From, To)],
%   [at(To, _)],
%   [],
%   [],
%   [
%     del(moving_block(A, Block, From, To)),
%     add(at(To, Block))
%   ]
% ).
% ll_action(move_arch_start(A, Arch, From, To, Pos1, Pos2),
%   [at(From, Arch)],
%   [at(To, _)],
%   [at(To, Arch)],
%   [
%     % pos(Pos1, X1, Y1, Z1),pos(Pos2, X2, Y2, Z2),
%     % Xf is (X1+X2)/2.0, Yf is (Y1+Y2)/2.0, Zf is Z1,
%     % format('X ~w Y ~w Z ~w~n', [Xf, Yf, Zf]),
%     % \+pos(_, Xf, Yf, Zf) -> assertz(pos(To, Xf, Yf, Zf)) ; true
%   ],
%   [
%     del(at(From, Arch)),
%     add(moving_arch(A, Arch, From, To))
%   ]
% ).
% ll_action(move_arch_end(A, Arch, From, To, _Pos1, _Pos2),
%   [moving_arch(A, Arch, From, To)],
%   [at(To, _)],
%   [],
%   [],
%   [
%     del(moving_arch(A, Arch, From, To)),
%     add(at(To, Arch))
%   ]
% ).


% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %%                                     LL                                     %%
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
ll_action(grip_start(A, B),
  [gripper(A, open)],
  [moving_arm(A, _, _, _, _, _, _), gripping(A, _), releasing(A), gripped(A)],
  [],
  [gripper(A), arm(A)],
  [
    del(gripper(A, open)),
    add(gripping(A, B))
  ]
).
ll_action(grip_end(A, B),
  [gripping(A, B)],
  [],
  [],
  [gripper(A), arm(A)],
  [
    del(gripping(A, B)),
    add(gripped(A)), add(gripper(A, close))
  ]
).
ll_action(release_start(A),
  [gripped(A)],
  [moving_arm(A, _, _, _, _, _, _), gripping(A, _), releasing(A)],
  [],
  [gripper(A), arm(A)],
  [
    del(gripped(A)), del(gripper(A, close)),
    add(releasing(A))
  ]
).
ll_action(release_end(A),
  [releasing(A)],
  [],
  [],
  [gripper(A), arm(A)],
  [
    del(releasing(A)),
    add(gripper(A, open))
  ]
).
