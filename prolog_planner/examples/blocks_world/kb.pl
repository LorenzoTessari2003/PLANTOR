:-discontiguous pos/2.
:-discontiguous block/1.
:-discontiguous agent/1.

:- discontiguous resources/1.

pos(0,0).
pos(1,1).
pos(2,2).
pos(3,3).
pos(4,4).
% pos(5,5).
% pos(6,6).

block(b1).
block(b2).
block(b3).
block(b4).
block(b5).
% block(b6).

agent(a1).
agent(a2).
% agent(a3).

ll_arm(a1).
ll_arm(a2).

resources(agent(_)).
resources(ll_arm(_)).