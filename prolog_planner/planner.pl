:- ensure_loaded('full_planner.pl').

time_plan_hl(MaxDepth) :-
  time(plan_hl(MaxDepth)).
  % format('Time: ~w~n', [D]).

time_plan_ll(MaxDepth) :-
  time(plan_ll(MaxDepth)).
  % format('Time: ~w~n', [D]).

plan_hl(MaxDepth):-
  disable_debug,
  init_state(Init),
  goal_state(Goal),
  format('Planning from: ~w to: ~w~n', [Init, Goal]),
  generate_plan_hl(Init, Goal, [], [], MaxDepth, Actions),
  % generate_plan(Init, Goal, Actions, _, MaxDepth),
  format('Plan: ~n'),
  reverse(Actions, ActionsReversed),
  print_list(ActionsReversed).

plan_ll(MaxDepth):-
  disable_debug,
  init_state(Init),
  goal_state(Goal),
  debug_format('Planning from: ~w to: ~w~n', [Init, Goal]),
  generate_plan(Init, Goal, Actions, LastAchievers, MaxDepth),
  reverse(Actions, ActionsReversed),
  format('Plan: ~n'),
  print_list(ActionsReversed),
  reverse(LastAchievers, LastAchieversReversed),
  format('\nAchievers: ~n'),
  print_list(LastAchieversReversed).


plan(Actions, AdjMatrix, TTActionList, Resources, ActionXResources, ResourcesList, LLActions) :-
  plan(6, Actions, AdjMatrix, TTActionList, Resources, ActionXResources, ResourcesList, LLActions).

plan(PlanLength, Actions, AdjMatrix, TTActionList, Resources, ActionXResources, ResourcesList, LLActions) :-
  disable_debug,
  init_state(Init),
  goal_state(Goal),
  % trace(apply_action_map),
  format('Planning from: ~w to: ~w~n', [Init, Goal]),
  % leash(-all), etrace,
  % extract_hl_goal(Goal, HLGoal),
  generate_plan(Init, Goal, TOActions, LastAchievers, PlanLength),
  format('Total-order plan: ~n'),
  reverse(TOActions, TOActionsReversed),
  % print_list(TOActionsReversed),
  format('Last achievers: ~n'),
  reverse(LastAchievers, LastAchieversReversed),
  % print_list(LastAchieversReversed),
  nl,nl,nl,
  
  extract_adj_matrix_actions(LastAchievers, AdjMatrix, Actions),
  debug_format('Adjacency matrix:~n'),
  % print_list(AdjMatrix),
  debug_format('Actions:~n'),
  % print_list(Actions),
  nl,nl,nl,

  extract_tt_action_list(Actions, TTActionList),
  debug_format('Time-triggered actions:~n'),
  % print_list(TTActionList),
  nl,nl,nl,

  extract_ll_actions(LLActions),
  debug_format('Low-level Action:~n'),
  % print_list(LLActions),
  nl,nl,nl,

  extract_resources_number(Resources),
  debug_format('Resources:~n'),
  % print_list(Resources),
  nl,nl,nl,

  extract_resources_list(ResourcesList),
  debug_format('Resources list:~n'),
  % print_list(ResourcesList),
  nl,nl,nl,

  extract_resources_per_action(TTActionList, Resources, ActionXResources),
  debug_format('Resources per action:~n'),
  % print_list(ActionXResources),
  nl,nl,nl,

  format('Finished planning.~n'),

  true.

plan(_PlanLength, _Actions, _AdjMatrix, _TTActionList, _Resources, _ActionXResources, _ResourcesList, _LLActionsList) :-
  init_state(Init),
  goal_state(Goal),
  % trace(apply_action_map),
  debug_format('Planning from: ~w to: ~w~n', [Init, Goal]),
  % leash(-all), etrace,
  % extract_hl_goal(Goal, HLGoal),
  \+generate_plan(Init, Goal, TOActions, LastAchievers),
  format('Could not generate TO plan').

plan :-
  disable_debug,
  plan(_, _, _, _, _, _, _).  


plan(PlanLength) :-
  disable_debug,
  plan(PlanLength, _, _, _, _, _, _, _).  


