:- ensure_loaded('utility/utility.pl').
:- ensure_loaded('includes.pl').

:- dynamic map/2.

goal_reached(State, Goal) :-
  subset(Goal, State).
  % equal_sets(Goal, State).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This function applies the mappings of an action. It also checks that the ll action is applicable and changes the state accordingly 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
apply_mappings(Init, HL_Plan, LL_Plan) :-
  debug_format('[apply_mappings1] The HL plan is ~w\n', [HL_Plan]),
  apply_mappings(Init, HL_Plan, [], LL_Plan).
  

apply_mappings(_, [], LL_Plan, LL_Plan) :-
  debug_format('[apply_mappings2] Reached this point ~w\n', [LL_Plan]),
  true.


apply_mappings(State, [[IDHLAction-HL_Action]|T_HL_Actions], Plan, RetPlan) :-
  debug_format('\n\n[apply_mappings3] HL_Action: ~w-~w\n', [IDHLAction, HL_Action]), 
  
  length(Plan, Length),
  action(HL_Action, PreconditionsT, PreconditionsF, _FinalConditionsF, Verify, Effects),
  append([Length-HL_Action], Plan, TempPlan),
  change_state(State, Effects, CurrentState),

  debug_format('[apply_mappings3] Finding last achievers for: ~w\n', [HL_Action]),
  debug_format('[apply_mappings3] PreconditionsT: ~w\n', [PreconditionsT]),
  debug_format('[apply_mappings3] PreconditionsF: ~w\n', [PreconditionsF]),
  debug_format('[apply_mappings3] Verify: ~w\n', [Verify]),
  debug_format('[apply_mappings3] TempPlan: ~w\n', [TempPlan]),
  debug_format('[apply_mappings3] CurrentState: ~w\n', [CurrentState]),

  Pre = '\t',
  (
    debug_format('[apply_mappings3] Checking if there is a mapping for action ~w\n', [HL_Action]),
    mapping(HL_Action, Mappings) 
    ->(      
      debug_format('[apply_mappings3] Found mapping for action ~w ~w ~w\n', [HL_Action, Mappings, Length]),
      debug_format('[apply_mappings3] Calling apply_action_map with'),
      debug_format('[apply_mappings3] Mappings: ~w\n', [Mappings]), 
      debug_format('[apply_mappings3] Length: ~w\n', [Length]), 
      debug_format('[apply_mappings3] CurrentState: ~w\n', [CurrentState]), 
      debug_format('[apply_mappings3] TempPlan: ~w\n', [TempPlan]),

      apply_action_map(Mappings, Length, CurrentState, TempPlan, NewState, NewPlan, Pre),
      debug_format('[apply_mappings3] Action name: ~w\n', [HL_Action]),
      debug_format('[apply_mappings3] Current state: ~w\n', [NewState]),
      debug_format('[apply_mappings3] NewPlan: ~w\n', [NewPlan]),
      true
    );(
      debug_format('[apply_mappings3] No mappings for action ~w\n', [HL_Action]),
      NewState = CurrentState,
      NewPlan = TempPlan,
      debug_format('[apply_mappings3] Action name: ~w\n', [HL_Action]),
      debug_format('[apply_mappings3] Current state: ~w\n', [NewState]),
      debug_format('[apply_mappings3] NewPlan: ~w\n', [TempPlan]),
      true
    )
  ),

  debug_format('[apply_mappings3] Applying next action ~w\n', [T_HL_Actions]),

  apply_mappings(NewState, T_HL_Actions, NewPlan, RetPlan).


apply_mappings(_, _, _, _, _) :-
  debug_format('[apply_mappings5] Could not apply mappings\n'),
  fail.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This function applies the mappings of an action. It also checks that the ll action is applicable and changes the state accordingly 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
apply_action_map([], _IDHLAction, State, Plan, State, Plan, _).
apply_action_map([HAction|TActions], IDHLAction, State, Plan, RetState, RetPlan, Pre) :-
  % disable_debug,
  debug_format('\n~w[apply_action_map] Adding map to ~w ~w\n', [Pre, HAction, State]),
  ll_action(HAction, PreconditionsT, PreconditionsF, FinalConditionsF, Verify, Effects),
  debug_format('~w[apply_action_map] found action ~w ~w ~w ~w ~w ~w \n', [Pre, HAction, PreconditionsT, PreconditionsF, FinalConditionsF, Verify, Effects]),
  disable_debug,
  (
    is_applicable(State, PreconditionsT, PreconditionsF, FinalConditionsF, Verify)
    -> (
      debug_format('~w[apply_action_map] applicable ~w\n', [Pre, HAction]),
      length(Plan, Length),

      % NewPre is the concatenation of Pre and a tab
      format(atom(NewPre), '\t~w', [Pre]),

      stack(Length-HAction, Plan, NewPlan),
      debug_format('~w[apply_action_map] NewPlan: ~w\n', [Pre, NewPlan]),
      % Change state.
      change_state(State, Effects, NewState),
      debug_format('~w[apply_action_map] changed to ~w\n', [Pre, NewState]),
      (
        mapping(HAction, Mappings)
        ->(
          debug_format('~w[apply_action_map] Found mapping for action ~w ~w\n', [Pre, HAction, Mappings]),
          append(Mappings, TActions, NewActionList),
          apply_action_map(NewActionList, Length, NewState, NewPlan, RetState, RetPlan, NewPre)
        );(
          debug_format('~w[apply_action_map] No mappings for action ~w\n', [Pre, HAction]),
          debug_format('~w[apply_action_map] Applying next action\n', [Pre]), 
          debug_format('~w[apply_action_map] TActions: ~w\n', [Pre, TActions]),
          debug_format('~w[apply_action_map] IDHLAction: ~w\n', [Pre, IDHLAction]),
          debug_format('~w[apply_action_map] NewState: ~w\n', [Pre, NewState]),
          debug_format('~w[apply_action_map] NewPlan: ~w\n', [Pre, NewPlan]),
          apply_action_map(TActions, Length, NewState, NewPlan, RetState, RetPlan, Pre)
        )
      ),
      true
    );(
      debug_format('~w[apply_action_map] Not applicable ~w\n', [Pre, HAction]),
      why_not_applicable(State, PreconditionsT, PreconditionsF, FinalConditionsF, Verify),
      fail
    )
  ),
  true
  .

apply_action_map(_, _, _, _, _, _, _, _, _) :-
  debug_format('[apply_action_map] Could not apply action map\n'),
  fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This function generates a plan
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
generate_plan(Init, Goal, Plan, Enablers) :-
  generate_plan(Init, Goal, Plan, Enablers, 4).

generate_plan(Init, Goal, Plan, Enablers, MaxDepth) :-
  % enable_debug,
  debug_format('Checking if the initial state is the goal state ~w ~w\n', [Init, Goal]),
  goal_reached(Init, Goal),
  debug_format('Goal reached\n').

generate_plan(Init, Goal, Plan, Enablers, MaxDepth) :-
  % enable_debug,
  debug_format('Generating the high-level temporal plan from ~w to ~w\n', [Init, Goal]),
  (
    generate_plan_hl(Init, Goal, [], [], MaxDepth, HL_Plan)
    ->(
      % Print information on the high-level part
      debug_format('High-level plan generated\n~w\n', [HL_Plan]),
      
      debug_format('Applying the mappings to obtain the low-level temporal plan\n'),
      reverse(HL_Plan, HL_PlanReversed),
      (
        apply_mappings(Init, HL_PlanReversed, Plan)
        ->(
          debug_format('Plan generated~w\n', [Plan]),
          (
            % reverse(Plan, PlanReversed),
            extract_achievers(Plan, EnablersD)
            ->(
              debug_format('Achievers found ~w\n', [EnablersD]),
              clean_achievers(EnablersD, Plan, Enablers),
              debug_format('Cleaned achievers\n'),
              true
            );(
              format('Could not extract achievers\n'), fail
            )
          )
        );(
          format('Could not apply mappings\n'), fail
        )
      ),
      true
    );(
      format('Could not generate a HL plan\n'), fail
    )
  ),
  true.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This function extracts the high-level achievers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_ll_enablers([ID-Action|TActions], LL_Achievers):-
  add_ll_enablers([ID-Action|TActions], [], LL_Achievers, 0).

add_ll_enablers([_ID-HAction|_TActions], LL_Achievers, LL_Achievers, 1) :-
  action(HAction, _, _, _, _, _).
add_ll_enablers([_ID-HAction|TActions], TempLL_Achievers, LL_Achievers, 0) :-
  action(HAction, _, _, _, _, _),
  add_ll_enablers(TActions, TempLL_Achievers, LL_Achievers, 0).
add_ll_enablers([ID-HAction|TActions], TempLL_Achievers, LL_Achievers, _) :-
  ll_action(HAction, _, _, _, _, _),
  append([ID], TempLL_Achievers, NewTempLL_Achievers),
  add_ll_enablers(TActions, NewTempLL_Achievers, LL_Achievers, 1).


add_hl_to_ll_enablers(ID, PassedPlanActions, AchieversD, RetAchieversD) :-
  debug_format('\t[add_hl_to_ll_enablers0]\n\t\tID: ~w\n\t\tUsed: ~w\n\t\tRetAchieversD: ~w\n', [ID, PassedPlanActions, RetAchieversD]),
  add_hl_to_ll_enablers(ID, PassedPlanActions, [], AchieversD, RetAchieversD, 0).

add_hl_to_ll_enablers(HL_ID, [LL_ID-HAction|TActions], Used, AchieversD, RetAchieversD, 0) :-
  action(HAction, _, _, _, _, _),
  debug_format('\t[add_hl_to_ll_enablers2] Action ~w is HL -> continuing\n', [HAction]),
  add_hl_to_ll_enablers(HL_ID, TActions, Used, AchieversD, RetAchieversD, 0).

add_hl_to_ll_enablers(HL_ID, [LL_ID-HAction|TActions], Used, AchieversD, RetAchieversD, _) :-
  ll_action(HAction, _, _, _, _, _),
  debug_format('\t[add_hl_to_ll_enablers3] Action ~w is LL -> changing\n', [HAction]),
  OldAchievers = AchieversD.get(LL_ID, []),
  append([HL_ID|Used], OldAchievers, NewAchievers),
  NewAchieversD = AchieversD.put(LL_ID, NewAchievers),
  append([LL_ID], Used, NewUsed),
  debug_format('\t[add_hl_to_ll_enablers3] Calling with\n\t\tHL_ID ~w\n\t\tTActions ~w\n\t\tNewUsed: ~w\n\t\tNewAchieversD ~w\n\t\tRetAchieversD ~w\n\t\t1\n', [HL_ID, TActions, NewUsed, NewAchieversD, RetAchieversD]),
  add_hl_to_ll_enablers(HL_ID, TActions, NewUsed, NewAchieversD, RetAchieversD, 1).

add_hl_to_ll_enablers(_HL_ID, [_ID-HAction|_TActions], _Used, AchieversD, AchieversD, 1):-
  action(HAction, _, _, _, _, _),
  debug_format('\t[add_hl_to_ll_enablers1] Action ~w is -> stopping\n', [HAction]).

extract_achievers(Plan, AchieversD):-
  debug_format('Extracting all achievers for plan~n'),
  extract_achievers(Plan, [], achieversD{}, AchieversD),
  debug_format('Achievers found ~w~n', [AchieversD]).

extract_achievers([], _, AchieversD, AchieversD).

extract_achievers([ID-HAction|TActions], Used, AchieversD, RetAchieversD) :-
  action(HAction, PreconditionsT, PreconditionsF, FinalConditionsF, Verify, Effects),
  functor(HAction, ActionName, _),
  sub_string(ActionName, _, _, _, '_end'),
  debug_format('\n[extract_achievers-2]action [~w][~w] is END and HL~n', [ID, HAction]),
  (
    \+(_ = AchieversD.get(ID))
    ->(
      TempAchieversD = AchieversD.put(ID, [])
    );(
      TempAchieversD = AchieversD
    )
  ),
  % Take old achievers because one can never be too sure
  OldAchievers = TempAchieversD.get(ID),
  debug_format('[extract_achievers-2]Old achievers ~w\n', [OldAchievers]),

  % Find the achievers for this action
  last_achievers_ids(PreconditionsT, PreconditionsF, Verify, TActions, HL_Achievers),
  debug_format('[extract_achievers-2]HL achievers ~w\n', [HL_Achievers]),
  append(OldAchievers, HL_Achievers, NewAchievers),
  debug_format('[extract_achievers-2]New achievers ~w\n', [NewAchievers]),

  % Add the low-level actions as achievers
  add_ll_enablers(TActions, LL_Achievers),
  debug_format('[extract_achievers-2]LL achievers ~w\n', [LL_Achievers]),

  append(NewAchievers, LL_Achievers, NewAchievers2),
  debug_format('[extract_achievers-2]New achievers2 ~w\n', [NewAchievers2]),

  % Set the new correct achievers for the action
  NewAchieversD = TempAchieversD.put(ID, NewAchievers2),

  append([ID-HAction], Used, NewUsed),
  extract_achievers(TActions, NewUsed, NewAchieversD, RetAchieversD).

extract_achievers([ID-HAction|TActions], Used, AchieversD, RetAchieversD) :-
  action(HAction, PreconditionsT, PreconditionsF, FinalConditionsF, Verify, Effects),
  functor(HAction, ActionName, _),
  sub_string(ActionName, _, _, _, '_start'),
  debug_format('\n[extract_achievers-3]action [~w][~w] is START and HL~n', [ID, HAction]),
  (
    \+(_ = AchieversD.get(ID))
    ->(
      TempAchieversD = AchieversD.put(ID, [])
    );(
      TempAchieversD = AchieversD
    )
  ),
  % Take old achievers because one can never be too sure
  OldAchievers = TempAchieversD.get(ID),
  debug_format('[extract_achievers-3]Old achievers ~w\n', [OldAchievers]),

  % Find the achievers for this action
  last_achievers_ids(PreconditionsT, PreconditionsF, Verify, TActions, HL_Achievers),
  debug_format('[extract_achievers-3]HL achievers ~w\n', [HL_Achievers]),
  append(OldAchievers, HL_Achievers, NewAchievers),
  debug_format('[extract_achievers-3]New achievers ~w\n', [NewAchievers]),

  % Set this action as an achiever of the previous low-level actions
  add_hl_to_ll_enablers(ID, Used, TempAchieversD, TempTempAchieversD),
  debug_format('[extract_achievers-3]LL achievers ~w\n', [TempTempAchieversD]),

  % Set the new correct achievers for the action
  NewAchieversD = TempTempAchieversD.put(ID, NewAchievers),

  append([ID-HAction], Used, NewUsed),
  extract_achievers(TActions, NewUsed, NewAchieversD, RetAchieversD).


extract_achievers([ID-HAction|TActions], Used, AchieversD, RetAchieversD) :-
  ll_action(HAction, PreconditionsT, PreconditionsF, FinalConditionsF, Verify, Effects),
  (
    \+(_ = AchieversD.get(ID))
    ->(
      NewAchieversD = AchieversD.put(ID, [])
    );(
      NewAchieversD = AchieversD
    )
  ),
  debug_format('\n[extract_achievers-4]action [~w][~w] is LL ~w~n', [ID, HAction, NewAchieversD]),

  append([ID-HAction], Used, NewUsed),
  extract_achievers(TActions, NewUsed, NewAchieversD, RetAchieversD).


extract_achievers(_, _, _, _):-
  debug_format('\n[extract_achievers-5]~n'),
  fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This function generates a HL plan
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
generate_plan_hl(State, Goal, _Been_list, Plan, _MaxDepth, Plan) :-
  % enable_debug,
  goal_reached(State, Goal),
  debug_format('Goal reached\n').

generate_plan_hl(State, Goal, Been_list, Plan, MaxDepth, FinalPlan) :-
  % enable_debug,
  length(Plan, Length), 
  Length >= MaxDepth,
  debug_format('Max depth reached Length ~w, MaxDepth ~w, Plan ~w\n\n', [Length, MaxDepth, Plan]),
  fail.

generate_plan_hl(State, Goal, Been_list, Plan, MaxDepth, FinalPlan) :-
  % enable_debug,
  length(Plan, Length), Length < MaxDepth,
  debug_format('\n\nCurrent plan: ~w\n', [Plan]),

  % choose_action(State, Goal, Name, PreconditionsT, PreconditionsF, FinalConditionsF, Verify, Effects),
  action(Name, PreconditionsT, PreconditionsF, FinalConditionsF, Verify, Effects),
  debug_format('Checking action ~w for state: ~w\n', [Name, State]),
  is_applicable(State, PreconditionsT, PreconditionsF, FinalConditionsF, Verify, Goal),
  debug_format('Action ~w is applicable for state ~w\n', [Name, State]),
  
  change_state(State, Effects, NewState),
  debug_format('Obtained new state ~w\n', [NewState]),
  
  \+member_state(NewState, Been_list),
 
  % Change state and add action to plan
  stack(NewState, Been_list, NewBeen_list),
  debug_format('New state: ~w\n', [NewState]),
  stack([Length-Name], Plan, NewPlan),
  debug_format('New plan: ~w\n', [NewPlan]),
  generate_plan_hl(NewState, Goal, NewBeen_list, NewPlan, MaxDepth, FinalPlan),
  true.

generate_plan_hl(State, Goal, Been_list, Plan, MaxDepth, FinalPlan) :-
  fail. 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This function takes a list of achievers and removes the duplicates 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
find_action_name_from_plan([ID-Action|_], ID, Action).
find_action_name_from_plan([_ID-_Action|T], ID, RetAction) :-
  find_action_name_from_plan(T, ID, RetAction).
find_action_name_from_plan([], _, _) :-
  debug_format('Could not find action name, SOMETHING IS VERY WRONG\n'),
  fail.

clean_achievers(EnablersD, Plan, RetEnablers) :-
  clean_achievers(EnablersD, Plan, [], RetEnablers, 0).

clean_achievers(EnablersD, Plan, RetEnablers, RetEnablers, Counter) :-
  length(Plan, Counter).
clean_achievers(EnablersD, Plan, Enablers, RetEnablers, Counter):-
  TempActionEnablers = EnablersD.get(Counter),
  move_to_set(TempActionEnablers, ActionEnablers),
  find_action_name_from_plan(Plan, Counter, ActionName),
  debug_format('Action ~w has enablers ~w\n', [ActionName, ActionEnablers]),
  append([Counter-ActionName-ActionEnablers], Enablers, NewEnablers),
  NewCounter is Counter + 1,
  clean_achievers(EnablersD, Plan, NewEnablers, RetEnablers, NewCounter).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This function removes duplicates from a list
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
move_to_set(Ach, Ret) :-
  move_to_set(Ach, [], TmpRet),
  reverse(TmpRet, Ret).
  
move_to_set([], R, R).
move_to_set([H|T], Temp, Ret) :-
  member(H, Temp),
  move_to_set(T, Temp, Ret).
move_to_set([H|T], Temp, Ret) :-
  \+member(H, Temp),
  append([H], Temp, NewTemp),
  move_to_set(T, NewTemp, Ret).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This function reverses a list
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
reverse([], Ret, Ret).
reverse([H|T], Temp, Ret) :-
  append([H], Temp, NewTemp),
  reverse(T, NewTemp, Ret).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%