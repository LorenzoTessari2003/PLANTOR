:- ensure_loaded('utility/utility.pl').
:- ensure_loaded('includes.pl').

:- dynamic map/2.

goal_reached(State, Goal) :-
  subset(Goal, State).
  % equal_sets(Goal, State).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This function is used to add the last achievers for end actions.
% For each end action, the start action is an achiever and if the action is not
% low-level, then all the lower-level action between the start action and the 
% end action are achievers of the end action
% TODO I should also check that the actions have the same arguments here
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_achievers_end(_PrevActionName, [], LastAchievers, LastAchievers, _).

add_achievers_end(PrevActionName, [[_ID-HAction]|_TActions], LastAchievers, LastAchievers, Pre) :-
  functor(HAction, ActionName, _),
  sub_string(ActionName, _, _, _, PrevActionName).
add_achievers_end(PrevActionName, [[ID-HAction]|TActions], LastAchievers, RetLastAchievers, Pre) :-
  functor(HAction, ActionName, _),
  \+sub_string(ActionName, _, _, _, PrevActionName),
  append([ID], LastAchievers, TempLastAchievers),
  add_achievers_end(PrevActionName, TActions, TempLastAchievers, RetLastAchievers, Pre).

add_achievers_end_ll(_PrevActionName, [], LastAchievers, LastAchievers, _).

add_achievers_end_ll(PrevActionName, [_ID-HAction|_TActions], LastAchievers, LastAchievers, Pre) :-
  debug_format('~w[add_achievers_end_ll] Is ~w the start action ~w_start\n', [Pre, HAction, PrevActionName]),
  functor(HAction, ActionName, _),
  sub_string(ActionName, _, _, _, PrevActionName),
  sub_string(ActionName, _, _, _, '_start'),
  debug_format('~w[add_achievers_end_ll] Found start action ~w for action ~w\n', [Pre, ActionName, PrevActionName]),
  true.
add_achievers_end_ll(PrevActionName, [ID-HAction|TActions], LastAchievers, RetLastAchievers, Pre) :-
  debug_format('~w[add_achievers_end_ll] ~w is not the start action of ~w_end\n', [Pre, HAction, PrevActionName]),
  % functor(HAction, ActionName, _),
  % \+((
  %   sub_string(ActionName, _, _, _, PrevActionName),
  %   sub_string(ActionName, _, _, _, '_start')
  % )),
  append([ID], LastAchievers, TempLastAchievers),
  debug_format('~w[add_achievers_end_ll] Adding achiever ~w for action ~w\n', [Pre, HAction, ID]),
  debug_format('~w[add_achievers_end_ll] to ~w\n', [Pre, LastAchievers]),
  debug_format('~w[add_achievers_end_ll] obtaining ~w\n', [Pre, TempLastAchievers]),
  add_achievers_end_ll(PrevActionName, TActions, TempLastAchievers, RetLastAchievers, Pre).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% As a first iteration, all the low-level actions that are not mapped to any 
% lower level action share the same resources and hence are achievers of the 
% following low-level actions in the same mapping. This function adds all the
% previous low-level actions as achievers of the current low-level action, up 
% until the previous high-level action.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_no_mapping_achievers(Length-HAction, Plan, IDHLAction, TempLastAchievers, RetLastAchievers, Pre) :-
  \+mapping(HAction, _),
  debug_format('~w[add_no_mapping_achievers] No mapping for action ~w adding prev resource constraints\n', [Pre, HAction]),
  debug_format('~w[add_no_mapping_achievers] Length: ~w\n', [Pre, Length]),
  debug_format('~w[add_no_mapping_achievers] IDHLAction: ~w\n', [Pre, IDHLAction]),
  debug_format('~w[add_no_mapping_achievers] Plan: ~w\n', [Pre, Plan]),
  debug_format('~w[add_no_mapping_achievers] HAction: ~w\n', [Pre, HAction]),
  add_no_mapping_achievers_wrapped(Length-HAction, Plan, IDHLAction, TempLastAchievers, RetLastAchievers, Pre),
  debug_format('~w[add_no_mapping_achievers] New achievers ~w\n', [Pre, RetLastAchievers]).

add_no_mapping_achievers(_Length-HAction, _Plan, _IDHLAction, TempLastAchievers, TempLastAchievers, Pre) :-
  mapping(HAction, _),
  debug_format('~w[add_no_mapping_achievers] Mapping for action ~w, not adding prev resource constraints\n', [Pre, HAction]),
  true.
add_no_mapping_achievers_wrapped(_ID-_Action, [IDHLAction-_PrevAction|_], IDHLAction, RetAchievers, RetAchievers, Pre) :-
  debug_format('~w[add_no_mapping_achievers_wrapped] New mappings ~w\n', [Pre, RetAchievers]),
  true.
add_no_mapping_achievers_wrapped(ID-Action, [PrevID-PrevAction|T], IDHLAction, TmpAchievers, RetAchievers, Pre) :-
  IDHLAction \= PrevID,
  append([PrevID], TmpAchievers, NewTmpAchievers),
  debug_format('~w[add_no_mapping_achievers_wrapped] Adding achiever ~w ~w for action ~w ~w, achievers: ~w\n', [Pre, PrevID, PrevAction, ID, Action, NewTmpAchievers]),
  add_no_mapping_achievers_wrapped(ID-Action, T, IDHLAction, NewTmpAchievers, RetAchievers, Pre).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


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
      assertz(map(IDHLAction, Length)),
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
generate_plan(Init, Goal, Plan, LastAchievers) :-
  generate_plan(Init, Goal, Plan, LastAchievers, 4).

generate_plan(Init, Goal, Plan, LastAchievers, MaxDepth) :-
  % enable_debug,
  format('Checking if the initial state is the goal state ~w ~w\n', [Init, Goal]),
  goal_reached(Init, Goal),
  format('Goal reached\n').

generate_plan(Init, Goal, Plan, LastAchievers, MaxDepth) :-
  % enable_debug,
  format('Generating the high-level temporal plan from ~w to ~w\n', [Init, Goal]),
  (
    generate_plan_hl(Init, Goal, [], [], MaxDepth, HL_Plan)
    ->(
      % Print information on the high-level part
      format('High-level plan generated\n~w\n', [HL_Plan]),
      format('Finding high-level achievers\n'),
      extract_hl_achievers(HL_Plan, HL_Achievers),
      format('High-level achievers found ~w\n', [HL_Achievers]),
      print_list(HL_Achievers),
      
      format('Applying the mappings to obtain the low-level temporal plan\n'),
      reverse(HL_Plan, HL_PlanReversed),
      (
        apply_mappings(Init, HL_PlanReversed, Plan)
        ->(
          format('Plan generated~w\n', [Plan]),
          (
            % reverse(Plan, PlanReversed),
            enable_debug,!,
            extract_achievers(Plan, LastAchievers)
            ->(
              format('Achievers found ~w\n', [LastAchievers]),
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

  %     extract_adj_matrix_actions(HL_Achievers, AdjMatrix, Actions),
  %     debug_format('Adjacency matrix:\n'),
  %     print_list(AdjMatrix),
  %     print_list(Actions),
  %     % Find the low-level plan
  %     debug_format('Applying the mappings to obtain the low-level temporal plan from ~w to ~w\n', [Init, Goal]),
  %     findall(Mapping, mapping(Mapping, _), Mappings),
  %     debug_format('Mappings available: ~w\n', [Mappings]),
  %     reverse(HL_Plan, HL_PlanReversed),
  %     reverse(HL_Achievers, HL_AchieversReversed),
  %     (
  %       % leash(-all),trace,
  %       apply_mappings(Init, Goal, HL_PlanReversed, HL_AchieversReversed, Plan, LL_Achievers)
  %       ->(
  %         debug_format('Plan generated~w\n', [Plan]),
  %         debug_format('LL achievers:\n'),
  %         print_list(LL_Achievers),
  %         clean_achievers(LL_Achievers, LastAchievers)
  %       );(
  %         format('Could not apply mappings\n'), fail
  %       ),
  %       % notrace,
  %       true
  %     ),
  %     true
  %   );(
  %     format('Could not generate a HL plan\n'), fail
  %   )
  % ).
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
  format('\t[add_hl_to_ll_enablers0]\n\t\tID: ~w\n\t\tUsed: ~w\n\t\tRetAchieversD: ~w\n', [ID, PassedPlanActions, RetAchieversD]),
  add_hl_to_ll_enablers(ID, PassedPlanActions, [], AchieversD, RetAchieversD, 0).

add_hl_to_ll_enablers(HL_ID, [LL_ID-HAction|TActions], Used, AchieversD, RetAchieversD, 0) :-
  action(HAction, _, _, _, _, _),
  format('\t[add_hl_to_ll_enablers2] Action ~w is HL -> continuing\n', [HAction]),
  add_hl_to_ll_enablers(HL_ID, TActions, Used, AchieversD, RetAchieversD, 0).

add_hl_to_ll_enablers(HL_ID, [LL_ID-HAction|TActions], Used, AchieversD, RetAchieversD, _) :-
  ll_action(HAction, _, _, _, _, _),
  format('\t[add_hl_to_ll_enablers3] Action ~w is LL -> changing\n', [HAction]),
  OldAchievers = AchieversD.get(LL_ID, []),
  append([HL_ID|Used], OldAchievers, NewAchievers),
  NewAchieversD = AchieversD.put(LL_ID, NewAchievers),
  append([LL_ID], Used, NewUsed),
  format('\t[add_hl_to_ll_enablers3] Calling with\n\t\tHL_ID ~w\n\t\tTActions ~w\n\t\tNewUsed: ~w\n\t\tNewAchieversD ~w\n\t\tRetAchieversD ~w\n\t\t1\n', [HL_ID, TActions, NewUsed, NewAchieversD, RetAchieversD]),
  add_hl_to_ll_enablers(HL_ID, TActions, NewUsed, NewAchieversD, RetAchieversD, 1).

add_hl_to_ll_enablers(_HL_ID, [_ID-HAction|_TActions], _Used, AchieversD, AchieversD, 1):-
  action(HAction, _, _, _, _, _),
  format('\t[add_hl_to_ll_enablers1] Action ~w is -> stopping\n', [HAction]).

extract_achievers(Plan, AchieversD):-
  format('Extracting all achievers for plan~n'),
  print_list(Plan),
  extract_achievers(Plan, [], achieversD{1:[1]}, AchieversD),
  format('Achievers found ~w~n', [AchieversD]).

extract_achievers([], _, AchieversD, AchieversD).

extract_achievers([ID-HAction|TActions], Used, AchieversD, RetAchieversD) :-
  action(HAction, PreconditionsT, PreconditionsF, FinalConditionsF, Verify, Effects),
  functor(HAction, ActionName, _),
  sub_string(ActionName, _, _, _, '_end'),
  format('\n[extract_achievers-2]action [~w][~w] is END and HL~n', [ID, HAction]),
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
  format('[extract_achievers-2]Old achievers ~w\n', [OldAchievers]),

  % Find the achievers for this action
  last_achievers_ids(PreconditionsT, PreconditionsF, Verify, TActions, HL_Achievers),
  format('[extract_achievers-2]HL achievers ~w\n', [HL_Achievers]),
  append(OldAchievers, HL_Achievers, NewAchievers),
  format('[extract_achievers-2]New achievers ~w\n', [NewAchievers]),

  % Add the low-level actions as achievers
  add_ll_enablers(TActions, LL_Achievers),
  format('[extract_achievers-2]LL achievers ~w\n', [LL_Achievers]),

  append(NewAchievers, LL_Achievers, NewAchievers2),
  format('[extract_achievers-2]New achievers2 ~w\n', [NewAchievers2]),

  % Set the new correct achievers for the action
  NewAchieversD = TempAchieversD.put(ID, NewAchievers2),

  append([ID-HAction], Used, NewUsed),
  extract_achievers(TActions, NewUsed, NewAchieversD, RetAchieversD).

extract_achievers([ID-HAction|TActions], Used, AchieversD, RetAchieversD) :-
  action(HAction, PreconditionsT, PreconditionsF, FinalConditionsF, Verify, Effects),
  functor(HAction, ActionName, _),
  sub_string(ActionName, _, _, _, '_start'),
  format('\n[extract_achievers-3]action [~w][~w] is START and HL~n', [ID, HAction]),
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
  format('[extract_achievers-3]Old achievers ~w\n', [OldAchievers]),

  % Find the achievers for this action
  last_achievers_ids(PreconditionsT, PreconditionsF, Verify, TActions, HL_Achievers),
  format('[extract_achievers-3]HL achievers ~w\n', [HL_Achievers]),
  append(OldAchievers, HL_Achievers, NewAchievers),
  format('[extract_achievers-3]New achievers ~w\n', [NewAchievers]),

  % Set this action as an achiever of the previous low-level actions
  add_hl_to_ll_enablers(ID, Used, TempAchieversD, TempTempAchieversD),
  format('[extract_achievers-3]LL achievers ~w\n', [TempTempAchieversD]),

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
  format('\n[extract_achievers-4]action [~w][~w] is LL ~w~n', [ID, HAction, NewAchieversD]),

  append([ID-HAction], Used, NewUsed),
  extract_achievers(TActions, NewUsed, NewAchieversD, RetAchieversD).


extract_achievers(_, _, _, _):-
  format('\n[extract_achievers-5]~n'),
  fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This function extracts the high-level achievers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
extract_hl_achievers(HL_Plan, HL_Achievers):-
  format('Extracting high-level achievers ~w~n', [HL_Plan]),
  extract_hl_achievers(HL_Plan, [], HL_Achievers).

extract_hl_achievers([], HL_Achievers, Ret_HL_Achievers):-
  reverse(HL_Achievers, Ret_HL_Achievers).

extract_hl_achievers([ID-HAction|TActions], TempHL_Achievers, HL_Achievers) :-
  extract_hl_achievers([[ID-HAction]|TActions], TempHL_Achievers, HL_Achievers).

extract_hl_achievers([[ID-HAction]|TActions], TempHL_Achievers, HL_Achievers) :-
  format('Extracting achievers for [~w] ~w\n', [ID, HAction]),
  action(HAction, PreconditionsT, PreconditionsF, FinalConditionsF, Verify, Effects),

  format('Checking last achievers against: ~w\n', [TActions]),  
  last_achievers_ids(PreconditionsT, PreconditionsF, Verify, TActions, Achievers),
  format('Achievers for ~w: ~w\n', [HAction, Achievers]),
  
  append([ID-Achievers], TempHL_Achievers, NewTempHL_Achievers),
  extract_hl_achievers(TActions, NewTempHL_Achievers, HL_Achievers).
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
% This function takes a list of achievers, removes the duplicates and reverses the list
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clean_achievers(LastAchievers, RetLastAchievers) :-
  clean_achievers(LastAchievers, [], TmpLastAchievers),
  reverse(TmpLastAchievers, [], RetLastAchievers).

clean_achievers([], LastAchievers, LastAchievers).
clean_achievers([ID-Action-Achievers|TActions], TempLastAchievers, RetLastAchievers) :-
  move_to_set(Achievers, NewAchievers),
  debug_format('Cleaned achievers for ~w ~w ~w from ~w\n', [ID, Action, NewAchievers, Achievers]),
  append([ID-Action-NewAchievers], TempLastAchievers, NewLastAchievers),
  clean_achievers(TActions, NewLastAchievers, RetLastAchievers).
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