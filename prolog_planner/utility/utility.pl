:- ensure_loaded('actions.pl').
:- ensure_loaded('conditions.pl').
:- ensure_loaded('resources.pl').
:- ensure_loaded('state.pl').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                             UTILITY FUNCTIONS                              %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% :brief: Change planner_debug(X) to set the debug mode on or off. 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
enable_debug  :- retractall(planner_debug(_)), assert(planner_debug(true)).
disable_debug :- retractall(planner_debug(_)), assert(planner_debug(fasle)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% :brief: prints the message with the format provided if the predicate 
% planner_debug(true) is set.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
debug_format(Format) :- 
	planner_debug(true) -> format(Format); true.

debug_format(Format, Args) :- 
	planner_debug(true) -> format(Format, Args); true.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% :brief: prints the list of actions reversed.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
reverse_print_actions(Actions) :-
	length_stack(Actions, Len),
	I is Len,
	reverse_print_actions(Actions, I).

reverse_print_actions(Actions, _I) :-
	empty_stack(Actions).

reverse_print_actions(Actions, I) :-
	stack(A, TActions, Actions), 
	NewI is I - 1,
	reverse_print_actions(TActions, NewI),
	format("[~w] ~w~n", [I, A]).

reverse_print_actions_times(Actions, Times) :-
	length_stack(Actions, Len), length_stack(Times, Len),
	I is Len,
	reverse_print_actions_times(Actions, Times, I).

reverse_print_actions_times(Actions, Times, _I) :-
	empty_stack(Actions),
	empty_stack(Times).

reverse_print_actions_times(Actions, Times, I) :-
	stack(A, TActions, Actions), 
	stack(T, TTimes, Times),
	NewI is I - 1,
	reverse_print_actions_times(TActions, TTimes, NewI),
	format("[~w] ~w ~w~n", [I, A, T]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% :brief: prints a list.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
print_list([], _I, _S).
print_list([H|T], I, S) :- 
    format('~w[~w] ~w~n', [S, I, H]),
    NewI is I + 1,
    print_list(T, NewI, S).

print_list(L, S) :- print_list(L, 0, S).

print_list(L) :- print_list(L, 0, "").
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% :brief: reverses a list 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
reverse_list([],Acc,Acc).
reverse_list([H|T],Acc,Ret) :- reverse_list(T,[H|Acc],Ret).
reverse_list(X,Y) :- reverse_list(X,[],Y).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

