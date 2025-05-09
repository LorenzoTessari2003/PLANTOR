% This file was automatically generated by the LLM system
%%%%%%%%%%%%%%%%%%%%%%%
% kb
%%%%%%%%%%%%%%%%%%%%%%%
% Locations
location(location1).
location(location2).

% Containers
container(c1).
container(c2).

% Robot
robot(r1).

% Cranes
crane(crane1).
crane(crane2).

% Connections
connected(location1, location2).
connected(location2, location1).

% Resources
resources(robot(_)).
resources(crane(_)).

%%%%%%%%%%%%%%%%%%%%%%%
% init
%%%%%%%%%%%%%%%%%%%%%%%
init_state([
  at(c1, location1), on_ground(c1), 
  at(c2, location1), on(c2, c1), clear(c2),
  at(r1, location1), available(r1),
  at(crane1, location1), available(crane1),
  at(crane2, location2), available(crane2)
]).

%%%%%%%%%%%%%%%%%%%%%%%
% goal
%%%%%%%%%%%%%%%%%%%%%%%
goal_state([
  at(c1, location1), on_ground(c1), 
  at(c2, location2), on_ground(c2), clear(c2),
  at(r1, location1), available(r1),
  at(crane1, location1), available(crane1),
  at(crane2, location2), available(crane2)
]).

%%%%%%%%%%%%%%%%%%%%%%%
% actions
%%%%%%%%%%%%%%%%%%%%%%%
% Move a container from the ground to the top of another container within the same location using a crane
action(move_ground_to_top_start(Crane, Container1, Container2, Location),
  [available(Crane), on_ground(Container1), clear(Container1), at(Container1, Location), at(Container2, Location), clear(Container2)],
  [moving_ground_to_top(_, Container1, _, _)],
  [],
  [crane(Crane), container(Container1), container(Container2), location(Location), Container1 \= Container2],
  [
    del(available(Crane)), del(on_ground(Container1)), del(clear(Container2)),
    add(moving_ground_to_top(Crane, Container1, Container2, Location))
  ]
).
action(move_ground_to_top_end(Crane, Container1, Container2, Location),
  [moving_ground_to_top(Crane, Container1, Container2, Location)],
  [],
  [],
  [crane(Crane)],
  [
    del(moving_ground_to_top(Crane, Container1, Container2, Location)),
    add(on(Container1, Container2)), add(clear(Container1)), add(available(Crane))
  ]
).

% Load a container onto the robot using a crane
action(load_container_start(Crane, Robot, Container, Location),
  [available(Crane), available(Robot), clear(Container), at(Container, Location), at(Robot, Location)],
  [loading_container(_, _, Container, _)],
  [],
  [crane(Crane), robot(Robot), container(Container), location(Location)],
  [
    del(available(Crane)), del(available(Robot)), del(clear(Container)),
    add(loading_container(Crane, Robot, Container, Location))
  ]
).
action(load_container_end(Crane, Robot, Container, Location),
  [loading_container(Crane, Robot, Container, Location)],
  [],
  [],
  [crane(Crane), robot(Robot)],
  [
    del(loading_container(Crane, Robot, Container, Location)),
    add(on(Container, Robot)), add(clear(Container)), add(available(Crane)), add(available(Robot))
  ]
).

% Unload a container from the robot using a crane
action(unload_container_start(Crane, Robot, Container, Location),
  [available(Crane), available(Robot), on(Container, Robot), at(Robot, Location)],
  [unloading_container(_, _, Container, _)],
  [],
  [crane(Crane), robot(Robot), container(Container), location(Location)],
  [
    del(available(Crane)), del(available(Robot)), del(on(Container, Robot)),
    add(unloading_container(Crane, Robot, Container, Location))
  ]
).
action(unload_container_end(Crane, Robot, Container, Location),
  [unloading_container(Crane, Robot, Container, Location)],
  [],
  [],
  [crane(Crane), robot(Robot)],
  [
    del(unloading_container(Crane, Robot, Container, Location)),
    add(on_ground(Container)), add(clear(Container)), add(available(Crane)), add(available(Robot))
  ]
).

% Move a container from the ground to another position on the ground within the same location using a crane
action(move_ground_to_ground_start(Crane, Container, Location1, Location2),
  [available(Crane), on_ground(Container), clear(Container), at(Container, Location1)],
  [moving_ground_to_ground(_, Container, _, _)],
  [],
  [crane(Crane), container(Container), location(Location1), location(Location2), Location1 \= Location2],
  [
    del(available(Crane)), del(on_ground(Container)), del(at(Container, Location1)),
    add(moving_ground_to_ground(Crane, Container, Location1, Location2))
  ]
).
action(move_ground_to_ground_end(Crane, Container, Location1, Location2),
  [moving_ground_to_ground(Crane, Container, Location1, Location2)],
  [],
  [],
  [crane(Crane)],
  [
    del(moving_ground_to_ground(Crane, Container, Location1, Location2)),
    add(on_ground(Container)), add(at(Container, Location2)), add(clear(Container)), add(available(Crane))
  ]
).

% Move a container from the top of another container to the ground within the same location using a crane
action(move_top_to_ground_start(Crane, Container1, Container2, Location),
  [available(Crane), on(Container1, Container2), clear(Container1), at(Container2, Location)],
  [moving_top_to_ground(_, Container1, _, _)],
  [],
  [crane(Crane), container(Container1), container(Container2), location(Location), Container1 \= Container2],
  [
    del(available(Crane)), del(on(Container1, Container2)), del(clear(Container1)),
    add(moving_top_to_ground(Crane, Container1, Container2, Location)), add(clear(Container2))
  ]
).
action(move_top_to_ground_end(Crane, Container1, Container2, Location),
  [moving_top_to_ground(Crane, Container1, Container2, Location)],
  [],
  [],
  [crane(Crane)],
  [
    del(moving_top_to_ground(Crane, Container1, Container2, Location)),
    add(on_ground(Container1)), add(at(Container1, Location)), add(clear(Container1)), add(available(Crane))
  ]
).

% Move a container from the top of another container to the top of a different container within the same location using a crane
action(move_top_to_top_start(Crane, Container1, Container2, Container3, Location),
  [available(Crane), on(Container1, Container2), clear(Container1), at(Container2, Location), at(Container3, Location), clear(Container3)],
  [moving_top_to_top(_, Container1, _, _, _)],
  [],
  [crane(Crane), container(Container1), container(Container2), container(Container3), location(Location), Container1 \= Container2, Container1 \= Container3, Container2 \= Container3],
  [
    del(available(Crane)), del(on(Container1, Container2)), del(clear(Container1)),
    add(moving_top_to_top(Crane, Container1, Container2, Container3, Location)), add(clear(Container2))
  ]
).
action(move_top_to_top_end(Crane, Container1, Container2, Container3, Location),
  [moving_top_to_top(Crane, Container1, Container2, Container3, Location)],
  [],
  [],
  [crane(Crane)],
  [
    del(moving_top_to_top(Crane, Container1, Container2, Container3, Location)),
    add(on(Container1, Container3)), add(at(Container1, Location)), add(clear(Container1)), add(available(Crane))
  ]
).

% Move the robot from one location to another
action(move_robot_start(Robot, Location1, Location2),
  [available(Robot), at(Robot, Location1)],
  [moving_robot(_, _, _)],
  [],
  [robot(Robot), location(Location1), location(Location2), Location1 \= Location2],
  [
    del(available(Robot)), del(at(Robot, Location1)),
    add(moving_robot(Robot, Location1, Location2))
  ]
).
action(move_robot_end(Robot, Location1, Location2),
  [moving_robot(Robot, Location1, Location2)],
  [],
  [],
  [robot(Robot)],
  [
    del(moving_robot(Robot, Location1, Location2)),
    add(at(Robot, Location2)), add(available(Robot))
  ]
).
