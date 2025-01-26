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
resources([robot(r1), crane(crane1), crane(crane2)]).

%%%%%%%%%%%%%%%%%%%%%%%
% init
%%%%%%%%%%%%%%%%%%%%%%%
init_state([
  at(c1, location1, ground), at(c2, location1, c1),
  at(r1, location1), 
  clear(c2), clear(r1), 
  available(crane1), available(crane2)
]).

%%%%%%%%%%%%%%%%%%%%%%%
% goal
%%%%%%%%%%%%%%%%%%%%%%%
goal_state([
  at(c1, location1, ground), at(c2, location2, ground),
  at(r1, location2), 
  clear(c1), clear(c2), clear(r1), 
  available(crane1), available(crane2)
]).

%%%%%%%%%%%%%%%%%%%%%%%
% actions
%%%%%%%%%%%%%%%%%%%%%%%
% Move a container from the ground to the top of another container within the same location
action(move_ground_to_top_start(Crane, Container1, Container2, Location),
  [available(Crane), at(Container1, Location, ground), at(Container2, Location, _), clear(Container1), clear(Container2)],
  [moving_ground_to_top(_, _, _, _), moving_top_to_ground(_, _, _, _), moving_top_to_top(_, _, _, _, _), moving_ground_to_robot(_, _, _, _), moving_robot_to_ground(_, _, _, _), moving_robot_to_top(_, _, _, _, _)],
  [],
  [crane(Crane), container(Container1), container(Container2), location(Location), Container1 \= Container2],
  [
    del(available(Crane)), del(clear(Container1)), del(at(Container1, Location, ground)),
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
    add(on(Container1, Container2)), add(at(Container1, Location, Container2)), add(clear(Container1)), add(available(Crane))
  ]
).

% Move a container from the top of another container to the ground within the same location
action(move_top_to_ground_start(Crane, Container1, Container2, Location),
  [available(Crane), at(Container1, Location, Container2), clear(Container1)],
  [moving_ground_to_top(_, _, _, _), moving_top_to_ground(_, _, _, _), moving_top_to_top(_, _, _, _, _), moving_ground_to_robot(_, _, _, _), moving_robot_to_ground(_, _, _, _), moving_robot_to_top(_, _, _, _, _)],
  [],
  [crane(Crane), container(Container1), container(Container2), location(Location), Container1 \= Container2],
  [
    del(available(Crane)), del(clear(Container1)), del(at(Container1, Location, Container2)),
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
    add(at(Container1, Location, ground)), add(clear(Container1)), add(available(Crane))
  ]
).

% Move a container from the top of another container to the top of a different container within the same location
action(move_top_to_top_start(Crane, Container1, Container2, Container3, Location),
  [available(Crane), at(Container1, Location, Container2), at(Container3, Location, _), clear(Container1), clear(Container3)],
  [moving_ground_to_top(_, _, _, _), moving_top_to_ground(_, _, _, _), moving_top_to_top(_, _, _, _, _), moving_ground_to_robot(_, _, _, _), moving_robot_to_ground(_, _, _, _), moving_robot_to_top(_, _, _, _, _)],
  [],
  [crane(Crane), container(Container1), container(Container2), container(Container3), location(Location), Container1 \= Container2, Container1 \= Container3, Container2 \= Container3],
  [
    del(available(Crane)), del(clear(Container1)), del(at(Container1, Location, Container2)),
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
    add(on(Container1, Container3)), add(at(Container1, Location, Container3)), add(clear(Container1)), add(available(Crane))
  ]
).

% Load a container onto the robot
action(load_robot_start(Crane, Robot, Container, Location),
  [available(Crane), at(Container, Location, ground), at(Robot, Location), clear(Container), clear(Robot)],
  [moving_ground_to_top(_, _, _, _), moving_top_to_ground(_, _, _, _), moving_top_to_top(_, _, _, _, _), moving_ground_to_robot(_, _, _, _), moving_robot_to_ground(_, _, _, _), moving_robot_to_top(_, _, _, _, _)],
  [],
  [crane(Crane), robot(Robot), container(Container), location(Location)],
  [
    del(available(Crane)), del(clear(Container)), del(at(Container, Location, ground)),
    add(moving_ground_to_robot(Crane, Robot, Container, Location))
  ]
).
action(load_robot_end(Crane, Robot, Container, Location),
  [moving_ground_to_robot(Crane, Robot, Container, Location)],
  [],
  [],
  [crane(Crane)],
  [
    del(moving_ground_to_robot(Crane, Robot, Container, Location)),
    add(on(Container, Robot)), add(at(Container, Location, Robot)), add(clear(Container)), add(available(Crane))
  ]
).

% Unload a container from the robot to the ground
action(unload_robot_start(Crane, Robot, Container, Location),
  [available(Crane), at(Container, Location, Robot), at(Robot, Location), clear(Container)],
  [moving_ground_to_top(_, _, _, _), moving_top_to_ground(_, _, _, _), moving_top_to_top(_, _, _, _, _), moving_ground_to_robot(_, _, _, _), moving_robot_to_ground(_, _, _, _), moving_robot_to_top(_, _, _, _, _)],
  [],
  [crane(Crane), robot(Robot), container(Container), location(Location)],
  [
    del(available(Crane)), del(clear(Container)), del(at(Container, Location, Robot)),
    add(moving_robot_to_ground(Crane, Robot, Container, Location))
  ]
).
action(unload_robot_end(Crane, Robot, Container, Location),
  [moving_robot_to_ground(Crane, Robot, Container, Location)],
  [],
  [],
  [crane(Crane)],
  [
    del(moving_robot_to_ground(Crane, Robot, Container, Location)),
    add(at(Container, Location, ground)), add(clear(Container)), add(available(Crane))
  ]
).

% Move the robot from one location to another while carrying a container
action(move_robot_start(Robot, Container, Location1, Location2),
  [at(Robot, Location1), at(Container, Location1, Robot), clear(Robot)],
  [moving_ground_to_top(_, _, _, _), moving_top_to_ground(_, _, _, _), moving_top_to_top(_, _, _, _, _), moving_ground_to_robot(_, _, _, _), moving_robot_to_ground(_, _, _, _), moving_robot_to_top(_, _, _, _, _)],
  [],
  [robot(Robot), container(Container), location(Location1), location(Location2), connected(Location1, Location2)],
  [
    del(at(Robot, Location1)), del(at(Container, Location1, Robot)),
    add(moving_robot(Robot, Container, Location1, Location2))
  ]
).
action(move_robot_end(Robot, Container, Location1, Location2),
  [moving_robot(Robot, Container, Location1, Location2)],
  [],
  [],
  [robot(Robot)],
  [
    del(moving_robot(Robot, Container, Location1, Location2)),
    add(at(Robot, Location2)), add(at(Container, Location2, Robot)), add(clear(Robot))
  ]
).
