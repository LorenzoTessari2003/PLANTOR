# Report Experiment DWR - 4o - 2 

## Consistency Check

The LLM did not detect (correctly) any inconsistency in the descriptions.

## High-Level Generation

Action are generated in the wrong way

One of the errors is:

action(move_top_to_top_start(Crane, Container1, Container2, Container3, Location),
  [on(Container1, Container2), clear(Container1), clear(Container3), crane(Crane, Location)],
  [on(_, Container1), on(Container1, _), moving_top_to_top(_, Container1, _, _, _)],
  [],
  [container(Container1), container(Container2), container(Container3), location(Location), crane(Crane, Location)],
  [
    del(on(Container1, Container2)), del(clear(Container1)), del(clear(Container3)),
    add(moving_top_to_top(Crane, Container1, Container2, Container3, Location)), add(clear(Container2))
  ]
).

[on(_, Container1), on(Container1, _), moving_top_to_top(_, Container1, _, _, _)],

on(Container1, _) must not be there.


## Low-Level Generation

Could not generate HL plan