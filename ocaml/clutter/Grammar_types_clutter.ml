open Grammar_types

type transition = {
  action: int;
  condition: clock_constraint;
  clock_resets: string array;
  next_location: int
}

type location = {
  location_index: int;
  invariant: clock_constraint;
  departures: transition array
}

type timed_automaton = {
  numlocations: int;
  numtrans: int;
  numclocks: int;
  numactions: int;
  numinit: int;
  clock_names: string array;
  locations: location array
}

type lbound =
  Lslack of int
| Lstrict of int

type zone_using_list = {
  zone_location1: int;
  zone_constraint1: clock_constraint
}
