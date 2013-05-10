open Relations
open ZVG_modules
open Zone_stubs
open Grammar_types

(*OK, for the next few examples, we're just going to let the numbers
  go nuts, OK? Thinking up a realistic example is hard.*)

let pt01 =
  {common_action = None;
   succession = Succession_both; child = (0, 0)
  } 

let pt02 =
  {common_action = Some 0;
   succession = Succession_both; child = (0, 1)
  } 

let pt03 =
  {common_action = Some 0;
   succession = Succession_both; child = (0, 0)
  } 

let pt04 =
  {common_action = Some 0;
   succession = Succession_both; child = (1, 2)
  } 

let pt05 =
  {common_action = Some 0;
   succession = Succession_both; child = (1, 2)
  } 

let s01 = Stack.create ();;

Stack.push
  {product_transition =
      pt01;
   successor_transition_list = [pt02; pt04];
   map1 = Action_option_with_node_map.empty;
   map2 = Action_option_with_node_map.empty;
  }
  s01;;

let test93 =
  if
    lookup_in_stack s01 (0, 0)
  then
    "test93 passed"
  else
    "test93 failed";;

let test94 =
  if
    not (lookup_in_stack s01 (0, 1))
  then
    "test94 passed"
  else
    "test94 failed";;

Stack.push
  {product_transition =
      pt02;
   successor_transition_list = [pt03; pt05];
   map1 = Action_option_with_node_map.empty;
   map2 = Action_option_with_node_map.empty;
  }
  s01;;

Stack.push
  {product_transition =
      pt04;
   successor_transition_list = [];
   map1 = Action_option_with_node_map.empty;
   map2 = Action_option_with_node_map.empty;
  }
  s01;;

let test95 =
  if
    lookup_in_stack s01 (0, 1)
  then
    "test95 passed"
  else
    "test95 failed";;

Stack.push
  {product_transition =
      pt03;
   successor_transition_list = [pt02; pt04];
   map1 = Action_option_with_node_map.empty;
   map2 = Action_option_with_node_map.empty;
  }
  s01;;

let ta03 = {
  numlocations = 3;
  numtrans = 2;
  numclocks = 1;
  numactions = 1;
  numinit = 0;
  clock_names = [|"X"|];
  locations =
    [|{
      location_index = 0;
      invariant = [True];
      departures =
        [|
          {
            action = 0;
            condition = [True];
            clock_resets = [||];
            next_location =  0
          };
          {
            action = 0;
            condition = [True];
            clock_resets = [||];
            next_location = 1
          }
        |]
     };
      {
        location_index = 1;
        invariant = [True];
        departures = [||]
      }
    |]
}

let ta04 = {
  numlocations = 3;
  numtrans = 2;
  numclocks = 1;
  numactions = 1;
  numinit = 0;
  clock_names = [|"X"|];
  locations =
    [|{
      location_index = 0;
      invariant = [True];
      departures =
        [|
          {
            action = 0;
            condition = [True];
            clock_resets = [||];
            next_location =  1
          };
          {
            action = 0;
            condition = [True];
            clock_resets = [||];
            next_location = 2
          }
        |]
     };
      {
      location_index = 1;
      invariant = [True];
      departures =
        [|
          {
            action = 0;
            condition = [True];
            clock_resets = [||];
            next_location =  0
          };
          {
            action = 0;
            condition = [True];
            clock_resets = [||];
            next_location = 2
          }
        |]
     };
      {
        location_index = 2;
        invariant = [True];
        departures = [||]
      }
    |]
}

