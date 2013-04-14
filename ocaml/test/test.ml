open Grammar_types
open Clock_constraint_utilities
open Graph_functions3
open Unit_constraint_intersection
open Zone_stubs
open UDBM_utilities
open Test_base

let test2 = 
  if
    (match_clock_constraints
       (unit_constraint_intersection
          (Lt ("x1", 3))
          (Lt ("x2", 4))
       )
       ([(Lt ("x1", 3));
         (Lt ("x2", 4))])
    )
  then
    "test2 passed"
  else
    "test2 failed"

let test3 = 
  if
    (match_clock_constraints
       (unit_constraint_intersection
          (Lt ("x1", 3))
          (Lt ("x1", 4))
       )
       ([(Lt ("x1", 3))])
    )
  then
    "test3 passed"
  else
    "test3 failed"
      
let test4 = 
  if
    (match_clock_constraints
       (unit_constraint_intersection
          (Gt ("x1", 3))
          (Lt ("x1", 4))
       )
       ([(Gt ("x1", 3));
         (Lt ("x1", 4))])
    )
  then
    "test4 passed"
  else
    "test4 failed"

let test5 = 
  if
    (match_clock_constraints
       (unit_constraint_intersection
          (Lt ("x1", 3))
          (Gt ("x1", 4))
       )
       ([False])
    )
  then
    "test5 passed"
  else
    "test5 failed"

let test6 =
  let a = 
    split_zone_on_clock_constraint
      {zone_location1 = 0;
       zone_constraint1 = [Le ("X", 8); Ge ("X", 2)]
      }
      [Le ("X", 6); Ge ("X", 4)]
      [|"X"|]
  in
  (Printf.sprintf "test6: %s" (string_of_int (List.length a)))

let test8 =
  if
    match_minimised [False; False; False; Eq ("X", 7)] ["X"] [False]
  then
    "test8 passed"
  else
    "test8 failed"
  
let test9 =
  if
    match_minimised [True; True; True; Eq ("X", 7)] ["X"] [Ge ("X", 7); Le ("X", 7)]
  then
    "test9 passed"
  else
    "test9 failed"
  
let test10 =
  if
    match_minimised [Lt ("X", 5); Lt ("X", 7); Lt ("X", 3)] ["X"] [Lt ("X", 3)]
  then
    "test10 passed"
  else
    "test10 failed"
  
let test11 =
  if
    match_minimised [Le ("X", 5); Le ("X", 7); Le ("X", 3)] ["X"] [Le ("X", 3)]
  then
    "test11 passed"
  else
    "test11 failed"
  
let test12 =
  if
    match_minimised [Ge ("X", 5); Ge ("X", 7); Ge ("X", 3)] ["X"] [Ge ("X", 7)]
  then
    "test12 passed"
  else
    "test12 failed"
  
let test13 =
  if
    match_minimised [Gt ("X", 5); Gt ("X", 7); Gt ("X", 3)] ["X"] [Gt ("X", 7)]
  then
    "test13 passed"
  else
    "test13 failed"
  
let test14 =
  if
    0 = clock_name_to_index "X" [|"X"; "Y"|]
  then
    "test14 passed"
  else
    "test14 failed"

let test15 =
  if
    1 = clock_name_to_index "Y" [|"X"; "Y"|]
  then
    "test15 passed"
  else
    "test15 failed"

let test16 =
  if
    -1 = clock_name_to_index "Z" [|"X"; "Y"|]
  then
    "test16 passed"
  else
    "test16 failed"

let test17 =
  if
    None <>
    (clock_constraint_to_raw_t_option
       [|"X"; "Y"|]
       [Le("X", 4); Ge ("X", 3)]
    )
  then
    "test17 passed"
  else
    "test17 failed"

let test18 =
  if
    None = (clock_constraint_to_raw_t_option
       [|"X"; "Y"|]
       [Le("X", 3); Ge ("X", 4)]
    )
  then
    "test18 passed"
  else
    "test18 failed"

let test19 =
  if
    [Eq ("X1", 0); Eq ("X3", 0); Gt ("X2", 3); Lt
      ("X2", 5)]
      =
    (clock_constraint_after_clock_resets
       [Ge ("X1", 5); Gt ("X2", 3); Le ("X1", 7); Lt ("X3", 5); Lt
         ("X2", 5)]
       [|"X1"; "X3"|])
  then
    "test19 passed"
  else
    "test19 failed"

let test20 =
  let
      c = [Le ("X1", 5); Lt ("X1", 7); Gt ("X1", 2); Ge ("X1", 3)]
  in
  if
    (match_clock_constraints
       (pseudo_future c)
       [Gt ("X1", 2); Ge ("X1", 3)]
    )
  then
    "test20 passed"
  else
    "test20 failed"

let test21 =
  if
    (match_clock_constraints
       (pseudo_future [Eq ("X1", 9)])
       [Ge ("X1", 9)]
    )
  then
    "test21 passed"
  else
    "test21 failed"

(* CAV paper example *)
let test22 = {
  numlocations = 3;
  numtrans = 3;
  numclocks = 1;
  numactions = 3;
  numinit = 0;
  clock_names = [|"X"|];
  locations =
    [|
      {
        location_index = 0;
        invariant = [True];
        departures =
          [|
            {
              action = 0;
              condition = [Gt ("X", 2)];
              clock_resets = [||];
              next_location = 1
            }
          |]
      };
      {
        location_index = 1;
        invariant = [True];
        departures =
          [|
            {
              action = 1;
              condition = [Gt ("X", 5)];
              clock_resets = [|"X"|];
              next_location = 2
            }
          |]
      };
      {
        location_index = 2;
        invariant = [True];
        departures =
          [|
            {
              action = 2;
              condition = [Gt ("X", 8)];
              clock_resets = [||];
              next_location = 0
            }
          |]
      }
    |]
}

let test23 ta =
  let
      zone_list_array = init_zone_list_array ta
  in
  (Array.length zone_list_array = ta.numlocations)
    &&
    (let
        truth = ref true
     in
     Array.iteri
       (function i -> function zone_list ->
         if
           (i = ta.numinit)
         then
           truth :=
             match
               zone_list
             with
             | [zone] ->
               (zone.zone_location1 = i &&
                   (match_clock_constraints
                      zone.zone_constraint1
                      (pseudo_future
                         (List.map
                            (function cn -> Eq (cn, 0))
                            (Array.to_list ta.clock_names)
                         )
                      )
                   )
               )
             | _ -> false
         else
           truth := (zone_list = [])
       )
       zone_list_array;
     !truth
    )

let test24 =
  if
    test23 test22
  then
    "test24 passed"
  else
    "test24 failed"

let test25 queue location =
  let
      next_queue = enqueue_without_repetition queue location
  in
  List.fold_left
    (function truth -> function location ->
      (List.length
         (List.filter
            ((=) location)
            next_queue
         )
      )
      =
        1
    )
    true
    (location::queue)

let test26  =
  if
    test25 [0; 1; 3] 2
  then
    "test26 passed"
  else
    "test26 failed"

let test27  =
  if
    test25 [0; 1; 2] 2
  then
    "test27 passed"
  else
    "test27 failed"

let test28 found expected =
  (List.length found = List.length expected) &&
  (List.for_all
     (function c1 ->
       (List.length
          (List.filter
             (function zone -> match_clock_constraints zone.zone_constraint1 c1)
             found
          )
       )
       =
         (List.length
            (List.filter
               (match_clock_constraints c1)
               expected
            )
         )
     )
     expected
  )

let test29 = 
  (self_split
     test22
     test22.numinit
     [{zone_location1 = test22.numinit;
       zone_constraint1 = [True]
      }]
  )
    
let test30 =
  (self_split
     test22
     1
     [{zone_location1 = 1;
       zone_constraint1 = [Gt ("X", 2)]
      }]
  )
    
let test31 =
  let
      found =
    test29
  in
  let
      expected =
    [[Le ("X", 2)]; [Gt ("X", 2)]]
  in
  if
    test28 found expected
  then
    "test31 passed"
  else
    "test31 failed"

let test32 =
  let
      found =
    test30
  in
  let
      expected =
    [[Gt ("X", 2); Le ("X", 5)]; [Gt ("X", 5)]]
  in
  if
    test28 found expected
  then
    "test32 passed"
  else
    "test32 failed"

let test33 =
  let
      found =
    useful_predecessor_zones
      test22
      test29
      [Gt ("X", 2)]
  in
  let
      expected =
    [[Le ("X", 2)]; [Gt ("X", 2)]]
  in
  if
    test28 found expected
  then
    "test33 passed"
  else
    "test33 failed"

let test34 =
  let
      found =
    useful_predecessor_zones
      test22
      test30
      [Gt ("X", 5)]
  in
  let
      expected =
    [[Gt ("X", 2); Le ("X", 5)]; [Gt ("X", 5)]]
  in
  if
    test28 found expected
  then
    "test34 passed"
  else
    "test34 failed"

let test35 =
  let
      found =
    useful_predecessor_zones
      test22
      test30
      [Le ("X", 5)]
  in
  let
      expected =
    [[Gt ("X", 2); Le ("X", 5)]]
  in
  if
    test28 found expected
  then
    "test35 passed"
  else
    "test35 failed"

let test36 =
  let
      found =
    successor_zones_from_predecessor
      test22
      test30
      {clock_resets = [||]; condition = [Le ("X", 5)]; action = 0;
       next_location = 1}
  in
  let
      expected =
    [[Gt ("X", 2)]]
  in
  if
    test28 found expected
  then
    "test36 passed"
  else
    "test36 failed"

let test37 =
  let
      found =
    successor_zones_from_predecessor
      test22
      test30
      {clock_resets = [||]; condition = [Gt ("X", 5)]; action = 0;
       next_location = 1}
  in
  let
      expected =
    [[Gt ("X", 5)]; [Gt ("X", 5)]]
  in
  if
    test28 found expected
  then
    "test37 passed"
  else
    "test37 failed"

let test38 =
  let
      found =
    successor_zones_from_predecessor
      test22
      test30
      {clock_resets = [|"X"|]; condition = [Gt ("X", 5)]; action = 0;
       next_location = 1}
  in
  let
      expected =
    [[Ge ("X", 0)]; [Ge ("X", 0)]]
  in
  if
    test28 found expected
  then
    "test38 passed"
  else
    "test38 failed"

let test39 =
  let
      found =
    new_successor_zones
      test22
      test29
      {clock_resets = [||]; condition = [Gt ("X", 2)]; action = 0;
       next_location = 1}
      test30
  in
  let
      expected =
    [[Gt ("X", 2); Le ("X", 5)]; [Gt ("X", 5)]]
  in
  if
    test28 found expected
  then
    "test39 passed"
  else
    "test39 failed"

let _ =
  print_string test2;
  print_newline ();
  print_string test3;
  print_newline ();
  print_string test4;
  print_newline ();
  print_string test5;
  print_newline ();
  print_string test6;
  print_newline ();
  print_string "test6 should be 3.\n";
  print_string test8;
  print_newline ();
  print_string test9;
  print_newline ();
  print_string test10;
  print_newline ();
  print_string test11;
  print_newline ();
  print_string test12;
  print_newline ();
  print_string test13;
  print_newline ();
  print_string test14;
  print_newline ();
  print_string test15;
  print_newline ();
  print_string test16;
  print_newline ();
  print_string test17;
  print_newline ();
  print_string test18;
  print_newline ();
  print_string test19;
  print_newline ();
  print_string test20;
  print_newline ();
  print_string test21;
  print_newline ();
  print_string test24;
  print_newline (); 
  print_string test26;
  print_newline ();
  print_string test27;
  print_newline ();
  print_string test31;
  print_newline ();
  print_string test32;
  print_newline ();
  print_string test33;
  print_newline ();
  print_string test34;
  print_newline ();
  print_string test35;
  print_newline ();
  print_string test36;
  print_newline ();
  print_string test37;
  print_newline ();
  print_string test38;
  print_newline ();
  print_string test39;
  print_newline ();
 exit 0
