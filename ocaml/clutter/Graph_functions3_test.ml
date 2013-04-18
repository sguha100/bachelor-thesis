open Grammar_types
open Graph_functions3
open Test_base

let verify_zone_lists ta =
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

let test24 =
  if
    verify_zone_lists ta1
  then
    "test24 passed"
  else
    "test24 failed"

let verify_enqueue queue location =
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
    verify_enqueue [0; 1; 3] 2
  then
    "test26 passed"
  else
    "test26 failed"

let test27  =
  if
    verify_enqueue [0; 1; 2] 2
  then
    "test27 passed"
  else
    "test27 failed"

let verify_zone_list found expected =
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

let ta1_zone_list4 = 
  (self_split
     ta1
     ta1.numinit
     [{zone_location1 = ta1.numinit;
       zone_constraint1 = [True]
      }]
  )
    
let ta1_zone_list5 =
  (self_split
     ta1
     1
     [{zone_location1 = 1;
       zone_constraint1 = [Gt ("X", 2)]
      }]
  )
    
let test31 =
  let
      found =
    ta1_zone_list4
  in
  let
      expected =
    [[Le ("X", 2)]; [Gt ("X", 2)]]
  in
  if
    verify_zone_list found expected
  then
    "test31 passed"
  else
    "test31 failed"

let test32 =
  let
      found =
    ta1_zone_list5
  in
  let
      expected =
    [[Gt ("X", 2); Le ("X", 5)]; [Gt ("X", 5)]]
  in
  if
    verify_zone_list found expected
  then
    "test32 passed"
  else
    "test32 failed"

let test33 =
  let
      found =
    useful_predecessor_zones
      ta1
      ta1_zone_list4
      [Gt ("X", 2)]
  in
  let
      expected =
    [[Le ("X", 2)]; [Gt ("X", 2)]]
  in
  if
    verify_zone_list found expected
  then
    "test33 passed"
  else
    "test33 failed"

let test34 =
  let
      found =
    useful_predecessor_zones
      ta1
      ta1_zone_list5
      [Gt ("X", 5)]
  in
  let
      expected =
    [[Gt ("X", 2); Le ("X", 5)]; [Gt ("X", 5)]]
  in
  if
    verify_zone_list found expected
  then
    "test34 passed"
  else
    "test34 failed"

let test35 =
  let
      found =
    useful_predecessor_zones
      ta1
      ta1_zone_list5
      [Le ("X", 5)]
  in
  let
      expected =
    [[Gt ("X", 2); Le ("X", 5)]]
  in
  if
    verify_zone_list found expected
  then
    "test35 passed"
  else
    "test35 failed"

let test36 =
  let
      found =
    successor_zones_from_predecessor
      ta1
      ta1_zone_list5
      {clock_resets = [||]; condition = [Le ("X", 5)]; action = 0;
       next_location = 1}
  in
  let
      expected =
    [[Gt ("X", 2)]]
  in
  if
    verify_zone_list found expected
  then
    "test36 passed"
  else
    "test36 failed"

let test37 =
  let
      found =
    successor_zones_from_predecessor
      ta1
      ta1_zone_list5
      {clock_resets = [||]; condition = [Gt ("X", 5)]; action = 0;
       next_location = 1}
  in
  let
      expected =
    [[Gt ("X", 5)]; [Gt ("X", 5)]]
  in
  if
    verify_zone_list found expected
  then
    "test37 passed"
  else
    "test37 failed"

let test38 =
  let
      found =
    successor_zones_from_predecessor
      ta1
      ta1_zone_list5
      {clock_resets = [|"X"|]; condition = [Gt ("X", 5)]; action = 0;
       next_location = 1}
  in
  let
      expected =
    [[Ge ("X", 0)]; [Ge ("X", 0)]]
  in
  if
    verify_zone_list found expected
  then
    "test38 passed"
  else
    "test38 failed"

let test39 =
  let
      found =
    new_successor_zones
      ta1
      ta1_zone_list4
      {clock_resets = [||]; condition = [Gt ("X", 2)]; action = 0;
       next_location = 1}
      ta1_zone_list5
  in
  let
      expected =
    [[Gt ("X", 2); Le ("X", 5)]; [Gt ("X", 5)]]
  in
  if
    verify_zone_list found expected
  then
    "test39 passed"
  else
    "test39 failed"
