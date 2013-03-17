open Grammar_types
open Graph_functions
open Unit_constraint_intersection
open Zone_stubs
open UDBM_utilities

let test1 found expected =
  (List.for_all
     (function expectedelem ->
       List.exists
         ((=) expectedelem)
         found
     )
     expected
  )
  &&
    (List.for_all
       (function foundelem ->
         (List.exists
            ((=) foundelem)
            expected)
         ||
           (foundelem = True)
       )
       found
    )

let test2 = 
  if
    (test1
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
    (test1
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
    (test1
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
    (test1
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
      {zone_location = 0;
       zone_constraint = [Le ("X", 8); Ge ("X", 2)]
      }
      [Le ("X", 6); Ge ("X", 4)]
  in
  (Printf.sprintf "test6: %s" (string_of_int (List.length a)))

let test7 clock_constraint clock_name_list expected =
  (minimise_clock_constraint clock_constraint clock_name_list) = expected

let test8 =
  if
    test7 [False; False; False; Eq ("X", 7)] ["X"] [False]
  then
    "test8 passed"
  else
    "test8 failed"
  
let test9 =
  if
    test7 [True; True; True; Eq ("X", 7)] ["X"] [Eq ("X", 7)]
  then
    "test9 passed"
  else
    "test9 failed"
  
let test10 =
  if
    test7 [Lt ("X", 5); Lt ("X", 7); Lt ("X", 3)] ["X"] [Lt ("X", 3)]
  then
    "test10 passed"
  else
    "test10 failed"
  
let test11 =
  if
    test7 [Le ("X", 5); Le ("X", 7); Le ("X", 3)] ["X"] [Le ("X", 3)]
  then
    "test11 passed"
  else
    "test11 failed"
  
let test12 =
  if
    test7 [Ge ("X", 5); Ge ("X", 7); Ge ("X", 3)] ["X"] [Ge ("X", 7)]
  then
    "test12 passed"
  else
    "test12 failed"
  
let test13 =
  if
    test7 [Gt ("X", 5); Gt ("X", 7); Gt ("X", 3)] ["X"] [Gt ("X", 7)]
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
  exit 0
