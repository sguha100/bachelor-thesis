open Grammar_types
open Clock_constraint_utilities
open Graph_functions3_test
open Unit_constraint_intersection
open Zone_stubs
open Zone_stubs_test
open UDBM_utilities
open UDBM_utilities_test
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
    match_minimised [|"X"|] [False; False; False; Eq ("X", 7)] [False]
  then
    "test8 passed"
  else
    "test8 failed"
  
let test9 =
  if
    match_minimised [|"X"|] [True; True; True; Eq ("X", 7)] [Ge ("X", 7); Le ("X", 7)]
  then
    "test9 passed"
  else
    "test9 failed"
  
let test10 =
  if
    match_minimised [|"X"|] [Lt ("X", 5); Lt ("X", 7); Lt ("X", 3)] [Lt ("X", 3)]
  then
    "test10 passed"
  else
    "test10 failed"
  
let test11 =
  if
    match_minimised [|"X"|] [Le ("X", 5); Le ("X", 7); Le ("X", 3)] [Le ("X", 3)]
  then
    "test11 passed"
  else
    "test11 failed"
  
let test12 =
  if
    match_minimised [|"X"|] [Ge ("X", 5); Ge ("X", 7); Ge ("X", 3)] [Ge ("X", 7)]
  then
    "test12 passed"
  else
    "test12 failed"
  
let test13 =
  if
    match_minimised [|"X"|] [Gt ("X", 5); Gt ("X", 7); Gt ("X", 3)] [Gt ("X", 7)]
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
  print_string test40;
  print_newline ();
  print_string test41;
  print_newline ();
  print_string test42;
  print_newline ();
  print_string test43;
  print_newline ();
  print_string test44;
  print_newline ();
  print_string test45;
  print_newline ();
  print_string test46;
  print_newline ();
  print_string test47;
  print_newline ();
  exit 0
