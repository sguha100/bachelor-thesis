open Grammar_types
open Grammar_types_clutter
open Clock_constraint_clutter
open Test_base
open Test_base_clutter

let match_minimised clock_names clock_constraint expected =
  match_clock_constraints (minimise_clock_constraint clock_names clock_constraint) expected

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
