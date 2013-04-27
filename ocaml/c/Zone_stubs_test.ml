open Test_base
open Zone_stubs
open Grammar_types
open UDBM_utilities

let test40 =
  let
      found = int_toList 3
  in
  if
    found = [(2, true); (1, true); (0, true)]
  then
    "test40 passed"
  else
    "test40 failed"

let verify_raw_t dim dbm expected =
  let
      found = dbm_toConstraintList dbm dim
  in
  List.length found = List.length expected &&
  List.for_all
    (function e ->
      (List.length (List.filter ((=) e) found))
      =
        (List.length (List.filter ((=) e) expected))
    )
    expected
    
let verify_raw_t_translation clock_names clock_constraint expected =
  match
    (clock_constraint_to_raw_t_option clock_names clock_constraint)
  with
  | None -> false
  | Some dbm -> verify_raw_t (1 + Array.length clock_names) dbm expected
    
let test41 =
  if
    verify_raw_t_translation [|"X"|] [Le ("X", 3)] [(1, 0, false, 3)]
  then
    "test41 passed"
  else
    "test41 failed"

let test42 =
  if
    verify_raw_t_translation [|"X"|] [Gt ("X", 3)] [(0, 1, true, -3)]
  then
    "test42 passed"
  else
    "test42 failed"

let test43 =
  if
    verify_raw_t_translation
      [|"X"; "Y"|]
      [Gt ("Y", 3); Ge ("Y", 3)]
      [(0, 2, true, -3)]
  then
    "test43 passed"
  else
    "test43 failed"

let test44 =
  if
    verify_raw_t_translation
      [|"X"; "Y"|]
      [Lt ("X", 3); Gt ("Y", 3); Ge ("Y", 3); Le ("X", 2)]
      [(0, 2, true, -3); (1, 0, false, 2)]
  then
    "test44 passed"
  else
    "test44 failed"

let test58 =
  let dim = 3 in
  if
    dbm_areEqual (dbm_init dim) (dbm_init dim) dim
  then
    "test58 passed"
  else
    "test58 failed"

let test59 =
  let dim = 3 in
  if
    dbm_areEqual (dbm_zero (dbm_init dim) dim) (dbm_zero (dbm_init dim) dim) dim
  then
    "test59 passed"
  else
    "test59 failed"

let test60 =
  let dim = 3 in
  if
    dbm_areEqual (dbm_init dim) (dbm_zero (dbm_init dim) dim) dim
  then
    "test60 failed"
  else
    "test60 passed"

let test72 =
  let dim = 3 in
  if dbm_isZeroIncluded (dbm_init dim) dim
  then
    "test72 passed"
  else
    "test72 failed"

let test73 =
  let dim = 3 in
  if dbm_isZeroIncluded (dbm_zero (dbm_init dim) dim) dim
  then
    "test73 passed"
  else
    "test73 failed"

let test74 =
  let dim = 3 in
  if
    dbm_isZeroIncluded (dbm_updateValue (dbm_init dim) dim 1 2) dim
  then
    "test74 failed"
  else
    "test74 passed"

let test75 =
  let dim = 3 in
  if
    dbm_isZeroIncluded
      (dbm_up
         (dbm_updateValue
            (dbm_updateValue (dbm_init dim) dim 2 2)
            dim
            1
            2)
         dim)
      dim
  then
    "test75 failed"
  else
    "test75 passed"
