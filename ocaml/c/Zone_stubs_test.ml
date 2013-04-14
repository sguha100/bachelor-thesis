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

let verify_raw_t_translation clock_names clock_constraint expected =
  (match
      (clock_constraint_to_raw_t_option clock_names clock_constraint)
   with
   | None -> false
   | Some dbm ->
     let
         found = dbm_toConstraintList dbm (1 + Array.length clock_names)
     in
     (List.for_all
        (function e ->
          (List.length (List.filter ((=) e) found))
          =
            (List.length (List.filter ((=) e) expected))
        )
        expected
     )
  )

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
