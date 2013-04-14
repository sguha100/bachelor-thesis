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

let test41 =
  if
    (match
        (clock_constraint_to_raw_t_option [|"X"|] [Le ("X", 3)])
     with
     | None -> false
     | Some dbm ->
       match
         dbm_toConstraintList dbm 2
       with
       | [(i, j, strictness, bound)] ->
          (i = 1) && (j = 0) && not(strictness) && (bound = 3)
       | _ -> false
    )
  then
    "test41 passed"
  else
    "test41 failed"
