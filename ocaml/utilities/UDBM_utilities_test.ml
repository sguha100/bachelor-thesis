open Grammar_types
open Test_base
open UDBM_utilities
open Zone_stubs
open Zone_stubs_test

let dim01 = 3

let dbm01 =
  (dbm_updateValue
       (dbm_updateValue
          (dbm_init dim01)
          dim01
          2
          3
       )
       dim01
       1
       2
    )

let test45 =
  if
    verify_raw_t
      dim01
      dbm01
      [(2, 0, false, 3); (1, 2, false, -1); (0, 1, false, -2)]
  then
    "test45 passed"
  else
    "test45 failed"

let dim02 = dim01

let dbm02 = raw_t_without_reset_clocks [|"X"; "Y"|] [|"X"|] dbm01 

let test46 =
  if
    verify_raw_t
      dim02
      dbm02
      [(2, 0, false, 3); (0, 2, false, -3)]
  then
    "test46 passed"
  else
    "test46 failed"

let dim03 = dim01

let dbm03 = raw_t_after_clock_resets [|"X"; "Y"|] [|"X"|] dbm01 

let test47 =
  if
    verify_raw_t
      dim03
      dbm03
      [(2, 0, false, 3); (1, 2, false, -3)]
  then
    "test47 passed"
  else
    ("test47 failed, found  = " ^ (raw_t_to_string [|"X"; "Y"|] dbm02))
