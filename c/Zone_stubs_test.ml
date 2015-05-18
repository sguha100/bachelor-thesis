open Test_base
open Zone_stubs
open Grammar_types
open UDBM_utilities.Test
    
let test41 =
  if
    verify_dbm_translation (String_map.singleton "X" 0) [Le ("X", 3)] [(1, 0, false, 3)]
  then
    "test41 passed"
  else
    "test41 failed"

let test42 =
  if
    verify_dbm_translation (String_map.singleton "X" 0) [Gt ("X", 3)] [(0, 1, true, -3)]
  then
    "test42 passed"
  else
    "test42 failed"

let test43 =
  if
    verify_dbm_translation
      (String_map.add "Y" 1 (String_map.singleton "X" 0))
      [Gt ("Y", 3); Ge ("Y", 3)]
      [(0, 2, true, -3)]
  then
    "test43 passed"
  else
    "test43 failed"

let test44 =
  if
    verify_dbm_translation
      (String_map.add "Y" 1 (String_map.singleton "X" 0))
      [Lt ("X", 3); Gt ("Y", 3); Ge ("Y", 3); Le ("X", 2)]
      [(0, 2, true, -3); (1, 0, false, 2)]
  then
    "test44 passed"
  else
    "test44 failed"

let test58 =
  let dim = 3 in
  if
    dbm_init dim =  dbm_init dim
  then
    "test58 passed"
  else
    "test58 failed"

let test59 =
  let dim = 3 in
  if
    dbm_zero (dbm_init dim) = dbm_zero (dbm_init dim)
  then
    "test59 passed"
  else
    "test59 failed"

let test60 =
  let dim = 3 in
  if
    dbm_init dim = dbm_zero (dbm_init dim)
  then
    "test60 failed"
  else
    "test60 passed"

let test72 =
  let dim = 3 in
  if
    dbm_isZeroIncluded (dbm_init dim)
  then
    "test72 passed"
  else
    "test72 failed"

let test73 =
  let dim = 3 in
  if
    dbm_isZeroIncluded (dbm_zero (dbm_init dim))
  then
    "test73 passed"
  else
    "test73 failed"

let test74 =
  let dim = 3 in
  if
    dbm_isZeroIncluded (dbm_updateValue (dbm_init dim) 1 2)
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
            (dbm_updateValue (dbm_init dim) 2 2)
            1
            2)
      )
  then
    "test75 failed"
  else
    "test75 passed"

let test80 = 
  if
    dbm_init 2 < dbm_init 3 || dbm_init 2 > dbm_init 3
  then
    "test80 passed"
  else
    "test80 failed"

let test81 = 
  if
    dbm_init 3 = dbm_init 3
  then
    "test81 passed"
  else
    "test81 failed"

let test82 = 
  if
    dbm_updateValue (dbm_init 3) 1 5 <> dbm_updateValue (dbm_init 3) 2 5
  then
    "test82 passed"
  else
    "test82 failed"

let test83 = 
  if
    dbm_updateValue (dbm_updateValue (dbm_init 3) 1 5) 2 5
    = dbm_updateValue (dbm_updateValue (dbm_init 3) 2 5) 1 5
  then
    "test83 passed"
  else
    "test83 failed"

let dbm11 = 
  dbm_constrain_using_tuple (dbm_init 3) (2, 1, false, 5)

let test84 = 
  if
    verify_dbm 3 dbm11 [(2, 1, false, 5)]
  then
    "test84 passed"
  else
    "test84 failed"

let dbm12 = 
  dbm_constrain_using_tuple dbm11 (2, 1, false, 4)

let test85 = 
  if
    verify_dbm 3 dbm12 [(2, 1, false, 4)]
  then
    "test85 passed"
  else
    "test85 failed"

let dbm13 = 
  dbm_constrain_using_tuple (dbm_init 3) (1, 2, false, -5)

let test86 = 
  if
    not (dbm_haveIntersection dbm12 dbm13)
  then
    "test86 passed"
  else
    "test86 failed"

let test87 = 
  if
    dbm_haveIntersection dbm12 dbm11
  then
    "test87 passed"
  else
    "test87 failed"

let dbm14 = 
  dbm_intersection dbm11 dbm13

let test88 = 
  if
    verify_dbm 3 dbm14 [(2, 1, false, 5); (1, 2, false, -5)]
  then
    "test88 passed"
  else
    "test88 failed"

let dbm15 =
  dbm_freeClock dbm14 1

let test89 = 
  if
    verify_dbm 3 dbm15 [(0, 2, false, -5)]
  then
    "test89 passed"
  else
    "test89 failed"

let dbm16 =
  dbm_freeClock dbm14 2

let test90 = 
  if
    verify_dbm 3 dbm16 []
  then
    "test90 passed"
  else
    "test90 failed"

let dbm17 = dbm_up dbm14

let test91 = 
  if
    verify_dbm 3 dbm17 [(2, 1, false, 5); (1, 2, false, -5)]
  then
    "test91 passed"
  else
    "test91 failed"

let dbm18 = dbm_up dbm15

let test92 = 
  if
    verify_dbm 3 dbm18 [(0, 2, false, -5)]
  then
    "test92 passed"
  else
    "test92 failed"

let dbm19 =
  dbm_constrain_using_tuple
    (dbm_constrain_using_tuple
       (dbm_constrain_using_tuple
	  (dbm_init 3)
	  (1, 2, false, 0)
       )
       (2, 1, false, 0)
    )
    (1, 0, true, 1)

let test151 () =
  if
    (dbm_isZeroIncluded dbm19)
  then
    "test151 passed"
  else
    "test151 failed"
