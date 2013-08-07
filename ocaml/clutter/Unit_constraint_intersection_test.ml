open Grammar_types
open Test_base
open Test_base_clutter
open Unit_constraint_intersection

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
