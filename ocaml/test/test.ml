open Grammar_types
open Graph_functions
open Unit_constraint_intersection

let test1 a1 b1 =
  (List.for_all
    (function b1elem ->
      List.exists
        ((=) b1elem)
        a1
    )
    b1
  )
    &&
    (List.for_all
       (function a1elem ->
         (List.exists
           ((=) a1elem)
           b1)
         ||
           (a1elem = True)
       )
       a1
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

let _ =
  print_string test2;
  print_newline ();
  print_string test3;
  print_newline ();
  print_string test4;
  print_newline ();
  print_string test5;
  print_newline ();
  exit 0
