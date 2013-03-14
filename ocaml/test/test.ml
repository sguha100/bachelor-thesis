open Grammar_types
open Graph_functions
open Unit_constraint_intersection

let test1 =
  let
      a1 = unit_constraint_intersection (Lt ("x1", 3)) (Lt ("x2", 4))
  in
  (List.exists
     ((=) (Lt ("x1", 3)))
     a1
  )
    &&
    (List.exists
       ((=) (Lt ("x2", 4)))
       a1
    )
    
