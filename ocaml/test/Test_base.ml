open Grammar_types
open Clock_constraint_utilities

let match_clock_constraints found expected =
  (List.for_all
     (function expectedelem ->
       (List.exists
         ((=) expectedelem)
         found
       )
     ||
         (expectedelem = True)
     )
     expected
  )
  &&
    (List.for_all
       (function foundelem ->
         (List.exists
            ((=) foundelem)
            expected)
         ||
           (foundelem = True)
       )
       found
    )

let match_minimised clock_constraint clock_name_list expected =
  (minimise_clock_constraint clock_constraint clock_name_list) = expected
