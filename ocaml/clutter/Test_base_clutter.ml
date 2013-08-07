open Grammar_types
open Clock_constraint_utilities
open Clock_constraint_clutter
open Test_base

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
