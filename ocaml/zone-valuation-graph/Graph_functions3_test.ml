open Grammar_types
open Graph_functions3
open Test_base

let verify_zone_lists ta =
  let
      zone_list_array = init_zone_list_array ta
  in
  (Array.length zone_list_array = ta.numlocations)
  &&
    (let
        truth = ref true
     in
     Array.iteri
       (function i -> function zone_list ->
         if
           (i = ta.numinit)
         then
           truth :=
             match
               zone_list
             with
             | [zone] ->
               (zone.zone_location1 = i &&
                   (match_clock_constraints
                      zone.zone_constraint1
                      (pseudo_future
                         (List.map
                            (function cn -> Eq (cn, 0))
                            (Array.to_list ta.clock_names)
                         )
                      )
                   )
               )
             | _ -> false
         else
           truth := (zone_list = [])
       )
       zone_list_array;
     !truth
    )

