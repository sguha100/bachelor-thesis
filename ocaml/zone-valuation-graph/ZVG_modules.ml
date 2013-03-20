open Grammar_types
open Graph_functions
open Fernandez_modules

module ZVGLT =
  struct
    type node_ref_t = zone_using_list
    type action_t = int
    type lts_t = {nodes:((zone_using_list * ((transition *
                                                (zone_using_list list)) list)) list) array;
                  action_count: int}
    let node_name =
      function l -> function zone -> string_of_int zone.zone_location (*This is bad!*)
    let expand_action = function l -> function a -> string_of_int a
    let nodes =
      function l ->
        List.map
          (function (zone, _) -> zone)
          (List.concat
             (Array.to_list
                l.nodes
             )
          )
    let actions =
      function l ->
        Array.to_list (Array.init l.action_count (function a-> a))
    let in_adjacency = function l -> function zone ->
      let ll = 
        (match
            (List.find
               (function (zone1, _) -> zone1 = zone)
               l.nodes.(zone.zone_location)
            )
         with
           (_, ll) -> ll
        )
      in
      Array.to_list
        (Array.init
           l.action_count
           (function a ->
             (a,
              List.concat
                (List.map
                   (function (departure, arrival_zones) ->
                     if
                       (a = departure.action)
                     then
                       arrival_zones
                     else
                       []
                   )
                   ll
                )
             )
           )
        )
  end

module ZVGLTS = LTS (ZVGLT)

let lts_of_zone_valuation_graph ta =
  {ZVGLT.action_count = ta.numactions;
   ZVGLT.nodes = generate_zone_valuation_graph ta
  }
