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
      function l -> function zone -> ((string_of_int
                                        zone.zone_location) ^ " " ^
                                         (string_of_clock_constraint zone.zone_constraint) )
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
      List.map
        (function a ->
          (a,
           let
               all_zones_with_departures =
             List.concat
               (Array.to_list l.nodes)
           in
           let
               useful_zones_with_departures =
             List.filter
               (function (zone1, dl) ->
                 List.exists
                   (function (departure, zone_list) ->
                     departure.action = a
                       &&
                         (List.mem zone zone_list)
                   )
                   dl
               )
               all_zones_with_departures
           in
           let
               useful_zones =
             List.map
               (function (zone1, _) -> zone1)
               useful_zones_with_departures
           in
           useful_zones
          )
        )
        (actions l)
  end

module ZVGLTS = LTS (ZVGLT)

let lts_of_zone_valuation_graph ta =
  {ZVGLT.action_count = ta.numactions;
   ZVGLT.nodes = generate_zone_valuation_graph ta
  }
