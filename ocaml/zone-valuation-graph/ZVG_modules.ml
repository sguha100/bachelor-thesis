open Grammar_types
open UDBM_utilities
open Clock_constraint_utilities
open Graph_functions2
open Fernandez_modules
open Zone_stubs

open Graph

module ZVGLT2 =
struct
  type node_ref_t = zone_using_raw_t
  type action_t = int
  type lts_t = {nodes:((zone_using_raw_t * ((transition *
                                              (zone_using_raw_t list)) list)) list) array;
                action_count: int;
                clock_names: string array
               }
  let node_equality =
    function l -> function zone1 -> function zone2 ->
      (let dim = 1 + Array.length l.clock_names in
       zone1.zone_location2 = zone2.zone_location2 &&
          (dbm_areEqual zone1.zone_constraint2 zone2.zone_constraint2 dim)
      )
  let node_name =
    function l -> function zone -> ((string_of_int
                                       zone.zone_location2) ^ " " ^
                                       (raw_t_to_string l.clock_names zone.zone_constraint2) )
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
      Array.to_list
        (Array.init
           (1 + l.action_count) (*The extra action is -1, for time transitions.*)
           (function a-> a - 1)
        )
  let in_adjacency = function l -> function zone ->
    (function a ->
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
                  (List.exists
                     (node_equality l zone)
                     zone_list
                  )
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
end

module ZVGLTS2 = LTS (ZVGLT2)

let lts_of_zone_valuation_graph ta =
  {ZVGLT2.action_count = ta.numactions;
   ZVGLT2.nodes = generate_zone_valuation_graph ta;
   ZVGLT2.clock_names = ta.clock_names
  }

