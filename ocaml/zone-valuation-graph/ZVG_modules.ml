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

module ZVGQuotient2 = ZVGLTS2.Quotient_LTS

module ZVGQuotientLTS2 = LTS (ZVGQuotient2)

let lts_of_zone_valuation_graph ta =
  {ZVGLT2.action_count = ta.numactions;
   ZVGLT2.nodes = generate_zone_valuation_graph ta;
   ZVGLT2.clock_names = ta.clock_names
  }

type half_key = ZVGQuotient2.node_ref_t

module type DP_TABLE_TYPE =
sig
  type table (*The 'a type is the co-ordinate type for the table.*)
  val empty_table: unit -> table
  val lookup:
    table ->
    ZVGQuotient2.lts_t ->
    ZVGQuotient2.lts_t ->
    (half_key * half_key) ->
    bool (*Whether it was found or not.*)
  val remove:
    table ->
    ZVGQuotient2.lts_t ->
    ZVGQuotient2.lts_t ->
    (half_key * half_key) ->
    bool (*Whether we needed to remove it or not.*)
  val insert:
    table ->
    ZVGQuotient2.lts_t ->
    ZVGQuotient2.lts_t ->
    (half_key * half_key) ->
    bool (*Whether we needed to remove something before inserting it.*)
end
  
module type TA_RELATION_TYPE =
sig
  val nodes_to_other_nodes:
    ZVGQuotient2.lts_t ->
    ZVGQuotient2.lts_t ->
    half_key ->
    half_key ->
    (((half_key * (half_key list)) list) * (((half_key list) * half_key) list))
end

module Table_using_list =
struct
  type table = (half_key * half_key) list ref
  let empty_table: unit -> table = function () -> ref []
  let lookup table l1 l2 (h1, h2) =
    try
      List.find
        (function (h3, h4) ->
          ZVGQuotient2.node_equality l1 h1 h3 && ZVGQuotient2.node_equality l2 h2 h4
        )
        !table;
      true
    with
    | Not_found -> false
  let remove table l1 l2 (h1, h2) =
    if
      lookup table l1 l2 (h1, h2)
    then
      (table :=
         List.filter
          (function (h3, h4) ->
            not
              (ZVGQuotient2.node_equality l1 h1 h3 && ZVGQuotient2.node_equality l2 h2 h4)
          )
          !table;
       true)
    else false
  let insert table l1 l2 (h1, h2) =
    let
        result = remove table l1 l2 (h1, h2)
    in
    (table := (h1, h2)::!table);
    result
end

let out_adjacency l z a
    =
  List.filter
    (function zz ->
      List.exists
        (ZVGQuotient2.node_equality l z)
        (ZVGQuotient2.in_adjacency l zz a)
    )
    (ZVGQuotient2.nodes l)

let out_adjacency_with_delay_earlier l z a
    =
  let rec f ol =
    let ext =
      List.filter
        (function z ->
          not
            (List.exists
               (ZVGQuotient2.node_equality l z)
               ol
            )
        )
        (List.concat
           (List.map
              (function z -> out_adjacency l z (-1))
              ol
           )
        )
    in
    match ext with
    | [] -> ol
    | _ -> f (ext@ol)
  in
  List.concat
    (List.map
       (function z -> out_adjacency l z a)
       (f [z])
    )

let out_adjacency_with_delay_earlier_and_later l z a =
  let rec f ol =
    let ext =
      List.filter
        (function z ->
          not
            (List.exists
               (ZVGQuotient2.node_equality l z)
               ol
            )
        )
        (List.concat
           (List.map
              (function z -> out_adjacency l z (-1))
              ol
           )
        )
    in
    match ext with
    | [] -> ol
    | _ -> f (ext@ol)
  in
  f (out_adjacency_with_delay_earlier l z a)

module STAB =
struct
  let nodes_to_other_nodes
      l1
      l2
      z1
      z2
      = 
    (
      List.concat
        (List.map
           (function a ->
             Printf.printf "a = %s\n" (ZVGQuotient2.expand_action l1 a);
             let
                 lz4 = out_adjacency l2 z2 a
             in
             List.map
               (function z3 ->
                 Printf.printf "z3 = %s\n" (ZVGQuotient2.node_name l1 z3);
                 (z3, lz4)
               )
               (out_adjacency l1 z1 a)
           )
           (ZVGQuotient2.actions l1)
        )
        ,
      List.concat
        (List.map
           (function a ->
             Printf.printf "a = %s\n" (ZVGQuotient2.expand_action l2 a);
             let
                 lz3 = out_adjacency l1 z1 a
             in
             List.map
               (function z4 ->
                 Printf.printf "z4 = %s\n" (ZVGQuotient2.node_name l2 z4);
                 (lz3, z4))
               (out_adjacency l2 z2 a)
           )
           (ZVGQuotient2.actions l2)
        )
    )
end

module TADB =
struct
  let nodes_to_other_nodes
      l1
      l2
      z1
      z2
      = 
    (
      List.concat
        (List.map
           (function a ->
             let
                 lz4 = out_adjacency_with_delay_earlier l2 z2 a
             in
             List.map
               (function z3 -> (z3, lz4))
               (out_adjacency l1 z1 a)
           )
           (ZVGQuotient2.actions l1)
        )
        ,
      List.concat
        (List.map
           (function a ->
             let
                 lz3 = out_adjacency_with_delay_earlier l1 z1 a
             in
             List.map
               (function z4 -> (lz3, z4))
               (out_adjacency l2 z2 a)
           )
           (ZVGQuotient2.actions l2)
        )
    )
end

module TAOB =
struct
  let nodes_to_other_nodes
      l1
      l2
      z1
      z2
      = 
    (
      List.concat
        (List.map
           (function a ->
             let
                 lz4 = out_adjacency_with_delay_earlier_and_later l2 z2 a
             in
             List.map
               (function z3 -> (z3, lz4))
               (out_adjacency l1 z1 a)
           )
           (ZVGQuotient2.actions l1)
        )
        ,
      List.concat
        (List.map
           (function a ->
             let
                 lz3 = out_adjacency_with_delay_earlier_and_later l1 z1 a
             in
             List.map
               (function z4 -> (lz3, z4))
               (out_adjacency l2 z2 a)
           )
           (ZVGQuotient2.actions l2)
        )
    )
end
      
