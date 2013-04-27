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
             let
                 lz4 = out_adjacency l2 z2 a
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
                 lz3 = out_adjacency l1 z1 a
             in
             List.map
               (function z4 -> (lz3, z4))
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

module RelationCheckingFunctor =
  functor (Table: DP_TABLE_TYPE) ->
    functor (Relation: TA_RELATION_TYPE) ->
struct
  type table = Table.table
  let empty_table = Table.empty_table
  let lookup = Table.lookup
  let remove = Table.remove
  let insert = Table.insert
  let nodes_to_other_nodes = Relation.nodes_to_other_nodes
  let rec check_relation_on_nodes
      l1
      l2
      yes_table
      no_table
      z1
      z2
      =
    if
      (Printf.printf
         "Looking up (%s, %s) in yes_table\n"
         (ZVGQuotient2.node_name l1 z1)
         (ZVGQuotient2.node_name l2 z2)
      ;
       lookup
         yes_table
         l1
         l2
         (z1, z2)
      )
    then
      true
    else
      (if
          (Printf.printf
             "Looking up (%s, %s) in no_table\n"
             (ZVGQuotient2.node_name l1 z1)
             (ZVGQuotient2.node_name l2 z2)
          ;
           lookup
             no_table
             l1
             l2
             (z1, z2)
          )
       then
          false
       else
          (Printf.printf
             "Speculatively inserting (%s, %s) in yes_table\n"
             (ZVGQuotient2.node_name l1 z1)
             (ZVGQuotient2.node_name l2 z2)
          ;
           insert yes_table l1 l2 (z1, z2);
           let
               (x1, x2) =
             nodes_to_other_nodes
               l1
               l2
               z1
               z2             
           in
           match
             (List.for_all
                (function (z3, lz4) ->
                  List.exists
                    (function z4 ->
                      check_relation_on_nodes
                        l1
                        l2
                        yes_table
                        no_table
                        z3
                        z4
                    )
                    lz4
                )
                x1
                ,
              List.for_all
                (function (lz3, z4) ->
                  List.exists
                    (function z3 ->
                      check_relation_on_nodes
                        l1
                        l2
                        yes_table
                        no_table
                        z3
                        z4
                    )
                    lz3
                )
                x2
             )
           with
           | (true, true) ->
             Printf.printf
               "Letting  (%s, %s) stay in yes_table\n"
               (ZVGQuotient2.node_name l1 z1)
               (ZVGQuotient2.node_name l2 z2)
             ;
             true
           | (t1, t2) ->
             Printf.printf
               ("Inserting  (%s, %s) in no_table after removal from yes_table after getting (%s, %s)\n")
               (ZVGQuotient2.node_name l1 z1)
               (ZVGQuotient2.node_name l2 z2)
               (if t1 then "true" else "false")
               (if t2 then "true" else "false")
             ;
             remove yes_table l1 l2 (z1, z2);
             insert no_table l1 l2 (z1, z2);
             false
          )
      )
  let check_relation_on_timed_automata
      ta1
      ta2
      q1
      q2
      =
    let yes_table = empty_table () in
    let no_table = empty_table () in
    let
        z1 =
      try (
        List.find
          (function z1 ->
            List.exists
              (function node_ref1 ->
                (node_ref1.zone_location2 = ta1.numinit)
                &&
                  (dbm_isZeroIncluded node_ref1.zone_constraint2 (1+ta1.numclocks))
              )
              q1.ZVGQuotient2.nodes.(z1).ZVGQuotient2.node_ref_list
          )
          (ZVGQuotient2.nodes q1)
      ) with
      | Not_found -> invalid_arg "We caught Not_found while searching for z1."
    in
    Printf.printf "%s\n" "After selecting z1:";
    List.iter
      (function node_ref1 ->
        Printf.printf
          "node_ref1 = %s %s\n"
          (string_of_int node_ref1.zone_location2)
          (raw_t_to_string ta1.clock_names node_ref1.zone_constraint2)
        ;
      )
      q1.ZVGQuotient2.nodes.(z1).ZVGQuotient2.node_ref_list;
    let
        z2 =
      try (
        List.find
          (function z2 ->
            List.exists
              (function node_ref2 ->
                (node_ref2.zone_location2 = ta2.numinit)
                &&
                  (dbm_isZeroIncluded node_ref2.zone_constraint2 (1+ta2.numclocks))
              )
              q2.ZVGQuotient2.nodes.(z2).ZVGQuotient2.node_ref_list
          )
          (ZVGQuotient2.nodes q2)
      ) with
      | Not_found -> invalid_arg "We caught Not_found while searching for z2."
    in
    Printf.printf "%s\n" "After selecting z2:";
    List.iter
      (function node_ref2 ->
        Printf.printf
          "node_ref2 = %s %s\n"
          (string_of_int node_ref2.zone_location2)
          (raw_t_to_string ta2.clock_names node_ref2.zone_constraint2)
        ;
      )
      q2.ZVGQuotient2.nodes.(z2).ZVGQuotient2.node_ref_list;
    check_relation_on_nodes
      q1
      q2
      yes_table
      no_table
      z1
      z2
end
      
module STABChecker = RelationCheckingFunctor (Table_using_list) (STAB)
module TADBChecker = RelationCheckingFunctor (Table_using_list) (TADB)
module TAOBChecker = RelationCheckingFunctor (Table_using_list) (TAOB)
