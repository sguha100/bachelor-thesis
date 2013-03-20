(* File calc.ml *)
open Parse_timed_automaton
open Graph_functions
open Grammar_types
open UDBM_utilities
open Zone_stubs
open Fernandez_modules

let _ =
  let result = parse_timed_automaton stdin in
  let
      unit = (Printf.printf "Timed automaton parsed successfully!\n");
    (flush stdout)
  in
  let g = generate_zone_valuation_graph result in
  let
      unit = Printf.printf "Zone-valuation graph generated successfully!\n"
  in
  Printf.printf "#locations %s\n" (string_of_int result.numlocations);
  Printf.printf "#trans %s\n" (string_of_int result.numtrans);
  Printf.printf "#clocks %s\n" (string_of_int result.numclocks);
  Printf.printf "#actions %s\n" (string_of_int result.numactions);
  Printf.printf "#init %s\n" (string_of_int result.numinit);
  let len = (Array.length g) in
  for i = 0 to len - 1 do
    (* Printf.printf *)
    (*   "i = %s, %s\n" *)
    (*   (string_of_int *)
    (*      i) *)
    (*   (string_of_int *)
    (*      (List.length *)
    (*         g.(i) *)
    (*      )); *)
    List.iter
      (function (zone, edges_of_zone) ->
        Printf.printf "\nlocation: %s\n" (string_of_int i);
        Printf.printf
          "invar: %s\n"
          (String.concat
             " AND "
             (List.map
                (function
                | True -> "TRUE"
                | False -> "FALSE"
                | Lt (cn, n) -> cn ^ " < " ^ (string_of_int n)
                | Le (cn, n) -> cn ^ " <= " ^ (string_of_int n)
                | Eq (cn, n) -> cn ^ " = " ^ (string_of_int n)
                | Ge (cn, n) -> cn ^ " >= " ^ (string_of_int n)
                | Gt (cn, n) -> cn ^ " > " ^ (string_of_int n)
                )
                zone.zone_constraint
             )
          )
        ;
        Printf.printf "trans:\n";
        List.iter
          (function (departure, _) ->
            Printf.printf
              "ACT %s; RESET { %s }; goto %s\n"
              (string_of_int departure.action)
              (String.concat
                 " "
                 (Array.to_list departure.clock_resets)
              )
              (string_of_int departure.next_location)
          )
          edges_of_zone
      )
      g.(i)
    ;
  done;
  flush stdout;
  exit 0

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

let lts_of_zone_valuation_graph ta tla =
  {ZVGLT.action_count = ta.numactions;
   ZVGLT.nodes = generate_zone_valuation_graph ta
  }
