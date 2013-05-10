open ZVG_modules
open Grammar_types
open Zone_stubs
open UDBM_utilities

module Ordered_node_ref =
struct
  type t = ZVGQuotient2.node_ref_t
  let compare = Pervasives.compare
end

module Node_ref_set = Set.Make (Ordered_node_ref)

module Ordered_action_option_with_node =
struct
  type t = (ZVGQuotient2.action_t option) * ZVGQuotient2.node_ref_t
  let compare = Pervasives.compare
end

module Action_option_with_node_map = Map.Make (Ordered_action_option_with_node)

module Ordered_node_ref_pair =
struct
  type t = ZVGQuotient2.node_ref_t * ZVGQuotient2.node_ref_t
  let compare = Pervasives.compare
end

module Node_ref_pair_set = Set.Make (Ordered_node_ref_pair)

type succession = Succession1 | Succession2 | Succession_both

(*The action_pair field is required because the same child may occur
  for different pairs of actions.*)
type product_transition = {
  common_action : ZVGQuotient2.action_t option;
  succession : succession;
  child : ZVGQuotient2.node_ref_t * ZVGQuotient2.node_ref_t
}

type succession_element = {
  product_transition: product_transition;
  successor_transition_list: product_transition list;
  map1: bool Action_option_with_node_map.t;
  map2: bool Action_option_with_node_map.t;
}

type information_wrapper = {
  not_related: Node_ref_pair_set.t;
  visited: Node_ref_pair_set.t;
  multiply_visited: Node_ref_pair_set.t;
  stable: bool;
  stack_for_current_path: succession_element Stack.t;
  verify_roots1: bool;
  verify_roots2: bool;
}

let lookup_in_stack s (p, q) =
  let
      found = ref false
  in
  Stack.iter
    (function e ->
      if e.product_transition.child = (p, q) then found := true else ()
    )
    s;
  !found

let get_initial_location ta q =
  try (
    List.find
      (function z ->
        List.exists
          (function node_ref1 ->
            (node_ref1.zone_location2 = ta.numinit)
            &&
              (dbm_isZeroIncluded node_ref1.zone_constraint2)
          )
          q.ZVGQuotient2.nodes.(z).ZVGQuotient2.node_ref_list
      )
      (ZVGQuotient2.nodes q)
  ) with
  | Not_found -> invalid_arg "We caught Not_found while searching for z."

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

  let initialise ta1 ta2 l1 l2 not_related =
      let
          s = Stack.create ()
      in
      let
          z1 = get_initial_location ta1 l1
      in
      let
          z2 = get_initial_location ta2 l2
      in
      let
          (x1, x2) =
        nodes_to_other_nodes
          l1
          l2
          z1
          z2
      in
      Stack.push
        {product_transition =
            {common_action = None;
             succession = Succession_both;
             child = (z1, z2)
            };
         successor_transition_list =
            (List.concat
               (List.map
                  (function (z3, a, lz4) ->
                    List.map
                      (function z4 ->
                        {common_action = Some a;
                         succession = Succession1;
                         child = (z3, z4)
                        }
                      )
                      lz4
                  )
                  x1
               )
            ) @
            (List.concat
               (List.map
                  (function (lz3, a, z4) ->
                    List.map
                      (function z3 ->
                        {common_action = Some a;
                         succession = Succession2;
                         child = (z3, z4)
                        }
                      )
                      lz3
                  )
                  x2
               )
            );
             map1 =
            List.fold_left
              (function map1 ->function (z3, a, lz4) ->
                Action_option_with_node_map.add (Some a, z3) false map1
              )
              Action_option_with_node_map.empty
              x1
             ;
             map2 =
            List.fold_left
              (function map1 ->function (lz3, a, z4) ->
                Action_option_with_node_map.add (Some a, z4) false map1
              )
              Action_option_with_node_map.empty
              x2
             ;
        }
        s;
      {
        not_related = not_related;
        visited = Node_ref_pair_set.empty;
        multiply_visited = Node_ref_pair_set.empty;
        stable = true;
        stack_for_current_path = s;
        verify_roots1 = false;
        verify_roots2 = false;
      }

  let move_forward wrapper next_successor_transition =
    let f1 wrapper next_successor_transition =
      try
        let
            succession_element = Stack.pop wrapper.stack_for_current_path
        in
        Stack.push
        (match
          next_successor_transition.succession
        with
        | Succession1 ->
          {
            product_transition = succession_element.product_transition;
            successor_transition_list =
              succession_element.successor_transition_list;
            map1 =
              Action_option_with_node_map.add
                (next_successor_transition.common_action,
                 fst next_successor_transition.child
                )
                true
                succession_element.map1
            ;
            map2 = succession_element.map2;
          }
        | Succession2 ->
          {
            product_transition = succession_element.product_transition;
            successor_transition_list =
              succession_element.successor_transition_list;
            map1 = succession_element.map1;
            map2 =
              Action_option_with_node_map.add
                (next_successor_transition.common_action,
                 snd next_successor_transition.child
                )
                true
                succession_element.map2
            ;
          }
        | Succession_both ->
          {
            product_transition = succession_element.product_transition;
            successor_transition_list =
              succession_element.successor_transition_list;
            map1 =
              Action_option_with_node_map.add
                (next_successor_transition.common_action,
                 fst next_successor_transition.child
                )
                true
                succession_element.map1
            ;
            map2 =
              Action_option_with_node_map.add
                (next_successor_transition.common_action,
                 snd next_successor_transition.child
                )
                true
                succession_element.map2
            ;
          }
        )
          wrapper.stack_for_current_path;
        wrapper
      with
      | Stack.Empty ->
        match
          next_successor_transition.succession
        with
        | Succession1 ->
          {
            not_related = wrapper.not_related;
            visited = wrapper.visited;
            multiply_visited = wrapper.multiply_visited;
            stable = wrapper.stable;
            stack_for_current_path = wrapper.stack_for_current_path;
            verify_roots1 = true;
            verify_roots2 = wrapper.verify_roots2;
          }
        | Succession2 ->
          {
            not_related = wrapper.not_related;
            visited = wrapper.visited;
            multiply_visited = wrapper.multiply_visited;
            stable = wrapper.stable;
            stack_for_current_path = wrapper.stack_for_current_path;
            verify_roots1 = wrapper.verify_roots1;
            verify_roots2 = true;
          }
        | Succession_both ->
          {
            not_related = wrapper.not_related;
            visited = wrapper.visited;
            multiply_visited = wrapper.multiply_visited;
            stable = wrapper.stable;
            stack_for_current_path = wrapper.stack_for_current_path;
            verify_roots1 = true;
            verify_roots2 = true;
          }
    in
    if
      Node_ref_pair_set.mem
        next_successor_transition.child
        wrapper.visited
    then
      if
        Node_ref_pair_set.mem
          next_successor_transition.child
          wrapper.not_related
      then
        (*We found out really quick that this child is in not_related,
          because of prior computations.*)
        wrapper 
      else
        (*Prior computations suggest that the child is in related,
          so we sent this information one level below in the stack.*)
        f1 wrapper next_successor_transition
    else
      if
        lookup_in_stack
          wrapper.stack_for_current_path
          next_successor_transition.child
      then
        (*Cycle detection suggests that the child is in related,
          so we sent this information one level below in the stack.*)
        f1
          {
            not_related = wrapper.not_related;
            visited = wrapper.visited;
            multiply_visited =
              Node_ref_pair_set.add
                next_successor_transition.child
                wrapper.multiply_visited;
            stable = wrapper.stable;
            stack_for_current_path = wrapper.stack_for_current_path;
            verify_roots1 = wrapper.verify_roots1;
            verify_roots2 = wrapper.verify_roots2;
          }
          next_successor_transition
      else
        (*Wrong*)
        wrapper
          
  let backtrack wrapper =
    (*Wrong!*)
    wrapper

  let partial_dfs ta1 ta2 l1 l2 not_related =
    let
        init = initialise ta1 ta2 l1 l2 not_related
    in
    let rec empty_stack wrapper =
      try
        let
            succession_element = Stack.pop wrapper.stack_for_current_path
        in
        match
          succession_element.successor_transition_list
        with
        | [] ->
          Stack.push
            succession_element
            wrapper.stack_for_current_path;
          backtrack
            {
              not_related = wrapper.not_related;
              visited =
                Node_ref_pair_set.add
                  succession_element.product_transition.child
                  wrapper.visited;
              multiply_visited = wrapper.multiply_visited;
              stable = wrapper.stable;
              stack_for_current_path = wrapper.stack_for_current_path;
              verify_roots1 = wrapper.verify_roots1;
              verify_roots2 = wrapper.verify_roots2;
            }
        | next_successor_transition::remaining_succession_elements ->
          Stack.push
            {
              product_transition =
                succession_element.product_transition;
              successor_transition_list =
                remaining_succession_elements;
              map1 = succession_element.map1;
              map2 = succession_element.map2;
            }
            wrapper.stack_for_current_path;
          move_forward
            wrapper
            next_successor_transition
      with
      Stack.Empty -> wrapper
    in
    let
        changed_wrapper = empty_stack init
    in
    let
        z1 = get_initial_location ta1 l1
    in
    let
        z2 = get_initial_location ta2 l2
    in
    if
      changed_wrapper.verify_roots1 && changed_wrapper.verify_roots2
    then
      if
        changed_wrapper.stable
      then
        Some true
      else
        None
    else
      Some false

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
         (ZVGQuotient2.node_name l2 z2);
       flush stdout;
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
           Printf.printf
             "x1 = [%s]\nx2 = [%s]\n"
             (String.concat
                "; "
                (List.map
                   (function (z3, _, lz4) ->
                     "(" ^ (ZVGQuotient2.node_name l1 z3) ^ ", [" ^
                       (String.concat
                          "; "
                          (List.map
                             (function z4 ->
                               ZVGQuotient2.node_name l2 z4)
                             lz4
                          )
                       )
                     ^ "])"
                   )
                   x1
                )
             )
             (String.concat
                "; "
                (List.map
                   (function (lz3, _, z4) ->
                     "([" ^
                       (String.concat
                          "; "
                          (List.map
                             (function z3 ->
                               ZVGQuotient2.node_name l1 z3)
                             lz3
                          )
                       )
                     ^ "], " ^
                       (ZVGQuotient2.node_name l2 z4)
                     ^ ")"
                   )
                   x2
                )
             )
           ;
           match
             (List.for_all
                (function (z3, _, lz4) ->
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
                (function (lz3, _, z4) ->
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
                  (dbm_isZeroIncluded node_ref1.zone_constraint2)
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
          (dbm_to_string ta1.clock_names node_ref1.zone_constraint2)
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
                  (dbm_isZeroIncluded node_ref2.zone_constraint2)
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
          (dbm_to_string ta2.clock_names node_ref2.zone_constraint2)
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
