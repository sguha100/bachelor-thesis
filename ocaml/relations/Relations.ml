open ZVG_modules
open Grammar_types
open Zone_stubs
open UDBM_utilities

type succession = Succession1 | Succession2 | Succession_both

type product_transition = {
  action_pair : (ZVGQuotient2.action_t* ZVGQuotient2.action_t) option;
  succession : succession;
  child : ZVGQuotient2.node_ref_t * ZVGQuotient2.node_ref_t
}

type succession_element = {
  product_transition: product_transition;
  successor_transition_list: product_transition list;
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

module Ordered_node_ref =
struct
  type t = ZVGQuotient2.node_ref_t
  let compare = Pervasives.compare
end

module Node_ref_set = Set.Make (Ordered_node_ref)

module Ordered_node_ref_pair =
struct
  type t = ZVGQuotient2.node_ref_t * ZVGQuotient2.node_ref_t
  let compare = Pervasives.compare
end

module Node_ref_pair_set = Set.Make (Ordered_node_ref_pair)

module Succession_stack_element =
struct
  type t = (ZVGQuotient2.action_t * ZVGQuotient2.action_t) *
    (ZVGQuotient2.node_ref_t * ZVGQuotient2.node_ref_t)
  let compare = Pervasives.compare
end

module Set_for_succession_stack = Set.Make (Succession_stack_element)

type information_wrapper = {
  not_related: Node_ref_pair_set.t;
  multiply_visited: Node_ref_pair_set.t;
  stable: bool;
  stack_for_current_path: succession_element Stack.t;
  successor_1_set_stack: Node_ref_set.t Stack.t;
  successor_2_set_stack: Node_ref_set.t Stack.t;
}

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
      let
          succession1set =
        List.fold_left
          (function s ->
            function (z3, a, lz4) ->
              List.fold_left
                (function s -> function z4 ->
                  Set_for_succession_stack.add ((a, a), (z3, z4)) s
                )
                s
                lz4
          )
          Set_for_succession_stack.empty
          x1
      in
      let
          succession2set =
        List.fold_left
          (function s ->
            function (lz3, a, z4) ->
              List.fold_left
                (function s -> function z3 ->
                  Set_for_succession_stack.add ((a, a), (z3, z4)) s
                )
                s
                lz3
          )
          Set_for_succession_stack.empty
          x2
      in
      Stack.push
        {product_transition =
            {action_pair = None;
             succession = Succession_both;
             child = (z1, z2)
            };
         successor_transition_list =
            (List.map
               (function ((a1, a2), (z3, z4)) ->
                 {action_pair = Some (a1, a2);
                  succession = Succession_both;
                  child = (z3, z4)
                 }
               )
               (Set_for_succession_stack.elements
                  (Set_for_succession_stack.inter
                     succession1set
                     succession2set
                  )
               )
            ) @
              (List.map
                 (function ((a1, a2), (z3, z4)) ->
                   {action_pair = Some (a1, a2);
                    succession = Succession_both;
                    child = (z3, z4)
                   }
                 )
                 (Set_for_succession_stack.elements
                    (Set_for_succession_stack.diff
                       succession1set
                       succession2set
                    )
               )
              ) @
              (List.map
                 (function ((a1, a2), (z3, z4)) ->
                   {action_pair = Some (a1, a2);
                    succession = Succession_both;
                    child = (z3, z4)
                   }
                 )
                 (Set_for_succession_stack.elements
                    (Set_for_succession_stack.diff
                       succession2set
                       succession1set
                    )
               )
              )
        }
        s;
      let
          s1 = Stack.create ()
      in
      Stack.push
        (Node_ref_set.empty)
        s1;
      Stack.push
        (Node_ref_set.empty)
        s1;
      let
          s2 = Stack.create ()
      in
      Stack.push
        (Node_ref_set.empty)
        s2;
      Stack.push
        (Node_ref_set.empty)
        s2;
    {
      not_related = not_related;
      multiply_visited = Node_ref_pair_set.empty;
      stable = true;
      stack_for_current_path = s;
      successor_1_set_stack = s1;
      successor_2_set_stack = s2;
    }

  let move_forward wrapper next_successor_transition =
    (*Wrong!*)
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
          backtrack wrapper
        | next_successor_transition::remaining_succession_elements ->
          Stack.push
            {
              product_transition =
                succession_element.product_transition;
              successor_transition_list =
                remaining_succession_elements
            }
            wrapper.stack_for_current_path;
          move_forward
            wrapper
            next_successor_transition
      with
      | Stack.empty -> wrapper
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
      Node_ref_set.mem z1 (Stack.top changed_wrapper.successor_1_set_stack)
        &
      Node_ref_set.mem z2 (Stack.top changed_wrapper.successor_2_set_stack)
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
