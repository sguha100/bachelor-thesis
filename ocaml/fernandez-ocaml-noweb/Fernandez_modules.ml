module type LTS_TYPE =
sig
  type node_ref_t
  type action_t
  type lts_t
  val node_name: lts_t -> node_ref_t -> string
  val expand_action: lts_t -> action_t -> string
  val nodes: lts_t -> node_ref_t list
  val actions: lts_t -> action_t list
  val in_adjacency: lts_t -> node_ref_t -> (action_t * (node_ref_t list)) list
end

module LTS =
  functor (LT: LTS_TYPE) ->
struct
  type node_ref_t = LT.node_ref_t
  type action_t = LT.action_t
      (* type lts = {nodes: node_ref_t array; actions: action_t
  array} *)
  let node_name = LT.node_name
  let expand_action = LT.expand_action
  let nodes = LT.nodes
  let actions = LT.actions
  let in_adjacency = LT.in_adjacency
  type block =
    {node_refs: node_ref_t list;
     info: (action_t -> (node_ref_t -> int))}
  type partition = block list
  type splitter = Simple of block | Compound of (block * splitter *
                                                   splitter)
  let rec two_way_update_tree t x x1 x2 =
    match t with
      Simple t0 ->
        if
          (
                (List.for_all
                   (function t0elem ->
                     (List.exists ((=) t0elem) x.node_refs)
                   )
                   t0.node_refs
                )
                &&
                (List.for_all
                   (function xelem ->
                     (List.exists ((=) xelem) t0.node_refs)
                   )
                   x.node_refs
                )
          ) 
        then
          (Compound (x, Simple x1, Simple x2), true)
        else
          (t, false)
    | Compound (t0, t1, t2) -> (
      match
        (two_way_update_tree t1 x x1 x2)
      with
        (_, false) -> (
          match
            (two_way_update_tree t2 x x1 x2)
          with
            (_, false) -> (t, false)
          | (tt2, true) -> (Compound (t0, t1, tt2), true)
         )
      | (tt1, true) -> (Compound (t0, tt1, t2), true)
    )

  let rec two_way_update_queue w x x1 x2 =
    match w with
      [] -> [Compound (x, Simple x1, Simple x2)]
    | t::ts -> (match (two_way_update_tree t x x1 x2) with
      (_, false) -> t::(two_way_update_queue ts x x1 x2)
      | (tt, true) -> tt::ts
    )

  let get_info l node_refs =
    function a ->
      function node_ref ->
        List.length
          (List.filter
             (function belem ->
               List.exists
                 (function (aa, nl) -> a=aa && List.mem node_ref nl)
                 (in_adjacency l belem)
             )
             node_refs)

 let get_block_from_node_refs l node_refs =
   {node_refs = node_refs;
    info =
       (get_info l node_refs)
   }

 let simple_split_block_on_action l x b a =
   match
     List.partition
       (function xelem ->
         List.exists
           (function belem ->
             List.exists
               (function (aa, nl) -> a=aa && List.mem xelem nl)
         (in_adjacency l belem))
           b.node_refs) x.node_refs
   with
     (x1indices, x2indices) -> (get_block_from_node_refs l x1indices, get_block_from_node_refs l x2indices)

 let compound_split_block_on_action l x b b1 b2 a =
   let 
     (b1, b2) =
     if
       ((List.length b2.node_refs) < (List.length b1.node_refs))
     then
       (b2, b1)
     else
       (b1, b2)
   in
   match
     (List.partition
        (function xelem -> (b1.info a xelem) > 0)
        x.node_refs)
  with
    (y1, y2) ->
      (match
          (List.partition
             (function xelem ->
               (b1.info a xelem) < (b.info a xelem)
             )
             y1
             ,
           List.partition
             (function xelem ->
               (b.info a xelem) > 0
               )
             y2)
       with
         ((x3noderefs, x1noderefs), (x2noderefs, x4noderefs)) ->
           ((get_block_from_node_refs l x3noderefs,
             get_block_from_node_refs l x1noderefs),
            (get_block_from_node_refs l x2noderefs,
             get_block_from_node_refs l x4noderefs))
      )
        
let rec simple_split_blocks_on_action l (w, xl) b a =
  match xl with
    [] -> (w, [])
  | x::xs ->
    (let
        (ww, xxs) = (simple_split_blocks_on_action l (w, xs) b a)
     in
     (match (simple_split_block_on_action l x b a) with
    ({node_refs= []; info= _}, x2) -> (ww, x2::xxs)
    | (x1, {node_refs=[]; info= _}) -> (ww, x1::xxs)
    | (x1, x2) -> (let www = (two_way_update_queue ww x x1 x2) in (www, x1::x2::xxs))
                                                                           ))

let rec compound_split_blocks_on_action l (w, xl) b b1 b2 a =
  match xl with
    [] -> (w, [])
  | x::xs ->
    (let
        (ww, xxs) =
       (compound_split_blocks_on_action l (w, xs) b b1 b2 a)
     in
     (match
         (compound_split_block_on_action l x b b1 b2 a)
      with
        (({node_refs=[]; info=_}, {node_refs=[]; info=_}), ({node_refs=[];
                                                         info=_}, x4)) ->
          (ww, x4::xxs)
      | (({node_refs=[]; info=_}, x1), ({node_refs=[]; info=_}, {node_refs=[];
                                                             info=_})) ->
        (ww, x1::xxs)
    | (({node_refs=[]; info=_}, {node_refs=[]; info=_}), (x2, {node_refs=[];
                                                           info=_})) ->
      (ww, x2::xxs)
    | ((x3, {node_refs=[]; info=_}), ({node_refs=[]; info=_}, {node_refs=[];
                                                           info=_})) ->
      (ww, x3::xxs)
    | ((x3, x1), ({node_refs=[]; info=_}, {node_refs=[]; info=_})) ->
      (let
          www = (two_way_update_queue ww x x3 x1)
       in
       (www, x3::x1::xxs))
    | ((x3, {node_refs=[]; info=_}), (x2, {node_refs=[]; info=_})) ->
      (let
          www = (two_way_update_queue ww x x3 x2)
       in
       (www, x3::x2::xxs))
    | (({node_refs=[]; info=_}, x1), (x2, {node_refs=[]; info=_})) ->
      (let
          www = (two_way_update_queue ww x x1 x2)
       in
       (www, x1::x2::xxs))
    | ((x3, x1), (x2, {node_refs=[]; info=_})) ->
      (let www =
         (let x23 =
            {node_refs = x2.node_refs@x3.node_refs;
             info =
                (function a -> function node_ref ->
                  x2.info a node_ref + x3.info a node_ref
                )}
          in
          (two_way_update_queue
             (two_way_update_queue ww x x1 x23) x23 x2 x3))
       in
       (www, x1::x2::x3::xxs))
     ))

let split_partition_on_action l (w, pi) sp a =
  match
    sp
  with
    Simple b -> simple_split_blocks_on_action l (w, pi) b a
  | Compound (b, Simple b1, Simple b2) -> compound_split_blocks_on_action l (w, pi) b b1 b2 a
  | Compound (b, Compound (b1, _, _), Simple b2) -> compound_split_blocks_on_action l (w, pi) b b1 b2 a
  | Compound (b, Simple b1, Compound (b2, _, _)) -> compound_split_blocks_on_action l (w, pi) b b1 b2 a
  | Compound (b, Compound (b1, _, _), Compound (b2, _, _)) ->
    compound_split_blocks_on_action l (w, pi) b b1 b2 a

let rec split_partition_on_actions l (w, pi) b action_list =
  List.fold_left
    (function (w, pi) ->
      split_partition_on_action l (w, pi) b
    )
    (w, pi)
    action_list

let dequeue l (w, pi) =
  match
    w
  with
  [] -> ([], pi)
| (Simple b)::wtail ->
  split_partition_on_actions l (wtail, pi) (Simple b)
  (actions l)
| (Compound (b, b1, b2))::wtail ->
  split_partition_on_actions
    l
    (wtail@[b1; b2], pi)
    (Compound (b, b1, b2))
    (actions l)

let rec emptyqueue l (w, pi) =
  match
    w
  with
    [] -> ([], pi)
  | whead::wtail -> emptyqueue l (dequeue l (w, pi))

let fernandez l =
  let
      llistblock = (get_block_from_node_refs l (nodes l))
  in
  let
      (_, pi) = emptyqueue l ([Simple llistblock], [llistblock])
  in
  pi

let print_dot l partition filename =
  let
      out = open_out filename  
  in
  Printf.fprintf out "digraph \"Bisimilarity quotient graph\" {\n";
  List.fold_left
    (function count -> function block ->
      Printf.fprintf out
        "subgraph cluster_%s { \nrank = same; %s ;\n}\n"
        (string_of_int count)
        (String.concat
           ";"
           (List.map
              (function node_ref -> "\"" ^ (node_name l node_ref) ^ "\"")
              block.node_refs
           )
        )
      ;
      List.iter
        (function node_ref ->
          (* Printf.fprintf out "\"%s\";\n" (node_name l node_ref); *)
          List.iter
            (function (a, nl) ->
              List.iter
                (function nlelem ->
                  Printf.fprintf
                    out
                    "\"%s\" -> \"%s\" [label = \"%s\"]\n"
                    (node_name l node_ref)
                    (node_name l nlelem)
                    (expand_action l a)
                )
                nl
            )
            (in_adjacency l node_ref);
        )
        block.node_refs;
      (count + 1)
    )
    0
    partition;
  Printf.fprintf out "}\n";
  flush out
end

module SimpleLT =
  struct
    type node_ref_t = int
    type action_t = int
    type lts_t =
      {action_count : action_t;
       in_adjacencies: ((action_t *(node_ref_t list)) list) array}
    let node_name l i= (string_of_int i)
    let expand_action l a = string_of_int a
    let nodes=
      function l ->
        Array.to_list (Array.init (Array.length l.in_adjacencies) (function i -> i))
    let actions =
      function l ->
        Array.to_list (Array.init l.action_count (function a-> a))
    let in_adjacency = function l -> Array.get l.in_adjacencies
  end

module SimpleLTS = LTS (SimpleLT)
let lts1 = {SimpleLT.in_adjacencies = [|[(2, [6])]; 
                    [(0, [1])]; 
                    [(0, [0]); (1, [7])];
                    [(1, [2])];
                    [(1, [2])];
                    [(0, [4; 5]); (2, [6])];
                    [(1, [3]); (2, [2])];
                    [(0, [1])]|]; SimpleLT.action_count=3}
let pi1 = SimpleLTS.fernandez lts1
let lts2 = {SimpleLT.in_adjacencies=[|[(0, [5])]; 
                    [(0, [0; 1])]; 
                    [(1, [4])];
                    [(0, [6]); (1, [2])];
                    [(1, [3])];
                    [];
                    [(1, [5])]
                  |]; SimpleLT.action_count=2}
let pi2 = SimpleLTS.fernandez lts2
