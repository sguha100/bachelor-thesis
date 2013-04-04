open Grammar_types
type tree = {parent_with_edge: (int * transition) option; elements:
  int list}

let add_element_to_tree tree element =
  {parent_with_edge = tree.parent_with_edge;
   elements =
      if
        (List.exists
           ((=) element)
           tree.elements
        )
      then
        tree.elements
      else
        element::tree.elements
  }

let add_parent_with_edge_to_tree tree parent edge =
  (add_element_to_tree
     {parent_with_edge = Some (parent, edge);
      elements = tree.elements
     }
     parent
  )

let tree_element_difference tree1 tree2 =
  List.exists
    (function l1 -> List.for_all ((<>) l1) tree2.elements)
    tree1.elements
