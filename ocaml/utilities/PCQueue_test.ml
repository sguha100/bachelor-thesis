open NRQueue
open PCQueue
open PCQueueElement

let test61 =
  try
    (function _ -> "test61 failed") (peek (create ()))
  with
    Empty -> "test61 passed"

let test62 =
  try
    (function _ -> "test62 failed") (take (create ()))
  with
    Empty -> "test62 passed"

(* let test63 = *)
(*   if *)
(*     element_exists empty {parent = Some 0; edge = None; child = 1} *)
(*   then *)
(*     "test63 failed" *)
(*   else *)
(*     "test63 passed" *)

(* let test64 = *)
(*   if *)
(*     element_exists (enqueue empty {parent = Some 0; edge = None; child = 1}) *)
(*       {parent = Some 2; edge = None; child = 1} *)
(*   then *)
(*     "test64 passed" *)
(*   else *)
(*     "test64 failed" *)

let test65 =
  let
      q1 = create ()
  in
  let
      () = add {parent = Some 0; edge = None; child = 1} q1
  in
  let
      q2 = create ()
  in
  let
      () = add {parent = Some 0; edge = None; child = 1} q2
  in
  let
      () = add {parent = Some 2; edge = None; child = 1} q2
  in
  if
    (length q1)
      =
    (length q2)
  then
    "test65 passed"
  else
    "test65 failed"

let test66 =
  let
      q1 = create ()
  in
  let
      () = add {parent = Some 0; edge = None; child = 1} q1
  in
  let
      q2 = create ()
  in
  let
      () = add {parent = None; edge = None; child = 1} q2
  in
  let
      () = add {parent = Some 0; edge = None; child = 2} q2
  in
  if
    (length q1)
      <
    (length q2)
  then
    "test66 passed"
  else
    "test66 failed"
