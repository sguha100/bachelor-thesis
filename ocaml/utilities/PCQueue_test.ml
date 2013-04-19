open NRQueue
open PCQueue
open PCQueueElement

let test61 =
  try
    (function _ -> "test61 failed") (head empty)
  with
    Empty_queue -> "test61 passed"

let test62 =
  try
    (function _ -> "test62 failed") (dequeue empty)
  with
    Empty_queue -> "test62 passed"

let test63 =
  if
    element_exists empty {parent = Some 0; edge = None; child = 1}
  then
    "test63 failed"
  else
    "test63 passed"

let test64 =
  if
    element_exists (enqueue empty {parent = Some 0; edge = None; child = 1})
      {parent = Some 2; edge = None; child = 1}
  then
    "test64 passed"
  else
    "test64 failed"

let test65 =
  if
    (List.length (enqueue empty {parent = Some 0; edge = None; child = 1}))
      =
    (List.length (enqueue (enqueue empty {parent = Some 0; edge = None; child = 1})
                    {parent = Some 2; edge = None; child = 1}))
  then
    "test65 passed"
  else
    "test65 failed"

let test66 =
  if
    (List.length (enqueue empty {parent = Some 0; edge = None; child = 1}))
      <
    (List.length (enqueue (enqueue empty {parent = None; edge = None; child = 1})
                    {parent = Some 0; edge = None; child = 2}))
  then
    "test66 passed"
  else
    "test66 failed"
