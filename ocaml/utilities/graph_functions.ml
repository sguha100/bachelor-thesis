let propositionmax inv cn = (
  match inv with
    True -> 0
  | False -> 0
  | Comparison (cn1, _, value) -> (if (cn1 = cn) then value else 0)
  | And l -> List.fold_right (function max -> (function inv1 -> (let
  current = (propositionmax inv1 cn) in (if current > max then
  current else max)))) l 0
)

let invariantmax t cn = List.fold_right (function max -> (function l
-> (propositionmax l.invariant cn))) t.locations 0

let locationguardmax tl cn = List.fold_right (function max -> (function tr
-> (let
  current = (propositionmax tr.condition cn) in (if current > max then
  current else max)))) t.locations 0

let guardmax t cn = List.fold_right (function max -> (function l
-> (locationguardmax l.departures))) t.locations 0

let clockmax t = List.map (function clock -> (let (a, b) =
  (invariantmax t clock.clock_name, guardmax t clock.clock_name) in (if (a
  > b) then a
  else b))) (Array.to_list t.clocks) 
