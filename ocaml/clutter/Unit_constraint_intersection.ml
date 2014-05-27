open Grammar_types

let unit_constraint_intersection c1 c2 =
  match
    (c1, c2)
  with
    (True, _) -> [c2]
  | (_, True) -> [c1]
  | (False, _)
  | (_, False) -> [False]
  | (Lt (cn1, n1), Lt (cn2, n2)) ->
    if
      (cn1 = cn2)
    then
      if
        (n1 < n2)
      then
        [c1]
      else
        [c2]
    else
      [c1; c2]
  | (Le (cn2, n2), Lt (cn1, n1))
  | (Lt (cn1, n1), Le (cn2, n2)) ->
    if
      (cn1 = cn2)
    then
      if
        (n1 <= n2)
      then
        [Lt (cn1, n1)]
      else
        [Le (cn2, n2)]
    else
      [c1; c2]
  | (Eq (cn2, n2), Lt (cn1, n1))
  | (Lt (cn1, n1), Eq (cn2, n2)) ->
    if
      (cn1 = cn2)
    then
      if
        (n1 >= n2)
      then
        [False]
      else
        [Eq (cn2, n2)]
    else
      [c1; c2]
  | (Ge (cn2, n2), Lt (cn1, n1))
  | (Lt (cn1, n1), Ge (cn2, n2)) ->
    if
      ((cn1 = cn2) && (n1 <= n2))
    then
      [False]
    else
      [c1; c2]
  | (Gt (cn2, n2), Lt (cn1, n1))
  | (Lt (cn1, n1), Gt (cn2, n2)) ->
    if
      ((cn1 = cn2) && (n1 <= n2))
    then
      [False]
    else
      [c1; c2]
  | (Le (cn1, n1), Le (cn2, n2)) ->
    if
      (cn1 = cn2)
    then
      if
        (n1 <= n2)
      then
        [c1]
      else
        [c2]
    else
      [c1; c2]
  | (Eq (cn2, n2), Le (cn1, n1))
  | (Le (cn1, n1), Eq (cn2, n2)) ->
    if
      (cn1 = cn2)
    then
      if
        (n1 < n2)
      then
        [False]
      else
        [Eq (cn2, n2)]
    else
      [c1; c2]
  | (Ge (cn2, n2), Le (cn1, n1))
  | (Le (cn1, n1), Ge (cn2, n2)) ->
    if
      ((cn1 = cn2) && (n1 < n2))
    then
      [False]
    else
      [c1; c2]
  | (Gt (cn2, n2), Le (cn1, n1))
  | (Le (cn1, n1), Gt (cn2, n2)) ->
    if
      ((cn1 = cn2) && (n1 <= n2))
    then
      [False]
    else
      [c1; c2]
  | (Eq (cn1, n1), Eq (cn2, n2)) ->
    if
      (cn1 = cn2)
    then
      if
        (n1 == n2)
      then
        [c1]
      else
        [False]
    else
      [c1; c2]
  | (Eq (cn2, n2), Ge (cn1, n1))
  | (Ge (cn1, n1), Eq (cn2, n2)) ->
    if
      (cn1 = cn2)
    then
      if
        (n1 <= n2)
      then
        [Eq (cn2, n2)]
      else
        [False]
    else
      [c1; c2]
  | (Ge (cn1, n1), Ge (cn2, n2)) ->
    if
      (cn1 = cn2)
    then
      if
        (n1 >= n2)
      then
        [c1]
      else
        [c2]
    else
      [c1; c2]
  | (Eq (cn2, n2), Gt (cn1, n1))
  | (Gt (cn1, n1), Eq (cn2, n2)) ->
    if
      (cn1 = cn2)
    then
      if
        (n1 < n2)
      then
        [Eq (cn2, n2)]
      else
        [False]
    else
      [c1; c2]
  | (Ge (cn2, n2), Gt (cn1, n1))
  | (Gt (cn1, n1), Ge (cn2, n2)) ->
    if
      (cn1 = cn2)
    then
      if
        (n1 >= n2)
      then
        [Gt (cn1, n1)]
      else
        [Ge (cn2, n2)]
    else
      [c1; c2]
  | (Gt (cn1, n1), Gt (cn2, n2)) ->
    if
      (cn1 = cn2)
    then
      if
        (n1 > n2)
      then
        [c1]
      else
        [c2]
    else
      [c1; c2]
