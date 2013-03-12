open Grammar_types

let unit_constraint_intersection c1 c2 =
  match
    (c1, c2)
  with
    (True, _) -> [c2]
  | (False, _) -> [False]
  | (_, True) -> [c1]
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
  | (Lt (cn1, n1), Le (cn2, n2)) ->
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
  | (Lt (cn1, n1), Eq (cn2, n2)) ->
    if
      (cn1 = cn2)
    then
      if
        (n1 >= n2)
      then
        [False]
      else
        [c2]
    else
      [c1; c2]
  | (Lt (cn1, n1), Ge (cn2, n2)) ->
    if
      ((cn1 = cn2) && (n1 <= n2))
    then
      [False]
    else
      [c1; c2]
  | (Lt (cn1, n1), Gt (cn2, n2)) ->
    if
      ((cn1 = cn2) && (n1 <= n2))
    then
      [False]
    else
      [c1; c2]
  | (Le (cn1, n1), Lt (cn2, n2)) ->
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
  | (Le (cn1, n1), Eq (cn2, n2)) ->
    if
      (cn1 = cn2)
    then
      if
        (n1 < n2)
      then
        [False]
      else
        [c2]
    else
      [c1; c2]
  | (Le (cn1, n1), Ge (cn2, n2)) ->
    if
      ((cn1 = cn2) && (n1 < n2))
    then
      [False]
    else
      [c1; c2]
  | (Le (cn1, n1), Gt (cn2, n2)) ->
    if
      ((cn1 = cn2) && (n1 <= n2))
    then
      [False]
    else
      [c1; c2]
  | (Eq (cn1, n1), Lt (cn2, n2)) ->
    if
      (cn1 = cn2)
    then
      if
        (n1 < n2)
      then
        [c1]
      else
        [False]
    else
      [c1; c2]
  | (Eq (cn1, n1), Le (cn2, n2)) ->
    if
      (cn1 = cn2)
    then
      if
        (n1 <= n2)
      then
        [c1]
      else
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
  | (Eq (cn1, n1), Ge (cn2, n2)) ->
    if
      (cn1 = cn2)
    then
      if
        (n1 >= n2)
      then
        [c1]
      else
        [False]
    else
      [c1; c2]
  | (Eq (cn1, n1), Gt (cn2, n2)) ->
    if
      (cn1 = cn2)
    then
      if
        (n1 > n2)
      then
        [c1]
      else
        [False]
    else
      [c1; c2]
  | (Ge (cn1, n1), Lt (cn2, n2)) ->
    if
      ((cn1 = cn2) && (n1 >= n2))
    then
      [False]
    else
      [c1; c2]
  | (Ge (cn1, n1), Le (cn2, n2)) ->
    if
      ((cn1 = cn2) && (n1 > n2))
    then
      [False]
    else
      [c1; c2]
  | (Ge (cn1, n1), Eq (cn2, n2)) ->
    if
      (cn1 = cn2)
    then
      if
        (n1 <= n2)
      then
        [c2]
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
  | (Ge (cn1, n1), Gt (cn2, n2)) ->
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
  | (Gt (cn1, n1), Lt (cn2, n2)) ->
    if
      ((cn1 = cn2) && (n1 >= n2))
    then
      [False]
    else
      [c1; c2]
  | (Gt (cn1, n1), Le (cn2, n2)) ->
    if
      ((cn1 = cn2) && (n1 >= n2))
    then
      [False]
    else
      [c1; c2]
  | (Gt (cn1, n1), Eq (cn2, n2)) ->
    if
      (cn1 = cn2)
    then
      if
        (n1 < n2)
      then
        [c2]
      else
        [False]
    else
      [c1; c2]
  | (Gt (cn1, n1), Ge (cn2, n2)) ->
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
