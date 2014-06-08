let _ =
  match
    Sys.argv.(1)
  with
  | "compare" -> ()
  | "analyse"
  | "analyze" -> ()
  | "test" -> ()
  | _ -> ()
    ;
  exit 0
