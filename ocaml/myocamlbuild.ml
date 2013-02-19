open Ocamlbuild_plugin
open Command

(* Configuration section *)
let have_udbm = "-DHAVE_UDBM"
let udbm_lib = "-ludbm"
let udbm_libdir = "-L/home/mihir/uppaal/lib"
let udbm_include = "-I/home/mihir/uppaal/include"
let cpp_lib = "-libstdc++"
  
let static = true
  
(* List of headers *)
let headers =
  []
    
;;

dispatch begin function
| After_rules ->
  
  (* When one is making a C library that uses the udbm library with
     ocamlmklib,then issue these flags. *)
  flag ["ocamlmklib"; "c"; "use_udbm"]
    (S[A udbm_libdir; A udbm_lib; A cpp_lib; A"-custom"]);
  
  (* When one compiles C code using the udbm library *)
  flag ["c"; "compile"; "include_udbm"]
    (S[A"-ccopt"; A udbm_include; A"-ccopt"; A have_udbm]);
  
  flag ["link"; "ocaml"; "library"; "use_udbm"]
    (S[A"-ccopt"; A udbm_libdir; A"-cclib"; A udbm_lib]);
  
  (* If `static' is true then every ocaml link in bytecode will add
     -custom *)
  if static then flag ["link"; "ocaml"; "byte"] (A"-custom");
  
  (* zone is an ocaml library.
     This will declare use_zone and include_zone *)
  ocaml_lib "zone";
  
  flag ["link"; "library"; "ocaml"; "byte"; "use_libzone"]
    (S[A"-dllib"; A"-lzone"; A"-cclib"; A"-lzone"]);
  
  flag ["link"; "library"; "ocaml"; "native"; "use_libzone"]
    (S[A"-cclib"; A"-lzone"]);
  
  (* When ocaml is linking something that use the libzone library,
     then one need that file to be up to date. *)
  dep  ["link"; "ocaml"; "use_libzone"] ["libzone.a"];
  
  (* As an approximation all our C files use the headers.
     Note: This will import headers in the build directory. *)
  dep  ["compile"; "c"] headers;
| _ -> ()
end
