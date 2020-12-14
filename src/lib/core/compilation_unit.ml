
(** Compilation units

    A compilation unit is typically a source OCaml file (so a module,
    but note not all modules are compilation units).

*)

(* Type definitions *)
(* ************************************************************************* *)

type t = {
  id : Ident.t;
  linkage_name : Linkage_name.t;
}

(* Printing *)
(* ************************************************************************* *)

let print fmt { id; linkage_name = _; } = Ident.print fmt id

