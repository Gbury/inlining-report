
(** Compilation units

    A compilation unit is typically a source OCaml file (so a module,
    but note not all modules are compilation units).

*)

type t = {
  id : Ocaml_common.Ident.t;
  linkage_name : Linkage_name.t;
}

