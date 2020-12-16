
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

(* Printing & usual functions *)
(* ************************************************************************* *)

let print fmt { id; linkage_name = _; } =
  Format.fprintf fmt "%s" (Ident.unique_toplevel_name id)

let hash t = Hashtbl.hash t.linkage_name

let compare t t' =
  if t == t' then 0
  (* Linkage_names should be unique *)
  else Stdlib.compare t.linkage_name t'.linkage_name

let equal t t' = compare t t' = 0

