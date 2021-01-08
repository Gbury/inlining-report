
(** Definition taken from:
    flambda1:middle_end/compilation_unit.ml
*)

type t = {
  id : Ident.t;
  linkage_name : Linkage_name.t;
  hash : int;
} [@@deriving yojson]

(* Conversion functions *)
(* ******************** *)

let conv { id; linkage_name; hash = _; } : Ocir_core.Compilation_unit.t =
  let id = Ident.conv id in
  let linkage_name = Linkage_name.conv linkage_name in
  { id; linkage_name; }

