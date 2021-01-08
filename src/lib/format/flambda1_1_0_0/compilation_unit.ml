
(** Definition taken from:
    flambda1:middle_end/compilation_unit.ml
*)

type t = {
  id : Ident.t;
  linkage_name : Linkage_name.t;
  hash : int;
} [@@deriving yojson]

let compare v1 v2 =
  if v1 == v2 then 0
  else
    let c = compare v1.hash v2.hash in
    if c = 0 then
      let v1_id = Ident.name v1.id in
      let v2_id = Ident.name v2.id in
      let c = String.compare v1_id v2_id in
      if c = 0 then
        Linkage_name.compare v1.linkage_name v2.linkage_name
      else
        c
    else c

(* Conversion functions *)
(* ******************** *)

let conv { id; linkage_name; hash = _; } : Ocir_core.Compilation_unit.t =
  let id = Ident.conv id in
  let linkage_name = Linkage_name.conv linkage_name in
  { id; linkage_name; }

