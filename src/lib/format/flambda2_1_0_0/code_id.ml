
(** Defintion taken from:
    flambda2:middle_end/flambda/basic/code_id.ml
    where it is defined as
    [type exported = Code_id_data.t] *)

type t = {
  compilation_unit : Compilation_unit.t;
  name : string;
  linkage_name : Linkage_name.t;
} [@@deriving yojson]


(* Conversion functions *)
(* ******************** *)

let conv { compilation_unit; name; linkage_name = _; } : Ocir_core.Code_id.t =
  let compilation_unit = Compilation_unit.conv compilation_unit in
  { name; compilation_unit; }

