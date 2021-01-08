
(** Definition taken from:
    flambda1:middle_end/variable.ml
*)

type t = {
  compilation_unit : Compilation_unit.t;
  name : string;
  name_stamp : int;
} [@@deriving yojson]

(* Conversion functions *)
(* ******************** *)

let conv { compilation_unit; name; name_stamp = _; } : Ocir_core.Code_id.t =
  let compilation_unit = Compilation_unit.conv compilation_unit in
  { name; compilation_unit; }

