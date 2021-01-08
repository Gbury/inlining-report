
(** Definition taken from:
    flambda1:middle_end/variable.ml
*)

type t = {
  compilation_unit : Compilation_unit.t;
  name : string;
  name_stamp : int;
} [@@deriving yojson]

let compare t1 t2 =
  if t1 == t2 then 0
  else
    let c = t1.name_stamp - t2.name_stamp in
    if c <> 0 then c
    else Compilation_unit.compare t1.compilation_unit t2.compilation_unit

(* Conversion functions *)
(* ******************** *)

let conv { compilation_unit; name; name_stamp = _; } : Ocir_core.Code_id.t =
  let compilation_unit = Compilation_unit.conv compilation_unit in
  { name; compilation_unit; }

