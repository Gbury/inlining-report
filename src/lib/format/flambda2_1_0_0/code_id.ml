
(** Defintion taken from:
    flambda2:middle_end/flambda/basic/code_id.ml
    where it is defined as
    [type exported = Code_id_data.t] *)

type t = {
  compilation_unit : Compilation_unit.t;
  name : string;
  linkage_name : Linkage_name.t;
} [@@deriving yojson]
