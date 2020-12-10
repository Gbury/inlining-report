
(** Definition taken from:
    flambda2:middle_end/flambda/compilenv_deps/compilation_unit.ml
    where it is defined as
    [type t] *)

type t = {
  id : Ident.t;
  linkage_name : Linkage_name.t;
  hash : int;
} [@@deriving yojson]
