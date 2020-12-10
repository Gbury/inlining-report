
(** Definition taken from:
    flambda2:typing/ident.ml
    where it is defined as
    [type t] *)

type t =
  | Local of { name: string; stamp: int; }
  | Scoped of { name: string; stamp: int; scope: int; }
  | Global of string
  | Predef of { name: string; stamp: int; }
[@@deriving yojson]

