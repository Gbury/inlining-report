
(** Definition taken from:
    flambda2:parsing/location.ml
    for the type t
*)

type position = Lexing.position = {
  pos_fname : string;
  pos_lnum : int;
  pos_bol : int;
  pos_cnum : int;
} [@@deriving yojson]

type t = Ocaml_common.Location.t = {
  loc_start: position;
  loc_end : position;
  loc_ghost : bool;
} [@@deriving yojson]

