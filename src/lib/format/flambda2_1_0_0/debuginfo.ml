
(** Definitions taken from:
    flambda2:lambda/debuginfo.ml *)

module Scoped_location = struct

  type scope_item = Ocaml_common.Debuginfo.Scoped_location.scope_item =
    | Sc_anonymous_function
    | Sc_value_definition of string
    | Sc_module_definition of string
    | Sc_class_definition of string
    | Sc_method_definition of string
  [@@deriving yojson]

  type scopes = scope_item list [@@deriving yojson]

  type t = Ocaml_common.Debuginfo.Scoped_location.t =
    | Loc_unknown
    | Loc_known of
        { loc : Location.t;
          scopes : scopes; }
  [@@deriving yojson]

end

type item = {
  dinfo_file: string;
  dinfo_line: int;
  dinfo_char_start: int;
  dinfo_char_end: int;
  dinfo_start_bol: int;
  dinfo_end_bol: int;
  dinfo_end_line: int;
  dinfo_scopes: Scoped_location.scopes;
} [@@deriving yojson]

type t = item list (* = Ocaml_common.Debuginfo.t *) [@@deriving yojson]

