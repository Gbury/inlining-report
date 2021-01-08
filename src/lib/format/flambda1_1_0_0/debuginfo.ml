
(** Definitions taken from:
    flambda1:lambda/debuginfo.ml *)

module Scoped_location = struct

  type scope_item = Ocir_core.Debuginfo.Scoped_location.scope_item =
    | Sc_anonymous_function
    | Sc_value_definition of string
    | Sc_module_definition of string
    | Sc_class_definition of string
    | Sc_method_definition of string
  [@@deriving yojson]

  type scopes = scope_item list [@@deriving yojson]

  type t = Ocir_core.Debuginfo.Scoped_location.t =
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
(* the deriving yojson here fails if the type is declared as an alias of
   Debuginfo.item, because the requires to declare it private, since it is
   in Debuginfo. *)

type t = item list (* = Ocir_core.Debuginfo.t *) [@@deriving yojson]


(* Useful function for the conversion *)
(* ********************************** *)

let compare_item d1 d2 =
  let c = String.compare d1.dinfo_file d2.dinfo_file in
  if c <> 0 then c else
  let c = compare d1.dinfo_line d2.dinfo_line in
  if c <> 0 then c else
  let c = compare d1.dinfo_char_end d2.dinfo_char_end in
  if c <> 0 then c else
  let c = compare d1.dinfo_char_start d2.dinfo_char_start in
  if c <> 0 then c else
  let c = compare d1.dinfo_start_bol d2.dinfo_start_bol in
  if c <> 0 then c else
  let c = compare d1.dinfo_end_bol d2.dinfo_end_bol in
  if c <> 0 then c else
  let c = compare d1.dinfo_end_line d2.dinfo_end_line in
  c

let compare dbg1 dbg2 =
  let rec loop ds1 ds2 =
    match ds1, ds2 with
    | [], [] -> 0
    | _ :: _, [] -> 1
    | [], _ :: _ -> -1
    | d1 :: ds1, d2 :: ds2 ->
      let c = compare_item d1 d2 in
      if c <> 0 then c else loop ds1 ds2
  in
  loop (List.rev dbg1) (List.rev dbg2)

let rec is_prefix ~prefix dbg =
  match prefix, dbg with
  | [], _ -> true
  | _, [] -> false
  | p :: prefix, d :: dbg ->
    begin match compare_item p d with
      | 0 -> is_prefix ~prefix dbg
      | _ -> false
    end

(* Conversion function *)
(* ******************* *)

(* Because the type Debuginfo.item is private, it is not easy to ensure
   type safety correctly, hence the indirection here where we try to
   rebuild locations with the adequate private type. *)

let item_to_location (d : item) : Location.t =
  let loc_start : Lexing.position =
    { pos_fname = d.dinfo_file;
      pos_lnum = d.dinfo_line;
      pos_bol = d.dinfo_start_bol;
      pos_cnum = d.dinfo_start_bol + d.dinfo_char_start;
    } in
  let loc_end : Lexing.position =
    { pos_fname = d.dinfo_file;
      pos_lnum = d.dinfo_end_line;
      pos_bol = d.dinfo_end_bol;
      pos_cnum = d.dinfo_start_bol + d.dinfo_char_end;
    } in
  { loc_ghost = false; loc_start; loc_end; }

let conv_item (item : item) : Ocir_core.Debuginfo.t =
  let scopes = item.dinfo_scopes in
  let loc = item_to_location item in
  let scoped_location = Scoped_location.Loc_known { loc; scopes; } in
  Ocir_core.Debuginfo.from_location scoped_location

let conv (t : t) : Ocir_core.Debuginfo.t = List.concat (List.map conv_item t)

