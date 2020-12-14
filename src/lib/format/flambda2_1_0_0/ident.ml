
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


(* Conversion function *)
(* ******************* *)

(* Since the {core} lib re-uses the compiler-libs type and module,
   we cannot preserve the stamps, and thus must go through the creation
   functions exposed and memoise the translation. *)

let conv_tbl = Hashtbl.create 1013

let conv_aux = function
  | Local { name; stamp = _; } ->
    Ocir_core.Ident.create_local name
  | Scoped { name; scope; stamp = _; } ->
    Ocir_core.Ident.create_scoped ~scope name
  | Global name ->
    Ocir_core.Ident.create_persistent name
  | Predef { name; stamp = _; } ->
    Ocir_core.Ident.create_predef name

let conv t =
  try Hashtbl.find conv_tbl t
  with Not_found ->
    let res = conv_aux t in
    Hashtbl.add conv_tbl t res;
    res


