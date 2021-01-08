(** Definition taken from:
    flambda1:middle_end/flambda/linkage_name.ml
    where it is defined as
    [type t = string] *)

type t = string
[@@deriving yojson]

(* Conversion function *)
(* ******************* *)

let conv (t: t) : Ocir_core.Linkage_name.t = t


