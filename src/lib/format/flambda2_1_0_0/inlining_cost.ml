
(** Definition taken from:
    flambda2:middle_end/flambda/inlining/inlining_cost.ml
    where it is defined as
    [type Threshold.t] *)

module Threshold = struct
  type t =
    | Never_inline
    | Can_inline_if_no_larger_than of int
  [@@deriving yojson]
end

(* Conversion function *)
(* ******************* *)

let conv t : Ocir_core.Inlining_threshold.t =
  match (t : Threshold.t) with
  | Never_inline -> Never_inline
  | Can_inline_if_no_larger_than i -> Can_inline_if_no_larger_than i


