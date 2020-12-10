
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
