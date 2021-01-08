
(** Definitions taken from:
    flambda1:middle_end/flambda/inlining_cost.ml
*)

module Threshold = struct

  type t =
    | Never_inline
    | Can_inline_if_no_larger_than of int
  [@@deriving yojson]

end

module Benefit = struct

  type t = {
    remove_call : int;
    remove_alloc : int;
    remove_prim : int;
    remove_branch : int;
    direct_call_of_indirect : int;
    requested_inline : int;
  } [@@deriving yojson]

end

module Whether_sufficient_benefit = struct

  type t = {
    round : int;
    benefit : Benefit.t;
    toplevel : bool;
    branch_depth : int;
    lifting : bool;
    original_size : int;
    new_size : int;
    evaluated_benefit : int;
    estimate : bool;
  } [@@deriving yojson]

end

