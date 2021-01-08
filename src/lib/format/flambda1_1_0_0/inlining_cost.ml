
(** Definitions taken from:
    flambda1:middle_end/flambda/inlining_cost.ml
*)

module Threshold = struct

  type t =
    | Never_inline
    | Can_inline_if_no_larger_than of int
  [@@deriving yojson]

  let conv t : Ocir_core.Inlining_threshold.t =
    match (t : t) with
    | Never_inline -> Never_inline
    | Can_inline_if_no_larger_than i -> Can_inline_if_no_larger_than i

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

  let conv (t : t) : Ocir_core.Benefit.t = {
    remove_call = t.remove_call;
    remove_alloc = t.remove_alloc;
    remove_prim = t.remove_prim;
    remove_branch = t.remove_branch;
    direct_call_of_indirect = t.direct_call_of_indirect;
    requested_inline = t.requested_inline;
  }


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

  let conv (t : t) : Ocir_core.Whether_sufficient_benefit.t = {
    round = t.round;
    benefit = Benefit.conv t.benefit;
    toplevel = t.toplevel;
    branch_depth = t.branch_depth;
    lifting = t.lifting;
    original_size = t.original_size;
    new_size = t.new_size;
    evaluated_benefit = t.evaluated_benefit;
    estimate = t.estimate;
  }

end

