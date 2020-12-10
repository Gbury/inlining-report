
(** Definitions taken from:
    flambda2:middle_end/flambda/inlining/inlining_decision.ml
    where they are defined in their respective modules *)

module Function_declaration_decision = struct
  type t =
    | Never_inline_attribute
    | Function_body_too_large of Inlining_cost.Threshold.t
    | Stub
    | Inline of (int * Inlining_cost.Threshold.t) option
  [@@deriving yojson]
end

module Call_site_decision = struct
  type attribute_causing_inlining =
    | Unroll
    | Always
  [@@deriving yojson]

  type t =
    | Environment_says_never_inline
    | Unrolling_depth_exceeded
    | Max_inlining_depth_exceeded
    | Recursion_depth_exceeded
    | Never_inline_attribute
    | Inline of {
        attribute : attribute_causing_inlining option;
        unroll_to : int option;
      }
  [@@deriving yojson]
end

