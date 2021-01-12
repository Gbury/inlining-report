
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

(* Conversion functions *)
(* ******************** *)

let conv_fundecl t : Ocir_core.Function_declaration_decision.decision =
  match (t : Function_declaration_decision.t) with
  | Stub -> Stub
  | Never_inline_attribute -> Never_inline_attribute
  | Function_body_too_large threshold ->
    Function_body_too_large (Inlining_cost.conv threshold)
  | Inline None -> Inline Attribute
  | Inline Some (body_size, threshold) ->
    Inline (Size { body_size ; size_threshold = Inlining_cost.conv threshold; })

let conv_callsite t : Ocir_core.Call_site_decision.decision =
  match (t : Call_site_decision.t) with
  | Environment_says_never_inline ->
    Unchanged (Flambda2, Environment_says_never_inline)
  | Unrolling_depth_exceeded ->
    Unchanged(Flambda2, Unrolling_depth_exceeded)
  | Max_inlining_depth_exceeded ->
    Unchanged(Flambda2, Max_inlining_depth_exceeded)
  | Recursion_depth_exceeded ->
    Unchanged(Flambda2, Recursion_depth_exceeded)
  | Never_inline_attribute ->
    Unchanged(Flambda2, Forbidden_by_attribute Never)
  | Inline { attribute = None; unroll_to = None; } ->
    Inlined(Flambda2, Forced_by_decision_at_declaration)
  | Inline { attribute = Some Always; unroll_to = _; } ->
    Inlined(Flambda2, Forced_by_attribute Always)
  | Inline { attribute = Some Unroll; unroll_to = Some n; } ->
    Inlined(Flambda2, Forced_by_attribute (Unroll n))
  | Inline { attribute = None; unroll_to = Some _; }
  | Inline { attribute = Some Unroll; unroll_to = None; }
    -> assert false



