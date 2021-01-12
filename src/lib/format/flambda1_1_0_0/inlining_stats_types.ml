
(** Definitions taken from:
    flambda1:middle_end/flambda/inlining_stats_type.ml
*)

module Inlined = struct
  type t =
    | Classic_mode
    | Annotation
    | Decl_local_to_application
    | Without_subfunctions of
        Inlining_cost.Whether_sufficient_benefit.t
    | With_subfunctions of
        Inlining_cost.Whether_sufficient_benefit.t
        * Inlining_cost.Whether_sufficient_benefit.t
  [@@deriving yojson]

  let conv t : Ocir_core.Inlining.inlined =
    match (t : t) with
    | Classic_mode -> Forced_by_decision_at_declaration
    | Annotation -> Forced_by_attribute Unknown
    | Decl_local_to_application -> Declaration_local_to_application
    | Without_subfunctions wsb ->
      Without_subfunctions (Inlining_cost.Whether_sufficient_benefit.conv wsb)
    | With_subfunctions (wsb, wsb') ->
      With_subfunctions (Inlining_cost.Whether_sufficient_benefit.conv wsb,
                         Inlining_cost.Whether_sufficient_benefit.conv wsb')

end

module Not_inlined = struct
  type t =
    | Classic_mode
    | Above_threshold of int
    | Annotation
    | No_useful_approximations
    | Unrolling_depth_exceeded
    | Self_call
    | Without_subfunctions of
        Inlining_cost.Whether_sufficient_benefit.t
    | With_subfunctions of
        Inlining_cost.Whether_sufficient_benefit.t
        * Inlining_cost.Whether_sufficient_benefit.t
  [@@deriving yojson]

  let conv t : Ocir_core.Inlining.not_inlined =
    match (t : t) with
    | Classic_mode -> Forbidden_by_decision_at_declaration
    | Above_threshold t -> Above_threshold (Some t)
    | Annotation -> Forbidden_by_attribute Unknown
    | No_useful_approximations -> No_useful_approximations
    | Unrolling_depth_exceeded -> Unrolling_depth_exceeded
    | Self_call -> Self_call
    | Without_subfunctions wsb ->
      Without_subfunctions (Inlining_cost.Whether_sufficient_benefit.conv wsb)
    | With_subfunctions (wsb, wsb') ->
      With_subfunctions (Inlining_cost.Whether_sufficient_benefit.conv wsb,
                         Inlining_cost.Whether_sufficient_benefit.conv wsb')

end

module Specialised = struct
  type t =
    | Annotation
    | Without_subfunctions of
        Inlining_cost.Whether_sufficient_benefit.t
    | With_subfunctions of
        Inlining_cost.Whether_sufficient_benefit.t
        * Inlining_cost.Whether_sufficient_benefit.t
  [@@deriving yojson]

  let conv t : Ocir_core.Specialization.specialized =
    match (t : t) with
    | Annotation -> Forced_by_attribute Unknown
    | Without_subfunctions wsb ->
      Without_subfunctions (Inlining_cost.Whether_sufficient_benefit.conv wsb)
    | With_subfunctions (wsb, wsb') ->
      With_subfunctions (Inlining_cost.Whether_sufficient_benefit.conv wsb,
                         Inlining_cost.Whether_sufficient_benefit.conv wsb')

end

module Not_specialised = struct
  type t =
    | Classic_mode
    | Above_threshold of int
    | Annotation
    | Not_recursive
    | Not_closed
    | No_invariant_parameters
    | No_useful_approximations
    | Self_call
    | Not_beneficial of
        Inlining_cost.Whether_sufficient_benefit.t
        * Inlining_cost.Whether_sufficient_benefit.t
  [@@deriving yojson]

  let conv t : Ocir_core.Specialization.not_specialized =
    match (t: t) with
    | Classic_mode -> Classic_mode
    | Above_threshold t -> Above_threshold (Some t)
    | Annotation -> Forbidden_by_attribute Unknown
    | Not_recursive -> Not_recursive
    | Not_closed -> Not_closed
    | No_invariant_parameters -> No_invariant_parameters
    | No_useful_approximations -> No_useful_approximations
    | Self_call -> Self_call
    | Not_beneficial (wsb, wsb') ->
      Not_beneficial (Inlining_cost.Whether_sufficient_benefit.conv wsb,
                      Inlining_cost.Whether_sufficient_benefit.conv wsb')

end

module Prevented = struct
  type t =
    | Function_prevented_from_inlining
    | Level_exceeded
  [@@deriving yojson]
end

module Decision = struct

  type t =
    | Prevented of Prevented.t
    | Specialised of Specialised.t
    | Inlined of Not_specialised.t * Inlined.t
    | Unchanged of Not_specialised.t * Not_inlined.t
  [@@deriving yojson]

  let conv t : Ocir_core.Call_site_decision.decision =
    match (t : t) with
    | Prevented Function_prevented_from_inlining ->
      Unchanged (Above_threshold None, Above_threshold None)
    | Prevented Level_exceeded ->
      Unchanged (Max_inlining_depth_exceeded, Max_inlining_depth_exceeded)
    | Specialised specialised ->
      Specialized (Specialised.conv specialised)
    | Inlined (not_specialised, inlined) ->
      Inlined (Not_specialised.conv not_specialised, Inlined.conv inlined)
    | Unchanged (not_specialised, not_inlined) ->
      Unchanged (Not_specialised.conv not_specialised, Not_inlined.conv not_inlined)

end
