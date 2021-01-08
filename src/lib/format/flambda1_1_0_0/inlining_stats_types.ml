
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

end
