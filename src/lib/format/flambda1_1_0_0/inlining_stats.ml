
(** Definition taken from:
    flambda1:middle_end/flambda/inlining_stats.ml
*)

module Inlining_report = struct

  module Place = struct

    type kind =
      | Closure
      | Call
    [@@deriving yojson]

    type t = Debuginfo.t * Closure_id.t * kind
    [@@deriving yojson]

  end

  (* Flambda1 uses maps from Places to nodes, but for conveninence
     wrt to serilization, we simply use an association list *)
  type t = (Place.t * node) list
  [@@deriving yojson]

  and node =
    | Closure of t
    | Call of call
  [@@deriving yojson]

  and call =
    { decision: Inlining_stats_types.Decision.t option;
      inlined: t option;
      specialised: t option; }
  [@@deriving yojson]

  type metadata = {
    compilation_unit : Compilation_unit.t;
  }
  [@@deriving yojson]

  type report = [ `Flambda1_1_0_0 of metadata * t ]
  [@@deriving yojson]

end

