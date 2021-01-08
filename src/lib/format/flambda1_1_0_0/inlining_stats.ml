
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

    let compare ((d1, cl1, k1) : t) ((d2, cl2, k2) : t) =
      let c = Debuginfo.compare d1 d2 in
      if c <> 0 then c else
        let c = Closure_id.compare cl1 cl2 in
        if c <> 0 then c else
          match k1, k2 with
          | Closure, Closure -> 0
          | Call, Call -> 0
          | Closure, Call -> 1
          | Call, Closure -> -1
  end

  module Place_map = struct
    include Map.Make(Place)

    (* TODO: fix these one day *)
    let t_of_yojson _ _ =
      failwith "json serialization is not available for flambda1 inlining reports"

    let yojson_of_t _ _ =
      failwith "json serialization is not available for flambda1 inlining reports"
  end

  type t = node Place_map.t
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

