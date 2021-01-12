
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

  let rec conv_aux t =
    Place_map.fold (fun (dbg, closure_id, kind) node acc ->
        match kind, node with
        | Closure, Closure closure ->
          let body = conv_aux closure in
          let dbg = Debuginfo.conv dbg in
          let code_id = Closure_id.conv closure_id in
          let decision : Ocir_core.Function_declaration_decision.t = {
            code_id;
            before_simplify = No_decision_flambda1;
            after_simplify = No_decision_flambda1;
          } in
          let node = Ocir_core.Tree.Closure { decision; body; } in
          Ocir_core.Debuginfo.Map.add dbg node acc
        | Call, Call call ->
          let dbg = Debuginfo.conv dbg in
          let code_id = Closure_id.conv closure_id in
          let inlined = Option.map conv_aux call.inlined in
          let specialized = Option.map conv_aux call.specialised in
          let decision = match call.decision with
            | Some d -> Inlining_stats_types.Decision.conv d
            | None -> assert false (* a missing call decision is an error
                                      (flambda1 should not emit such reports) *)
          in
          let decision =
            Ocir_core.Call_site_decision.Known_function { code_id; decision; }
          in
          let node = Ocir_core.Tree.Call { decision; inlined; specialized; } in
          Ocir_core.Debuginfo.Map.add dbg node acc
        (* These should not happen *)
        | Closure, Call _
        | Call, Closure _ -> assert false
      ) t Ocir_core.Debuginfo.Map.empty

  let conv (`Flambda1_1_0_0 (meta, t) : report) : Ocir_core.Report.t =
    let compilation_unit = Compilation_unit.conv meta.compilation_unit in
    let decisions = conv_aux t in
    { decisions; compilation_unit; }

end

