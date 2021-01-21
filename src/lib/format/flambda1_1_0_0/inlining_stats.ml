
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
          conv_closure acc dbg closure_id closure

        | Call, Call call ->
          let dbg, known_call = conv_call dbg closure_id call in
          let node = Ocir_core.Report.Known_call known_call in
          Ocir_core.Debuginfo.Map.add dbg node acc

        (* These should not happen *)
        | Closure, Call _
        | Call, Closure _ -> assert false

      ) t Ocir_core.Debuginfo.Map.empty

  and conv_closure acc dbg closure_id closure =
    let body = conv_aux closure in
    let dbg = Debuginfo.conv dbg in
    let code_id = Closure_id.conv closure_id in
    let decision : Ocir_core.Function_declaration_decision.t = {
      code_id;
      before_simplify = No_decision_flambda1;
      after_simplify = No_decision_flambda1;
    } in
    let node = Ocir_core.Report.Closure { decision; body; } in
    Ocir_core.Debuginfo.Map.add dbg node acc

  and conv_call dbg closure_id call =
    let dbg = Debuginfo.conv dbg in
    let code_id = Closure_id.conv closure_id in
    begin match call.decision with
      | None -> assert false (* a missing call decision is an error
                                (flambda1 should not emit such reports) *)
      | Some Prevented Function_prevented_from_inlining ->
        conv_simple_call dbg code_id call
          (Ocir_core.Specialization.Above_threshold None)
          (Ocir_core.Inlining.Not_inlined (Above_threshold None))
      | Some Prevented Level_exceeded ->
        conv_simple_call dbg code_id call
          (Ocir_core.Specialization.Max_inlining_depth_exceeded)
          (Ocir_core.Inlining.Not_inlined Max_inlining_depth_exceeded)
      | Some Inlined (not_specialised, inlined) ->
        conv_simple_call dbg code_id call
          (Inlining_stats_types.Not_specialised.conv not_specialised)
          (Ocir_core.Inlining.Inlined (Inlining_stats_types.Inlined.conv inlined))
      | Some Unchanged (not_specialised, not_inlined) ->
        conv_simple_call dbg code_id call
          (Inlining_stats_types.Not_specialised.conv not_specialised)
          (Ocir_core.Inlining.Not_inlined (Inlining_stats_types.Not_inlined.conv not_inlined))
      | Some Specialised specialised ->
        conv_specialised dbg code_id call
          (Inlining_stats_types.Specialised.conv specialised)
    end

  and conv_simple_call dbg code_id call no_spec_reason inlining_summary =
    let after_spec : Ocir_core.Report.after_spec_call = {
      dbg; code_id;
      no_spec_reason;
      no_spec_analysis = conv_opt call.specialised;
      inlining_summary;
      inlining_analysis = conv_opt call.inlined;
    } in
    let known_call : Ocir_core.Report.known_call = { specializations = []; after_spec; } in
    dbg, known_call

  and conv_specialised dbg orig_code_id call spec_reason =
    assert (call.inlined = None);
    match call.specialised with
    | None -> assert false
    | Some analysis ->
      let def_dbg, def, call_debug, call, new_closure_id =
        split_specialisation_analysis analysis
      in
      let def_dbg = Debuginfo.conv def_dbg in
      let call_dbg = Debuginfo.conv call_debug in
      assert (Ocir_core.Debuginfo.compare dbg call_dbg = 0);
      (* Create the specialised call *)
      let spec_code_id = Closure_id.conv new_closure_id in
      let spec_analysis = conv_aux def in
      let spec_call : Ocir_core.Report.specialized_call = {
        def_dbg; call_dbg;
        orig_code_id; spec_code_id;
        spec_reason; spec_analysis;
      } in
      (* Recurse on the application of the specialised version *)
      let dbg', known_call = conv_call call_debug new_closure_id call in
      assert (Ocir_core.Debuginfo.compare dbg dbg' = 0);
      let known_call : Ocir_core.Report.known_call =
        { known_call with specializations = spec_call :: known_call.specializations; }
      in
      call_dbg, known_call

  and split_specialisation_analysis analysis =
    let def, call = Place_map.partition (fun key _ ->
        match key with
        | (_, _, Closure) -> true
        | (_, _, Call) -> false
      ) analysis
    in
    assert (Place_map.cardinal def = 1);
    assert (Place_map.cardinal call = 1);
    let def_dbg, def_closure_id, def =
      match Place_map.choose def with
      | (def_dbg, def_closure_id, Closure), Closure t ->
        def_dbg, def_closure_id, t
      | _ -> assert false
    in
    let call_dbg, call_closure_id, call =
      match Place_map.choose call with
      | (call_dbg, call_closure_id, Call), Call call ->
        call_dbg, call_closure_id, call
      | _ -> assert false
    in
    assert (Closure_id.compare def_closure_id call_closure_id = 0);
    def_dbg, def, call_dbg, call, def_closure_id

  and conv_opt = function
    | None -> Ocir_core.Debuginfo.Map.empty
    | Some t -> conv_aux t

  let conv (`Flambda1_1_0_0 (meta, t) : report) : Ocir_core.Report.t =
    let compilation_unit = Compilation_unit.conv meta.compilation_unit in
    let decisions = conv_aux t in
    { decisions; compilation_unit; }

end

