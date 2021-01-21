
(** Definitions taken from:
    flambda2:middle_end/flambda/inlining/inlining_report.ml
    without whanges *)

type at_call_site =
  | Unknown_function
  | Non_inlinable_function of {
      code_id : Code_id.t;
    }
  | Inlinable_function of {
      code_id : Code_id.t;
      decision : Inlining_decision.Call_site_decision.t;
    }
[@@deriving yojson]

type fundecl_pass =
  | Before_simplify
  | After_simplify
[@@deriving yojson]

type at_function_declaration = {
  pass : fundecl_pass;
  code_id : Code_id.t;
  decision : Inlining_decision.Function_declaration_decision.t;
} [@@deriving yojson]

type decision =
  | At_call_site of at_call_site
  | At_function_declaration of at_function_declaration
[@@deriving yojson]

type t = {
  dbg : Debuginfo.t;
  decision : decision;
}
[@@deriving yojson]

type metadata = {
  compilation_unit : Compilation_unit.t;
} [@@deriving yojson]

type report = [ `Flambda2_1_0_0 of metadata * t list ][@@deriving yojson]


(* Conversion function *)
(* ******************* *)

(*
let conv_callsite t : Ocir_core.Call_site_decision.t =
*)

type cur =
  | Toplevel
  | Unknown_callsite of Debuginfo.t
  | Known_callsite of Debuginfo.t * Ocir_core.Report.known_call
  | Fundecl of Debuginfo.t * Code_id.t * Ocir_core.Function_declaration_decision.decision

let opt_of_map m = if Ocir_core.Debuginfo.Map.is_empty m then None else Some m

let rec conv_aux acc = function
  | [] ->
    begin match acc with
      | [ Toplevel, res ] -> res
      | _ -> assert false
    end
  | { dbg; decision; } :: r ->
    begin match decision with
      | At_function_declaration { pass = Before_simplify; code_id; decision; } ->
        let before = Inlining_decision.conv_fundecl decision in
        let acc = (Fundecl (dbg, code_id, before), Ocir_core.Debuginfo.Map.empty) :: acc in
        conv_aux acc r
      | At_function_declaration { pass = After_simplify; code_id; decision; } ->
        let after = Inlining_decision.conv_fundecl decision in
        let acc = insert_fundecl dbg code_id after acc in
        conv_aux acc r
      | At_call_site d ->
        let decision =
          match d with
          | Unknown_function -> Unknown_callsite dbg
          | Non_inlinable_function { code_id; } ->
            let code_id = Code_id.conv code_id in
            let known_call : Ocir_core.Report.known_call = {
              specializations = [];
              after_spec = {
                code_id;
                dbg = Debuginfo.conv dbg;
                no_spec_reason = Flambda2;
                no_spec_analysis = Ocir_core.Debuginfo.Map.empty;
                inlining_summary = Not_inlined Forbidden_by_decision_at_declaration;
                inlining_analysis = Ocir_core.Debuginfo.Map.empty;
              };
            } in
            Known_callsite (dbg, known_call)
          | Inlinable_function { code_id; decision; } ->
            let code_id = Code_id.conv code_id in
            let known_call : Ocir_core.Report.known_call = {
              specializations = [];
              after_spec = {
                code_id;
                dbg = Debuginfo.conv dbg;
                no_spec_reason = Flambda2;
                no_spec_analysis = Ocir_core.Debuginfo.Map.empty;
                inlining_summary = Inlining_decision.conv_callsite decision;
                inlining_analysis = Ocir_core.Debuginfo.Map.empty;
              };
            } in
            Known_callsite (dbg, known_call)
        in
        let acc = insert_callsite dbg decision acc in
        conv_aux acc r
    end

and insert_fundecl dbg code_id after = function
  | (Fundecl (dbg, code_id', before), body) :: (previous, map) :: acc' ->
    assert (code_id = code_id');
    let dbg = Debuginfo.conv dbg in
    let code_id = Code_id.conv code_id in
    let fundecl : Ocir_core.Function_declaration_decision.t = {
      code_id;
      before_simplify = before;
      after_simplify = after;
    } in
    let node : Ocir_core.Report.node =
      Closure { decision = fundecl; body; }
    in
    let map = Ocir_core.Debuginfo.Map.add dbg node map in
    (previous, map) :: acc'

  | (Unknown_callsite dbg', inlined) :: (previous, map) :: acc ->
     assert (Ocir_core.Debuginfo.Map.is_empty inlined);
     let dbg' = Debuginfo.conv dbg' in
     let node : Ocir_core.Report.node = Unknown_call in
     let map = Ocir_core.Debuginfo.Map.add dbg' node map in
     insert_fundecl dbg code_id after ((previous, map) :: acc)

  | (Known_callsite (dbg', known_call), inlined) :: (previous, map) :: acc ->
    let dbg' = Debuginfo.conv dbg' in
    let node : Ocir_core.Report.node =
      Known_call { known_call with after_spec = {
          known_call.after_spec with inlining_analysis = inlined; }; }
    in
    let map = Ocir_core.Debuginfo.Map.add dbg' node map in
    insert_fundecl dbg code_id after ((previous, map) :: acc)

  (* These are invalid stacks *)
  | (Toplevel, _) :: _
  | [] | _ :: [] -> assert false


and insert_callsite dbg cur = function

  | (((Fundecl _ | Toplevel), _) :: _) as acc ->
    (cur, Ocir_core.Debuginfo.Map.empty) :: acc

  | (((Unknown_callsite prefix, _) :: _) as acc)
  | (((Known_callsite (prefix, _), _) :: _) as acc)
    when Debuginfo.is_prefix ~prefix dbg ->
    (cur, Ocir_core.Debuginfo.Map.empty) :: acc

  | (Unknown_callsite dbg', inlined) :: (previous, map) :: acc ->
    assert (Ocir_core.Debuginfo.Map.is_empty inlined);
    let dbg' = Debuginfo.conv dbg' in
    let node : Ocir_core.Report.node = Unknown_call in
    let map = Ocir_core.Debuginfo.Map.add dbg' node map in
    insert_callsite dbg cur ((previous, map) :: acc)

  | (Known_callsite (dbg', known_call), inlined) :: (previous, map) :: acc ->
    let dbg' = Debuginfo.conv dbg' in
    let node : Ocir_core.Report.node =
      Known_call { known_call with after_spec = {
          known_call.after_spec with inlining_analysis = inlined; }; }
    in
    let map = Ocir_core.Debuginfo.Map.add dbg' node map in
    insert_callsite dbg cur ((previous, map) :: acc)

  (* These are invalid stacks *)
  | []
  | (Known_callsite _, _) :: []
  | (Unknown_callsite _, _) :: []
    -> assert false

let conv (`Flambda2_1_0_0 (meta, l) : report) : Ocir_core.Report.t =
  let compilation_unit = Compilation_unit.conv meta.compilation_unit in
  let decisions = conv_aux [Toplevel, Ocir_core.Debuginfo.Map.empty] l in
  { decisions; compilation_unit; }


