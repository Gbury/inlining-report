
(** Alias to provide a uniform interface across inlining report formats *)

type t = [ | Inlining_report.report ][@@deriving yojson]


(* Conversion Function *)
(* ******************* *)

type cur =
  | Toplevel
  | Callsite of Debuginfo.t * Ocir_core.Call_site_decision.t
  | Fundecl of Debuginfo.t * Code_id.t * Ocir_core.Function_declaration_decision.decision

let rec conv_aux acc = function
  | [] ->
    begin match acc with
      | [ Toplevel, res ] -> res
      | _ -> assert false
    end
  | Inlining_report.{ dbg; decision; } :: r ->
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
        let decision = Inlining_report.conv_callsite d in
        let acc = insert_callsite dbg decision acc in
        conv_aux acc r
    end

and insert_fundecl dbg code_id after = function
  | (Fundecl (dbg, code_id', before), body) :: (previous, map) :: acc' ->
    (* assert (dbg = dbg'); *)
    assert (code_id = code_id');
    let dbg = Debuginfo.conv dbg in
    let code_id = Code_id.conv code_id in
    let fundecl : Ocir_core.Function_declaration_decision.t = {
      code_id;
      before_simplify = before;
      after_simplify = after;
    } in
    let node : (_, _) Ocir_core.Tree.node =
      Closure { decision = fundecl; body; }
    in
    let map = Ocir_core.Debuginfo.Map.add dbg node map in
    (previous, map) :: acc'

  | (Callsite (dbg', decision'), inlined) :: (previous, map) :: acc
    (* not Ocir_core.Debuginfo.is_prefix ~prefix dbg *) ->
    let dbg' = Debuginfo.conv dbg' in
    let node : (_, _) Ocir_core.Tree.node =
      Call { decision = decision'; inlined; }
    in
    let map = Ocir_core.Debuginfo.Map.add dbg' node map in
    insert_fundecl dbg code_id after ((previous, map) :: acc)

  (* These are invalid stacks *)
  | (Toplevel, _) :: _
  | [] | _ :: [] -> assert false


and insert_callsite dbg decision = function

  | (((Fundecl _ | Toplevel), _) :: _) as acc ->
    (Callsite (dbg, decision), Ocir_core.Debuginfo.Map.empty) :: acc

  | ((Callsite (prefix, _), _) :: _) as acc
    when Debuginfo.is_prefix ~prefix dbg ->
    (Callsite (dbg, decision), Ocir_core.Debuginfo.Map.empty) :: acc

  | (Callsite (dbg', decision'), inlined) :: (previous, map) :: acc
    (* not Debuginfo.is_prefix ~prefix dbg *) ->
    let dbg' = Debuginfo.conv dbg' in
    let node : (_, _) Ocir_core.Tree.node =
      Call { decision = decision'; inlined; }
    in
    let map = Ocir_core.Debuginfo.Map.add dbg' node map in
    insert_callsite dbg decision ((previous, map) :: acc)

  (* These are invalid stacks *)
  | [] | (Callsite _, _) :: [] -> assert false

let conv (`Flambda2_1_0_0 (meta, l) : t) : Ocir_core.Report.t =
  let compilation_unit = Compilation_unit.conv meta.compilation_unit in
  let decisions = conv_aux [Toplevel, Ocir_core.Debuginfo.Map.empty] l in
  { decisions; compilation_unit; }

