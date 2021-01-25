
(** Difference between inlining reports

    This represents the difference between two inlining reports.
    Currently we will stop at the first difference.

*)


(* Utility traversal functions *)
(* ************************************************************************* *)

let map_iter2 ~f map1 map2 =
  let rec aux seq1 seq2 =
    match (seq1 : _ Seq.node), (seq2 : _ Seq.node) with
    | Nil, Nil -> ()
    | Nil, Cons ((dbg, v), seq') ->
      f dbg None (Some v);
      aux seq1 (seq' ())
    | Cons ((dbg, v), seq'), Nil ->
      f dbg (Some v) None;
      aux (seq' ()) seq2
    | Cons ((dbg1, v1), seq1'),
      Cons ((dbg2, v2), seq2') ->
      let c = Debuginfo.compare dbg1 dbg2 in
      if c = 0 then begin
        f dbg1 (Some v1) (Some v2);
        aux (seq1' ()) (seq2' ())
      end else if c < 0 then begin
        f dbg1 (Some v1) None;
        aux (seq1' ()) seq2
      end else (* c > 0 *) begin
        f dbg2 None (Some v2);
        aux seq1 (seq2' ())
      end
  in
  let seq1 = Debuginfo.Map.to_seq map1 in
  let seq2 = Debuginfo.Map.to_seq map2 in
  aux (seq1 ()) (seq2 ())

(* Diff summary *)
(* ************************************************************************* *)

type summary = {
  (* Inlinings/specializations that only occur on the left side *)
  mutable left_only_inlining : int;
  mutable left_only_specializations : int;
  (* Inlinings/specializations that only occur on the right side *)
  mutable right_only_inlining : int;
  mutable right_only_specializations : int;
}

let summary = {
  left_only_inlining = 0;
  left_only_specializations = 0;
  right_only_inlining = 0;
  right_only_specializations = 0;
}

let reset_summary () =
  summary.left_only_inlining <- 0;
  summary.left_only_specializations <- 0;
  summary.right_only_inlining <- 0;
  summary.right_only_specializations <- 0;
  ()

let print_summary fmt () =
  let { left_only_inlining;
        left_only_specializations;
        right_only_inlining;
        right_only_specializations; }
    = summary
  in
  if left_only_inlining = 0 &&
     left_only_specializations = 0 &&
     right_only_inlining = 0 &&
     right_only_specializations = 0 then
    Format.fprintf fmt "No differences found@."
  else
    Format.fprintf fmt "@[<v 2>Summary:@;\
                        * left only inlinings : %d@;\
                        * left only specializations : %d@;\
                        * right only inlinings : %d@;\
                        * right only specializations : %d@;\
                        @]@."
      left_only_inlining
      left_only_specializations
      right_only_inlining
      right_only_specializations

let add_left_only_inlining n =
  summary.left_only_inlining <- summary.left_only_inlining + n
let add_left_only_specialization n =
  summary.left_only_specializations <- summary.left_only_specializations + n
let add_right_only_inlining n =
  summary.right_only_inlining <- summary.right_only_inlining + n
let add_right_only_specialization n =
  summary.right_only_specializations <- summary.right_only_specializations + n


(* Diff traversal *)
(* ************************************************************************* *)

let rec map ~left ~right ~depth fmt (l, r) =
  map_iter2 l r ~f:(fun dbg l r ->
      match l, r with
      | None, None -> assert false
      | None, Some Report.Unknown_call
      | Some Report.Unknown_call, None -> ()
      | None, Some _ ->
        Format.fprintf fmt "WARNING: Missing event on the left at %a@\n@\n"
          Debuginfo.print_compact dbg
      | Some _, None ->
        Format.fprintf fmt "WARNING: Missing event on the right %a@\n@\n"
          Debuginfo.print_compact dbg
      | Some l, Some r ->
        node ~left ~right ~depth fmt (dbg, l, r)
    )

and node ~left ~right ~depth fmt (dbg, l, r) =
  match (l : Report.node), (r : Report.node) with
  | Unknown_call, Unknown_call -> ()
  | Known_call l_call, Known_call r_call ->
    known_call ~left ~right ~depth fmt (dbg, l_call, r_call)
  | Closure l_clos, Closure r_clos ->
    closure ~left ~right ~depth fmt (dbg, l_clos, r_clos)
  | Unknown_call, (Known_call _| Closure _)
  | Known_call _, (Unknown_call | Closure _)
  | Closure _, (Unknown_call | Known_call _)
    -> Format.fprintf fmt "Incoherent events at %a" Debuginfo.print_compact dbg

and known_call ~left ~right ~depth fmt (dbg, l, r) =
  specializations ~left ~right ~depth fmt
    (dbg, l.Report.specializations, r.Report.specializations);
  after_spec_call ~left ~right ~depth fmt
    (dbg, l.Report.after_spec, r.Report.after_spec)

and specializations ~left ~right ~depth fmt (dbg, l, r) =
  match l, r with
  | [], [] -> ()
  | [], s :: rest ->
    let n = Lazy.force depth in
    add_right_only_specialization 1;
    Format.fprintf fmt "%a @[<v>Specialization of ∅::%a->%a{%a}@;@;\
      Specialized on the right but not on the left@]@\n@\n"
      Report.stars n
      (Report.print_code_id ~t:right) s.Report.orig_code_id
      (Report.print_code_id ~t:right) s.Report.spec_code_id
      Debuginfo.print_compact dbg;
    specializations ~left ~right ~depth fmt (dbg, l, rest)
  | s :: rest, [] ->
    let n = Lazy.force depth in
    add_left_only_specialization 1;
    Format.fprintf fmt "%a @[<v>Specialization of %a->%a::∅{%a}@;@;\
      Specialized on the left but not on the right@]@\n@\n"
      Report.stars n
      (Report.print_code_id ~t:left) s.Report.orig_code_id
      (Report.print_code_id ~t:left) s.Report.spec_code_id
      Debuginfo.print_compact dbg;
    specializations ~left ~right ~depth fmt (dbg, rest, r)
  | l :: l_rest, r :: r_rest ->
    specialization ~left ~right ~depth fmt (dbg, l, r);
    specializations ~left ~right ~depth fmt (dbg, l_rest, r_rest)

and specialization ~left ~right ~depth fmt (dbg, l, r) =
  let depth = lazy (
    let n = Lazy.force depth in
    Format.fprintf fmt "%a @[<v>Specialized bodies of %a->%a::%a->%a{%a}@]@\n@\n"
      Report.stars n
      (Report.print_code_id ~t:left) l.Report.orig_code_id
      (Report.print_code_id ~t:left) l.Report.spec_code_id
      (Report.print_code_id ~t:right) r.Report.orig_code_id
      (Report.print_code_id ~t:right) r.Report.spec_code_id
      Debuginfo.print_compact dbg;
    n + 1
  ) in
  map ~left ~right ~depth fmt (l.Report.spec_analysis, r.Report.spec_analysis)

and after_spec_call ~left ~right ~depth fmt (dbg, l, r) =
  match l.Report.inlining_summary, r.Report.inlining_summary with
  | Not_inlined _, Not_inlined _ -> ()
  | Inlined _, Inlined _ ->
    let depth = lazy (
      let n = Lazy.force depth in
      Format.fprintf fmt "%a @[<v>Inlined body of %a::%a{%a}@]@\n@\n"
        Report.stars n
        (Report.print_code_id ~t:left) l.Report.code_id
        (Report.print_code_id ~t:right) r.Report.code_id
        Debuginfo.print_compact dbg;
      n + 1
    ) in
    map ~left ~right ~depth fmt (l.Report.inlining_analysis, r.Report.inlining_analysis)
  | Not_inlined _, Inlined _ ->
    let n = Lazy.force depth in
    add_right_only_inlining 1;
    Format.fprintf fmt "%a @[<v>Application of %a::%a{%a}@;@;%s@]@\n@\n"
      Report.stars n
      (Report.print_code_id ~t:left) l.Report.code_id
      (Report.print_code_id ~t:right) r.Report.code_id
      Debuginfo.print_compact dbg
      "Inlined on the right but not on the left"
  | Inlined _, Not_inlined _ ->
    let n = Lazy.force depth in
    add_left_only_inlining 1;
    Format.fprintf fmt "%a @[<v>Application of %a::%a{%a}@;@;%s@]@\n@\n"
      Report.stars n
      (Report.print_code_id ~t:left) l.Report.code_id
      (Report.print_code_id ~t:right) r.Report.code_id
      Debuginfo.print_compact dbg
      "Inlined on the left but not on the right"

and closure ~left ~right ~depth fmt (dbg, l, r) =
  (* Decision at the definition are too different to be interesting to compare
     (e.g. flambda1 will never have any decision and flambda1 always one, so the
     comparison would be mostly noise),
     so we only compare the decisions inside the functions bodies for now. *)
  let depth = lazy (
    let n = Lazy.force depth in
    Format.fprintf fmt "%a Definition of %a:%a{%a}@\n"
      Report.stars n
      (Report.print_code_id ~t:left) l.Report.decision.code_id
      (Report.print_code_id ~t:right) r.Report.decision.code_id
      Debuginfo.print_compact dbg;
    n + 1
  ) in
  map ~left ~right ~depth fmt (l.Report.body, r.Report.body)

let report fmt (left, right) =
  reset_summary ();
  map ~left ~right ~depth:(lazy 1) fmt
    (left.Report.decisions, right.Report.decisions);
  print_summary fmt ()


