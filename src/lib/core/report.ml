
(** Inlining reports

    A report is basically a tree of decisions. The
    decisions about function calls will be about
    inlining of top-level code (i.e. module
    initialization code), whereas the decisions about
    function declarations will be about the functions
    defined at toplevel.

    There are two points where one can make decision about
    inlining:
    - during the definition of a function, some computation
      can take place to determine whether it should be inlined
      or not. And thus a decision can be made (note however that
      it is not required/mandatory to make a decision at that
      point).
      This is typically what Closure does.
      Flambda1 does not perform any computation at that point
      however.
    - at a function call, it must be decided whether to inline
      the call or not, so there will always be a decision.

    Additionally, in both cases, there is also a set of
    "sub"-decisions that can be associated:
    - in the case of a function declaration, the body of the
      function may contain decisions
    - in the case of a function call, **if** the call has been
      inlined, the inlined code can itself contain decision
      about inlining.

    To handle such cases, i.e. a set of decisions about
    a piece of code (by opposition to a single decision about
    a particular function call/declaration), we index the
    decisions by their debuginfo, resulting in a map from
    debuginfos to decisions.
*)


(* Typedef *)
(* ************************************************************************* *)

type node =
  | Unknown_call
  | Known_call of known_call
  | Closure of closure (**)
(** A decision at a particular point in code
    (and the tree of sub-decision rooted at that point). *)

and known_call = {
  specializations : specialized_call list;
  after_spec  : after_spec_call;
}

and specialized_call = {
  def_dbg : Debuginfo.t;
  call_dbg : Debuginfo.t;
  orig_code_id : Code_id.t;
  spec_code_id : Code_id.t;
  spec_reason : Specialization.specialized;
  spec_analysis : map;
}

and after_spec_call = {
  dbg : Debuginfo.t;
  code_id : Code_id.t;
  no_spec_reason : Specialization.not_specialized;
  no_spec_analysis : map;
  inlining_summary : Inlining.t;
  inlining_analysis : map;
}

and closure = {
  decision : Function_declaration_decision.t;
  body : map;
}

and map = node Debuginfo.Map.t
(** A set of decisions, indexed by debuginfo. *)


type t = {
  (* Actual inlining decisions *)
  decisions: map;

  (* Meta-data about the report *)
  compilation_unit : Compilation_unit.t;
}
(** An inlining report. *)
(* TODO: add some more meta-data to the report (such as
         input files, compilation options, etc...) ? *)


(* Printing *)
(* ************************************************************************* *)

let stars fmt depth =
  Format.fprintf fmt "%s" (String.make (depth + 1) '*')

let print_code_id ~t fmt code_id =
  Code_id.print_ctx ~ctx:t.compilation_unit fmt code_id

let rec print_map ~t ~depth fmt (map : map) =
  Debuginfo.Map.iter (print_node ~t ~depth fmt) map

and print_node ~t ~depth fmt dbg (node : node) =
  match node with
  | Unknown_call ->
    Format.fprintf fmt "%a @[<v>%s of %s{%a}@;@;@[<hov>%a@]@]@\n@\n"
      stars depth
      (if depth = 0 then "Toplevel application" else "Application")
      "<unknown function>" Debuginfo.print_compact dbg
      Format.pp_print_text "The function call has not been inlined because \
                            the optimizer had not enough information about \
                            the function"
  | Known_call { specializations; after_spec } ->
    List.iter (print_specialization ~t ~depth fmt) specializations;
    print_after_spec ~t ~depth fmt after_spec
  | Closure { body; decision = { code_id; before_simplify; after_simplify; }; } ->
    Format.fprintf fmt "%a Definition of %a{%a}@\n"
      stars depth (print_code_id ~t) code_id Debuginfo.print_compact dbg;
    Format.fprintf fmt "%a @[<v>Before simplification:@;@;@[<hov>%a@]@]@\n@\n"
      stars (depth + 1)
      Function_declaration_decision.print_decision before_simplify;
    print_map ~t ~depth:(depth + 1) fmt body;
    Format.fprintf fmt "%a @[<v>After simplification of %a{%a}:@;@;@[<hov>%a@]@]@\n@\n@\n"
      stars (depth + 1) (print_code_id ~t) code_id Debuginfo.print_compact dbg
      Function_declaration_decision.print_decision after_simplify;

and print_specialization ~t ~depth fmt
    { def_dbg = _; call_dbg; orig_code_id; spec_code_id; spec_reason; spec_analysis; } =
  Format.fprintf fmt "%a @[<v>%s of %a{%a}@;@;@[<hov>%a@]@]@\n@\n"
    stars depth
    (if depth = 0 then "Toplevel application" else "Application")
    (print_code_id ~t) orig_code_id Debuginfo.print_compact call_dbg
    Specialization.summary  (Specialized spec_reason);
  (* TODO: print computation details (i.e. benefit) *)
  print_map_if_not_empty fmt spec_analysis ~t ~depth:(depth + 1)
    ~purpose:(Format.asprintf "Specialisation of %a into %a"
                (print_code_id ~t) orig_code_id (print_code_id ~t) spec_code_id)

and print_after_spec ~t ~depth fmt
    { dbg; code_id;
      no_spec_reason; no_spec_analysis;
      inlining_summary; inlining_analysis; }=
  Format.fprintf fmt "%a @[<v>%s of %a{%a}@;@;@[<hov>%a@]@;@;@[<hov>%a@]@]@\n@\n"
    stars depth
    (if depth = 0 then "Toplevel application" else "Application")
    (print_code_id ~t) code_id Debuginfo.print_compact dbg
    Specialization.summary (Not_specialized no_spec_reason)
    Inlining.summary inlining_summary;
  print_map_if_not_empty fmt no_spec_analysis
    ~purpose:"Specialization analysis" ~t ~depth:(depth + 1);
  print_map_if_not_empty fmt inlining_analysis
    ~purpose:"Inlining analysis" ~t ~depth:(depth + 1);
  ()

and print_map_if_not_empty ~purpose ~t ~depth fmt map =
  if Debuginfo.Map.is_empty map then ()
  else begin
    Format.fprintf fmt "%a @[<v>%s@]@\n@\n" stars depth purpose;
    print_map ~t ~depth:(depth + 1) fmt map
  end

let print fmt ({ decisions; compilation_unit = _; } as t) =
  print_map ~t ~depth:0 fmt decisions

