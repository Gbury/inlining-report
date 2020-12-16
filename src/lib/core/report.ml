
(** Inlining reports

    A report is basically a tree of decisions. The
    decisions about function calls will be about
    inlining of top-level code (i.e. module
    initialization code), whereas the decisions about
    function declarations will be about the functions
    defined at toplevel.
*)

(* Typedef *)
(* ************************************************************************* *)

type node = (Call_site_decision.t, Function_declaration_decision.t) Tree.node
type map = (Call_site_decision.t, Function_declaration_decision.t) Tree.t

type t = {

  (* Actual inlining decisions *)
  decisions: map;

  (* Meta-data about the report *)
  compilation_unit : Compilation_unit.t;
}
(** An inlining report. *)
(* CR gbury: add some meta-data to the report (such as
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
  | Closure { body; decision = { code_id; before_simplify; after_simplify; }; } ->
    Format.fprintf fmt "%a Definition of %a{%a}@\n"
      stars depth (print_code_id ~t) code_id Debuginfo.print_compact dbg;
    Format.fprintf fmt "%a @[<v>Before simplification:@ @ %a@]@\n@\n"
      stars (depth + 1)
      Function_declaration_decision.print_decision before_simplify;
    print_map ~t ~depth:(depth + 1) fmt body;
    Format.fprintf fmt "%a @[<v>After simplification of %a{%a}:@ @ %a@]@\n@\n@\n"
      stars (depth + 1) (print_code_id ~t) code_id Debuginfo.print_compact dbg
      Function_declaration_decision.print_decision after_simplify;
  | Call { inlined; decision = Unknown_function; } ->
    Format.fprintf fmt "%a @[<v>%s of %s{%a}@ @ %s@ %s@]@\n@\n"
      stars depth
      (if depth = 0 then "Toplevel application" else "Application")
      "<unknown function>" Debuginfo.print_compact dbg
      "The function call has not been inlined"
      "because the optimizer had not enough information about the function";
    print_map ~t ~depth:(depth + 1) fmt inlined
  | Call { inlined; decision = Non_inlinable_function { code_id; }; } ->
    Format.fprintf fmt "%a @[<v>%s of %a{%a}@ @ %s@ %s@]@\n@\n"
      stars depth
      (if depth = 0 then "Toplevel application" else "Application")
      (print_code_id ~t) code_id Debuginfo.print_compact dbg
      "The function call has not been inlined"
      "because its definition was deemed not inlinable";
    print_map ~t ~depth:(depth + 1) fmt inlined
  | Call { inlined; decision = Inlinable_function { code_id; decision; }; } ->
    Format.fprintf fmt "%a @[<v>%s of %a{%a}@ @ %a@]@\n@\n"
      stars depth
      (if depth = 0 then "Toplevel application" else "Application")
      (print_code_id ~t) code_id Debuginfo.print_compact dbg
      Call_site_decision.print_decision decision;
    print_map ~t ~depth:(depth + 1) fmt inlined

let print fmt ({ decisions; compilation_unit = _; } as t) =
  print_map ~t ~depth:0 fmt decisions

