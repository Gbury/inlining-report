
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



