
(** Call site decisions

    This defines the kind of decisions tha can take place
    when processing a function call.
    Closure will typically inline base on the declaration info
    (if it is available).
    On the other hand, flambda1 has a more detailed notion of
    benefit, not yet integrated into this type (TODO: this, ^^).
*)

(* Type definitions *)
(* ************************************************************************* *)

type decision =
  | Specialized of Specialization.specialized
  | Inlined of Specialization.not_specialized * Inlining.inlined
  | Nothing of Specialization.not_specialized * Inlining.not_inlined

type t =
  | Unknown_function
  (** nothing can be done for unknown functions *)
  | Known_function of {
      code_id : Code_id.t;
      decision : decision;
    }
  (** case for known functions. *)
(** The various situations that can be encountered at a function call. *)

(* Printing *)
(* ************************************************************************* *)

let print_and_explain_decision fmt = function
  | Specialized specialized ->
    Format.fprintf fmt
      "@[<v>@[<hov>The function was specialized because %a.@]@ \
            The function call was not inlined because it was specialized.@]"
      Specialization.explain_why specialized
  | Inlined (not_specialized, inlined) ->
    Format.fprintf fmt
      "@[<v>@[<hov>The function was not specialized because %a.@]@ \
            @[<hov>The function call was inlined because %a@].@]"
      Specialization.explain_why_not not_specialized
      Inlining.explain_why inlined
  | Nothing (not_specialized, not_inlined) ->
    Format.fprintf fmt
      "@[<v>@[<hov>The function was not specialized because %a.@]@ \
            @[<hov>The function call was not inlined because %a@].@]"
      Specialization.explain_why_not not_specialized
      Inlining.explain_why_not not_inlined


