
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

type t =
  | Specialized of Specialization.specialized
  | Inlined of Specialization.not_specialized * Inlining.inlined
  | Unchanged of Specialization.not_specialized * Inlining.not_inlined

(* Printing *)
(* ************************************************************************* *)

let print_and_explain fmt = function
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
  | Unchanged (not_specialized, not_inlined) ->
    Format.fprintf fmt
      "@[<v>@[<hov>The function was not specialized because %a.@]@ \
            @[<hov>The function call was not inlined because %a@].@]"
      Specialization.explain_why_not not_specialized
      Inlining.explain_why_not not_inlined


