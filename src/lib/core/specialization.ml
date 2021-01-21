
(** Specialization decisions

    This defines the various decisions (and associated reasons) that
    can occur with regards to specialization.
*)

(* Type definitions *)
(* ************************************************************************* *)

type attribute_forcing =
  | Always              (** the [@specialized] attribute *)
  | Unknown             (** the exact attribute is missing *)
(** The attrbiute(s?) that can force specialization of a function at
    call site. *)

type attribute_forbidding =
  | Never               (** the [@specialize never] attribute *)
  | Unknown             (** the exact attribute is missing *)
(** The attribute that can prevent specialization of a function at
    call site. *)

type specialized =

  | Forced_by_attribute of attribute_forcing
  (** The specified attribute forces specialization. *)

  | Without_subfunctions of Whether_sufficient_benefit.t
  (** Regular case of a function with a benefit computation that is enough
      to justify inlining. *)

  | With_subfunctions of
      Whether_sufficient_benefit.t *
      Whether_sufficient_benefit.t
  (** TODO: understand what differs with the previous case and what the first
      benefit refers to. *)


type not_specialized =

  | Flambda2
  (** Currently, flambda2 does not perform any specialization. *)

  | Classic_mode
  (** In `-OClassic` mode, specialization never happens because it never
      happens in Closure. *)

  | Forbidden_by_attribute of attribute_forbidding
  (** An unspecified attribute has prevented inlining. *)

  | Above_threshold of int option
  (** From flambda1: there is not enough remaining inlining "fuel" to
      inline the function, i.e. the maximum inlining threshold has been
      reached, thus the function wasn't inlined. *)

  | Not_recursive
  (** The function is not recursive hence no specialization is
      required/useful. *)

  | Not_closed
  (** The function is not closed, which prevents specialization. *)

  | No_invariant_parameters
  (** None of the function's parameters could be determined to be
      invariant, hence there is no use in specializing. *)

  | No_useful_approximations
  (** There was no useful approximations for any of the invariant
      parameters, hence the specialization couldn't produce any
      simplification. *)

  | Self_call
  (** This is a self/recursive call, specialization cannot happen. *)

  | Not_beneficial of
      Whether_sufficient_benefit.t *
      Whether_sufficient_benefit.t
  (** Benefit computation concluded there wasn't enough benefit to
      specialize.
      TODO: understand what the two benefits represent. *)

  | Max_inlining_depth_exceeded
  (** The maximum inlining depth has been exceeded.
      This maximum depth is usually set by command line options
      of the compiler invocation, typically by one of the `-O`
      options. Exceeding this depth prevents any inlining or
      specialization. *)

type t =
  | Specialized of specialized
  | Not_specialized of not_specialized


(* Type definitions *)
(* ************************************************************************* *)

let print_threshold_opt fmt = function
  | None -> ()
  | Some i -> Format.fprintf fmt " (%d)" i

let explain_why fmt = function
  | Forced_by_attribute Always ->
    Format.fprintf fmt "of the [@specialized] attribute"
  | Forced_by_attribute Unknown ->
    Format.fprintf fmt "of an annotation"
  | Without_subfunctions _
  | With_subfunctions _ ->
    Format.fprintf fmt "the expected benefit outweigh the change in code size"

let explain_why_not fmt = function
  | Flambda2 ->
    Format.pp_print_text fmt "flambda2 does not currently perform any specialization"
  | Classic_mode ->
    Format.pp_print_text fmt "the `-OClassic` mode prevents any specialization"
  | Forbidden_by_attribute Never ->
    Format.pp_print_text fmt "of the [@specialized never] attribute"
  | Forbidden_by_attribute Unknown ->
    Format.pp_print_text fmt "of an annotation"
  | Above_threshold opt ->
    Format.fprintf fmt "it is larger than the remaining size threshold%a"
      print_threshold_opt opt
  | Not_recursive ->
    Format.fprintf fmt "it is not recursive"
  | Not_closed ->
    Format.fprintf fmt "it is not closed"
  | No_invariant_parameters ->
    Format.fprintf fmt "it has no invariant parameters"
  | No_useful_approximations ->
    Format.pp_print_text fmt "there was no useful information about \
                              any of its invariant parameters"
  | Self_call ->
    Format.pp_print_text fmt "it was a self call"
  | Not_beneficial _ ->
    Format.pp_print_text fmt "the expected benefit did not outweigh the change in code size"
  | Max_inlining_depth_exceeded ->
    Format.pp_print_text fmt "the maximum inlining depth was exceeded"

let summary fmt = function
  | Specialized specialized ->
    Format.fprintf fmt "The function was specialized because %a" explain_why specialized
  | Not_specialized not_specialized ->
    Format.fprintf fmt "The function was not specialized because %a" explain_why_not not_specialized

