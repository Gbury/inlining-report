
(** Specialization decisions

    This defines the various decisions (and associated reasons) that
    can occur with regards to specialization.
*)

(* Type definitions *)
(* ************************************************************************* *)

type attribute_forcing =
  | Always              (** the [@specialized] attribute *)
(** The attrbiute(s?) that can force specialization of a function at
    call site. *)

type attribute_forbidding =
  | Never               (** the [@specialize never] attribute *)
(** The attribute that can prevent specialization of a function at
    call site. *)

type specialized =

  | Forced_by_attribute of attribute_forcing
  (** The specified attribute forces specialization. *)

  | Without_subfunctions of benefit
  (** Regular case of a function with a benefit computation that is enough
      to justify inlining. *)

  | With_subfunctions of benefit * benefit
  (** TODO: understand what differs with the previous case and what the first
      benefit refers to. *)


type not_specialized =

  | Classic_mode
  (** In `-OClassic` mode, specialization never happens because it never
      happens in Closure. *)

  | Never_specialize_attribute
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

  | Not_beneficial of benefit * benefit
  (** Benefit computation concluded there wasn't enough benefit to
      specialize.
      TODO: understand what the two benefits represent. *)

  | Max_inlining_depth_exceeded
  (** The maximum inlining depth has been exceeded.
      This maximum depth is usually set by command line options
      of the compiler invocation, typically by one of the `-O`
      options. Exceeding this depth prevents any inlining or
      specialization. *)

