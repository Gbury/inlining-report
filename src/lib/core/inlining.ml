
(** Inlining decisions

*)

(* Type definitions *)
(* ************************************************************************* *)

type attribute_forcing =
  | Always          (** the [@inlined] attribute *)
  | Unroll of int   (** the [@unrolled] attribute; the int payload indicates
                        how much unrolling still needs to be performed. *)
  | Unknown         (** the attribute was not propagated (e.g. flambda1
                        does not propagate such attributes) *)
(** The various attributes attached to a function call that can force
    its inlining. *)

type attribute_forbidding =
  | Never           (** the [@specialized never] attribute *)
  | Unknown         (** the exact attribute was not propagated *)
(** The various attributes attached to a function call that can prevent
    its inlining. *)


type inlined =

  | Forced_by_decision_at_declaration
  (** Inlined because this was the decision at the declaration of the function.
      This is the main reason for inlining in `-OClassic` mode where flambda
      emulates the behaviour of Closure. *)

  | Forced_by_attribute of attribute_forcing
  (** The specified attribute forces inlining of the call. *)

  | Declaration_local_to_application
  (** From flambda1: the function was local.
      TODO: get more details about that. *)

  | Without_subfunctions of Whether_sufficient_benefit.t
  (** Regular case of a function with a benefit computation that is enough
      to justify inlining. *)

  | With_subfunctions of
      Whether_sufficient_benefit.t *
      Whether_sufficient_benefit.t
  (** TODO: understand what differs with the previous case and what the first
      benefit refers to. *)
(** The reason for why a funciton call has been inlined. *)


type not_inlined =
  | Forbidden_by_decision_at_declaration
  (** Not-inlined becasue this was the decision at the declaration of the
      function. This is the main reason for not inlining in `-OClassic` mode
      where flambda emulates the behaviour of Closure. *)

  | Forbidden_by_attribute of attribute_forbidding
  (** An unspecified attribute has prevented inlining. *)

  | Above_threshold of int option
  (** From flambda1: there is not enough remaining inlining "fuel" to
      inline the function, i.e. the maximum inlining threshold has been
      reached, thus the function wasn't inlined. *)

  | Self_call
  (** This was a self (i.e. recursive) call, hence there was no use in
      inlining the call. *)

  | No_useful_approximations
  (** In this case, there is no interesting approximation for the arguments;
      which means that inlining cannot discover simplifications (e.g. no
      constant propagation or other simplifications). Additionally, the body
      of the function is not particularly small, so the cost of the call/jump
      to the function body should be small enough (compared to the cost of
      executing the function body) that it is not interesting to inline. *)

  | Environment_says_never_inline
  (** The environment has prevented inlining.
      TODO: ask @mshinwell more about this. *)

  | Unrolling_depth_exceeded
  (** The maximum unrolling depth has been exceeded.
      This maximum depth is usually set by command line options
      of the compiler invocation, typically by one of the `-O`
      options. *)

  | Max_inlining_depth_exceeded
  (** The maximum inlining depth has been exceeded.
      This maximum depth is usually set by command line options
      of the compiler invocation, typically by one of the `-O`
      options. *)

  | Recursion_depth_exceeded
  (** The maximum recursion depth has been exceeded. *)

  | Without_subfunctions of
      Whether_sufficient_benefit.t
  (** Regular case of a function with a benefit computation that is enough
      to justify inlining. *)

  | With_subfunctions of
      Whether_sufficient_benefit.t *
      Whether_sufficient_benefit.t
  (** TODO: understand what differs with the previous case and what the first
      benefit refers to. *)
(** The reason why a funciton call was not inlined. *)

type t =
  | Inlined of inlined
  | Not_inlined of not_inlined


(* Printing explanations *)
(* ************************************************************************* *)

let print_threshold_opt fmt = function
  | None -> ()
  | Some i -> Format.fprintf fmt " (%d)" i

let explain_why fmt = function
  | Forced_by_decision_at_declaration ->
    Format.pp_print_text fmt "the function was deemed inlinable from its declaration"
  | Forced_by_attribute Always ->
    Format.pp_print_text fmt "the call has an [@@inline always] attribute"
  | Forced_by_attribute (Unroll n) ->
    Format.fprintf fmt "the@ call@ has@ an@ [@@unroll %d]@ attribute" n
  | Forced_by_attribute Unknown ->
    Format.pp_print_text fmt "of an annotation"
  | Declaration_local_to_application ->
    Format.pp_print_text fmt "the function declaration was local to the application"
  | Without_subfunctions _
  | With_subfunctions _ ->
    Format.pp_print_text fmt "the expected benefit outweighed the change in code size"

let explain_why_not fmt = function
  | Forbidden_by_decision_at_declaration ->
    Format.pp_print_text fmt "it was forbidden by the decision at the function's declaration"
  | Forbidden_by_attribute Never ->
    Format.fprintf fmt "the@ call@ has@ an@ attribute@ forbidding@ inlining"
  | Forbidden_by_attribute Unknown ->
    Format.fprintf fmt "of an annotation"
  | Above_threshold opt ->
    Format.fprintf fmt "it is larger than the remaining size threshold%a"
      print_threshold_opt opt
  | Self_call ->
    Format.fprintf fmt "it was a self call"
  | No_useful_approximations ->
    Format.pp_print_text fmt "there was no useful approximation about any of \
                              its parameters, and it was not particularly small"
  | Environment_says_never_inline ->
    Format.fprintf fmt "the@ environment@ says@ never@ to@ inline"
  | Unrolling_depth_exceeded ->
    Format.fprintf fmt "the@ maximum@ unrolling@ depth@ has@ been@ exceeded"
  | Max_inlining_depth_exceeded ->
    Format.fprintf fmt "the@ maximum@ inlining@ depth@ has@ been@ exceeded"
  | Recursion_depth_exceeded ->
    Format.fprintf fmt "the@ maximum@ recursion@ depth@ has@ been@ exceeded"
  | Without_subfunctions _
  | With_subfunctions _ ->
    Format.pp_print_text fmt "the expected benefit did not outweigh the change in code size"

let summary fmt = function
  | Inlined inlined ->
    Format.fprintf fmt "The@ call@ was@ inlined@ because@ %a" explain_why inlined
  | Not_inlined not_inlined ->
    Format.fprintf fmt "The@ call@ was@ not@ inlined@ because@ %a" explain_why_not not_inlined


