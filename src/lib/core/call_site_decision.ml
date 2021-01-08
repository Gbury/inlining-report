
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


type attribute_causing_inlining =
  | Always          (** the [@inlined] attribute *)
  | Unroll of int   (** the [@unrolled] attribute; the int payload indicates
                        how much unrolling still needs to be performed. *)
  | Unknown         (** the attribute was not propagated (e.g. flambda1
                        does not propagate such attributes) *)
(** The various attributes attached to a function call that can force
    its inlining. *)


type inlined =

  | Inline_because_of_decision_at_declaration
  (** Inlined because this was the decision at the declaration of the function.
      This is the main reason for inlining in `-OClassic` mode where flambda
      emulates the behaviour of Closure. *)

  | Inline_attribute of attribute_causing_inlining
  (** The specified attribute forces inlining of the call. *)

  | Declaration_local_to_application
  (** From flambda1: the function was local.
      TODO: get more details about that. *)

  | Without_subfunctions of benefit
  (** Regular case of a function with a benefit computation that is enough
      to justify inlining. *)

  | With_subfunctions of benefit * benefit
  (** TODO: understand what differs with the previous case and what the first
      benefit refers to. *)


type not_inlined =

  | Do_not_inline_because_of_decision_at_declaration
  (** Not-inlined becasue this was the decision at the declaration of the
      function. This is the main reason for not inlining in `-OClassic` mode
      where flambda emulates the behaviour of Closure. *)

  | Never_inline_attribute
  (** An unspecified attribute has prevented inlining.
      TODO: add some specification of which attribute ? since this is done for
            attributes that force inlining, why not for attributes preventing
            it ? *)

  | Above_threshold of int option
  (** From flambda1: there is not enough remaining inlining "fuel" to
      inline the function, i.e. the maximum inlining threshold has been
      reached, thus the function wasn't inlined. *)

  | Self_call
  (** This was a self (i.e. recursive) call, hence there was no use in
      inlining the call. *)

  | No_useful_approximation
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

  | Without_subfunctions of benefit
  (** Regular case of a function with a benefit computation that is enough
      to justify inlining. *)

  | With_subfunctions of benefit * benefit
  (** TODO: understand what differs with the previous case and what the first
      benefit refers to. *)


(** The various decision related to inlining of a function call, or rather, and
    more importantly the reason for the decision to inline or not the call. *)

type decision =
  | Specialized of specialized
  | Inlined of not_specialized * inlined
  | Nothing of not_specialized * not_inlined

type t =
  | Unknown_function
  (** nothing can be done for unknown functions *)
  | Known_function of {
      code_id : Code_id.t;
      decision : decision;
    }
  (** case for known functions. *)
(** The various situations that can be encountered at a function call. *)


(*

  | Non_inlinable_function of {
      code_id : Code_id.t;
    }
  (** Function call where some information about the function was known,
      but an earlier decision (usually the decision at declaration time),
      prevented the call from being inlined.
      Typically, currently in flambda2 (which tries and imitate closure),
      functions deemed not inlinable at their declaration do not carry
      enough information in their type/approximation to make a decision. *)
  | Inlinable_function of {
      code_id : Code_id.t;
      decision : decision;
    }
  (** There was enough information about the function to make a decision. *)
*)


(* Printing *)
(* ************************************************************************* *)

let reason_says_inline = function
  | Environment_says_never_inline
  | Unrolling_depth_exceeded
  | Max_inlining_depth_exceeded
  | Recursion_depth_exceeded
  | Never_inline_attribute -> false
  | Inline_because_of_declaration
  | Inline_attribute _ -> true

let was_inlined = function
  | Unknown_function
  | Non_inlinable_function _ -> false
  | Inlinable_function { decision; _ } -> reason_says_inline decision


(* Printing *)
(* ************************************************************************* *)

let print_decision_reason fmt = function
  | Environment_says_never_inline ->
    Format.fprintf fmt "the@ environment@ says@ never@ to@ inline"
  | Unrolling_depth_exceeded ->
    Format.fprintf fmt "the@ maximum@ unrolling@ depth@ has@ been@ exceeded"
  | Max_inlining_depth_exceeded ->
    Format.fprintf fmt "the@ maximum@ inlining@ depth@ has@ been@ exceeded"
  | Recursion_depth_exceeded ->
    Format.fprintf fmt "the@ maximum@ recursion@ depth@ has@ been@ exceeded"
  | Never_inline_attribute ->
    Format.fprintf fmt "the@ call@ has@ an@ attribute@ forbidding@ inlining"
  | Inline_because_of_declaration ->
    Format.fprintf fmt "the@ function@ was@ deemed@ inlinable@ from@ its@ declaration"
  | Inline_attribute Always ->
    Format.fprintf fmt "the@ call@ has@ an@ [@@inline always]@ attribute"
  | Inline_attribute (Unroll n) ->
    Format.fprintf fmt "the@ call@ has@ an@ [@@unroll %d]@ attribute" n

let print_decision fmt t =
    Format.fprintf fmt "@[<v>The function call %s been inlined@ because @[<hov>%a@]@]"
      (if reason_says_inline t then "has" else "has not") print_decision_reason t

