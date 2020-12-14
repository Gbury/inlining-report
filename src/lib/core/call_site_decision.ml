
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
(** The various attributes attached to a function call that can force
    its inlining. *)

type decision =

  (* Flambda1 *)

  (* TODO *)


  (* Flambda2 *)

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

  | Never_inline_attribute
  (** An unspecified attribute has prevented inlining.
      TODO: add some specification of which attribute ? since this is done for
            attributes that force inlining, why not for attributes preventing
            it ? *)

  | Inline_because_of_declaration
  (** Inlined because this was the decision at the declaration of the function. *)

  | Inline_attribute of attribute_causing_inlining
  (** The specified attribute forces inlining of the call. *)

(** The various decision related to inlining of a function call, or rather, and
    more importantly the reason for the decision to inline or not the call. *)

type t =
  | Unknown_function
  (** unknown function cannot be inline (not enough information). *)
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
(** The various situations that can be encountered at a function call. *)


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

