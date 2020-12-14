
(** Inlining threshold

    Threshold used for code size, and determining
    whether or not to inline a function based on the
    size of its body. The size is a simple integer
    with no real unit or formal specification, it's
    simply a mix of estimate between actual size of
    the assembly generated, and speed of execution. *)

(* Type definitions *)
(* ************************************************************************* *)

type t =
  | Never_inline
  | Can_inline_if_no_larger_than of int (**)
(** Threshold for inlining decisions. *)

(* Printing *)
(* ************************************************************************* *)

let print fmt = function
  | Never_inline ->
    Format.fprintf fmt "never inline"
  | Can_inline_if_no_larger_than i ->
    Format.fprintf fmt "can inline if size <= %d" i

