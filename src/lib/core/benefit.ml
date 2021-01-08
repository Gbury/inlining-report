
(** Benefits

    Kind of a misnormer, but this represents the change in terms of relevant
    characteristics between two pieces of code, typically an original code
    and a simplified (i.e. inlined or specialized) version of it.

*)

(* Type definitions *)
(* ************************************************************************* *)

type t = {
  remove_call : int;
  remove_alloc : int;
  remove_prim : int;
  remove_branch : int;
  direct_call_of_indirect : int;
  requested_inline : int;
}

