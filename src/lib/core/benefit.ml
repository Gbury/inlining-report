
(** Benefits

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

type result = {

  (* Raw Benefit of the simplification (inlining/specialization) *)
  benefit : t;

  (* Context of the simplification *)
  round : int;
  toplevel : bool;
  branch_depth : int;
  lifting : bool;

  (* Size before and after simplification *)
  original_size : int;
  new_size : int;

  (* Cached results of the benefit vs. code size comparison *)
  evaluated_benefit : int;
  estimate : bool;

}

