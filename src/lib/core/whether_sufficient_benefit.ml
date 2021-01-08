
(** Code simplification analysis results

    Named Whether_sufficient_benefit in flambda1 (in inlining_cost.ml),
    this representes the result of a benefit analysis on a piece of original
    code and a simplified version of it.
*)

(* Type definitions *)
(* ************************************************************************* *)

type t = {

  (* Changes cause by the simplification (inlining/specialization) *)
  benefit : Benefit.t;

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

