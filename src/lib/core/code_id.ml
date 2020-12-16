
(** Code ids

    These ids refer to a pice of code (hence the code_id), that is used
    as the body for a (potentially unnamed) function (and thus transitively
    by closures).
    This is equivalent to:
    - {Code_id.t} in flambda2
    - {Closure_id.t} in flambda1
*)

(* Type definitions *)
(* ************************************************************************* *)

type t = {
  name : string;
  compilation_unit : Compilation_unit.t;
}


(* Printing *)
(* ************************************************************************* *)

let print fmt { compilation_unit; name; } =
  Format.fprintf fmt "%a.%s" Compilation_unit.print compilation_unit name

let print_ctx ~ctx fmt ({ compilation_unit; name; } as t) =
  if Compilation_unit.equal ctx compilation_unit
  then Format.fprintf fmt "%s" name
  else print fmt t

