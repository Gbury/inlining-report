
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
  stamp : int option;
  compilation_unit : Compilation_unit.t;
}

(* Printing *)
(* ************************************************************************* *)

let print_stamp fmt = function
  | None -> ()
  | Some i ->
    let aux fmt i = Format.fprintf fmt "/%d" i in
    Fmt.styled (`Fg (`Hi `Black)) aux fmt i

let print fmt { compilation_unit; name; stamp } =
  Format.fprintf fmt "%a.%s%a"
    Compilation_unit.print compilation_unit name print_stamp stamp

let print_short fmt { compilation_unit = _; name; stamp; } =
  Format.fprintf fmt "%s%a" name print_stamp stamp

let print_ctx ~ctx fmt ({ compilation_unit; _ } as t) =
  if Compilation_unit.equal ctx compilation_unit
  then print_short fmt t
  else print fmt t

