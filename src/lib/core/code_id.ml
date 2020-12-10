
(** Code ids

    These ids refer to a pice of code (hence the code_id), that is used
    as the body for a (potentially unnamed) function (and thus transitively
    by closures).
    This is equivalent to:
    - {Code_id.t} in flambda2
    - {Closure_id.t} in flambda1
*)

type t = {
  name : string;
  compilation_unit : Compilation_unit.t;
}
