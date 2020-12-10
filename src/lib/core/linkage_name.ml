
(** Linkage names

    Names used in the emitted assembly. Each of these must refer to
    a unique element (or the linker would likely complain, assuming
    the same name is defined twice).
*)

type t = string


