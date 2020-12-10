
(** Debuginfos

    Debuginfos are stacks of locations. Most debuginfos only need
    one location, but the stack part it used most notably when inlining
    as that allows to stack the location of the inlined call, with the
    location inside the inlined code.
*)

include Ocaml_common.Debuginfo

module Map = Map.Make(struct
    type nonrec t = t
    let compare = compare
end)
