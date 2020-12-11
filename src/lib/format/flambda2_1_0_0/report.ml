
(** Compatibility layer over reports as lists of decisions.
    This helps immensely with versioning of the report formats. *)

type t = [ `Flambda2_1_0_0 of Inlining_report.t list ] [@@deriving yojson]

let conv (_ : t) : Ocir_core.Report.t = assert false

