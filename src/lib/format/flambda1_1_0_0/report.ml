
(** Alias to provide a uniform interface across inlining report formats *)

type t = [ | Inlining_stats.Inlining_report.report ][@@deriving yojson]


(* Conversion Function *)
(* ******************* *)

let conv (`Flambda1_1_0_0 (_meta, _l) : t) : Ocir_core.Report.t =
  assert false


