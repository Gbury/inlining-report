
(** Alias to provide a uniform interface across inlining report formats *)

type t = [ | Inlining_stats.Inlining_report.report ][@@deriving yojson]


(* Conversion Function *)
(* ******************* *)

let conv = Inlining_stats.Inlining_report.conv


