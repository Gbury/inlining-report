
(** Alias to provide a uniform interface across inlining report formats *)

type t = [ | Inlining_report.report ][@@deriving yojson]

let conv = Inlining_report.conv
