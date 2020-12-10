
(* All format versions supported *)

type t = [
  | Inlining_report_format_flambda2_1_0_0.Report.t
] [@@deriving yojson]

let conv = function
  | #Inlining_report_format_flambda2_1_0_0.Report.t as t ->
    Inlining_report_format_flambda2_1_0_0.Report.conv t

(* marshall format parsing *)

let read_marshalled_file file =
  let ch = open_in_bin file in
  let t : t = Marshal.from_channel ch in
  conv t

(* json format parsing *)

let read_json_file file =
  let json = Yojson.Safe.from_file file in
  let t = t_of_yojson json in
  conv t

(* parse a file *)

let read_file file =
  match Filename.extension file with
  | ".json" -> read_json_file file
  | _ -> read_marshalled_file file

