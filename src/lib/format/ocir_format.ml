
(* Versions for supported formats *)

module Version = struct

  type flambda1 =
    | V1_0_0

  type flambda2 =
    | V1_0_0

  type t =
    | Flambda1 of flambda1
    | Flambda2 of flambda2

end

(* All format versions supported *)

type t = [
  | Ocir_format_flambda1_1_0_0.Report.t
  | Ocir_format_flambda2_1_0_0.Report.t
] [@@deriving yojson]

let version t : Version.t =
  match (t : t) with
  | #Ocir_format_flambda1_1_0_0.Report.t -> Flambda1 V1_0_0
  | #Ocir_format_flambda2_1_0_0.Report.t -> Flambda2 V1_0_0

let conv = function
  | #Ocir_format_flambda1_1_0_0.Report.t as t ->
    Ocir_format_flambda1_1_0_0.Report.conv t
  | #Ocir_format_flambda2_1_0_0.Report.t as t ->
    Ocir_format_flambda2_1_0_0.Report.conv t

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

