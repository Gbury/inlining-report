
let () =
  let file = Sys.argv.(1) in
  let _t = Inlining_report_format.read_file file in
  Format.printf "ok@."

