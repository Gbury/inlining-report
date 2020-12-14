
let () =
  let file = Sys.argv.(1) in
  let t = Ocir_format.read_file file in
  Format.printf "%a@." Ocir_core.Report.print t

