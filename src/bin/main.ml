
let () =
  let () = Fmt.set_style_renderer Format.std_formatter `Ansi_tty in
  let file = Sys.argv.(1) in
  let t = Ocir_format.read_file file in
  Format.printf "%a@." Ocir_core.Report.print t

