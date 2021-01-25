
let () =
  let () = Fmt.set_style_renderer Format.std_formatter `Ansi_tty in
  match Sys.argv with
  | [| _ ; file |] ->
    let t = Ocir_format.read_file file in
    Format.printf "%a@." Ocir_core.Report.print t
  | [| _ ; left ; right |] ->
    let left = Ocir_format.read_file left in
    let right = Ocir_format.read_file right in
    Format.printf "%a@." Ocir_core.Diff.report (left, right)
  | _ ->
    Format.eprintf "@[<v 2>usage:@;\
                        ocir file\t\t\t%s@;\
                        ocir left_file right_file\t%s@]@."
      "print a human readable version of a serialized inlining report"
      "print a comparison of the two serialized inlining reports";
    exit 1


