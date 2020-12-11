
let () =
  let file = Sys.argv.(1) in
  let _t = Ocir_format.read_file file in
  Format.printf "ok@."

