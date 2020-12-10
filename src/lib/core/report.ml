
(** Inlining reports

    A report is basically a map of decisions. The
    decisions about function calls will be about
    inlining of top-level code (i.e. module
    initialization code), whereas the decisions about
    function declarations will be about the functions
    defined at toplevel.
*)

type t = {
  decisions: Decision.map;
}
(** An inlining report. *)
(* CR gbury: add some meta-data to the report (such as
   input files, compilation options, etc...) ? *)
