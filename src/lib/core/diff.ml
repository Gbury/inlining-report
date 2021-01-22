
(** Difference between inlining reports

    This represents the difference between two inlining reports.
    Currently we will stop at the first difference.

*)


(* Utility traversal functions *)
(* ************************************************************************* *)

let map_iter2 ~f map1 map2 =
  let rec aux seq1 seq2 =
    match (seq1 : _ Seq.node), (seq2 : _ Seq.node) with
    | Nil, Nil -> ()
    | Nil, Cons ((dbg, v), seq') ->
      f dbg None (Some v);
      aux seq1 (seq' ())
    | Cons ((dbg, v), seq'), Nil ->
      f dbg (Some v) None;
      aux (seq' ()) seq2
    | Cons ((dbg1, v1), seq1'),
      Cons ((dbg2, v2), seq2') ->
      let c = Debuginfo.compare dbg1 dbg2 in
      if c = 0 then begin
        f dbg1 (Some v1) (Some v2);
        aux (seq1' ()) (seq2' ())
      end else if c < 0 then begin
        f dbg1 (Some v1) None;
        aux (seq1' ()) seq2
      end else (* c > 0 *) begin
        f dbg2 None (Some v2);
        aux seq1 (seq2' ())
      end
  in
  let seq1 = Debuginfo.Map.to_seq map1 in
  let seq2 = Debuginfo.Map.to_seq map2 in
  aux (seq1 ()) (seq2 ())

(* Diff traversal *)
(* ************************************************************************* *)

let rec map acc left right =
  map_iter left right ~f:(fun dbg l r ->
      match l, r with
      | 

