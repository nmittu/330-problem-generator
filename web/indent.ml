open! Core
open Js_of_ocaml

let indent s in_lines =
  let output =
    { IndentPrinter.debug = false
    ; config = IndentConfig.default
    ; in_lines
    ; indent_empty = true
    ; adaptive = true
    ; kind = IndentPrinter.Print (fun s acc -> acc ^ s)
    }
  in
  let stream = Nstream.of_string s in
  IndentPrinter.proceed output stream IndentBlock.empty ""
;;

let textarea (textbox : Dom_html.textAreaElement Js.t) : string =
  let rec loop s acc (i, pos') =
    try
      let pos = String.index_from_exn s pos' '\n' in
      loop s ((i, (pos', pos)) :: acc) (succ i, succ pos)
    with
    | _ -> List.rev ((i, (pos', String.length s)) :: acc)
  in
  let rec find (l : (int * (int * int)) list) c =
    match l with
    | [] -> assert false
    | (i, (lo, up)) :: _ when up >= c -> c, i, lo, up
    | (_, (_lo, _up)) :: rem -> find rem c
  in
  let v = textbox##.value in
  let pos =
    let c1 = textbox##.selectionStart
    and c2 = textbox##.selectionEnd in
    if Js.Opt.test (Js.Opt.return c1) && Js.Opt.test (Js.Opt.return c2)
    then (
      let l = loop (Js.to_string v) [] (0, 0) in
      Some (find l c1, find l c2))
    else None
  in
  let f =
    match pos with
    | None -> fun _ -> true
    | Some ((_c1, line1, _lo1, _up1), (_c2, line2, _lo2, _up2)) ->
      fun l -> l >= line1 + 1 && l <= line2 + 1
  in
  let v = indent (Js.to_string v) f in
  textbox##.value := Js.string v;
  match pos with
  | Some ((c1, line1, _lo1, up1), (c2, line2, _lo2, up2)) ->
    let l = loop v [] (0, 0) in
    let lo1'', up1'' = ListLabels.assoc line1 l in
    let lo2'', up2'' = ListLabels.assoc line2 l in
    let n1 = max (c1 + up1'' - up1) lo1'' in
    let n2 = max (c2 + up2'' - up2) lo2'' in
    let () = (Obj.magic textbox)##setSelectionRange n1 n2 in
    textbox##focus;
    v
  | None -> v
;;
