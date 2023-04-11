open! Core
open Type
open Result.Let_syntax

type token =
  | Tok_Lam
  | Tok_Id of string
  | Tok_Dot
  | Tok_LParen
  | Tok_RParen

let reg_lam = Str.regexp {|λ\|L|}
let reg_id = Str.regexp {|[a-z]+|}
let reg_dot = Str.regexp {|\.|}
let reg_lparen = Str.regexp {|(|}
let reg_rparen = Str.regexp {|)|}
let whitespace_p = Str.regexp {|[\n\r\t ]+|}

let tokenize str =
  let rec tokenize pos =
    if pos >= String.length str
    then Ok []
    else if Str.string_match reg_lam str pos
    then (
      let match_len = Str.matched_string str |> String.length in
      let%map toks = tokenize (pos + match_len) in
      Tok_Lam :: toks)
    else if Str.string_match reg_id str pos
    then (
      let matched_str = Str.matched_string str in
      let match_len = matched_str |> String.length in
      let%map toks = tokenize (pos + match_len) in
      Tok_Id matched_str :: toks)
    else if Str.string_match reg_dot str pos
    then (
      let match_len = Str.matched_string str |> String.length in
      let%map toks = tokenize (pos + match_len) in
      Tok_Dot :: toks)
    else if Str.string_match reg_lparen str pos
    then (
      let match_len = Str.matched_string str |> String.length in
      let%map toks = tokenize (pos + match_len) in
      Tok_LParen :: toks)
    else if Str.string_match reg_rparen str pos
    then (
      let match_len = Str.matched_string str |> String.length in
      let%map toks = tokenize (pos + match_len) in
      Tok_RParen :: toks)
    else if Str.string_match whitespace_p str pos
    then (
      let match_len = Str.matched_string str |> String.length in
      tokenize (pos + match_len))
    else Error "Could not tokenize lambda expression"
  in
  tokenize 0
;;

let lookahead = List.hd

let tok_to_s = function
  | Tok_Lam -> "lambda"
  | Tok_Id id -> id
  | Tok_Dot -> "."
  | Tok_LParen -> "("
  | Tok_RParen -> ")"
;;

let match_tok toks t =
  match toks with
  | h :: tl when Poly.(h = t) -> Ok tl
  | h :: _ -> Error (Printf.sprintf "Expected %s but got %s" (tok_to_s t) (tok_to_s h))
  | [] -> Error (Printf.sprintf "Expected %s but got end of string" (tok_to_s t))
;;

let rec parse str =
  let%bind toks = tokenize str in
  let%bind toks, e = parse_expr toks in
  if Poly.(toks = [])
  then return e
  else Error (Printf.sprintf "Did not reach EOF: %s" (tok_to_s (List.hd_exn toks)))

and parse_expr toks = parse_app toks

and parse_app toks =
  let%bind toks, p = parse_lam toks in
  let rec consume_apps e toks =
    match lookahead toks with
    | Some (_ as t) when Poly.(t <> Tok_RParen) ->
      let%bind toks, e' = parse_lam toks in
      let%bind toks, e = consume_apps (App (e, e')) toks in
      return (toks, e)
    | _ -> return (toks, e)
  in
  consume_apps p toks

and parse_lam toks =
  match lookahead toks with
  | Some Tok_Lam ->
    let%bind toks = match_tok toks Tok_Lam in
    let%bind toks, id = parse_id toks in
    let%bind toks = match_tok toks Tok_Dot in
    let%bind toks, e = parse_expr toks in
    return (toks, Lam (id, e))
  | _ -> parse_primary toks

and parse_primary toks =
  match lookahead toks with
  | Some (Tok_Id id) ->
    let%map toks = match_tok toks (Tok_Id id) in
    toks, Var id
  | Some Tok_LParen ->
    let%bind toks = match_tok toks Tok_LParen in
    let%bind toks, e = parse_expr toks in
    let%bind toks = match_tok toks Tok_RParen in
    return (toks, e)
  | Some t -> Error (Printf.sprintf "Unexpected token: %s" (tok_to_s t))
  | None -> Error "Unexpected EOF"

and parse_id toks =
  match lookahead toks with
  | Some (Tok_Id id) ->
    let%map toks = match_tok toks (Tok_Id id) in
    toks, id
  | Some t -> Error (Printf.sprintf "Unexpected token: %s" (tok_to_s t))
  | None -> Error "Unexpected EOF"
;;
