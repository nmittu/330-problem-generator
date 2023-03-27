open! Core
open Result.Let_syntax
open Type_intf

type token =
  | IntT
  | FloatT
  | StringT
  | BoolT
  | ListT
  | StarT
  | ArrowT
  | PolyT of string
  | LParen
  | RParen
  | EOF

let int_p = Str.regexp {|int|}
let float_p = Str.regexp {|float|}
let string_p = Str.regexp {|string|}
let bool_p = Str.regexp {|bool|}
let list_p = Str.regexp {|list|}
let star_p = Str.regexp {|\*|}
let arrow_p = Str.regexp {|->|}
let poly_p = Str.regexp {|'[a-z]+|}
let lparen_p = Str.regexp {|(|}
let rparen_p = Str.regexp {|)|}
let whitespace_p = Str.regexp {|[\n\r\t ]+|}

let tokenize str =
  let rec tokenize pos =
    if pos >= String.length str
    then Ok [ EOF ]
    else if Str.string_match int_p str pos
    then (
      let match_len = Str.matched_string str |> String.length in
      let%map toks = tokenize (pos + match_len) in
      IntT :: toks)
    else if Str.string_match float_p str pos
    then (
      let match_len = Str.matched_string str |> String.length in
      let%map toks = tokenize (pos + match_len) in
      FloatT :: toks)
    else if Str.string_match string_p str pos
    then (
      let match_len = Str.matched_string str |> String.length in
      let%map toks = tokenize (pos + match_len) in
      StringT :: toks)
    else if Str.string_match bool_p str pos
    then (
      let match_len = Str.matched_string str |> String.length in
      let%map toks = tokenize (pos + match_len) in
      BoolT :: toks)
    else if Str.string_match list_p str pos
    then (
      let match_len = Str.matched_string str |> String.length in
      let%map toks = tokenize (pos + match_len) in
      ListT :: toks)
    else if Str.string_match star_p str pos
    then (
      let match_len = Str.matched_string str |> String.length in
      let%map toks = tokenize (pos + match_len) in
      StarT :: toks)
    else if Str.string_match arrow_p str pos
    then (
      let match_len = Str.matched_string str |> String.length in
      let%map toks = tokenize (pos + match_len) in
      ArrowT :: toks)
    else if Str.string_match poly_p str pos
    then (
      let matched_str = Str.matched_string str in
      let match_len = matched_str |> String.length in
      let%map toks = tokenize (pos + match_len) in
      let name = String.sub matched_str ~pos:1 ~len:(match_len - 1) in
      PolyT name :: toks)
    else if Str.string_match lparen_p str pos
    then (
      let match_len = Str.matched_string str |> String.length in
      let%map toks = tokenize (pos + match_len) in
      LParen :: toks)
    else if Str.string_match rparen_p str pos
    then (
      let match_len = Str.matched_string str |> String.length in
      let%map toks = tokenize (pos + match_len) in
      RParen :: toks)
    else if Str.string_match whitespace_p str pos
    then (
      let match_len = Str.matched_string str |> String.length in
      tokenize (pos + match_len))
    else Error "Could not tolkenize type"
  in
  tokenize 0
;;

let lookahead toks =
  match toks with
  | h :: _ -> Ok h
  | [] -> Error "Unexpected end of string"
;;

let tok_to_s = function
  | IntT -> "int"
  | FloatT -> "float"
  | StringT -> "string"
  | BoolT -> "bool"
  | ListT -> "list"
  | StarT -> "*"
  | ArrowT -> "->"
  | PolyT s -> Printf.sprintf "'%s" s
  | LParen -> "("
  | RParen -> ")"
  | EOF -> "EOF"
;;

let match_tok toks t =
  match toks with
  | h :: tl when Poly.(h = t) -> Ok tl
  | h :: _ ->
    Error (failwith @@ Printf.sprintf "Expected %s but got %s" (tok_to_s t) (tok_to_s h))
  | [] ->
    Error (failwith @@ Printf.sprintf "Expected %s but got end of string" (tok_to_s t))
;;

let rec parse str =
  let%bind toks = tokenize str in
  let%bind toks, typ = parse_func toks in
  if Poly.(toks <> [ EOF ])
  then Error (Printf.sprintf "Did not reach EOF: %s" (tok_to_s @@ List.hd_exn toks))
  else Ok typ

and parse_func toks =
  let%bind toks, typ = parse_tuple toks in
  match%bind lookahead toks with
  | ArrowT ->
    let%bind toks = match_tok toks ArrowT in
    let%bind toks, ret = parse_func toks in
    return (toks, Func (typ, ret))
  | _ -> return (toks, typ)

and parse_tuple toks =
  let%bind toks, typ = parse_list toks in
  match%bind lookahead toks with
  | StarT ->
    let%bind toks = match_tok toks StarT in
    let%bind toks, typ' = parse_tuple toks in
    return (toks, Tuple (typ, typ'))
  | _ -> return (toks, typ)

and parse_list toks =
  let%bind toks, typ = parse_base toks in
  let rec consume_lists t toks =
    match%bind lookahead toks with
    | ListT ->
      let%bind toks = match_tok toks ListT in
      let%bind toks, t = consume_lists (List t) toks in
      return (toks, t)
    | _ -> return (toks, t)
  in
  consume_lists typ toks

and parse_base toks =
  match%bind lookahead toks with
  | IntT ->
    let%bind toks = match_tok toks IntT in
    return (toks, Int)
  | StringT ->
    let%bind toks = match_tok toks StringT in
    return (toks, String)
  | FloatT ->
    let%bind toks = match_tok toks FloatT in
    return (toks, Float)
  | BoolT ->
    let%bind toks = match_tok toks BoolT in
    return (toks, Bool)
  | PolyT s ->
    let%bind toks = match_tok toks (PolyT s) in
    return (toks, Polymorphic s)
  | _ ->
    let%bind toks = match_tok toks LParen in
    let%bind toks, typ = parse_func toks in
    let%bind toks = match_tok toks RParen in
    return (toks, typ)
;;
