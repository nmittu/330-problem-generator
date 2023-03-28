open! Core

type func

type _ expr =
  | Var : 'a expr
  | List : 'a expr list -> 'a list expr
  | Cons : 'a expr * 'a list expr -> 'a list expr
  | Tuple : 'a expr * 'b expr -> ('a * 'b) expr
  | App : func expr * 'a expr list -> 'a expr
  | Eq : 'a expr * 'a expr -> bool expr
  | Gt : 'a expr * 'a expr -> bool expr
  | Lt : 'a expr * 'a expr -> bool expr
  | Or : bool expr * bool expr -> bool expr
  | And : bool expr * bool expr -> bool expr
  | Not : bool expr -> bool expr
  | If : bool expr * 'a expr * 'a expr -> 'a expr

type polyVal

type _ typ =
  | AT : polyVal typ
  | ListT : 'a typ -> 'a list typ
  | TupleT : 'a typ * 'b typ -> ('a * 'b) typ
  | FuncT : func typ
  | BoolT : bool typ

let rec generate : 'a. 'a typ -> int -> 'a expr =
  fun (type a) (typ : a typ) depth : a expr ->
   if depth = 0
   then Var
   else (
     match typ with
     | AT ->
       (match Random.int 2 with
        | 0 ->
          let len = Random.int 2 + 1 in
          App
            ( generate FuncT (depth - 1)
            , List.init len ~f:(fun _ -> generate AT (depth - 1)) )
        | _ ->
          If (generate BoolT (depth - 1), generate AT (depth - 1), generate AT (depth - 1)))
     | ListT a ->
       (match Random.int 3 with
        | 0 -> Cons (generate a (depth - 1), generate (ListT a) (depth - 1))
        | 1 ->
          let len = Random.int 3 in
          List (List.init len ~f:(fun _ -> generate a (depth - 1)))
        | _ ->
          If
            ( generate BoolT (depth - 1)
            , generate (ListT a) (depth - 1)
            , generate (ListT a) (depth - 1) ))
     | TupleT (a, b) ->
       (match Random.int 2 with
        | 0 -> Tuple (generate a (depth - 1), generate b (depth - 1))
        | _ ->
          If
            ( generate BoolT (depth - 1)
            , generate (TupleT (a, b)) (depth - 1)
            , generate (TupleT (a, b)) (depth - 1) ))
     | FuncT ->
       (match Random.int 2 with
        | 0 -> Var
        | _ ->
          If
            ( generate BoolT (depth - 1)
            , generate FuncT (depth - 1)
            , generate FuncT (depth - 1) ))
     | BoolT ->
       (match Random.int 7 with
        | x when x < 3 -> generate_poly_compare (depth - 1)
        | 3 -> Or (generate BoolT (depth - 1), generate BoolT (depth - 1))
        | 4 -> And (generate BoolT (depth - 1), generate BoolT (depth - 1))
        | 5 -> Not (generate BoolT (depth - 1))
        | _ ->
          If
            ( generate BoolT (depth - 1)
            , generate BoolT (depth - 1)
            , generate BoolT (depth - 1) )))

and generate_poly_compare depth : bool expr =
  let depth = max depth 1 in
  let combiner : 'a. 'a expr -> 'a expr -> bool expr =
   fun l r ->
    match Random.int 3 with
    | 0 -> Eq (l, r)
    | 1 -> Lt (l, r)
    | _ -> Gt (l, r)
  in
  match Random.int 3 with
  | 0 ->
    let l, r = generate AT (depth - 1), generate AT (depth - 1) in
    combiner l r
  | 1 ->
    let l, r = generate (ListT AT) (depth - 1), generate (ListT AT) (depth - 1) in
    combiner l r
  | _ ->
    let l, r =
      generate (TupleT (AT, AT)) (depth - 1), generate (TupleT (AT, AT)) (depth - 1)
    in
    combiner l r
;;

let get_next () =
  let next = ref "a" in
  fun vars ->
    let n = !next in
    let nexts = if Char.(n.[String.length n - 1] = 'z') then n ^ "a" else n in
    let last = nexts.[String.length nexts - 1] in
    let last = last |> Char.to_int |> succ |> Char.of_int_exn |> Char.to_string in
    let nexts = String.sub nexts ~pos:0 ~len:(String.length nexts - 1) ^ last in
    next := nexts;
    vars := n :: !vars;
    n
;;

let to_string e =
  let next = get_next () in
  let vars = ref [] in
  let rec to_string_h : 'a. 'a expr -> string =
    fun (type a) (e : a expr) ->
     match e with
     | Var -> next vars
     | List es ->
       let inner = List.map ~f:to_string_h es |> String.concat ~sep:";" in
       Printf.sprintf "[%s]" inner
     | Cons (e1, e2) -> Printf.sprintf "(%s::%s)" (to_string_h e1) (to_string_h e2)
     | Tuple (e1, e2) -> Printf.sprintf "(%s, %s)" (to_string_h e1) (to_string_h e2)
     | App (e, es) ->
       let args = List.map ~f:to_string_h es |> String.concat ~sep:" " in
       Printf.sprintf "(%s %s)" (to_string_h e) args
     | Eq (e1, e2) -> Printf.sprintf "(%s = %s)" (to_string_h e1) (to_string_h e2)
     | Gt (e1, e2) -> Printf.sprintf "(%s > %s)" (to_string_h e1) (to_string_h e2)
     | Lt (e1, e2) -> Printf.sprintf "(%s < %s)" (to_string_h e1) (to_string_h e2)
     | Or (e1, e2) -> Printf.sprintf "(%s || %s)" (to_string_h e1) (to_string_h e2)
     | And (e1, e2) -> Printf.sprintf "(%s && %s)" (to_string_h e1) (to_string_h e2)
     | Not e -> Printf.sprintf "(not %s)" (to_string_h e)
     | If (e1, e2, e3) ->
       Printf.sprintf
         "(if %s then %s else %s)"
         (to_string_h e1)
         (to_string_h e2)
         (to_string_h e3)
  in
  let body = to_string_h e in
  let fun_header =
    let vars = if List.is_empty !vars then [ "a" ] else !vars in
    let vars = List.rev vars |> String.concat ~sep:" " in
    Printf.sprintf "fun %s -> " vars
  in
  let source = fun_header ^ body in
  let open Ocamlformat_lib in
  let res =
    Translation_unit.parse_and_format
      Syntax.Expression
      ~input_name:"problem.ml"
      ~source
      Conf.default
  in
  match res with
  | Ok s -> s
  | Error _ -> failwith ("Failed to generate expr: " ^ source)
;;

let gen_string depth =
  let g t = generate t depth |> to_string in
  match Random.int 5 with
  | 1 -> g AT
  | 2 ->
    (match Random.int 5 with
     | 0 -> g (ListT AT)
     | 1 -> g (ListT (ListT AT))
     | 2 -> g (ListT (TupleT (AT, AT)))
     | 3 -> g (ListT (ListT BoolT))
     | _ -> g BoolT)
  | 3 ->
    (match Random.int 2 with
     | 0 -> g (TupleT (AT, AT))
     | _ -> g (TupleT (ListT AT, AT)))
  | _ -> g BoolT
;;
