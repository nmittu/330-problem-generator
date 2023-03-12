open! Core
module StringSet = Set.Make (String)
module StringMap = Hashtbl.Make (String)

type ocaml_type =
  | Int
  | Float
  | String
  | Bool
  | Tuple of ocaml_type * ocaml_type
  | List of ocaml_type
  | Func of (ocaml_type * ocaml_type)
  | Polymorphic of string

type mode =
  [ `Non_polymorphic
  | `Polymorphic of int
  ]
[@@deriving sexp, compare]

type typ =
  [ `Func of int
  | `Norm
  ]
[@@deriving sexp, compare]

let rec gen_base ?(tuples = true) ?(lists = true) ~mode () =
  let rand = Random.int 10 in
  if tuples && rand = 0
  then Tuple (gen_base ~mode ~tuples:false (), gen_base ~mode ~tuples:false ())
  else if lists && rand = 1
  then List (gen_base ~mode ~tuples:false ~lists:false ())
  else (
    match mode with
    | `Non_polymorphic ->
      (match Random.int 4 with
       | 0 -> Int
       | 1 -> Float
       | 2 -> String
       | _ -> Bool)
    | `Polymorphic -> Polymorphic "UNK")
;;

let rec gen_func ~mode n =
  let ret = if n > 1 then gen_func ~mode (n - 1) else gen_base ~mode () in
  Func (gen_base ~mode (), ret)
;;

let generate_skeleton ~mode ~typ n =
  let get_fun_param_num () = max (Random.int 5 / 2) 1 in
  let rec generate ~typ n =
    let param, typ =
      match typ with
      | `Func n when n > 0 -> gen_func ~mode @@ get_fun_param_num (), `Func (n - 1)
      | `Norm | _ -> gen_base ~mode ~lists:(n <> 1) (), typ
    in
    let ret = if n > 1 then generate ~typ (n - 1) else gen_base ~mode () in
    Func (param, ret)
  in
  generate ~typ n
;;

let rec poly_pass ~alphabet ?(in_use = StringSet.empty) t =
  let rec update_poly ~in_use t =
    match t with
    | Polymorphic "UNK" ->
      let alpha = alphabet |> StringSet.elements in
      let alpha = List.nth_exn alpha (Random.int (List.length alpha)) in
      Polymorphic alpha, StringSet.add in_use alpha
    | Tuple (l, r) ->
      let l, in_use = update_poly ~in_use l in
      let r, in_use = update_poly ~in_use r in
      Tuple (l, r), in_use
    | _ -> t, in_use
  in
  match t with
  | Func (p, ret) ->
    let p, in_use = update_poly ~in_use p in
    let ret, in_use = poly_pass ~alphabet ~in_use ret in
    Func (p, ret), in_use
  | _ -> t, in_use
;;

let rec fill_in ~alphabet t =
  match t with
  | Int | Float | String | Bool -> t, StringSet.empty
  | Tuple (l, r) ->
    let l, using = fill_in ~alphabet l in
    let r, usingr = fill_in ~alphabet r in
    let using = StringSet.union using usingr in
    Tuple (l, r), using
  | List t ->
    let t, using = fill_in ~alphabet t in
    List t, using
  | Func (_, _) -> t, StringSet.empty (* Shouldn't have funcs nested twice deep *)
  | Polymorphic "UNK" ->
    let alpha = alphabet |> StringSet.elements in
    let alpha = List.nth_exn alpha (Random.int (List.length alpha)) in
    Polymorphic alpha, StringSet.singleton alpha
  | Polymorphic n -> t, StringSet.singleton n
;;

let rec func_pass ~alphabet ~in_use t =
  let rec update_func t =
    match t with
    | Func (l, r) ->
      let l, _ = fill_in ~alphabet:in_use l in
      let r, in_use = update_func r in
      Func (l, r), in_use
    | _ -> fill_in ~alphabet t
  in
  match t with
  | Func ((Func (_, _) as param), r) ->
    let param, in_use' = update_func param in
    let in_use = StringSet.union in_use' in_use in
    let r, in_use = func_pass ~alphabet ~in_use r in
    Func (param, r), in_use
  | Func (l, r) ->
    let r, in_use = func_pass ~alphabet ~in_use r in
    Func (l, r), in_use
  | _ ->
    let t, _ = fill_in ~alphabet:in_use t in
    t, in_use
;;

let rec list_pass ~alphabet t =
  match t with
  | Func (l, r) -> Func (list_pass ~alphabet l, list_pass ~alphabet r)
  | List t -> List (fill_in ~alphabet t |> fst)
  | _ -> t
;;

let normalize t =
  let mapping = StringMap.create () in
  let next = ref 'a' in
  let rec normalize t =
    match t with
    | Int | Float | String | Bool -> t
    | Tuple (l, r) ->
      let l = normalize l in
      let r = normalize r in
      Tuple (l, r)
    | List t -> List (normalize t)
    | Func (l, r) ->
      let l = normalize l in
      let r = normalize r in
      Func (l, r)
    | Polymorphic a ->
      (match StringMap.find mapping a with
       | Some a -> Polymorphic a
       | None ->
         StringMap.add ~key:a ~data:(Char.to_string !next) mapping |> ignore;
         let a = !next in
         next := Char.of_int_exn (Char.to_int !next + 1);
         Polymorphic (Char.to_string a))
  in
  normalize t
;;

let gen_poly ~alphabet ~typ n =
  let skeleton = generate_skeleton ~mode:`Polymorphic ~typ n in
  let t, in_use = poly_pass ~alphabet skeleton in
  let t, _ = func_pass ~alphabet ~in_use t in
  list_pass ~alphabet t |> normalize
;;

let rec to_string t =
  match t with
  | Int -> "int"
  | Float -> "float"
  | String -> "string"
  | Bool -> "bool"
  | Tuple (l, r) ->
    let l = to_string l in
    let r = to_string r in
    Printf.sprintf "%s * %s" l r
  | List t ->
    let s = to_string t in
    Printf.sprintf "%s list" s
  | Func ((Func (_, _) as p), r) ->
    let p = to_string p in
    let r = to_string r in
    Printf.sprintf "(%s) -> %s" p r
  | Func (p, r) ->
    let p = to_string p in
    let r = to_string r in
    Printf.sprintf "%s -> %s" p r
  | Polymorphic t -> Printf.sprintf "'%s" t
;;

let generate ~mode ~typ n =
  match mode with
  | `Non_polymorphic -> generate_skeleton ~mode:`Non_polymorphic ~typ n
  | `Polymorphic a ->
    let alphabet =
      List.init a ~f:(fun i -> Char.of_int_exn (Char.to_int 'a' + i) |> Char.to_string)
      |> StringSet.of_list
    in
    gen_poly ~alphabet ~typ n
;;
