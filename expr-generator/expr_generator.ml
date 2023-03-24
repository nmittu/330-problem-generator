open! Core

module NonPolymorphic = struct
  type _ value =
    | Int : int -> int value
    | Float : float -> float value
    | Bool : bool -> bool value
    | String : string -> string value

  type _ typ =
    | IntT : int typ
    | FloatT : float typ
    | BoolT : bool typ
    | StringT : string typ
    | ListT : 'a typ -> 'a list typ

  let int = IntT
  let float = FloatT
  let bool = BoolT
  let string = StringT
  let list t = ListT t

  type _ expr =
    | IntVar : int expr
    | FloatVar : float expr
    | StringVar : string expr
    | BoolVar : bool expr
    | Const : 'a value -> 'a expr
    | List : 'a expr list -> 'a list expr
    | Cons : 'a expr * 'a list expr -> 'a list expr
    | Int_of_string : string expr -> int expr
    | Int_of_float : float expr -> int expr
    | Float_of_int : int expr -> float expr
    | Float_of_string : string expr -> float expr
    | String_of_float : float expr -> string expr
    | String_of_int : int expr -> string expr
    | Concat : string expr * string expr -> string expr
    | Plus : int expr * int expr -> int expr
    | Minus : int expr * int expr -> int expr
    | Mult : int expr * int expr -> int expr
    | Plus_dot : float expr * float expr -> float expr
    | Minus_dot : float expr * float expr -> float expr
    | Mult_dot : float expr * float expr -> float expr
    | Eq : 'a expr * 'a expr -> bool expr
    | Gt : 'a expr * 'a expr -> bool expr
    | Lt : 'a expr * 'a expr -> bool expr
    | Or : bool expr * bool expr -> bool expr
    | And : bool expr * bool expr -> bool expr
    | Not : bool expr -> bool expr
    | If : bool expr * 'a expr * 'a expr -> 'a expr

  let rec generate_int ?(forced = false) depth =
    if depth = 0
    then
      if forced
      then (
        let r = Random.int 3 in
        let value = Const (Int (Random.int 10)) in
        match r with
        | 0 -> Plus (IntVar, value)
        | 1 -> Minus (IntVar, value)
        | _ -> Mult (IntVar, value))
      else IntVar
    else (
      let r = Random.int 6 in
      match r with
      | 0 -> Int_of_string (generate_str (depth - 1))
      | 1 -> Int_of_float (generate_float (depth - 1))
      | 2 -> Plus (generate_int (depth - 1), generate_int (depth - 1))
      | 3 -> Minus (generate_int (depth - 1), generate_int (depth - 1))
      | 4 -> Mult (generate_int (depth - 1), generate_int (depth - 1))
      | _ ->
        If
          ( generate_bool (depth - 1)
          , generate_int (depth - 1) ~forced
          , generate_int (depth - 1) ))

  and generate_float ?(forced = false) depth =
    if depth = 0
    then
      if forced
      then (
        let r = Random.int 3 in
        let value = Const (Float (Random.float 9.5)) in
        match r with
        | 0 -> Plus_dot (FloatVar, value)
        | 1 -> Minus_dot (FloatVar, value)
        | _ -> Mult_dot (FloatVar, value))
      else FloatVar
    else (
      let r = Random.int 6 in
      match r with
      | 0 -> Float_of_int (generate_int (depth - 1))
      | 1 -> Float_of_string (generate_str (depth - 1))
      | 2 -> Plus_dot (generate_float (depth - 1), generate_float (depth - 1))
      | 3 -> Minus_dot (generate_float (depth - 1), generate_float (depth - 1))
      | 4 -> Mult_dot (generate_float (depth - 1), generate_float (depth - 1))
      | _ ->
        If
          ( generate_bool (depth - 1)
          , generate_float (depth - 1) ~forced
          , generate_float (depth - 1) ))

  and generate_str ?(forced = false) depth =
    if depth = 0
    then
      if forced
      then (
        let value = Const (String "") in
        Concat (StringVar, value))
      else StringVar
    else (
      let r = Random.int 4 in
      match r with
      | 0 -> String_of_float (generate_float (depth - 1))
      | 1 -> String_of_int (generate_int (depth - 1))
      | 2 -> Concat (generate_str (depth - 1), generate_str (depth - 1))
      | _ ->
        If
          ( generate_bool (depth - 1)
          , generate_str (depth - 1) ~forced
          , generate_str (depth - 1) ))

  and generate_bool ?(forced = false) depth =
    if depth = 0
    then
      if forced
      then (
        let value = Const (Bool false) in
        Or (BoolVar, value))
      else BoolVar
    else (
      let r = Random.int 7 in
      match r with
      | x when x < 3 -> generate_poly_compare (depth - 1)
      | 3 -> And (generate_bool (depth - 1), generate_bool (depth - 1))
      | 4 -> Or (generate_bool (depth - 1), generate_bool (depth - 1))
      | 5 -> Not (generate_bool (depth - 1))
      | _ ->
        If
          ( generate_bool (depth - 1)
          , generate_bool (depth - 1) ~forced
          , generate_bool (depth - 1) ))

  and generate_poly_compare depth =
    let combiner : 'a. 'a expr -> 'a expr -> bool expr =
     fun l r ->
      match Random.int 3 with
      | 0 -> Eq (l, r)
      | 1 -> Lt (l, r)
      | _ -> Gt (l, r)
    in
    match Random.int 4 with
    | 0 ->
      let l, r = generate_int depth ~forced:true, generate_int depth in
      combiner l r
    | 1 ->
      let l, r = generate_float depth ~forced:true, generate_float depth in
      combiner l r
    | 2 ->
      let l, r = generate_str depth ~forced:true, generate_str depth in
      combiner l r
    | _ ->
      let l, r = generate_bool depth ~forced:true, generate_bool depth in
      combiner l r

  and generate_list : 'a. 'a typ -> int -> 'a list expr =
    fun (type a) (t : a typ) (depth : int) : a list expr ->
     let listexps =
       List.init
         (Random.int 2 + 1)
         ~f:(fun i -> generate t (max (depth - 1) 1) ~forced:(i = 0))
     in
     match Random.int 2 with
     | 0 -> Cons (generate t (max (depth - 1) 1) ~forced:false, List listexps)
     | _ -> List listexps

  and generate : 'a. ?forced:bool -> 'a typ -> int -> 'a expr =
    fun (type a) ?(forced = true) (t : a typ) (depth : int) : a expr ->
     match t with
     | IntT -> generate_int depth ~forced
     | FloatT -> generate_float depth ~forced
     | StringT -> generate_str depth ~forced
     | BoolT -> generate_bool depth ~forced
     | ListT t -> generate_list t depth
  ;;

  let value_to_string (type a) (v : a value) : string =
    match v with
    | Int i -> Int.to_string i
    | Float f -> Printf.sprintf "%.2F" f
    | String s -> Printf.sprintf "\"%s\"" s
    | Bool b -> Bool.to_string b
  ;;

  type vars =
    { mutable int : string list
    ; mutable float : string list
    ; mutable string : string list
    ; mutable bool : string list
    ; next : unit -> string
    }

  let get_next () =
    let next = ref "a" in
    fun () ->
      let n = !next in
      next := Char.of_string n |> Char.to_int |> succ |> Char.of_int_exn |> String.of_char;
      n
  ;;

  let get_var (type a) (t : a typ) vars max_cat =
    let vs, set =
      match t with
      | IntT -> vars.int, fun x -> vars.int <- x
      | FloatT -> vars.float, fun x -> vars.float <- x
      | StringT -> vars.string, fun x -> vars.string <- x
      | BoolT -> vars.bool, fun x -> vars.bool <- x
      | _ -> failwith "Invalid var type"
    in
    if List.length vs < max_cat
    then (
      let n = vars.next () in
      set (n :: vs);
      n)
    else List.nth_exn vs (Random.int (List.length vs))
  ;;

  let shuffle d =
    let nd = List.map ~f:(fun c -> Random.bits (), c) d in
    let sond = List.sort ~compare:Poly.compare nd in
    List.map ~f:snd sond
  ;;

  let to_string (type a) ?(max_cat = 2) (e : a expr) =
    let vars = { int = []; float = []; string = []; bool = []; next = get_next () } in
    let rec to_string_helper : 'a. 'a expr -> string =
      fun (type a) (e : a expr) : string ->
       match e with
       | IntVar -> get_var int vars max_cat
       | FloatVar -> get_var float vars max_cat
       | StringVar -> get_var string vars max_cat
       | BoolVar -> get_var bool vars max_cat
       | Const v -> value_to_string v
       | List l ->
         "(["
         ^ List.foldi
             ~f:(fun i a e -> a ^ (if i = 0 then "" else ";") ^ to_string_helper e)
             ~init:""
             l
         ^ "])"
       | Cons (l, r) ->
         Printf.sprintf "(%s::%s)" (to_string_helper l) (to_string_helper r)
       | Int_of_string e -> Printf.sprintf "(int_of_string %s)" (to_string_helper e)
       | Int_of_float e -> Printf.sprintf "(int_of_float %s)" (to_string_helper e)
       | Float_of_int e -> Printf.sprintf "(float_of_int %s)" (to_string_helper e)
       | Float_of_string e -> Printf.sprintf "(float_of_string %s)" (to_string_helper e)
       | String_of_int e -> Printf.sprintf "(string_of_int %s)" (to_string_helper e)
       | String_of_float e -> Printf.sprintf "(string_of_float %s)" (to_string_helper e)
       | Concat (l, r) ->
         Printf.sprintf "(%s ^ %s)" (to_string_helper l) (to_string_helper r)
       | Plus (l, r) ->
         Printf.sprintf "(%s + %s)" (to_string_helper l) (to_string_helper r)
       | Minus (l, r) ->
         Printf.sprintf "(%s - %s)" (to_string_helper l) (to_string_helper r)
       | Mult (l, r) ->
         Printf.sprintf "(%s * %s)" (to_string_helper l) (to_string_helper r)
       | Plus_dot (l, r) ->
         Printf.sprintf "(%s +. %s)" (to_string_helper l) (to_string_helper r)
       | Minus_dot (l, r) ->
         Printf.sprintf "(%s -. %s)" (to_string_helper l) (to_string_helper r)
       | Mult_dot (l, r) ->
         Printf.sprintf "(%s *. %s)" (to_string_helper l) (to_string_helper r)
       | Eq (l, r) -> Printf.sprintf "(%s = %s)" (to_string_helper l) (to_string_helper r)
       | Gt (l, r) -> Printf.sprintf "(%s > %s)" (to_string_helper l) (to_string_helper r)
       | Lt (l, r) -> Printf.sprintf "(%s < %s)" (to_string_helper l) (to_string_helper r)
       | And (l, r) ->
         Printf.sprintf "(%s && %s)" (to_string_helper l) (to_string_helper r)
       | Or (l, r) ->
         Printf.sprintf "(%s || %s)" (to_string_helper l) (to_string_helper r)
       | Not e -> Printf.sprintf "(not %s)" (to_string_helper e)
       | If (p, e1, e2) ->
         Printf.sprintf
           "(if %s then %s else %s)"
           (to_string_helper p)
           (to_string_helper e1)
           (to_string_helper e2)
    in
    let body = to_string_helper e in
    let fun_header =
      let vars = vars.int @ vars.float @ vars.string @ vars.bool in
      let vars = shuffle vars in
      let vars_list = String.concat ~sep:" " vars in
      Printf.sprintf "fun %s -> " vars_list
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
    | Error _ -> failwith "Failed to generate expr"
  ;;
end
