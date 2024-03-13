

let rec length = function
     [] -> 0
  | b::q -> 1 + length q;;


let rec append l m = match l with
    [] -> m
  | [a] -> a::m
  | b::q -> b::(append q m);;



let rec coupleSize = function
           [] -> 0
  | (x, y)::q -> x+coupleSize q;;



let rec nth i l = match (i, l) with
    (x, l) when x < 0 -> invalid_arg " nth: index must be a natural"
  | (_, []) -> failwith("nth: list too short")
  | (0, b::q) -> b
  | (x, c::q) -> nth (i-1) q;;


let search_pos a l =
  let rec search_rec c a l = match (a, l) with
    (_, []) -> failwith("search_pos: not found")
    | (a, b::q) when b = a -> c
    | (e, f::q) -> search_rec (c+1) a q
  in search_rec 0 a l;;



let rec sum_digits = function
    0 -> 0
  | n -> n mod 10 + sum_digits (n/10);;



let rec is_common l a = match l with
    [] -> false
  | b::q -> a = b || is_common q a;;

let rec common (l, q) = match (l, q) with
    ([], []) -> 0
  | (_, []) | ([], _) -> 0
  | (l, b::m) -> if is_common l b then b else common (l,m);;



let rec prefix = function
    ([], []) -> true
  | (_, []) | ([], _) -> true
  | (a::m, b::n) -> a = b && prefix (m, n);;


let rec init_list n x = match n with
    n when n < 0 -> invalid_arg "int_list: n must be a natural"
  | 0 -> []
  | n -> x::init_list (n-1) x;;



let rec put_list v i list = match (i, list) with
    (i, []) -> list
  | (0, a::l) -> v::l
  | (i, a::q) -> a::put_list v (i-1) q;;


let init_board (i, c) x = init_list i (init_list c x) ;;


let get_cell (x, y) board = nth y (nth x board);;



let put_cell v (x, y) board = put_list (put_list v y (nth x board)) x board;;