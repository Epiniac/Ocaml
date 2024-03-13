#use "list.ml";;

#load "graphics.cma";;
open Graphics;;

(*#Pour creer une fenetre de %size de cote#*)

let open_window size =
  open_graph (" " ^ string_of_int size ^ "x" ^ string_of_int (size+20));;


(*#Creation  couleur#*)

let grey = rgb 127 127 127;;


(*#Creation d'une cellule#*)

let draw_cell (x, y) size color =
  set_color grey;
  draw_rect (x+1) (y+1) size size;
  set_color color;
  fill_rect (x+2) (y+2) (size-2) (size-2);;



let cell_color = function
  | 0 -> white
  | _ -> black;;

(*#Creation du tableau qui sera cree par la matrice %board#*)

let draw_board board size =
  let rec draw_column (x, y) (i, j) =
    let color = cell_color (get_cell (i, j)  board) in
         match i with
    | i when i = length (nth 0 board)-1  ->
      draw_cell (x, y) size color;
    | i ->
      draw_cell (x, y) size color;
        draw_column (x+size, y) (i+1, j);
  in
  let rec draw_line (x, y) (i, j) = match j with
    | j when j = length board-1 -> draw_column (x, y) (i, j);
    | j -> draw_column (x, y) (i, j);
      draw_line (x, y+size) (i, j+1);
  in
  draw_line (10, 10) (0, 0);;





(*#Debut du jeu#*)

let rules0 cell near = match (cell, near) with
    (0, 3) -> 1
  | (1, 2) | (1, 3) -> 1
  | _ -> 0 ;;


let new_cell = 1 ;;
let empty = 0;;
let is_alive cell = cell <> empty;; (*#Verification cellule noir ou non#*)


let count_neighbours (x, y) board size =
  let state (x, y) board = if x < 0 || x > size-1 || y < 0 || y > size-1 then 0
    else if is_alive (get_cell (x,y) board) then 1 else 0 in
  state (x, y-1) board +
    state (x, y+1) board +
    state (x-1, y)  board +
    state (x-1, y-1) board +
    state (x-1, y+1) board +
    state (x+1, y) board +
    state (x+1, y-1) board +
    state (x+1, y+1) board;;




let rec seed_life board size nb_cell =
  if nb_cell > size*siz then failwith("Nombre de celulles > taille du tableau")
  else match nb_cell with
    0 -> board
    | x -> let a = Random.int(size-1) in
           let b = Random.int(size-1) in
           if get_cell (a,b) board = 1 then seed_life (put_cell 1 (Random.int(size-1),Random.int(size-1)) board) size (nb_cell)
           else
             seed_life (put_cell 1 (Random.int(size-1),Random.int(size-1)) board) size (nb_cell-1);;




let new_board size nb = let board = init_board (size, size) 0 in
                        seed_life board size nb ;;



let next_generation board size =
  
  let rec generation_column (x, y) b =
    let val_cell = get_cell (x, y) board in
    let nb_neighb = count_neighbours (x, y) board size in
    match y with
    | y when y = size-1 -> put_cell (rules0 val_cell nb_neighb) (x, y) b
    | y -> let b = put_cell (rules0 val_cell nb_neighb) (x, y) b in
           generation_column (x, y+1) b
  in
  let rec generation_line (x, y) b = match x with
    | x when x = size-1 -> generation_column (x, y) b
    | x -> let b = generation_column (x, y) b in
           generation_line (x+1, y) b
  in generation_line (0, 0) board;;



let rec game board size n = match n with
      1 -> draw_board (next_generation board size) 15;
  | x -> draw_board (next_generation board size) 15;
    game (next_generation board size) size (x-1);;



let new_game size nb_cell n =
  close_graph();
  open_window (40 * 40 + 40);
  let board = new_board size nb_cell in
  draw_board board 15;
  game board size n;;
