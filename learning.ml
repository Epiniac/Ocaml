(*Learning bases*)

let rec build_line n str = match n with
    0 -> ""
  | _ -> str ^ build_line(n-1) str;;
(*Faire une ligne avec s une chaine de caractere et n le nombre
de fois a afficher s. *)

let square n str =
  let line = build_line n str in
  let rec square2 n = match n with
      0 -> ()
    |  _ -> print_string line ; print_string "\n" ; square2 (n-1)
  in square2 n;;
(*Meme fonction que build_line mais affiche la ligne n fois. *)

let triangle n s =
  let rec go q f s =
    (* q est le compteur de caracteres permettant d4en mettre un nouveau 
      caractere a chaque nouvelle ligne, elle meme generee par print_newline().
 s est la chaine de caracteres a afficher. *)

    if (q - 1) = f then ()
    else
      begin
      print_string (build_line q s) ;
      print_newline ();
      go (q + 1) f s
      end
  in go 1 n s;;

let rec build_line2 n (str, str2) = match n with
     0 -> ""
    | _ -> str ^ str2 ^ build_line2(n-1) (str,str2);;
(*Faire une nouvelle ligne avec deux caractereres*)

let square2 n (str,str2)  =
  let line = build_line2 n (str,str2)  in
  let rec square2X n = match n with
       0 -> ()
    |  _ -> print_string line ; print_string "\n" ; square2X (n-1)
  in square2X n;;

(*faire le carre avec les deux caracteres differents*)






#load "graphics.cma";;
open Graphics;;
open_graph "";;
open_graph "300x100";;







let draw_line a b c d =
  moveto a b;
    lineto c d;;
(* Faire une ligne de A(a,b) jusque B(c,d) *)






let rec mountain a b c d q =
  (* A(a,b) et B(c,d) & q est un compteur premettant la recusivite*)

  let m = (a +c) / 2 and n = (b + d)/2 + Random.int(abs(c - a)/5 + 20) in
  (* I(m,n) est le point au milieu du segment nouvellement cree*)

  match q with

      0 -> draw_line m n c d
    | _ -> mountain a b m n (q -1);
      mountain m n c d (q - 1);;






let rec dragon (a,b) (c,d) q =
  (* q est un compteur permettant la recusivite. Sinon, dragon possede
     globalenent meme fonctionnement global que mountain *)
  
  let u = (a + c)/2 + (d - b)/2 and v = (b + d)/2 - (c - a)/2
  in
    match q with
      0 -> draw_line a b u v ;
        draw_line u v c d
    | _ -> dragon (a,b) (u,v) (q-1) ;
      dragon (c,d) (u,v) (q-1) ;;






let rec sponge a b h q =
(* A(a,b) est le point en haut a gauche. h est la longueur du carre.
   q est le compteur des repetitions *)
  
  let h = h/3 in
  match q with
      0 -> fill_rect a (b - h) h h
    | _ -> sponge a b h (q - 1);
      sponge a (b - h) h (q -1);
      sponge a (b - 2*h) h (q -1);
      sponge (a + h) (b - 2*h) h (q-1);
      sponge (a + 2*h) (b - 2*h) h (q - 1);
      sponge (a + 2*h) (b - h) h (q - 1);
      sponge (a + 2*h) b h (q - 1);
      sponge (a + h) b h (q - 1);;

