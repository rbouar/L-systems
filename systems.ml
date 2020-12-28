open Turtle
(** Words, rewrite systems, and rewriting *)

type 's word =
  | Symb of 's
  | Seq of 's word list
  | Branch of 's word

type 's rewrite_rules = 's -> 's word

type 's system = {
    axiom : 's word;
    rules : 's rewrite_rules;
    interp : 's -> Turtle.command list }

(** Put here any type and function implementations concerning systems *)
(** Computing the right scale *)


(* Coordonnées de départ de la tortue *)
let turtle_start_x = 0.0
let turtle_start_y = 0.0


(* Calcule le nouveau coin haut-gauche en fonction de la position de la tortue *)
let update_up_left turtle up_left =
  let pos = turtle_pos turtle in
  { up_left with x = min up_left.x pos.x; y = max up_left.y pos.y }

(* Calcule le nouveau coin bas-droite en fonction de la position de la tortue *)
let update_down_right turtle down_right =
  let pos = turtle_pos turtle in
  { down_right with x = max down_right.x pos.x; y = min down_right.y pos.y }


let rec frame_symb interp symb (turtle, up_left, down_right) =
  let t = symb |> interp |> Turtle.exec turtle in
  (t, update_up_left t up_left, update_down_right t down_right)

and frame_seq interp seq (turtle, up_left, down_right) =
  match seq with
  | [] -> (turtle, up_left, down_right)
  | x :: seq -> let res = frame_word interp x (turtle, up_left, down_right) in
                frame_seq interp seq res

and frame_branch interp branch (turtle, up_left, down_right) =
  let t1 = Turtle.exec turtle [Store] in
  let (t2, ul, dr) = frame_word interp branch (t1, up_left, down_right) in
  (Turtle.exec t2 [Restore], ul, dr)

and frame_word interp word (turtle, up_left, down_right) =
  match word with
  | Symb sym -> frame_symb interp sym (turtle, up_left, down_right)
  | Branch bra -> frame_branch interp bra (turtle, up_left, down_right)
  | Seq seq -> frame_seq interp seq (turtle, up_left, down_right)


(** Compute the minimal rectangle framing the lsystem *)
let frame_system sys =
  let frame_interp s = sys.interp s |> List.map (fun cmd -> match cmd with
                                                            | Line n -> Move n
                                                            | c -> c) in
  let turtle = Turtle.create_turtle_at turtle_start_x turtle_start_y in
  let pos = turtle_pos turtle in
  let _, upper_left, down_right = frame_word frame_interp sys.axiom (turtle, pos, pos) in
  upper_left, down_right;;


(* Calcule le facteur d'agrandissement tel que 
   le rectangle encadrant soit le plus grand possible sans sortir de la fenêtre *)
let scale_factor window_height window_width frame_height frame_width =
  min (window_height /. frame_height) (window_width /. frame_width)


let create_scaled_exec (factor : float) =
  fun turtle cmd_list ->
  match cmd_list with
  | [] -> turtle
  | c :: cmd_list -> match c with
                     | Line n -> exec turtle [Line (Int.of_float ((Float.of_int n) *. factor))]
                     | Move n -> exec turtle [Move (Int.of_float ((Float.of_int n) *. factor))]
                     | cmd -> exec turtle [cmd]


let new_turtle_start_x window_width up_left down_right turtle_x factor =
  (window_width /. 2.) -. factor *. ((up_left.x +. down_right.x) /. 2.) +. turtle_x *. (factor -. 1.)

let new_turtle_start_y window_height up_left down_right turtle_y factor =
  (window_height /. 2.) -. factor *. ((up_left.y +. down_right.y) /. 2.) +. turtle_y *. (factor -. 1.)


let rec draw_symb interp symb f turtle =
  symb |> interp |> f turtle

and draw_seq interp seq f turtle =
  match seq with
  | [] -> turtle
  | w :: seq -> let turtle' = draw_word interp w f turtle in
                draw_seq interp seq f turtle'

and draw_branch interp branch f turtle =
  let t1 = f turtle [Store] in
  let t2 = draw_word interp branch f t1 in
  f t2 [Restore]

and draw_word interp word f turtle =
  match word with
  | Symb symb -> draw_symb interp symb f turtle
  | Branch branch -> draw_branch interp branch f turtle 
  | Seq seq -> draw_seq interp seq f turtle 

(** Draw the given lsystem with right scale *)
let draw_system sys =
  let padding = 50. in
  let ul, dr = frame_system sys in

  let window_width = (Float.of_int (Graphics.size_x ())) -. padding in
  let window_height = (Float.of_int (Graphics.size_y ())) -. padding in
  let frame_height = Float.abs (ul.y -. dr.y) in
  let frame_width = Float.abs (dr.x -. ul.x) in

  let factor = scale_factor window_height window_width frame_height frame_width in

  let scaled_exec = create_scaled_exec factor in

  let turtle_x = new_turtle_start_x (window_width +. padding) ul dr turtle_start_x factor in
  let turtle_y = new_turtle_start_y (window_height +. padding) ul dr turtle_start_y factor in
  
  let turtle = Turtle.create_turtle_at turtle_x turtle_y in
  Graphics.clear_graph ();
  let _ = draw_word sys.interp sys.axiom scaled_exec turtle in
  ();;



let rec next_symb rules symb i =
  if i = 0 then Symb symb
  else next_word rules (rules symb) (i-1)

and next_seq rules seq i =
  let rec next_seq_list seq = match seq with
    | [] -> []
    | x :: l -> (next_word rules x i) :: (next_seq_list l)
  in Seq (next_seq_list seq)

and next_branch rules branch i =
  if i = 0 then Branch branch
  else
    Branch (next_word rules branch i)

and next_word rules word i =
  if i = 0 then word
  else match word with
       | Symb sym -> next_symb rules sym i
       | Branch bra -> next_branch rules bra i
       | Seq seq -> next_seq rules seq i

let next sys i =
  { sys with axiom = next_word sys.rules sys.axiom i}
