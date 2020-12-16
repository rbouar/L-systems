type command =
  | Line of int
  | Move of int
  | Turn of int
  | Store
  | Restore

type position = {
    x: float;      (** position x *)
    y: float;      (** position y *)
    a: int;        (** angle of the direction *)
  }

(** Put here any type and function implementations concerning turtle *)
type turtle = {
    pos : position;
    states : position list;
  }


let init_graphics () =
  Graphics.open_graph " 800x800";;


let create_turtle () =
  let middle_x = (Graphics.size_x ()) / 2 in
  let middle_y = (Graphics.size_y ()) / 2 in  
  Graphics.moveto middle_x middle_y;
  let pos = {x = (Float.of_int middle_x);
             y = (Float.of_int middle_y);
             a = 90} in
  { pos = pos; states = [] };;


let store t =
  { t with states = t.pos :: t.states }


let restore t =
  match t.states with
  | [] -> failwith "No more state"
  | p :: l' -> Graphics.moveto (Float.to_int p.x) (Float.to_int p.y);
               {pos = p; states = l'}

let turn t angle =
  let new_pos = { t.pos with a = t.pos.a + angle } in
  { t with pos = new_pos }

let radian_of_degree deg =
  deg *. (Float.pi /. 180.);;

let update_pos t n =
  let cur_pos = t.pos in
  let a' = radian_of_degree (Float.of_int cur_pos.a) in
  let new_x = (Float.cos a' *. Float.of_int n) +. cur_pos.x in
  let new_y = (Float.sin a' *. Float.of_int n) +. cur_pos.y in  
  let new_pos = { t.pos with x = new_x; y = new_y } in
  { t with pos = new_pos }


let move t n =
  let t' = update_pos t n in
  Graphics.moveto (Float.to_int t'.pos.x) (Float.to_int t'.pos.y);
  t'

let line t n =
  let t' = update_pos t n in
  Graphics.lineto (Float.to_int t'.pos.x) (Float.to_int t'.pos.y);
  t'

let exec t c_list =
  match c_list with
  | [] -> t
  | x :: c_list -> match x with
                   | Line n -> line t n
                   | Move n -> move t n
                   | Turn n -> turn t n
                   | Store -> store t
                   | Restore -> restore t
