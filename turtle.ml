
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
  mutable pos: position ;
  states: position Stack.t ;
}

let t = { pos = { x = 0.; y = 0.; a = 0}; states = Stack.create ()};;

let init () =
  Graphics.open_graph " 800x800";
  Graphics.moveto 400 400;
  t.pos <- {x = 400.; y=400.; a = 90}

let store () =
  Stack.push t.pos t.states

let restore () =
  t.pos <- Stack.pop t.states;
  Graphics.moveto (Float.to_int t.pos.x) (Float.to_int t.pos.y)

let turn a =
  let cur_pos = t.pos in
  t.pos <- { cur_pos with a = cur_pos.a + a}


let update_pos (n : int) : unit =
  let cur_pos = t.pos in
  let a' = (Float.of_int cur_pos.a) *. (Float.pi /. 180.) in
  let new_x = (Float.cos a' *. Float.of_int n) +. cur_pos.x in
  let new_y = (Float.sin a' *. Float.of_int n) +. cur_pos.y in
  t.pos <- { cur_pos with x = new_x; y = new_y}


let move n =
  update_pos n;
  Graphics.moveto (Float.to_int t.pos.x) (Float.to_int t.pos.y)

let line n =
  update_pos n;
  Graphics.lineto (Float.to_int t.pos.x) (Float.to_int t.pos.y)

let exec c_list = match c_list with
  | [] -> ()
  | x :: c_list -> match x with
    | Line n -> line n
    | Move n -> move n
    | Turn n -> turn n
    | Store -> store ()
    | Restore -> restore ()
