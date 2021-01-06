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


type turtle = {
    pos : position;
    states : position list;
  }

let create_turtle_at x y =
  Graphics.moveto (Int.of_float x) (Int.of_float y);
  let pos = {x = x; y = y; a = 90} in
  { pos = pos; states = [] }


let turtle_pos turtle =
  turtle.pos


(** Degree to radian *)
let radian_of_degree deg =
  deg *. (Float.pi /. 180.);;

(** Advance turtle t by n units *)
let update_pos t n f =
  let cur_pos = t.pos in
  let a' = radian_of_degree (Float.of_int cur_pos.a) in
  let dist = (float_of_int n) *. f in
  let new_x = (Float.cos a' *. dist) +. cur_pos.x in
  let new_y = (Float.sin a' *. dist) +. cur_pos.y in
  let new_pos = { t.pos with x = new_x; y = new_y } in
  { t with pos = new_pos }


let line t n f =
  let t' = update_pos t n f in
  Graphics.lineto (Float.to_int t'.pos.x) (Float.to_int t'.pos.y);
  t'

let move t n f =
  let t' = update_pos t n f in
  Graphics.moveto (Float.to_int t'.pos.x) (Float.to_int t'.pos.y);
  t'

let turn t angle =
  let new_pos = { t.pos with a = t.pos.a + angle } in
  { t with pos = new_pos }

let store t =
  { t with states = t.pos :: t.states }

let restore t =
  match t.states with
  | [] -> failwith "No more state"
  | p :: l' -> Graphics.moveto (Float.to_int p.x) (Float.to_int p.y);
               {pos = p; states = l'}


let rec exec f t l =
  match l with
  | [] -> t
  | x :: l' -> let t' = match x with
                 | Line n -> line t n f
                 | Move n -> move t n f
                 | Turn n -> turn t n
                 | Store -> store t
                 | Restore -> restore t in
               exec f t' l'
