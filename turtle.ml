
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

let store () = Stack.push t.pos t.states

let restore () = t.pos <- Stack.pop t.states
