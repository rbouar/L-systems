
(** Turtle graphical commands *)
type command =
| Line of int      (** advance turtle while drawing *)
| Move of int      (** advance without drawing *)
| Turn of int      (** turn turtle by n degrees *)
| Store            (** save the current position of the turtle *)
| Restore          (** restore the last saved position not yet restored *)
| Color of Graphics.color

(** Position and angle of the turtle *)
type position = {
  x: float;        (** position x *)
  y: float;        (** position y *)
  a: int;          (** angle of the direction *)
}

type turtle

(** Create turtle at given positions *)
val create_turtle_at : float -> float -> turtle

(** Get turtle's current position *)
val turtle_pos : turtle -> position

(** Exec turtle commands list *)
val exec : float -> turtle -> command list -> turtle
