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

let rec draw_simb interp symb = symb |> interp |> Turtle.exec
and
draw_seq interp seq = match seq with
  | [] -> ()
  | x :: seq -> draw_word interp x; draw_seq interp seq
and
draw_branch interp branch =
  let _ = Turtle.exec [Store] in
  let _ = draw_word interp branch in
  Turtle.exec [Restore]
and
draw_word interp w = match w with
  | Symb sym -> draw_simb interp sym
  | Branch bra -> draw_branch interp bra
  | Seq seq -> draw_seq interp seq

let draw sys = draw_word sys.interp sys.axiom
