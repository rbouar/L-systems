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

let rec draw_symb interp symb turtle =
  symb |> interp |> Turtle.exec turtle

and draw_seq interp seq turtle =
  match seq with
  | [] -> turtle
  | x :: seq -> let t' = draw_word interp x turtle in
                draw_seq interp seq t'

and draw_branch interp branch turtle =
  let t1 = Turtle.exec turtle [Store] in
  let t2 = draw_word interp branch t1 in
  Turtle.exec t2 [Restore]

and draw_word interp word turtle =
  match word with
  | Symb sym -> draw_symb interp sym turtle
  | Branch bra -> draw_branch interp bra turtle
  | Seq seq -> draw_seq interp seq turtle

let draw_system sys =
  let turtle = Turtle.create_turtle () in
  let _ = draw_word sys.interp sys.axiom turtle in
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
