let remove_if_exist filename =
  if Sys.file_exists filename then Sys.remove filename

let add_svg_line c_out (pos1: Turtle.position) (pos2 : Turtle.position) =
  Printf.fprintf c_out "<line stroke=\"black\" x1=\"%f\" x2=\"%f\" y1=\"%f\" y2=\"%f\"/>\n" pos1.x pos2.x pos1.y pos2.y


let line t n f c_out =
  let t' = Turtle.update_pos t n f in
  let pos = Turtle.turtle_pos t in
  let pos' = Turtle.turtle_pos t' in
  let _ = add_svg_line c_out pos pos' in
  t'

let rec exec c_out f t l =
  match l with
  | [] -> t
  |  x :: l' -> let t' = match x with
    | Turtle.Line n -> line t n f c_out
    | Turtle.Move n -> Turtle.update_pos t n f
    | _ -> Turtle.exec f t [x] in
    exec c_out f t' l'


let write_content_of_svg sys c_out =
  let factor, turtle_x, turtle_y = Systems.compute_factor 800 800 sys in
  let t = Turtle.create_turtle_at turtle_x turtle_y in
  Systems.iter_word sys.axiom sys.interp (exec c_out factor) t


let save sys filename =
  let _ = remove_if_exist in
  let c_out = open_out filename in
  let _  = output_string c_out "<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" width=\"800\" height=\"800\">\n" in
  let _ = write_content_of_svg sys c_out in
  let _ = output_string c_out "</svg>\n" in
  close_out c_out
