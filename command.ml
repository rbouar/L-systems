exception Quit
let rec wait or_sys cur_sys cur_index =
  let rec cmd_pressed () =
    let ev = Graphics.wait_next_event [Graphics.Key_pressed] in
    if ev.keypressed
    then match ev.key with
      | 'q' -> raise Quit
      | ' ' -> Graphics.clear_graph (); Systems.draw_system cur_sys; cmd_pressed ()
      | '+' -> wait or_sys (Systems.next cur_sys 1) (cur_index + 1)
      | '-' -> wait or_sys (Systems.next or_sys (cur_index - 1)) (cur_index - 1)
      | '0'.. '9' as x ->
        let n = (Char.code x) - (Char.code '0') in
        if n >= cur_index then wait or_sys (Systems.next cur_sys (n - cur_index)) n
        else wait or_sys (Systems.next or_sys n) n
      | x -> Printf.printf "Commande inconnue: %c" x; print_newline (); cmd_pressed ()
    else cmd_pressed ()
  in cmd_pressed ()
