open Systems

(* Cherche le début de la branche (i.e. '[') dans la sous-chaîne [inf, branch_end] de s.
 * On suppose que branch_end est la fin de la branche (i.e s.[branch_end] = ']') *)
let find_branch_start s inf branch_end =
  let rec rloop i nb =
    if nb < 0 || i < inf then failwith "Unbalanced branch"
    else match s.[i] with
         | '[' -> if nb = 0 then i else rloop (i - 1) (nb - 1)
         | ']' -> rloop (i - 1) (nb + 1)
         |  _  -> rloop (i - 1) nb in
  rloop (branch_end - 1) 0;;


(* Parse un mot dans la sous-chaîne [inf, sup[ de s *)
let rec parse_word s inf sup =
  if sup - inf = 1 then Symb s.[inf] else
    if s.[sup - 1] = ']' && inf = find_branch_start s inf (sup - 1) then
      Branch (parse_word s (inf + 1) (sup - 1)) else
      parse_seq s inf sup []

(* Parse une séquence dans la sous-chaîne [inf, sup[ de s *)
and parse_seq s inf sup acc =
  if sup - inf = 0 then Seq acc else
    if s.[sup - 1] = ']' then
      let bs = find_branch_start s inf (sup - 1) in (*bs : branch start *)
      parse_seq s inf bs ((parse_word s bs sup) :: acc) else
      parse_seq s inf (sup - 1) ((Symb s.[sup - 1]) :: acc);;

(* Convertit une chaîne de caractères en char word *)
let word_of_string s =
  let len = String.length s in
  if len = 0 then raise (Invalid_argument "empty string")
  else parse_word s 0 len;;


let create_match_function assoc default =
  fun c -> match List.assoc_opt c assoc with
           | None -> default
           | Some r -> r;;


let command_of_string s =
  let len = String.length s in
  if len = 0 then raise (Invalid_argument "empty string")
  else let n = (String.sub s 1 (len - 1)) |> int_of_string in
       match s.[0] with
       | 'T' -> Turtle.Turn n
       | 'M' -> Turtle.Move n
       | 'L' -> Turtle.Line n
       |  _  -> raise (Invalid_argument "bad pattern");;


let rec skip ci =
  let l = input_line ci in
  if (String.length l) = 0 || l.[0] = '#' then skip ci
  else l;;


let parse ci =  
  try
    let axiom = word_of_string (skip ci) in
    Some axiom
  with End_of_file -> None;;


let parse_system file =
  let ic = open_in file in
  let res = ref (parse ic) in 
  close_in ic;
  !res;;
