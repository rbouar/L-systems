open Systems

(* Cherche le début de la branche (i.e. '[') dans la sous-chaîne [inf, branch_end] de s.
 * On suppose que s.[branch_end] = ']' i.e. branch_end est la fin de la branche *)
let find_branch_start s inf branch_end =
  let rec rloop i nb =
    if nb < 0 || i < inf then failwith "Unbalanced branch"
    else match s.[i] with
         | '[' -> if nb=0 then i else rloop (i-1) (nb-1)
         | ']' -> rloop (i-1) (nb+1)
         |  _  -> rloop (i-1) nb in
  rloop (branch_end-1) 0;;

(* Parse une séquence dans la sous-chaîne [inf, sup[ de s *)
let rec parse_seq s inf sup acc =
  if sup - inf = 0 then Seq acc
  else if s.[sup-1] = ']' then
    let bs = find_branch_start s inf (sup-1) in (*bs : branch start *)
    parse_seq s inf bs (parse_word s bs sup::acc)                               
  else parse_seq s inf (sup-1) ((Symb s.[sup-1])::acc)

and parse_word s inf sup =
  if sup - inf = 1 then Symb s.[inf]
  else if s.[sup-1] = ']' && inf = find_branch_start s inf (sup-1) then
    Branch (parse_word s (inf+1) (sup-1))
  else parse_seq s inf sup [];;

(* Convertit une chaîne de caractères en char word *)
let word_of_string s =
  let len = String.length s in
  if len = 0 then failwith "Empty string"
  else parse_word s 0 len;;
