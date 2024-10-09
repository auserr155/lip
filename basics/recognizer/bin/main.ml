open Recognizer
    
(* read one line from standard input, and output it to a string *)

let read_line () =
  try Some(read_line())
  with End_of_file -> None

(* convert a string to a list of char *)

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let string_of_list s l =
  assert (List.length l = 5);
  if List.for_all (fun x -> x=false) l
  then s ^ " does not belong to any of the languages"
  else s ^ " belongs to languages: " ^ (List.fold_left (fun s i -> s ^ (if s="" then "" else ",") ^ string_of_int (i+1)) "" ((List.filter (fun i -> i>=0) (List.mapi (fun i b -> if b then i else -1) l))))
    
(* main routine: reads a line from the stdin, converts it into a list of chars, and then applies the function: *)

let () = match read_line () with
    Some s -> let l = belongsTo (explode s) in
    print_endline (string_of_list s l)
  | None -> print_endline "no winner"


(* Utils to recognize each language *)
type word = char list


(* Finite state automa *)
type state = int 
type trans = state -> char -> state
type dfa = {init : state; delta: trans; finals : state list}
type recognizer = dfa -> word -> bool

(* Transition function, word to read *)
let rec steps (t: trans) (q : state) (w : word) : state =
    match w with
    | [] -> q
    | c::w' ->
    let q' = t q c in
    steps t q' w'

let recognize : recognizer =
  fun a w ->
    let result = steps a.delta a .init w in
    List.mem result a.finals
  
let lang1_trans : trans = 
  fun q c ->
    match (q,c) with
    | 0, '0' -> 1
    | 0, '1' -> 1
    | 1, '0' -> 1
    | 1, '1' -> 1
    | _ -> -1

let lan1_dfa dfa {init = 0; delta = lang1_trans; finals = [1]}

let charlist_of_string (lst : string) : word =
  lst |> String.to_seq |> List.of_seq
        
(* [01]+ *)
(*Questa espressione regolare rappresenta una sequenza di uno o più caratteri 0 o 1 in qualsiasi posizione*)
let rec lang1 (w : word) : bool = 
  match w with
  | [] -> false
  | ['0'] | ['1'] -> true
  | '0' :: ls | '1' :: ls -> lang1 ls
  | _ -> false

(* 0?1* *)
(*Questa espressione regolare rappresenta una sequenza che può iniziare con zero o un 0, seguita da zero o più 1 *)
let rec lang2 (w : word) : bool = 
  match w with
  | [] -> true
  | ['0'] -> true
  | '1' :: ls -> lang2 ls
  | '0' :: '1' :: ls -> lang2 ('1' :: ls)
  | _ -> false

(* 0[01]*0 *)
(*Questa espressione regolare rappresenta una sequenza che inizia e finisce con 0, con zero o più caratteri 0 o 1 tra di essi *)
let rec lang3 = function
| ['0'] -> false
| '0' :: rest -> (match List.rev rest with
                  | '0' :: rev_rest -> true
                  | _ -> false)
| _ -> false

(* 0*10*10* *)
(* Questa espressione regolare rappresenta una sequenza che contiene uno o più 0 seguiti da un 1, poi zero o più 0, un altro 1, e infine zero o più 0 *)
let rec lang4 = function
| [] -> false
| '1' :: rest -> (match rest with
                            | [] -> false
                            | '0' :: rest2 -> (match List.rev rest2 with
                                              | '1' :: rev_rest2 -> true
                                              | _ -> false))
| '0' :: rest -> lang4 rest
| _ -> false

(* (00|11)+ *)
(* Questa espressione regolare rappresenta una sequenza di uno o più gruppi di 00 o 11 *)
let rec lang5 = function
| [] -> false
| '0' :: '0' :: rest -> lang5 rest
| '1' :: '1' :: rest -> lang5 rest
| _ -> false

(* Main belongsTo function *)
let belongsTo w =
[lang1 w; lang2 w; lang3 w; lang4 w; lang5 w]