(* Utility for func definition*)
type word = char list

(* [01]+ *)
(*Questa espressione regolare rappresenta una sequenza di uno o più caratteri 0 o 1 in qualsiasi posizione*)
let rec lang1 (w : word) : bool = 
  match w with
  | [] -> false
  | ['0'] | ['1'] -> true
  | '0' :: l | '1' :: l -> lang1 l
  | _ -> false


(* 0?1* *)
(*Questa espressione regolare rappresenta una sequenza che può iniziare con zero o un 0, seguita da zero o più 1 *)
(*step2 determines the new state based on the current state (q) and the current char (a) *)
let step2 q a = match q with
    0 when a='0' || a='1' -> 1
  | 1 when a='1' -> 1
  | _ -> -1

(* lang2 applies step2 to every character of the list w and updates the current state*)
let lang2 w = match List.fold_left step2 0 w with
    0 | 1 -> true
  | _ -> false


(* 0[01]*0 *)
(*Questa espressione regolare rappresenta una sequenza che inizia e finisce con 0, con zero o più caratteri 0 o 1 tra di essi *)
(*step3 determines the new state based on the current state (q) and the current char (a) *)
let step3 q a = match q with
    0 when a='0' -> 1
  | 1 when a='0' -> 2
  | 1 when a='1' -> 1
  | 2 when a='0' -> 2
  | 2 when a='1' -> 1    
  | _ -> -1

(* lang3 applies step3 to every character of the list w and updates the current state*)
let lang3 w = match List.fold_left step3 0 w with
    2 -> true
  | _ -> false


(* 0*10*10* *)
(* Questa espressione regolare rappresenta una sequenza che contiene uno o più 0 seguiti da un 1, poi zero o più 0, un altro 1, e infine zero o più 0 *)
(*step4 determines the new state based on the current state (q) and the current char (a) *)
let step4 q a = match q with
    0 when a='0' -> 0
  | 0 when a='1' -> 1
  | 1 when a='0' -> 1
  | 1 when a='1' -> 2
  | 2 when a='0' -> 2
  | 2 when a='1' -> -1
  | _ -> -1

(* lang4 applies step4 to every character of the list w and updates the current state*)
let lang4 w = match List.fold_left step4 0 w with
    2 -> true
  | _ -> false


(* (00|11)+ *)
(* Questa espressione regolare rappresenta una sequenza di uno o più gruppi di 00 o 11 *)
(*step5 determines the new state based on the current state (q) and the current char (a) *)
let step5 q a = match q with
    0 when a='0' -> 1
  | 0 when a='1' -> 4
  | 1 when a='0' -> 2
  | 2 when a='0' -> 3
  | 2 when a='1' -> 6
  | 3 when a='0' -> 2
  | 4 when a='1' -> 5
  | 5 when a='0' -> 3
  | 5 when a='1' -> 6   
  | 6 when a='1' -> 5   
  | _ -> -1

(* lang5 applies step5 to every character of the list w and updates the current state*)
  let lang5 w = match List.fold_left step5 0 w with
  2 | 5 -> true
| _ -> false


let recognizers = [lang1;lang2;lang3;lang4;lang5]
                  
let belongsTo w = List.map (fun f -> f w) recognizers