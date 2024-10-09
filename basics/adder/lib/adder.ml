(* val addlist : int list -> int *)
(* addlist l adds the element of the list of integers l *)

let rec addlist l =
  match l with
  (* If the list is empty ([]), it returns 0.*)
  | [] -> 0
  (* Otherwise, it adds the head of the list to the result of recursively calling addlist on the tail of the list.*)
  | head :: tail -> head + addlist tail