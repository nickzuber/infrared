(* Generic Stack data structure *)

exception PopFromEmptyList

module Stack : sig
  type 'a t = Node of 'a * 'a t | Nil
  val create : 'a -> 'a t
  val push : 'a t -> 'a -> 'a t
  val pop : 'a t -> 'a t
  val peek : 'a t -> 'a option
  val size : 'a t -> int
end = struct
  type 'a t = 
    | Node of 'a * 'a t
    | Nil
  let create n = Node (n, Nil)
  let push s n = Node (n, s)
  let pop s = 
    match s with
    | Node (n, s) -> s
    | Nil -> s
  let peek s = 
    match s with
    | Node (n, s) -> Some n
    | Nil -> None
  let size s = 
    let rec _size s n =
      match s with 
      | Node (_, s) -> _size s (n + 1)
      | Nil -> n
    in _size s 0
end

(* http://stackoverflow.com/questions/10068713/string-to-list-of-char *)
let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let rest l =
  match l with
  | _ :: r -> r
  | _ -> l

let pop_last_element lst =
  let rec loop original rest =
    match original with
    | [] -> raise PopFromEmptyList
    | hd :: [] -> hd, rest
    | hd :: tl -> loop tl (rest @ [hd])
  in loop lst []

(* Split a path into two parts; the target and its path 
 * Eg. `path/to/some/file` -> `path/to/some/`, `file` *)
let depath path = Core.Std.(
  let parts = String.split_on_chars ~on:['/'] path in
  let target, rest_of_path = pop_last_element parts in
  let rest_of_path_string = String.concat ~sep:"/" rest_of_path
  in target, rest_of_path_string ^ "/")
