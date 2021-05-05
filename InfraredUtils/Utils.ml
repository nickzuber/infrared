let is_substring (needle : string) (haystack : string) =
  let rgx_string = Printf.sprintf ".*\\(%s\\).*" needle in
  let rgx = Str.regexp rgx_string in
  Str.string_match rgx haystack 0

let string_of_char_list cl =
  String.concat "" (List.map (String.make 1) cl)

let char_list_of_string s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let rand_lowercase_letter () : string = String.make 1 (Char.chr (97 + (Random.int 26)))
let rand_uppercase_letter () : string = String.make 1 (Char.chr (65 + (Random.int 26)))
let rand_number_as_string () : string = string_of_int (Random.int 10)
let rand_char_as_string () : string =
  let seed = Random.int 3 in
  match seed with
  | 0 -> rand_lowercase_letter ()
  | 1 -> rand_uppercase_letter ()
  | _ -> rand_number_as_string ()

let generate_hash () : string =
  let size = 8 in
  let list_of_chars_as_strings = List.init size (fun _ -> rand_char_as_string ()) in
  String.concat "" list_of_chars_as_strings

let math_min (a : int) (b : int) : int =
  if a < b then a else b

let math_max (a : int) (b : int) : int =
  if a < b then b else a

let parse_args_for_flags (args : string list) : (string list * string list) =
  List.partition (fun arg -> (String.sub arg 0 2) <> "--") args
