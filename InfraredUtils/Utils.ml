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
