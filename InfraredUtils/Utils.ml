let is_substring (needle : string) (haystack : string) =
  let rgx_string = Printf.sprintf ".*\\(%s\\).*" needle in
  let rgx = Str.regexp rgx_string in
  Str.string_match rgx haystack 0
