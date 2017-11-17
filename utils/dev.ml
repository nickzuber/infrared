
let __DEV__ = false

open Token

let mock_loc = { Loc.
  line = 0;
  column = 0;
  length = 0;
}
let mock_token = { Token.
  loc = mock_loc;
  body = Empty_Token;
}

let __debug__ ?(token=mock_token) token_list call_source =
  if __DEV__ <> true then () else
  let token_string = lazy_token_to_string token in
  let line = String.make ((String.length call_source) + 2) '-' in
  Printf.printf "\x1b[90m ┌%s┐\n" line;
  Printf.printf "\x1b[90m┌┤ %s │\n" call_source;
  Printf.printf "\x1b[90m│└%s┘\n" line;
  Printf.printf "\x1b[90m├─ TOKEN      := \x1b[35m%s\n" token_string;
  Printf.printf "\x1b[90m└─ TOKEN_LIST := \x1b[39m";
  let tls = List.fold_left (fun acc x ->
    let s = lazy_token_to_string x in
    let ss = s ^ "\n\t\t "
    in acc ^ ss) ""
    token_list
  in Printf.printf "\x1b[33m%s\x1b[90m\x1b[39m\n\n" tls;
