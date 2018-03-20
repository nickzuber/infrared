
open Batteries
open Ast
open Lex_env
module Program_parser = Token_parser.Program

let tokenize file =
  let input = open_in file in
  let filebuf = Lexing.from_input input in
  let env = { Lex_env.new_env with source = file; } in
  let final_env = Lexer.token env filebuf in
  IO.close_in input;
  (* debug final_env *)
  if List.length final_env.state > 1 then begin
    let source = "\x1b[90m" ^ file ^ "\x1b[39m" in
    let msg = "\n   We've found some unbalanced brackets or parenthesis.\n" in
    let lvl = Level.SyntaxError in
    (Error_handler.report ~msg:(source ^ " " ^ msg) ~level:(lvl));
    None
  end else begin
    match final_env.error with
    (* might be unused *)
    | Some (msg, lvl) ->
      let msg = "\n   " ^ msg ^ "\n" in
      (Error_handler.report ~msg:(msg) ~level:(lvl));
      None
    | None -> Some final_env
  end

let print_single_token_list env =
  Printf.printf "FILE:\t%s\nTOKENS:\n" env.source;
  List.iter (fun tok ->
      Printf.printf "%s\n"
        (Token.full_token_to_string ~indent:2 tok))
    (List.rev env.token_list)

let print_tokens envs =
  List.iter (fun env -> print_single_token_list env) envs

let print_ast asts = Parser_env.(
    List.iter (fun ast -> print_single_ast ast) asts)

let print_typecheck asts_and_path =
  let ast, path = asts_and_path in
  let path = "\x1b[90m" ^ path ^ "\x1b[39m" in
  Printf.printf "✨  \x1b[32mLooks good %s\x1b[37m\n" path

let parse_may_throw file =
  let maybe_lex_env = tokenize file in
  match maybe_lex_env with
  | Some lex_env -> begin
      (* We don't do anything with comments at the moment and I highly doubt we ever will,
       * so remove them so they don't get in our way when parsing *)
      let token_list = lex_env.token_list
                       |> List.rev
                       |> List.filter (fun token -> Token.(token.body <> Comment))
      in Some (Program_parser.parse token_list lex_env.source)
    end
  | None -> None

(* We still want parse to return and continue in our program.
   Catch parsing errors here, report, and move on. *)
let parse file = Token_parser.(
    try
      parse_may_throw file
    with
    | Unimplemented e -> Error_handler.(report e Level.UnknownError); None
    | ParsingError e -> Error_handler.(report e Level.ParseError); None)
