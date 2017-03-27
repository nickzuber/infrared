
module Token = Lexer.Token
module Lex_env = Lexer.Lex_env

module Infrared_ast = struct
  (* Tentative *)
  type t = 
    | Binop of (t * t)
    | Variable
    | Null

  let defaultEnv = {
    Lex_env.
    file = "undefined";
    line_number = 1;
    is_in_comment = false;
    state = REGULAR;
    expr = [];
    body_builders = [];
    ast = [];
  }
end

