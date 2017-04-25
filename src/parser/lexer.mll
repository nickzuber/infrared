
{
(* Pretty dumb Loc definition, make this better by using Lexing.lexbuf positions *)
module rec Loc : sig
  type t = {
    pos_start: int;
    pos_end: int;
  }
end = Loc

module Token = struct
  type t = {
    loc: Loc.t;
    body: t';
  }

  (* These keywords are organized alphabetically rather than by relevant
   * association, and separated by their specifications. Keep that in mind when
   * looking for particular keywords to add or change.
   * https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Lexical_grammar *)
  and t' = 
    (* Custom *)
    (* Like Expression but with curly brackets *)
    | Block of t' list 
    (* Like Block but with parethesis *)
    | Expression of t' list 
    (* Words like the names of functions or variables, not to be confused with Strings *)
    | Identifier of string
    | Bool
    (* Surrounded by quotes, not to be confused with Identifiers *)
    | String of string
    | Number
    | Variable of var_t
    | Assignment
    | Equality 
    (* Possibly same as `Equality` if we disallow implicit coersion *)
    | StrictEquality 
    (* Standard *)
    | Break
    | Case
    | Catch
    | Comment
    | Continue
    | Debugger
    | Default
    | Delete
    | Do
    | Else
    | Export
    | Extends
    | Finally
    | For
    (* Bind function names in env while parsing 
     * Ex:
       * Function Identifier "Foo", Expression ( ... ), Block ( ... ) ....
       * or 
       * Function Expression ( ... ) Block ( ... ) ....
     * *)
    | Function
    | If
    | In
    | Instanceof
    | New
    | Null
    | Return
    | Super
    | Switch
    | This
    | Throw
    | Try
    | Typeof
    | Void
    | While
    | With
    | Yield
    (* ES6 *)
    | Class
    | Implements
    | Import
    | Spread
    | TemplateString
    | Rest
    (* ES7 *)
    | Async
    | Await
    (* Miscellaneous & FutureReserved *)
    | Enum
    | Interface
    | Package
    | Private
    | Protected
    | Public
    | Static
    | Eof

  and var_t = 
    (* Standard *)
    | Var
    (* ES6 *)
    | Let
    | Const

  let var_to_string = function
    | Var -> "Var"
    | Let -> "Let"
    | Const -> "Const"
  
  let rec token_to_string tok =
    match tok with
    | Block content -> (
      Printf.sprintf "Block (%s)"
        (List.fold_left (fun acc e -> 
          match acc with
          | "" -> acc ^ (token_to_string e)
          | _ -> Printf.sprintf "%s, %s" acc (token_to_string e))
        "" content))
    | Variable t -> 
      Printf.sprintf "Variable (%s)"
        (var_to_string t)
    | Expression expr -> (
      Printf.sprintf "Expression (%s)"
        (List.fold_left (fun acc e -> 
          match acc with
          | "" -> acc ^ (token_to_string e)
          | _ -> Printf.sprintf "%s, %s" acc (token_to_string e))
        "" expr))
    | Number -> "Number"
    | Bool -> "Bool"
    | String str -> Printf.sprintf "String (%s)" str
    | Comment -> "Comment"
    | Break -> "Break"
    | Case -> "Case"
    | Catch -> "Catch"
    | Continue -> "Continue"
    | Debugger -> "Debugger"
    | Default -> "Default"
    | Delete -> "Delete"
    | Do -> "Do"
    | Else -> "Else"
    | Export -> "Exports"
    | Extends -> "Extends"
    | Finally -> "Finally"
    | For -> "For"
    | Function -> "Function"
    | If -> "If"
    | Import -> "Import"
    | In -> "In"
    | Instanceof -> "Instanceof"
    | New -> "New"
    | Return -> "Return"
    | Super -> "Super"
    | Switch -> "Switch"
    | This -> "This"
    | Throw -> "Throw"
    | Try -> "Try"
    | Typeof -> "Typeof"
    | Void -> "Void"
    | While -> "While"
    | With -> "With"
    | Yield -> "Yields"
    | Class -> "Class"
    | Implements -> "Implements"
    | Spread -> "Spread"
    | TemplateString -> "TemplateString"
    | Rest -> "Rest"
    | Async -> "Async"
    | Await -> "Await"
    | Enum -> "Enum"
    | Interface -> "Interface"
    | Package -> "Package"
    | Private -> "Private"
    | Protected -> "Protected"
    | Public -> "Public"
    | Static -> "Static"
    | Eof -> "Eof"
    | Null -> "Null"
    | Assignment -> "Assignment"
    | Equality -> "Equality"
    | StrictEquality -> "StrictEquality"
    | Identifier str -> Printf.sprintf "Identifier \"%s\"" str

  (* List of faux keywords that need to be checked separately:
   *  - Spread
   *  - TemplateString
   *  - Rest
   *)
  let keywords = Hashtbl.create 53
  let _ = List.iter (fun (kwd, tok) -> Hashtbl.add keywords kwd tok) 
    [ "break", Break;
      "case", Case;
      "catch", Catch;
      "continue", Continue;
      "debugger", Debugger;
      "default", Default;
      "delete", Delete;
      "do", Do;
      "else", Else;
      "export", Export;
      "extends", Extends;
      "finally", Finally;
      "for", For;
      "function", Function;
      "if", If;
      "in", In;
      "instanceof", Instanceof;
      "new", New;
      "null", Null;
      "return", Return;
      "super", Super;
      "this", This;
      "throw", Throw;
      "try", Try;
      "typeof", Typeof;
      "void", Void;
      "while", While;
      "with", With;
   (* "yield", Yield; *)
      "class", Class;
   (* "implements", Implements; *)
      "import", Import;
   (* "async", Async; *)
   (* "await", Await; *)
      "enum", Enum;
   (* "interface", Interface; *)
   (* "package", Package; *)
   (* "private", Private; *)
   (* "protected", Protected; *)
   (* "public", Public; *)
   (* "static", Static; *)
      "var", (Variable Var);
      "let", (Variable Let);
      "const", (Variable Const);  ]

  let full_token_to_string tok =
    let open Loc in
    Printf.sprintf "%d:%d %s"
      tok.loc.pos_start
      tok.loc.pos_end
      (token_to_string tok.body)

  let lazy_token_to_string tok =
    let open Loc in
    token_to_string tok.body

end
open Token

module Lex_env = struct
  type t = {
    (* Meta *)
    source: string;
    is_in_comment: bool;
    (* States *)
    state: state_t;
    expr: Token.t list;
    expr_buffers: Token.t list Utils.Stack.t;
    (* The ast is "backwards" -- newest token is 
     * inserted into the front of the list. 
     * TODO: Consider using a queue? *)
    ast: Token.t list;
  }

  and state_t = 
    | S_REGULAR
    | S_CLOSURE_START
    | S_CLOSURE_INSIDE
    | S_CLOSURE_END

  let state_to_string = function
    | S_REGULAR -> "S_REGULAR"
    | S_CLOSURE_START -> "S_CLOSURE_START"
    | S_CLOSURE_INSIDE -> "S_CLOSURE_INSIDE"
    | S_CLOSURE_END -> "S_CLOSURE_END"

  let defaultEnv = { 
    source = "Null";
    is_in_comment = false;
    state = S_REGULAR;
    expr = [];
    expr_buffers = Utils.Stack.create [];
    ast = [];
  }

  let dress body env = 
    let open Loc in
    let loc = { pos_start = 0; pos_end = 0; } in      
    { loc; body; }

  let push tok env =
    let tok = dress tok env in
    match env.state with
    | _ -> { 
        env with 
        ast = tok::env.ast;
      }
end 
open Lex_env
}

(* Different ways you can write a number 
   https://github.com/facebook/flow/blob/master/src/parser/lexer_flow.mll#L755 *)
let hex = ['0'-'9''a'-'f''A'-'F']
let binnumber = '0' ['B''b'] ['0''1']+
let hexnumber = '0' ['X''x'] hex+
let octnumber = '0' ['O''o'] ['0'-'7']+
let legacyoctnumber = '0' ['0'-'7']+
let scinumber = ['0'-'9']*'.'?['0'-'9']+['e''E']['-''+']?['0'-'9']+
let digit = ['0'-'9']
let wholenumber = digit+'.'?
let floatnumber = ['0'-'9']*'.'['0'-'9']+

let number = hex | binnumber | hexnumber | octnumber | legacyoctnumber |
             scinumber | wholenumber | floatnumber
let whitespace = [' ' '\t' '\r' '\n']
let letter = ['a'-'z''A'-'Z''_''$']
let alphanumeric = digit | letter

let word = letter alphanumeric*

rule token env = parse
  | whitespace+ | ';' { token env lexbuf }
  | word as word      {
                        try
                          let env = push (Hashtbl.find keywords word) env in
                          token env lexbuf
                        with Not_found -> 
                          let env = push (Identifier word) env in
                          token env lexbuf
                      }
  | number            {
                        let env = push Number env in
                        token env lexbuf
                      } 
  | '='               {
                        let env = push Assignment env in
                        token env lexbuf
                      }
  | "=="              {
                        let env = push Equality env in
                        token env lexbuf
                      }

  | "==="              {
                        let env = push StrictEquality env in
                        token env lexbuf
                      }
  | eof               { env }







