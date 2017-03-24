
{
module Token = struct

  (* These keywords are organized alphabetically rather than by relevant
   * association, and separated by their specifications. Keep that in mind when
   * looking for particular keywords to add or change.
   * https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Lexical_grammar *)
  type t = 
    (* Standard & Custom *)
    | Block of t list (* Like Expression but with curly brackets *)
    | Bool
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
    | Expression of t list (* Like Block but with parethesis *)
    | Export
    | Extends
    | Finally
    | For
    (* Bind function names in env while parsing 
     * Ex:
       * Function (String "Foo", Block ( ... )) ....
       * or 
       * Function (Block ( ... )) ....
     * *)
    | Function of t 
    (* Words like the names of functions or variables, not to be confused with Strings *)
    | Identifier of string
    | If
    | Import
    | In
    | Instanceof
    | New
    | Null
    | Number
    | Return
    (* Surrounded by quotes, not to be confused with Identifiers *)
    | String of string
    | Super
    | Switch
    | This
    | Throw
    | Try
    | Typeof
    (* All variables have identifers so we mark that in token *)
    | Variable of string * (var_t * t)
    | Void
    | While
    | With
    | Yield
    (* ES6 *)
    | Class
    | Implements
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
 
  let rec token_to_string tok =
    match tok with
    | Block content -> (
      Printf.sprintf "Block (%s)"
        List.fold_left (fun acc e -> 
          match acc with
          | "" -> acc ^ (token_to_string e)
          | _ -> Prinft.sprintf "%s, %s" acc (token_to_string e))
        "" content)
    | Variable var -> (
      let (var_t, value) = var in
      Printf.sprintf "Variable (%s: %s)"
        (token_to_string var_t)
        (token_to_string value))
    | Expression expr -> (
      Printf.sprintf "Expression (%s)"
        List.fold_left (fun acc e -> 
          match acc with
          | "" -> acc ^ (token_to_string e)
          | _ -> Prinft.sprintf "%s, %s" acc (token_to_string e))
        "" expr)
    | Number -> "Number"
    | Bool -> "Bool"
    | String -> "String"
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
end
open Token

module Lex_env = struct
  type t = {
    (* Current state of the Lexer *)
    state: state_t;
    (*  *)
    expr: Token.t list;
    (*  *)
    body_builders: Token.t list list;
    (*  *)
    ast: Token.t list;
  }

  and state_t = 
    | REGULAR
    | CLOSURE_START
    | CLOSURE_INSIDE
    | CLOSURE_END

  let state_to_string = function
    | REGULAR -> "REGULAR"
    | CLOSURE_START -> "CLOSURE_START"
    | CLOSURE_INSIDE -> "CLOSURE_INSIDE"
    | CLOSURE_END -> "CLOSURE_END"
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
let wholenumber = ['0'-'9']+'.'?
let floatnumber = ['0'-'9']*'.'['0'-'9']+

let number = hex | binnumber | hexnumber | octnumber | legacyoctnumber |
             scinumber | wholenumber | floatnumber
let whitespace = [' ' '\t' '\r']
let letter = ['a'-'z''A'-'Z''_''$']

rule token env = parse
  | [' ' '\t' '\n']   {
                        token env lexbuf
                      }
  | ['\n']            {
                        (**)
                      }
  | ['\n']            {
                        (**)
                      }







