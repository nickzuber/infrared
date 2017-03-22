
{
module Token = struct

  (* These keywords are organized alphabetically rather than by relevant
   * association, and separated by their specifications. Keep that in mind when
   * looking for particular keywords to add or change.
   * https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Lexical_grammar *)
  type t = 
    (* Standard *)
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
    (* Bind function names in env while parsing *)
    | Function of t 
    | If
    | Import
    | In
    | Instanceof
    | New
    | Null
    | Number
    | Return
    | String
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

  type var_t = 
    (* Standard *)
    | Var
    (* ES6 *)
    | Let
    | Const

  type state_t = 
    | REGULAR
    | CLOSURE_START
    | CLOSURE_END

  type env_t = {
    mutable state: state_t list;
    mutable exprs: t list;
    mutable body_builders: t list list;
    mutable ast: t list;
  }

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

  let state_to_string = function
    | 

end
open Token
}

rule token env = parse
  | '\n'

