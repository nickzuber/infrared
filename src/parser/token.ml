
open Loc

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
  | Block of t list 
  (* Like Block but with parethesis *)
  | Expression of t list 
  (* Like Block/Expression but with square brackets *)
  | Array of t list
  (* Words like the names of functions or variables, not to be confused with Strings *)
  | Identifier of string
  | Bool
  (* Surrounded by quotes, not to be confused with Identifiers *)
  | String of string
  | Number
  | Variable of var_t
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
  (* Template string w/ list of TokenTrees *)
  | TemplateString of string * t list list
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
  | Operator of ops
  (* Error handling *)
  | Syntax_Error of string
  | Unknown_Token of string
  (* End of tokens/file *)
  | Empty_Token

and ops =
  (* Compound Assignment Operators *)
  | CA_Plus                (*    +=    *)
  | CA_Minus               (*    -=    *)
  | CA_Mult                (*    *=    *)
  | CA_Div                 (*    /=    *)
  | CA_Mod                 (*    %=    *)
  | CA_Pow                 (*    **=   *)
  | CA_LeftShift           (*    <<=   *)
  | CA_RightShift          (*    >>=   *)
  | CA_RightShiftUnsigned  (*    >>>=  *)
  | CA_Or                  (*    |=    *)
  | CA_Xor                 (*    ^=    *)
  | CA_And                 (*    &=    *)
  (* Binary Operators *)
  | Equal                  (*    ==    *)
  | NotEqual               (*    !=    *)
  | StrictEqual            (*    ===   *)
  | StrictNotEqual         (*    !==   *)
  | LessThan               (*    <     *)
  | LessThanEqual          (*    <=    *)
  | GreaterThan            (*    >     *)
  | GreaterThanEqual       (*    >=    *)
  | LeftShift              (*    <<    *)
  | RightShift             (*    >>    *)
  | RightShiftUnsigned     (*    >>>   *)
  | Plus                   (*    +     *)
  | Minus                  (*    -     *)
  | Mult                   (*    *     *)
  | Div                    (*    /     *)
  | Mod                    (*    %     *)
  | Pow                    (*    **    *)
  | Comma                  (*    ,     *)
  | LogicalOr              (*    ||    *)
  | LogicalAnd             (*    &&    *)
  | Or                     (*    |     *)
  | Xor                    (*    ^     *)
  | And                    (*    &     *)
  | Bang                   (*    !     *)
  | Not                    (*    ~     *)
  | Increment              (*    ++    *)
  | Decrement              (*    --    *)
  | Dot                    (*    .     *)
  | Colon                  (*    :     *)
  | Ternary                (*    ?     *)
  | Assignment             (*    =     *)

and var_t = 
  (* Standard *)
  | Var
  (* ES6 *)
  | Let
  | Const

let rec token_to_string tok i =
  let indentation = String.make i ' ' in
  let step = 4 in
  match tok with
  | Block content -> (
    if List.length content = 0 
      then indentation ^ "Block {}"
      else Printf.sprintf "%sBlock {\n%s%s}"
        indentation
        (List.fold_left 
          (fun acc e -> 
            Printf.sprintf "%s%s\n"
            acc
            (full_token_to_string ~indent:(i + step) e))
        "" content)
        indentation)
  | Expression content -> (
    if List.length content = 0 
      then indentation ^ "Expression ()"
      else Printf.sprintf "%sExpression (\n%s%s)"
        indentation
        (List.fold_left 
          (fun acc e -> 
            Printf.sprintf "%s%s\n"
            acc
            (full_token_to_string ~indent:(i + step) e))
        "" content)
        indentation)
  | Array content -> (
    if List.length content = 0 
      then indentation ^ "Array []"
      else Printf.sprintf "%sArray [\n%s%s]"
        indentation
        (List.fold_left 
          (fun acc e -> 
            Printf.sprintf "%s%s\n"
            acc
            (full_token_to_string ~indent:(i + step) e))
        "" content)
        indentation)
  | Variable t -> indentation ^ "Variable " ^ (var_to_string t)
  | Number -> indentation ^ "Number"
  | Bool -> indentation ^ "Bool"
  | String str -> indentation ^ "String " ^ str
  | Comment -> indentation ^ "Comment"
  | Break -> indentation ^ "Break"
  | Case -> indentation ^ "Case"
  | Catch -> indentation ^ "Catch"
  | Continue -> indentation ^ "Continue"
  | Debugger -> indentation ^ "Debugger"
  | Default -> indentation ^ "Default"
  | Delete -> indentation ^ "Delete"
  | Do -> indentation ^ "Do"
  | Else -> indentation ^ "Else"
  | Export -> indentation ^ "Exports"
  | Extends -> indentation ^ "Extends"
  | Finally -> indentation ^ "Finally"
  | For -> indentation ^ "For"
  | Function -> indentation ^ "Function"
  | If -> indentation ^ "If"
  | Import -> indentation ^ "Import"
  | In -> indentation ^ "In"
  | Instanceof -> indentation ^ "Instanceof"
  | New -> indentation ^ "New"
  | Return -> indentation ^ "Return"
  | Super -> indentation ^ "Super"
  | Switch -> indentation ^ "Switch"
  | This -> indentation ^ "This"
  | Throw -> indentation ^ "Throw"
  | Try -> indentation ^ "Try"
  | Typeof -> indentation ^ "Typeof"
  | Void -> indentation ^ "Void"
  | While -> indentation ^ "While"
  | With -> indentation ^ "With"
  | Yield -> indentation ^ "Yields"
  | Class -> indentation ^ "Class"
  | Implements -> indentation ^ "Implements"
  | Spread -> indentation ^ "Spread"
  | TemplateString (str, tok_lists) -> (
    if List.length tok_lists = 0 
      then Printf.sprintf "%sTemplateString `%s` {}" indentation str
      else Printf.sprintf "%sTemplateString `%s` {\n%s%s}"
        indentation
        str
        (List.fold_left 
          (fun acc toks -> 
            Printf.sprintf "%s%s\n"
            acc
            (List.fold_left 
              (fun acc e -> 
                Printf.sprintf "%s%s\n"
                acc
                (full_token_to_string ~indent:(i + step) e))
            "" toks))
        "" tok_lists)
        indentation)
  | Async -> indentation ^ "Async"
  | Await -> indentation ^ "Await"
  | Enum -> indentation ^ "Enum"
  | Interface -> indentation ^ "Interface"
  | Package -> indentation ^ "Package"
  | Private -> indentation ^ "Private"
  | Protected -> indentation ^ "Protected"
  | Public -> indentation ^ "Public"
  | Static -> indentation ^ "Static"
  | Null -> indentation ^ "Null"
  | Identifier str -> indentation ^ "Identifier: " ^ str
  | Operator op -> indentation ^ "Operator: " ^ (op_to_string op)
  (* Error Handling *)
  | Unknown_Token str -> Printf.sprintf "%s\x1b[31mUnknown_Token: %s \x1b[39m" indentation str
  | Syntax_Error str -> Printf.sprintf "%s\x1b[31mSyntax_Error: %s \x1b[39m" indentation str
  | Empty_Token -> "Empty_Token"

and op_to_string = function
  (* Operators *)
  | CA_Plus -> "CA_Plus"
  | CA_Minus -> "CA_Minus"
  | CA_Mult -> "CA_Mult"
  | CA_Div -> "CA_Div"
  | CA_Mod -> "CA_Mod"
  | CA_Pow -> "CA_Pow"
  | CA_LeftShift -> "CA_LeftShift"
  | CA_RightShift -> "CA_RightShift"
  | CA_RightShiftUnsigned -> "CA_RightShiftUnsigned"
  | CA_Or -> "CA_Or"
  | CA_Xor -> "CA_Xor"
  | CA_And -> "CA_And"
  | Equal -> "Equal"
  | NotEqual -> "NotEqual"
  | StrictEqual -> "StrictEqual"
  | StrictNotEqual -> "StrictNotEqual"
  | LessThan -> "LessThan"
  | LessThanEqual -> "LessThanEqual"
  | GreaterThan -> "GreaterThan"
  | GreaterThanEqual -> "GreaterThanEqual"
  | LeftShift -> "LeftShift"
  | RightShift -> "RightShift"
  | RightShiftUnsigned -> "RightShiftUnsigned"
  | Plus -> "Plus"
  | Minus -> "Minus"
  | Mult -> "Mult"
  | Div -> "Div"
  | Mod -> "Mod"
  | Pow -> "Pow"
  | Comma -> "Comma"
  | LogicalOr -> "LogicalOr"
  | LogicalAnd -> "LogicalAnd"
  | Or -> "Or"
  | Xor -> "Xor"
  | And -> "And"
  | Bang -> "Bang"
  | Not -> "Not"
  | Increment -> "Increment"
  | Decrement -> "Decrement"
  | Dot -> "Dot"
  | Colon -> "Colon"
  | Ternary -> "Ternary"
  | Assignment -> "Assignment"

and var_to_string = function
  | Var -> "Var"
  | Let -> "Let"
  | Const -> "Const"

and full_token_to_string ?(indent=0) tok =
  let open Loc in
  Printf.sprintf "%s \x1b[90m(%d:%d)\x1b[39m"
    (token_to_string tok.body indent)
    tok.loc.line
    tok.loc.column

and lazy_token_to_string tok =
  let open Loc in
  token_to_string tok.body 0

(* @NOTE @TODO
  * If we want to identify these complex operators, we can't use this
  * hashtable trick. We need to explicitly check each complex operator
  * manually in `token` *)
let operators = Hashtbl.create 53
let _ = List.iter (fun (sym, tok) -> Hashtbl.add operators sym tok) 
  [
    '+', Plus;
    '-', Minus;
    '*', Mult;
    '/', Div;
    '%', Mod;
    '<', LessThan;
    '>', GreaterThan;
    ',', Comma;
    '|', Or;
    '^', Xor;
    '&', And;
    '!', Bang;
    '~', Not;
    '.', Dot;
    ':', Colon;
    '?', Ternary;
    '=', Assignment;
  ]

(* List of faux keywords that need to be checked separately:
  *  - Spread (Rest)
  *  - TemplateString
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
    "switch", Switch;
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
