open Encoder
module StandardInfraredAst = InfraredAst.StandardInfraredAst

let pass (source : string) : unit =
  let source' = Printf.sprintf "\x1b[90m%s\x1b[39m" source in
  Printf.printf "\n\x1b[42;1m PASS \x1b[49;39;0m %s" source'

let check file =
  (* Printf.printf "[DEGUB] Checking  %s\n" file; *)
  let ast = NativeEncoder.parse file in
  let open NativeEncoder.Util in
  let items = ast |> member "items" in
  let fileName = ast |> member "fileName" |> to_string in
  try
    let stmts = InfraredEncoder.parse_items items ~fileName:fileName in
    (** @TODO extract imports/exports *)
    let ast =
      { StandardInfraredAst.
        imports = []
      ; exports = []
      ; statements = stmts }
    in
    print_endline ("\n\n" ^ (StandardInfraredAst.string_of_ast ast));
    pass fileName
  with
  | Unimplemented reason ->
    let message = Printf.sprintf
        "\tTried to parse something we haven't implemented yet\n\n\
         %s" reason in
    (Error_handler.report ~source:fileName ~msg:message ~level:Level.Med)
  | Malformed_json_ast reason ->
    let message = Printf.sprintf "Bad JSON ast: `%s`\n" reason in
    (Error_handler.report ~source:fileName ~msg:message ~level:Level.High)
