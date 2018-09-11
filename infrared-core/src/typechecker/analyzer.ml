open Encoder

let check file =
  Printf.printf "checking.. %s\n" file;
  let ast = NativeEncoder.parse file in
  let open NativeEncoder.Util in
  let items = ast |> member "items" in
  let fileName = ast |> member "fileName" |> to_string in
  try
    let stmts = InfraredEncoder.parse_items items ~fileName:fileName in
    (** @TODO extract imports/exports *)
    let ast =
      { InfraredAst.
        imports = []
      ; exports = []
      ; statements = stmts }
    in
    print_endline (InfraredAst.string_of_ast ast)
  with
  | Unimplemented reason ->
    let message = Printf.sprintf
        "\tTried to parse something we haven't implemented yet\n\n\
         %s" reason in
    (Error_handler.report ~source:fileName ~msg:message ~level:Level.Med)
  | Malformed_json_ast reason ->
    let message = Printf.sprintf "Bad JSON ast: `%s`\n" reason in
    (Error_handler.report ~source:fileName ~msg:message ~level:Level.High)
