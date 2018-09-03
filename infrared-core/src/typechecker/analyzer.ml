open Encoder

let check file =
  Printf.printf "checking.. %s\n" file;
  let ast = NativeEncoder.parse file in
  let open NativeEncoder.Util in
  let items = ast |> member "items" in
  let fileName = ast |> member "fileName" |> to_string in
  try
    let _infrared_ast = InfraredEncoder.parse_items items ~fileName:fileName in
    ()
  with
  | Unimplemented reason ->
    let message = Printf.sprintf
        "Tried to parse something we haven't implemented yet\n\n\
         \x1b[31m  ● \x1b[39m\x1b[1m%s\x1b[0m" reason in
    Error_handler.(report message Level.High)
  | Malformed_json_ast reason ->
    let message = Printf.sprintf "Bad JSON ast: `%s`\n" reason in
    Error_handler.(report message Level.High)
