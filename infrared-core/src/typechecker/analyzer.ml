open Encoder

let check file =
  Printf.printf "checking.. %s\n" file;
  let ast = NativeEncoder.parse file in
  let open NativeEncoder.Util in
  let items = ast |> member "items" in
  try
    let _infrared_ast = InfraredEncoder.parse_items items in
    ()
  with
  | Unimplemented reason ->
    let message = Printf.sprintf
        "Tried to parse something we haven't implemented yet: `%s`\n" reason in
    Error_handler.(report message Level.High)
  | Malformed_json_ast reason ->
    let message = Printf.sprintf "Bad JSON ast: `%s`\n" reason in
    Error_handler.(report message Level.High)
