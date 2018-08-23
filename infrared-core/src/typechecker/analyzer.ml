open Encoder

let check file =
  Printf.printf "checking.. %s\n" file;
  let parsetree = NativeEncoder.parse file in
  (* let open NativeEncoder in *)
  let open NativeEncoder.Util in
  let ast = parsetree |> member "tree" in
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

(* let directives = ast |> member "directives" |> to_list in
   let t = List.hd directives |> member "type" |> to_string in
   print_endline ("\n--> " ^ t) *)
