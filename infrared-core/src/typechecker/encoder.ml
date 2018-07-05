open Yojson

module NativeEncoder : sig
  val parse : string -> Yojson.Basic.json
  val member : Yojson.Basic.json -> string -> Yojson.Basic.json
  val to_string : Yojson.Basic.json -> string
  val to_int : Yojson.Basic.json -> int
  val to_bool : Yojson.Basic.json -> bool
  val to_bool_option : Yojson.Basic.json -> bool option
  val to_list : Yojson.Basic.json -> Yojson.Basic.json list
end = struct
  let parse file = Basic.from_file file

  let member json key =
    let open Basic.Util in
    json |> member key

  let to_string item =
    let open Basic.Util in
    item |> to_string

  let to_int item =
    let open Basic.Util in
    item |> to_int

  let to_bool item =
    let open Basic.Util in
    item |> to_bool

  let to_bool_option item =
    let open Basic.Util in
    item |> to_bool_option

  let to_list item =
    let open Basic.Util in
    item |> to_list
end
