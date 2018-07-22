open Yojson

module NativeEncoder : sig
  include module type of Yojson.Basic
  val parse : string -> Yojson.Basic.json
end = struct
  include Yojson.Basic
  let parse file = Basic.from_file file
end
