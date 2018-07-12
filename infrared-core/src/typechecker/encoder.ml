open Yojson

module NativeEncoder = struct
  include Yojson.Basic
  let parse file = Basic.from_file file
end
