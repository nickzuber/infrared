
let spec = CommandSpec.create
  ~name:"check"
  ~doc:""
  ~flags:[
    Flags
  ]

let exec = generate_ast_list;

