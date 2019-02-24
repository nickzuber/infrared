

let () =
  let parse_source ~types_in_comments source =
    let parse_options = Some Flow_parser.Parser_env.({
        esproposal_optional_chaining = false;
        esproposal_class_instance_fields = true;
        esproposal_class_static_fields = true;
        esproposal_decorators = true;
        esproposal_export_star_as = true;
        types = true;
        types_in_comments;
        use_strict = false;
      })
    in
    Flow_parser.Parser_flow.program source ~parse_options
  in
  let source = "let x = 1; /* : */" in
  let _ = parse_source ~types_in_comments:false source in
  print_endline "types_in_comments=false: OK";
  let () = try
    parse_source ~types_in_comments:true source |> ignore
  with
  | Flow_parser.Parse_error.Error _ ->
    print_endline "types_in_comments=true: OK";
  in
  ()
