open FlowError

exception InfraredParsingError of int * string

let parse_source ~(file : string) ~(source : string) =
  let parse_options = Some Flow_parser.Parser_env.({
      esproposal_optional_chaining = false;
      esproposal_class_instance_fields = true;
      esproposal_class_static_fields = true;
      esproposal_decorators = true;
      esproposal_export_star_as = true;
      types = true;
      types_in_comments = false;
      use_strict = false;
    })
  in
  try
    Flow_parser.Parser_flow.program source ~parse_options
  with
  | Flow_parser.Parse_error.Error errs ->
    let message = FlowError.string_of_errors_in_file file errs in
    let count = List.length errs in
    raise (InfraredParsingError (count, message))
