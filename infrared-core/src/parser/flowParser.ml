module FlowParser = Flow_parser.Parser_flow
module ParserEnv = Flow_parser.Parser_env

let parse_source source =
  let open ParserEnv in
  let parse_options =
    Some
      {
        esproposal_nullish_coalescing    = false;
        esproposal_optional_chaining     = false;
        esproposal_class_instance_fields = true;
        esproposal_class_static_fields   = true;
        esproposal_decorators            = true;
        esproposal_export_star_as        = true;
        types                            = false;
        types_in_comments                = false;
        use_strict                       = false;
      }
      FlowParser.program source, ~parse_options:parse_options;
