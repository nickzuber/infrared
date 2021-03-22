open InfraredParser
open InfraredCompiler
open InfraredUtils
open Ast

(* @TODO: Once we start working on the checker, this type will become useful.
 * @NOTE: I don't think we need to encode type "errors" in the typing routine.
 * Like a type contradiction isn't something we have a type for in our AST.
*)
type typing_result =
  | TypedProgram of string * program (* file, TypedInfraredProgram *)
  | ParsingError of parser_result

and parser_result =
  | Success of string * program
  | Fail of string * int * string (* file, InfraredParsingError *)
  | Nil of string

let parse_file (file : string) : parser_result =
  let open InfraredParser.Parser in
  let source = Fs.read_file file in
  try
    let prog = Parser.parse_source ~file ~source in
    Success (file, prog)
  with
  | InfraredParsingError (count, message) ->
    Fail (file, count, message)
  | _ ->
    Nil file

let print_result_summary start_time files err_files err_count : unit =
  let end_time = Unix.gettimeofday () in
  Printf.printf "\n%sChecked %s files in %ss"
    (Chalk.green " ↗ ")
    (files
     |> List.length
     |> string_of_int
     |> Chalk.green)
    (String.sub (string_of_float (end_time -. start_time)) 0 4);
  if err_files > 0 || err_count > 0 then
    Printf.printf "\n%sFailed to parse %s files with %s errors"
      (Chalk.red " ↘ ")
      (err_files
       |> string_of_int
       |> Chalk.red)
      (err_count
       |> string_of_int
       |> Chalk.red);
  Printf.printf "\n\n"

let check_file (file : string) : parser_result =
  let parsing_output = parse_file file in
  let _typed_program : typing_result = match parsing_output with
    | Success (file, program) ->
      let typed_program = Compiler.assign_types ~file ~program in
      TypedProgram (file, typed_program)
    | _ -> ParsingError parsing_output
  in
  (* @TODO: this should eventually return both parsing errors and
   * type checking errors and results. *)
  parsing_output

let string_of_parser_result (res : parser_result) : string =
  let open Chalk in
  match res with
  | Success (file, _prog) ->
    Printf.sprintf "%s %s\n"
      (" Pass " |> green |> bold)
      (gray file)
  | Fail (file, _count, message) ->
    let failure = Printf.sprintf "%s %s\n"
        (" Fail " |> red |> bold)
        (gray file)
    in
    Printf.sprintf "%s%s\n" failure message
  | Nil file ->
    Printf.sprintf "%s %s\n"
      (" Fatal " |> red |> bold)
      (gray file)

let check_files (files : string list) : unit =
  let start_time = Unix.gettimeofday () in
  let results = List.map (fun file -> check_file file) files in
  let result_strings = List.map string_of_parser_result results in
  (* @TODO: This is printing the parsing results, we'd want to print the
   * type checking results here isntead eventually. *)
  let () = List.iter (fun str -> Printf.printf "%s" str) result_strings in
  (* @TODO: Count up all of the failed parsed files and errors. We'll want to do
   * the same thing for type checking results too eventually. *)
  let (err_files, err_count) = List.fold_left (fun acc res ->
      match res with
      | Fail (_file, count, _message) ->
        let (acc_f, acc_c) = acc in
        (acc_f + 1, acc_c + count)
      | Nil _file ->
        let (acc_f, acc_c) = acc in
        (acc_f + 1, acc_c)
      | _ -> acc)
      (0, 0) results
  in
  print_result_summary start_time files err_files err_count

let type_check args : unit =
  print_endline "";
  match args with
  | [] -> ()
  | arg :: [] ->
    let paths =
      arg
      |> Fs.files_from_path
      |> List.rev
    in
    check_files paths
  | args ->
    let paths =
      args
      |> List.map Fs.files_from_path
      |> List.flatten
      |> List.rev
    in
    check_files paths

let spec = Command.create
    ~name:"check"
    ~aliases:["ch"]
    ~doc:"Type check the given JavaScript files"
    ~flags:[]

let exec = type_check

type t =
  { spec: Command.t
  ; exec: string list -> unit
  }

let command : t =
  { spec = spec
  ; exec = exec
  }
