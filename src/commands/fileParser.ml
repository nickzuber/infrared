
let get_files_from_args args =
  List.fold_left (fun acc arg ->
    let response = Fs.extract_files arg in
    match response with
    | Some paths -> acc @ (Fs.extract_whitelisted_files paths)
    | None -> acc
  ) [] args
