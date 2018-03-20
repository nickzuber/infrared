
type t = {
  flag: string;
  doc: string;
}

let create ~flag ~doc = {
  flag = flag;
  doc = doc;
}

let create_list l =
  List.fold_left (fun acc tup ->
      let (flag, doc) = tup in
      let flag' = create ~flag:(flag) ~doc:(doc) in
      flag' :: acc
    ) [] l
