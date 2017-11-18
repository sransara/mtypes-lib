type message =
    | PopQ
    | PoppedQ of int
    | Store of int
    | All of int list
    | Stored of int
    | Error of string

let string_to_message x =
  let parts = String.split_on_char ' ' x   in
  match parts with
  | ["PopQ"] -> PopQ
  | ["PoppedQ"; x] ->  PoppedQ (int_of_string x)
  | ["Store"; x] -> Store (int_of_string x)
  | ["Stored"; x] -> Stored (int_of_string x)
  | ["All"; xs] ->
    let xlist = List.map (fun x -> int_of_string x) (String.split_on_char ',' xs) in
    All xlist
  | ["Error"; x] -> Error x
  | _ -> Error "string_to_message: error"

let message_to_string x =
  let string_of_list f l = 
  "[ " ^ List.fold_left (fun a b -> a ^ (f b) ^ "; ") "" l ^ "]" in
  match x with
  | PopQ -> "PopQ"
  | PoppedQ x -> Printf.sprintf "PoppedQ %d" x
  | Store x -> Printf.sprintf "Store %d" x
  | Stored x -> Printf.sprintf "Stored %d" x
  | All xs -> Printf.sprintf "All %s" (string_of_list string_of_int xs)
  | Error x -> Printf.sprintf "Stored %s" x

