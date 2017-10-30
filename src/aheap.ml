(* Pairing Heap *)

type 'a t = E | T of 'a * 'a t list

let empty = E
let is_empty h = h = E

let merge cmp h1 h2 = match h1, h2 with
  | _, E -> h1
  | E, _ -> h2
  | T (x, hs1), T (y, hs2) ->
    if cmp x y <= 0 then T (x, h2 :: hs1)
    else T (y, h1 :: hs2)

let insert cmp x h = merge cmp (T (x, [])) h

let rec merge_pairs cmp = function
  | [] -> E
  | [h] -> h
  | h1 :: h2 :: hs -> merge cmp (merge cmp h1 h2) (merge_pairs cmp hs)

exception Empty

let find_min = function
  | E -> raise Empty
  | T (x, _) -> x

let delete_min cmp = function
  | E -> raise Empty
  | T (m, hs) -> m, merge_pairs cmp hs

let rec fold_u f acc t =
  let rec loop acc to_visit =
    match to_visit with
    | [] -> acc
    | E :: rest ->
      loop acc rest
    | T (value, children) :: rest ->
      let acc = f acc value in
      let to_visit = List.append children rest in
      loop acc to_visit
  in
  match t with
  | E -> acc 
  | node -> loop acc [node]