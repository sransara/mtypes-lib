(*
   Original source code in SML from:

     Purely Functional Data Structures
     Chris Okasaki
     Cambridge University Press, 1998
     Copyright (c) 1998 Cambridge University Press

   Translation from SML to OCAML (this file):

     Copyright (C) 1999, 2000, 2001  Markus Mottl
     email:  markus.mottl@gmail.com
     www:    http://www.ocaml.info

   Licensed under the Apache License, Version 2.0 (the "License"); you may
   not use this file except in compliance with the License.  You may obtain
   a copy of the License at
     http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
   License for the specific language governing permissions and limitations
   under the License.
*)

exception Empty

module type ATOM = Mheap.ATOM

module Make (Atom: ATOM) (* : Mheap.S *) =
struct
  type atom = Atom.t
  type t = E | T of atom * t list

  let empty = E
  let is_empty h = h = E

  let merge h1 h2 = match h1, h2 with
    | _, E -> h1
    | E, _ -> h2
    | T (x, hs1), T (y, hs2) ->
        if Atom.compare x y <= 0 then T (x, h2 :: hs1)
        else T (y, h1 :: hs2)

  let insert x h = merge (T (x, [])) h

  let rec merge_pairs = function
    | [] -> E
    | [h] -> h
    | h1 :: h2 :: hs -> merge (merge h1 h2) (merge_pairs hs)

  let find_min = function
    | E -> raise Empty
    | T (x, _) -> x

  let delete_min = function
    | E -> raise Empty
    | T (_, hs) -> merge_pairs hs
  
  let pop_min = function
    | E -> raise Empty
    | T (x, hs) -> x, merge_pairs hs

  let rec elements h =
    if is_empty h then []
    else
      let min, h' = pop_min h in
      min::(elements h')

  (* Patching *)
  type edit =
    | Insert of atom
    | Delete of atom
  type patch = edit list

  let edit_to_string atom_to_string = function
  | Insert (a) -> Printf.sprintf "Insert (%s)" (atom_to_string a)
  | Delete (a) -> Printf.sprintf "Delete (%s)" (atom_to_string a)

  let op_diff xt yt =
    let rec heap_diff hx hy =
      match hx, hy with
      | E, E -> []
      | E, _ ->
        let m, hy = pop_min hy in
        Insert m :: heap_diff hx hy
      | _, E ->
        let m, hx = pop_min hx in
        Delete m :: heap_diff hx hy
      | _, _ ->
        let a1 = find_min hx in
        let a2 = find_min hy in
        let c = Atom.compare a1 a2 in
        if c = 0 then
          let hy = delete_min hy in
          let hx = delete_min hx in
          heap_diff hx hy
        else if c < 0 then (* a1 < a2 *)
          let hx = delete_min hx in
          Delete a1 :: heap_diff hx hy
        else (* c > 0 = a1 > a2 *)
          let hy = delete_min hy in
          Insert a2 :: heap_diff hx hy
    in
    heap_diff xt yt

  let op_transform p q = 
    let rec transform_aux xs ys =
      match xs, ys with
      | [], [] -> [], []
      | [], _ -> [], ys
      | _, [] -> xs, []   
      | hx::rxs, hy::rys ->
        let handle kx ky on_conflict =
          let c = Atom.compare kx ky in
          if c = 0 then on_conflict ()
          else if c < 0 then 
            let a, b = transform_aux rxs ys in
            hx::a, b
          else (* c > 0 *)
            let a, b = transform_aux xs rys in
            a, hy::b in
        match hx, hy with
        | Insert x, Insert y
        | Delete x, Delete y ->
          let on_conflict () = transform_aux rxs rys in
          handle x y on_conflict
        | Insert x, Delete y ->
          let on_conflict () =
            let a, b = transform_aux rxs rys in
            (* Insert takes precedence: So reinsert the deleted element *)
            hx::a, hy::b in
          handle x y on_conflict
        | Delete x, Insert y ->
          let on_conflict () =
            let a, b = transform_aux rxs rys in
            (* Insert takes precedence: So reinsert the deleted element *)
            hx::a, hy::b in
          handle x y on_conflict
    in
    transform_aux p q

  (* Merging *)
  let resolve x y = merge x y

  let apply h edits = 
    let rec aux h hacc edits =
    match edits with
    | [] -> merge h hacc
    | Insert x::r -> 
      let hacc' = insert x hacc in 
      aux h hacc' r
    | Delete x::r -> 
      let xx, h' = pop_min h in
      let _ = assert (x = xx) in
      aux h' hacc r
    in
    aux h empty edits

  let merge3 ~ancestor l r =
    let p = op_diff ancestor l in
    let q = op_diff ancestor r in
    let _,q' = op_transform p q in
    apply l q'
    (* let p',_ = op_transform p q in
    apply r p' *)
end