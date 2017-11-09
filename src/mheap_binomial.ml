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
  type tree = Node of int * atom * tree list
  type t = tree list

  let empty = []
  let is_empty ts = ts = []

  let rank (Node (r, _, _)) = r
  let root (Node (_, x, _)) = x

  let link (Node (r, x1, c1) as t1) (Node (_, x2, c2) as t2) =
    if Atom.compare x1 x2 <= 0  then Node (r + 1, x1, t2 :: c1)
    else Node (r + 1, x2, t1 :: c2)

  let rec ins_tree t = function
    | [] -> [t]
    | t' :: ts' as ts ->
      if rank t < rank t' then t :: ts
      else ins_tree (link t t') ts'

  let insert x ts = ins_tree (Node (0, x, [])) ts

  let rec merge ts1 ts2 = match ts1, ts2 with
    | _, [] -> ts1
    | [], _ -> ts2
    | t1 :: ts1', t2 :: ts2' ->
      if rank t1 < rank t2 then t1 :: merge ts1' ts2
      else if rank t2 < rank t1 then t2 :: merge ts1 ts2'
      else ins_tree (link t1 t2) (merge ts1' ts2')

  let rec remove_min_tree = function
    | [] -> raise Empty
    | [t] -> t, []
    | t :: ts ->
      let t', ts' = remove_min_tree ts in
      if Atom.compare (root t) (root t') <= 0 then (t, ts)
      else (t', t :: ts')

  let find_min ts = root (fst (remove_min_tree ts))

  let delete_min ts =
    let Node (_, x, ts1), ts2 = remove_min_tree ts in
    merge (List.rev ts1) ts2

  let pop_min ts =
    let Node (_, x, ts1), ts2 = remove_min_tree ts in
    x, merge (List.rev ts1) ts2

  let rec elements h =
    if is_empty h then []
    else
      let x, h' = pop_min h in
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
      | [], [] -> []
      | [], _ ->
        let m, hy = pop_min hy in
        Insert m :: heap_diff hx hy
      | _, [] ->
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
            hx::hx::a, b in
          handle x y on_conflict
        | Delete x, Insert y ->
          let on_conflict () =
            let a, b = transform_aux rxs rys in
            (* Insert takes precedence: So reinsert the deleted element *)
            a, hy::hy::b in
          handle x y on_conflict
    in
    transform_aux p q

  (* Merging *)
  let resolve x y = merge x y

  let rec apply s = function
    | [] -> s
    | Insert x::r -> let s' = insert x s in apply s' r
    | Delete x::r -> 
      let xx, s' = pop_min s in
      let _ = assert (x = xx) in
      apply s' r

  let merge3 ~ancestor l r =
    let p = op_diff ancestor l in
    let q = op_diff ancestor r in
    let _,q' = op_transform p q in
    apply l q'
end