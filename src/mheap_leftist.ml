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
  type t = E | T of int * atom * t * t

  let rank = function E -> 0 | T (r,_,_,_) -> r

  let makeT x a b =
    if rank a >= rank b then T (rank b + 1, x, a, b)
    else T (rank a + 1, x, b, a)

  let empty = E
  let is_empty h = h = E

  let rec merge h1 h2 = match h1, h2 with
    | _, E -> h1
    | E, _ -> h2
    | T (_, x, a1, b1), T (_, y, a2, b2) ->
      if Atom.compare x y <= 0 then makeT x a1 (merge b1 h2)
      else makeT y a2 (merge h1 b2)

  let insert x h = merge (T (1, x, E, E)) h
  let find_min = function E -> raise Empty | T (_, x, _, _) -> x
  let delete_min = function E -> raise Empty | T (_, x, a, b) -> x, merge a b

  let rec elements h =
    if is_empty h then []
    else
      let min = find_min h in
      let x, h' = delete_min h in
      min::(elements h')

  (* Patching *)
  type edit =
    | Insert of atom
    | Delete of atom
  type patch = edit list

  let op_diff xt yt =
    let heap_cmp hx hy =
      match hx, hy with
      | E, E ->  0
      | E, _ -> -1 (* hx < hy *) 
      | _, E ->  1 (* hx > hy *)
      | T (_, mx, _, _), T (_, my, _, _) -> 
        Atom.compare mx my in
    let ahi = Aheap.insert heap_cmp in
    let rec diff_heap edits ah1s ah2s =
      if Aheap.is_empty ah1s then
        let h2 = Aheap.fold_u merge empty ah2s in
        let e = List.fold_right (fun x y -> Insert x :: y) (elements h2) [] in
        e, Aheap.empty, Aheap.empty
      else if Aheap.is_empty ah2s then
        let h1 = Aheap.fold_u merge empty ah1s in
        let e = List.fold_right (fun x y -> Delete x :: y) (elements h1) [] in
        e, Aheap.empty, Aheap.empty
      else
        let h1, h1r = Aheap.delete_min heap_cmp ah1s in
        let h2, h2r  = Aheap.delete_min heap_cmp ah2s in
        match h1, h2 with
        | E, E -> diff_heap edits h1r h2r
        | E, _ -> diff_heap edits h1r ah2s
        | _, E -> diff_heap edits ah1s h2r
        | T(r1, a1, h11, h12), T(r2, a2, h21, h22) ->
          let c = Atom.compare a1 a2 in
          if c = 0 then (* a1 = a2 *)
            let h1n = ahi h11 @@ ahi h12 h1r  in
            let h2n = ahi h21 @@ ahi h22 h2r in
            diff_heap edits h1n h2n
          else if c < 0 then (* a1 < a2 *)
            let h1n = ahi h11 @@ ahi h12 h1r  in
            diff_heap (Delete a1 :: edits) h1n h2r
          else
            let h2n = ahi h21 @@ ahi h22 h2r in
            diff_heap (Insert a2 :: edits) h1r h2n
    in 
    let edits, _, _ = diff_heap [] (ahi xt Aheap.empty) (ahi yt Aheap.empty) in
    edits

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
      let xx, s' = delete_min s in
      let _ = assert (x = xx) in
      apply s' r

  let merge3 ~ancestor l r =
    let p = op_diff ancestor l in
    let q = op_diff ancestor r in
    let _,q' = op_transform p q in
    apply l q'
end