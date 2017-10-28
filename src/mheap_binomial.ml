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
exception Impossible_pattern of string

(* A totally ordered type and its comparison functions *)
module type ORDERED = sig
  type t

  val eq : t -> t -> bool
  val lt : t -> t -> bool
  val leq : t -> t -> bool
end

module type HEAP = sig
  module Elem : ORDERED

  type heap

  val empty : heap
  val is_empty : heap -> bool

  val insert : Elem.t -> heap -> heap
  val merge : heap -> heap -> heap

  val find_min : heap -> Elem.t  (* raises Empty if heap is empty *)
  val delete_min : heap -> heap  (* raises Empty if heap is empty *)
end

module BinomialHeap (Element : ORDERED) : (HEAP with module Elem = Element) =
struct
  module Elem = Element

  type tree = Node of int * Elem.t * tree list
  type heap = tree list

  let empty = []
  let is_empty ts = ts = []

  let rank (Node (r, _, _)) = r
  let root (Node (_, x, _)) = x

  let link (Node (r, x1, c1) as t1) (Node (_, x2, c2) as t2) =
    if Elem.leq x1 x2 then Node (r + 1, x1, t2 :: c1)
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
      if Elem.leq (root t) (root t') then (t, ts)
      else (t', t :: ts')

  let find_min ts = root (fst (remove_min_tree ts))

  let delete_min ts =
    let Node (_, _, ts1), ts2 = remove_min_tree ts in
    merge (List.rev ts1) ts2
end