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

module LeftistHeap (Element : ORDERED) : (HEAP with module Elem = Element) =
struct
  module Elem = Element

  type heap = E | T of int * Elem.t * heap * heap

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
        if Elem.leq x y then makeT x a1 (merge b1 h2)
        else makeT y a2 (merge h1 b2)

  let insert x h = merge (T (1, x, E, E)) h
  let find_min = function E -> raise Empty | T (_, x, _, _) -> x
  let delete_min = function E -> raise Empty | T (_, _, a, b) -> merge a b
end