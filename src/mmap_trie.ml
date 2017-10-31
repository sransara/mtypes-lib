(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Jean-Christophe Filliatre                               *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE-FILLIATR.                                   *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)


(* A trie is a tree-like structure to implement dictionaries over
   keys which have list-like structures. The idea is that each node
   branches on an element of the list and stores the value associated
   to the path from the root, if any. Therefore, a trie can be
   defined as soon as a map over the elements of the list is
   given. *)

module type S = sig
  type t
  type atom
  type key
  val empty : unit -> t
  val is_empty : t -> bool
  val add : key -> atom -> t -> t
  val find : key -> t -> atom
  val remove : key -> t -> t
  val mem : key -> t -> bool
  val iter : (key -> atom -> unit) -> t -> unit
  val map : (atom -> atom) -> t -> t 
  val mapi : (key -> atom -> atom) -> t -> t
  val fold : (key -> atom -> atom -> atom) -> t -> atom -> atom
  val compare : (atom -> atom -> int) -> t -> t -> int
  val equal : t -> t -> bool
  include Mtypes.RESOLVEABLE with type t := t
end

(* TODO: Functorise the Mergeable map module *)
module Map = Mmap_avltree
module type KEY = Mmap.KEY
module type ATOM = Mmap.ATOM

module Make (Key: KEY) (Atom: ATOM)  : S 
  with type atom = Atom.t 
   and type key = Key.t list = 
struct
  (* TODO: To fix Error: Cannot safely evaluate the definition
        of the recursively-defined module Trie 
        Map.empty signature was changed to unit -> t 
        Is there a way to use the default? *)
  module rec Trie : S 
    with type atom = Atom.t 
     and type key = Key.t list = 
  struct
    type key = Key.t list
    type atom = Atom.t

    module M = Map.Make(Key)(Trie)

    type t = Node of atom option * M.t

    let empty () = Node (None, M.empty ())

    let is_empty = function
      | Node (None, m1) -> M.is_empty m1
      | _ -> false

    (* To find a mapping in a trie is easy: when all the elements of the
       key have been read, we just inspect the optional info at the
       current node; otherwise, we descend in the appropriate sub-trie
       using [M.find]. *)

    let rec find l t = match (l,t) with
      | [], Node (None,_)   -> raise Not_found
      | [], Node (Some v,_) -> v
      | x::r, Node (_,m)    -> find r (M.find x m)

    let rec mem l t = match (l,t) with
      | [], Node (None,_)   -> false
      | [], Node (Some _,_) -> true
      | x::r, Node (_,m)    -> try mem r (M.find x m) with Not_found -> false

    (*s Insertion is more subtle. When the final node is reached, we just
        put the information ([Some v]). Otherwise, we have to insert the
        binding in the appropriate sub-trie [t']. But it may not exists,
        and in that case [t'] is bound to an empty trie. Then we get a new
        sub-trie [t''] by a recursive insertion and we modify the
        branching, so that it now points to [t''], with [M.add]. *)

    let add l v t =
      let rec ins = function
        | [], Node (_,m) -> Node (Some v,m)
        | x::r, Node (v,m) ->
          let t' = try M.find x m with Not_found -> empty () in
          let t'' = ins (r,t') in
          Node (v, M.add x t'' m)
      in
      ins (l,t)

    (* When removing a binding, we take care of not leaving bindings to empty
       sub-tries in the nodes. Therefore, we test wether the result [t'] of
       the recursive call is the empty trie [empty]: if so, we just remove
       the branching with [M.remove]; otherwise, we modify it with [M.add]. *)

    let rec remove l t = match (l,t) with
      | [], Node (_,m) -> Node (None,m)
      | x::r, Node (v,m) ->
        try
          let t' = remove r (M.find x m) in
          Node (v, if is_empty t' then M.remove x m else M.add x t' m)
        with Not_found ->
          t

    (* The iterators [map], [mapi], [iter] and [fold] are implemented in
       a straigthforward way using the corresponding iterators [M.map],
       [M.mapi], [M.iter] and [M.fold]. For the last three of them,
       we have to remember the path from the root, as an extra argument
       [revp]. Since elements are pushed in reverse order in [revp],
       we have to reverse it with [List.rev] when the actual binding
       has to be passed to function [f]. *)

    let rec map f t =
      match t with
      | Node (None,m)   -> Node (None, M.map (map f) m)
      | Node (Some v,m) -> Node (Some (f v), M.map (map f) m)

    let mapi f t =
      let rec maprec revp = function
        | Node (None,m) ->
          Node (None, M.mapi (fun x -> maprec (x::revp)) m)
        | Node (Some v,m) ->
          Node (Some (f (List.rev revp) v), M.mapi (fun x -> maprec (x::revp)) m)
      in
      maprec [] t

    let iter f t =
      let rec traverse revp = function
        | Node (None,m) ->
          M.iter (fun x -> traverse (x::revp)) m
        | Node (Some v,m) ->
          f (List.rev revp) v; M.iter (fun x t -> traverse (x::revp) t) m
      in
      traverse [] t

    let rec fold f t acc =
      let rec traverse revp t acc = match t with
        | Node (None,m) ->
          M.fold (fun x -> traverse (x::revp)) m acc
        | Node (Some v,m) ->
          f (List.rev revp) v (M.fold (fun x -> traverse (x::revp)) m acc)
      in
      traverse [] t acc

    let compare cmp a b =
      let rec comp a b = match a,b with
        | Node (Some _, _), Node (None, _) -> 1
        | Node (None, _), Node (Some _, _) -> -1
        | Node (None, m1), Node (None, m2) ->
          M.compare comp m1 m2
        | Node (Some a, m1), Node (Some b, m2) ->
          let c = cmp a b in
          if c <> 0 then c else M.compare comp m1 m2
      in
      comp a b

    let equal a b =
      let rec comp a b = match a,b with
        | Node (None, m1), Node (None, m2) ->
          M.equal m1 m2
        | Node (Some a, m1), Node (Some b, m2) ->
          Atom.equal a b && M.equal m1 m2
        | _ ->
          false
      in
      comp a b

    let merge3 ~ancestor xx yy =
      match ancestor, xx, yy with
      | Node (_, ma), Node (None, mx), Node (None, my) -> 
        Node (None, M.merge3 ~ancestor:ma mx my)
      | Node (_, ma), Node (e, mx), Node (None, my)
      | Node (_, ma), Node (None, mx), Node (e, my) -> 
        Node (e, M.merge3 ~ancestor:ma mx my)
      | Node (None, ma), Node (Some x, mx), Node (Some y, my) -> 
        Node (Some (Atom.resolve x y), M.merge3 ~ancestor:ma mx my)
      | Node (Some a, ma), Node (Some x, mx), Node (Some y, my) -> 
        Node (Some (Atom.merge3 ~ancestor:a x y), M.merge3 ~ancestor:ma mx my)

    let resolve xx yy = 
      match xx, yy with
      | Node (None, mx), Node (None, my) -> 
        Node (None, M.resolve mx my)
      | Node (e, mx), Node (None, my)
      | Node (None, mx), Node (e, my) -> 
        Node (e, M.resolve mx my)
      | Node (Some x, mx), Node (Some y, my) -> 
        Node (Some (Atom.resolve x y), M.resolve mx my)
  end

  include Trie
end