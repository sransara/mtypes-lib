(*---------------------------------------------------------------------------
   Copyright (c) 2017 KC Sivaramakrishnan. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

module type ATOM = Mvector.ATOM

module Make (Atom: ATOM) : Mvector.S 
  with type atom = Atom.t
   and type t = Atom.t list = 
struct
  module V = struct
    type atom = Atom.t
    type t = atom list

    let empty = []

    let length (x:t) = List.length x 

    let split_at l i =
      if List.length l > i then
        let rec aux xs i acc =
          if i >= 0 then aux (List.tl xs) (i - 1) ((List.hd xs) :: acc)
          else acc, xs
        in
        aux l i []
      else
        raise (Invalid_argument "out of bound index")

    let insert l i a =
      if List.length l = i then
        List.append l [a]
      else
        let rxs, ys = split_at l i in
        let ixs = match rxs with
        | [] -> [a]
        | x::[] -> [x; a]
        | x::y -> x::a::y in
        List.rev_append ixs ys

    let get l i =
      List.nth l i

    let set l i a =
      let rxs, ys = split_at l i in
      let sxs = a :: (List.tl rxs) in
      List.rev_append sxs ys

    let delete l i =
      let rxs, ys = split_at l i in
      let dxs = List.tl rxs in
      List.rev_append dxs ys
  end

  module M = Mvector.Make(Atom)(V)

  include M
end

(*---------------------------------------------------------------------------
   Copyright (c) 2017 KC Sivaramakrishnan

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)