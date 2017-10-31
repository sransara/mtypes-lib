module RString = struct
  module A = struct
    type t = char
    let resolve x y = 'f'
    let merge3 ~ancestor x y = 'f'
  end

  module V = struct
    type atom = char
    type t = string

    let length = String.length

    let set t i a =
      let s = Bytes.unsafe_of_string t in
      Bytes.set s i a;
      Bytes.unsafe_to_string s

    let get = String.get

    let insert t i c =
      assert (0 <= i && i <= String.length t);
      let left = String.sub t 0 i in
      let right = String.sub t i (String.length t - i) in
      String.concat "" [left; String.make 1 c; right]

    let delete t i =
      assert (0 <= i && (i + 1) <= String.length t);
      let left = String.sub t 0 i in
      let right = String.sub t (i + 1) (String.length t - (i + 1)) in
      String.concat "" [left; right]
  end

  module M = Mvector.Make(A)(V)
  include M

  let equal (x:M.t) (y:M.t) = x = y

  let resolve (x:M.t) (y:M.t):M.t =
    let l, x, y = 
      if M.length x >= M.length y then
        M.length y, x, y 
      else
        M.length x, y, x in
    let rec loop i m s x y =
      if i < m then
        let s = M.set s i (A.resolve (M.get x i) (M.get y i)) in
        loop (i+1) m s x y
      else s in
    let s = loop 0 l "" x y in
    let rec loop i m s x =
      if i < m then
        M.set s i (M.get x i)
      else s in
    loop l (M.length x) s x
end

module V = Mvector_list.Make(RString)

module IntKey = struct
  type t = int 
  let compare = compare
  let to_string = string_of_int
end

module M = Mmap_trie.Make(IntKey)(RString)

let keylist_to_string x = List.fold_left (fun x y -> x ^ IntKey.to_string y) "" x

let _ =
  let original = M.empty () |> M.add [1] "ehllo" |> M.add [1;2] "hello world" in
  let v1 = original |> M.add [1] "hello" |> M.add [1; 2] "hello there" in
  let v2 = original |> M.add [1] "hello" |> M.add [1; 2] "hello ocaml" |> M.add [2] "bye" in
  let m = M.merge3 ~ancestor:original v1 v2 in
  M.iter (fun k s -> Printf.printf "%s\n" ((keylist_to_string k) ^ " : " ^ s)) m
