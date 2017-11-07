module A = struct
  type t = char
  let resolve x y = 'f'
  let merge3 ~ancestor x y = 
    let _ = Printf.printf "%c %c %c\n" ancestor x y in
    'f'
  let equal = Pervasives.(=)
end

module RString = struct
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

module IntKey = struct
  type t = int 
  let compare = compare
  let to_string = string_of_int
end

module M = struct
  module B = Mmap_avltree.Make(IntKey)(A)
  include B
  let ets s = function
  | Add (key, atom) -> s ^ "Add " ^ IntKey.to_string key ^ "," ^ String.make 1 atom^ "\n" 
  | Remove (key) -> s ^ "Remove" ^ IntKey.to_string key^ "\n" 
  | Replace (key, ax, y) -> s ^ "Replace " ^ IntKey.to_string key ^ "," ^ String.make 1 ax ^ "," ^ String.make 1 y ^ "\n" 

  let pts (p:patch) = List.fold_left ets "" p

  let merge3 ~ancestor l r =
    let p = op_diff ancestor l in
    let _ = Printf.printf "p: %s" (pts p) in
    let q = op_diff ancestor r in
    let _ = Printf.printf "q: %s" (pts q) in
    let _,q' = op_transform p q in
    let _ = Printf.printf "q': %s" (pts q') in
    apply l q'
end

let _ =
  let original = M.empty |> M.add 12 'c' in
  let v1 = original |> M.add 12 'd' in
  let v2 = original |> M.add 12 'e' in
  let m = M.merge3 ~ancestor:original v1 v2 in
  let _ = M.iter (fun k s -> Printf.printf "%s\n" (string_of_int k ^ " : " ^ String.make 1 s)) m in
  ()

(* module M = Mmap_avltree.Make(IntKey)(RString)

let _ =
  let original = M.empty |> M.add 12 "hello world" in
  let v1 = original |> M.add 12 "hello there" in
  let v2 = original |> M.add 12 "hello ocaml" in
  let m = M.merge3 ~ancestor:original v1 v2 in
  let _ = M.iter (fun k s -> Printf.printf "%s\n" (string_of_int k ^ " : " ^ s)) m in
  () *)

(* module M = Mmap_trie.Make(IntKey)(RString)

let klts (kl:IntKey.t list) = List.fold_right (fun a b -> IntKey.to_string a ^ b) kl "" 
let _ =
  let original = M.empty |> M.add [1] "ehllo" |> M.add [1;2] "hello world" in
  let v1 = original |> M.add [1] "hello" |> M.add [1; 2] "hello there" in
  let v2 = original |> M.add [1] "hello" |> M.add [1; 2] "hello ocaml" |> M.add [2] "bye" in
  let m = M.merge3 ~ancestor:original v1 v2 in
  M.iter (fun k s -> Printf.printf "%s\n" ((klts k) ^ " : " ^ s)) m *)
