open Mtypes

(* Utility functions *)
module U = struct
  let string_of_list f l = "[ " ^ List.fold_left (fun a b -> a ^ (f b) ^ "; ") "" l ^ "]"
  let print_header h = Printf.printf "%s" ("\n" ^ h ^ "\n")
end

(* Vector - List *)
let _ =
  U.print_header "Vector - List";
  let module CharAtom = struct
    type t = char

    (* User defined merges for atom values *)
    let resolve x y = '#'
    let merge3 ~ancestor x y = '#'

    (* Used for presentation purposes *)
    let to_string c = String.make 1 c
  end in

  let module M = Mvector_list.Make(CharAtom) in

  let original = ['h';'e';'l';'l';'o'] in
  let v1 = ['h';'i';'l';'l';'o'] in
  let v2 = ['h';'e';'l';'l';'o';'w';'o';'r';'l';'d'] in

  (* Edit seq generation demonstration *)
  let edit_seq_printer = U.string_of_list (M.edit_to_string CharAtom.to_string) in 
  (* edit seq generation with diff *)
  let p = M.op_diff original v1 in
  let q = M.op_diff original v2 in
  let _ = 
    Printf.printf "p = diff original v1: %s\n" (edit_seq_printer p);
    Printf.printf "q = diff original v2: %s\n" (edit_seq_printer q)
  in
  (* op_transform demonstration *)
  let p', q' = M.op_transform p q in
  let _ = 
    Printf.printf "p' = transformed p: %s\n" (edit_seq_printer p');
    Printf.printf "q' = transformed q: %s\n" (edit_seq_printer q')
  in

  let m = M.merge3 ~ancestor:original v1 v2 in

  Printf.printf "merged = apply q' on v1: %s\n" (U.string_of_list CharAtom.to_string m)

(* Vector - Functorized vector *)
let _ =
  U.print_header "Vector - User defined vector";
  let module M = struct
    module CharAtom = struct
      type t = char

      (* User defined merges for atom values *)
      let resolve x y = '#'
      let merge3 ~ancestor x y = '#'

      (* Used for presentation purposes *)
      let to_string c = String.make 1 c
    end

    (* User defined Vector implementation *)
    module V = struct
      type atom = char
      type t = string

      let empty = ""

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

    module Mstring = Mvector.Make(CharAtom)(V)
    include Mstring
  end in

  let original = "hello" in
  let v1 = "hillo" in
  let v2 = "helloworld" in

  (* Edit seq generation demonstration *)
  let edit_seq_printer = U.string_of_list (M.edit_to_string M.CharAtom.to_string) in
  (* edit seq generation with diff *)
  let p = M.op_diff original v1 in
  let q = M.op_diff original v2 in
  let _ = 
    Printf.printf "p = diff original v1: %s\n" (edit_seq_printer p);
    Printf.printf "q = diff original v2: %s\n" (edit_seq_printer q)
  in
  (* op_transform demonstration *)
  let p', q' = M.op_transform p q in
  let _ = 
    Printf.printf "p' = transformed p: %s\n" (edit_seq_printer p');
    Printf.printf "q' = transformed q: %s\n" (edit_seq_printer q')
  in

  let m = M.merge3 ~ancestor:original v1 v2 in

  Printf.printf "merged = apply q' on v1: %s\n" m


(* Set - AVL Tree *)
let _ =
  U.print_header "Set - AVL tree";
  let module IntAtom = struct
    type t = int
    let compare = Pervasives.compare
    (* Used for presentation purposes *)
    let to_string = string_of_int
  end in

  let module M =  Mset_avltree.Make(IntAtom) in

  let original = M.empty |> M.add 10 |> M.add 5 |> M.add 20 in
  let v1 = original |> M.add 40 |> M.add 60 |> M.remove 10 in
  let v2 = original |> M.add 4 |> M.add 3 |> M.add 2 |> M.add 1 in

  (* Edit seq generation demonstration *)
  let edit_seq_printer = U.string_of_list (M.edit_to_string IntAtom.to_string) in
  (* edit seq generation with diff *)
  let p = M.op_diff original v1 in
  let q = M.op_diff original v2 in
  let _ = 
    Printf.printf "p = diff original v1: %s\n" (edit_seq_printer p);
    Printf.printf "q = diff original v2: %s\n" (edit_seq_printer q)
  in
  (* op_transform demonstration *)
  let p', q' = M.op_transform p q in
  let _ = 
    Printf.printf "p' = transformed p: %s\n" (edit_seq_printer p');
    Printf.printf "q' = transformed q: %s\n" (edit_seq_printer q')
  in

  let m = M.merge3 ~ancestor:original v1 v2 in

  Printf.printf "merged = apply q' on v1: %s\n" (U.string_of_list IntAtom.to_string (M.elements m))

(* Map - AVL tree *)
let _ =
  U.print_header "Map - AVL tree";
  let module IntAtom = struct
    type t = int
    let resolve x y = x + y
    let merge3 ~ancestor x y = ancestor + (x - ancestor) + (y - ancestor)
    let equal x y = Pervasives.compare x y = 0 
    (* Used for presentation purposes *)
    let to_string = string_of_int
  end in

  let module StringKey = struct
    include String
    (* Used for presentation purposes *)
    let to_string (s:t):t = s
  end in

  let module M = Mmap_avltree.Make(StringKey)(IntAtom) in

  let original = M.empty |> M.add "C" 10 |> M.add "A" 5 |> M.add "D" 20 in
  let v1 = original |> M.add "A" 40 |> M.add "D" 60 |> M.remove "C" in
  let v2 = original |> M.add "Z" 4 |> M.add "D" 70 in

  (* Edit seq generation demonstration *)
  let itos = IntAtom.to_string in
  let ktos = StringKey.to_string in 
  let edit_seq_printer = U.string_of_list (M.edit_to_string ktos itos) in
  (* edit seq generation with diff *)
  let p = M.op_diff original v1 in
  let q = M.op_diff original v2 in
  let _ = 
    Printf.printf "p = diff original v1: %s\n" (edit_seq_printer p);
    Printf.printf "q = diff original v2: %s\n" (edit_seq_printer q)
  in
  (* op_transform demonstration *)
  let p', q' = M.op_transform p q in
  let _ = 
    Printf.printf "p' = transformed p: %s\n" (edit_seq_printer p');
    Printf.printf "q' = transformed q: %s\n" (edit_seq_printer q')
  in

  let m = M.merge3 ~ancestor:original v1 v2 in

  Printf.printf "merged = apply q' on v1:\n";
  M.iter (fun k a -> Printf.printf "%s : %s\n" k (IntAtom.to_string a) ) m 


(* Map - Trie *)
let _ =
  U.print_header "Map - Trie";
  let module IntAtom = struct
    type t = int
    let resolve x y = x + y
    let merge3 ~ancestor x y = ancestor + (x - ancestor) + (y - ancestor)
    let equal x y = Pervasives.compare x y = 0 
    (* Used for presentation purposes *)
    let to_string = string_of_int
  end in

  let module CharKey = struct
    type t = char
    let compare = Pervasives.compare
    (* Used for presentation purposes *)
    let to_string c = String.make 1 c
  end in

  let module M = Mmap_trie.Make(CharKey)(IntAtom)(Mmap_avltree.Make) in

  let original = M.empty |> M.add ['C'] 10 |> M.add ['C'; 'A'] 5 |> M.add ['D'] 20 in
  let v1 = original |> M.add ['C'; 'A'] 40 |> M.add ['C'; 'A'; 'R'] 60 |> M.remove ['C'] in
  let v2 = original |> M.add ['Z'] 4 |> M.add ['D'] 70 in

  let m = M.merge3 ~ancestor:original v1 v2 in

  Printf.printf "merged:\n";
  M.iter (fun k a -> Printf.printf "%s : %s\n" (U.string_of_list CharKey.to_string k) (IntAtom.to_string a)) m 

(* Heap - Pairing *)
let _ =
  U.print_header "Heap - Pairing";
  let module CharAtom = struct
    type t = char
    let compare = Pervasives.compare
    (* Used for presentation purposes *)
    let to_string c = String.make 1 c
  end in

  let module M = Mheap_pairing.Make(CharAtom) in

  let original = M.empty |> M.insert 'z' |> M.insert 'x' |> M.insert 'c' in
  let v1 = original |> M.delete_min |> M.insert 'a' in
  let v2 = original |> M.insert 'c' |> M.insert 'z' in

  (* Edit seq generation demonstration *)
  let ctos = CharAtom.to_string in
  let edit_seq_printer = U.string_of_list (M.edit_to_string ctos) in
  (* edit seq generation with diff *)
  let p = M.op_diff original v1 in
  let q = M.op_diff original v2 in
  let _ = 
    Printf.printf "p = diff original v1: %s\n" (edit_seq_printer p);
    Printf.printf "q = diff original v2: %s\n" (edit_seq_printer q)
  in
  (* op_transform demonstration *)
  let p', q' = M.op_transform p q in
  let _ = 
    Printf.printf "p' = transformed p: %s\n" (edit_seq_printer p');
    Printf.printf "q' = transformed q: %s\n" (edit_seq_printer q')
  in

  let m = M.merge3 ~ancestor:original v1 v2 in

  Printf.printf "merged = apply q' on v1: %s\n" (U.string_of_list CharAtom.to_string (M.elements m))