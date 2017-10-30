let _ =
  let module IntAtom = struct
    type t = int
    let compare = Pervasives.compare
    let to_string = string_of_int
  end in

  let module M =  Mset_avltree.Make(IntAtom) in

  let original = M.empty |> M.add 10 |> M.add 5 |> M.add 20 in
  let v1 = original |> M.add 40 |> M.add 60 |> M.remove 10 in
  let v2 = original |> M.add 4 |> M.add 3 |> M.add 2 |> M.add 1 in
  let _ = M.merge3 ~ancestor:original v1 v2 in
  ()

let _ =
  let module IntAtom = struct
    type t = int
    let resolve x y = x + y
    let merge3 ~ancestor x y = ancestor + (x - ancestor) + (y - ancestor)
    let to_string = string_of_int
    let equal x y = Pervasives.compare x y = 0 
  end in

  let module StringKey = struct
    include String
    let to_string s = s 
  end in

  let module M = Mmap_avltree.Make(StringKey)(IntAtom) in
  let original = M.empty |> M.add "C" 10 |> M.add "A" 5 |> M.add "D" 20 in
  let v1 = original |> M.add "A" 40 |> M.add "D" 60 |> M.remove "C" in
  let v2 = original |> M.add "Z" 4 |> M.add "D" 70 in
  let _ = M.merge3 ~ancestor:original v1 v2 in
  ()

let _ =
  let module String = struct
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
  end in
  ()