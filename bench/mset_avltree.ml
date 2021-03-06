(* Hand derived versioned from Mtypes.Mset_avltree *)


open Lwt.Infix

(* Sets over ordered types *)
module type ATOM =
sig
  type t
  val t: t Irmin.Type.t 
  val compare: t -> t -> int
  val of_string: string -> t
end

module Make (Atom: ATOM) (* : Mset.S *) =
struct
  type atom = Atom.t
  type node = {l:t; v:atom; r:t; h:int}
  and t = Empty | Node of node

  (* Sets are represented by balanced binary trees (the heights of the
     children differ by at most 2 *)

  let height = function
      Empty -> 0
    | Node {h; _} -> h

  (* Creates a new node with left son l, value v and right son r.
     We must have all elements of l < v < all elements of r.
     l and r must be balanced and | height l - height r | <= 2.
     Inline expansion of height for better speed. *)

  let create l v r =
    let hl = match l with Empty -> 0 | Node {h; _} -> h in
    let hr = match r with Empty -> 0 | Node {h; _} -> h in
    Node{l; v; r; h=(if hl >= hr then hl + 1 else hr + 1)}

  (* Same as create, but performs one step of rebalancing if necessary.
     Assumes l and r balanced and | height l - height r | <= 3.
     Inline expansion of create for better speed in the most frequent case
     where no rebalancing is required. *)

  let bal l v r =
    let hl = match l with Empty -> 0 | Node {h; _} -> h in
    let hr = match r with Empty -> 0 | Node {h; _} -> h in
    if hl > hr + 2 then begin
      match l with
        Empty -> invalid_arg "Set.bal"
      | Node{l=ll; v=lv; r=lr; _} ->
        if height ll >= height lr then
          create ll lv (create lr v r)
        else begin
          match lr with
            Empty -> invalid_arg "Set.bal"
          | Node{l=lrl; v=lrv; r=lrr; _}->
            create (create ll lv lrl) lrv (create lrr v r)
        end
    end else if hr > hl + 2 then begin
      match r with
        Empty -> invalid_arg "Set.bal"
      | Node{l=rl; v=rv; r=rr; _} ->
        if height rr >= height rl then
          create (create l v rl) rv rr
        else begin
          match rl with
            Empty -> invalid_arg "Set.bal"
          | Node{l=rll; v=rlv; r=rlr; _} ->
            create (create l v rll) rlv (create rlr rv rr)
        end
    end else
      Node{l; v; r; h=(if hl >= hr then hl + 1 else hr + 1)}

  (* Insertion of one element *)

  let rec add x = function
      Empty -> Node{l=Empty; v=x; r=Empty; h=1}
    | Node{l; v; r; _} as t ->
      let c = Atom.compare x v in
      if c = 0 then t else
      if c < 0 then
        let ll = add x l in
        if l == ll then t else bal ll v r
      else
        let rr = add x r in
        if r == rr then t else bal l v rr

  let singleton x = Node{l=Empty; v=x; r=Empty; h=1}

  (* Beware: those two functions assume that the added v is *strictly*
     smaller (or bigger) than all the present elements in the tree; it
     does not test for equality with the current min (or max) element.
     Indeed, they are only used during the "join" operation which
     respects this precondition.
  *)

  let rec add_min_element x = function
    | Empty -> singleton x
    | Node {l; v; r; _} ->
      bal (add_min_element x l) v r

  let rec add_max_element x = function
    | Empty -> singleton x
    | Node {l; v; r; _} ->
      bal l v (add_max_element x r)

  (* Same as create and bal, but no assumptions are made on the
     relative heights of l and r. *)

  let rec join l v r =
    match (l, r) with
      (Empty, _) -> add_min_element v r
    | (_, Empty) -> add_max_element v l
    | (Node{l=ll; v=lv; r=lr; h=lh}, Node{l=rl; v=rv; r=rr; h=rh}) ->
      if lh > rh + 2 then bal ll lv (join lr v r) else
      if rh > lh + 2 then bal (join l v rl) rv rr else
        create l v r

  (* Smallest and greatest element of a set *)

  let rec min_elt = function
      Empty -> raise Not_found
    | Node{l=Empty; v; _} -> v
    | Node{l; _} -> min_elt l

  let rec min_elt_opt = function
      Empty -> None
    | Node{l=Empty; v; _} -> Some v
    | Node{l; _} -> min_elt_opt l

  let rec max_elt = function
      Empty -> raise Not_found
    | Node{v; r=Empty; _} -> v
    | Node{r; _} -> max_elt r

  let rec max_elt_opt = function
      Empty -> None
    | Node{v; r=Empty; _} -> Some v
    | Node{r; _} -> max_elt_opt r

  (* Remove the smallest element of the given set *)

  let rec remove_min_elt = function
      Empty -> invalid_arg "Set.remove_min_elt"
    | Node{l=Empty; r; _} -> r
    | Node{l; v; r; _} -> bal (remove_min_elt l) v r

  (* Merge two trees l and r into one.
     All elements of l must precede the elements of r.
     Assume | height l - height r | <= 2. *)

  let merge t1 t2 =
    match (t1, t2) with
      (Empty, t) -> t
    | (t, Empty) -> t
    | (_, _) -> bal t1 (min_elt t2) (remove_min_elt t2)

  (* Merge two trees l and r into one.
     All elements of l must precede the elements of r.
     No assumption on the heights of l and r. *)

  let concat t1 t2 =
    match (t1, t2) with
      (Empty, t) -> t
    | (t, Empty) -> t
    | (_, _) -> join t1 (min_elt t2) (remove_min_elt t2)

  (* Splitting.  split x s returns a triple (l, present, r) where
      - l is the set of elements of s that are < x
      - r is the set of elements of s that are > x
      - present is false if s contains no element equal to x,
        or true if s contains an element equal to x. *)

  let rec split x = function
      Empty ->
      (Empty, false, Empty)
    | Node{l; v; r; _} ->
      let c = Atom.compare x v in
      if c = 0 then (l, true, r)
      else if c < 0 then
        let (ll, pres, rl) = split x l in (ll, pres, join rl v r)
      else
        let (lr, pres, rr) = split x r in (join l v lr, pres, rr)

  (* Implementation of the set operations *)

  let empty = Empty

  let is_empty = function Empty -> true | _ -> false

  let rec mem x = function
      Empty -> false
    | Node{l; v; r; _} ->
      let c = Atom.compare x v in
      c = 0 || mem x (if c < 0 then l else r)

  let rec remove x = function
      Empty -> Empty
    | (Node{l; v; r; _} as t) ->
      let c = Atom.compare x v in
      if c = 0 then merge l r
      else
      if c < 0 then
        let ll = remove x l in
        if l == ll then t
        else bal ll v r
      else
        let rr = remove x r in
        if r == rr then t
        else bal l v rr

  let rec union s1 s2 =
    match (s1, s2) with
    | (Empty, t2) -> t2
    | (t1, Empty) -> t1
    | (Node{l=l1; v=v1; r=r1; h=h1}, Node{l=l2; v=v2; r=r2; h=h2}) ->
      if h1 >= h2 then
        if h2 = 1 then add v2 s1 
        else begin
          let (l2, _, r2) = split v1 s2 in
          join (union l1 l2) v1 (union r1 r2)
        end
      else
      if h1 = 1 then add v1 s2 
      else begin
        let (l1, _, r1) = split v2 s1 in
        join (union l1 l2) v2 (union r1 r2)
      end

  let rec inter s1 s2 =
    match (s1, s2) with
      (Empty, _) -> Empty
    | (_, Empty) -> Empty
    | (Node{l=l1; v=v1; r=r1; _}, t2) ->
      match split v1 t2 with
        (l2, false, r2) ->
        concat (inter l1 l2) (inter r1 r2)
      | (l2, true, r2) ->
        join (inter l1 l2) v1 (inter r1 r2)

  let rec diff s1 s2 =
    match (s1, s2) with
      (Empty, _) -> Empty
    | (t1, Empty) -> t1
    | (Node{l=l1; v=v1; r=r1; _}, t2) ->
      match split v1 t2 with
        (l2, false, r2) ->
        join (diff l1 l2) v1 (diff r1 r2)
      | (l2, true, r2) ->
        concat (diff l1 l2) (diff r1 r2)

  type enumeration = End | More of atom * t * enumeration

  let rec cons_enum s e =
    match s with
      Empty -> e
    | Node{l; v; r; _} -> cons_enum l (More(v, r, e))

  let rec compare_aux e1 e2 =
    match (e1, e2) with
      (End, End) -> 0
    | (End, _)  -> -1
    | (_, End) -> 1
    | (More(v1, r1, e1), More(v2, r2, e2)) ->
      let c = Atom.compare v1 v2 in
      if c <> 0
      then c
      else compare_aux (cons_enum r1 e1) (cons_enum r2 e2)

  let compare s1 s2 =
    compare_aux (cons_enum s1 End) (cons_enum s2 End)

  let equal s1 s2 =
    compare s1 s2 = 0

  let rec subset s1 s2 =
    match (s1, s2) with
      Empty, _ ->
      true
    | _, Empty ->
      false
    | Node {l=l1; v=v1; r=r1; _}, (Node {l=l2; v=v2; r=r2; _} as t2) ->
      let c = Atom.compare v1 v2 in
      if c = 0 then
        subset l1 l2 && subset r1 r2
      else if c < 0 then
        subset (Node {l=l1; v=v1; r=Empty; h=0}) l2 && subset r1 t2
      else
        subset (Node {l=Empty; v=v1; r=r1; h=0}) r2 && subset l1 t2

  let rec iter f = function
      Empty -> ()
    | Node{l; v; r; _} -> iter f l; f v; iter f r

  let rec fold f s accu =
    match s with
      Empty -> accu
    | Node{l; v; r; _} -> fold f r (f v (fold f l accu))

  let rec for_all p = function
      Empty -> true
    | Node{l; v; r; _} -> p v && for_all p l && for_all p r

  let rec exists p = function
      Empty -> false
    | Node{l; v; r; _} -> p v || exists p l || exists p r

  let rec filter p = function
      Empty -> Empty
    | (Node{l; v; r; _}) as t ->
      (* call [p] in the expected left-to-right order *)
      let l' = filter p l in
      let pv = p v in
      let r' = filter p r in
      if pv then
        if l==l' && r==r' then t else join l' v r'
      else concat l' r'

  let rec partition p = function
      Empty -> (Empty, Empty)
    | Node{l; v; r; _} ->
      (* call [p] in the expected left-to-right order *)
      let (lt, lf) = partition p l in
      let pv = p v in
      let (rt, rf) = partition p r in
      if pv
      then (join lt v rt, concat lf rf)
      else (concat lt rt, join lf v rf)

  let rec cardinal = function
      Empty -> 0
    | Node{l; r; _} -> cardinal l + 1 + cardinal r

  let rec elements_aux accu = function
      Empty -> accu
    | Node{l; v; r; _} -> elements_aux (v :: elements_aux accu r) l

  let elements s =
    elements_aux [] s

  let choose = min_elt

  let choose_opt = min_elt_opt

  let rec find x = function
      Empty -> raise Not_found
    | Node{l; v; r; _} ->
      let c = Atom.compare x v in
      if c = 0 then v
      else find x (if c < 0 then l else r)

  let rec find_first_aux v0 f = function
      Empty ->
      v0
    | Node{l; v; r; _} ->
      if f v then
        find_first_aux v f l
      else
        find_first_aux v0 f r

  let rec find_first f = function
      Empty ->
      raise Not_found
    | Node{l; v; r; _} ->
      if f v then
        find_first_aux v f l
      else
        find_first f r

  let rec find_first_opt_aux v0 f = function
      Empty ->
      Some v0
    | Node{l; v; r; _} ->
      if f v then
        find_first_opt_aux v f l
      else
        find_first_opt_aux v0 f r

  let rec find_first_opt f = function
      Empty ->
      None
    | Node{l; v; r; _} ->
      if f v then
        find_first_opt_aux v f l
      else
        find_first_opt f r

  let rec find_last_aux v0 f = function
      Empty ->
      v0
    | Node{l; v; r; _} ->
      if f v then
        find_last_aux v f r
      else
        find_last_aux v0 f l

  let rec find_last f = function
      Empty ->
      raise Not_found
    | Node{l; v; r; _} ->
      if f v then
        find_last_aux v f r
      else
        find_last f l

  let rec find_last_opt_aux v0 f = function
      Empty ->
      Some v0
    | Node{l; v; r; _} ->
      if f v then
        find_last_opt_aux v f r
      else
        find_last_opt_aux v0 f l

  let rec find_last_opt f = function
      Empty ->
      None
    | Node{l; v; r; _} ->
      if f v then
        find_last_opt_aux v f r
      else
        find_last_opt f l

  let rec find_opt x = function
      Empty -> None
    | Node{l; v; r; _} ->
      let c = Atom.compare x v in
      if c = 0 then Some v
      else find_opt x (if c < 0 then l else r)

  let try_join l v r =
    (* [join l v r] can only be called when (elements of l < v <
       elements of r); use [try_join l v r] when this property may
       not hold, but you hope it does hold in the common case *)
    if (l = Empty || Atom.compare (max_elt l) v < 0)
    && (r = Empty || Atom.compare v (min_elt r) < 0)
    then join l v r
    else union l (add v r)

  let rec map f = function
    | Empty -> Empty
    | Node{l; v; r; _} as t ->
      (* enforce left-to-right evaluation order *)
      let l' = map f l in
      let v' = f v in
      let r' = map f r in
      if l == l' && v == v' && r == r' then t
      else try_join l' v' r'

  let of_sorted_list l =
    let rec sub n l =
      match n, l with
      | 0, l -> Empty, l
      | 1, x0 :: l -> Node {l=Empty; v=x0; r=Empty; h=1}, l
      | 2, x0 :: x1 :: l ->
        Node{l=Node{l=Empty; v=x0; r=Empty; h=1}; v=x1; r=Empty; h=2}, l
      | 3, x0 :: x1 :: x2 :: l ->
        Node{l=Node{l=Empty; v=x0; r=Empty; h=1}; v=x1;
             r=Node{l=Empty; v=x2; r=Empty; h=1}; h=2}, l
      | n, l ->
        let nl = n / 2 in
        let left, l = sub nl l in
        match l with
        | [] -> assert false
        | mid :: l ->
          let right, l = sub (n - nl - 1) l in
          create left mid right, l
    in
    fst (sub (List.length l) l)

  let of_list l =
    match l with
    | [] -> empty
    | [x0] -> singleton x0
    | [x0; x1] -> add x1 (singleton x0)
    | [x0; x1; x2] -> add x2 (add x1 (singleton x0))
    | [x0; x1; x2; x3] -> add x3 (add x2 (add x1 (singleton x0)))
    | [x0; x1; x2; x3; x4] -> add x4 (add x3 (add x2 (add x1 (singleton x0))))
    | _ -> of_sorted_list (List.sort_uniq Atom.compare l)

  (* Patching *)
  type edit = 
    | Add of atom
    | Remove of atom
  type patch = edit list

  let edit_to_string atom_to_string = function
    | Add (a) -> Printf.sprintf "Add (%s)" (atom_to_string a)
    | Remove (a) -> Printf.sprintf "Remove (%s)" (atom_to_string a)

  let op_diff xt yt =
    (* TODO: Use reverse appends for faster list manipulations *)
    let rec diff_avlt s1 s2 =
      match (s1, s2) with
      | (Empty, t2) -> fold (fun x y -> y @ [Add x]) t2 []
      | (t1, Empty) -> fold (fun x y -> y @ [Remove x]) t1 []     
      | (Node{l=l1; v=v1; r=r1; h=h1}, Node{l=l2; v=v2; r=r2; h=h2}) ->
        if h1 >= h2 then
          let (l2, p, r2) = split v1 s2 in
          let l = diff_avlt l1 l2 in
          let r = diff_avlt r1 r2 in
          if p then
            List.append l r
          else
            List.append l (Remove v1 :: r)
        else
          let (l1, p, r1) = split v2 s1 in
          let l = diff_avlt l1 l2 in
          let r = diff_avlt r1 r2 in
          if p then
            List.append l r
          else
            List.append l (Add v2 :: r)
    in
    diff_avlt xt yt

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
        | Add x, Add y
        | Remove x, Remove y ->
          let on_conflict () = transform_aux rxs rys in
          handle x y on_conflict
        | Add x, Remove y 
        | Remove x, Add y ->
          (* Impossible condition *)
          let on_conflict = fun () -> assert false in
          handle x y on_conflict
    in
    transform_aux p q

  (* Merging *)
  let resolve = union

  let rec apply s = function
    | [] -> s
    | Add x::r -> let s' = add x s in apply s' r
    | Remove x::r -> let s' = remove x s in apply s' r

  let merge3 ~ancestor l r =
    let p = op_diff ancestor l in
    let q = op_diff ancestor r in
    let _,q' = op_transform p q in
    apply l q'
end

module type Config = sig
  val root: string
  val shared: string
  val init: unit -> unit
end

module MakeVersioned (Config: Config) (Atom: ATOM) = struct
  module OM = Make(Atom)
  module K = Irmin.Hash.SHA1

  let from_just = function (Some x) -> x
  | None -> failwith "Expected Some. Got None."

  type vnode = {l:K.t; v:OM.atom; r:K.t; h:int64}

  type vt = 
    | Empty 
    | Node of vnode

  module M = struct
    module AO_value = struct
      type t = vt

      let node = 
        let open Irmin.Type in
        record "node" (fun l v r h -> {l;v;r;h})
        |+ field "l" K.t (fun t -> t.l)
        |+ field "v" Atom.t (fun t -> t.v)
        |+ field "r" K.t (fun t -> t.r)
        |+ field "h" int64 (fun t -> t.h)
        |> sealr


      let t =
        let open Irmin.Type in
        variant "t" (fun empty node -> function
            | Empty -> empty
            | Node n -> node n)
        |~ case0 "Empty" Empty
        |~ case1 "Node" node (fun x -> Node x)
        |> sealv

      let pp = Irmin.Type.dump t

      let of_string (s:string) =
        let from_just_ok = function (Ok x) -> x
                                  | _ -> failwith "Expected Ok. Got error." in
        let remove_blanks = Str.global_replace (Str.regexp "[\n\r\t ]") "" in
        let s = remove_blanks s in
        match Str.split (Str.regexp "[{}]") s with
        | ["Empty"] -> Ok Empty
        | ["Node"; node] ->
          let rx = Str.regexp "l=\\(.+\\);v=\\(.+\\);r=\\(.+\\);h=\\(.+\\);" in
          let _ = Str.search_forward rx node 0 in
          let l = from_just_ok @@ K.of_string @@ Str.matched_group 1 node in
          let v = Atom.of_string @@ Str.matched_group 2 node in
          let r = from_just_ok @@ K.of_string @@ Str.matched_group 3 node in
          let h = Int64.of_string @@ Str.matched_group 4 node in
          Ok (Node {l;v;r;h})
        | _ -> Error (`Msg ("invalid entry: " ^ s))
    end

    module AO_store = struct
      (* Immutable collection of all versionedt *)
      module S = Irmin_git.AO(Git_unix.FS)(AO_value)
      include S

      let create config =
        let level = Irmin.Private.Conf.key ~doc:"The Zlib compression level."
            "level" Irmin.Private.Conf.(some int) None
        in
        let root = Irmin.Private.Conf.get config Irmin.Private.Conf.root in
        let level = Irmin.Private.Conf.get config level in
        Git_unix.FS.create ?root ?level ()

      (* Somehow pulls the config set by Store.init *)
      (* And creates a Git backend *)
      let create () = create @@ Irmin_git.config Config.shared
    end

    type t = K.t

    let rec of_adt (a:OM.t) : t Lwt.t  =
      let aostore = AO_store.create () in
      let aostore_add value =
        aostore >>= (fun ao_store -> AO_store.add ao_store value) in
      aostore_add =<<
      (match a with
       | OM.Empty -> Lwt.return @@ Empty
       | OM.Node {l;v;r;h} -> 
         (of_adt l >>= fun l' ->
          of_adt r >>= fun r' ->
          Lwt.return {l=l'; v; r=r'; h=Int64.of_int h})
         >>= ((fun n -> Lwt.return @@ (Node n))))

    let rec to_adt (k:t) : OM.t Lwt.t =
      AO_store.create () >>= fun ao_store ->
      AO_store.find ao_store k >>= fun t ->
      let t = from_just t in
      match t with
      | Empty -> Lwt.return @@ OM.Empty
      | Node {l;v;r;h} ->
        (to_adt l >>= fun l' ->
         to_adt r >>= fun r' ->
         Lwt.return {OM.l=l'; OM.v; OM.r=r'; OM.h=Int64.to_int h})
        >>= (fun n -> Lwt.return @@ (OM.Node n))

    let t = K.t

    let pp = K.pp

    let of_string = K.of_string

    let rec merge ~(old:t Irmin.Merge.promise) v1_k v2_k =
      let open Irmin.Merge.Infix in
      old () >>=* fun old_k ->
      let old_k = from_just old_k in
      to_adt old_k >>= fun oldv  ->
      to_adt v1_k >>= fun v1  ->
      to_adt v2_k >>= fun v2 ->
      let v = OM.merge3 oldv v1 v2 in
      of_adt v >>= fun merged_k ->
      Irmin.Merge.ok merged_k

    let merge = Irmin.Merge.(option (v t merge))
  end

  module BC_store = struct
    module Store = Irmin_unix.Git.FS.KV(M)
    module Sync = Irmin.Sync(Store)

    type t = Store.t

    let init ?root ?bare () =
      let config = Irmin_git.config Config.root in
      Store.Repo.v config

    let master (repo:Store.repo) = Store.master repo

    let clone t name = Store.clone t name

    let get_branch r ~branch_name = Store.of_branch r branch_name

    let merge s ~into = Store.merge s ~into

    let update t k v = Store.set t k v

    let read t k = Store.find t k
  end

  module Vpst = struct
    type store = BC_store.t
    type st = {master   : store;
               local    : store;
               name     : string;
               next_id  : int}
    type 'a t = st -> ('a * st) Lwt.t

    let info s = Irmin_unix.info "[repo %s] %s" Config.root s  

    let path = ["state"]

    let return (x : 'a) : 'a t = fun st -> Lwt.return (x,st)

    let bind (m1: 'a t) (f: 'a -> 'b t) : 'b t = 
      fun st -> (m1 st >>= fun (a,st') -> f a st')

    let with_init_version_do (v: OM.t) (m: 'a t) =
      Lwt_main.run 
        begin
          BC_store.init () >>= fun repo -> 
          BC_store.master repo >>= fun m_br -> 
          M.of_adt v >>= fun k ->
          let cinfo = info "creating state of master" in
          BC_store.update m_br path k ~info:cinfo >>= fun () ->
          BC_store.clone m_br "1_local" >>= fun t_br ->
          let st = {master=m_br; local=t_br; name="1"; next_id=1} in
          m st >>= fun (a,_) -> Lwt.return a
        end

    let with_init_forked_do (m: 'a t) = 
      BC_store.init () >>= fun repo -> 
      BC_store.master repo >>= fun m_br ->
      BC_store.clone m_br "1_local" >>= fun t_br ->
      let st = {master=m_br; local=t_br; name="1"; next_id=1} in
      m st >>= fun (a, _) -> Lwt.return a

    let fork_version (m: 'a t) : unit t = fun (st: st) ->
      let thread_f () = 
        let child_name = st.name^"_"^(string_of_int st.next_id) in
        let parent_m_br = st.master in
        (* Ideally, the following has to happen: *)
        (* BC_store.clone_force parent_m_br m_name >>= fun m_br -> *)
        (* But, we currently default to an SC mode. Master is global. *)
        let m_br = parent_m_br in
        BC_store.clone m_br (child_name^"_local") >>= fun t_br ->
        let new_st = {master = m_br; local  = t_br; name = child_name; next_id = 1} in
        m new_st in
      begin
        Lwt.async thread_f;
        Lwt.return ((), {st with next_id=st.next_id+1})
      end

    let get_latest_version () : OM.t t = fun (st: st) ->
      BC_store.read st.local path >>= fun k ->
      M.to_adt @@ from_just k >>= fun td ->
      Lwt.return (td,st)

    let sync_remote_version remote_uri ?v : OM.t t = fun (st: st) ->
      (* How do you commit the next version? Simply update path? *)
      (* 1. Commit to the local branch *)
      let cinfo = info "committing local state" in
      (match v with 
       | None -> Lwt.return ()
       | Some v -> 
         M.of_adt v >>= fun k -> 
         BC_store.update st.local path k cinfo) >>= fun () ->

      (* 2.. Pull from remote to master *)
      let cinfo = info (Printf.sprintf "Merging remote: %s" remote_uri) in
      BC_store.Sync.pull st.master (Irmin.remote_uri remote_uri) (`Merge  cinfo) >>= fun _ ->
      (* 2. Merge local master to the local branch *)
      let cinfo = info "Merging master into local" in
      BC_store.merge st.master ~into:st.local ~info:cinfo >>= fun _ ->
      (* 3. Merge local branch to the local master *)
      let cinfo = info "Merging local into master" in
      BC_store.merge st.local ~into:st.master ~info:cinfo >>= fun _ ->
      get_latest_version () st

    let sync_next_version ?v : OM.t t = fun (st: st) ->
      (* How do you commit the next version? Simply update path? *)
      (* 1. Commit to the local branch *)
      let cinfo = info "committing local state" in
      (match v with 
       | None -> Lwt.return ()
       | Some v -> 
         M.of_adt v >>= fun k -> 
         BC_store.update st.local path k cinfo) >>= fun () ->

      (* 2. Merge local master to the local branch *)
      let cinfo = info "Merging master into local" in
      BC_store.merge st.master ~into:st.local ~info:cinfo >>= fun _ ->
      (* 3. Merge local branch to the local master *)
      let cinfo = info "Merging local into master" in
      BC_store.merge st.local ~into:st.master ~info:cinfo >>= fun _ ->
      get_latest_version () st

    let liftLwt (m: 'a Lwt.t) : 'a t = fun st ->
      m >>= fun a -> Lwt.return (a,st)
  end
end