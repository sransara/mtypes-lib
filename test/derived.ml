module Canvas =
  struct
    type pixel = {
      r: char ;
      g: char ;
      b: char }[@@derive ezjsonm]
    let default_pixel =
      { r = (Char.chr 255); g = (Char.chr 255); b = (Char.chr 255) } 
    type quadrants = {
      tl_t: t ;
      tr_t: t ;
      bl_t: t ;
      br_t: t }
    and t =
      | N of pixel 
      | B of quadrants [@@derive versioned]
    type canvas = {
      max_x: int ;
      max_y: int ;
      t: t }
    type loc = {
      x: int ;
      y: int }
    let blank = N default_pixel 
    let plain px = N px 
    let new_canvas max_x max_y = { max_x; max_y; t = blank } 
    let rec set_px canvas loc px =
      if (canvas.max_x <= loc.x) && (canvas.max_y <= loc.y)
      then N px
      else
        (let mid_x = canvas.max_x / 2  in
         let mid_y = canvas.max_y / 2  in
         match ((loc.x <= mid_x), (loc.y <= mid_y)) with
         | (true ,true ) ->
             let tl_t =
               match canvas.t with | N px -> N px | B { tl_t } -> tl_t  in
             let tl_c = { max_x = mid_x; max_y = mid_y; t = tl_t }  in
             let tl_t' = set_px tl_c loc px  in
             let t' =
               match canvas.t with
               | N px ->
                   B
                     {
                       tl_t = tl_t';
                       tr_t = (N px);
                       bl_t = (N px);
                       br_t = (N px)
                     }
               | B y -> B { y with tl_t = tl_t' }  in
             t'
         | (false ,true ) ->
             let tr_t =
               match canvas.t with | N px -> N px | B { tr_t } -> tr_t  in
             let tr_c =
               { max_x = (canvas.max_x - mid_x); max_y = mid_y; t = tr_t }
                in
             let loc' = { loc with x = (loc.x - mid_x) }  in
             let tr_t' = set_px tr_c loc' px  in
             let t' =
               match canvas.t with
               | N px ->
                   B
                     {
                       tl_t = (N px);
                       tr_t = tr_t';
                       bl_t = (N px);
                       br_t = (N px)
                     }
               | B y -> B { y with tr_t = tr_t' }  in
             t'
         | (true ,false ) ->
             let bl_t =
               match canvas.t with | N px -> N px | B { bl_t } -> bl_t  in
             let bl_c =
               { max_x = mid_x; max_y = (canvas.max_y - mid_y); t = bl_t }
                in
             let loc' = { loc with y = (loc.y - mid_y) }  in
             let bl_t' = set_px bl_c loc' px  in
             let t' =
               match canvas.t with
               | N px ->
                   B
                     {
                       tl_t = (N px);
                       tr_t = (N px);
                       bl_t = bl_t';
                       br_t = (N px)
                     }
               | B y -> B { y with bl_t = bl_t' }  in
             t'
         | (false ,false ) ->
             let br_t =
               match canvas.t with | N px -> N px | B { br_t } -> br_t  in
             let br_c =
               {
                 max_x = (canvas.max_x - mid_x);
                 max_y = (canvas.max_y - mid_y);
                 t = br_t
               }  in
             let loc' = { x = (loc.x - mid_x); y = (loc.y - mid_y) }  in
             let br_t' = set_px br_c loc' px  in
             let t' =
               match canvas.t with
               | N px ->
                   B
                     {
                       tl_t = (N px);
                       tr_t = (N px);
                       bl_t = (N px);
                       br_t = br_t'
                     }
               | B y -> B { y with br_t = br_t' }  in
             t')
      
    let set_px canvas loc px =
      if (loc.x > canvas.max_x) || (loc.y > canvas.max_y)
      then failwith "set_px: location out of canvas bounds"
      else (let t' = set_px canvas loc px  in { canvas with t = t' }) 
    let rec get_px canvas loc =
      match canvas.t with
      | N px -> px
      | B y ->
          let mid_x = canvas.max_x / 2  in
          let mid_y = canvas.max_y / 2  in
          (match ((loc.x <= mid_x), (loc.y <= mid_y)) with
           | (true ,true ) ->
               let tl_t =
                 match canvas.t with | N px -> N px | B { tl_t } -> tl_t  in
               let tl_c = { max_x = mid_x; max_y = mid_y; t = tl_t }  in
               get_px tl_c loc
           | (false ,true ) ->
               let tr_t =
                 match canvas.t with | N px -> N px | B { tr_t } -> tr_t  in
               let tr_c =
                 { max_x = (canvas.max_x - mid_x); max_y = mid_y; t = tr_t }
                  in
               let loc' = { loc with x = (loc.x - mid_x) }  in
               get_px tr_c loc'
           | (true ,false ) ->
               let bl_t =
                 match canvas.t with | N px -> N px | B { bl_t } -> bl_t  in
               let bl_c =
                 { max_x = mid_x; max_y = (canvas.max_y - mid_y); t = bl_t }
                  in
               let loc' = { loc with y = (loc.y - mid_y) }  in
               get_px bl_c loc'
           | (false ,false ) ->
               let br_t =
                 match canvas.t with | N px -> N px | B { br_t } -> br_t  in
               let br_c =
                 {
                   max_x = (canvas.max_x - mid_x);
                   max_y = (canvas.max_y - mid_y);
                   t = br_t
                 }  in
               let loc' = { x = (loc.x - mid_x); y = (loc.y - mid_y) }  in
               get_px br_c loc')
      
    let color_mix px1 px2 =
      (let f = Char.code  in
       let h x y = Char.chr @@ ((x + y) / 2)  in
       let (r1,g1,b1) = ((f px1.r), (f px1.g), (f px1.b))  in
       let (r2,g2,b2) = ((f px2.r), (f px2.g), (f px2.b))  in
       let (r,g,b) = ((h r1 r2), (h g1 g2), (h b1 b2))  in { r; g; b } : 
      pixel) 
    let b_of_n px =
      B { tl_t = (N px); tr_t = (N px); bl_t = (N px); br_t = (N px) } 
    let make_b (tl,tr,bl,br) =
      B { tl_t = tl; tr_t = tr; bl_t = bl; br_t = br } 
    let rgb px = { r = px; g = px; b = px } 
    let char_to_json x = Tc.Int.to_json @@ (Char.code x) 
    let char_of_json x = Char.chr @@ (Tc.Int.of_json x) 
    let rec merge old v1 v2 =
      if v1 = v2
      then v1
      else
        if v1 = old
        then v2
        else
          if v2 = old
          then v1
          else
            (match (old, v1, v2) with
             | (_,B _,N px2) -> (merge old v1) @@ (b_of_n px2)
             | (_,N px1,B _) -> merge old (b_of_n px1) v2
             | (N px,B _,B _) -> merge (b_of_n px) v1 v2
             | (B x,B x1,B x2) ->
                 let tl_t' = merge x.tl_t x1.tl_t x2.tl_t  in
                 let tr_t' = merge x.tr_t x1.tr_t x2.tr_t  in
                 let bl_t' = merge x.bl_t x1.bl_t x2.bl_t  in
                 let br_t' = merge x.br_t x1.br_t x2.br_t  in
                 B { tl_t = tl_t'; tr_t = tr_t'; bl_t = bl_t'; br_t = br_t' }
             | (_,N px1,N px2) -> let px' = color_mix px1 px2  in N px')
      
    let rec print min_x min_y max_x max_y t =
      if (min_x > max_x) || (min_y > max_y)
      then ()
      else
        (match t with
         | N px when not (px = default_pixel) ->
             if (min_x = max_x) && (min_y = max_y)
             then
               Printf.printf "<%d,%d>: (%d,%d,%d)\n" min_x min_y
                 (Char.code px.r) (Char.code px.g) (Char.code px.b)
             else
               Printf.printf "<%d,%d> to <%d,%d>: (%d,%d,%d)\n" min_x min_y
                 max_x max_y (Char.code px.r) (Char.code px.g)
                 (Char.code px.b)
         | N px -> ()
         | B { tl_t; tr_t; bl_t; br_t } ->
             let (mid_x,mid_y) =
               ((min_x + (((max_x - min_x) + 1) / 2)),
                 (min_y + (((max_y - min_y) + 1) / 2)))
                in
             (print min_x min_y mid_x mid_y tl_t;
              print (mid_x + 1) min_y max_x mid_y tr_t;
              print min_x (mid_y + 1) mid_x max_y bl_t;
              print (mid_x + 1) (mid_y + 1) max_x max_y br_t))
      
    let print { max_x; max_y; t } = print 0 0 max_x max_y t 
    let print c =
      for x = 1 to c.max_x do
        for y = 1 to c.max_y do
          let px = get_px c { x; y }  in
          if not (px = default_pixel)
          then
            Printf.printf "<%d,%d>: (%d,%d,%d)\n" x y (Char.code px.r)
              (Char.code px.g) (Char.code px.b)
          else ()
        done
      done 
  end[@@derive_versioned ]
module ICanvas =
  struct
    open Lwt.Infix
    open Irmin_unix
    module OM = Canvas
    open OM
    module G = Git_unix.FS
    module K = Irmin.Hash.SHA1
    module M = Irmin.Merge
    type path = string list
    let from_just =
      function | Some x -> x | None  -> failwith "Expected Some. Got None." 
    module type MERGEABLE  =
      sig
        include Irmin.Contents.S
        val to_string : t -> string
      end
    module type MCANVAS  =
      sig
        include MERGEABLE
        val of_adt : OM.t -> t Lwt.t
        val to_adt : t -> Canvas.t Lwt.t
      end
    module MCanvas : MCANVAS =
      struct

      let pp = assert false
      let t = assert false

        type quadrants = {
          tl_t: K.t ;
          tr_t: K.t ;
          bl_t: K.t ;
          br_t: K.t }
        and madt =
          | N of pixel 
          | B of quadrants 
        module JsonConvert =
          struct
            let rec pixel_to_json =
              function
              | { r; g; b } ->
                  `O
                    [("r", (char_to_json r));
                    ("g", (char_to_json g));
                    ("b", (char_to_json b))]
              
            let rec quadrants_to_json =
              function
              | { tl_t; tr_t; bl_t; br_t } ->
                  `O
                    [("tl_t", (K.to_json tl_t));
                    ("tr_t", (K.to_json tr_t));
                    ("bl_t", (K.to_json bl_t));
                    ("br_t", (K.to_json br_t))]
            
            and madt_to_json =
              function
              | N a0 -> `A [`String "N"; pixel_to_json a0]
              | B a0 -> `A [`String "B"; quadrants_to_json a0]
            
            let rec pixel_of_json =
              function
              | `O (("r",r)::("g",g)::("b",b)::[]) ->
                  {
                    r = (char_of_json r);
                    g = (char_of_json g);
                    b = (char_of_json b)
                  }
              | j -> Ezjsonm.parse_error j "pixel_of_json" 
            let rec quadrants_of_json =
              function
              | `O
                  (("tl_t",tl_t)::("tr_t",tr_t)::("bl_t",bl_t)::("br_t",br_t)::[])
                  ->
                  {
                    tl_t = (K.of_json tl_t);
                    tr_t = (K.of_json tr_t);
                    bl_t = (K.of_json bl_t);
                    br_t = (K.of_json br_t)
                  }
              | j -> Ezjsonm.parse_error j "quadrants_of_json"
            
            and madt_of_json =
              function
              | `A ((`String "N")::a0::[]) -> N (pixel_of_json a0)
              | `A ((`String "B")::a0::[]) -> B (quadrants_of_json a0)
              | j -> Ezjsonm.parse_error j "madt_of_json"
            
          end
        module AO_value =
          struct
            type t = madt
            let equal t1 t2 = t1 = t2 
            let compare = compare 
            let hash = Hashtbl.hash 
            let to_json = JsonConvert.madt_to_json 
            let of_json = JsonConvert.madt_of_json 
            let to_string t = Ezjsonm.to_string (to_json t) 
            let of_string s = of_json (Ezjsonm.from_string s) 
            let write t buf =
              let str = to_string t  in
              let len = String.length str  in
              Cstruct.blit_from_string str 0 buf 0 len; Cstruct.shift buf len 
            let read buf =
              (Mstruct.get_string buf (Mstruct.length buf)) |> of_string 
            let size_of t = let str = to_string t  in String.length str 
          end
        module AO_store =
          struct
            module S = (((Irmin_git.AO)(G))(K))(AO_value)
            include S
            let create config =
              let level =
                Irmin.Private.Conf.key ~doc:"The Zlib compression level."
                  "level" (let open Irmin.Private.Conf in some int) None
                 in
              let root =
                Irmin.Private.Conf.get config Irmin.Private.Conf.root  in
              let level = Irmin.Private.Conf.get config level  in
              G.create ?root ?level () 
            let create () = create @@ (Irmin_git.config ()) 
          end
        let rec of_adt t =
          let aostore = AO_store.create ()  in
          let aostore_add value =
            aostore >>= (fun ao_store  -> AO_store.add ao_store value)  in
          aostore_add =<<
            (match t with
             | Canvas.N a0 -> Lwt.return @@ (N a0)
             | Canvas.B a0 ->
                 (match a0 with
                  | { tl_t; tr_t; bl_t; br_t;_} ->
                      (of_adt tl_t) >>=
                        ((fun tl_t'  ->
                            (of_adt tr_t) >>=
                              (fun tr_t'  ->
                                 (of_adt bl_t) >>=
                                   (fun bl_t'  ->
                                      (of_adt br_t) >>=
                                        (fun br_t'  ->
                                           Lwt.return @@
                                             {
                                               tl_t = tl_t';
                                               tr_t = tr_t';
                                               bl_t = bl_t';
                                               br_t = br_t'
                                             }))))))
                   >>= ((fun a0'  -> Lwt.return @@ (B a0'))))
          
        let rec to_adt t_key =
          (AO_store.create ()) >>=
            (fun ao_store  ->
               (AO_store.read ao_store t_key) >>=
                 (fun t  ->
                    let t = from_just t  in
                    match t with
                    | N a0 -> Lwt.return @@ (Canvas.N a0)
                    | B a0 ->
                        (match a0 with
                         | { tl_t; tr_t; bl_t; br_t;_} ->
                             (to_adt tl_t) >>=
                               ((fun tl_t'  ->
                                   (to_adt tr_t) >>=
                                     (fun tr_t'  ->
                                        (to_adt bl_t) >>=
                                          (fun bl_t'  ->
                                             (to_adt br_t) >>=
                                               (fun br_t'  ->
                                                  Lwt.return @@
                                                    {
                                                      Canvas.tl_t = tl_t';
                                                      Canvas.tr_t = tr_t';
                                                      Canvas.bl_t = bl_t';
                                                      Canvas.br_t = br_t'
                                                    }))))))
                          >>= ((fun a0'  -> Lwt.return @@ (Canvas.B a0')))))
          
        exception MergeConflict 
        let rec merge old_k v1_k v2_k =
          (to_adt old_k) >>=
            (fun old  ->
               (to_adt v1_k) >>=
                 (fun v1  ->
                    (to_adt v2_k) >>=
                      (fun v2  -> let v = OM.merge old v1 v2  in of_adt v)))
          
        type t = K.t
        let equal t1 t2 = true 
        let compare = compare 
        let hash = Hashtbl.hash 
        let to_json k = `A [`String (K.to_hum k)] 
        let of_json =
          function
          | `A ((`String kstr)::[]) -> K.of_hum kstr
          | j -> Ezjsonm.parse_error j "MList_contents.C.of_json" 
        let to_string t = Ezjsonm.to_string (to_json t) 
        let of_string s = of_json (Ezjsonm.from_string s) 
        let write t buf =
          let str = to_string t  in
          let len = String.length str  in
          Cstruct.blit_from_string str 0 buf 0 len; Cstruct.shift buf len 
        let read buf =
          (Mstruct.get_string buf (Mstruct.length buf)) |> of_string 
        let size_of t = let str = to_string t  in String.length str 
        module Path = Irmin.Path.String_list
        let merge : Path.t -> t option Irmin.Merge.t =
          let merge' path ~old  (v1 : t option) (v2 : t option) =
            (let old =
               match Lwt_main.run @@ (old ()) with
               | `Ok (Some x) -> x
               | _ -> failwith "Impossible"  in
             try
               (merge (from_just old) (from_just v1) (from_just v2)) >>=
                 (fun k  -> Lwt.return @@ (`Ok (Some k)))
             with
             | MergeConflict  ->
                 Lwt.return @@
                   (`Conflict "TreeDoc: Incompatible concurrent versions") : 
            t option Irmin.Merge.result Lwt.t)  in
          merge' 
      end 
    module BC_store =
      struct
        module MA = MCanvas
        module Path = Irmin.Path.String_list
        module Store =
          (((Irmin_git.FS)(MA))(Irmin.Ref.String))(Irmin.Hash.SHA1)
        type repo = Store.Repo.t
        type branch = string -> Store.t
        type path = string list
        let init ?root  ?bare  () =
          let config = Irmin_git.config ?root ?bare ()  in
          Store.Repo.create config 
        let master (repo : repo) = (Store.master task repo : branch Lwt.t) 
        let clone_force t name = Store.clone_force task (t "cloning") name 
        let get_branch r ~branch_name  =
          Store.of_branch_id task branch_name r 
        let merge b ~into  = Store.merge_exn "" b ~into 
        let get_branch_name b = Store.name (b "name") 
        let update = Store.update 
        let read = Store.read 
      end
    module Vpst :
      sig
        type 'a t
        val return : 'a -> 'a t
        val bind : 'a t -> ('a -> 'b t) -> 'b t
        val with_init_version_do : Canvas.t -> 'a t -> 'a
        val fork_version : 'a t -> unit t
        val get_latest_version : unit -> Canvas.t t
        val sync_next_version : ?v:Canvas.t -> Canvas.t t
        val liftLwt : 'a Lwt.t -> 'a t
      end =
      struct
        module MA = MCanvas
        module BC_store = BC_store
        type branch = BC_store.branch
        type st =
          {
          master: branch ;
          local: branch ;
          name: string ;
          next_id: int }
        type 'a t = st -> ('a * st) Lwt.t
        let path = ["state"] 
        let return (x : 'a) = (fun st  -> Lwt.return (x, st) : 'a t) 
        let bind (m1 : 'a t) (f : 'a -> 'b t) =
          (fun st  -> (m1 st) >>= (fun (a,st')  -> f a st') : 'b t) 
        let with_init_version_do v (m : 'a t) =
          Lwt_main.run
            ((BC_store.init ()) >>=
               (fun repo  ->
                  (BC_store.master repo) >>=
                    (fun m_br  ->
                       let m_store = m_br "creating state on master"  in
                       (MA.of_adt v) >>=
                         (fun k  ->
                            (BC_store.update m_store path k) >>=
                              (fun ()  ->
                                 (BC_store.clone_force m_br "1_local") >>=
                                   (fun t_br  ->
                                      let st =
                                        {
                                          master = m_br;
                                          local = t_br;
                                          name = "1";
                                          next_id = 1
                                        }  in
                                      (m st) >>= (fun (a,_)  -> Lwt.return a)))))))
          
        let fork_version (m : 'a t) =
          (fun (st : st)  ->
             let thread_f () =
               let child_name = st.name ^ ("_" ^ (string_of_int st.next_id))
                  in
               let parent_m_br = st.master  in
               let m_br = parent_m_br  in
               (BC_store.clone_force m_br (child_name ^ "_local")) >>=
                 (fun t_br  ->
                    let new_st =
                      {
                        master = m_br;
                        local = t_br;
                        name = child_name;
                        next_id = 1
                      }  in
                    m new_st)
                in
             Lwt.async thread_f;
             Lwt.return ((), { st with next_id = (st.next_id + 1) }) : 
          unit t) 
        let get_latest_version () (st : st) =
          let bc_store = st.local "reading local state"  in
          (BC_store.read bc_store path) >>=
            (fun k  ->
               (MA.to_adt @@ (from_just k)) >>=
                 (fun td  -> Lwt.return (td, st)))
          
        let sync_next_version ?v  (st : st) =
          let bc_store = st.local "committing local state"  in
          (match v with
           | None  -> Lwt.return ()
           | Some v ->
               (MA.of_adt v) >>=
                 ((fun k  -> BC_store.update bc_store path k)))
            >>=
            (fun ()  ->
               (BC_store.merge st.master ~into:(st.local)) >>=
                 (fun ()  ->
                    (BC_store.merge st.local ~into:(st.master)) >>=
                      (fun ()  -> get_latest_version () st)))
          
        let liftLwt (m : 'a Lwt.t) =
          (fun st  -> m >>= (fun a  -> Lwt.return (a, st)) : 'a t) 
      end 
  end
