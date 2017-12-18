(* Utility functions *)

(*

Setting up the Git daemon:
git daemon --export-all --reuseaddr --verbose --enable=receive-pack

*)

let processing_time = Bench.processing_time (* simulated by cpu sleep *)
let sync_freq = Bench.m_sync_freq
let n_procs = Bench.m_replicas

module U = struct
  let string_of_list f l = "[ " ^ List.fold_left (fun a b -> a ^ (f b) ^ "; ") "" l ^ "]"
  let print_header h = Printf.printf "%s" ("\n" ^ h ^ "\n")
end

module MkConfig (Vars: sig val root: string end) : Mset_avltree.Config = struct
  let root = Vars.root
  let shared = "/tmp/repos/shared.git"

  let init () =
    let _ = Sys.command (Printf.sprintf "rm -rf %s" root) in
    let _ = Sys.command (Printf.sprintf "mkdir -p %s" root) in
    ()
end

module IntAtom = struct
  type t = int64
  let compare = Pervasives.compare
  let t = Irmin.Type.int64
  let to_string = Int64.to_string
  let of_string = Int64.of_string
end

module CInit = MkConfig(struct let root = "/tmp/repos/init.git" end) 
module MInit = Mset_avltree.MakeVersioned(CInit)(IntAtom)


open Lwt.Infix

let () = Lwt_log.add_rule "*" Lwt_log.Info;;

let inputq_address = 
  let address = Unix.inet_addr_loopback in
  let port =  3333 in
  Unix.ADDR_INET (address, port)


module WorkerModule (Hack: sig val r: int val rr: int end) = struct
  let repon = Hack.r
  let rrepon = Hack.rr

  let uri = "/tmp/repos/r" ^ string_of_int repon ^ ".git"
  let ruri = "git://localhost/tmp/repos/r" ^ string_of_int rrepon ^ ".git"

  let muri = "git://localhost" ^ CInit.root

  module C = MkConfig(struct let root = uri end) 
  module M = Mset_avltree.MakeVersioned(C)(IntAtom)
  module Vpst = M.Vpst

  let rec inputq_handle input output n =
    let open Bench in
    let (>>=) = Vpst.bind in
    Vpst.liftLwt @@ Lwt_io.write_value output PopQ >>= fun () ->
    Vpst.liftLwt @@ Lwt_io.read_value input >>= fun (msg:message) ->
    match msg with
    | PoppedQ x ->
      Vpst.get_latest_version () >>= fun v ->
      Vpst.liftLwt @@ Lwt_unix.sleep processing_time >>= fun _ ->
      let v' = M.OM.add x v in
      (if n = 1 then Vpst.sync_remote_version ruri ~v:v' 
      else Vpst.sync_next_version ~v:v')  >>= fun _ ->
      Vpst.liftLwt @@ Lwt_log.info_f "Stored element %Ld" x >>= fun _ ->
      inputq_handle input output ((n + 1) mod sync_freq)
    | PoppedAll x -> Vpst.return ()
    | _ -> 
      inputq_handle input output n

  let main () =
    let handler (input, output) =
      Vpst.with_init_forked_do (inputq_handle input output 0) in
    Lwt_io.with_connection inputq_address handler

  let init () =
    let open M in
    BC_store.init () >>= fun repo -> 
    BC_store.master repo >>= fun m_br ->
    BC_store.Sync.pull_exn m_br (Irmin.remote_uri muri) `Set

end

let () = 
  match Sys.argv with
  | [| me; "init" |] ->
    Lwt_main.run begin
      let open MInit in
      BC_store.init () >>= fun repo -> 
      BC_store.master repo >>= fun m_br -> 
      M.of_adt OM.Empty >>= fun k ->
      let cinfo = Irmin_unix.info "THE Ancestor" in
      BC_store.update m_br ["state"] k ~info:cinfo >>= fun _ -> 
      let rec aux i n =
        let _ = Sys.command (Printf.sprintf "%s init worker %d" me i) in
        if i < (n-1) then aux (i+1) n
        else () in
      aux 0 n_procs;
      Lwt.return_unit
    end
  | [| me; "init"; "worker"; repon; |] ->
    let repon = int_of_string repon in
    let module WM = WorkerModule(struct let r = repon let rr = 0 end) in
    Lwt_main.run begin
      WM.init ()
    end
  | [| me; "worker"; repon; rrepon |] ->
    let repon = int_of_string repon in
    let rrepon = int_of_string rrepon in
    let module WM = WorkerModule(struct let r = repon let rr = rrepon end) in
    Lwt_main.run begin
      WM.main ()
    end
  | [| me; |] ->
    let _ = Sys.command (Printf.sprintf "%s init" me) in
    let rec aux i n =
      match Unix.fork () with
      | 0 ->
        Unix.execv me [| me; "worker"; string_of_int i; string_of_int ((i+1) mod n) |]
      | pid ->
        if i < (n-1) then aux (i+1) n
        else () in
    aux 0 n_procs
  | x -> exit(1)