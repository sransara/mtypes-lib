(* Utility functions *)

(*

Setting up the Git daemon:
git daemon --export-all --reuseaddr --verbose --enable=receive-pack

*)

Printexc.record_backtrace true

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

module CMain = MkConfig(struct let root = "/tmp/repos/init" end) 
module MMain = Mset_avltree.MakeVersioned(CMain)(IntAtom)

let n_procs = 2

open Lwt.Infix

let () = Lwt_log.add_rule "*" Lwt_log.Info;;

let inputq_address = 
  let address = Unix.inet_addr_loopback in
  let port =  3333 in
  Unix.ADDR_INET (address, port)


module MagicModule (Hack: sig val i: int end) = struct
  let uri = "/tmp/repos/r" ^ string_of_int Hack.i ^ ".git"
  let ruri = "git://localhost/tmp/repos/r" ^ string_of_int ((Hack.i + 1) mod n_procs) ^ ".git"

  let mrui = "git://localhost" ^ CMain.root

  module C = MkConfig(struct let root = uri end) 
  module M = Mset_avltree.MakeVersioned(C)(IntAtom)
  module Vpst = M.Vpst

  let funf = fun () ->
    let (>>=) = Vpst.bind in
    Vpst.get_latest_version () >>= fun t0 -> 
    let t0' =  M.OM.add (Int64.of_int @@ Random.bits ())  t0 in
    Vpst.liftLwt @@ Lwt_unix.sleep 0.4 >>= fun () ->
    Vpst.sync_next_version ~v:t0' >>= fun t1 ->
    let t1' =  M.OM.add (Int64.of_int @@ Random.bits ())  t1 in
    Vpst.liftLwt @@ Lwt_unix.sleep 0.1 >>= fun () ->
    Vpst.sync_next_version ~v:t1' >>= fun t2 ->
    let _ = Printf.printf "Alice: %s\n" (U.string_of_list IntAtom.to_string (M.OM.elements t2)) in
    Vpst.liftLwt @@ Lwt_unix.sleep 1.1 >>= fun () ->
    Vpst.return ()

  let run () = 
    Random.init Hack.i;
    Vpst.with_init_remote_do mrui @@ funf ()
end

let main_f =
  let module Vpst = MMain.Vpst in
  let (>>=) = Vpst.bind in
  Vpst.get_latest_version () >>= fun t0 -> 
  let rec aux i n =
    match Lwt_unix.fork () with
    | 0 -> 
      let module MM = MagicModule(struct let i = i end) in
      MM.run ()
    | pid ->
      if i < (n-1) then aux (i+1) n
      else () in
  let _ = aux 0 n_procs in
  Vpst.return ()

let main () = MMain.Vpst.with_init_version_do MMain.OM.Empty main_f

let () = main ()
