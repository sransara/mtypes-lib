(* Utility functions *)

(*

Setting up the Git daemon:
git daemon --export-all --reuseaddr --base-path=/tmp/repos --verbose –enable=receive-pack

*)


module U = struct
  let string_of_list f l = "[ " ^ List.fold_left (fun a b -> a ^ (f b) ^ "; ") "" l ^ "]"
  let print_header h = Printf.printf "%s" ("\n" ^ h ^ "\n")
end

module MkConfig (Vars: sig val root: string end) = struct
  let root = Vars.root

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

module CMain = MkConfig(struct let root = "/tmp/repos/main.git" end) 
module MMain = Mset_avltree.MakeVersioned(CMain)(IntAtom)

let bob_f =
  let module M = MMain.OM in
  let module Vpst = MMain.Vpst in
  let (>>=) = Vpst.bind in
  Vpst.get_latest_version () >>= fun t0 ->
  let t0' =  M.add (Int64.of_int 101)  t0 in
  Vpst.liftLwt @@ Lwt_unix.sleep 0.4 >>= fun () ->
  Vpst.sync_next_version ~v:t0' >>= fun t1 ->
  let t1' =  M.add (Int64.of_int 202)  t1 in
  Vpst.liftLwt @@ Lwt_unix.sleep 0.1 >>= fun () ->
  Vpst.sync_next_version ~v:t1' >>= fun t2 ->
  let _ = Printf.printf "Bob: %s\n" (U.string_of_list IntAtom.to_string (M.elements t2)) in
  Vpst.liftLwt @@ Lwt_unix.sleep 1.1 >>= fun () ->
  Vpst.return ()

let alice_f =
  let module M = MMain.OM in
  let module Vpst = MMain.Vpst in
  let (>>=) = Vpst.bind in
  Vpst.get_latest_version () >>= fun t0 ->
  Vpst.fork_version bob_f >>= fun () ->
  let t0' =  M.add (Int64.of_int 32)  t0 in
  Vpst.liftLwt @@ Lwt_unix.sleep 0.4 >>= fun () ->
  Vpst.sync_next_version ~v:t0' >>= fun t1 ->
  let t1' =  M.add (Int64.of_int 42)  t1 in
  Vpst.liftLwt @@ Lwt_unix.sleep 0.1 >>= fun () ->
  Vpst.sync_next_version ~v:t1' >>= fun t2 ->
  let _ = Printf.printf "Alice: %s\n" (U.string_of_list IntAtom.to_string (M.elements t2)) in
  Vpst.liftLwt @@ Lwt_unix.sleep 1.1 >>= fun () ->
  Vpst.return ()

let main () = 
  let init_set = MMain.OM.empty in
  MMain.Vpst.with_init_version_do init_set alice_f;;

main ();;