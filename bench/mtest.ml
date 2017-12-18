open Lwt.Infix

let info = Irmin_unix.info

module Store = Irmin_unix.Git.FS.KV(Irmin.Contents.String)
module Sync = Irmin.Sync(Store)

let upstream = Irmin.remote_uri "git://localhost/tmp/repos/init.git"

let test () =
  let config = Irmin_git.config "/tmp/repos/init.git" in
  Store.Repo.v config >>= fun repo ->
  Store.master repo >>= fun t ->
  let cinfo = Irmin_unix.info "THE Ancestor" in
  Store.set t ["key"] "value" ~info:cinfo >>= fun _ ->
  
  let config = Irmin_git.config "/tmp/repos/test.git" in
  Store.Repo.v config >>= fun repo ->
  Store.master repo >>= fun t ->
  Sync.pull_exn t upstream `Set >>= fun _ ->
  Store.get t ["key"] >>= fun v ->
  Lwt_io.printf "%s\n" v

let () =
  Lwt_main.run (test ())
