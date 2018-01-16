open Lwt.Infix

let () = Lwt_log.add_rule "*" Lwt_log.Info;;

let inputq_address = 
  let address = Unix.inet_addr_loopback in
  let port =  3333 in
  Unix.ADDR_INET (address, port)

let storage_address =
  let address = Unix.inet_addr_loopback in
  let port =  4444 in
  Unix.ADDR_INET (address, port)

let process x = x

let storage_handle input output x =
  let processed_x = process x in
  let open Bench in
  Lwt_io.write_value output (Store processed_x)  >>= fun () ->
  Lwt_io.read_value input >>= fun (msg:message) ->
  match msg with
  | Stored x -> Lwt_log.info_f "Stored element #%d" x
  | _ -> Lwt_log.info_f "Errr"

let rec inputq_handle input output =
  let open Bench in
  Lwt_io.write_value output PopQ >>= fun () ->
  Lwt_io.read_value input >>= fun (msg:message) ->
  (match msg with
    | PoppedQ x ->
      let handler (input, output) =
        Lwt.catch (fun () -> storage_handle input output x) (fun _exn -> Lwt.return_unit) in
      Lwt_io.with_connection storage_address handler >>= fun _ ->
      inputq_handle input output
    | PoppedAll x -> 
      Lwt.return_unit
    | _ -> 
      Lwt_log.info_f "Errr" >>= fun _ ->
      inputq_handle input output
   )

let client =
  let handler (input, output) =
    Lwt.catch (fun () -> inputq_handle input output) (fun _exn -> Lwt.return_unit) in
  Lwt_io.with_connection inputq_address handler

let () = 
  let (t, _) = Lwt.wait () in
  Lwt_main.run t
