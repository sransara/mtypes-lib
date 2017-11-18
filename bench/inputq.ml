open Lwt.Infix 

let () = Lwt_log.add_rule "*" Lwt_log.Info;;
let () = Random.init 55;;

let pops = ref 0

let sockaddr = 
  let address = Unix.inet_addr_loopback in
  let port =  3333 in
  Unix.ADDR_INET (address, port)

let handle_msg msg =
  let open Protocol in
  match msg with
  | PopQ -> 
    let r = Random.bits () in
    pops := !pops + 1;
    Lwt_log.ign_info_f "Popping %d" r;
    PoppedQ r
  | _ -> 
    Error "Unknown command"

let handle_s_msg msg =
  let open Protocol in
  msg |> string_to_message |> handle_msg |> message_to_string

let rec handle input output =
  Lwt_io.read_value input >>= fun msg ->
  Lwt_io.write_value  output (handle_msg msg) >>=  fun () ->
  handle input output

let server = 
  let handler _ (input, output) =
    Lwt.catch (fun () -> handle input output) (fun _exn -> Lwt.return_unit) in
  Lwt_io.establish_server_with_client_address sockaddr handler

let () = 
  let (t, _) = Lwt.wait () in
  Lwt_main.run t