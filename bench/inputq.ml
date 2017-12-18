open Lwt.Infix 

let () = Lwt_log.add_rule "*" Lwt_log.Info;;
let () = Random.init 55;;

let pops = ref 0
let max_pops = Bench.max_pops

let start_time = ref 0.0
let end_time = ref 0.0

let sockaddr = 
  let address = Unix.inet_addr_loopback in
  let port =  3333 in
  Unix.ADDR_INET (address, port)

let handle_msg msg =
  let open Bench in
  match msg with
  | PopQ -> 
    let r = Random.bits () in
    if !start_time = 0.0 then start_time := Unix.gettimeofday ();
    if !pops <> max_pops then begin
        pops := !pops + 1;
        let exec_time = Unix.gettimeofday () -. !start_time in
        Lwt_log.ign_info_f "Popping %d is %d th exectim: %f" r !pops exec_time;
        PoppedQ (Int64.of_int r)
      end
    else begin
        if !end_time = 0.0 then end_time := Unix.gettimeofday ();
        let exec_time = !end_time -. !start_time in
        Lwt_log.ign_info_f "Execution time: %f Throuput: %f" exec_time (float max_pops /. exec_time);
        PoppedAll max_pops
      end
  | _ -> 
    Error "Unknown command"

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