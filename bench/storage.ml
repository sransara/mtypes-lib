open Lwt.Infix

let () = Lwt_log.add_rule "*" Lwt_log.Info;;

let sockaddr = 
  let address = Unix.inet_addr_loopback in
  let port =  4444 in
  Unix.ADDR_INET (address, port)

let counter = ref 0
let file = "ff"
module IntSet = Set.Make(Int64)

let init_file = 
  let oc = open_out file in
  Marshal.to_channel oc IntSet.empty [];
  close_out oc

let get_filename () = file ^ string_of_int !counter 

let write_to_file s = 
  let oc = open_out (get_filename ()) in
  Marshal.to_channel oc s [];
  counter := !counter + 1;
  close_out oc

let handle_msg msg =
  let open Bench in
  match msg with
  | Store x ->
    let ic = open_in file in
    let s = Marshal.from_channel ic in
    close_in ic;
    let s' = IntSet.add x s in
    write_to_file s';
    Stored !counter
  | _ -> Error "Unknown command"

let rec handle input output =
  Lwt_io.read_value input >>= fun msg ->
  Lwt_io.write_value output (handle_msg msg) >>= fun () -> 
  handle input output

let server = 
  let handle_request _ (input, output) =
    Lwt.catch (fun () -> handle input output) (fun _exn -> Lwt.return_unit) in
  Lwt_io.establish_server_with_client_address sockaddr handle_request

let () = 
  let (t, _) = Lwt.wait () in
  Lwt_main.run t