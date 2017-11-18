open Lwt.Infix

let () = Lwt_log.add_rule "*" Lwt_log.Info;;

let sockaddr = 
  let address = Unix.inet_addr_loopback in
  let port =  4444 in
  Unix.ADDR_INET (address, port)

module Gset : sig 
  type t
  val create: t
  val add: t -> int  -> int
  val all: t -> int list
end 
= struct
  module IntSet = Set.Make(struct type t = int let compare = compare end)
  type t = {mutable set: IntSet.t; mutable counter: int}
  let create = {set = IntSet.empty; counter = 0}
  let add s x = 
    s.set <- IntSet.add x s.set;
    s.counter <- s.counter + 1;
    s.counter
  let all s = IntSet.elements s.set
end

let gset = Gset.create

let handle_msg msg =
  let open Protocol in
  match msg with
  | Store x ->
    Lwt_log.ign_info_f "Storing %d" x;
    Stored (Gset.add gset x)
  | _ -> Error "Unknown command"

let handle_s_msg msg =
  let open Protocol in
  msg |> string_to_message |> handle_msg |> message_to_string

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