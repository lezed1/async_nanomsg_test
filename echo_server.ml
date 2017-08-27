open Core
open Async
open Nanomsg
open Nanomsg_async

let run_server port =
  let soc =
    match socket Pair with
    | Ok s -> s
    | Error (e1, e2) -> failwith (e1 ^ e2)
  in

  let addr =
    let open Addr in
    bind_of_string ("tcp://*:" ^ (Int.to_string port))
  in

  let b = bind soc addr in

  let%map res = recv_string soc  in

  match res with
  | Core.Ok str -> print_endline str
  | Core.Error (e1, e2) -> failwith (e1 ^ e2)


let param =
  let open Command.Let_syntax in
  let%map port =
    Command.Param.flag "-port" ~doc:"PORT Port to listen on" (Command.Param.optional_with_default 12345 Command.Param.int)
  in
  fun () ->
    run_server port

let command = Command.async' ~summary:"Command summary!" param

let () = Command.run command
