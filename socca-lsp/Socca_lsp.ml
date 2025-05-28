(**************************************************************************)
(*                                                                        *)
(*                                 VSRocq                                  *)
(*                                                                        *)
(*                   Copyright INRIA and contributors                     *)
(*       (see version control and README file for authors & dates)        *)
(*                                                                        *)
(**************************************************************************)
(*                                                                        *)
(*   This file is distributed under the terms of the MIT License.         *)
(*   See LICENSE file.                                                    *)
(*                                                                        *)
(**************************************************************************)

(** This toplevel implements an LSP-based server language for VsCode,
    used by the VsRocq extension. *)

open Logger


module type Manager = sig
  type event
  val init : unit -> event Sel.Event.t list
  val handle_event : event -> event Sel.Event.t list
  val print_event : Format.formatter -> event -> unit
end

module LspManager : Manager = struct
  type event =
    | Receive of Jsonrpc.Packet.t option
    | Send of Jsonrpc.Packet.t

  let handle_raw_request = function
  | Ok raw ->
    begin
    match Channel.raw_to_rpc (Bytes.to_string raw) with
    | Some pkt -> Receive (Some pkt)
    | None ->
      log "Failed to decode JSON request";
      Receive None
    end
  | Error exn ->
    log ("Failed to read message: " ^ Printexc.to_string exn);
    (* do not remove this line otherwise the server stays running in some scenarios *)
    exit 0

  let init () =
    let events = Channel.receive_raw_request Channel.std_channel handle_raw_request in
    [events]

  let print_event _fmt = function
    | Receive _ -> log "Receive event"
    | Send _ -> log "Send event"

  let handle_event e=
    match e with
    | _ -> print_event Format.std_formatter e; []

end

module Make(Manager:Manager) = struct
let loop () =
  let events = Manager.init () in
  let rec loop (todo : Manager.event Sel.Todo.t) =
    (*log fun () -> "looking for next step";*)
    flush_all ();
    let ready, todo = Sel.pop todo in
    let nremaining = Sel.Todo.size todo in
    log (Format.asprintf "Main loop event ready: %a, %d events waiting\n\n" Manager.print_event ready nremaining);
    log ("==========================================================");
    log (Format.asprintf "Todo events: %a" (Sel.Todo.pp Manager.print_event) todo);
    log ("==========================================================\n\n");
    let new_events = Manager.handle_event ready in
    let todo = Sel.Todo.add todo new_events in
    log ("==========================================================");
    log (Format.asprintf "New Todo events: %a" (Sel.Todo.pp Manager.print_event) todo);
    log ("==========================================================\n\n");
    loop todo
  in
  let todo = Sel.Todo.add Sel.Todo.empty events in
  try loop todo
  with exn ->
    log "Exception raised."
end

module LspLoop = Make(LspManager)

let () =
  log "Starting the main loop.";
  LspLoop.loop()


(* [%%if rocq = "8.18" || rocq = "8.19" || rocq = "8.20"]
let _ =
  Coqinit.init_ocaml ();
  log (fun () -> "------------------ begin ---------------");
  let cwd = Unix.getcwd () in
  let opts = Args.get_local_args  cwd in
  let _injections = Coqinit.init_runtime opts in
  Safe_typing.allow_delayed_constants := true; (* Needed to delegate or skip proofs *)
  Flags.load_vos_libraries := true;
  Sys.(set_signal sigint Signal_ignore);
  loop ()
[%%else]

let () =
  Coqinit.init_ocaml ();
  log (fun () -> "------------------ begin ---------------");
  let cwd = Unix.getcwd () in
  let opts = Args.get_local_args cwd in
  let () = Coqinit.init_runtime ~usage:(Args.usage ()) opts in
  Safe_typing.allow_delayed_constants := true; (* Needed to delegate or skip proofs *)
  Flags.load_vos_libraries := true;
  Sys.(set_signal sigint Signal_ignore);
  loop ()
[%%endif] *)
