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


type event =
  | Receive of Jsonrpc.Packet.t option
  | Send of Jsonrpc.Packet.t

module type EventCaster = sig
  type event

  val cast_event : Jsonrpc.Packet.t -> event list
end


let server_info = Lsp.Types.InitializeResult.create_serverInfo
  ~name:"socca-lsp"
  ~version:"0.0.1"
  ()

let conf_request_id = max_int


let send_configuration_request () =
  let id = `Int conf_request_id in
  let mk_configuration_item section =
    Lsp.Types.ConfigurationItem.({ scopeUri = None; section = Some section })
  in
  let items = List.map mk_configuration_item ["vsrocq"] in
  let req = Lsp.Server_request.(to_jsonrpc_request (WorkspaceConfiguration { items }) ~id) in
  Send (Request req)

type error = {
  code : Jsonrpc.Response.Error.Code.t option;
  message : string;
}



module LspManager = struct


  let initialize id params =
    log_file "We have done something" "/home/bourbeillon/test.log";
    let Lsp.Types.InitializeParams.{ initializationOptions } = params in
    begin match initializationOptions with
    | None -> log_file "Warning : initialize request empty" "/home/bourbeillon/test.log"
    | Some initializationOptions -> ()
    end;
    let textDocumentSync = `TextDocumentSyncKind Lsp.Types.TextDocumentSyncKind.Incremental in
    let capabilities = Lsp.Types.ServerCapabilities.create ~textDocumentSync ()
    in
    let initialize_result = Lsp.Types.InitializeResult.{
      capabilities = capabilities;
      serverInfo = Some server_info;
    } in
    (* let debug_events = Common.Log.lsp_initialization_done () |> inject_debug_events in *)
    Ok initialize_result, [Sel.now @@ (send_configuration_request ())]
    (* debug_events@[Sel.now @@ LspManagerEvent (send_configuration_request ())] *)

  let shutdown id =
    Ok(()), []

  let handle_lsp_request : type a. Jsonrpc.Id.t -> a Lsp.Client_request.t -> (a, error) result * event Sel.Event.t list = fun
  id req ->
  match req with
    | Initialize params ->
      initialize id params
    | Shutdown ->
      shutdown id
    | TextDocumentDefinition _ ->
      Ok None, []
    | TextDocumentHover _ ->
      Ok None, []
    | DocumentSymbol _ ->
      Ok None, []
    | UnknownRequest _ ->
       Error({message = "Unknown request"; code=None}), []
    | _ ->
      Error({message = "Not handled request"; code=None}), []

  let unpack_rpc_request (req: Jsonrpc.Request.t) : event Sel.Event.t list =
    let id = req.id in
    let req = Lsp.Client_request.of_jsonrpc (req) in
    match req with
    | Error e -> log ("Failed to handle request: " ^ e); []
    | Ok (Lsp.Client_request.E req) ->
        let response,events = handle_lsp_request id req in
        begin
        match response with
        | Error {code; message} -> ()
        | Ok resp ->
          let response = Lsp.Client_request.yojson_of_result req resp in
          ignore (Channel.send_rpc_request Channel.std_channel response)
        end;
        events
end


module BaseEventCaster = struct

  type event =
  | LspEvent of Lsp.Client_request.packed

  let cast_request (req:Jsonrpc.Request.t) = LspManager.unpack_rpc_request req

  let cast_notification (notif:Jsonrpc.Notification.t) = []

  let cast_response (resp:Jsonrpc.Response.t) = []

  let cast_batch_response (batch_resp:Jsonrpc.Response.t list) =
    List.concat (List.map cast_response batch_resp)

  let cast_batch_call (batch_call:[ `Request of Jsonrpc.Request.t | `Notification of Jsonrpc.Notification.t ] list) = []

  let cast_event (pkt:Jsonrpc.Packet.t) =
    match pkt with
    | Request req -> cast_request req
    | Notification notif -> cast_notification notif
    | Response resp -> cast_response resp
    | Batch_response batch_resp -> cast_batch_response batch_resp
    | Batch_call batch_call -> cast_batch_call batch_call

end

module ProtocolManager = struct

  let handle_raw_receive_request = function
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

  let await_events () =
    Logger.log_file "TEST\n" "/home/bourbeillon/test.log";
    let events = Channel.receive_raw_request Channel.std_channel handle_raw_receive_request in
    [events]

  let print_event _fmt = function
    | Receive _ -> log "Receive event"
    | Send _ -> log "Send event"

  let handle_event e =
    match e with
    | Receive None -> await_events ()
    | Receive (Some pkt) -> await_events() @ BaseEventCaster.cast_event pkt (* TODO : handle request, transform this module to a functor*)
    | Send pkt ->
      let _ = Channel.send_rpc_request Channel.std_channel (Jsonrpc.Packet.yojson_of_t pkt) in (*We could use ignore, this function return int because of exit code*)
      await_events ()
end

let loop () =
  let events = ProtocolManager.await_events () in
  let rec loop (todo : event Sel.Todo.t) =
    (*log fun () -> "looking for next step";*)
    flush_all ();
    let ready, todo = Sel.pop todo in
    log_file "Oui" "/home/bourbeillon/test.log";
    let new_events = ProtocolManager.handle_event ready in
    let todo = Sel.Todo.add todo new_events in
    log_file "Oui" "/home/bourbeillon/test.log";
    loop todo
  in
  let todo = Sel.Todo.add Sel.Todo.empty events in
  try loop todo
  with exn ->
    log_file "Exception raised." "/home/bourbeillon/test.log"


let () =
  log "Starting the main loop.";
  loop()
