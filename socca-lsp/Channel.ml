open Logger

type t = {
  in_channel : Unix.file_descr;
  out_channel : Unix.file_descr;
}

let std_channel : t = {
  in_channel = Unix.stdin;
  out_channel = Unix.stdout;
}

let receive_raw_request t = log_file "receive_raw_request\n" log_filepath; Sel.On.httpcle ~priority:(-6) ~name:"lsp" t.in_channel

let send_raw_request t str =
  try
     Unix.write_substring t.out_channel str 0 (String.length str)
  with
  | Unix.Unix_error(Unix.EPIPE, _, _) ->
      exit 0
  | Unix.Unix_error(errno, _, _) ->
      exit 1
  | exn ->
      exit 2


let raw_to_rpc raw =
  log_file raw log_filepath;
  log_file "raw_to_rpc\n" log_filepath;
  try
    let json = Yojson.Safe.from_string raw in
    log_file "Jsonrpc packet received\n" log_filepath; Some (Jsonrpc.Packet.t_of_yojson json)
  with
  | Yojson.Json_error _ ->
     log_file "Json_error\n" log_filepath;
      None
  | exn ->
    log_file "Error decoding JSON\n" log_filepath;
      None

let send_rpc_request t json =
  let msg  = Yojson.Safe.pretty_to_string ~std:true json in
  let size = String.length msg in
  let s = Printf.sprintf "Content-Length: %d\r\n\r\n%s" size msg in
  send_raw_request t s