


let log str =
  Printf.printf "%s\n" str



let log_file str path =
  let oc = open_out_gen [Open_wronly; Open_creat; Open_text] 0o666 path in
  Printf.fprintf oc "%s\n" str;
  close_out oc