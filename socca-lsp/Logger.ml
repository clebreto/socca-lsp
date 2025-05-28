


let log str =
  Printf.printf "%s\n" str



let log_file str path =
  let oc = open_out path in
  Printf.fprintf oc "%s\n" str;
  close_out oc