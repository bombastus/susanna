(* (\* daemonize a process *\) *)
(* let daemonize () = *)
(*   match Unix.fork () with *)
(*   | 0 -> *)
(*     if Unix.setsid () == -1 then *)
(*       failwith "Unix.setsid failed"; *)
(*     begin match Unix.fork () with *)
(*       | 0 -> *)
(*         let nullfd = Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0 in *)
(*         begin try *)
(*             Unix.close Unix.stdin; *)
(*             Unix.dup2 nullfd Unix.stdout; *)
(*             Unix.dup2 nullfd Unix.stderr; *)
(*           with exn -> Unix.close nullfd; raise exn *)
(*         end; *)
(*         Unix.close nullfd *)
(*       | _ -> exit 0 *)
(*     end *)
(*   | _ -> exit 0 *)
let i = ref 0

let rec main () =
  let fd = Unix.openfile "/home/peter/data/susanna/out.txt" [ Unix.O_CREAT; Unix.O_WRONLY; Unix.O_APPEND ] 0o640 in
  let time = Unix.time () in
  let s =(string_of_float time) ^ ": " ^ (string_of_int !i) ^ "\n" in
  let _ = Unix.write fd s 0 (Bytes.length s) in
  Unix.close fd;
  i := !i + 1;
  Unix.sleep 5;
    main ()

let _ =  
  print_endline "This is a test";
  match Unix.fork () with
  | 0 -> main ()          (* child *)
  | i -> exit 0           (* parent *)

