(* ocamlfind ocamlopt -o exmpl -package curl -linkpkg exmpl.ml *)
open Printf

let _ = Curl.global_init Curl.CURLINIT_GLOBALALL

(*
 *************************************************************************
 ** Aux. functions
 *************************************************************************
 *)

let writer_callback a d =
  Buffer.add_string a d;
  String.length d

let init_conn url =
  let r = Buffer.create 16384
  and c = Curl.init () in
  Curl.set_timeout c 30;
  Curl.set_sslverifypeer c false;
  Curl.set_sslverifyhost c Curl.SSLVERIFYHOST_EXISTENCE;
  Curl.set_writefunction c (writer_callback r);
  Curl.set_tcpnodelay c true;
  Curl.set_verbose c false;
  Curl.set_post c false;
  Curl.set_url c url; r,c


(*
 *************************************************************************
 ** GET, POST, PUT, DELETE
 *************************************************************************
 *)


(* GET *)
let get ?(headers = ["Content-Type","text/html"]) url =
  let r,c = init_conn url in
  Curl.set_httpget c true;
  Curl.set_followlocation c true;
  Curl.set_httpheader c @@ List.map (fun (h,v) -> h ^ ": " ^ v ) headers;
  Curl.perform c;
  let rc = Curl.get_responsecode c in
  Curl.cleanup c;
  rc, (Buffer.contents r)


(* POST *)
let post ?(headers = ["Content-Type","text/html"]) ~url data =
  let r,c = init_conn url in
  Curl.set_post c true;
  Curl.set_httpheader c @@ List.map (fun (h,v) -> h ^ ": " ^ v ) headers;
  Curl.set_postfields c data;
  Curl.set_postfieldsize c (String.length data);
  Curl.perform c;
  let rc = Curl.get_responsecode c in
  Curl.cleanup c;
  rc, (Buffer.contents r)


(* PUT *)
let put ?(headers = ["Content-Type","text/html"]) ~url data =
  let pos = ref 0
  and len = String.length data in
  let rf cnt =
    let can_send = len - !pos in
    let to_send = if can_send > cnt then cnt else can_send in
    let r = String.sub data !pos to_send in
    pos := !pos + to_send; r
  and r,c = init_conn url in
  Curl.set_put c true;
  Curl.set_upload c true;
  Curl.set_readfunction c rf;
  Curl.set_httpheader c @@ List.map (fun (h,v) -> h ^ ": " ^ v ) headers;
  Curl.perform c;
  let rc = Curl.get_responsecode c in
  Curl.cleanup c;
  rc, (Buffer.contents r)


(* DELETE *)
let delete ?(headers = ["Content-Type","text/html"]) url =
  let r,c = init_conn url in
  Curl.set_customrequest c "DELETE";
  Curl.set_followlocation c false;
  Curl.set_httpheader c @@ List.map (fun (h,v) -> h ^ ": " ^ v ) headers;
  Curl.perform c;
  let rc = Curl.get_responsecode c in
  Curl.cleanup c;
  rc, (Buffer.contents r)

(*
 *************************************************************************
 ** Check
 *************************************************************************
 *)


(* let _ = *)
(*  let r,c = put "http://localhost/test" "test" in *)
(*  printf "%d -> %s\n" r c *)
