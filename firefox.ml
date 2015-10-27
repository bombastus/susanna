open Batteries

(* Get information from firefox and its local databases *)
let home_dir = ref (try BatSys.getenv "HOME" with _ -> "")
let profile_dir = ref "/.mozilla/firefox/gqz3fxx2.default"
let places_filename = ref (!home_dir ^ !profile_dir ^ "/places.sqlite")
(* maybe store this somewhere for each startup? *)
let last_check = ref 0.0

(* sqlite *)
let get_db () = Sqlite3.db_open !places_filename

let get_data_from_table db table_name =
  let data = ref [] in
  let accum_data row headers = data := (row,headers)::!data in
  let stmt = Printf.sprintf "SELECT * FROM %s" table_name in
  let code = Sqlite3.exec db ~cb:accum_data stmt in
  match code with
  |    Sqlite3.Rc.OK -> !data
  |    x -> raise (Failure (Sqlite3.Rc.to_string x))


let list_data_from_table db table_name =
  let row_lister row headers =
    if Array.length row = Array.length headers 
    then
      print_string "   ";
    Array.iteri (fun i ro -> 
        match ro with
        | None -> ()
        | Some r ->
          let h = headers.(i) in
          print_string ( r ^ " : " ^ h ^ ", ")
      ) row; 
    print_endline ""
  in
  let stmt = Printf.sprintf "SELECT * FROM %s" table_name in
  let code = Sqlite3.exec db ~cb:row_lister stmt in
  match code with
  |    Sqlite3.Rc.OK -> ()
  |    x -> (* raise (Failure ("HERE: " ^ (Sqlite3.Rc.to_string x)) *)print_endline (Sqlite3.Rc.to_string x); ()

let list_tables db =
  (* List the table names of the given database. *)
  let row_lister row headers =
    let table_name = row.(0) in
    Printf.printf "  %s:\n" table_name;
    list_data_from_table db table_name
  in 
  print_endline "Tables :" ;
  let code = Sqlite3.exec_not_null db ~cb:row_lister
      "SELECT name FROM sqlite_master;" 
  in
  begin
    match code with
    | Sqlite3.Rc.OK -> ()
    |  x -> raise (Failure (Sqlite3.Rc.to_string x))
  end;
  print_endline "------------------------------------------------"


(* firefox *)
let get_places_filename () =
  let places_filename = 
    (!home_dir ^ !profile_dir ^ "/places.sqlite") 
  in
  if Sys.file_exists places_filename
  then places_filename
  else raise 
      (Failure "get_places_filename: places.sqlite does not exist")
  
let check_places () =
  let stats = Unix.stat !places_filename in
  if !last_check < stats.Unix.st_mtime
  then 
    begin
    print_endline "Checking!";
    last_check := stats.Unix.st_mtime
    end
  else print_endline "No need to check."
