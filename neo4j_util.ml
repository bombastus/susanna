open Batteries
open Ocurl_util
open Yojson

(* Module signature to hold the basic server information. *)
module type Neo4jServerInfo = sig
  val service_root_path : string
  val login_creds       : string
end

module DefaultNeo4jServerInfo : Neo4jServerInfo = struct
  let service_root_path = "http://localhost:7474/db/data"
  let login_creds = 
    File.with_file_in "/home/peter/projects/susanna/auth.txt" IO.read_line |>
    Base64.str_encode
end

(* Main interface to the server *)
module Neo4j (I : Neo4jServerInfo) = struct
  exception Neo4jException of string

  type server_paths = {
    extensions : (string * string) list;
    extensions_info : string;

    node : string;
    node_index : string;

    relationship_index : string;
    relationship_types : string;

    batch : string; 
    cypher : string;
    indexes : string;
    constraints : string;
    node_labels : string;
    neo4j_version : string;
    transaction : string;
    transaction_commit : string;
  }

  type transaction_path = OPEN of string | CLOSED

  include I

  let headers =
    ["Accept", "application/json; charset=UTF-8";
     "Authorization", "basic " ^ login_creds;
     "Content-Type", "application/json";
    ] 

  let get_db_info () =
    get ~headers service_root_path

  let server_up () =
    (* should check what is being sent back.. 
       this is really just checking that a server responded at all. *)
    try 
      let rc,_ = get ~headers service_root_path in
      if rc >= 300 || rc < 200
      then failwith @@ "Server returned http status code of " ^ string_of_int rc
      else true
    with _ -> false

  let get_server_paths () =
    try
      let json_info = get_db_info () |> (fun (_,s) -> Basic.from_string s) in
      let member = Yojson.Basic.Util.member in
      let to_string = Yojson.Basic.Util.to_string in
      let to_assoc j = Yojson.Basic.Util.to_assoc j |> 
                       List.map (fun (s,t) -> s,to_string t) 
      in
      { node               = member "node"               json_info |> to_string;
        node_index         = member "node_index"         json_info |> to_string;
        node_labels        = member "node_labels"        json_info |> to_string;
        relationship_index = member "relationship_index" json_info |> to_string;
        relationship_types = member "relationship_types" json_info |> to_string;
        indexes            = member "indexes"            json_info |> to_string;
        batch              = member "batch"              json_info |> to_string;
        cypher             = member "cypher"             json_info |> to_string;
        constraints        = member "constraints"        json_info |> to_string;
        neo4j_version      = member "neo4j_version"      json_info |> to_string;
        extensions         = member "extensions"         json_info |> to_assoc;
        extensions_info    = member "extensions_info"    json_info |> to_string;
        transaction        = member "transaction"        json_info |> to_string;
        transaction_commit = member "transaction"        json_info |>
                             (fun j -> (to_string j) ^ "/commit"); }
    with e -> 
      raise e
      (* failwith "The server is either not up or it is not providing the expected paths" *)


  let get_errors json =
    let open Yojson.Basic in
    Util.member "errors" json

  let parse_response r = ()

  (* *** *)
  (* Internal data structure that provides the different necessary urls 
     for the server. *)
  let path = get_server_paths ()
  (* try get_server_paths () *)
  (* with _ -> { *)
  (*     extensions = []; *)
  (*     node = ""; *)
  (*     node_index = ""; *)
  (*     relationship_index = ""; *)
  (*     extensions_info = ""; *)
  (*     relationship_types = ""; *)
  (*     batch = "";  *)
  (*     cypher = ""; *)
  (*     indexes = ""; *)
  (*     constraints = ""; *)
  (*     node_labels = ""; *)
  (*     neo4j_version = ""; *)
  (*     transaction = ""; *)
  (*     transaction_commit = ""; *)
  (*   } *)


  let query q =
    let json_str = "{  \"statements\" : [ {\"statement\" : \"" ^ q ^ "\"  } ]" in
    let _,res = post ~headers ~url:path.transaction_commit json_str in
    Yojson.Basic.from_string res


  (* module Transaction = struct *)
  (*   let empty_transaction = "{ \"statements\" : []}" *)

  (*   (\* let make_json_statement st = "{ \"statement\" : " ^ st ^ " }" *\) *)
  (*   (\* let make_json_statements sts =  *\) *)
  (*   (\*   "{ \"statements\" : ["  *\) *)
  (*   (\* ^ "]"  *\) *)

  (*   let neo4j_commit_new_transaction data =  *)
  (*     post ~headers path.transaction_commit data *)

  (*   (\* let neo4j_rollback_open_trans trans = *\) *)
  (*   (\*   delete ~headers path.transaction_commit *\) *)


  (* end *)

  module Label = struct
  end



  module Node = struct
    (* type t = { *)
    (*   id : int; *)
    (*   name : string; *)
    (*   labels : *)
    (*   props : (string * string) list *)
    (* } *)

    let get_by_id id =
      get ~headers (path.node ^ "/" ^ (string_of_int id))

    let delete_by_id id =
      delete ~headers (path.node ^ "/" ^ (string_of_int id))

    let create () =
      post ~headers ~url:path.node ""

    let get_all () =
      query 
      
  end

  module Relationship = struct
  end
end
