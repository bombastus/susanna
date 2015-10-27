open Lwt

let rec print_doc docs =
  match docs with
  | [] -> print_endline "---"
  | Nethtml.Data s :: rest -> 
    print_endline ("Data: " ^ s);
    print_doc rest
  | Nethtml.Element (s, ts, rs) :: rest ->
    print_endline ("Element: " ^ s);
    List.iter (fun (t1,t2) ->
        print_endline ("  " ^ t1 ^ ";;;" ^ t2)
      ) ts;
    print_doc rs;
    print_doc rest


(* Note: flattens the doc by ignoring parent/child relationships *)
let rec output_doc ?(sep="\n") ochan docs =
  match docs with
  | [] -> Lwt_io.write ochan sep
  | (Nethtml.Data s)::rest -> 
    ignore @@ Lwt_io.write ochan (s ^ sep);
    output_doc ~sep ochan rest
  | (Nethtml.Element (name,attrs,subnodes))::rest ->
    let outstr = ref ("<"^name) in 
    List.iter (fun (a,c) -> outstr := !outstr^" "^a^"="^c) attrs;
    outstr := !outstr ^ "><\\"^name^">";
    ignore @@ Lwt_io.write ochan (!outstr ^ sep);
    output_doc ~sep ochan (subnodes@rest)


let html_extract 
    ?(attr_filter=Str.regexp ".*") ?(attr_value_filter=Str.regexp ".*") 
    ?(include_content=false) ?(content_filter=Str.regexp ".*") element_filter  doc =
  let rec aux d  result =
    match d with
    | [] -> result
    | Nethtml.Data s::rest -> 
      if include_content && String.length s > 0 && Str.string_match content_filter s 0 
      then aux rest (Nethtml.Data s :: result) 
      else aux rest result      (* ignore *)
    | Nethtml.Element (name,args,subnodes)::rest ->
      if Str.string_match element_filter name 0 then
        let args' =
          List.filter (fun (a,ac) -> 
              Str.string_match attr_filter a 0 &&
              Str.string_match attr_value_filter ac 0
            ) args in
        if List.length args' > 0 then
          aux (subnodes @ rest) (Nethtml.Element (name,args',[])::result)
        else     
          aux (subnodes @ rest) result
      else
        aux (subnodes @ rest) result
  in
  List.rev (aux doc [])

let rec extract_content doc =
  match doc with
  | [] -> []
  | Nethtml.Data s::rest ->
    s::(extract_content rest)
  | Nethtml.Element (name,args,subnodes)::rest ->
    extract_content rest

type xpath = Arg of string * (string list)
           | Content of string * (string list)


(* 
   A simplified version of xpath for easy html extraction.
   Syntax of this implementation of xpath:
   --
   1. /path/to/node
   returns the content of 'node'
   2. /path/to//nodes
   returns the content of all the 'nodes' at the level of '/path/to'
   3. /path/to/node/@attr
   return all the values associated with 'attr'
*)
(* let xpath xpath_str doc = *)
(*   let path_list = Str.split (Str.regexp "/") xpath_str in *)

(*   let rec traverse doc path_list result = *)
(*     match path_list with *)
(*     | [] -> extract_content doc *)
(*     | node :: [] ->  *)
(*       (\* we reached the desired path. collect content of nodes *\) *)
(*       begin *)
(*       match doc with *)
(*       | Nethtml.Data s :: rest -> *)
(*       | Nethtml.Element (name,args,subnodes) :: rest -> *)
        
(*       end *)
(*     | node :: rest_path -> *)
(*       (\* continue traversing the path.  *\) *)
(*       begin *)
(*         match doc with *)
(*         | [] -> result *)
(*         | Nethtml.Data s :: rest -> *)
(*         | Nethtml.Element (name,args,subnodes) :: rest -> *)

(*       end *)


(*   in *)
(*   traverse doc path_list [] *)


(* get all of the "a" elements with a href attribute and an optional url pattern *)
let filter_links ?(attr_value_filter=Str.regexp ".*") = 
  html_extract 
    ~attr_filter:(Str.regexp "href") ~attr_value_filter (Str.regexp "a")


(* Look at the href links in the given doc and retrieve them. 
 * Return a list of hrefs as strings.
 * *)
let retrieve_href_links docs =
  let rec aux docs result =
    match docs with
    | [] -> result
    | Nethtml.Element ("a",attrs,subnodes) :: rest ->
      if List.mem_assoc "href" attrs then
        let href = List.assoc "href" attrs in
        aux rest (href::result)
      else 
        aux rest result
    | Nethtml.Element _ :: rest -> aux rest result
    | Nethtml.Data _    :: rest -> aux rest result
  in
  List.rev (aux docs [])


let get_urls urls =
  let requests = Lwt_list.map_p (fun x -> Ocurl_util.get x |> Lwt.return) urls in
  let results = Lwt_main.run requests in
  results

exception Not_found_exception

let () = 
  let open Lwt_io in
  let usage_msg = 
    "Oget: \n" ^
    "Simple webpage fetching and scraping. Use -ft, -fa, -fv, and fc to " ^
    "respectively filter by tagname, attribute, attribute value, and content." ^
    "\nEx: <tagname attr=attr_value ...> content <\\tagname>\n ---"
  in

  let output = ref Lwt_io.stdout in
  (* let input_file = ref "" in *)

  (* follow all of the links in the given pages *)
  (* let follow_links        = ref false in (\* TODO ~~ not yet implemented *\) *)
  (* let follow_depth        = ref 10 in (\* how many levels deep to follow links *\) *)

  (* If a url matches one of these patterns then it should be viewed as an 
   * html document otherwise,
   * *)
  (* let html_doc_filter = ref (List.map Str.regexp [".*/"; ".*\\.htm\\(l\\)?"]) in *)
  (* let is_html =  *)

  let perform_filtering   = ref true in

  let element_regstr      = ref ".*" in
  let attr_regstr         = ref ".*" in
  let attr_value_regstr = ref ".*" in

  let include_content        = ref false in
  let content_regstr         = ref ".*" in

  let url_strings = ref [] in
  let add_url_string s = url_strings := s::!url_strings in

  let speclist = [
    ("-o", Arg.String (fun filename -> 
         try
           let fd = Unix.openfile filename [Unix.O_WRONLY;Unix.O_CREAT] 0o640 in
           Unix.ftruncate fd 0;
           output := Lwt_io.of_unix_fd ~mode:Lwt_io.output fd;
         with _ -> failwith "Bad output file"),
     "Set the output file. By default output to stdout");

    ("-nf", Arg.Clear perform_filtering, 
     "do not perform filtering and instead returnt the whole " ^
     "content of the request");

    ("-fe", Arg.String (fun r -> element_regstr := r),
     "Regexp to filter elements.");

    ("-fa", Arg.String (fun r -> attr_regstr := r),
     "Regexp to filter elements by attribute");

    ("-fc", Arg.String (fun r -> attr_value_regstr := r),
     "Regexp to filter elements by attribute value.");

    ("-fd", Arg.String (fun r -> content_regstr := r),
     "Regexp to filter elements by content");

    ("-id", Arg.Set include_content, 
     "If -id is used, include the content filter. Otherwise just ignore it and " ^
     "only look at elements");
    (* ("-d", Arg.) *)
  ] in

  Arg.parse speclist add_url_string usage_msg;
  List.iter print_endline ["---";  
                           "element filter:" ^ !element_regstr;
                           "attribute filter:" ^ !attr_regstr;
                           "attr value filter" ^ !attr_value_regstr; 
                           if !include_content 
                           then "content filter: " ^ !content_regstr ^ "\n---"
                           else "---";
                          ];

  try
    let content = List.combine !url_strings (get_urls !url_strings) in
    let _,html =
      (* List.filter (fun (url,_) ->  *)
      (*   List.fold_left (fun b r -> b && Str.string_match r url 0) true !html_doc_filter  *)
      (* ) content |> *) List.split content
    in
    (* ignore the response code *)
    let _,html  = List.split html in
    (* let () = List.iter print_endline !url_strings in *)
    (* let () = List.iter print_endline html in *)
    if !perform_filtering
    then
      let docs =
        List.map (fun s -> 
            let ich = new Netchannels.input_string s in
            Nethtml.parse ich
          ) html
      in 
      let results = 

        let element_filter      = Str.regexp !element_regstr in
        let attr_filter         = Str.regexp !attr_regstr in
        let attr_value_filter = Str.regexp !attr_value_regstr in
        let content_filter         = Str.regexp !content_regstr in
        let include_content        = !include_content in
        List.map (html_extract 
                    ~attr_filter ~attr_value_filter
                    ~include_content ~content_filter element_filter) docs
      in
      ignore (Lwt_list.iter_s (output_doc !output) results)

    else
      List.iter (fun h -> print_string h; print_endline "\n") html

  with 
  | Invalid_argument s -> print_endline ("Make sure the provided urls are valid. " ^ s)
  | Unix.Unix_error _ -> print_endline "Could not connect to site."


