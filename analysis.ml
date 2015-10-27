open Batteries

(* Analysis of text! *)


let count_elements ?(tbl_size=256) e = 
  (* given an enumeration of elements construct a hash table where the keys 
     are unique elements and the values are the count of that elemnt in the 
     enumeration.
  *)
  let h = Hashtbl.create tbl_size in
  let handler c = 
    match Hashtbl.find_option h c with
    | None -> Hashtbl.add h c 1
    | Some count -> Hashtbl.modify c (fun i -> i+1) h
  in
  Enum.iter handler e;
  h


let print_hashtbl ~print_key ?(print_val=print_int) h =
  Hashtbl.iter (fun c i -> print_key c; print_string ": "; print_val i; print_endline "") h

let test_file = "/home/peter/Documents/literature/aesops_fables.txt"


(* let () =  *)
(*   let test_str = File.open_in test_file |> BatInnerIO.read_all in *)
(*   let char_count = count_elements (String.enum test_str) in *)
(*   let word_count = count_elements (String.nsplit ~by:" " test_str |> List.enum) in *)
(*   print_endline "--- chars ---"; *)
(*   print_hashtbl print_char char_count; *)
(*   print_endline "--- words ---"; *)
(*   print_hashtbl print_string word_count *)


