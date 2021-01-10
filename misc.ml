open Base;;
open Base.Poly;;
open Printf;;

module Bytes_stdlib = Stdlib.Bytes
module Option_stdlib = Stdlib.Option

module Ht = Stdlib.Hashtbl

let add x y = x + y

let sub x y = x - y

type db_data = { created : float; key_size: int; key  : string; value_size : int; value: string }
type bytes_data = { bytes_size : int; bytes_data  : bytes }

                 
type file_data  = { file_name: string ; 
                      fd_file: Unix.file_descr;
                      file_name_index: string ;
                      fd_file_index: Unix.file_descr;                
                      index_map: (string, int) Ht.t }

type hash_data = {hash_map: (string, int) Ht.t}


type db = { data_fn: string;
           index_fn: string;
           data: Unix.file_descr;
           index: (string, int ) Ht.t }
          



  (*
   *  In utop to load a module :
   #use "./misc.ml";;

   * To use a function in the module:

     utop # Misc.trim " sd fsd ";;
- : string = "sd fsd"

   *)

module Dbmod = struct
  (* type db_data = { created : float; key_size: int; key  : string; value_size : int; value: string }  *)

  let create_bytes_data value =
  {
     bytes_size = Bytes.length value;
     bytes_data = value;
  }

  let get_bytes_data a_record =
     match a_record with
     | { bytes_size ; bytes_data  } -> bytes_size, bytes_data;;

  let create_db_data key value =
  {
     created  = Unix.gettimeofday();
     key_size = String.length key;
     key = key;
     value_size = String.length value;
     value = value;
  }

  let print_created a_record =
     let tm = Unix.localtime ( a_record.created ) in
     Printf.sprintf "%.4d/%.2d/%.2d %.2d:%.2d:%.2d"
     (1900+tm.Unix.tm_year) (tm.Unix.tm_mon + 1)
     tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec

  let get_key a_record =
     match a_record with
     | { created ; key_size; key; value_size ; value } -> key, key_size;;


  let get_value a_record =
     match a_record with
     | { created ; key_size; key; value_size ; value } -> value, value_size;;

  let get_key_value a_record =
     match a_record with
     | { created ; key_size; key; value_size ; value } -> key, value;;

  let marshal_to_bytes var1 =
         Marshal.to_bytes var1 [Marshal.No_sharing];;

  let marshal_from_bytes var_as_bytes =
     let var2 : db_data  = Marshal.from_bytes var_as_bytes  0 in
     var2;;

  let marshal_from_bytes_to_hash var_as_bytes =

     let var  = Marshal.from_bytes var_as_bytes  0 in  
     (* TODO !! *)
     (* let var : hash_data = Marshal.from_bytes var_as_bytes  0 in  *)
     var;;

  let create fn =
    let data_fn = fn in
    let index_fn = fn ^ ".idx" in
    let data =
      Unix.(openfile data_fn [O_RDWR; O_CREAT; O_EXCL] 0o600) in
    (* we just check there is not already an index file *)
    let index_file =
      Unix.(openfile index_fn [O_RDWR; O_CREAT; O_EXCL] 0o600) in
    Unix.close index_file;
    let index = Ht.create 11 in
    { data_fn; index_fn; data; index }

  let create_file file_name  =
     (* Creata a file if it doesn't exist *)
     let fd_file = Unix.openfile file_name [O_RDWR; O_CREAT] 0o600 in  

     (* Unix.close(fd_file) in   *)

     let file_name_index = file_name ^ ".idx" in
     let fd_file_index = Unix.openfile file_name_index [O_RDWR; O_CREAT] 0o600 in  
     (* Unix.close(fd_file) ;  *)
     Unix.close(fd_file_index) ;
    
     let index_map : (string, int) Ht.t = Ht.create 1024 in 
     

     { file_name; fd_file; file_name_index; fd_file_index; index_map };;  


  let close_simple fd_file =
    Unix.close fd_file;;

  let open_existing_file file_name = 
     let fd_file = Unix.(openfile file_name  [O_RDWR] 0o600) in
     let file_name_index = file_name ^ ".idx" in
     let fd_file_index = Unix.(openfile file_name_index  [O_RDWR] 0o600) in
     let index_map : (string, int) Ht.t = Ht.create 1024 in 
     { file_name; fd_file; file_name_index; fd_file_index; index_map };;  

     
  let open_simple_file file_name = 
      let fd_file = Unix.(openfile file_name  [O_RDWR] 0o600) in
      fd_file;;


  let get_current_pos file_data = 
      let result = Unix.(lseek file_data.fd_file 0 SEEK_CUR) in
      result
  ;;

  let go_to_pos file_data lseek_offset =
    let offset_new = Unix.(lseek file_data.fd_file lseek_offset SEEK_SET) in
    begin
      if offset_new <> lseek_offset then
        let err_msg =
          sprintf "Dbmod.read_string: db: %s off: %d  off': %d"
            file_data.file_name lseek_offset offset_new in
        failwith err_msg
    end;
    ();;

  let write_string file_data my_str =
     (* go to end of data file *)
     let off = Unix.(lseek file_data.fd_file 0 SEEK_END) in
     let len = String.length my_str in
     let written = Unix.write_substring file_data.fd_file my_str 0 len in
     begin
       if written <> len then
         let err_msg =
           sprintf
             "Dbmod.write_string: file_data: %s my_str: %s written: %d len: %d"
             file_data.file_name my_str written len in
         failwith err_msg
     end;
     written;;
   
  let read_int file_data lseek_offset =
      let int_len = 8 in   (*  (* TODO hardcoded int size = 8 !! *) *)
      let my_buffer = Bytes.create int_len  in
      (* go to offset position in the file*)
      let offset_new = Unix.(lseek file_data.fd_file lseek_offset SEEK_SET) in
      begin
        if offset_new <> lseek_offset then
          let err_msg =
            sprintf "Dbmod.read_string: db: %s off: %d  off': %d"
              file_data.file_name lseek_offset offset_new in
          failwith err_msg
      end;
       
      let bytes_read = (Unix.read file_data.fd_file my_buffer 0 int_len  ) in
      begin
         if bytes_read <> int_len then
           let err_msg =
             sprintf "Db.Internal.raw_read: db: %s off: %d len: %d read: %d"
             file_data.file_name lseek_offset int_len bytes_read in
           failwith err_msg
      end;
      
      let result = Bytes_stdlib.get_int64_ne my_buffer 0 in
      (* let result : int = Marshal.from_bytes  my_buffer 0  in *)
      (* Int64.to_int result *)
      result
   
   ;;

   
   let read_int_current_pos file_data   =
      let int_len = 8 in   (*  (* TODO hardcoded int size = 8 !! *) *)
      let my_buffer = Bytes.create int_len  in

      let bytes_read = (Unix.read file_data.fd_file my_buffer 0 int_len  ) in
      begin
         if bytes_read <> int_len then
           let err_msg =
             sprintf "Db.Internal.raw_read: db: %s off: %d len: %d read: %d"
             file_data.file_name (get_current_pos file_data) int_len bytes_read in
           failwith err_msg
      end;
      
      let result = Bytes_stdlib.get_int64_ne my_buffer 0 in
      (* let result : int = Marshal.from_bytes  my_buffer 0  in *)
      (* Int64.to_int result *)
      result
   
   ;;
   
   
  let write_bytes file_data my_buffer =
     (* go to end of data file *)
     let off = Unix.(lseek file_data.fd_file 0 SEEK_END) in
     let len = Bytes.length my_buffer in
  
(*     Unix.single_write fd  ~buf: my_buf  ~pos: 0  ~len:  7  ;; *)

     (* Write  *)
     let written_bytes = Unix.write file_data.fd_file my_buffer 0 len in 
     begin
       if written_bytes <> len then
         let err_msg =
           sprintf 
             "Dbmod..write_bytes:" in
             (*
             "Dbmod..write_bytes: file_data: %s my_str: %s written_bytes: %d len: %d"
             file_data.file_name Bytes.to_string(my_buffer) written_bytes len in
                *)
         failwith err_msg
     end;
     written_bytes;;

   let write_int file_data my_int =
      let int_len = 8 in   (*  TODO hardcoded int size = 8 !! *) 
      (* let my_buffer =  marshal_to_bytes  my_int in   *)

      let my_buffer = Bytes.create int_len  in
      Bytes_stdlib.set_int64_ne  my_buffer 0  (Int64.of_int my_int);

      let wriiten_bytes = write_bytes file_data my_buffer in

      wriiten_bytes;
   ;;   

   
   let write_full_record file_data my_buffer key =
      let curr_pos = get_current_pos file_data in 
      let res5 = write_int file_data (Bytes.length my_buffer) in
      begin 
        print_endline ("Result of write_int   = " ^ string_of_int(res5)); 
      end;
      let res3 = write_bytes file_data my_buffer in
      begin 
         print_endline ("Result of write_bytes   = " ^ string_of_int(res3)); 
       end;
       (* Ht.add db.index k { off; len }   *)
       Ht.add file_data.index_map key curr_pos;
      (res3 + res5, curr_pos);  
   ;;



   let get_exn = function
  | Some x -> x
  | None   -> raise (Invalid_argument "Option.get");;

   let read_bytes file_data lseek_offset bytes_len64 =

      let bytes_len = get_exn(Int64.to_int bytes_len64) in
      let my_buffer = Bytes.create bytes_len  in
      (* go to offset position in the file*)
      let offset_new = Unix.(lseek file_data.fd_file lseek_offset SEEK_SET) in
      begin
        if offset_new <> lseek_offset then
          let err_msg =
            sprintf "Dbmod.read_bytes: db: %s off: %d  off': %d"
              file_data.file_name lseek_offset offset_new in
          failwith err_msg
      end;

      print_endline ("lseek_offset    = " ^ string_of_int(lseek_offset));
      print_endline ("bytes_len    = " ^ string_of_int(bytes_len));
      print_endline ("offset_new    = " ^ string_of_int(offset_new));
       
      (* *)
      let bytes_read = Unix.(read file_data.fd_file my_buffer 0 bytes_len  ) in
 
      begin
         if bytes_read <> bytes_len then
           let err_msg =
             sprintf "Dbmod.read_bytes: db: %s off: %d len: %d read: %d"
             file_data.file_name lseek_offset bytes_len bytes_read in
           failwith err_msg
      end;
      (* *) 

      my_buffer
 
   ;;

   let read_bytes_current_pos file_data bytes_len64 =

      let bytes_len = get_exn(Int64.to_int bytes_len64) in
      let my_buffer = Bytes.create bytes_len  in

      let bytes_read = Unix.(read file_data.fd_file my_buffer 0 bytes_len  ) in
 
      begin
         if bytes_read <> bytes_len then
           let err_msg =
             sprintf "Dbmod.read_bytes: db: %s off: %d len: %d read: %d"
             file_data.file_name (get_current_pos file_data) bytes_len bytes_read in
           failwith err_msg
      end;
      (* *) 

      my_buffer
 
   ;;



   let read_full_record_current_pos file_data  =
     let curr_pos = get_current_pos file_data in 
     let result_int = read_int_current_pos file_data in
     let result_binary = read_bytes_current_pos  file_data result_int in
     (result_binary, curr_pos, result_int)
     ;;

   let print_hash_map file_data = 
     print_endline "Starting print_hash_map() >>==================" ;
     Ht.iter (fun key value -> Stdlib.Printf.printf "%s -> %d\n" key value) file_data.index_map;
     print_endline "<<==================== Completed print_hash_map()" ;
   ;;



   let find_full_record file_data lseek_offset =
      go_to_pos file_data lseek_offset;
      let result_int = read_int_current_pos file_data in
      let result_binary = read_bytes_current_pos  file_data result_int in
      (result_binary, lseek_offset, result_int)
     ;;

   let get_full_record_and_print file_data start_pos =
    print_endline ("get_full_record_and_print() ===== pos = " ^ string_of_int(start_pos) );
    let var1_tuple = find_full_record file_data start_pos in 
    let (var1_data, start_pos1, bytes_size1 ) = var1_tuple in
    let var1_1 = marshal_from_bytes var1_data in 
    let key1, val1 = get_key_value var1_1 in 
    begin
      print_endline ("key = " ^ key1  ^ " value = " ^ val1 );
      print_endline("Created var1 = " ^  print_created var1_1);
      print_endline("Start pos = " ^  string_of_int(start_pos1) ^ " byte_size = " ^ Int64.to_string(bytes_size1) );
    end;
  ;;


  let print_hash_map_full_record file_data = 
    print_endline "Starting print_hash_map_full_record()  >>==================" ;
    Ht.iter (fun key value -> (Stdlib.Printf.printf "%s -> %d\n" key value);  get_full_record_and_print file_data value  ) file_data.index_map;
    print_endline "<<==================== Completed  print_hash_map_full_record() " ;
  ;;
  
  (*  We need to combine with the write_bytes() *)
  let write_bytes_simple fd_file my_buffer =
    (* go to end of data file *)
    let off = Unix.(lseek fd_file 0 SEEK_END) in
    let len = Bytes.length my_buffer in
 
(*     Unix.single_write fd  ~buf: my_buf  ~pos: 0  ~len:  7  ;; *)

    (* Write  *)
    let written_bytes = Unix.write fd_file my_buffer 0 len in 
    begin
      if written_bytes <> len then
        let err_msg =
          sprintf 
            "Dbmod..write_bytes:" in
            (*
            "Dbmod..write_bytes: file_data: %s my_str: %s written_bytes: %d len: %d"
            file_data.file_name Bytes.to_string(my_buffer) written_bytes len in
               *)
        failwith err_msg
    end;
    written_bytes;;



let file_size file_name = 
  let my_stats = Unix.stat file_name in
  my_stats.st_size;;

(* TODO *)
let read_whole_file file_name = 
  let fd_file = open_simple_file file_name in
  let file_size = file_size file_name in
  
  let my_buffer = Bytes.create file_size  in

  let bytes_read = Unix.(read fd_file my_buffer 0 file_size  ) in

  begin
     if bytes_read <> file_size then
       let err_msg =
         sprintf "Dbmod.read_bytes: db: %s  len: %d read: %d"
         file_name file_size bytes_read in
       failwith err_msg
  end;
  (* *) 

  my_buffer

;;

let hash_from_file file_name =
  let my_buffer2 = read_whole_file file_name in
  let len = Bytes.length my_buffer2 in
  let my_hash =  marshal_from_bytes_to_hash my_buffer2 in
  begin
      print_endline ("file name "  ^ file_name);
      print_endline ("buffer len "  ^ string_of_int(len ));
      print_endline ("hash tbl len = " ^ string_of_int(Ht.length my_hash))
  end;
  my_hash;;  
  
  (*
let print_hash my_hash = 
  Hashtbl.iter (fun key value -> Printf.printf "%s -> %s\n" key value) my_hash in
  (* Ht.iter (fun key value -> Stdlib.Printf.printf "%s -> %d\n" key value) my_hash in  *)
  ();;  
*)


end;; 

module M = struct 
  let x = 42 
end;;

module Misc = struct

  (* size of int *)
  let rec size_of_int x =
    match (x lsr 1, x land 1) with
    | 0, n -> n
    | y, n -> n + size_of_int y  ;;

  (* To test it
    size_of_int (-1);;
  *)
end;;

(*
module Misc = struct

  let x = 43

  let rec trim s =
    let l = String.length s in 
    if l=0 then s
    else if s.[0]=' ' || s.[0]='\t' || s.[0]='\n' || s.[0]='\r' then
      trim (String.sub s 1 (l-1))
    else if s.[l-1]=' ' || s.[l-1]='\t' || s.[l-1]='\n' || s.[l-1]='\r' then
      trim (String.sub s 0 (l-1))
    else
      s;;

  (* sun of a container see 
    http://www.codecodex.com/wiki/Calculate_the_sum_over_a_container
    For example:
    # sum [1; 2; 4; 40];;
    - : int = 47

  let sum = List.fold_left ( + ) 0;;
   *)

  let rec sum = function
  | [] -> 0
  | h::t -> h + sum t ;;


  (*
   For example:
   # save "foo" "bar\n";;
   - : unit = ()
  *)

  let save file string =
     (* let channel = open_out file in *)
     let channel = Out_channel.create file in
     (* output_string channel string; *)
     Out_channel.output_string channel string;
     (* close_out channel;;  *)
     Out_channel.close channel;;

  (* See serializibg/deserializing // marshall in
   * http://www.codecodex.com/wiki/Serializing_an_object
   *
   *  Marshal.to_string [1;2;3;4] [];;
   
   (Marshal.from_string
     "\132\149\166\190\000\000\000\t\000\000\000\004\000\000\000\012\000\000\000\012\160A\160B\160C\160D@"
     0 : int list);;
- : int list = [1; 2; 3; 4]
   *
   *)

end;;

*)
