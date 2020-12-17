open Base;;
open Base.Poly;;
open Printf;;

module Bytes_stdlib = Stdlib.Bytes

let add x y = x + y

let sub x y = x - y

type db_data = { created : float; key_size: int; key  : string; value_size : int; value: string }
type bytes_data = { bytes_size : int; bytes_data  : bytes }
type file_data  = { file_name: string ; fd_file: Unix.file_descr }

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

  let create_file file_name  =
     (* Creata a file if it doesn't exist *)
     let fd_file = Unix.openfile file_name [O_RDWR; O_CREAT] 0o600 in  

     (* It doesn't work ??
     let fd_file = Unix.openfile  ~mode: [Unix.O_RDWR; Unix.O_CREAT]  ~perm: 0o644 file_name in 
        *)
     Unix.close(fd_file);;

  let close_simple fd_file =
    Unix.close fd_file;;

  let open_existing_file file_name = 
     let fd_file = Unix.(openfile file_name  [O_RDWR] 0o600) in
     { file_name; fd_file };;

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
   
     (*
   let raw_read db pos =
      let off = pos.off in
      let len = pos.len in
      let buff = Bytes.create len in
      let off' = Unix.(lseek db.data off SEEK_SET) in
      begin
        if off' <> off then
          let err_msg =
            sprintf "Db.Internal.raw_read: db: %s off: %d len: %d off': %d"
              db.data_fn off len off' in
          failwith err_msg
      end;
      let read = Unix.read db.data buff 0 len in
      begin
        if read <> len then
          let err_msg =
            sprintf "Db.Internal.raw_read: db: %s off: %d len: %d read: %d"
              db.data_fn off len read in
          failwith err_msg
      end;
      Bytes.unsafe_to_string buff
    *)

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
       
      let bytes_read = Unix.(read file_data.fd_file my_buffer lseek_offset int_len  ) in
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
   
  let write_bytes file_data my_buffer =
     (* go to end of data file *)
     let off = Unix.(lseek file_data.fd_file 0 SEEK_END) in
     let len = Bytes.length my_buffer in
  
(*     Unix.single_write fd  ~buf: my_buf  ~pos: 0  ~len:  7  ;; *)

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
