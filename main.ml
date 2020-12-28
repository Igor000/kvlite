open Kvlite 

module M=Misc.M
module My_misc=Misc.Misc
module Dbmod=Misc.Dbmod

let () =
  (* print_endline "Hello, world!" in  *)
  let result = Misc.add 2 3 in
  print_endline (string_of_int result);
  let result = Misc.sub 3 1 in
  print_endline (string_of_int result);;

  (* let result = Misc.M.x in  *)
  let result = Misc.M.x in
  print_endline (string_of_int result);;
  print_endline "=========================";;


  let result = My_misc.size_of_int (-1);;
  print_endline ("Size of int " ^ string_of_int (result));;
  print_endline "=========================";;

  print_endline "=== Int to Bytes ========";;
  let var1 = 1000;;
  let var_bytes1 =  Dbmod.marshal_to_bytes var1;;

  print_endline ("Size of var_bytes1 = 1000  => " ^ string_of_int(Bytes.length(var_bytes1)));;  
  print_endline "=========================";;


  let bytes_test = Bytes.of_string("And bye");;
  let bytes_test_buf = Dbmod.create_bytes_data bytes_test;;
  let res1 , res2 = Dbmod.get_bytes_data bytes_test_buf;;
  print_endline("Bytes buff = " ^  string_of_int(res1) ^ "|" ^ Bytes.to_string(res2));;
  print_endline "=========================";;

  let var1 = Dbmod.create_db_data "key1" "val1";;
  let var2 = Dbmod.create_db_data "key22" "val22";;

  let res1, res2 = Dbmod.get_key_value var1 in 
  print_endline (res1 ^ " " ^ res2 );;
  print_endline("Created var1 = " ^  Dbmod.print_created var1);;

  let res1, res2 = Dbmod.get_key_value var2 in 
  print_endline (res1 ^ " " ^ res2 );;
  print_endline("Created var2 = " ^  Dbmod.print_created var2);;

  let var_bytes1 =  Dbmod.marshal_to_bytes var1;;
  let var_bytes2 =  Dbmod.marshal_to_bytes var2;; 

  print_endline ("Size of var_bytes1 = " ^ string_of_int(Bytes.length(var_bytes1)));;  
  print_endline ("Size of var_bytes2 = " ^ string_of_int(Bytes.length(var_bytes2)));;  

  let var1_1 = Dbmod.marshal_from_bytes var_bytes1;;
  let var2_1 = Dbmod.marshal_from_bytes var_bytes2;;

  let res1, res2 = Dbmod.get_key_value var1_1 in 
  print_endline (res1 ^ " " ^ res2 );;
  print_endline("Created var1_1 = " ^  Dbmod.print_created var1_1);;

  let res1, res2 = Dbmod.get_key_value var2_1 in 
  print_endline (res1 ^ " " ^ res2 );;
  print_endline("Created var1_1 = " ^  Dbmod.print_created var2_1);;
  print_endline "=========================";;


  let bytes_test2 = Dbmod.marshal_to_bytes var1;;
  let bytes_test_buf = Dbmod.create_bytes_data bytes_test2;;
  let res1 , res2 = Dbmod.get_bytes_data bytes_test_buf;;
  print_endline("Bytes buff = " ^  string_of_int(res1) ^ "|" ^ Bytes.to_string(res2));;
  let var1_2 = Dbmod.marshal_from_bytes res2;;

  let res1, res2 = Dbmod.get_key_value var1_2 in 
  print_endline (res1 ^ " " ^ res2 );;
  print_endline("Created var1_1 = " ^  Dbmod.print_created var1_2);;
  print_endline "=========================";;


  let file_name = "test1.dat";;
  let res1 = Dbmod.create_file file_name ;;
  print_endline ("Create file " ^ file_name);;
  print_endline "=========================";;

  let my_file_data = Dbmod.open_existing_file file_name;;
  let my_str = "Testing1";;
  let res1 = Dbmod.write_string my_file_data my_str

  let file_name2 = "test_bytes1.dat";;
  let res2 = Dbmod.create_file file_name2 ;;
  print_endline ("Create file " ^ file_name2);;
  print_endline "=========================";;


  (*==========================*)

  let var3 = Dbmod.create_db_data "key3" "val3";;
  print_endline("Created var3 = " ^  Dbmod.print_created var3);;

  let bytes_test3 = Dbmod.marshal_to_bytes var3;;
  
  let my_file_data2 = Dbmod.open_existing_file file_name2;;
  let res3 = Dbmod.write_bytes  my_file_data2  bytes_test3;;
  print_endline ("Size of written bytes  = " ^ string_of_int(res3));;
  print_endline "=========================";;

  let var4 = Dbmod.create_db_data "key40" "val40";;
  print_endline("Created var4 = " ^  Dbmod.print_created var4);;

  let bytes_test4 = Dbmod.marshal_to_bytes var4;;
  
  (*  let my_file_data2 = Dbmod.open_existing_file file_name2;;  *)
  let res4 = Dbmod.write_bytes  my_file_data2  bytes_test4;;
  print_endline ("Size of written bytes  = " ^ string_of_int(res4));;
  print_endline "=========================";;

  (* ------------------- *)
  let file_name_int = "test_in1.dat";;
  let res1 = Dbmod.create_file file_name_int ;;
  print_endline ("Create file " ^ file_name_int);;
  print_endline "=========================";;

  let my_file_data_int = Dbmod.open_existing_file file_name_int;;
  let my_int = 28;;
  print_endline ("my_int   = " ^ string_of_int(my_int));;
  let res5 = Dbmod.write_int my_file_data_int my_int;;
  print_endline ("Result of write_int   = " ^ string_of_int(res5));;

  let cur_pos = Dbmod.get_current_pos my_file_data_int in 
  print_endline ("Curr pos after write_int   = " ^ string_of_int(cur_pos ));;

  let my_int2 = 34;;
  print_endline ("my_int2   = " ^ string_of_int(my_int2));;
  let res5 = Dbmod.write_int my_file_data_int my_int2;;
  print_endline ("Result of write_int   = " ^ string_of_int(res5));;

  let cur_pos = Dbmod.get_current_pos my_file_data_int in 
  print_endline ("Curr pos after write_int   = " ^ string_of_int(cur_pos ));;

  Dbmod.close_simple my_file_data_int.fd_file ;;

  (*   ********************   *)
  let my_file_data_int = Dbmod.open_existing_file file_name_int;;
  
  let cur_pos = Dbmod.get_current_pos my_file_data_int in 
  print_endline ("Curr pos before read_int   = " ^ string_of_int(cur_pos ));;
  let lseek_offset = 0;;
  let result6  = Dbmod.read_int my_file_data_int lseek_offset in
  print_endline ("Result of read_int   = " ^ Int64.to_string result6);;


  let cur_pos = Dbmod.get_current_pos my_file_data_int in 
  print_endline ("Curr pos after read_int   = " ^ string_of_int(cur_pos ));;

  let lseek_offset = 8;;
  let result6  = Dbmod.read_int my_file_data_int lseek_offset in
  print_endline ("Result of read_int2   = " ^ Int64.to_string result6);;

  let cur_pos = Dbmod.get_current_pos my_file_data_int in 
  print_endline ("Curr pos after read_int 2  = " ^ string_of_int(cur_pos ));;

 (*-----------------------------*)
 let file_name_bytes = "test_bytes.dat";;
 let res1 = Dbmod.create_file file_name_bytes ;;
 print_endline ("Create file " ^ file_name_bytes);;
 print_endline "=========================";;

 let my_file_data_bytes = Dbmod.open_existing_file file_name_bytes;;
 let my_string  = "Testing bytes !!";;
 print_endline ("my_string   = " ^ my_string);;
 let res5 = Dbmod.write_bytes my_file_data_bytes (Bytes.of_string my_string);;
 print_endline ("Result of write_bytes   = " ^ string_of_int(res5));;

 Dbmod.close_simple my_file_data_bytes.fd_file ;;
 let my_file_data_bytes = Dbmod.open_existing_file file_name_bytes;;
 
 let lseek_offset = 0;;
 (* let result6  = Dbmod.read_bytes my_file_data_bytes 0  (Int64.of_int(String.length my_string) )in *)
 let result6  = Dbmod.read_bytes_current_pos my_file_data_bytes   (Int64.of_int(String.length my_string) )in
 print_endline ("Result of read_bytes   = " ^ Bytes.to_string result6);;

  (*-----------------------------*)
  
  let file_name_full = "test_full_record.dat";;
  let res1 = Dbmod.create_file file_name_full ;;
  print_endline ("Create file " ^ file_name_full);;
  let my_file_data_full = Dbmod.open_existing_file file_name_full;;
  print_endline "=========================";;
  let my_string = "Just a test!";;
  print_endline ("Before read_full_record my_string = " ^  my_string);;
  let result7, start_pos  = Dbmod.write_full_record my_file_data_full Bytes.(of_string my_string) "key1";;

  print_endline ("Result of write_full_record   = " ^ string_of_int( result7) ^ " start_pos =" ^ string_of_int( start_pos));;


  let my_string = "Another test record!";;
  print_endline ("Before read_full_record my_string = " ^  my_string);;
  let result7, start_pos  = Dbmod.write_full_record my_file_data_full Bytes.(of_string my_string) "key2";;
  print_endline ("Result of write_full_record   = " ^ string_of_int( result7) ^ " start_pos =" ^ string_of_int( start_pos));;

  let my_string = "test record ## 3";;
  print_endline ("Before read_full_record my_string = " ^  my_string);;
  let result7, start_pos  = Dbmod.write_full_record my_file_data_full Bytes.(of_string my_string) "key3";;
  print_endline ("Result of write_full_record   = " ^ string_of_int( result7) ^ " start_pos =" ^ string_of_int( start_pos));;

  Dbmod.close_simple my_file_data_full.fd_file ;;
  let my_file_data_full = Dbmod.open_existing_file file_name_full;;

  (*  ===================   *)
  let result_bytes,_,_ = Dbmod.read_full_record_current_pos my_file_data_full ;;
  print_endline ("After read_full_record " ^  (Bytes.to_string result_bytes));;


  let result_bytes,_,_ = Dbmod.read_full_record_current_pos my_file_data_full ;;
  print_endline ("After read_full_record " ^  (Bytes.to_string result_bytes));;

  let result_bytes, _, _ = Dbmod.read_full_record_current_pos my_file_data_full ;;
  print_endline ("After read_full_record " ^  (Bytes.to_string result_bytes));;

(* ********************** *)
let var1 = Dbmod.create_db_data "key1" "val1";;
let var2 = Dbmod.create_db_data "key2" "val2";;
let var3 = Dbmod.create_db_data "key3_3" "val3_3";;


let res1, res2 = Dbmod.get_key_value var1 in 
print_endline (res1 ^ " " ^ res2 );;
print_endline("Created var1 = " ^  Dbmod.print_created var1);;

let res1, res2 = Dbmod.get_key_value var2 in 
print_endline (res1 ^ " " ^ res2 );;
print_endline("Created var2 = " ^  Dbmod.print_created var2);;

let res1, res2 = Dbmod.get_key_value var3 in 
print_endline (res1 ^ " " ^ res2 );;
print_endline("Created var3 = " ^  Dbmod.print_created var3);;


let file_name_full = "test_full_record2.dat";;
let res1 = Dbmod.create_file file_name_full ;;
print_endline ("Create file " ^ file_name_full);;
let my_file_data_full = Dbmod.open_existing_file file_name_full;;
print_endline "=========================";;

let key1, _ = Dbmod.get_key var1 in 
let result1, start_pos1  = Dbmod.write_full_record my_file_data_full (Dbmod.marshal_to_bytes var1) key1 in
print_endline ("Result of write_full_record   = " ^ string_of_int( result1) ^ " start_pos =" ^ string_of_int( start_pos1));

let key2, _ = Dbmod.get_key var2 in 
let result2, start_pos2  = Dbmod.write_full_record my_file_data_full (Dbmod.marshal_to_bytes var2) key2 in 
print_endline ("Result of write_full_record   = " ^ string_of_int( result2) ^ " start_pos =" ^ string_of_int( start_pos2));

let key3, _ = Dbmod.get_key var3 in 
let result3, start_pos3  = Dbmod.write_full_record my_file_data_full (Dbmod.marshal_to_bytes var3) key3 in
print_endline ("Result of write_full_record   = " ^ string_of_int( result3) ^ " start_pos =" ^ string_of_int( start_pos3));

Dbmod.print_hash_map my_file_data_full;

Dbmod.print_hash_map_full_record my_file_data_full;

Dbmod.close_simple my_file_data_full.fd_file ;
let my_file_data_full = Dbmod.open_existing_file file_name_full in

  (*  ===================   *)


let var1_tuple = Dbmod.read_full_record_current_pos my_file_data_full in
let (var1_data, start_pos1, bytes_size1 ) = var1_tuple in
let var1_1 = Dbmod.marshal_from_bytes var1_data in 
let key1, val1 = Dbmod.get_key_value var1_1 in 
begin
  print_endline ("key = " ^ key1  ^ " value = " ^ val1 );
  print_endline("Created var1 = " ^  Dbmod.print_created var1_1);
  print_endline("Start pos = " ^  string_of_int(start_pos1) ^ " byte_size = " ^ Int64.to_string(bytes_size1) );
end;

let var2_tuple = Dbmod.read_full_record_current_pos my_file_data_full in
let (var2_data, start_pos2, bytes_size2 ) = var2_tuple in
let var2_1 = Dbmod.marshal_from_bytes var2_data in
let key2, val2  = Dbmod.get_key_value var2_1 in 
begin
  print_endline ("key = " ^ key2  ^ " value = " ^ val2 );
  print_endline("Created var2 = " ^  Dbmod.print_created var2_1); 
  print_endline("Start pos = " ^  string_of_int(start_pos2) ^ " byte_size = " ^ Int64.to_string(bytes_size2) );
end;

let var3_tuple = Dbmod.read_full_record_current_pos my_file_data_full in
let (var3_data, start_pos3, bytes_size3 ) = var3_tuple in
let var3_1 = Dbmod.marshal_from_bytes var3_data in
let key3, val3 = Dbmod.get_key_value var3_1 in 
begin
  print_endline ("key = " ^ key3  ^ " value = " ^ val3 );
  print_endline("Created var3 = " ^  Dbmod.print_created var3_1);
  print_endline("Start pos = " ^  string_of_int(start_pos3) ^ " byte_size = " ^ Int64.to_string(bytes_size3) );
end;

print_endline ("Testing find_full_record() =========================");
let start_pos = 0 in 
print_endline ("===== Pos = " ^ string_of_int(start_pos) );
let var1_tuple = Dbmod.find_full_record my_file_data_full start_pos in 
let (var1_data, start_pos1, bytes_size1 ) = var1_tuple in
let var1_1 = Dbmod.marshal_from_bytes var1_data in 
let key1, val1 = Dbmod.get_key_value var1_1 in 
begin
  print_endline ("key = " ^ key1  ^ " value = " ^ val1 );
  print_endline("Created var1 = " ^  Dbmod.print_created var1_1);
  print_endline("Start pos = " ^  string_of_int(start_pos1) ^ " byte_size = " ^ Int64.to_string(bytes_size1) );
end;

let start_pos = 50 in 
print_endline ("===== Pos = " ^ string_of_int(start_pos) );
let var2_tuple = Dbmod.find_full_record my_file_data_full start_pos in 
let (var2_data, start_pos2, bytes_size2 ) = var2_tuple in
let var2_1 = Dbmod.marshal_from_bytes var2_data in
let key2, val2  = Dbmod.get_key_value var2_1 in 
begin
  print_endline ("key = " ^ key2  ^ " value = " ^ val2 );
  print_endline("Created var2 = " ^  Dbmod.print_created var2_1); 
  print_endline("Start pos = " ^  string_of_int(start_pos2) ^ " byte_size = " ^ Int64.to_string(bytes_size2) );
end;

let start_pos = 100 in 
print_endline ("===== Pos = " ^ string_of_int(start_pos) );
let var3_tuple = Dbmod.find_full_record my_file_data_full start_pos in 
let (var3_data, start_pos3, bytes_size3 ) = var3_tuple in
let var3_1 = Dbmod.marshal_from_bytes var3_data in
let key3, val3 = Dbmod.get_key_value var3_1 in 
begin
  print_endline ("key = " ^ key3  ^ " value = " ^ val3 );
  print_endline("Created var3 = " ^  Dbmod.print_created var3_1);
  print_endline("Start pos = " ^  string_of_int(start_pos3) ^ " byte_size = " ^ Int64.to_string(bytes_size3) );
end;
(****************** *)


let start_pos = 0 in
Dbmod.get_full_record_and_print my_file_data_full start_pos;

let start_pos = 50 in
Dbmod.get_full_record_and_print my_file_data_full start_pos;

let start_pos = 100 in
Dbmod.get_full_record_and_print my_file_data_full start_pos;


 


let file_name = "test_create.dat" in
  let res1 = Dbmod.create file_name in
  print_endline ("Create file " ^ file_name);
  print_endline ("=========================");




(*
M.x;;
Misc.x;;
let var1 = Dbmod.create_db_data "key1" "val1";;
Dbmod.get_key_value var1;;


let res1, res2 = Dbmod.get_key_value var1;;
print_endline res1;;

   *)

