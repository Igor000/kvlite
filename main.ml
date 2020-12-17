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

  Dbmod.close_simple my_file_data_int.fd_file ;;
  let my_file_data_int = Dbmod.open_existing_file file_name_int;;
  
  let lseek_offset = 0;;
  let result6  = Dbmod.read_int my_file_data_int 0 in
  print_endline ("Result of read_int   = " ^ Int64.to_string result6);;

   

(*
print_endline res2;;
   *)

(*
M.x;;
Misc.x;;
let var1 = Dbmod.create_db_data "key1" "val1";;
Dbmod.get_key_value var1;;


let res1, res2 = Dbmod.get_key_value var1;;
print_endline res1;;

   *)

