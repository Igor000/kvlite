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

