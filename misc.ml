  (*
#use "./misc.ml";;

let r11 = Dbmod.create_db_data "key1" "val1";;
val r11 : Dbmod.db_data =
  {Dbmod.created = 1590890792.6847849; key_size = 4; key = "key1";
   value_size = 4; value = "val1"}

Dbmod.print_created r11;;

     let a, b = Dbmod.get_key  r11;;

let r12 = Dbmod.create_db_data "key2" "val2";;
Dbmod.get_key_value r12;;


   *
   *
   * *)
open Base;;
open Base.Poly;;
open Stdio;;

  (*
   *  In utop to load a module :
   #use "./misc.ml";;

   * To use a function in the module: 
   
     utop # Misc.trim " sd fsd ";;
- : string = "sd fsd"
    
   *)

module Dbmod = struct
  type db_data = { created : float; key_size: int; key  : string; value_size : int; value: string }

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
     let var2 : Dbmod.db_data  = Marshal.from_bytes var_as_bytes  0 in
     var2;;


end;;


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



module M = struct 
  let x = 42 
end;;
