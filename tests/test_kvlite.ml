open OUnit2;;

open Kvlite;;

module M=Misc.M

let test1 test_ctxt = assert_equal "x" (Foo.unity "x");;

let test2 test_ctxt = assert_equal 100 (Foo.unity 100);;

let test3 test_ctxt = assert_equal 5   (Misc.add 2 3 );;

(* Name the test cases and group them together *)
let suite =
"suite">:::
 ["test1">:: test1;
  "test2">:: test2;
  "test3">:: test3]
;;

let () =
  run_test_tt_main suite
;;
