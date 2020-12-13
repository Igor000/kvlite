# kvlite

# To complie (not working now :{

ocamlfind ocamlopt -o main_test -package base,stdio,unix  misc.ml  main.ml  

### To build using "dune"

# To build:
# update "dune" file and run

dune clean
dune build ./main.exe
dune exec  ./main.exe

# To run the executable
_build/default/main.exe


## To build unitest test_list and test_foo
ocamlfind ocamlc -o test_list -package oUnit -linkpkg -g  test_list.ml
./test_list

ocamlfind ocamlc -o test_foo -package oUnit -linkpkg -g foo.ml test_foo.ml
./test_foo 
