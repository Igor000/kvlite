# To complie (not working now :{

ocamlfind ocamlopt -o main_test -package base,stdio,unix  misc.ml  main.ml  

### To build using "dune"

# To build:
# update "dune" file and run

dune clean
dune build ./main.exe
dune exec  ./main.exe

dune clean
dune exec ./main.exe



# To run the executable
_build/default/main.exe

