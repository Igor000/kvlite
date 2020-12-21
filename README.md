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



## To build tests
cd ./tests
# Build multiple tests using this dune
(tests
  (names test_foo test_list)
  (libraries ounit2)
)


#####
dune exec runtest

## execute individual tests
dune exec ./test_list.exe
dune exec ./test_foo.exe



## To build unitest test_list and test_foo
ocamlfind ocamlc -o test_list -package oUnit -linkpkg -g  test_list.ml
./test_list

ocamlfind ocamlc -o test_foo -package oUnit -linkpkg -g foo.ml test_foo.ml
./test_foo 


## see example of diff dune files
in https://github.com/dune-universe/dune-universe/
## For example
https://github.com/dune-universe/dune-universe/blob/master/packages/elasticsearch-cli.1.2/src/dune

## utop commands

#list;;      ## display all available modules
open Base;;  ## opens Base 
open Core;;  ## opens Core

#use "misc.ml";;      ## load misc.ml as a module
#mod_use "main.ml";;  ## load a file / not a module

#load "unix.cma";;    ## load Unix module
