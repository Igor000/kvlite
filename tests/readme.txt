cd tests
dune build ./test_list.exe
dune exec  ./test_list.exe

dune is 
(executable
  (name test_list)
  (libraries ounit2)
)
