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

=====
cd tests
dune build ./test_list.exe
dune exec  ./test_list.exe

dune is 
(executable
  (name test_list)
  (libraries ounit2)
)


#####
dune
(test
  (name test_foo)
  (libraries ounit2)
)


dune runtest
dune exec ./test_foo.exe
