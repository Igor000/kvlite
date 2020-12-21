#/bin/bash
rm -f *.dat
dune clean
dune build ./main.exe
dune exec  ./main.exe
