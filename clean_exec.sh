#/bin/bash
rm -f *.dat
rm -f test*.dat.idx  
dune clean
dune build ./main.exe
dune exec  ./main.exe
