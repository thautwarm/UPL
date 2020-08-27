cd examples
mlfsc prim2.mlfs --name prim2 --o ./out-prim2
mlfsc idss.mlfs --sigs "./out-prim2/prim2.mlfsa" --name idss --o ./out-idss
cd ..