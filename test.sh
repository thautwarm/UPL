cd examples
mlfsc simplest-hrt.mlfs --name shrt --o ./out-simplest-hrt
mlfsc --be julia ./out-simplest-hrt/*.mlfso --o ./out-simplest-hrt/main.jl
mlfsc prim.mlfs --name prim --o ./out-prim
mlfsc record.mlfs --name record --o ./out-record
mlfsc functor.mlfs --name functor --o ./out-functor
mlfsc prim2.mlfs --name prim2 --o ./out-prim2
mlfsc import_prim2.mlfs --sigs "./out-prim2/prim2.mlfsa" --name import_prim2 --o ./out-import_prim2
cd ..