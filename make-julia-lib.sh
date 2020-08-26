cd julia-lib
mlfsc prim.mlfs --name prim --o ./
mlfsc --be julia ./prim.mlfso --o prim.jl
cd ..