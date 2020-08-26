cd julia-lib
mlfsc prim.mlfs --name prim --o ./

mlfsc test-prim.mlfs --name test-prim --o ./ --sigs prim.mlfsa
mlfsc --be julia ./*prim.mlfso --o test_prim.jl
if [ "`julia test_prim.jl`" = "true" ]
then
    echo "succeeded.."
else
    echo "test failed"
fi
cd ..