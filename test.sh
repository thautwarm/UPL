function timec(){
    a=$(($(date +%s%N)/1000000))
    eval $1
    delta=$(($(date +%s%N)/1000000 - $a))
    echo "$delta ms"
}

cd examples

timec "mlfsc simplest-hrt.mlfs --name shrt --o ./out-simplest-hrt"

timec "mlfsc --be julia ./out-simplest-hrt/*.mlfso --o ./out-simplest-hrt/main.jl"
timec "mlfsc prim.mlfs --name prim --o ./out-prim"
timec "mlfsc record.mlfs --name record --o ./out-record"
timec "mlfsc functor.mlfs --name functor --o ./out-functor"
timec "mlfsc prim2.mlfs --name prim2 --o ./out-prim2"
timec "mlfsc import_prim2.mlfs --sigs \"./out-prim2/prim2.mlfsa\" --name import_prim2 --o ./out-import_prim2"
timec "mlfsc idss.mlfs --sigs \"./out-prim2/prim2.mlfsa\" --name idss --o ./out-idss"
cd ..
