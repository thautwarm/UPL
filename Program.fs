// Learn more about F# at http://fsharp.org

open System
open FSharp.Json


type Args =
    { src_files : string list
    ; sig_files : string list
    ; out_dir   : string
    ; out_lib_name: string
    }

[<EntryPoint>]
let main argv =
    match argv with
    | [|a|] -> Json.deserialize<Args> a
    | _ ->
    printfn "Hello World from F#!"
    0 // return an integer exit code
