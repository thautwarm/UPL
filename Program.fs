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
    | [|a|] ->
        let { src_files=src_files
            ; sig_files=sig_files
            ; out_dir=out_dir
            ; out_lib_name=out_lib_name
            } = Json.deserialize<Args> a
        Modular.smlfs_compile
            src_files
            sig_files
            out_lib_name
            out_dir
        0
    | _ ->
    printfn "invalid json input!"
    1 // return an integer exit code
