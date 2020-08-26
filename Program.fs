open FSharp.Json
open Exceptions
open Common
// compile source code files(.mlfs)
// - signature files(.mlfsa) and,
// - ir files/object files (.mlfso)
// other .mlfsa files might be required
// in the arguments via --sigs = "xxx.mlfsa;yyy.mlfsa"
// because
type Compile =
    { src_files : string list
    ; sig_files : string list
    ; out_dir   : string
    ; out_lib_name: string
    }
// co
type Assembly =
    { object_files : string list
    ; out : string
    ; backend : string
    }

type Subcommands =
| Compile of Compile
| Assembly of Assembly

[<EntryPoint>]
let main argv =
    try
    match argv with
    | [|a|] ->
        match Json.deserialize<Subcommands> a with
        | Compile { src_files=src_files
                  ; sig_files=sig_files
                  ; out_dir=out_dir
                  ; out_lib_name=out_lib_name
                  } ->
            Modular.smlfs_compile
                src_files
                sig_files
                out_lib_name
                out_dir
            0
        | Assembly { backend = backend
                   ; out = out
                   ; object_files = object_files
                   } ->
            Modular.smlfs_assembly object_files out backend
            0
    | _ ->
        printfn "invalid json input!"
        1 // return an integer exit code
    with InferError({line=line; col=col; filename=filename}, a) as e ->
     printfn "%O, line %O, column %O:\n%O" filename line col a
     let path = System.IO.Path.GetTempFileName()
     printfn "Writing stack trace log to %s. You may raise an issue with this file attached." path
     CamlCompat.writeFile path e.StackTrace
     1
