module Common

(*
// position
// variable naming convention:
// pos, pos', position, *_pos, etc
*)
type pos = { line : int; col : int; filename : string}
    with
        override this.ToString() =
          sprintf
            "at %s, line %d, column %d"
            this.filename this.line this.col

type symbol = string


type value =
| I8 of int8
| I16 of int16
| I32 of int32
| I64 of int64
| U8 of uint8
| U16 of uint16
| U32 of uint32
| U64 of int64
| F32 of float32
| F64 of float
| Ch of char
| Bl of bool
| Str of string
