module Codegen.Julia

open Microsoft.FSharp.Text.StructuredFormat
open Microsoft.FSharp.Text.StructuredFormat.LayoutOps
open Microsoft.FSharp.Text.StructuredFormat.Display

open IR
open HM
open Common

let pretty =
    Microsoft
        .FSharp
        .Text
        .StructuredFormat
        .Display
        .layout_to_string
            FormatOptions.Default

let rec type_erasure =
     function
     | Core.T _ -> "String"
     | TApp _ -> "Any"
     | TNom s ->
       match s with
       | "i8" -> "Int8"
       | "i16" -> "Int16"
       | "i32" -> "Int32"
       | "i64" -> "Int64"
       | "u8" -> "UInt8"
       | "u16" -> "UInt16"
       | "u32" -> "UInt32"
       | "u64" -> "UInt64"
       | "str" -> "String"
       | "char" -> "Char"
       | "bool" -> "Bool"
       | _ -> "Any"
     | TTup [] -> "Nothing"
     | TTup xs ->
        List.map type_erasure xs
        |> String.concat ", "
        |> sprintf "Tuple{%s}"
     | TBound _ -> "Any" // just erase
     | TArrow _ -> "Function"
     | TImplicit t | TForall(_, t) -> type_erasure t
     // type variables shall be solved
     | TVar _ -> failwith "compiler internal error"

let (|Leaf|NonLeaf|NonLeafAndUntyped|) x =
    match x with
    | EVal _ -> Leaf x
    | ETypeVal _ -> Leaf x
    | EVar _ -> Leaf x
    | EExt _ -> NonLeaf x
    | EApp _ -> NonLeaf x
    | _ -> NonLeafAndUntyped

let isIdentifier (x: char) =
    x >= 'a' && x <= 'z'
    || x >= 'A' && x <= 'Z'
    || x = '_'
    || x >= '0' && x <= '9'

let rec cg_expr isGlobal =
    function
    | {expr.impl = NonLeaf impl; expr.typ=t} ->
        let t = type_erasure t
        leftL "(" -- cg_expr_impl isGlobal impl ^^ rightL ")" ^^ wordL "::"
        -- wordL t
    | {expr.impl = Leaf impl; expr.typ=t} ->
        let t = type_erasure t
        cg_expr_impl isGlobal impl ^^ wordL "::"
        -- wordL t
    | {expr.impl = impl} ->
        cg_expr_impl isGlobal impl

and cg_expr_impl isGlobal =
    function
    | ETypeVal t -> sprintf "\"%s\"" <| show_t t |> wordL
    | EVar s ->
        if String.forall isIdentifier s
        then wordL s
        else sprintf "var\"%s\"" s |> wordL
    | EVal (U8 i) -> sprintf "UInt8(%s)" (i.ToString("x")) |> wordL
    | EVal (U16 i) -> sprintf "UInt16(%s)" (i.ToString("x")) |> wordL
    | EVal (U32 i) -> sprintf "UInt32(%s)" (i.ToString("x")) |> wordL
    | EVal (U64 i) -> sprintf "UInt64(%s)" (i.ToString("x")) |> wordL
    | EVal (I64 i) -> sprintf "Int64(%d)" i |> wordL
    | EVal (I32 i) -> sprintf "Int32(%d)" i |> wordL
    | EVal (I16 i) -> sprintf "Int16(%d)" i |> wordL
    | EVal (I8 i) -> sprintf "Int8(%d)" i |> wordL
    | EVal (F64 f) -> sprintf "Float64(%f)" f |> wordL
    | EVal (F32 f) -> sprintf "Float64(%f)" f |> wordL
    | EVal (Bl i) -> sprintf "%b" i |> wordL
    | EVal (Ch c) -> sprintf "%A" c |> wordL
    | EVal (Str s) -> sprintf "%A" s |> wordL
    | EExt s -> wordL s
    | ELet(decls, exp) ->
        let decls = List.reduce (@@) <| List.map (cg_decl isGlobal) decls
        let exp = cg_expr isGlobal exp
        wordL "begin" -- (decls @@ exp) @@ wordL "end"
    | EITE(cond, arm1, arm2) ->
        wordL "if"  ^^ cg_expr isGlobal cond
        -- cg_expr isGlobal arm1
        @@ wordL "else"
        -- cg_expr isGlobal arm2
        @@ wordL "end"
    | EFun(arg, t, body) ->
        let t = type_erasure t
        wordL "function" ^^
            leftL "(" ^^ wordL arg ^^ wordL "::" ^^ wordL t
            ^^ rightL ")"
        -- cg_expr false body
        @@ wordL "end"
    | EApp(f, arg)->
        cg_expr isGlobal f ^^ sepL "(" ^^ cg_expr isGlobal arg ^^ rightL ")"
    | ETup [] -> wordL "nothing"
    | ETup xs -> tupleL <| List.map (cg_expr isGlobal) xs
    // implicits shall be solved
    | EIm _ -> failwith "compiler internal error"

and cg_decl isGlobal =
    function
    | Assign(s, t, exp) ->
        if isGlobal then
            wordL "const" ^^ wordL s ^^ wordL "="
            -- cg_expr isGlobal exp
        else
            let t = type_erasure t
            wordL s ^^ wordL t ^^ wordL "=" -- cg_expr isGlobal exp
    | Perform(exp) ->
        cg_expr isGlobal exp

and cg_decls isGlobal xs =
    List.reduce (@@) (List.map (cg_decl isGlobal) xs)
