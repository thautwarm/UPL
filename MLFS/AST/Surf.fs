module Surf
open CamlCompat
open Common

type ty_expr =
| TForall of symbol list * ty_expr
| TApp of ty_expr * ty_expr
| TArrow of ty_expr * ty_expr
| TTup of ty_expr list
| TVar of symbol
| TImplicit of ty_expr
| TNew of symbol
| TQuery of string * ty_expr

type decl =
| DAnn of symbol * ty_expr
| DOpen of ty_expr
| DLoc of pos
| DBind of symbol * expr
| DQuery of string * symbol

and expr =
| ELoc of pos * expr
| EVar of symbol
| EVal of value
| ELet of decl list * expr
| EITE of expr * expr * expr
| EFun of symbol * expr
| EApp of expr * expr
| EField of expr * symbol
| ETup of expr list
| EExt of string
| EQuery of string * expr

type module_record =
    { name : symbol
    ; imports : (string * string) list
    ; decls : decl list
    }
