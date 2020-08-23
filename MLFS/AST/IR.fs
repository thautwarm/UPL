module IR
open Common
open CamlCompat

type expr =
    { pos : pos
    ; typ : HM.t option
    ; expr : expr_impl
    }
and decl =
| Perform of expr
| Assign of symbol * HM.t * expr
and expr_impl =
| ETypeVal of HM.t
| EVal of value
| Ext of string
| EVar of symbol
| ELet of decl list * expr
| EITE of expr * expr * expr
| EFun of symbol * HM.t * expr
| EApp of expr * expr
| ETup of expr list
| EIm of expr * HM.t * Core.inst_resolv_ctx

let apply_implicits : expr_impl -> HM.t darray -> HM.t -> pos -> Core.inst_resolv_ctx -> expr =
    fun e implicits final_type pos local_implicits ->
    let e = ref e in
    for im in implicits do
        e := EIm(
            {pos=pos; typ=None; expr= !e}, im, local_implicits)
    done;
    {pos = pos; typ = Some final_type; expr = !e}

