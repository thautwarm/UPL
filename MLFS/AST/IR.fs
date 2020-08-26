module IR
open Common
open CamlCompat

// for type class resolution
// this record holds a variable(field 'exp') with the info of
// 1. pos: where it's defined(source code position)
// 2. t: what the type
// 3. isPruned:is the type pruned
// variable naming convention:
// evi, inst, ei, *_ei, *_evi, *_inst
type evidence
    = { mutable t   : HM.t
      ; pos : pos
      ; impl : expr_impl
      ; mutable isPruned : bool
      }
  with override this.ToString() =
        let {t=t; pos=pos} = this
        in sprintf "%O %O" t pos

// instance resolution context.
// it's local context.
// global evidence instances are stored somewhere
// else, whose type is
//    '(classKind, evidence_instance array) map'
// , for the performance concern
and evidence_resolv_ctx = evidence list


and expr =
    { pos : pos
    ; typ : HM.t
    ; impl : expr_impl
    }

and decl =
| Perform of expr
| Assign of symbol * HM.t * expr

and expr_impl =
| ETypeVal of HM.t
| EVal of value
| EExt of string
| EVar of symbol
| ELet of decl list * expr
| EITE of expr * expr * expr
| EFun of symbol * HM.t * expr
| EThunk of expr
| EApp of expr * expr
| ETup of expr list
| EIm of HM.t * evidence_resolv_ctx


let expr pos typ impl = {pos=pos; typ=typ; impl=impl}

let apply_implicits : expr_impl -> HM.t darray -> HM.t -> pos -> evidence_resolv_ctx -> expr =
    fun e implicits final_type pos local_implicits ->
    let e = ref e in
    for im in implicits do
        e := EApp
              ( {pos=pos; typ=HM.top_t; impl= !e}
              , {pos=pos; typ=HM.top_t; impl= EIm(im, local_implicits)}
              )
    done;
    {pos = pos; typ = final_type; impl= !e}

let apply_explicits : expr_impl -> expr array -> HM.t -> pos -> expr =
    fun e_impl explicits final_type pos ->
    let e_impl = ref e_impl in
    for explicit in explicits do
        e_impl := EApp(expr pos HM.top_t !e_impl, explicit);
    done;
    expr pos final_type !e_impl

type ('ctx, 'a) trans = 'ctx -> 'a -> 'a
type 'ctx transformer =
    { expr : 'ctx transformer -> ('ctx, expr) trans
    ; decl : 'ctx transformer -> ('ctx, decl) trans
    ; expr_impl : 'ctx transformer -> ('ctx, expr_impl) trans
    }

let gen_trans_decl : 'ctx transformer -> 'ctx -> decl  -> decl =
    fun ({expr=expr_} as self) ctx ->
    let (!) = expr_ self ctx in
    function
    | Perform e -> Perform !e
    | Assign(sym, hmt, expr) ->
        Assign(sym, hmt, !expr)

let gen_trans_expr_impl : 'ctx transformer -> 'ctx -> expr_impl  -> expr_impl
    = fun ({expr=expr_; decl=decl_} as self) ctx ->
    let (!) = expr_ self ctx in
    function
    | ETypeVal _ | EExt _ | EVal _ | EVar _ as root -> root
    | ELet(decls, expr) ->
        ELet(List.map (decl_ self ctx) decls, !expr)
    | EITE(a1, a2, a3) ->
        EITE(!a1, !a2, !a3)
    | EFun(s, ann, e) -> EFun(s, ann, !e)
    | EThunk(e) -> EThunk(!e)
    | EApp(f, a) -> EApp(!f, !a)
    | ETup xs -> ETup <| List.map (!) xs
    | EIm _ as e -> e

let gen_trans_expr : 'ctx transformer -> 'ctx -> expr  -> expr
    = fun ({expr_impl=expr_impl_} as self) ctx ->
    fun ({impl = impl} as expr) ->
    { expr with impl = expr_impl_ self ctx impl }

[<GeneralizableValue>]
let transformer =
    { expr = gen_trans_expr
    ; decl = gen_trans_decl
    ; expr_impl = gen_trans_expr_impl
    }

let evidence t pos impl isPruned =
    {t = t; pos = pos; impl = impl; isPruned = isPruned}


type library_signature =
    { module_names : (string, int) map
    ; implicits : (HM.t * evidence array) array
    }
