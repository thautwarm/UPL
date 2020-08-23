module Infer
open Core
open HM
open HMUnification
open CamlCompat
open Common
open Exceptions

// infer type
let rec infer_t : global_st -> local_st -> Surf.ty_expr -> t =
    fun global_st local_st ty_expr ->
    let pos = local_st.pos in
    let {new_tvar=new_tvar; unify=unify; prune=prune} = global_st.tcstate in

    match
        match ty_expr with
        | Surf.TNew tn ->
            sprintf "%A@(%A, %A)" tn pos.line <| gensym global_st
            |> TNom |> T
        | Surf.TImplicit ty_expr ->
            T(TImplicit <| infer_t global_st local_st ty_expr)
        | Surf.TQuery(label, ty_expr) ->
            let ret = T <| infer_t global_st local_st ty_expr in
            let _ = DArray.push global_st.queries (label, ret) in
            ret
        | Surf.TSym a ->
            T <| TNom a
        | Surf.TVar "_" ->
            T <| new_tvar()
        | Surf.TVar a ->
            match Map.tryFind a local_st.type_env with
            | None ->
                raise <| InferError(pos, UnboundTypeVariable a)
            | Some t -> t
        | Surf.TApp(f, b) ->
            let f = infer_t global_st local_st f in
            let b = infer_t global_st local_st b in
            T <| TApp(f, b)
        | Surf.TTup xs ->
            List.map (infer_t global_st local_st) xs |> TTup |> T
        | Surf.TForall(bounds, p) ->
            let unique_ns = List.map un bounds in
            let type_env = local_st.type_env in
            let new_bindings =
                [|for un in unique_ns -> un.name, TBound un|] in
            let p =
                infer_t
                    global_st
                    { local_st
                        with type_env = Map.let_seq type_env new_bindings }
                    p
            in T <| TForall(unique_ns, p)
        | Surf.TArrow(a, b) ->
            let a = infer_t global_st local_st a in
            let b = infer_t global_st local_st b in
            T <| TArrow(a, b)
        with
    | T x -> x
    | typetype ->
        let tvar = new_tvar() in
        let typetype' = T tvar in
        if unify (typetype') typetype
        then
            raise <|
            InferError(pos, UnificationFail(prune typetype', prune typetype))
        else prune tvar