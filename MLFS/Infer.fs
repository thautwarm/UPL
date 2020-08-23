module Infer
open Core
open HM
open HMUnification
open CamlCompat
open Common
open Exceptions

type type_info =
| NoPropagation
| InstantiateTo of t

type TopdownCheck = { run : (type_info * inst_resolv_ctx) -> IR.expr }
// infer type
let rec infer_t : global_st -> local_st -> Surf.ty_expr -> t =
    fun global_st local_st ty_expr ->
    let pos = local_st.pos in
    let {new_tvar=new_tvar; unify=unify; prune=prune} = global_st.tcstate in

    match
        match ty_expr with
        | Surf.TNew tn ->
            gensym global_st (sprintf "%A" tn)
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


// infer from Surf.decl
let rec infer_decls : global_st -> local_st -> Surf.decl list -> bool -> IR.decl Lazy list * local_st =
    fun global_st local_st decls is_global ->
    let {prune=prune; new_tvar}  = global_st.tcstate in
    let annotated = dict() in
    // a series of functions to postpone the top-down check&unification
    let rec recurse (rev_decls: IR.decl Lazy list, local_st: local_st) =
        function
        | [] -> List.rev rev_decls, local_st
        | hd::tl ->
        match hd with
        | Surf.DQuery(label, user_sym) ->
            match Map.tryFind user_sym local_st.type_env with
            | None ->
                raise <| InferError(local_st.pos, UnboundVar user_sym)
            | Some ty ->
                DArray.push global_st.queries (label, ty);
                recurse (rev_decls, local_st) tl
        | Surf.DLoc pos ->
            recurse (rev_decls, {local_st with pos = pos}) tl
        | Surf.DAnn(user_sym, ty_expr) ->
            match Dict.tryFind annotated user_sym with
            | Some t ->
                raise
                <| InferError(local_st.pos, UnusedAnnotation(user_sym, ty_expr))
            | None ->
            let symgen = gensym global_st user_sym
            let ann_ty = infer_t global_st local_st ty_expr
            let mutable local_st =
                { local_st
                   with symmap = Map.add user_sym symgen local_st.symmap
                        type_env = Map.add user_sym ann_ty local_st.type_env }
            let scoped_type_variables =
                match ann_ty with
                | TForall(ns, _) -> ns
                | TImplicit im when is_global ->
                    let im = prune im
                    match im with
                    | TVar _ | TBound _ ->
                        raise <| InferError(local_st.pos, MalformedTypeClassKind im)
                    | _ ->
                    let class_head = get_type_head im
                    let global_implicits =
                        Dict.getForce global_st.global_implicits class_head <| fun _ ->
                            darray()
                    let _ =
                        DArray.push global_implicits <|
                        IR.evidence_instance im local_st.pos (IR.EVar symgen) false
                    let _ =
                        global_st.global_implicits_deltas.[class_head] <-
                        1 + Dict.getForce global_st.global_implicits_deltas class_head (fun _ -> 0)
                    []
                | TImplicit im ->
                   let inst = IR.evidence_instance im local_st.pos (IR.EVar symgen) false in
                   let _ = local_st <- {local_st with local_implicits = inst::local_st.local_implicits }
                   []
                | _ -> []

            annotated.[user_sym] <- scoped_type_variables;
            recurse (rev_decls, local_st) tl
        | Surf.DBind("_", expr) ->
            let topdown_check = infer_expr global_st local_st expr
            let lazy_decl =
                lazy IR.Perform(topdown_check.run (NoPropagation, local_st.local_implicits))
            let rev_decls = lazy_decl::rev_decls
            recurse (rev_decls, local_st) tl
        | Surf.DBind(user_sym, expr) ->
            let symgen, ann_ty, scoped_type_variables =
                match Dict.tryFind annotated user_sym with
                | Some scoped_type_variables ->
                    local_st.symmap.[user_sym]
                    , prune (local_st.type_env.[user_sym])
                    , scoped_type_variables
                | _ ->
                    let symgen = gensym global_st user_sym
                    let tvar = new_tvar()
                    let local_st =
                        { local_st
                          with type_env =
                                Map.add user_sym symgen
                                local_st.type_env
                               symmap =
                                Map.add user_sym tvar
                                local_st.symmap
                        }
                    symgen, tvar, []
            let local_st =
                if List.isEmpty scoped_type_variables then
                    local_st
                else
                    let old_type_env = local_st.type_env in
                    let new_bindings =
                        [|for un in scoped_type_variables -> un.name, TBound un|]
                    { local_st with type_env = Map.let_seq old_type_env new_bindings }
            let topdown_check_expr = infer_expr global_st local_st expr
            let local_implicits = local_st.local_implicits
            let lazy_decl =
                lazy
                 let propagation = (InstantiateTo ann_ty, local_implicits)
                 IR.Assign(symgen, ann_ty, topdown_check_expr.run propagation)
            let rev_decls = lazy_decl :: rev_decls
            recurse (rev_decls, local_st) tl

        | Surf.DOpen _ -> failwith "TODO"

    in recurse ([], local_st) decls

and infer_expr : global_st -> local_st -> Surf.expr -> TopdownCheck =
    failwith ""
