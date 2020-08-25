module Infer
open Core
open HM
open HMUnification
open CamlCompat
open Common
open Exceptions
open TypeClass

type type_info =
| NoPropagation
| InstantiateTo of t

let map_type_info : (t -> t) -> type_info -> type_info =
    fun f ->
    function
    | NoPropagation -> NoPropagation
    | InstantiateTo t -> InstantiateTo <| f t

type topdown_check = { run : (type_info * inst_resolv_ctx) -> IR.expr }
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
                [|for un in unique_ns -> un.name, T <| TBound un|] in
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
        if not <| unify (typetype') typetype
        then
            raise <|
            InferError(pos, UnificationFail(prune typetype', prune typetype))
        else prune tvar


// infer from Surf.decl
let rec infer_decls : global_st -> local_st -> Surf.decl list -> bool -> IR.decl Lazy list * local_st =
    fun global_st local_st decls is_global ->
    let {prune=prune; new_tvar=new_tvar}  = global_st.tcstate in
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
                raise <| InferError(local_st.pos, UnboundVariable user_sym)
            | Some ty ->
                DArray.push global_st.queries (label, ty);
                recurse (rev_decls, local_st) tl
        | Surf.DLoc pos ->
            recurse (rev_decls, {local_st with pos = pos}) tl
        | Surf.DAnn(user_sym, ty_expr) ->
            match Dict.tryFind annotated user_sym with
            | Some _ ->
                raise
                <| InferError(local_st.pos, UnusedAnnotation user_sym)
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
                        IR.evidence im local_st.pos (IR.EVar symgen) false
                    let _ =
                        global_st.global_implicits_deltas.[class_head] <-
                        1 + Dict.getForce global_st.global_implicits_deltas class_head (fun _ -> 0)
                    []
                | TImplicit im ->
                   let inst = IR.evidence im local_st.pos (IR.EVar symgen) false in
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
            let symgen, ann_ty, scoped_type_variables, local_st =
                match Dict.tryFind annotated user_sym with
                | Some scoped_type_variables ->
                    local_st.symmap.[user_sym]
                    , prune (local_st.type_env.[user_sym])
                    , scoped_type_variables
                    , local_st
                | _ ->
                    let symgen = gensym global_st user_sym
                    let tvar = new_tvar()
                    let local_st =
                        { local_st
                          with symmap =
                                Map.add user_sym symgen local_st.symmap
                               type_env =
                                Map.add user_sym tvar local_st.type_env
                        }
                    symgen, tvar, [], local_st
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

        | Surf.DOpen expr ->
            let topdown_check = infer_expr global_st local_st expr
            let { pos=pos
                ; local_implicits=local_implicits
                ; type_env = old_type_env
                ; symmap = old_symmap
                } = local_st
            let module' = topdown_check.run (NoPropagation, local_implicits)
            let fields_type = new_tvar()
            let target =
                    TApp
                      ( namespace_class
                      , TTup[module'.typ; fields_type]
                      )
            let _ = inst_resolve global_st local_implicits target pos
            let (|ValidNamespace|InvalidNamespace|) x =
                match x with
                | TTup xs ->
                    let rec recurse res = function
                        | [] -> ValidNamespace res
                        | TNom n::tl ->
                            recurse (n::res) tl
                        | _ -> InvalidNamespace x
                    in recurse [] xs
                | _ -> InvalidNamespace x
            match prune fields_type with
            | InvalidNamespace x -> raise <| InferError(pos, InvalidNamespaceType x)
            | ValidNamespace fieldnames ->
            let user_sym_gen = gensym global_st "user_open"
            let sym_gen = gensym global_st "open"
            annotated.[user_sym_gen] <- [];
            let local_st = {
                 local_st
                   with type_env =
                            Map.add user_sym_gen module'.typ old_type_env
                        symmap =
                            Map.add user_sym_gen sym_gen old_symmap
               }
            let rev_decls =
                    lazy IR.Assign(sym_gen, module'.typ, module') :: rev_decls
            let tl =
                [ for f in fieldnames
                -> Surf.DBind(f, Surf.EField(Surf.EVar user_sym_gen, f))
                ]
                @ tl
            in recurse (rev_decls, local_st) tl

    in recurse ([], local_st) decls

and infer_expr : global_st -> local_st -> Surf.expr -> topdown_check =
    fun st_global st_local ->
    let { prune = prune
        ; unifyImplicits = unifyImplicits
        ; unify = unify
        ; unifyInsts = unifyInsts
        ; new_tvar = new_tvar
        } = st_global.tcstate

    let pos = st_local.pos
    let apply_prop : type_info -> t -> t * t darray
        = fun ti me ->
        let implicits = darray()
        match ti with
        | NoPropagation -> me, implicits
        | InstantiateTo instiated_type ->
            if not <| unifyImplicits implicits instiated_type me then
                raise <| InferError(pos, UnificationFail(prune instiated_type, prune me))
            else instiated_type, implicits

    let propagate_for_leaf : t -> IR.expr_impl -> topdown_check =
        fun leaf_type impl ->
            { run =
                fun (ti, local_implicits) ->
                let leaf_type = prune leaf_type
                let leaf_type, implicits = apply_prop ti leaf_type
                match prune leaf_type with
                | T t ->
                    IR.expr pos leaf_type (IR.ETypeVal t)
                | leaf_type ->
                    IR.apply_implicits impl implicits leaf_type pos local_implicits
            }
    function
    | Surf.ECoerce expr ->
        let topdown_check = infer_expr st_global st_local expr
        { run = fun (_, local_implicits) ->
            topdown_check.run(NoPropagation, local_implicits)
        }
    | Surf.EExt external ->
        { run = fun (ti: type_info, local_implicits) ->
            let t = new_tvar()
            let t, implicits = apply_prop ti t
            IR.apply_implicits (IR.EExt external) implicits t pos local_implicits
        }
    | Surf.EQuery(label, e) ->
        let topdown_check = infer_expr st_global st_local e
        let queries = st_global.queries
        { run = fun (ti: type_info, local_implicits) ->
            let e = topdown_check.run (ti, local_implicits)
            DArray.push queries (label, e.typ);
            e
        }
    | Surf.ELoc(pos, expr) ->
        infer_expr st_global ({st_local with pos = pos}) expr
    | Surf.ETup xs ->
        let topdown_checks = List.map (infer_expr st_global st_local) xs
        let n_xs = List.length xs
        { run = fun (ti: type_info, local_implicits) ->
            let ts = [for _ = 1 to n_xs do new_tvar()]
            let tup_t = TTup ts
            let tup_t, implicits = apply_prop ti tup_t
            let elts =
                [ for (topdown_check, t) in List.zip topdown_checks ts ->
                  topdown_check.run(InstantiateTo t, local_implicits)
                ]
            IR.apply_implicits (IR.ETup elts) implicits tup_t pos local_implicits
        }
    | Surf.EApp(f, arg) ->
        let topdown_check_f = infer_expr st_global st_local f
        let topdown_check_arg = infer_expr st_global st_local arg
        { run = fun (ti:type_info, local_implicits) ->
            let arg_t = new_tvar()
            let ret_t = new_tvar()
            let ret_t, implicits = apply_prop ti ret_t
            let ir_f =
                topdown_check_f.run
                    ( InstantiateTo <| TArrow(arg_t, ret_t)
                    , local_implicits
                    )
            let ir_arg =
                topdown_check_arg.run
                    ( InstantiateTo <| arg_t
                    , local_implicits
                    )
            IR.apply_implicits
                (IR.EApp(ir_f, ir_arg))
                implicits
                ret_t
                pos
                local_implicits
        }
    | Surf.EFun(user_sym, expr) ->
        let symgen = gensym st_global user_sym
        let arg_t = new_tvar()
        let ret_t = new_tvar()
        let st_local =
            let {symmap=old_symmap; type_env=old_type_env} = st_local in
            { st_local
               with
                symmap = Map.add user_sym symgen old_symmap
                type_env = Map.add user_sym arg_t old_type_env
            }
        let topdown_check = infer_expr st_global st_local expr
        { run = fun (ti:type_info, local_implicits) ->
            let arrow_t, implicits = apply_prop ti <| TArrow(arg_t, ret_t)
            // the reason why we also need to propagate local implicits
            // instead of only for types
            let local_implicits =
                match prune arg_t with
                | TImplicit instance ->
                    IR.evidence instance pos (IR.EVar symgen) false::local_implicits
                | _ -> local_implicits
            let ir_body =
                topdown_check.run
                    ( InstantiateTo ret_t
                    , local_implicits
                    )
            IR.apply_implicits
                (IR.EFun(symgen, arg_t, ir_body))
                implicits
                arrow_t
                pos
                local_implicits
        }
    | Surf.EField(subject, fieldname) ->
        let topdown_check_subject =
                infer_expr st_global st_local subject
        { run = fun (ti: type_info, local_implicits) ->
            let ir_sub = topdown_check_subject.run(NoPropagation, local_implicits)
            let ty_sub = ir_sub.typ
            let ret_tv, implicits = apply_prop ti <| new_tvar()
            let target =
                        TApp
                          ( field_class
                          , TTup[ty_sub; TNom fieldname; ret_tv]
                          )
            let implicit_call =
                    IR.EApp
                        ( IR.expr pos top_t (IR.EIm(target, local_implicits))
                        , ir_sub
                        )
            IR.apply_implicits
                implicit_call
                implicits
                ret_tv
                pos
                local_implicits
        }
    | Surf.ELet(decls, expr) ->
        let (delay_decls: IR.decl Lazy list, st_local) =
                infer_decls st_global st_local decls false
        let topdown_check = infer_expr st_global st_local expr
        { run = fun (ti: type_info, local_implicits) ->
             let ir_body = topdown_check.run(ti, local_implicits)
             let inferred_decls = [for f in delay_decls -> f.Force()]
             let ir_let = IR.ELet(inferred_decls, ir_body)
             IR.expr pos ir_body.typ ir_let
        }
    | Surf.EITE(cond, arm1, arm2) ->
        let topdown_check_cond =
                infer_expr st_global st_local cond
        let topdown_check_arm1 =
                infer_expr st_global st_local arm1
        let topdown_check_arm2 =
                infer_expr st_global st_local arm2
        { run = fun (ti: type_info, local_implicits) ->
            let t, implicits = apply_prop ti <| new_tvar()
            let ti = InstantiateTo t
            let ir_arm1 = topdown_check_arm1.run(ti, local_implicits)
            let ir_arm2 = topdown_check_arm2.run(ti, local_implicits)
            // because topdown propagation will give more information
            // after checking the return, so we propagate return types firstly.
            let ir_cond =
                 topdown_check_cond.run
                    ( InstantiateTo bool_t
                    , local_implicits
                    )
            let ir_ite = IR.EITE(ir_cond, ir_arm1, ir_arm2)
            IR.apply_implicits ir_ite implicits t pos local_implicits
        }
    | Surf.EVal value ->
        let t =
            match value with
            | I8 _ -> int_ts.[8]
            | I16 _ -> int_ts.[16]
            | I32 _ -> int_ts.[32]
            | I64 _ -> int_ts.[64]
            | U8 _ -> uint_ts.[8]
            | U16 _ -> uint_ts.[16]
            | U32 _ -> uint_ts.[32]
            | U64 _ -> uint_ts.[64]
            | F32 _ -> float_ts.[32]
            | F64 _ -> float_ts.[64]
            | Ch _ -> char_t
            | Bl _ -> bool_t
            | Str _ -> str_t
        let ir_val = IR.EVal value
        propagate_for_leaf t ir_val
    | Surf.EVar user_sym ->
        match Map.tryFind user_sym st_local.type_env with
        | None -> raise <| InferError(pos, UnboundVariable user_sym)
        | Some var_t ->
        let var_t = prune var_t
        propagate_for_leaf var_t (IR.EVar user_sym)
