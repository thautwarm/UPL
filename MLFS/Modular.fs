module Modular
open IR
open Core
open CamlCompat
open Exceptions
open TypeClass
open Common
open FSharp.Json
open HM
open HMUnification
open Infer

// what do post_infer_* do?
// 1. check if any unsolve type variables
// 2. prune types in IR
// 3. solve type class instances
let mk_post_infer (g: global_st) =
    let { HMUnification.prune_with_var_check
           = prune_with_var_check
        } = g.tcstate
    let unsolve_vars = DSet.ofList []
    let collect_unsolved(v : int) =
        DSet.add unsolve_vars v
    let _check_prune pos a =
        let ret = prune_with_var_check collect_unsolved a
        if DSet.isEmpty unsolve_vars
        then ret
        else
        raise
        <| InferError(pos, UnsolvedTypeVariables ret)
    let post_infer_decl
        ({ expr = expr_
         } as self) pos =
        function
        | Perform(expr) -> Perform <| expr_ self pos expr
        | Assign(a, t, e) ->
            Assign(a, _check_prune pos t, expr_ self pos e)
    let post_infer_expr
        ({ expr_impl = expr_impl_
        } as self) pos =
        function
        | {pos = pos; typ = Some typ; impl=impl} ->
            let impl = expr_impl_ self pos impl
            let ty = _check_prune pos typ
            {pos = pos; typ=Some typ; impl=impl}
        | {pos = pos; typ = None; impl=impl} as o ->
            let impl = expr_impl_ self pos impl
            { o with impl = impl }
    let post_infer_expr_impl
        ({ expr = expr_
        } as self) pos =
        function
        | EIm(expr, t, insts) ->
            let expr = expr_ self pos expr
            let t = _check_prune pos t
            IR.EApp(expr, inst_resolve g insts t pos)
        | ETypeVal t ->
            ETypeVal <| _check_prune pos t
        | a -> gen_trans_expr_impl self pos a

    let self =
         { expr = post_infer_expr
         ; expr_impl = post_infer_expr_impl
         ; decl = post_infer_decl
         }
    let main = post_infer_expr_impl self
    let post_infer_decls pos decls =
        match
            ELet(decls, expr pos None <| IR.EVal(I64 0L))
            |> main pos
          with
        | ELet(decls, _) -> decls
        | _ -> failwith "impossible, otherwise F# booooom!"
    post_infer_decls

let module_gensym n = "mlfs_" ^ n

type path = string
let smlfs_compile : path list -> path list -> symbol -> string -> unit =
    fun src_fles sig_files out_library_name out_directory ->
    let loadedModules = DSet.ofList []
    let g = empty_global_st()
    failwith ""


let load_module :
    symbol
    -> (symbol * symbol) list
    -> Surf.decl list
    -> path
    -> global_st
    -> IR.decl darray
    = fun module_name imports decls path g ->
    let {prune = prune} = g.tcstate
    let g = {g with current_module_name = module_name}
    let mutable l = { empty_local_st with type_env = predef }
    for (import_module, alias) in imports do
        let module_type = TApp(module_class, TNom import_module)
        l <-
          { l
            with
              type_env =
                Map.add alias module_type l.type_env
              symmap =
                Map.add
                    alias (module_gensym import_module) l.symmap
          }
    done;
    let results, local_tc = infer_decls g l decls true in
    let results = darray([|for i in results -> i.Force()|])
    let pos = {line = 1; col = 1; filename = path} in
    let untyped_expr impl = {typ=None; pos=pos;impl=impl}
    let field_inst_cnt, field_instances =
            (Dict.getForce g.global_implicits_deltas field_class <| fun _ -> 0)
            , Dict.getForce g.global_implicits field_class <| fun _ ->
                darray()
    let namespace_inst_cnt, namespace_instances =
            (Dict.getForce g.global_implicits_deltas namespace_class <| fun _ -> 0)
            , Dict.getForce g.global_implicits namespace_class <| fun _ ->
                darray()
    let module_type = TApp(module_class, TNom(module_name))

    let mk_field (user_sym: symbol) (gen_exp:expr_impl) (pruned_type: t) =
        let instance_type =
                TApp(field_class, TTup [module_type; TNom user_sym; pruned_type])
        DArray.push field_instances
        <| evidence instance_type pos gen_exp true
    let fields = dict()

    for KV(user_sym, ty) in local_tc.type_env do
        if not <|
            (Dict.contains fields user_sym
             && Some ty <> Map.tryFind user_sym predef)
        then
            let ty = prune ty in
            let gen_exp =
                match ty, Map.tryFind user_sym local_tc.symmap with
                | T t, None -> ETypeVal t
                // ???
                | _, None -> IR.EVal (I16 0s)
                | _, Some symgen -> EVar symgen
            let method_symgen = gensym g user_sym
            let _ = fields.[user_sym] <- ty
            let _ =
                let gen_func = untyped_expr <| EFun("_", top_t, untyped_expr gen_exp)
                DArray.push results
                <| Assign(method_symgen, TArrow(top_t, ty), gen_func)
            mk_field user_sym (IR.EVar method_symgen) ty
    done;

    g.global_implicits_deltas.[field_class] <- field_inst_cnt + Dict.size fields

    let instance_type =
          TApp( namespace_class
              , TTup
                [ module_type
                ; TTup [for KV(k, t) in fields -> TTup [TNom k; t]]
                ]
              )

    DArray.push namespace_instances <|
        evidence
            instance_type
            pos
            (IR.EVal <| I16 0s)
            true

    g.global_implicits_deltas.[field_class] <- namespace_inst_cnt + 1

    DArray.push results
    <| Assign(module_gensym module_name, module_type, untyped_expr <| IR.EVal (I16 0s))

    results


let load_sigs : path list -> symbol dset -> global_st -> unit =
    fun sig_files loaded_modules g ->
    for each in sig_files do
        let src_code = readFile each
        let { module_names = module_names
            ; implicits = implicits
            } = Json.deserialize<library_signature> src_code
        let reloadedModules = DSet.intersect loaded_modules module_names
        if DSet.isEmpty reloadedModules then
            failwithf
                "modile name conflicts: duplicate modules with the same names:\n %A"
                <| String.concat "\n" (Seq.map (fun a -> "- " ^ a) reloadedModules)
        let _ = DSet.update loaded_modules module_names
        for (t_head, insts) in implicits do
            let global_implicits = Dict.getForce g.global_implicits t_head <| fun _ ->
                darray()
            in ignore <| DArray.extend global_implicits insts
        done
    done

let load_srcs :
    path list
    -> symbol dset
    -> global_st
    -> decl darray * library_signature = failwith ""
