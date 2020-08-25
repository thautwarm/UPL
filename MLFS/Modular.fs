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
        } as self) _ =
        function
        | {pos = pos; typ = typ; impl=impl} ->
            let impl = expr_impl_ self pos impl
            let typ = if typ = top_t
                      then top_t
                      else  _check_prune pos typ
            {pos = pos; typ=typ; impl=impl}
    let post_infer_expr_impl
        ({ expr = expr_
        } as self) pos =
        function
        | EIm(_, _) ->
            failwith "compiler internal error: implicits not resolved"
        | ETypeVal t ->
            ETypeVal <| _check_prune pos t
        | a -> gen_trans_expr_impl self pos a

    let type_class_resolv
        ({ expr_impl = expr_impl_} as self)
        ______
        ({expr.pos = pos} as expr) =
          let expr =
              match expr.impl with
              | EIm(t, insts) -> {inst_resolve g insts t pos with pos = pos}
              | _ -> expr
          in gen_trans_expr self pos expr

    // resolve_type_classes
    let pass1 =
        { expr = type_class_resolv
        ; expr_impl = gen_trans_expr_impl
        ; decl = gen_trans_decl
        }
    let pass2 =
         { expr = post_infer_expr
         ; expr_impl = post_infer_expr_impl
         ; decl = post_infer_decl
         }
    let main ctx impl = pass2.expr_impl pass2 ctx <| pass1.expr_impl pass1 ctx impl
    let post_infer_decls pos decls =
        match
            ELet(decls, expr pos top_t <| IR.EVal(I64 0L))
            |> main pos
          with
        | ELet(decls, _) -> decls
        | _ -> failwith "impossible, otherwise F# booooom!"
    post_infer_decls

let module_gensym n = "mlfs_" ^ n

type path = string


let load_module :
    symbol
    -> (symbol * symbol) list
    -> Surf.decl list
    -> path
    -> global_st
    -> IR.decl list
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
    let untyped_expr impl = {typ=top_t; pos=pos;impl=impl}
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
        if Dict.contains fields user_sym
           || (Some ty = Map.tryFind user_sym predef)
        then ()
        else
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

    g.global_implicits_deltas.[namespace_class] <- namespace_inst_cnt + 1
    let results = mk_post_infer g pos (List.ofSeq results)
    Assign(module_gensym module_name, module_type, untyped_expr <| IR.EVal (I16 0s))::results

let raise_conflict_names : string seq -> 'a =
    fun reloadedModules ->
    failwithf
        "modile name conflicts: duplicate modules with the same names:\n %A"
        <| String.concat "\n" (Seq.map (fun a -> "- " ^ a) reloadedModules)

let load_sigs : path list -> symbol dset -> global_st -> unit =
    fun sig_files loaded_modules g ->
    for each in sig_files do
        let src_code = readFile each
        let { module_names = module_names
            ; implicits = implicits
            } = Json.deserialize<library_signature> src_code
        let reloadedModules = DSet.intersect loaded_modules module_names
        if not <| DSet.isEmpty reloadedModules then
            raise_conflict_names reloadedModules
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
    -> decl darray * library_signature
    = fun src_files loaded_modules g ->
    let global_implicits = g.global_implicits
    let global_implicits_deltas = g.global_implicits_deltas
    let decls = darray()
    let module_names = darray()
    let {prune = prune} = g.tcstate
    for path in src_files do
        let { Surf.name = name
            ; Surf.imports = imports
            ; Surf.decls = surf_decls
            } =
             Json.deserialize<Surf.module_record>
             <| readFile path
        if DSet.contains loaded_modules name
        then
            raise_conflict_names [name]
        else

        DSet.add loaded_modules name;
        DArray.push module_names name;
        DArray.extend decls
        <| load_module name imports surf_decls path g
    done;
    let prune_evi : evidence -> evidence =
        fun evi ->
            if evi.isPruned then
                evi.t <- prune evi.t
                evi.isPruned <- true
            evi
    let implicits =
        [| for KV(nom, n) in global_implicits_deltas ->
            ( nom
            , DArray.last_n (global_implicits.[nom]) n
              |> Seq.map prune_evi
              |> Array.ofSeq
            )
        |]
    decls,
    { module_names = List.ofSeq module_names
    ; implicits = implicits
    }

let smlfs_compile : path list -> path list -> symbol -> string -> unit =
    fun src_fles sig_files out_library_name out_directory ->
    let loadedModules = DSet.ofList []
    let g = empty_global_st()

    load_sigs sig_files loadedModules g
    let decls, signature = load_srcs src_fles loadedModules g
    show_hints g

    let declJSON = Json.serialize <| Array.ofSeq decls
    writeFile (joinPath out_directory <| sprintf "%s.mlfso" out_library_name) declJSON

    let sigJSON = Json.serialize signature
    writeFile (joinPath out_directory <| sprintf "%s.mlfsa" out_library_name) sigJSON
