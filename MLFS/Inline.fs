// IR used by MLFS is SSA so
// we can simply reduce trivial but extensive function calls,
// such as instance resolution.
// Combining this with our instance resolution,
// we got mornomorphization for type classes
module Inline
open IR
open CamlCompat
open Common
open Exceptions
open System.Linq

let perform_inline input =
    let defs = dict()
    let propagate_pos self (ctx, _) ({expr.pos=pos} as ex) =
          {ex with impl = self.expr_impl self (ctx, pos) ex.impl}

    let collect_defs ({expr = expr_} as self) (ctx: (symbol, _) dict, pos) =
            function
            | Assign(sym, _, value) as a ->
                ctx.[sym] <- (false, pos, expr_ self (ctx, pos) value);
                a
            | Perform ex as a ->
                ignore(expr_ self (ctx, pos) ex);
                a

    // simple alias analysis
    let rec ssa_prune recurs pos sym  =
        if Set.contains sym recurs
        then
            let (_, pos, _) = defs.[sym]
            raise <| InferError(pos, RecursiveDefinition sym)
        else
        let recurs = Set.add sym recurs in
        match Dict.tryFind defs sym with
        | None ->
            raise <| InferError(pos, UndefinedSymbolInIR sym)
        | Some (false, pos, {expr.impl = EVar sym'}) when sym' <> sym ->
                match ssa_prune recurs pos sym' with
                | (_, _, {expr.impl = EVar _}) as res ->
                    defs.[sym] <- res; res
                | (_, _, expr) ->
                    let res = true, pos, {expr with impl = EVar sym'}
                    defs.[sym] <- res; res
        | Some(false, a, b) ->
            let res = (true, a, b)
            defs.[sym] <- res; res
        | Some ((true, _, _) as res) -> res

    let self =
            { decl = collect_defs
            ; expr_impl = gen_trans_expr_impl
            ; expr = gen_trans_expr
            }
    let input' = self.expr self (defs, input.pos) input

    for each in defs.Keys do
        let (isPruned, pos, _) = defs.[each]
        if not isPruned then
            ignore(ssa_prune Set.empty pos each)
    done

    let defs = Map.ofArray [|
        for KV(symbol, (_, _, ex)) in defs ->
            symbol, ex
    |]

    let cnt = ref 0
    let gensym s =
         let i = !cnt
         incr cnt;
         sprintf "%s(%d)" s i

    let inline_calls ({expr=expr_} as self) ((recurs, scope) as ctx) =
          function
          | {expr.impl=EApp(f, arg)} as expr ->
            let f = expr_ self ctx f
            let arg = expr_ self ctx arg
            match f with
            | {impl=EVar(sym)} when not <| Set.contains sym recurs ->
                let recurs = Set.add sym recurs in
                match Map.find sym defs with
                | {impl=EFun(x, t, a)} as func ->
                    let x' = gensym x
                    let scope = Map.add x {arg with impl = EVar x'} scope
                    { func with
                            impl =
                             ELet
                              ( [Assign(x', t, arg)]
                              , self.expr self (recurs, scope) a
                              )
                    }
                | f -> {expr with impl = EApp(f, arg)}
            | _ -> {expr with impl = EApp(f, arg)}
          | {expr.impl = EVar(sym)} when Map.containsKey sym scope ->
            scope.[sym]
          | a -> gen_trans_expr self ctx a

    let self =
           { decl = gen_trans_decl
           ; expr = inline_calls
           ; expr_impl = gen_trans_expr_impl
           }
    in self.expr self (Set.empty, Map.empty) input'
