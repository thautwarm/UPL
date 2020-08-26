// TODO: SSA transformation and then do more optimizations
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

    let collect_defs_decl ({expr = expr_} as self) (ctx: (symbol, _) dict, pos) =
        function
        | Assign(sym, _, value) as a ->
            if Dict.contains defs sym
            then failwithf "redefinition of %s" sym
            else
            ctx.[sym] <- ref (false, pos, expr_ self (ctx, pos) value);
            a
        | Perform ex as a ->
            ignore(expr_ self (ctx, pos) ex);
            a
    let collect_defs_expr_impl ({expr = expr_} as self) (ctx: (symbol, _) dict, pos) x =
        match x with
        | EFun(s, t, _) as a ->
            if Dict.contains defs s
            then failwithf "redefinition of %s" s
            else
            ctx.[s] <- ref (false, pos, {pos = pos; typ = t; impl = EVar s})
            gen_trans_expr_impl self (ctx, pos) a
        | a -> gen_trans_expr_impl self (ctx, pos) a

    // simple alias analysis
    let rec ssa_prune recurs pos sym  =
        if Set.contains sym recurs
        then
            let (_, pos, _) = !defs.[sym]
            raise <| InferError(pos, RecursiveDefinition sym)
        else
        let recurs = Set.add sym recurs in
        match Dict.tryFind defs sym with
        | None ->
            raise <| InferError(pos, UndefinedSymbolInIR sym)
        | Some {contents = false, pos, {expr.impl = EVar sym'}} when sym' <> sym ->
                match ssa_prune recurs pos sym' with
                | (_, _, {expr.impl = EVar _}) as res ->
                    defs.[sym] := res; res
                | (_, _, expr) ->
                    let res = true, pos, {expr with impl = EVar sym'}
                    defs.[sym] := res; res
        | Some {contents = false, a, b} ->
            let res = (true, a, b)
            defs.[sym] := res; res
        | Some {contents = (true, _, _) as res} -> res

    let self =
            { decl = collect_defs_decl
            ; expr_impl = collect_defs_expr_impl
            ; expr = gen_trans_expr
            }
    let input' = self.expr self (defs, input.pos) input

    for each in defs.Keys do
        let (isPruned, pos, _) = !defs.[each]
        if not isPruned then
            ignore(ssa_prune Set.empty pos each)
    done

    let defs = Dict.ofList <| [
        for KV(symbol, {contents = _, _, ex}) in defs ->
            symbol, ex
    ]

    let cnt = ref 0
    let gensym s =
         let i = !cnt
         incr cnt;
         sprintf "%s(%d)" s i

    let hygiene_decl (self: _ transformer) (ctx: (_ , _) dict) =
        function
        | Assign(x, t, expr) ->
            let x' = gensym x
            ctx.[x] <- x';
            Assign(x', t, self.expr self ctx expr)
        | Perform e -> Perform <| self.expr self ctx e
    let hygiene_expr_impl (self: _ transformer) (ctx: (_ , _) dict) =
        function
        | EFun(x, t, expr) ->
            let x' = gensym x
            ctx.[x] <- x';
            EFun(x', t, self.expr self ctx expr)
        | EVar s as e ->
            match Dict.tryFind ctx s with
            | Some s -> EVar s
            | _ -> e
        | a -> gen_trans_expr_impl self ctx a

    let hygienize =
        let self = { transformer with decl = hygiene_decl; expr_impl = hygiene_expr_impl }
        in self.expr self (dict())

    let inline_calls ({expr=expr_} as self) recurs =
          function
          | {expr.impl=EApp(f, arg)} as expr ->
            let f = expr_ self recurs f
            let arg = expr_ self recurs arg
            match f with
            | {impl=EVar(sym)} when not <| Set.contains sym recurs ->
                let recurs = Set.add sym recurs in
                match hygienize <| defs.[sym] with
                | {impl=EThunk(a)} as func ->
                   { func
                        with impl = ELet ([Perform(arg)] , self.expr self recurs a)
                    }
                | {impl=EFun(x, t, a)} as func ->
                    defs.[x] <- arg;
                    { func
                        with impl =
                               ELet ([Assign(x, t, arg)] , self.expr self recurs a)
                    }
                | f -> {expr with impl = EApp(f, arg)}
            | _ -> {expr with impl = EApp(f, arg)}
          | a -> gen_trans_expr self recurs a

    let self =
           { decl = gen_trans_decl
           ; expr = inline_calls
           ; expr_impl = gen_trans_expr_impl
           }
    in self.expr self Set.empty input'
