module HMUnification

open HM
open CamlCompat
open Exceptions

type TCState
    = { tenv :  t darray
      ; unify : t -> t -> bool }

let substitute : (t, t) map -> t -> t
    = fun map ->
    let rec self : t -> t = fun a ->
        match Map.tryFind a map with
        | Some t -> t
        | None -> generic_transform self a
    in self
    
let mk_tcstate (tenv: t darray)
    =
    let bound_links : (un, int dset) dict = dict() in

    let unlink (b: un) f =
        match Dict.pop bound_links b with
        | None -> ()
        | Some rels ->
        let _ = DSet.foreach rels <| fun rel ->
            DArray.set tenv rel <| TVar rel
            f(rel)
        in
        DSet.clear rels
      in
    let new_tvar() =
        let typeref = DArray.len tenv in
        let tvar = TVar typeref in
        let _ = DArray.push tenv tvar in
        tvar
      in
    let occur_in : int -> t -> bool =
        fun i ty ->
        match ty with
        | TVar i' when i' = i -> false
        | _ -> 
            let rec check = function
                | TVar i' when i' = i -> false
                | x -> generic_check check x
            in not (check ty)
      in
    let link : int -> t -> t =
        fun typeref t ->
        
        match
            match t with
            | TBound un ->  Some un
            | _ -> None
            with
        | None ->
           let _ = DArray.set tenv typeref t
           in t
        | Some un ->
            let links =
                Dict.get_force bound_links un <|
                fun _ -> DSet.from_list [typeref]
            in
            let _ = if not <| DSet.contains links typeref
                    then DSet.add links typeref
            in t

      in
    let rec prune : t -> t =
        function
        | TVar i as a ->
            begin
                match DArray.ith tenv i with
                | TVar i' when i' = i -> a
                | t -> link i <| prune t
            end
        | a -> generic_transform prune a
      in
    
    // check if any unsolved type variables
    let prune_with_var_check : (int -> 'a) -> t -> t =
        fun f a ->
        match match a with
              | TVar i ->
                begin
                    match DArray.ith tenv i with
                    | TVar i' when i' = i -> a
                    | t -> link i <| prune t
                end
              | _ -> generic_transform prune a
            with
        | TVar v as a -> ignore(f v); a
        | a -> a
      in
    let instantiate =
        function
        | TForall(ns, t) ->
            let freshmap = Map.ofList <| List.map (fun x -> Bound x) ns
            in substitute freshmap t
        | a -> a
    let rec unifyInsts : t -> t -> bool
        = fun lhs rhs ->
          let lhs = prune lhs in
          let rhs = prune rhs in
          if lhs = rhs then true
          else
          match lhs, rhs with
          | TForall(_, lhs), _ ->
            unifyInsts lhs rhs
          | TVar i, b
          | b, TVar i -> 
            if occur_in i b then
                raise <| RecursiveType "a ~ a -> b"
            else
                ignore(link i b)
                true
          | _, TForall(_, _) ->
            unifyInsts lhs <| instantiate rhs
          
          | TImplicit lhs, rhs
          | lhs, TImplicit rhs ->
            unifyInsts lhs rhs
          | TArrow(a1, r1), TArrow(a2, r2) ->
            unifyInsts a2 a1 &&
            unifyInsts r1 r2
          | TApp(f1, a1), TApp(f2, a2) ->
            unifyInsts f1 f2 &&
            unifyInsts a1 a2
          | TTup xs1, TTup xs2 ->
            List.forall2 unifyInsts xs1 xs2
          | _ -> false
      in
    None
    // let unify : t -> t -> bool



        
        

    
        
        

 