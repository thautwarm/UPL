module HMUnification

open HM
open CamlCompat

type TCState
    = { tenv :  t darray
      ; unify : t -> t -> bool }

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
    None
        
        

    
        
        

 