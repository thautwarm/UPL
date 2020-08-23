module HMUnification

open HM
open CamlCompat
open Exceptions

let substitute : (t, t) map -> t -> t
    = fun map ->
    let rec self : t -> t = fun a ->
        match Map.tryFind a map with
        | Some t -> t
        | None -> generic_transform self a
    in self
    
let fresh :  (un, t) map -> t -> t =
  fun map ->
    let rec self : t -> t =
      function
      | TBound a as t ->
          begin
          match Map.tryFind a map with
          | Some t -> t
          | None -> t
          end
      | t -> generic_transform self t
    in self

type tcstate =
  { unify : t -> t -> bool
  ; unifyInsts : t -> t -> bool
  ; unifyImplicits : t darray -> t -> t -> bool
  ; tenv : t darray
  ; prune : t -> t
  ; new_tvar : unit -> t
  }
  
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
                Dict.getForce bound_links un <|
                fun _ -> DSet.ofList [typeref]
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
            let freshmap =
              Map.ofList <|
                List.map (fun x -> TBound x, new_tvar()) ns
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
    let rec unify : t -> t -> bool =
      fun lhs rhs ->
      let lhs = prune lhs in
      let rhs = prune rhs in
      if lhs = rhs then true
      else
      match lhs, rhs with
      | TForall(ns1, p1), TForall(ns2, p2) ->
        let n1, n2 = List.length ns1, List.length ns2 in
        if n1 <> n2 then false
        else
        let subst1 =
          Map.ofList <|
          List.map (fun n -> n, new_tvar()) ns1
          in
        let generic = p2 in
        if not <| unify (fresh subst1 p1) generic then false
        else
        let generic = prune generic in
        let allValidBounds = Set.ofList <| List.map (fun x -> TBound x) ns2
        (* forall a. a -> 'v ~ forall b. b -> b
           we firstly solve: 'tvar -> 'v ~ b -> b
           then: 'tvar ~ b ~'v
           then:  as we know 'tvar is from a, 
           hence: **b** is mapped to **a** in lhs type,
           we get: lhs = forall a. a -> a
         *)
        let rec getLHSBounds rhsBoundsToLHS =
          function
          | [] -> Some rhsBoundsToLHS
          | (k, v) :: tail ->
            let v = prune v in
            if Set.contains v allValidBounds
               || Map.containsKey v rhsBoundsToLHS
            then None
            else
            let rhsBoundsToLHS =
              Map.add v (TBound k) rhsBoundsToLHS
            in getLHSBounds rhsBoundsToLHS tail
        in 
        (match getLHSBounds Map.empty
               <| Map.toList subst1
          with
        | None -> false
        | Some lhsBounds ->
        let backmap1 = ref [] in
        let backmap2 = ref [] in
        let _ = List.foreach ns2 <| fun un ->
            unlink un <| fun i ->
              begin
                backmap1 := (TVar i, TBound un) :: !backmap1
                backmap2 := (TVar i, Map.find (TBound un) lhsBounds) :: !backmap1
              end
          in
        let backmap1, backmap2 =
          Map.ofList !backmap1, Map.ofList !backmap2
          in
        let p1, p2 = prune p1, prune p2 in
        unify (substitute backmap2 p2) p2 &&
        unify (substitute backmap1 p1) p1)
      | TVar i, b
      | b, TVar i -> 
        if occur_in i b then
            raise <| RecursiveType "a ~ a -> b"
        else
            ignore(link i b)
            true
      | TArrow(a1, r1), TArrow(a2, r2) 
      | TApp(a1, r1), TApp(a2, r2) ->
        unify a1 a2 && unify r1 r2
      | TTup xs1, TTup xs2 ->
        List.forall2 unify xs1 xs2
      | _ -> false
    in
    let rec unifyImplicits (implicits : t darray)
      = fun lhs rhs ->
      let lhs = prune lhs in
      let rhs = prune rhs in
      if lhs = rhs then true
      else
      match lhs, rhs with
      | TForall(_, lhs), _ ->
        unifyImplicits implicits lhs rhs
      | TVar _, _ | _, TVar _ ->
        unifyInsts lhs rhs
      | _, TForall(_, _) ->
        unifyImplicits
          implicits lhs (instantiate rhs)
      | _, TArrow(TImplicit im, rhs) ->
        let _ = DArray.push implicits im in
        unifyImplicits implicits lhs rhs
      | _ -> unifyInsts lhs rhs
    in 
    { tenv = tenv
    ; unify = unify
    ; unifyInsts = unifyInsts
    ; unifyImplicits = unifyImplicits
    ; new_tvar = new_tvar
    ; prune = prune
    }




        
        

    
        
        

 