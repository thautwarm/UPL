module TypeClass

open Exceptions
open Core
open CamlCompat
open Common
open HMUnification

let rec inst_resolve : global_st -> inst_resolv_ctx -> HM.t -> pos -> IR.expr
    = fun global_st local_implicits target pos ->
    let {prune=prune; unifyImplicits=unifyImplicits} =
        global_st.tcstate
    in
    let target = prune target in
    let candidates = darray() in
    let resolve_on_seq (xs : evidence_instance seq) =
        let _ =
            for inst in xs do
                // (if not <| inst.isPruned
                // then inst.t <- prune inst.t
                //      inst.isPruned <- true
                // );
                inst.t <- prune inst.t;
                // printfn "%O ?<= %O" target inst.t
                let is_less, evidences = less_than_under_evidences target inst.t

                if is_less
                then DArray.push candidates (inst, evidences)
            done
        in
        if DArray.isEmpty candidates
        then None
        else
        if DArray.len candidates = 1
        then
            let inst, evidences = candidates.[0]
            // this unification leverages type class to
            // enhance type inference.
            match unifyImplicits (darray ()) target inst.t with
            | false -> failwith "impossible as it already matched"
            | _ ->
            let explicits =
                [| for evi in evidences ->
                   inst_resolve global_st local_implicits evi pos
                |]
            in Some <| IR.apply_explicits inst.impl explicits target pos
        else
            raise <|
            InferError(
                pos,
                DuplicateInstanceError(
                    [|for (a, _) in candidates -> a|]
                ))

    match resolve_on_seq local_implicits with
    | Some inst -> inst
    | None ->
    let t_head = Core.get_type_head target in
    let global_implicits = Dict.getForce global_st.global_implicits t_head <| fun _ ->
        darray()
    in
    match resolve_on_seq global_implicits with
    | Some inst -> inst
    | None ->
        raise <| InferError(pos, InstanceNotFound target)
