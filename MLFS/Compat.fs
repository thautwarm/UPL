// compat ocaml and F#
module CamlCompat

let time_ns ()
    = System.DateTime.UtcNow.Ticks

let intern : string -> string
    = System.String.Intern

type 'a reduce_result =
    | Terminate
    | Continue of 'a

type ('a, 'b) Either =
    | Either of 'a
    | Otherwise of 'b


type 'a darray = 'a System.Collections.Generic.List
module DArray =
    let push (this: 'a darray) a =
        this.Add a

    let ith (this: 'a darray) i =
        this.[i]

    let set (this: 'a darray) i v =
        this.[i] <- v

    let len (this: 'a darray) =
        this.Count


type ('k, 'v) dict = System.Collections.Generic.Dictionary<'k, 'v>

let constant x = fun _ -> x
module Dict =
    let pop (this: ('k, 'v) dict) (k: 'k) =
        if this.ContainsKey k then
            ignore(this.Remove k)
            Some <| this.[k]
        else
            None
    let ofList (xs: ('k * 'v) list) =
        let mkkv (a, b) = System.Collections.Generic.KeyValuePair(a, b) in
        dict([|for kv in xs -> mkkv kv|])

    let getForce (this: ('k, 'v) dict) k default' =
        if this.ContainsKey k then
            this.[k]
        else
            let d = default' k
            this.[k] <- d
            d

module List =
    let foreach xs f = List.iter f xs
        
type 't dset = System.Collections.Generic.HashSet<'t>

module DSet =
    let clear (this: 't dset) =
        this.Clear()
    
    let foreach (xs: 't dset) f = 
        for x in xs do
            f x
        done
    let ofList (xs: 't list) =
        dset(xs)

    let contains (xs: 't dset) x =
        xs.Contains x
    
    let add (xs: 't dset) x: unit =
        ignore(xs.Add x)

type ('k, 'v) map when 'k : comparison = Map<'k, 'v>

module Map =
    let mem = Map.containsKey
   
let (^) : string -> string -> string = (+)
// ocaml:
// let (<|) f x = f x
// let (|>) x f = f x
// module List = struct
//      include List
//      let fold
// end

