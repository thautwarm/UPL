// compat ocaml and F#
module CamlCompat

let readFile = System.IO.File.ReadAllText
let writeFile f contents = System.IO.File.WriteAllText(f, contents)
let joinPath a b = System.IO.Path.JoinPath(a, b)

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

    let extend (this: 'a darray) a =
        this.AddRange a

    let ith (this: 'a darray) i =
        this.[i]

    let set (this: 'a darray) i v =
        this.[i] <- v

    let len (this: 'a darray) =
        this.Count

    let isEmpty (this: 'a darray) = this.Count = 0

    let last_n (this: _ darray) n =
        if n < 0 then
            darray()
        else
        let len = len this in
        if n >= len then darray(this)
        else this.GetRange(len-n, n)

type ('k, 'v) dict = System.Collections.Generic.Dictionary<'k, 'v>

let (|KV|) (x : System.Collections.Generic.KeyValuePair<'k, 'v>) =
    KV(x.Key, x.Value)

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
    let contains : ('k, 'v) dict -> 'k -> bool =
        fun this k -> this.ContainsKey k

    let tryFind : ('k, 'v) dict -> 'k -> 'v option =
        fun this k ->
            let v = ref (Unchecked.defaultof<'v>) in
            if this.TryGetValue(k, v) then
                Some !v
            else None

    let isEmpty : ('k, _) dict -> bool = fun this ->
        this.Count = 0

    let size : (_, _) dict -> int =
        fun this -> this.Count


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

    let isEmpty (xs : 't dset) =
        xs.Count = 0

    let toList (xs: 't dset) =
        List.ofSeq xs

    let intersect (xs: 't dset) ys =
        let r = dset(xs)
        r.IntersectWith ys;
        r

    let update (xs: 't dset) ys =
        xs.UnionWith ys


type ('k, 'v) map when 'k : comparison = Map<'k, 'v>

module Map =
    let mem = Map.containsKey
    let let_seq m1 m2 =
        Map.foldBack Map.add (Map.ofSeq m2) m1

let (^) : string -> string -> string = (+)
// ocaml:
// let (<|) f x = f x
// let (|>) x f = f x
// module List = struct
//      include List
//      let fold
// end
