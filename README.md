# U

Programming Abstractions are Unifiable.

The website is https://thautwarm.github.io/UPL/latest, under construction.

## Try U

You need `dotnet-runtime` and Python(>=3.7).

```
bash install-all.sh
bash test.sh
```

Check out `test.sh` for how to invoke the command line compiler.

Check out `examples/` directory for some valid example programs.

Check out `julia-lib/*.mlfs` directory for Julia backend status.

## Some Examples

- functor(example about implicits & type classes)

```F#
module Functor

// a runtime specific primitive
// if you want to use `type` keyword, this primitive is
// required to be declared.
val op_Element : u64 -> forall a b. a -> b
let op_Element = extern ""


// structure type, close to record.
// it's in fact a static type powered tuple
type Functor F = (fmap : forall a b. (a -> b) -> F a -> F b)


val fmap : forall f. {Functor f} -> forall a b. (a -> b) -> f a -> f b
let fmap = fun functor -> fun map -> fun data -> functor.fmap map data


// dummy types
val List : Type @List
let List = extern ""

// list instance
val listFunctor : {Functor List}
let listFunctor = extern ""

// list value
val lst : List i64
let lst = extern ""

let mapped = fmap (fun x -> "1") lst

check mapped as mapped
// => mapped : |x|Functor.List. str
```


- local opening

```F#
module SomeModule
let x =
    open AnotherModule
    in var + 1
```

It compiles if `var` is defined in `AnotherModule`.

