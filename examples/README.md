# Programming Language U: A Higher Rank ML with type classes

**U = Unifiable**

## Features

- [x] [**higher rank polymorphisms**](https://en.wikipedia.org/wiki/Parametric_polymorphism#Rank-n_(%22higher-rank%22)_polymorphism)(via MLFS type system), see more at [concerns #1](#Higher-rank-polymorphisms)
- [x] turing-complete [type classes](https://en.wikipedia.org/wiki/Type_class). see more at see more at [concerns #2](#Implicits)
- [x] [scoped type variables](https://wiki.haskell.org/Scoped_type_variables).
- [x] [type holes and wildcard type annotations](https://wiki.haskell.org/GHC/Typed_holes). see more at [concerns #3](#Type-holes).
- [x] ocaml-like separate compilation
- [ ] [type-safe staging programming](http://okmij.org/ftp/ML/MetaOCaml.html). see more at [concerns #4](#Type-safe-staging).
- [ ] object-oriented constructs(virtual tables and virtual methods). see more at [concerns #5](#OOP).
- [ ] standard library.

## Concerns

### Higher Rank Polymorphisms

Higher rank polymorphisms in U is based on an experimental type system and I call it MLFS(Raising ML to the power of System F in a Simplest way) so far.

Technically, it's an approach to separate type assignments into 2 phases.
Every module(a separable unit for compilation) in MLFS will apply this 2 phases once.

First phase is a bottom-up type propagation. Except for typeclass-based modules and records(which is actually beyond MLFS), **no instantiation will happen**. A variable occurrence might know its original type from the type environment/scope,
but it keeps as it instead of instantiating.

The second phase is a top-down type propagation. The expected return type of each expression might be specified from the outer.
The expected types from the outer must be the final type, so the inner types will try to unify with the expected, and perform instantiations if necessary. However, if no expected type is from the outer, or the expected type from the outer is known to
be a free type variable, they unify, and the no instantiation would happen, otherwise the normal unification similar to HM unification happens.

TODO.

### Implicits

Type classes in U is actually encoded in an approach called **implicit arguments**.

This implementation is a direct implication of [demystifying type classes](http://okmij.org/ftp/Computation/typeclass.html).

In U, for example

```F#
type Eq a = { (==) : a -> a -> bool }

val eq_int = { Eq i64 }
let eq_int = { (==) = fun a -> fun b -> (extern "eq_i64") a b }

val (==) : forall a. {Eq a} -> a -> a -> bool
let (==) = fun implicit_eq_provider ->
    fun lhs -> fun rhs ->
        implicit_eq_provider.(==) lhs rhs
    // or
    // open implicit_eq_provider
    // in lhs == rhs

1 == 2
// => false
```

`eq_int` is an implicit instance, and when calling `==` on 2 64-bit integers, it'll know that it needs a instance typed `{Eq i64}` from the its visible context.

It found `eq_int`, and filled it as the the first argument `implicit_eq_provider`, so the we finally get an [ad-hoc polymorphic](https://en.wikipedia.org/wiki/Ad_hoc_polymorphism) operator `==`.

We can extend this operator for array types(if we have),
by using implicit instance constructions:

```F#
val eq_array : {forall a. {Eq a} -> Eq (array a)}
let eq_array =
    fun eq_elt ->
    { (==) = fun xs ys -> Array.forall2 eq_elt.(==) xs ys }
```

(Though we don't have standard libraries `Array.forall2` so
it's kind of difficult to construct above code so far.
You can use `extern` create `Array` and `forall2`, and type check above example, but implementing the standard library is
quite far from now. Alas, a big project but minimal human resources..)

`{forall ...}`, in form of `{t}`, this is an implicit instance.
`{{t1} -> t2}`, it means when building instance `t2`, the compiler is going to build `t1` first.

You can also write `{t1} -> {t2} -> t3}`, then implicitly building `t3` simultaneously requires implicitly building `t1` and `t2` .

Implemented but need more documentations yet.

TODO.

### Type Holes

Implemented but undocumented yet.
TODO.


### Type Safe Staging


Not implemented yet.

This feature is basically about something as powerful
as LISP macros, but actually more expressive as it is
totally scope-safe and type-safe and is a runtime thing.

For instance, in LISP, or Julia, you write an expression,
which contains an unbound symbol, this is unhygienic and can very often produce unexpected program behaviors(I mean, bugs).

However, it's possible to keep type information in a quoted expression, so that finally when you want to *eval* your code,
you must satisfy all requirements of running the code.

For instance, what I'm to implement:

```F#
val (+) : {Add a} -> a -> a -> a

let x = quote { a + 1 }
// x : {Add i64} -> {  symbol ("a",  i64) } -> code i64

let y = quote { b + 1 }
// x : {Add i64} -> { symbol ("b",  i64) } -> code i64

let f = fun x -> quote { fun a -> $x }

runcode (f x) // okay

runcode (f y) // error, instance symbol ("b",  i64) not found.
```

This way allows us to do type-safe meta-programming as powerful
as the unhygienic macros but still keep those appealing static assurances.

TODO.


### OOP

Partially implemented.


If the back-end is JavaScript:

```F#
type vtable a

val oop_get_field : forall a field member.{vtable a} -> field (a, field, member) = fun vtable ->
    let f =
        extern "
        function (field_type_str, vtable) {
            let offset = vtable.find_offset(field_type_str)
            function (subject) {
                subject[offset]
            }
        }
        "
    in f(field, vtable)
```

`field` is a scoped type variable which can be statically reflected so `field_type_str` is always a constant.

If some `{ vtable t }` is defined as a constant,
getting members from an object with type `t` is efficient and needs no hash-table look up in the runtime.


## Primitive Operations

Following primitives are expected to defined for each backend:

`op_Element: u64 -> forall a b. a -> b`.
