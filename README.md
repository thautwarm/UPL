# Programming Language U: A Higher Rank ML with type classes

**U = Unifiable**

## Features

- [x] [**higher rank polymorphisms**](https://en.wikipedia.org/wiki/Parametric_polymorphism#Rank-n_(%22higher-rank%22)_polymorphism)(via MLFS type system), see more at [concerns #1](#Higher-rank-polymorphisms)
- [x] turing-complete [type classes](https://en.wikipedia.org/wiki/Type_class). see more at see more at [concerns #2](#Implicits)
- [x] [scoped type variables](https://wiki.haskell.org/Scoped_type_variables).
- [x] [type holes and wildcard type annotations](https://wiki.haskell.org/GHC/Typed_holes). see more at [concerns #3](#Type-holes).
- [ ] [type-safe staging programming](http://okmij.org/ftp/ML/MetaOCaml.html). see more at [concerns #4](#Type-safe-staging).
- [ ] object-oriented constructs(virtual tables and virtual methods). see more at [concerns #5](#OOP).

## Concerns

### Higher Rank Polymorphisms

Higher rank polymorphisms in U is based on an experimental type system and I call it MLFS(Raising ML to the power of System F in a Simplest way) so far.

Technically, it's an approach to separate type assignments into 2 phases.
Every module(a separable unit for compilation) in MLFS will apply this 2 phases once.

First phase is a bottom-up type propagation. Except for typeclass-based modules and records(which is actually beyond MLFS), **no instantiation will happen**. A variable occurrence might know its original type from the type environment/scope,
but it keeps as it instead of instantiating.

The second phase is a top-down type propagation. The expected return type of each expression might be specified from the outer.
The expected types from the outer must be the final type, so the inner types will try to unify with the expected, and perform instantiations if necessary. However, if no expected type is from the outer, or the expected type from the outer is known to
be a free type variable, they unify, and the no instantiation would happen —— I feel like to call this  as "eagerly lazy instantiation", because actually, there can be cases that even if the expected return type is a free type variable, it does not 

If the expected type is `v -> (forall a.a) -> (t -> t)`,
and the expression(simply like a variable) has the type
`forall b.b->b->b`, instantiating the latter to the former has an impediment in MLFS:






### Implicits

Type classes in U is actually encoded in an approach called **implicit arguments**.

This implementation is a direct implication of [demystifying type classes](http://okmij.org/ftp/Computation/typeclass.html), and is well implemented in [Agda](https://agda.readthedocs.io/en/v2.6.1/language/implicit-arguments.html).


 [Scala](https://docs.scala-lang.org/tour/implicit-parameters.html)


