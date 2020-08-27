# Core Language of U

U does actually not have a specific syntax frontend or runtime backend.
Language maintainers and designers spent too much time on this nonnutritive topic.
It turns out to be impossible to provide satisfactory syntax for all(or even most) users,
however people converge on the things under the hood:
efficiency, safety, documentations, packages for specific goals, etc.

However at here, for better understanding of the core language, a minimal syntax frontend is made.

The minimal syntax frontend is similar to the programming language OCaml or F#,
both of which belong to the ML language family. An ML is not only the simplest to
parse or analyze or understand(because it's the desugar version of your programs),
it also uses minimal syntax constructs to express full featured functionalities.

Expressing abstractions with an ML makes the idea "portable",
and avoids being language specific.

Besides, in this introduce, we specify the backend to JavaScript, just because it's popular
hence contributes to the readability.

## Preview

```F#
module main

let int_var = 1

val int_add : i64 -> i64 -> i64
let int_add = extern "x => y => x + y"

let add_integers = int_add int_var 2

val make_pair : forall a. a -> (a, a)
let make_pair = fun a -> (a, a)
```

`val` keyword is for declaring variables.

`extern` takes an expression from the backend, and you must give it a type to introduce it
into our static type checker.

`i64 -> i64 -> i64` is a type, a function type, which is no more different from
`i64 -> (i64 -> i64)`.

`a -> b` is a function type, which takes the argument of type `a` and returns a value of type `b`.

Multiple arguments might be more familiar with people from Java or Python:

```F#
val int_add': (i64, i64) -> i64
let int_add' = "([x, y]) => x + y)"
```

Still it is not an actual multi-ary function in the backend(JavaScript).

Function calls/applications in this syntax frontend does not need parentheses.

A type like `(a, b, ...)` is a tuple type, and values like `(a, b, ...)` are tuple values.

The concept of tuple is similar to the anonymous structure type in C. You can create a tuple to hold heterogenous data without some specific datatype predefined.

`fun arg -> return` is a function value, it doesn't need a name, and maps `arg` to `return`.

## Types


- function types : `a -> a`
- tuple types: `()`, `(a, b)`, `(a, b, c)`, ...
- *named types*(nominal types): `i32`, `i64`, `str`. Note that the types are not keywords, they're type bindings in current **scope**.
- polymorphic types: `forall a. a -> a`, we read it as "for any type `a`, map `a` to `a`".
- implicit types: `{ t }`. We don't talk about it at here.

Under construction.