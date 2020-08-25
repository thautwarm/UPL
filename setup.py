import glob
from setuptools import setup
README = """
'# Programming Language U: A Higher Rank ML with type classes\n\n**U = Unifiable**\n\n## Features\n\n- [x] [**higher rank polymorphisms**](https://en.wikipedia.org/wiki/Parametric_polymorphism#Rank-n_(%22higher-rank%22)_polymorphism)(via MLFS type system), see more at [concerns #1](#Higher-rank-polymorphisms)\n- [x] turing-complete [type classes](https://en.wikipedia.org/wiki/Type_class). see more at see more at [concerns #2](#Implicits)\n- [x] [scoped type variables](https://wiki.haskell.org/Scoped_type_variables).\n- [x] [type holes and wildcard type annotations](https://wiki.haskell.org/GHC/Typed_holes). see more at [concerns #3](#Type-holes).\n- [x] ocaml-like separate compilation\n- [ ] [type-safe staging programming](http://okmij.org/ftp/ML/MetaOCaml.html). see more at [concerns #4](#Type-safe-staging).\n- [ ] object-oriented constructs(virtual tables and virtual methods). see more at [concerns #5](#OOP).\n- [ ] standard library.\n\n## Concerns\n\n### Higher Rank Polymorphisms\n\nHigher rank polymorphisms in U is based on an experimental type system and I call it MLFS(Raising ML to the power of System F in a Simplest way) so far.\n\nTechnically, it\'s an approach to separate type assignments into 2 phases.\nEvery module(a separable unit for compilation) in MLFS will apply this 2 phases once.\n\nFirst phase is a bottom-up type propagation. Except for typeclass-based modules and records(which is actually beyond MLFS), **no instantiation will happen**. A variable occurrence might know its original type from the type environment/scope,\nbut it keeps as it instead of instantiating.\n\nThe second phase is a top-down type propagation. The expected return type of each expression might be specified from the outer.\nThe expected types from the outer must be the final type, so the inner types will try to unify with the expected, and perform instantiations if necessary. However, if no expected type is from the outer, or the expected type from the outer is known to\nbe a free type variable, they unify, and the no instantiation would happen, otherwise the normal unification similar to HM unification happens.\n\nTODO.\n\n### Implicits\n\nType classes in U is actually encoded in an approach called **implicit arguments**.\n\nThis implementation is a direct implication of [demystifying type classes](http://okmij.org/ftp/Computation/typeclass.html).\n\nIn U, for example\n\n```F#\ntype Eq a = { (==) : a -> a -> bool }\n\nval eq_int = { Eq i64 }\nlet eq_int = { (==) = fun a -> fun b -> (extern "eq_i64") a b }\n\nval (==) : forall a. {Eq a} -> a -> a -> bool\nlet (==) = fun implicit_eq_provider ->\n    fun lhs -> fun rhs ->\n        implicit_eq_provider.(==) lhs rhs\n    // or\n    // open implicit_eq_provider\n    // in lhs == rhs\n\n1 == 2\n// => false\n```\n\n`eq_int` is an implicit instance, and when calling `==` on 2 64-bit integers, it\'ll know that it needs a instance typed `{Eq i64}` from the its visible context.\n\nIt found `eq_int`, and filled it as the the first argument `implicit_eq_provider`, so the we finally get an [ad-hoc polymorphic](https://en.wikipedia.org/wiki/Ad_hoc_polymorphism) operator `==`.\n\nWe can extend this operator for array types(if we have),\nby using implicit instance constructions:\n\n```F#\nval eq_array : {forall a. {Eq a} -> Eq (array a)}\nlet eq_array =\n    fun eq_elt ->\n    { (==) = fun xs ys -> Array.forall2 eq_elt.(==) xs ys }\n```\n\n(Though we don\'t have standard libraries `Array.forall2` so\nit\'s kind of difficult to construct above code so far.\nYou can use `extern` create `Array` and `forall2`, and type check above example, but implementing the standard library is\nquite far from now. Alas, a big project but minimal human resources..)\n\n`{forall ...}`, in form of `{t}`, this is an implicit instance.\n`{{t1} -> t2}`, it means when building instance `t2`, the compiler is going to build `t1` first.\n\nYou can also write `{{t1} -> {t2} -> t3}`, then implicitly building `t3` simultaneously requires implicitly building `t1` and `t2` .\n\nImplemented but need more documentations yet.\n\nTODO.\n\n### Type Holes\n\nImplemented but undocumented yet.\nTODO.\n\n\n### Type Safe Staging\n\n\nNot implemented yet.\n\nThis feature is basically about something as powerful\nas LISP macros, but actually more expressive as it is\ntotally scope-safe and type-safe and is a runtime thing.\n\nFor instance, in LISP, or Julia, you write an expression,\nwhich contains an unbound symbol, this is unhygienic and can very often produce unexpected program behaviors(I mean, bugs).\n\nHowever, it\'s possible to keep type information in a quoted expression, so that finally when you want to *eval* your code,\nyou must satisfy all requirements of running the code.\n\nFor instance, what I\'m to implement:\n\n```F#\nval (+) : {Add a} -> a -> a -> a\n\nlet x = quote { a + 1 }\n// x : {Add i64} -> {  symbol ("a",  i64) } -> code i64\n\nlet y = quote { b + 1 }\n// x : {Add i64} -> { symbol ("b",  i64) } -> code i64\n\nlet f = fun x -> quote { fun a -> $x }\n\nruncode (f x) // okay\nruncode (f y) // error, instance symbol ("b",  i64) not found.\n```\n\nThis way allows us to do type-safe meta-programming as powerful\nas the unhygienic macros but still keep those appealing static assurances.\n\nTODO.\n\n\n### OOP\n\nPartially implemented.\n\n\nIf the back-end is JavaScript:\n\n```F#\ntype vtable a\n\nval oop_get_field : forall a field member.{vtable a} -> field (a, field, member) = fun vtable ->\n    let f =\n        extern "\n        function (field_type_str, vtable) {\n            let offset = vtable.find_offset(field_type_str)\n            function (subject) {\n                subject[offset]\n            }\n        }\n        "\n    in f(field, vtable)\n```\n\n`field` is a scoped type variable which can be statically reflected so `field_type_str` is always a constant.\n\nIf some `{ vtable t }` is defined as a constant,\ngetting members from an object with type `t` is efficient and needs no hash-table look up in the runtime.\n\n\n## Primitive Operations\n\nFollowing primitives are expected to defined for each backend:\n\n`op_Element: u64 -> forall a b. a -> b`.\n'
"""

setup(
    name='dexe_smlfs',

    # e.g., 0.1.1, 0.2
    # see more at https://www.python.org/dev/peps/pep-0440/
    version='',

    # keywords of your project that separated by comma ","
    keywords='',

    # a concise introduction of your project
    description='',

    # a very long introduction of your project.
    # usually, read it from a README file:
    long_description=README,

    long_description_content_type="text/markdown",

    # distribution under which license
    # https://opensource.org/licenses
    license='mit',

    python_requires='>=3.5.0',

    # A URL to your project's repository/website
    # e.g., https://github.com/thautwarm/MLFS
    url='',

    author='',
    author_email='',
    packages=['dexe_smlfs'],
    install_requires=[],
    package_data={
        'dexe_smlfs':
            [each[len('dexe_smlfs/'):]
             for each in glob.glob("dexe_smlfs/publish/**", recursive=True)]
    },
    entry_points={"console_scripts": [
        "smlfs=dexe_smlfs.main:exe"
    ]},

    # your executable supported platforms
    platforms="any",

    # usually you do not need to change it
    # if interests see:
    # https://pypi.org/pypi?%3Aaction=list_classifiers
    classifiers=[
        "Programming Language :: Python :: 3.5",
        "Programming Language :: Python :: 3.6",
        "Programming Language :: Python :: 3.7",
        "Programming Language :: Python :: 3.8",
        "Programming Language :: Python :: 3.9",
        "Programming Language :: Python :: Implementation :: CPython",
    ],
    zip_safe=False,
)