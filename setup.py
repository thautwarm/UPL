import glob
from setuptools import setup
README = """
'# Programming Language U: A Higher Rank ML with type classes\n\n**U = Unifiable**\n\n## Features\n\n- [x] [**higher rank polymorphisms**](https://en.wikipedia.org/wiki/Parametric_polymorphism#Rank-n_(%22higher-rank%22)_polymorphism)(via MLFS type system), see more at [concerns #1](#Higher-rank-polymorphisms)\n- [x] turing-complete [type classes](https://en.wikipedia.org/wiki/Type_class). see more at see more at [concerns #2](#Implicits)\n- [x] [scoped type variables](https://wiki.haskell.org/Scoped_type_variables).\n- [x] [type holes and wildcard type annotations](https://wiki.haskell.org/GHC/Typed_holes). see more at [concerns #3](#Type-holes).\n- [ ] [type-safe staging programming](http://okmij.org/ftp/ML/MetaOCaml.html). see more at [concerns #4](#Type-safe-staging).\n- [ ] object-oriented constructs(virtual tables and virtual methods). see more at [concerns #5](#OOP).\n\n## Concerns\n\n### Higher Rank Polymorphisms\n\nHigher rank polymorphisms in U is based on an experimental type system and I call it MLFS(Raising ML to the power of System F in a Simplest way) so far.\n\nTechnically, it\'s an approach to separate type assignments into 2 phases.\nEvery module(a separable unit for compilation) in MLFS will apply this 2 phases once.\n\nFirst phase is a bottom-up type propagation. Except for typeclass-based modules and records(which is actually beyond MLFS), **no instantiation will happen**. A variable occurrence might know its original type from the type environment/scope,\nbut it keeps as it instead of instantiating.\n\nThe second phase is a top-down type propagation. The expected return type of each expression might be specified from the outer.\nThe expected types from the outer must be the final type, so the inner types will try to unify with the expected, and perform instantiations if necessary. However, if no expected type is from the outer, or the expected type from the outer is known to\nbe a free type variable, they unify, and the no instantiation would happen —— I feel like to call this  as "eagerly lazy instantiation", because actually, there can be cases that even if the expected return type is a free type variable, it does not \n\nIf the expected type is `v -> (forall a.a) -> (t -> t)`,\nand the expression(simply like a variable) has the type\n`forall b.b->b->b`, instantiating the latter to the former has an impediment in MLFS:\n\n\n\n\n\n\n### Implicits\n\nType classes in U is actually encoded in an approach called **implicit arguments**.\n\nThis implementation is a direct implication of [demystifying type classes](http://okmij.org/ftp/Computation/typeclass.html), and is well implemented in [Agda](https://agda.readthedocs.io/en/v2.6.1/language/implicit-arguments.html).\n\n\n [Scala](https://docs.scala-lang.org/tour/implicit-parameters.html)\n\n\n'
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