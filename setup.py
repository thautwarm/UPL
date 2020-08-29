import glob
from setuptools import setup
README = """
'# U\n\nProgramming Abstractions are Unifiable.\n\nThe website is https://thautwarm.github.io/UPL/latest, under construction.\n\n## Try U\n\nYou need `dotnet-runtime` and Python(>=3.7).\n\n```\nbash install-all.sh\nbash test.sh\n```\n\nCheck out `test.sh` for how to invoke the command line compiler.\n\nCheck out `examples/` directory for some valid example programs.\n\nCheck out `julia-lib/*.mlfs` directory for Julia backend status.\n\n## Some Examples\n\n- functor(example about implicits & type classes)\n\n```F#\nmodule Functor\n\n// a runtime specific primitive\n// if you want to use `type` keyword, this primitive is\n// required to be declared.\nval op_Element : u64 -> forall a b. a -> b\nlet op_Element = extern ""\n\n\n// structure type, close to record.\n// it\'s in fact a static type powered tuple\ntype Functor F = (fmap : forall a b. (a -> b) -> F a -> F b)\n\n\nval fmap : forall f. {Functor f} -> forall a b. (a -> b) -> f a -> f b\nlet fmap = fun functor -> fun map -> fun data -> functor.fmap map data\n\n\n// dummy types\nval List : Type @List\nlet List = extern ""\n\n// list instance\nval listFunctor : {Functor List}\nlet listFunctor = extern ""\n\n// list value\nval lst : List i64\nlet lst = extern ""\n\nlet mapped = fmap (fun x -> "1") lst\n\ncheck mapped as mapped\n// => mapped : |x|Functor.List. str\n```\n\n\n- local opening\n\n```F#\nmodule SomeModule\nlet x =\n    open AnotherModule\n    in var + 1\n```\n\nIt compiles if `var` is defined in `AnotherModule`.\n\n'
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