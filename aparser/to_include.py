from io import StringIO
from itertools import islice
def parse_str(s):
    io = StringIO()
    esc = False
    for each in islice(s, 1, len(s)-1):
        if esc:
            io.write(each)
            esc = False
        elif each == '\\':
            esc = True
        else:
            io.write(each)

    return io.getvalue()


def str_concat(a, b):
    return a + b


def mk_tuple(T, xs):
    if len(xs) == 1:
        return xs[0]
    return T(xs)


def mk():
    TForall = "TForall"
    TApp = "TApp"
    TArrow = "TArrow"
    TTup = "TTup"
    TImplicit = "TImplicit"
    TVar = "TVar"
    TSym = "TSym"
    TNew = "TNew"
    TQuery = "TQuery"

    DAnn = "DAnn"
    DLoc = "DLoc"
    DBind = "DBind"
    DQuery = "DQuery"
    DOpen = "DOpen"

    ELoc = "ELoc"
    EVar = "EVar"
    EVal = "EVal"
    ELet = "ELet"
    EITE = "EITE"
    EFun = "EFun"
    EApp = "EApp"
    ETup = "ETup"
    EExt = "EExt"
    EQuery = "EQuery"
    EField = "EField"
    ECoerce = "ECoerce"
    I64 = "I64"
    F64 = "F64"
    U64 = "U64"
    Str = "Str"
    Bl = "Bl"
    out = {}

    for e in locals():

        def mk(e=e):
            def create(*args):
                if not args:
                    return {"value": e}
                if len(args) is 1:
                    return {e: args[0]}
                return {e: args}

            return create

        out[e] = mk()

    def Expr(pos, typ, impl):
        return {"pos": pos, "typ": typ, "impl": impl}

    def Pos(token):
        return {
            "line": token.lineno + 1,
            "col": token.colno,
            "filename": token.filename,
        }

    def ModuleRecord(name, imports, decls):
        return dict(name=name, imports=imports, decls=decls)

    out["ModuleRecord"] = ModuleRecord
    out["Expr"] = Expr
    out["Pos"] = Pos
    return out


globals().update(mk())


def tforall(a, b):
    if a:
        return TForall(a, b)
    return b


def mk_type_def(type_head, definition):
    mode, definition = definition
    if mode != 0:
        raise NotImplementedError("can only define record now")
    pos, type_name, bounds = type_head
    generated_ast = [
        DLoc(pos),
        DAnn(type_name, TApp(TVar("Type"), TNew(type_name))),
        DBind(type_name, EVar(type_name)),
    ]
    record_type = TVar(type_name)

    for each in bounds:
        record_type = TApp(record_type, TVar(each))

    field_class = TVar("field")
    structure_parameter_type = TTup([f_type for (_, _, f_type) in definition])
    nominal_parameter_type = record_type

    n_fields = len(definition)
    function = ETup([EVar(f_name) for (_, f_name, _) in definition])
    eltypes = []
    for i, (f_pos, f_name, f_type) in enumerate(reversed(definition)):
        i = n_fields - i - 1
        structure_parameter_type = TArrow(f_type, structure_parameter_type)
        nominal_parameter_type = TArrow(f_type, nominal_parameter_type)
        eltypes.append(f_type)
        function = EFun(f_name, function)

        generated_ast.append(DLoc(f_pos))
        field_instance = TImplicit(
            tforall(
                bounds, TApp(field_class, TTup([record_type, TSym(f_name), f_type]))
            )
        )

        eltypes.reverse()

        getter_name = "__get_" + f_name

        generated_ast.append(DAnn(getter_name, field_instance))

        generated_ast.append(
            DBind(getter_name, ECoerce(EApp(EVar("op_Element"), EVal(U64(i)))))
        )

    nominal_parameter_type = tforall(bounds, nominal_parameter_type)
    structure_parameter_type = tforall(bounds, structure_parameter_type)
    make_name = "__make_" + type_name
    make_instance = TImplicit(
        TApp(TVar("Make"), TTup([TVar(type_name), nominal_parameter_type]))
    )

    generated_ast.extend(
        [
            DAnn(make_name, make_instance),
            DBind(
                make_name,
                ECoerce(
                    ELet(
                        [
                            DAnn(make_name, structure_parameter_type),
                            DBind(make_name, function),
                        ],
                        EVar(make_name),
                    )
                ),
            ),
        ]
    )
    return generated_ast
