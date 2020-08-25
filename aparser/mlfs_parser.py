
"""
Copyright thautwarm (c) 2019

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of thautwarm nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
"""

from json.decoder import py_scanstring
def parse_str(s):
    return py_scanstring(s, 1)[0]

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
          return {'value': e}
        if len(args) is 1:
          return {e: args[0]}
        return {e: args}
      return create
    out[e] = mk()

  def Expr(pos, typ, impl):
    return {'pos':pos,  'typ': typ, 'impl': impl}
  def Pos(token):
    return {'line': token.lineno + 1, 'col': token.colno, 'filename': token.filename}
  def ModuleRecord(name, imports, decls):
    return dict(name=name, imports=imports, decls=decls)
  out['ModuleRecord'] = ModuleRecord
  out['Expr'] = Expr
  out['Pos'] = Pos
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
      DBind(type_name, EVar(type_name))
    ]
    record_type = TVar(type_name)

    for each in bounds:
      record_type = TApp(record_type, TBound(each))


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
          bounds,
          TApp(
            field_class,
            TTup([record_type, TSym(f_name), f_type]))))

      eltypes.reverse()

      getter_name = "__get_" + f_name

      generated_ast.append(
        DAnn(getter_name, field_instance))

      generated_ast.append(
        DBind(
          getter_name,
          ECoerce(
            EApp(
              EVar("op_Element"),
              EVal(U64(i))))))

    nominal_parameter_type = tforall(bounds, nominal_parameter_type)
    structure_parameter_type = tforall(bounds, structure_parameter_type)
    make_name = "__make_" + type_name
    make_instance = TImplicit(
        TApp(
          TVar("Make"),
          TTup(
            [
              TVar(type_name),
              nominal_parameter_type
            ])))

    generated_ast.extend([
      DAnn(make_name, make_instance),
      DBind(
        make_name,
        ECoerce(
          ELet(
            [
              DAnn(make_name, structure_parameter_type),
              DBind(make_name, function),
            ],
            EVar(make_name))))
    ])
    return generated_ast

def concat(xs):
  if len(xs) == 1:
    return xs[0]
  res = []
  for x in xs:
    res.extend(x)
  return res

def importName(x):
  return x.split('.')[-1]

def inc(x):
  return x + 1
from typing import Generic, TypeVar
T = TypeVar('T')

class Tokens():
    __slots__ = ['array', 'offset']

    def __init__(self, array):
        self.array = array
        self.offset = 0

class State():

    def __init__(self):
        pass

class AST(Generic[T]):
    __slots__ = ['tag', 'contents']

    def __init__(self, tag: str, contents: T):
        self.tag = tag
        self.contents = contents

class Nil():
    nil = None
    __slots__ = []

    def __init__(self):
        if (Nil.nil is None):
            Nil.nil = self
            return
        raise ValueError('Nil cannot get instantiated twice.')

    def __len__(self):
        return 0

    def __getitem__(self, n):
        raise IndexError('Out of bounds')

    @property
    def head(self):
        raise IndexError('Out of bounds')

    @property
    def tail(self):
        raise IndexError('Out of bounds')

    def __repr__(self):
        return '[]'
_nil = Nil()

class Cons():
    __slots__ = ['head', 'tail']

    def __init__(self, _head, _tail):
        self.head = _head
        self.tail = _tail

    def __len__(self):
        nil = _nil
        l = 0
        while (self is not nil):
            l += 1
            self = self.tail
        return l

    def __iter__(self):
        nil = _nil
        while (self is not nil):
            (yield self.head)
            self = self.tail

    def __getitem__(self, n):
        while (n != 0):
            self = self.tail
            n -= 1
        return self.head

    def __repr__(self):
        return repr(list(self))
try:

    def mk_pretty():
        from prettyprinter import register_pretty, pretty_call, pprint

        @register_pretty(Tokens)
        def pretty_tokens(value, ctx):
            return pretty_call(ctx, Tokens, offset=value.offset, array=value.array)

        @register_pretty(AST)
        def pretty_ast(value, ctx):
            return pretty_call(ctx, AST, tag=value.tag, contents=value.contents)
    mk_pretty()
    del mk_pretty
except ImportError:
    pass
del T, Generic, TypeVar
builtin_cons = Cons
builtin_nil = _nil
builtin_mk_ast = AST

def mk_parser():
    pass

    def rbnf_named_lr_step_atom(rbnf_tmp_0, builtin_state, builtin_tokens):
        try:
            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
            if (_rbnf_cur_token.idint is 7):
                builtin_tokens.offset += 1
            else:
                _rbnf_cur_token = None
        except IndexError:
            _rbnf_cur_token = None
        lcl_0 = _rbnf_cur_token
        rbnf_tmp_1 = lcl_0
        lcl_0 = (rbnf_tmp_1 is None)
        if lcl_0:
            lcl_1 = builtin_tokens.offset
            lcl_1 = (lcl_1, 'quote . not match')
            lcl_1 = builtin_cons(lcl_1, builtin_nil)
            lcl_1 = (False, lcl_1)
            lcl_0 = lcl_1
        else:
            lcl_1 = rbnf_named_parse_nameStr(builtin_state, builtin_tokens)
            rbnf_named__check_2 = lcl_1
            lcl_1 = rbnf_named__check_2[0]
            lcl_1 = (lcl_1 == False)
            if lcl_1:
                lcl_1 = rbnf_named__check_2
            else:
                lcl_2 = rbnf_named__check_2[1]
                rbnf_tmp_2 = lcl_2
                lcl_2 = EField(rbnf_tmp_0, rbnf_tmp_2)
                rbnf_tmp_1_ = lcl_2
                lcl_2 = (True, rbnf_tmp_1_)
                lcl_1 = lcl_2
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_lr_loop_atom(rbnf_tmp_0, builtin_state, builtin_tokens):
        rbnf_named_lr_atom_reduce = rbnf_tmp_0
        lcl_0 = builtin_tokens.offset
        rbnf_named__off_0 = lcl_0
        lcl_0 = rbnf_named_lr_step_atom(rbnf_named_lr_atom_reduce, builtin_state, builtin_tokens)
        rbnf_named_lr_atom_try = lcl_0
        lcl_0 = rbnf_named_lr_atom_try[0]
        lcl_0 = (lcl_0 is not False)
        while lcl_0:
            lcl_1 = builtin_tokens.offset
            rbnf_named__off_0 = lcl_1
            lcl_1 = rbnf_named_lr_atom_try[1]
            rbnf_named_lr_atom_reduce = lcl_1
            lcl_1 = rbnf_named_lr_step_atom(rbnf_named_lr_atom_reduce, builtin_state, builtin_tokens)
            rbnf_named_lr_atom_try = lcl_1
            lcl_1 = rbnf_named_lr_atom_try[0]
            lcl_1 = (lcl_1 is not False)
            lcl_0 = lcl_1
        lcl_0 = builtin_tokens.offset
        lcl_0 = (lcl_0 == rbnf_named__off_0)
        if lcl_0:
            lcl_1 = (True, rbnf_named_lr_atom_reduce)
            lcl_0 = lcl_1
        else:
            lcl_0 = rbnf_named_lr_atom_try
        return lcl_0

    def rbnf_named_lr_step_call(rbnf_tmp_0, builtin_state, builtin_tokens):
        lcl_0 = rbnf_named_parse_atom(builtin_state, builtin_tokens)
        rbnf_named__check_1 = lcl_0
        lcl_0 = rbnf_named__check_1[0]
        lcl_0 = (lcl_0 == False)
        if lcl_0:
            lcl_0 = rbnf_named__check_1
        else:
            lcl_1 = rbnf_named__check_1[1]
            rbnf_tmp_1 = lcl_1
            lcl_1 = EApp(rbnf_tmp_0, rbnf_tmp_1)
            rbnf_tmp_1_ = lcl_1
            lcl_1 = (True, rbnf_tmp_1_)
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_lr_loop_call(rbnf_tmp_0, builtin_state, builtin_tokens):
        rbnf_named_lr_call_reduce = rbnf_tmp_0
        lcl_0 = builtin_tokens.offset
        rbnf_named__off_0 = lcl_0
        lcl_0 = rbnf_named_lr_step_call(rbnf_named_lr_call_reduce, builtin_state, builtin_tokens)
        rbnf_named_lr_call_try = lcl_0
        lcl_0 = rbnf_named_lr_call_try[0]
        lcl_0 = (lcl_0 is not False)
        while lcl_0:
            lcl_1 = builtin_tokens.offset
            rbnf_named__off_0 = lcl_1
            lcl_1 = rbnf_named_lr_call_try[1]
            rbnf_named_lr_call_reduce = lcl_1
            lcl_1 = rbnf_named_lr_step_call(rbnf_named_lr_call_reduce, builtin_state, builtin_tokens)
            rbnf_named_lr_call_try = lcl_1
            lcl_1 = rbnf_named_lr_call_try[0]
            lcl_1 = (lcl_1 is not False)
            lcl_0 = lcl_1
        lcl_0 = builtin_tokens.offset
        lcl_0 = (lcl_0 == rbnf_named__off_0)
        if lcl_0:
            lcl_1 = (True, rbnf_named_lr_call_reduce)
            lcl_0 = lcl_1
        else:
            lcl_0 = rbnf_named_lr_call_try
        return lcl_0

    def rbnf_named_lr_step_calltype(rbnf_tmp_0, builtin_state, builtin_tokens):
        lcl_0 = rbnf_named_parse_type(builtin_state, builtin_tokens)
        rbnf_named__check_1 = lcl_0
        lcl_0 = rbnf_named__check_1[0]
        lcl_0 = (lcl_0 == False)
        if lcl_0:
            lcl_0 = rbnf_named__check_1
        else:
            lcl_1 = rbnf_named__check_1[1]
            rbnf_tmp_1 = lcl_1
            lcl_1 = TApp(rbnf_tmp_0, rbnf_tmp_1)
            rbnf_tmp_1_ = lcl_1
            lcl_1 = (True, rbnf_tmp_1_)
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_lr_loop_calltype(rbnf_tmp_0, builtin_state, builtin_tokens):
        rbnf_named_lr_calltype_reduce = rbnf_tmp_0
        lcl_0 = builtin_tokens.offset
        rbnf_named__off_0 = lcl_0
        lcl_0 = rbnf_named_lr_step_calltype(rbnf_named_lr_calltype_reduce, builtin_state, builtin_tokens)
        rbnf_named_lr_calltype_try = lcl_0
        lcl_0 = rbnf_named_lr_calltype_try[0]
        lcl_0 = (lcl_0 is not False)
        while lcl_0:
            lcl_1 = builtin_tokens.offset
            rbnf_named__off_0 = lcl_1
            lcl_1 = rbnf_named_lr_calltype_try[1]
            rbnf_named_lr_calltype_reduce = lcl_1
            lcl_1 = rbnf_named_lr_step_calltype(rbnf_named_lr_calltype_reduce, builtin_state, builtin_tokens)
            rbnf_named_lr_calltype_try = lcl_1
            lcl_1 = rbnf_named_lr_calltype_try[0]
            lcl_1 = (lcl_1 is not False)
            lcl_0 = lcl_1
        lcl_0 = builtin_tokens.offset
        lcl_0 = (lcl_0 == rbnf_named__off_0)
        if lcl_0:
            lcl_1 = (True, rbnf_named_lr_calltype_reduce)
            lcl_0 = lcl_1
        else:
            lcl_0 = rbnf_named_lr_calltype_try
        return lcl_0

    def rbnf_named_lr_step_rbnfmacro_0(rbnf_tmp_0, builtin_state, builtin_tokens):
        lcl_0 = rbnf_named_parse_import(builtin_state, builtin_tokens)
        rbnf_named__check_1 = lcl_0
        lcl_0 = rbnf_named__check_1[0]
        lcl_0 = (lcl_0 == False)
        if lcl_0:
            lcl_0 = rbnf_named__check_1
        else:
            lcl_1 = rbnf_named__check_1[1]
            rbnf_tmp_1 = lcl_1
            lcl_1 = rbnf_tmp_0.append
            lcl_1 = lcl_1(rbnf_tmp_1)
            rbnf_tmp_1_ = rbnf_tmp_0
            lcl_2 = (True, rbnf_tmp_1_)
            lcl_0 = lcl_2
        return lcl_0

    def rbnf_named_lr_loop_rbnfmacro_0(rbnf_tmp_0, builtin_state, builtin_tokens):
        rbnf_named_lr_rbnfmacro_0_reduce = rbnf_tmp_0
        lcl_0 = builtin_tokens.offset
        rbnf_named__off_0 = lcl_0
        lcl_0 = rbnf_named_lr_step_rbnfmacro_0(rbnf_named_lr_rbnfmacro_0_reduce, builtin_state, builtin_tokens)
        rbnf_named_lr_rbnfmacro_0_try = lcl_0
        lcl_0 = rbnf_named_lr_rbnfmacro_0_try[0]
        lcl_0 = (lcl_0 is not False)
        while lcl_0:
            lcl_1 = builtin_tokens.offset
            rbnf_named__off_0 = lcl_1
            lcl_1 = rbnf_named_lr_rbnfmacro_0_try[1]
            rbnf_named_lr_rbnfmacro_0_reduce = lcl_1
            lcl_1 = rbnf_named_lr_step_rbnfmacro_0(rbnf_named_lr_rbnfmacro_0_reduce, builtin_state, builtin_tokens)
            rbnf_named_lr_rbnfmacro_0_try = lcl_1
            lcl_1 = rbnf_named_lr_rbnfmacro_0_try[0]
            lcl_1 = (lcl_1 is not False)
            lcl_0 = lcl_1
        lcl_0 = builtin_tokens.offset
        lcl_0 = (lcl_0 == rbnf_named__off_0)
        if lcl_0:
            lcl_1 = (True, rbnf_named_lr_rbnfmacro_0_reduce)
            lcl_0 = lcl_1
        else:
            lcl_0 = rbnf_named_lr_rbnfmacro_0_try
        return lcl_0

    def rbnf_named_lr_step_rbnfmacro_1(rbnf_tmp_0, builtin_state, builtin_tokens):
        lcl_0 = rbnf_named_parse_declaration(builtin_state, builtin_tokens)
        rbnf_named__check_1 = lcl_0
        lcl_0 = rbnf_named__check_1[0]
        lcl_0 = (lcl_0 == False)
        if lcl_0:
            lcl_0 = rbnf_named__check_1
        else:
            lcl_1 = rbnf_named__check_1[1]
            rbnf_tmp_1 = lcl_1
            lcl_1 = rbnf_tmp_0.append
            lcl_1 = lcl_1(rbnf_tmp_1)
            rbnf_tmp_1_ = rbnf_tmp_0
            lcl_2 = (True, rbnf_tmp_1_)
            lcl_0 = lcl_2
        return lcl_0

    def rbnf_named_lr_loop_rbnfmacro_1(rbnf_tmp_0, builtin_state, builtin_tokens):
        rbnf_named_lr_rbnfmacro_1_reduce = rbnf_tmp_0
        lcl_0 = builtin_tokens.offset
        rbnf_named__off_0 = lcl_0
        lcl_0 = rbnf_named_lr_step_rbnfmacro_1(rbnf_named_lr_rbnfmacro_1_reduce, builtin_state, builtin_tokens)
        rbnf_named_lr_rbnfmacro_1_try = lcl_0
        lcl_0 = rbnf_named_lr_rbnfmacro_1_try[0]
        lcl_0 = (lcl_0 is not False)
        while lcl_0:
            lcl_1 = builtin_tokens.offset
            rbnf_named__off_0 = lcl_1
            lcl_1 = rbnf_named_lr_rbnfmacro_1_try[1]
            rbnf_named_lr_rbnfmacro_1_reduce = lcl_1
            lcl_1 = rbnf_named_lr_step_rbnfmacro_1(rbnf_named_lr_rbnfmacro_1_reduce, builtin_state, builtin_tokens)
            rbnf_named_lr_rbnfmacro_1_try = lcl_1
            lcl_1 = rbnf_named_lr_rbnfmacro_1_try[0]
            lcl_1 = (lcl_1 is not False)
            lcl_0 = lcl_1
        lcl_0 = builtin_tokens.offset
        lcl_0 = (lcl_0 == rbnf_named__off_0)
        if lcl_0:
            lcl_1 = (True, rbnf_named_lr_rbnfmacro_1_reduce)
            lcl_0 = lcl_1
        else:
            lcl_0 = rbnf_named_lr_rbnfmacro_1_try
        return lcl_0

    def rbnf_named_lr_step_rbnfmacro_2(rbnf_tmp_0, builtin_state, builtin_tokens):
        lcl_0 = rbnf_named_parse_nameStr(builtin_state, builtin_tokens)
        rbnf_named__check_1 = lcl_0
        lcl_0 = rbnf_named__check_1[0]
        lcl_0 = (lcl_0 == False)
        if lcl_0:
            lcl_0 = rbnf_named__check_1
        else:
            lcl_1 = rbnf_named__check_1[1]
            rbnf_tmp_1 = lcl_1
            lcl_1 = rbnf_tmp_0.append
            lcl_1 = lcl_1(rbnf_tmp_1)
            rbnf_tmp_1_ = rbnf_tmp_0
            lcl_2 = (True, rbnf_tmp_1_)
            lcl_0 = lcl_2
        return lcl_0

    def rbnf_named_lr_loop_rbnfmacro_2(rbnf_tmp_0, builtin_state, builtin_tokens):
        rbnf_named_lr_rbnfmacro_2_reduce = rbnf_tmp_0
        lcl_0 = builtin_tokens.offset
        rbnf_named__off_0 = lcl_0
        lcl_0 = rbnf_named_lr_step_rbnfmacro_2(rbnf_named_lr_rbnfmacro_2_reduce, builtin_state, builtin_tokens)
        rbnf_named_lr_rbnfmacro_2_try = lcl_0
        lcl_0 = rbnf_named_lr_rbnfmacro_2_try[0]
        lcl_0 = (lcl_0 is not False)
        while lcl_0:
            lcl_1 = builtin_tokens.offset
            rbnf_named__off_0 = lcl_1
            lcl_1 = rbnf_named_lr_rbnfmacro_2_try[1]
            rbnf_named_lr_rbnfmacro_2_reduce = lcl_1
            lcl_1 = rbnf_named_lr_step_rbnfmacro_2(rbnf_named_lr_rbnfmacro_2_reduce, builtin_state, builtin_tokens)
            rbnf_named_lr_rbnfmacro_2_try = lcl_1
            lcl_1 = rbnf_named_lr_rbnfmacro_2_try[0]
            lcl_1 = (lcl_1 is not False)
            lcl_0 = lcl_1
        lcl_0 = builtin_tokens.offset
        lcl_0 = (lcl_0 == rbnf_named__off_0)
        if lcl_0:
            lcl_1 = (True, rbnf_named_lr_rbnfmacro_2_reduce)
            lcl_0 = lcl_1
        else:
            lcl_0 = rbnf_named_lr_rbnfmacro_2_try
        return lcl_0

    def rbnf_named_lr_step_rbnfmacro_3(rbnf_tmp_0, builtin_state, builtin_tokens):
        try:
            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
            if (_rbnf_cur_token.idint is 11):
                builtin_tokens.offset += 1
            else:
                _rbnf_cur_token = None
        except IndexError:
            _rbnf_cur_token = None
        lcl_0 = _rbnf_cur_token
        rbnf_tmp_1 = lcl_0
        lcl_0 = (rbnf_tmp_1 is None)
        if lcl_0:
            lcl_1 = builtin_tokens.offset
            lcl_1 = (lcl_1, 'quote , not match')
            lcl_1 = builtin_cons(lcl_1, builtin_nil)
            lcl_1 = (False, lcl_1)
            lcl_0 = lcl_1
        else:
            lcl_1 = rbnf_named_parse_toptype(builtin_state, builtin_tokens)
            rbnf_named__check_2 = lcl_1
            lcl_1 = rbnf_named__check_2[0]
            lcl_1 = (lcl_1 == False)
            if lcl_1:
                lcl_1 = rbnf_named__check_2
            else:
                lcl_2 = rbnf_named__check_2[1]
                rbnf_tmp_2 = lcl_2
                lcl_2 = rbnf_tmp_0.append
                lcl_2 = lcl_2(rbnf_tmp_2)
                rbnf_tmp_1_ = rbnf_tmp_0
                lcl_3 = (True, rbnf_tmp_1_)
                lcl_1 = lcl_3
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_lr_loop_rbnfmacro_3(rbnf_tmp_0, builtin_state, builtin_tokens):
        rbnf_named_lr_rbnfmacro_3_reduce = rbnf_tmp_0
        lcl_0 = builtin_tokens.offset
        rbnf_named__off_0 = lcl_0
        lcl_0 = rbnf_named_lr_step_rbnfmacro_3(rbnf_named_lr_rbnfmacro_3_reduce, builtin_state, builtin_tokens)
        rbnf_named_lr_rbnfmacro_3_try = lcl_0
        lcl_0 = rbnf_named_lr_rbnfmacro_3_try[0]
        lcl_0 = (lcl_0 is not False)
        while lcl_0:
            lcl_1 = builtin_tokens.offset
            rbnf_named__off_0 = lcl_1
            lcl_1 = rbnf_named_lr_rbnfmacro_3_try[1]
            rbnf_named_lr_rbnfmacro_3_reduce = lcl_1
            lcl_1 = rbnf_named_lr_step_rbnfmacro_3(rbnf_named_lr_rbnfmacro_3_reduce, builtin_state, builtin_tokens)
            rbnf_named_lr_rbnfmacro_3_try = lcl_1
            lcl_1 = rbnf_named_lr_rbnfmacro_3_try[0]
            lcl_1 = (lcl_1 is not False)
            lcl_0 = lcl_1
        lcl_0 = builtin_tokens.offset
        lcl_0 = (lcl_0 == rbnf_named__off_0)
        if lcl_0:
            lcl_1 = (True, rbnf_named_lr_rbnfmacro_3_reduce)
            lcl_0 = lcl_1
        else:
            lcl_0 = rbnf_named_lr_rbnfmacro_3_try
        return lcl_0

    def rbnf_named_lr_step_rbnfmacro_4(rbnf_tmp_0, builtin_state, builtin_tokens):
        lcl_0 = rbnf_named_parse_tvar(builtin_state, builtin_tokens)
        rbnf_named__check_1 = lcl_0
        lcl_0 = rbnf_named__check_1[0]
        lcl_0 = (lcl_0 == False)
        if lcl_0:
            lcl_0 = rbnf_named__check_1
        else:
            lcl_1 = rbnf_named__check_1[1]
            rbnf_tmp_1 = lcl_1
            lcl_1 = rbnf_tmp_0.append
            lcl_1 = lcl_1(rbnf_tmp_1)
            rbnf_tmp_1_ = rbnf_tmp_0
            lcl_2 = (True, rbnf_tmp_1_)
            lcl_0 = lcl_2
        return lcl_0

    def rbnf_named_lr_loop_rbnfmacro_4(rbnf_tmp_0, builtin_state, builtin_tokens):
        rbnf_named_lr_rbnfmacro_4_reduce = rbnf_tmp_0
        lcl_0 = builtin_tokens.offset
        rbnf_named__off_0 = lcl_0
        lcl_0 = rbnf_named_lr_step_rbnfmacro_4(rbnf_named_lr_rbnfmacro_4_reduce, builtin_state, builtin_tokens)
        rbnf_named_lr_rbnfmacro_4_try = lcl_0
        lcl_0 = rbnf_named_lr_rbnfmacro_4_try[0]
        lcl_0 = (lcl_0 is not False)
        while lcl_0:
            lcl_1 = builtin_tokens.offset
            rbnf_named__off_0 = lcl_1
            lcl_1 = rbnf_named_lr_rbnfmacro_4_try[1]
            rbnf_named_lr_rbnfmacro_4_reduce = lcl_1
            lcl_1 = rbnf_named_lr_step_rbnfmacro_4(rbnf_named_lr_rbnfmacro_4_reduce, builtin_state, builtin_tokens)
            rbnf_named_lr_rbnfmacro_4_try = lcl_1
            lcl_1 = rbnf_named_lr_rbnfmacro_4_try[0]
            lcl_1 = (lcl_1 is not False)
            lcl_0 = lcl_1
        lcl_0 = builtin_tokens.offset
        lcl_0 = (lcl_0 == rbnf_named__off_0)
        if lcl_0:
            lcl_1 = (True, rbnf_named_lr_rbnfmacro_4_reduce)
            lcl_0 = lcl_1
        else:
            lcl_0 = rbnf_named_lr_rbnfmacro_4_try
        return lcl_0

    def rbnf_named_lr_step_rbnfmacro_5(rbnf_tmp_0, builtin_state, builtin_tokens):
        try:
            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
            if (_rbnf_cur_token.idint is 11):
                builtin_tokens.offset += 1
            else:
                _rbnf_cur_token = None
        except IndexError:
            _rbnf_cur_token = None
        lcl_0 = _rbnf_cur_token
        rbnf_tmp_1 = lcl_0
        lcl_0 = (rbnf_tmp_1 is None)
        if lcl_0:
            lcl_1 = builtin_tokens.offset
            lcl_1 = (lcl_1, 'quote , not match')
            lcl_1 = builtin_cons(lcl_1, builtin_nil)
            lcl_1 = (False, lcl_1)
            lcl_0 = lcl_1
        else:
            lcl_1 = rbnf_named_parse_record_field_def(builtin_state, builtin_tokens)
            rbnf_named__check_2 = lcl_1
            lcl_1 = rbnf_named__check_2[0]
            lcl_1 = (lcl_1 == False)
            if lcl_1:
                lcl_1 = rbnf_named__check_2
            else:
                lcl_2 = rbnf_named__check_2[1]
                rbnf_tmp_2 = lcl_2
                lcl_2 = rbnf_tmp_0.append
                lcl_2 = lcl_2(rbnf_tmp_2)
                rbnf_tmp_1_ = rbnf_tmp_0
                lcl_3 = (True, rbnf_tmp_1_)
                lcl_1 = lcl_3
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_lr_loop_rbnfmacro_5(rbnf_tmp_0, builtin_state, builtin_tokens):
        rbnf_named_lr_rbnfmacro_5_reduce = rbnf_tmp_0
        lcl_0 = builtin_tokens.offset
        rbnf_named__off_0 = lcl_0
        lcl_0 = rbnf_named_lr_step_rbnfmacro_5(rbnf_named_lr_rbnfmacro_5_reduce, builtin_state, builtin_tokens)
        rbnf_named_lr_rbnfmacro_5_try = lcl_0
        lcl_0 = rbnf_named_lr_rbnfmacro_5_try[0]
        lcl_0 = (lcl_0 is not False)
        while lcl_0:
            lcl_1 = builtin_tokens.offset
            rbnf_named__off_0 = lcl_1
            lcl_1 = rbnf_named_lr_rbnfmacro_5_try[1]
            rbnf_named_lr_rbnfmacro_5_reduce = lcl_1
            lcl_1 = rbnf_named_lr_step_rbnfmacro_5(rbnf_named_lr_rbnfmacro_5_reduce, builtin_state, builtin_tokens)
            rbnf_named_lr_rbnfmacro_5_try = lcl_1
            lcl_1 = rbnf_named_lr_rbnfmacro_5_try[0]
            lcl_1 = (lcl_1 is not False)
            lcl_0 = lcl_1
        lcl_0 = builtin_tokens.offset
        lcl_0 = (lcl_0 == rbnf_named__off_0)
        if lcl_0:
            lcl_1 = (True, rbnf_named_lr_rbnfmacro_5_reduce)
            lcl_0 = lcl_1
        else:
            lcl_0 = rbnf_named_lr_rbnfmacro_5_try
        return lcl_0

    def rbnf_named_lr_step_rbnfmacro_7(rbnf_tmp_0, builtin_state, builtin_tokens):
        try:
            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
            if (_rbnf_cur_token.idint is 11):
                builtin_tokens.offset += 1
            else:
                _rbnf_cur_token = None
        except IndexError:
            _rbnf_cur_token = None
        lcl_0 = _rbnf_cur_token
        rbnf_tmp_1 = lcl_0
        lcl_0 = (rbnf_tmp_1 is None)
        if lcl_0:
            lcl_1 = builtin_tokens.offset
            lcl_1 = (lcl_1, 'quote , not match')
            lcl_1 = builtin_cons(lcl_1, builtin_nil)
            lcl_1 = (False, lcl_1)
            lcl_0 = lcl_1
        else:
            lcl_1 = rbnf_named_parse_expr(builtin_state, builtin_tokens)
            rbnf_named__check_2 = lcl_1
            lcl_1 = rbnf_named__check_2[0]
            lcl_1 = (lcl_1 == False)
            if lcl_1:
                lcl_1 = rbnf_named__check_2
            else:
                lcl_2 = rbnf_named__check_2[1]
                rbnf_tmp_2 = lcl_2
                lcl_2 = rbnf_tmp_0.append
                lcl_2 = lcl_2(rbnf_tmp_2)
                rbnf_tmp_1_ = rbnf_tmp_0
                lcl_3 = (True, rbnf_tmp_1_)
                lcl_1 = lcl_3
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_lr_loop_rbnfmacro_7(rbnf_tmp_0, builtin_state, builtin_tokens):
        rbnf_named_lr_rbnfmacro_7_reduce = rbnf_tmp_0
        lcl_0 = builtin_tokens.offset
        rbnf_named__off_0 = lcl_0
        lcl_0 = rbnf_named_lr_step_rbnfmacro_7(rbnf_named_lr_rbnfmacro_7_reduce, builtin_state, builtin_tokens)
        rbnf_named_lr_rbnfmacro_7_try = lcl_0
        lcl_0 = rbnf_named_lr_rbnfmacro_7_try[0]
        lcl_0 = (lcl_0 is not False)
        while lcl_0:
            lcl_1 = builtin_tokens.offset
            rbnf_named__off_0 = lcl_1
            lcl_1 = rbnf_named_lr_rbnfmacro_7_try[1]
            rbnf_named_lr_rbnfmacro_7_reduce = lcl_1
            lcl_1 = rbnf_named_lr_step_rbnfmacro_7(rbnf_named_lr_rbnfmacro_7_reduce, builtin_state, builtin_tokens)
            rbnf_named_lr_rbnfmacro_7_try = lcl_1
            lcl_1 = rbnf_named_lr_rbnfmacro_7_try[0]
            lcl_1 = (lcl_1 is not False)
            lcl_0 = lcl_1
        lcl_0 = builtin_tokens.offset
        lcl_0 = (lcl_0 == rbnf_named__off_0)
        if lcl_0:
            lcl_1 = (True, rbnf_named_lr_rbnfmacro_7_reduce)
            lcl_0 = lcl_1
        else:
            lcl_0 = rbnf_named_lr_rbnfmacro_7_try
        return lcl_0

    def rbnf_named_parse_START(builtin_state, builtin_tokens):
        try:
            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
            if (_rbnf_cur_token.idint is 0):
                builtin_tokens.offset += 1
            else:
                _rbnf_cur_token = None
        except IndexError:
            _rbnf_cur_token = None
        lcl_0 = _rbnf_cur_token
        rbnf_tmp_0 = lcl_0
        lcl_0 = (rbnf_tmp_0 is None)
        if lcl_0:
            lcl_1 = builtin_tokens.offset
            lcl_1 = (lcl_1, 'BOF not match')
            lcl_1 = builtin_cons(lcl_1, builtin_nil)
            lcl_1 = (False, lcl_1)
            lcl_0 = lcl_1
        else:
            lcl_1 = rbnf_named_parse_toplevel(builtin_state, builtin_tokens)
            rbnf_named__check_1 = lcl_1
            lcl_1 = rbnf_named__check_1[0]
            lcl_1 = (lcl_1 == False)
            if lcl_1:
                lcl_1 = rbnf_named__check_1
            else:
                lcl_2 = rbnf_named__check_1[1]
                rbnf_tmp_1 = lcl_2
                try:
                    _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                    if (_rbnf_cur_token.idint is 1):
                        builtin_tokens.offset += 1
                    else:
                        _rbnf_cur_token = None
                except IndexError:
                    _rbnf_cur_token = None
                lcl_2 = _rbnf_cur_token
                rbnf_tmp_2 = lcl_2
                lcl_2 = (rbnf_tmp_2 is None)
                if lcl_2:
                    lcl_3 = builtin_tokens.offset
                    lcl_3 = (lcl_3, 'EOF not match')
                    lcl_3 = builtin_cons(lcl_3, builtin_nil)
                    lcl_3 = (False, lcl_3)
                    lcl_2 = lcl_3
                else:
                    rbnf_tmp_1_ = rbnf_tmp_1
                    lcl_3 = (True, rbnf_tmp_1_)
                    lcl_2 = lcl_3
                lcl_1 = lcl_2
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_arrowtype(builtin_state, builtin_tokens):
        lcl_0 = rbnf_named_parse_calltype(builtin_state, builtin_tokens)
        rbnf_named__check_0 = lcl_0
        lcl_0 = rbnf_named__check_0[0]
        lcl_0 = (lcl_0 == False)
        if lcl_0:
            lcl_0 = rbnf_named__check_0
        else:
            lcl_1 = rbnf_named__check_0[1]
            rbnf_tmp_0 = lcl_1
            lcl_1 = builtin_tokens.offset
            rbnf_named__off_0 = lcl_1
            try:
                builtin_tokens.array[(builtin_tokens.offset + 0)]
                _rbnf_peek_tmp = True
            except IndexError:
                _rbnf_peek_tmp = False
            lcl_1 = _rbnf_peek_tmp
            if lcl_1:
                lcl_3 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                lcl_3 = lcl_3.idint
                if (lcl_3 == 8):
                    _rbnf_old_offset = builtin_tokens.offset
                    _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                    builtin_tokens.offset = (_rbnf_old_offset + 1)
                    lcl_4 = _rbnf_cur_token
                    rbnf_tmp_1 = lcl_4
                    lcl_4 = rbnf_named_parse_toptype(builtin_state, builtin_tokens)
                    rbnf_named__check_2 = lcl_4
                    lcl_4 = rbnf_named__check_2[0]
                    lcl_4 = (lcl_4 == False)
                    if lcl_4:
                        lcl_4 = rbnf_named__check_2
                    else:
                        lcl_5 = rbnf_named__check_2[1]
                        rbnf_tmp_2 = lcl_5
                        lcl_5 = TArrow(rbnf_tmp_0, rbnf_tmp_2)
                        rbnf_tmp_1_ = lcl_5
                        lcl_5 = (True, rbnf_tmp_1_)
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                else:
                    rbnf_tmp_1_ = rbnf_tmp_0
                    lcl_4 = (True, rbnf_tmp_1_)
                    lcl_2 = lcl_4
                lcl_1 = lcl_2
            else:
                lcl_2 = (rbnf_named__off_0, 'arrowtype got EOF')
                lcl_2 = builtin_cons(lcl_2, builtin_nil)
                lcl_2 = (False, lcl_2)
                lcl_1 = lcl_2
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_atom(builtin_state, builtin_tokens):
        lcl_0 = builtin_tokens.offset
        rbnf_named__off_0 = lcl_0
        try:
            builtin_tokens.array[(builtin_tokens.offset + 0)]
            _rbnf_peek_tmp = True
        except IndexError:
            _rbnf_peek_tmp = False
        lcl_0 = _rbnf_peek_tmp
        if lcl_0:
            lcl_2 = builtin_tokens.array[(builtin_tokens.offset + 0)]
            lcl_2 = lcl_2.idint
            if (lcl_2 == 14):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = rbnf_tmp_0.value
                lcl_3 = parse_str(lcl_3)
                lcl_3 = Str(lcl_3)
                lcl_3 = EVal(lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_3 = rbnf_named_lr_loop_atom(rbnf_tmp_1_, builtin_state, builtin_tokens)
                lcl_1 = lcl_3
            elif (lcl_2 == 34):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = Bl(True)
                lcl_3 = EVal(lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_3 = rbnf_named_lr_loop_atom(rbnf_tmp_1_, builtin_state, builtin_tokens)
                lcl_1 = lcl_3
            elif (lcl_2 == 35):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = Bl(False)
                lcl_3 = EVal(lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_3 = rbnf_named_lr_loop_atom(rbnf_tmp_1_, builtin_state, builtin_tokens)
                lcl_1 = lcl_3
            elif (lcl_2 == 31):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                try:
                    _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                    if (_rbnf_cur_token.idint is 14):
                        builtin_tokens.offset += 1
                    else:
                        _rbnf_cur_token = None
                except IndexError:
                    _rbnf_cur_token = None
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_1 = lcl_3
                lcl_3 = (rbnf_tmp_1 is None)
                if lcl_3:
                    lcl_4 = builtin_tokens.offset
                    lcl_4 = (lcl_4, 'str not match')
                    lcl_4 = builtin_cons(lcl_4, builtin_nil)
                    lcl_4 = (False, lcl_4)
                    lcl_3 = lcl_4
                else:
                    lcl_4 = rbnf_tmp_1.value
                    lcl_4 = parse_str(lcl_4)
                    lcl_4 = EExt(lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_4 = rbnf_named_lr_loop_atom(rbnf_tmp_1_, builtin_state, builtin_tokens)
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 16):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = rbnf_named_parse_atom(builtin_state, builtin_tokens)
                rbnf_named__check_1 = lcl_3
                lcl_3 = rbnf_named__check_1[0]
                lcl_3 = (lcl_3 == False)
                if lcl_3:
                    lcl_3 = rbnf_named__check_1
                else:
                    lcl_4 = rbnf_named__check_1[1]
                    rbnf_tmp_1 = lcl_4
                    try:
                        _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                        if (_rbnf_cur_token.idint is 4):
                            builtin_tokens.offset += 1
                        else:
                            _rbnf_cur_token = None
                    except IndexError:
                        _rbnf_cur_token = None
                    lcl_4 = _rbnf_cur_token
                    rbnf_tmp_2 = lcl_4
                    lcl_4 = (rbnf_tmp_2 is None)
                    if lcl_4:
                        lcl_5 = builtin_tokens.offset
                        lcl_5 = (lcl_5, 'quote as not match')
                        lcl_5 = builtin_cons(lcl_5, builtin_nil)
                        lcl_5 = (False, lcl_5)
                        lcl_4 = lcl_5
                    else:
                        lcl_5 = rbnf_named_parse_nameStr(builtin_state, builtin_tokens)
                        rbnf_named__check_3 = lcl_5
                        lcl_5 = rbnf_named__check_3[0]
                        lcl_5 = (lcl_5 == False)
                        if lcl_5:
                            lcl_5 = rbnf_named__check_3
                        else:
                            lcl_6 = rbnf_named__check_3[1]
                            rbnf_tmp_3 = lcl_6
                            lcl_6 = EQuery(rbnf_tmp_3, rbnf_tmp_1)
                            rbnf_tmp_1_ = lcl_6
                            lcl_6 = rbnf_named_lr_loop_atom(rbnf_tmp_1_, builtin_state, builtin_tokens)
                            lcl_5 = lcl_6
                        lcl_4 = lcl_5
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 9):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = builtin_tokens.offset
                rbnf_named__off_1 = lcl_3
                try:
                    builtin_tokens.array[(builtin_tokens.offset + 0)]
                    _rbnf_peek_tmp = True
                except IndexError:
                    _rbnf_peek_tmp = False
                lcl_3 = _rbnf_peek_tmp
                if lcl_3:
                    lcl_5 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                    lcl_5 = lcl_5.idint
                    if (lcl_5 == 14):
                        lcl_6 = rbnf_named_parse_rbnfmacro_7(builtin_state, builtin_tokens)
                        rbnf_named__check_1 = lcl_6
                        lcl_6 = rbnf_named__check_1[0]
                        lcl_6 = (lcl_6 == False)
                        if lcl_6:
                            lcl_6 = rbnf_named__check_1
                        else:
                            lcl_7 = rbnf_named__check_1[1]
                            rbnf_tmp_1 = lcl_7
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 10):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_7 = _rbnf_cur_token
                            rbnf_tmp_2 = lcl_7
                            lcl_7 = (rbnf_tmp_2 is None)
                            if lcl_7:
                                lcl_8 = builtin_tokens.offset
                                lcl_8 = (lcl_8, 'quote ) not match')
                                lcl_8 = builtin_cons(lcl_8, builtin_nil)
                                lcl_8 = (False, lcl_8)
                                lcl_7 = lcl_8
                            else:
                                lcl_8 = mk_tuple(ETup, rbnf_tmp_1)
                                rbnf_tmp_1_ = lcl_8
                                lcl_8 = rbnf_named_lr_loop_atom(rbnf_tmp_1_, builtin_state, builtin_tokens)
                                lcl_7 = lcl_8
                            lcl_6 = lcl_7
                        lcl_4 = lcl_6
                    elif (lcl_5 == 19):
                        lcl_6 = rbnf_named_parse_rbnfmacro_7(builtin_state, builtin_tokens)
                        rbnf_named__check_1 = lcl_6
                        lcl_6 = rbnf_named__check_1[0]
                        lcl_6 = (lcl_6 == False)
                        if lcl_6:
                            lcl_6 = rbnf_named__check_1
                        else:
                            lcl_7 = rbnf_named__check_1[1]
                            rbnf_tmp_1 = lcl_7
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 10):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_7 = _rbnf_cur_token
                            rbnf_tmp_2 = lcl_7
                            lcl_7 = (rbnf_tmp_2 is None)
                            if lcl_7:
                                lcl_8 = builtin_tokens.offset
                                lcl_8 = (lcl_8, 'quote ) not match')
                                lcl_8 = builtin_cons(lcl_8, builtin_nil)
                                lcl_8 = (False, lcl_8)
                                lcl_7 = lcl_8
                            else:
                                lcl_8 = mk_tuple(ETup, rbnf_tmp_1)
                                rbnf_tmp_1_ = lcl_8
                                lcl_8 = rbnf_named_lr_loop_atom(rbnf_tmp_1_, builtin_state, builtin_tokens)
                                lcl_7 = lcl_8
                            lcl_6 = lcl_7
                        lcl_4 = lcl_6
                    elif (lcl_5 == 24):
                        lcl_6 = rbnf_named_parse_rbnfmacro_7(builtin_state, builtin_tokens)
                        rbnf_named__check_1 = lcl_6
                        lcl_6 = rbnf_named__check_1[0]
                        lcl_6 = (lcl_6 == False)
                        if lcl_6:
                            lcl_6 = rbnf_named__check_1
                        else:
                            lcl_7 = rbnf_named__check_1[1]
                            rbnf_tmp_1 = lcl_7
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 10):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_7 = _rbnf_cur_token
                            rbnf_tmp_2 = lcl_7
                            lcl_7 = (rbnf_tmp_2 is None)
                            if lcl_7:
                                lcl_8 = builtin_tokens.offset
                                lcl_8 = (lcl_8, 'quote ) not match')
                                lcl_8 = builtin_cons(lcl_8, builtin_nil)
                                lcl_8 = (False, lcl_8)
                                lcl_7 = lcl_8
                            else:
                                lcl_8 = mk_tuple(ETup, rbnf_tmp_1)
                                rbnf_tmp_1_ = lcl_8
                                lcl_8 = rbnf_named_lr_loop_atom(rbnf_tmp_1_, builtin_state, builtin_tokens)
                                lcl_7 = lcl_8
                            lcl_6 = lcl_7
                        lcl_4 = lcl_6
                    elif (lcl_5 == 34):
                        lcl_6 = rbnf_named_parse_rbnfmacro_7(builtin_state, builtin_tokens)
                        rbnf_named__check_1 = lcl_6
                        lcl_6 = rbnf_named__check_1[0]
                        lcl_6 = (lcl_6 == False)
                        if lcl_6:
                            lcl_6 = rbnf_named__check_1
                        else:
                            lcl_7 = rbnf_named__check_1[1]
                            rbnf_tmp_1 = lcl_7
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 10):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_7 = _rbnf_cur_token
                            rbnf_tmp_2 = lcl_7
                            lcl_7 = (rbnf_tmp_2 is None)
                            if lcl_7:
                                lcl_8 = builtin_tokens.offset
                                lcl_8 = (lcl_8, 'quote ) not match')
                                lcl_8 = builtin_cons(lcl_8, builtin_nil)
                                lcl_8 = (False, lcl_8)
                                lcl_7 = lcl_8
                            else:
                                lcl_8 = mk_tuple(ETup, rbnf_tmp_1)
                                rbnf_tmp_1_ = lcl_8
                                lcl_8 = rbnf_named_lr_loop_atom(rbnf_tmp_1_, builtin_state, builtin_tokens)
                                lcl_7 = lcl_8
                            lcl_6 = lcl_7
                        lcl_4 = lcl_6
                    elif (lcl_5 == 23):
                        lcl_6 = rbnf_named_parse_rbnfmacro_7(builtin_state, builtin_tokens)
                        rbnf_named__check_1 = lcl_6
                        lcl_6 = rbnf_named__check_1[0]
                        lcl_6 = (lcl_6 == False)
                        if lcl_6:
                            lcl_6 = rbnf_named__check_1
                        else:
                            lcl_7 = rbnf_named__check_1[1]
                            rbnf_tmp_1 = lcl_7
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 10):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_7 = _rbnf_cur_token
                            rbnf_tmp_2 = lcl_7
                            lcl_7 = (rbnf_tmp_2 is None)
                            if lcl_7:
                                lcl_8 = builtin_tokens.offset
                                lcl_8 = (lcl_8, 'quote ) not match')
                                lcl_8 = builtin_cons(lcl_8, builtin_nil)
                                lcl_8 = (False, lcl_8)
                                lcl_7 = lcl_8
                            else:
                                lcl_8 = mk_tuple(ETup, rbnf_tmp_1)
                                rbnf_tmp_1_ = lcl_8
                                lcl_8 = rbnf_named_lr_loop_atom(rbnf_tmp_1_, builtin_state, builtin_tokens)
                                lcl_7 = lcl_8
                            lcl_6 = lcl_7
                        lcl_4 = lcl_6
                    elif (lcl_5 == 20):
                        lcl_6 = rbnf_named_parse_rbnfmacro_7(builtin_state, builtin_tokens)
                        rbnf_named__check_1 = lcl_6
                        lcl_6 = rbnf_named__check_1[0]
                        lcl_6 = (lcl_6 == False)
                        if lcl_6:
                            lcl_6 = rbnf_named__check_1
                        else:
                            lcl_7 = rbnf_named__check_1[1]
                            rbnf_tmp_1 = lcl_7
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 10):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_7 = _rbnf_cur_token
                            rbnf_tmp_2 = lcl_7
                            lcl_7 = (rbnf_tmp_2 is None)
                            if lcl_7:
                                lcl_8 = builtin_tokens.offset
                                lcl_8 = (lcl_8, 'quote ) not match')
                                lcl_8 = builtin_cons(lcl_8, builtin_nil)
                                lcl_8 = (False, lcl_8)
                                lcl_7 = lcl_8
                            else:
                                lcl_8 = mk_tuple(ETup, rbnf_tmp_1)
                                rbnf_tmp_1_ = lcl_8
                                lcl_8 = rbnf_named_lr_loop_atom(rbnf_tmp_1_, builtin_state, builtin_tokens)
                                lcl_7 = lcl_8
                            lcl_6 = lcl_7
                        lcl_4 = lcl_6
                    elif (lcl_5 == 25):
                        lcl_6 = rbnf_named_parse_rbnfmacro_7(builtin_state, builtin_tokens)
                        rbnf_named__check_1 = lcl_6
                        lcl_6 = rbnf_named__check_1[0]
                        lcl_6 = (lcl_6 == False)
                        if lcl_6:
                            lcl_6 = rbnf_named__check_1
                        else:
                            lcl_7 = rbnf_named__check_1[1]
                            rbnf_tmp_1 = lcl_7
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 10):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_7 = _rbnf_cur_token
                            rbnf_tmp_2 = lcl_7
                            lcl_7 = (rbnf_tmp_2 is None)
                            if lcl_7:
                                lcl_8 = builtin_tokens.offset
                                lcl_8 = (lcl_8, 'quote ) not match')
                                lcl_8 = builtin_cons(lcl_8, builtin_nil)
                                lcl_8 = (False, lcl_8)
                                lcl_7 = lcl_8
                            else:
                                lcl_8 = mk_tuple(ETup, rbnf_tmp_1)
                                rbnf_tmp_1_ = lcl_8
                                lcl_8 = rbnf_named_lr_loop_atom(rbnf_tmp_1_, builtin_state, builtin_tokens)
                                lcl_7 = lcl_8
                            lcl_6 = lcl_7
                        lcl_4 = lcl_6
                    elif (lcl_5 == 29):
                        lcl_6 = rbnf_named_parse_rbnfmacro_7(builtin_state, builtin_tokens)
                        rbnf_named__check_1 = lcl_6
                        lcl_6 = rbnf_named__check_1[0]
                        lcl_6 = (lcl_6 == False)
                        if lcl_6:
                            lcl_6 = rbnf_named__check_1
                        else:
                            lcl_7 = rbnf_named__check_1[1]
                            rbnf_tmp_1 = lcl_7
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 10):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_7 = _rbnf_cur_token
                            rbnf_tmp_2 = lcl_7
                            lcl_7 = (rbnf_tmp_2 is None)
                            if lcl_7:
                                lcl_8 = builtin_tokens.offset
                                lcl_8 = (lcl_8, 'quote ) not match')
                                lcl_8 = builtin_cons(lcl_8, builtin_nil)
                                lcl_8 = (False, lcl_8)
                                lcl_7 = lcl_8
                            else:
                                lcl_8 = mk_tuple(ETup, rbnf_tmp_1)
                                rbnf_tmp_1_ = lcl_8
                                lcl_8 = rbnf_named_lr_loop_atom(rbnf_tmp_1_, builtin_state, builtin_tokens)
                                lcl_7 = lcl_8
                            lcl_6 = lcl_7
                        lcl_4 = lcl_6
                    elif (lcl_5 == 35):
                        lcl_6 = rbnf_named_parse_rbnfmacro_7(builtin_state, builtin_tokens)
                        rbnf_named__check_1 = lcl_6
                        lcl_6 = rbnf_named__check_1[0]
                        lcl_6 = (lcl_6 == False)
                        if lcl_6:
                            lcl_6 = rbnf_named__check_1
                        else:
                            lcl_7 = rbnf_named__check_1[1]
                            rbnf_tmp_1 = lcl_7
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 10):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_7 = _rbnf_cur_token
                            rbnf_tmp_2 = lcl_7
                            lcl_7 = (rbnf_tmp_2 is None)
                            if lcl_7:
                                lcl_8 = builtin_tokens.offset
                                lcl_8 = (lcl_8, 'quote ) not match')
                                lcl_8 = builtin_cons(lcl_8, builtin_nil)
                                lcl_8 = (False, lcl_8)
                                lcl_7 = lcl_8
                            else:
                                lcl_8 = mk_tuple(ETup, rbnf_tmp_1)
                                rbnf_tmp_1_ = lcl_8
                                lcl_8 = rbnf_named_lr_loop_atom(rbnf_tmp_1_, builtin_state, builtin_tokens)
                                lcl_7 = lcl_8
                            lcl_6 = lcl_7
                        lcl_4 = lcl_6
                    elif (lcl_5 == 31):
                        lcl_6 = rbnf_named_parse_rbnfmacro_7(builtin_state, builtin_tokens)
                        rbnf_named__check_1 = lcl_6
                        lcl_6 = rbnf_named__check_1[0]
                        lcl_6 = (lcl_6 == False)
                        if lcl_6:
                            lcl_6 = rbnf_named__check_1
                        else:
                            lcl_7 = rbnf_named__check_1[1]
                            rbnf_tmp_1 = lcl_7
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 10):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_7 = _rbnf_cur_token
                            rbnf_tmp_2 = lcl_7
                            lcl_7 = (rbnf_tmp_2 is None)
                            if lcl_7:
                                lcl_8 = builtin_tokens.offset
                                lcl_8 = (lcl_8, 'quote ) not match')
                                lcl_8 = builtin_cons(lcl_8, builtin_nil)
                                lcl_8 = (False, lcl_8)
                                lcl_7 = lcl_8
                            else:
                                lcl_8 = mk_tuple(ETup, rbnf_tmp_1)
                                rbnf_tmp_1_ = lcl_8
                                lcl_8 = rbnf_named_lr_loop_atom(rbnf_tmp_1_, builtin_state, builtin_tokens)
                                lcl_7 = lcl_8
                            lcl_6 = lcl_7
                        lcl_4 = lcl_6
                    elif (lcl_5 == 30):
                        lcl_6 = rbnf_named_parse_rbnfmacro_7(builtin_state, builtin_tokens)
                        rbnf_named__check_1 = lcl_6
                        lcl_6 = rbnf_named__check_1[0]
                        lcl_6 = (lcl_6 == False)
                        if lcl_6:
                            lcl_6 = rbnf_named__check_1
                        else:
                            lcl_7 = rbnf_named__check_1[1]
                            rbnf_tmp_1 = lcl_7
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 10):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_7 = _rbnf_cur_token
                            rbnf_tmp_2 = lcl_7
                            lcl_7 = (rbnf_tmp_2 is None)
                            if lcl_7:
                                lcl_8 = builtin_tokens.offset
                                lcl_8 = (lcl_8, 'quote ) not match')
                                lcl_8 = builtin_cons(lcl_8, builtin_nil)
                                lcl_8 = (False, lcl_8)
                                lcl_7 = lcl_8
                            else:
                                lcl_8 = mk_tuple(ETup, rbnf_tmp_1)
                                rbnf_tmp_1_ = lcl_8
                                lcl_8 = rbnf_named_lr_loop_atom(rbnf_tmp_1_, builtin_state, builtin_tokens)
                                lcl_7 = lcl_8
                            lcl_6 = lcl_7
                        lcl_4 = lcl_6
                    elif (lcl_5 == 22):
                        lcl_6 = rbnf_named_parse_rbnfmacro_7(builtin_state, builtin_tokens)
                        rbnf_named__check_1 = lcl_6
                        lcl_6 = rbnf_named__check_1[0]
                        lcl_6 = (lcl_6 == False)
                        if lcl_6:
                            lcl_6 = rbnf_named__check_1
                        else:
                            lcl_7 = rbnf_named__check_1[1]
                            rbnf_tmp_1 = lcl_7
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 10):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_7 = _rbnf_cur_token
                            rbnf_tmp_2 = lcl_7
                            lcl_7 = (rbnf_tmp_2 is None)
                            if lcl_7:
                                lcl_8 = builtin_tokens.offset
                                lcl_8 = (lcl_8, 'quote ) not match')
                                lcl_8 = builtin_cons(lcl_8, builtin_nil)
                                lcl_8 = (False, lcl_8)
                                lcl_7 = lcl_8
                            else:
                                lcl_8 = mk_tuple(ETup, rbnf_tmp_1)
                                rbnf_tmp_1_ = lcl_8
                                lcl_8 = rbnf_named_lr_loop_atom(rbnf_tmp_1_, builtin_state, builtin_tokens)
                                lcl_7 = lcl_8
                            lcl_6 = lcl_7
                        lcl_4 = lcl_6
                    elif (lcl_5 == 16):
                        lcl_6 = rbnf_named_parse_rbnfmacro_7(builtin_state, builtin_tokens)
                        rbnf_named__check_1 = lcl_6
                        lcl_6 = rbnf_named__check_1[0]
                        lcl_6 = (lcl_6 == False)
                        if lcl_6:
                            lcl_6 = rbnf_named__check_1
                        else:
                            lcl_7 = rbnf_named__check_1[1]
                            rbnf_tmp_1 = lcl_7
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 10):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_7 = _rbnf_cur_token
                            rbnf_tmp_2 = lcl_7
                            lcl_7 = (rbnf_tmp_2 is None)
                            if lcl_7:
                                lcl_8 = builtin_tokens.offset
                                lcl_8 = (lcl_8, 'quote ) not match')
                                lcl_8 = builtin_cons(lcl_8, builtin_nil)
                                lcl_8 = (False, lcl_8)
                                lcl_7 = lcl_8
                            else:
                                lcl_8 = mk_tuple(ETup, rbnf_tmp_1)
                                rbnf_tmp_1_ = lcl_8
                                lcl_8 = rbnf_named_lr_loop_atom(rbnf_tmp_1_, builtin_state, builtin_tokens)
                                lcl_7 = lcl_8
                            lcl_6 = lcl_7
                        lcl_4 = lcl_6
                    elif (lcl_5 == 10):
                        _rbnf_old_offset = builtin_tokens.offset
                        _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                        builtin_tokens.offset = (_rbnf_old_offset + 1)
                        lcl_6 = _rbnf_cur_token
                        rbnf_tmp_1 = lcl_6
                        lcl_6 = Pos(rbnf_tmp_0)
                        lcl_7 = []
                        lcl_7 = ETup(lcl_7)
                        lcl_6 = ELoc(lcl_6, lcl_7)
                        rbnf_tmp_1_ = lcl_6
                        lcl_6 = rbnf_named_lr_loop_atom(rbnf_tmp_1_, builtin_state, builtin_tokens)
                        lcl_4 = lcl_6
                    elif (lcl_5 == 9):
                        lcl_6 = rbnf_named_parse_rbnfmacro_7(builtin_state, builtin_tokens)
                        rbnf_named__check_1 = lcl_6
                        lcl_6 = rbnf_named__check_1[0]
                        lcl_6 = (lcl_6 == False)
                        if lcl_6:
                            lcl_6 = rbnf_named__check_1
                        else:
                            lcl_7 = rbnf_named__check_1[1]
                            rbnf_tmp_1 = lcl_7
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 10):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_7 = _rbnf_cur_token
                            rbnf_tmp_2 = lcl_7
                            lcl_7 = (rbnf_tmp_2 is None)
                            if lcl_7:
                                lcl_8 = builtin_tokens.offset
                                lcl_8 = (lcl_8, 'quote ) not match')
                                lcl_8 = builtin_cons(lcl_8, builtin_nil)
                                lcl_8 = (False, lcl_8)
                                lcl_7 = lcl_8
                            else:
                                lcl_8 = mk_tuple(ETup, rbnf_tmp_1)
                                rbnf_tmp_1_ = lcl_8
                                lcl_8 = rbnf_named_lr_loop_atom(rbnf_tmp_1_, builtin_state, builtin_tokens)
                                lcl_7 = lcl_8
                            lcl_6 = lcl_7
                        lcl_4 = lcl_6
                    elif (lcl_5 == 5):
                        lcl_6 = rbnf_named_parse_rbnfmacro_7(builtin_state, builtin_tokens)
                        rbnf_named__check_1 = lcl_6
                        lcl_6 = rbnf_named__check_1[0]
                        lcl_6 = (lcl_6 == False)
                        if lcl_6:
                            lcl_6 = rbnf_named__check_1
                        else:
                            lcl_7 = rbnf_named__check_1[1]
                            rbnf_tmp_1 = lcl_7
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 10):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_7 = _rbnf_cur_token
                            rbnf_tmp_2 = lcl_7
                            lcl_7 = (rbnf_tmp_2 is None)
                            if lcl_7:
                                lcl_8 = builtin_tokens.offset
                                lcl_8 = (lcl_8, 'quote ) not match')
                                lcl_8 = builtin_cons(lcl_8, builtin_nil)
                                lcl_8 = (False, lcl_8)
                                lcl_7 = lcl_8
                            else:
                                lcl_8 = mk_tuple(ETup, rbnf_tmp_1)
                                rbnf_tmp_1_ = lcl_8
                                lcl_8 = rbnf_named_lr_loop_atom(rbnf_tmp_1_, builtin_state, builtin_tokens)
                                lcl_7 = lcl_8
                            lcl_6 = lcl_7
                        lcl_4 = lcl_6
                    elif (lcl_5 == 32):
                        lcl_6 = rbnf_named_parse_rbnfmacro_7(builtin_state, builtin_tokens)
                        rbnf_named__check_1 = lcl_6
                        lcl_6 = rbnf_named__check_1[0]
                        lcl_6 = (lcl_6 == False)
                        if lcl_6:
                            lcl_6 = rbnf_named__check_1
                        else:
                            lcl_7 = rbnf_named__check_1[1]
                            rbnf_tmp_1 = lcl_7
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 10):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_7 = _rbnf_cur_token
                            rbnf_tmp_2 = lcl_7
                            lcl_7 = (rbnf_tmp_2 is None)
                            if lcl_7:
                                lcl_8 = builtin_tokens.offset
                                lcl_8 = (lcl_8, 'quote ) not match')
                                lcl_8 = builtin_cons(lcl_8, builtin_nil)
                                lcl_8 = (False, lcl_8)
                                lcl_7 = lcl_8
                            else:
                                lcl_8 = mk_tuple(ETup, rbnf_tmp_1)
                                rbnf_tmp_1_ = lcl_8
                                lcl_8 = rbnf_named_lr_loop_atom(rbnf_tmp_1_, builtin_state, builtin_tokens)
                                lcl_7 = lcl_8
                            lcl_6 = lcl_7
                        lcl_4 = lcl_6
                    elif (lcl_5 == 33):
                        lcl_6 = rbnf_named_parse_rbnfmacro_7(builtin_state, builtin_tokens)
                        rbnf_named__check_1 = lcl_6
                        lcl_6 = rbnf_named__check_1[0]
                        lcl_6 = (lcl_6 == False)
                        if lcl_6:
                            lcl_6 = rbnf_named__check_1
                        else:
                            lcl_7 = rbnf_named__check_1[1]
                            rbnf_tmp_1 = lcl_7
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 10):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_7 = _rbnf_cur_token
                            rbnf_tmp_2 = lcl_7
                            lcl_7 = (rbnf_tmp_2 is None)
                            if lcl_7:
                                lcl_8 = builtin_tokens.offset
                                lcl_8 = (lcl_8, 'quote ) not match')
                                lcl_8 = builtin_cons(lcl_8, builtin_nil)
                                lcl_8 = (False, lcl_8)
                                lcl_7 = lcl_8
                            else:
                                lcl_8 = mk_tuple(ETup, rbnf_tmp_1)
                                rbnf_tmp_1_ = lcl_8
                                lcl_8 = rbnf_named_lr_loop_atom(rbnf_tmp_1_, builtin_state, builtin_tokens)
                                lcl_7 = lcl_8
                            lcl_6 = lcl_7
                        lcl_4 = lcl_6
                    else:
                        lcl_6 = (rbnf_named__off_1, 'atom lookahead failed')
                        lcl_6 = builtin_cons(lcl_6, builtin_nil)
                        lcl_6 = (False, lcl_6)
                        lcl_4 = lcl_6
                    lcl_3 = lcl_4
                else:
                    lcl_4 = (rbnf_named__off_1, 'atom got EOF')
                    lcl_4 = builtin_cons(lcl_4, builtin_nil)
                    lcl_4 = (False, lcl_4)
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 5):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = rbnf_tmp_0.value
                lcl_3 = EVar(lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_3 = rbnf_named_lr_loop_atom(rbnf_tmp_1_, builtin_state, builtin_tokens)
                lcl_1 = lcl_3
            elif (lcl_2 == 32):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = rbnf_tmp_0.value
                lcl_3 = int(lcl_3)
                lcl_3 = I64(lcl_3)
                lcl_3 = EVal(lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_3 = rbnf_named_lr_loop_atom(rbnf_tmp_1_, builtin_state, builtin_tokens)
                lcl_1 = lcl_3
            elif (lcl_2 == 33):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = rbnf_tmp_0.value
                lcl_3 = float(lcl_3)
                lcl_3 = F64(lcl_3)
                lcl_3 = EVal(lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_3 = rbnf_named_lr_loop_atom(rbnf_tmp_1_, builtin_state, builtin_tokens)
                lcl_1 = lcl_3
            else:
                lcl_3 = (rbnf_named__off_0, 'atom lookahead failed')
                lcl_3 = builtin_cons(lcl_3, builtin_nil)
                lcl_3 = (False, lcl_3)
                lcl_1 = lcl_3
            lcl_0 = lcl_1
        else:
            lcl_1 = (rbnf_named__off_0, 'atom got EOF')
            lcl_1 = builtin_cons(lcl_1, builtin_nil)
            lcl_1 = (False, lcl_1)
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_call(builtin_state, builtin_tokens):
        lcl_0 = rbnf_named_parse_atom(builtin_state, builtin_tokens)
        rbnf_named__check_0 = lcl_0
        lcl_0 = rbnf_named__check_0[0]
        lcl_0 = (lcl_0 == False)
        if lcl_0:
            lcl_0 = rbnf_named__check_0
        else:
            lcl_1 = rbnf_named__check_0[1]
            rbnf_tmp_0 = lcl_1
            rbnf_tmp_1_ = rbnf_tmp_0
            lcl_1 = rbnf_named_lr_loop_call(rbnf_tmp_1_, builtin_state, builtin_tokens)
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_calltype(builtin_state, builtin_tokens):
        lcl_0 = rbnf_named_parse_type(builtin_state, builtin_tokens)
        rbnf_named__check_0 = lcl_0
        lcl_0 = rbnf_named__check_0[0]
        lcl_0 = (lcl_0 == False)
        if lcl_0:
            lcl_0 = rbnf_named__check_0
        else:
            lcl_1 = rbnf_named__check_0[1]
            rbnf_tmp_0 = lcl_1
            rbnf_tmp_1_ = rbnf_tmp_0
            lcl_1 = rbnf_named_lr_loop_calltype(rbnf_tmp_1_, builtin_state, builtin_tokens)
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_declaration(builtin_state, builtin_tokens):
        lcl_0 = builtin_tokens.offset
        rbnf_named__off_0 = lcl_0
        try:
            builtin_tokens.array[(builtin_tokens.offset + 0)]
            _rbnf_peek_tmp = True
        except IndexError:
            _rbnf_peek_tmp = False
        lcl_0 = _rbnf_peek_tmp
        if lcl_0:
            lcl_2 = builtin_tokens.array[(builtin_tokens.offset + 0)]
            lcl_2 = lcl_2.idint
            if (lcl_2 == 19):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                try:
                    _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                    if (_rbnf_cur_token.idint is 5):
                        builtin_tokens.offset += 1
                    else:
                        _rbnf_cur_token = None
                except IndexError:
                    _rbnf_cur_token = None
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_1 = lcl_3
                lcl_3 = (rbnf_tmp_1 is None)
                if lcl_3:
                    lcl_4 = builtin_tokens.offset
                    lcl_4 = (lcl_4, 'name not match')
                    lcl_4 = builtin_cons(lcl_4, builtin_nil)
                    lcl_4 = (False, lcl_4)
                    lcl_3 = lcl_4
                else:
                    try:
                        _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                        if (_rbnf_cur_token.idint is 17):
                            builtin_tokens.offset += 1
                        else:
                            _rbnf_cur_token = None
                    except IndexError:
                        _rbnf_cur_token = None
                    lcl_4 = _rbnf_cur_token
                    rbnf_tmp_2 = lcl_4
                    lcl_4 = (rbnf_tmp_2 is None)
                    if lcl_4:
                        lcl_5 = builtin_tokens.offset
                        lcl_5 = (lcl_5, 'quote : not match')
                        lcl_5 = builtin_cons(lcl_5, builtin_nil)
                        lcl_5 = (False, lcl_5)
                        lcl_4 = lcl_5
                    else:
                        lcl_5 = rbnf_named_parse_toptype(builtin_state, builtin_tokens)
                        rbnf_named__check_3 = lcl_5
                        lcl_5 = rbnf_named__check_3[0]
                        lcl_5 = (lcl_5 == False)
                        if lcl_5:
                            lcl_5 = rbnf_named__check_3
                        else:
                            lcl_6 = rbnf_named__check_3[1]
                            rbnf_tmp_3 = lcl_6
                            lcl_6 = []
                            lcl_7 = Pos(rbnf_tmp_0)
                            lcl_7 = DLoc(lcl_7)
                            _rbnf_immediate_lst = lcl_6
                            _rbnf_immediate_lst.append(lcl_7)
                            lcl_6 = _rbnf_immediate_lst
                            lcl_7 = rbnf_tmp_1.value
                            lcl_7 = DAnn(lcl_7, rbnf_tmp_3)
                            _rbnf_immediate_lst = lcl_6
                            _rbnf_immediate_lst.append(lcl_7)
                            lcl_6 = _rbnf_immediate_lst
                            rbnf_tmp_1_ = lcl_6
                            lcl_6 = (True, rbnf_tmp_1_)
                            lcl_5 = lcl_6
                        lcl_4 = lcl_5
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 24):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = rbnf_named_parse_type_head(builtin_state, builtin_tokens)
                rbnf_named__check_1 = lcl_3
                lcl_3 = rbnf_named__check_1[0]
                lcl_3 = (lcl_3 == False)
                if lcl_3:
                    lcl_3 = rbnf_named__check_1
                else:
                    lcl_4 = rbnf_named__check_1[1]
                    rbnf_tmp_1 = lcl_4
                    try:
                        _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                        if (_rbnf_cur_token.idint is 21):
                            builtin_tokens.offset += 1
                        else:
                            _rbnf_cur_token = None
                    except IndexError:
                        _rbnf_cur_token = None
                    lcl_4 = _rbnf_cur_token
                    rbnf_tmp_2 = lcl_4
                    lcl_4 = (rbnf_tmp_2 is None)
                    if lcl_4:
                        lcl_5 = builtin_tokens.offset
                        lcl_5 = (lcl_5, 'quote = not match')
                        lcl_5 = builtin_cons(lcl_5, builtin_nil)
                        lcl_5 = (False, lcl_5)
                        lcl_4 = lcl_5
                    else:
                        lcl_5 = builtin_tokens.offset
                        rbnf_named__off_2 = lcl_5
                        try:
                            builtin_tokens.array[(builtin_tokens.offset + 0)]
                            _rbnf_peek_tmp = True
                        except IndexError:
                            _rbnf_peek_tmp = False
                        lcl_5 = _rbnf_peek_tmp
                        if lcl_5:
                            lcl_7 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                            lcl_7 = lcl_7.idint
                            if (lcl_7 == 18):
                                lcl_8 = rbnf_named_parse_variant_def(builtin_state, builtin_tokens)
                                rbnf_named__check_3 = lcl_8
                                lcl_8 = rbnf_named__check_3[0]
                                lcl_8 = (lcl_8 == False)
                                if lcl_8:
                                    lcl_8 = rbnf_named__check_3
                                else:
                                    lcl_9 = rbnf_named__check_3[1]
                                    rbnf_tmp_3 = lcl_9
                                    lcl_9 = mk_type_def(rbnf_tmp_1, rbnf_tmp_3)
                                    rbnf_tmp_1_ = lcl_9
                                    lcl_9 = (True, rbnf_tmp_1_)
                                    lcl_8 = lcl_9
                                lcl_6 = lcl_8
                            elif (lcl_7 == 9):
                                lcl_8 = rbnf_named_parse_record_def(builtin_state, builtin_tokens)
                                rbnf_named__check_3 = lcl_8
                                lcl_8 = rbnf_named__check_3[0]
                                lcl_8 = (lcl_8 == False)
                                if lcl_8:
                                    lcl_8 = rbnf_named__check_3
                                else:
                                    lcl_9 = rbnf_named__check_3[1]
                                    rbnf_tmp_3 = lcl_9
                                    lcl_9 = mk_type_def(rbnf_tmp_1, rbnf_tmp_3)
                                    rbnf_tmp_1_ = lcl_9
                                    lcl_9 = (True, rbnf_tmp_1_)
                                    lcl_8 = lcl_9
                                lcl_6 = lcl_8
                            else:
                                lcl_8 = (rbnf_named__off_2, 'declaration lookahead failed')
                                lcl_8 = builtin_cons(lcl_8, builtin_nil)
                                lcl_8 = (False, lcl_8)
                                lcl_6 = lcl_8
                            lcl_5 = lcl_6
                        else:
                            lcl_6 = (rbnf_named__off_2, 'declaration got EOF')
                            lcl_6 = builtin_cons(lcl_6, builtin_nil)
                            lcl_6 = (False, lcl_6)
                            lcl_5 = lcl_6
                        lcl_4 = lcl_5
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 23):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = rbnf_named_parse_expr(builtin_state, builtin_tokens)
                rbnf_named__check_1 = lcl_3
                lcl_3 = rbnf_named__check_1[0]
                lcl_3 = (lcl_3 == False)
                if lcl_3:
                    lcl_3 = rbnf_named__check_1
                else:
                    lcl_4 = rbnf_named__check_1[1]
                    rbnf_tmp_1 = lcl_4
                    lcl_4 = []
                    lcl_5 = Pos(rbnf_tmp_0)
                    lcl_5 = DLoc(lcl_5)
                    _rbnf_immediate_lst = lcl_4
                    _rbnf_immediate_lst.append(lcl_5)
                    lcl_4 = _rbnf_immediate_lst
                    lcl_5 = DOpen(rbnf_tmp_1)
                    _rbnf_immediate_lst = lcl_4
                    _rbnf_immediate_lst.append(lcl_5)
                    lcl_4 = _rbnf_immediate_lst
                    rbnf_tmp_1_ = lcl_4
                    lcl_4 = (True, rbnf_tmp_1_)
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 20):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                try:
                    _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                    if (_rbnf_cur_token.idint is 5):
                        builtin_tokens.offset += 1
                    else:
                        _rbnf_cur_token = None
                except IndexError:
                    _rbnf_cur_token = None
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_1 = lcl_3
                lcl_3 = (rbnf_tmp_1 is None)
                if lcl_3:
                    lcl_4 = builtin_tokens.offset
                    lcl_4 = (lcl_4, 'name not match')
                    lcl_4 = builtin_cons(lcl_4, builtin_nil)
                    lcl_4 = (False, lcl_4)
                    lcl_3 = lcl_4
                else:
                    try:
                        _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                        if (_rbnf_cur_token.idint is 21):
                            builtin_tokens.offset += 1
                        else:
                            _rbnf_cur_token = None
                    except IndexError:
                        _rbnf_cur_token = None
                    lcl_4 = _rbnf_cur_token
                    rbnf_tmp_2 = lcl_4
                    lcl_4 = (rbnf_tmp_2 is None)
                    if lcl_4:
                        lcl_5 = builtin_tokens.offset
                        lcl_5 = (lcl_5, 'quote = not match')
                        lcl_5 = builtin_cons(lcl_5, builtin_nil)
                        lcl_5 = (False, lcl_5)
                        lcl_4 = lcl_5
                    else:
                        lcl_5 = rbnf_named_parse_expr(builtin_state, builtin_tokens)
                        rbnf_named__check_3 = lcl_5
                        lcl_5 = rbnf_named__check_3[0]
                        lcl_5 = (lcl_5 == False)
                        if lcl_5:
                            lcl_5 = rbnf_named__check_3
                        else:
                            lcl_6 = rbnf_named__check_3[1]
                            rbnf_tmp_3 = lcl_6
                            lcl_6 = []
                            lcl_7 = Pos(rbnf_tmp_0)
                            lcl_7 = DLoc(lcl_7)
                            _rbnf_immediate_lst = lcl_6
                            _rbnf_immediate_lst.append(lcl_7)
                            lcl_6 = _rbnf_immediate_lst
                            lcl_7 = rbnf_tmp_1.value
                            lcl_7 = DBind(lcl_7, rbnf_tmp_3)
                            _rbnf_immediate_lst = lcl_6
                            _rbnf_immediate_lst.append(lcl_7)
                            lcl_6 = _rbnf_immediate_lst
                            rbnf_tmp_1_ = lcl_6
                            lcl_6 = (True, rbnf_tmp_1_)
                            lcl_5 = lcl_6
                        lcl_4 = lcl_5
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 22):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = rbnf_named_parse_nameStr(builtin_state, builtin_tokens)
                rbnf_named__check_1 = lcl_3
                lcl_3 = rbnf_named__check_1[0]
                lcl_3 = (lcl_3 == False)
                if lcl_3:
                    lcl_3 = rbnf_named__check_1
                else:
                    lcl_4 = rbnf_named__check_1[1]
                    rbnf_tmp_1 = lcl_4
                    try:
                        _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                        if (_rbnf_cur_token.idint is 4):
                            builtin_tokens.offset += 1
                        else:
                            _rbnf_cur_token = None
                    except IndexError:
                        _rbnf_cur_token = None
                    lcl_4 = _rbnf_cur_token
                    rbnf_tmp_2 = lcl_4
                    lcl_4 = (rbnf_tmp_2 is None)
                    if lcl_4:
                        lcl_5 = builtin_tokens.offset
                        lcl_5 = (lcl_5, 'quote as not match')
                        lcl_5 = builtin_cons(lcl_5, builtin_nil)
                        lcl_5 = (False, lcl_5)
                        lcl_4 = lcl_5
                    else:
                        lcl_5 = rbnf_named_parse_nameStr(builtin_state, builtin_tokens)
                        rbnf_named__check_3 = lcl_5
                        lcl_5 = rbnf_named__check_3[0]
                        lcl_5 = (lcl_5 == False)
                        if lcl_5:
                            lcl_5 = rbnf_named__check_3
                        else:
                            lcl_6 = rbnf_named__check_3[1]
                            rbnf_tmp_3 = lcl_6
                            lcl_6 = []
                            lcl_7 = Pos(rbnf_tmp_0)
                            lcl_7 = DLoc(lcl_7)
                            _rbnf_immediate_lst = lcl_6
                            _rbnf_immediate_lst.append(lcl_7)
                            lcl_6 = _rbnf_immediate_lst
                            lcl_7 = DQuery(rbnf_tmp_3, rbnf_tmp_1)
                            _rbnf_immediate_lst = lcl_6
                            _rbnf_immediate_lst.append(lcl_7)
                            lcl_6 = _rbnf_immediate_lst
                            rbnf_tmp_1_ = lcl_6
                            lcl_6 = (True, rbnf_tmp_1_)
                            lcl_5 = lcl_6
                        lcl_4 = lcl_5
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            else:
                lcl_3 = (rbnf_named__off_0, 'declaration lookahead failed')
                lcl_3 = builtin_cons(lcl_3, builtin_nil)
                lcl_3 = (False, lcl_3)
                lcl_1 = lcl_3
            lcl_0 = lcl_1
        else:
            lcl_1 = (rbnf_named__off_0, 'declaration got EOF')
            lcl_1 = builtin_cons(lcl_1, builtin_nil)
            lcl_1 = (False, lcl_1)
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_expr(builtin_state, builtin_tokens):
        lcl_0 = builtin_tokens.offset
        rbnf_named__off_0 = lcl_0
        try:
            builtin_tokens.array[(builtin_tokens.offset + 0)]
            _rbnf_peek_tmp = True
        except IndexError:
            _rbnf_peek_tmp = False
        lcl_0 = _rbnf_peek_tmp
        if lcl_0:
            lcl_2 = builtin_tokens.array[(builtin_tokens.offset + 0)]
            lcl_2 = lcl_2.idint
            if (lcl_2 == 14):
                lcl_3 = rbnf_named_parse_call(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = rbnf_named__check_0[0]
                lcl_3 = (lcl_3 == False)
                if lcl_3:
                    lcl_3 = rbnf_named__check_0
                else:
                    lcl_4 = rbnf_named__check_0[1]
                    rbnf_tmp_0 = lcl_4
                    rbnf_tmp_1_ = rbnf_tmp_0
                    lcl_4 = (True, rbnf_tmp_1_)
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 19):
                lcl_3 = rbnf_named_parse_rbnfmacro_1(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = rbnf_named__check_0[0]
                lcl_3 = (lcl_3 == False)
                if lcl_3:
                    lcl_3 = rbnf_named__check_0
                else:
                    lcl_4 = rbnf_named__check_0[1]
                    rbnf_tmp_0 = lcl_4
                    try:
                        _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                        if (_rbnf_cur_token.idint is 28):
                            builtin_tokens.offset += 1
                        else:
                            _rbnf_cur_token = None
                    except IndexError:
                        _rbnf_cur_token = None
                    lcl_4 = _rbnf_cur_token
                    rbnf_tmp_1 = lcl_4
                    lcl_4 = (rbnf_tmp_1 is None)
                    if lcl_4:
                        lcl_5 = builtin_tokens.offset
                        lcl_5 = (lcl_5, 'quote in not match')
                        lcl_5 = builtin_cons(lcl_5, builtin_nil)
                        lcl_5 = (False, lcl_5)
                        lcl_4 = lcl_5
                    else:
                        lcl_5 = rbnf_named_parse_expr(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_5
                        lcl_5 = rbnf_named__check_2[0]
                        lcl_5 = (lcl_5 == False)
                        if lcl_5:
                            lcl_5 = rbnf_named__check_2
                        else:
                            lcl_6 = rbnf_named__check_2[1]
                            rbnf_tmp_2 = lcl_6
                            lcl_6 = concat(rbnf_tmp_0)
                            lcl_6 = ELet(lcl_6, rbnf_tmp_2)
                            rbnf_tmp_1_ = lcl_6
                            lcl_6 = (True, rbnf_tmp_1_)
                            lcl_5 = lcl_6
                        lcl_4 = lcl_5
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 24):
                lcl_3 = rbnf_named_parse_rbnfmacro_1(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = rbnf_named__check_0[0]
                lcl_3 = (lcl_3 == False)
                if lcl_3:
                    lcl_3 = rbnf_named__check_0
                else:
                    lcl_4 = rbnf_named__check_0[1]
                    rbnf_tmp_0 = lcl_4
                    try:
                        _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                        if (_rbnf_cur_token.idint is 28):
                            builtin_tokens.offset += 1
                        else:
                            _rbnf_cur_token = None
                    except IndexError:
                        _rbnf_cur_token = None
                    lcl_4 = _rbnf_cur_token
                    rbnf_tmp_1 = lcl_4
                    lcl_4 = (rbnf_tmp_1 is None)
                    if lcl_4:
                        lcl_5 = builtin_tokens.offset
                        lcl_5 = (lcl_5, 'quote in not match')
                        lcl_5 = builtin_cons(lcl_5, builtin_nil)
                        lcl_5 = (False, lcl_5)
                        lcl_4 = lcl_5
                    else:
                        lcl_5 = rbnf_named_parse_expr(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_5
                        lcl_5 = rbnf_named__check_2[0]
                        lcl_5 = (lcl_5 == False)
                        if lcl_5:
                            lcl_5 = rbnf_named__check_2
                        else:
                            lcl_6 = rbnf_named__check_2[1]
                            rbnf_tmp_2 = lcl_6
                            lcl_6 = concat(rbnf_tmp_0)
                            lcl_6 = ELet(lcl_6, rbnf_tmp_2)
                            rbnf_tmp_1_ = lcl_6
                            lcl_6 = (True, rbnf_tmp_1_)
                            lcl_5 = lcl_6
                        lcl_4 = lcl_5
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 34):
                lcl_3 = rbnf_named_parse_call(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = rbnf_named__check_0[0]
                lcl_3 = (lcl_3 == False)
                if lcl_3:
                    lcl_3 = rbnf_named__check_0
                else:
                    lcl_4 = rbnf_named__check_0[1]
                    rbnf_tmp_0 = lcl_4
                    rbnf_tmp_1_ = rbnf_tmp_0
                    lcl_4 = (True, rbnf_tmp_1_)
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 23):
                lcl_3 = rbnf_named_parse_rbnfmacro_1(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = rbnf_named__check_0[0]
                lcl_3 = (lcl_3 == False)
                if lcl_3:
                    lcl_3 = rbnf_named__check_0
                else:
                    lcl_4 = rbnf_named__check_0[1]
                    rbnf_tmp_0 = lcl_4
                    try:
                        _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                        if (_rbnf_cur_token.idint is 28):
                            builtin_tokens.offset += 1
                        else:
                            _rbnf_cur_token = None
                    except IndexError:
                        _rbnf_cur_token = None
                    lcl_4 = _rbnf_cur_token
                    rbnf_tmp_1 = lcl_4
                    lcl_4 = (rbnf_tmp_1 is None)
                    if lcl_4:
                        lcl_5 = builtin_tokens.offset
                        lcl_5 = (lcl_5, 'quote in not match')
                        lcl_5 = builtin_cons(lcl_5, builtin_nil)
                        lcl_5 = (False, lcl_5)
                        lcl_4 = lcl_5
                    else:
                        lcl_5 = rbnf_named_parse_expr(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_5
                        lcl_5 = rbnf_named__check_2[0]
                        lcl_5 = (lcl_5 == False)
                        if lcl_5:
                            lcl_5 = rbnf_named__check_2
                        else:
                            lcl_6 = rbnf_named__check_2[1]
                            rbnf_tmp_2 = lcl_6
                            lcl_6 = concat(rbnf_tmp_0)
                            lcl_6 = ELet(lcl_6, rbnf_tmp_2)
                            rbnf_tmp_1_ = lcl_6
                            lcl_6 = (True, rbnf_tmp_1_)
                            lcl_5 = lcl_6
                        lcl_4 = lcl_5
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 20):
                lcl_3 = rbnf_named_parse_rbnfmacro_1(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = rbnf_named__check_0[0]
                lcl_3 = (lcl_3 == False)
                if lcl_3:
                    lcl_3 = rbnf_named__check_0
                else:
                    lcl_4 = rbnf_named__check_0[1]
                    rbnf_tmp_0 = lcl_4
                    try:
                        _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                        if (_rbnf_cur_token.idint is 28):
                            builtin_tokens.offset += 1
                        else:
                            _rbnf_cur_token = None
                    except IndexError:
                        _rbnf_cur_token = None
                    lcl_4 = _rbnf_cur_token
                    rbnf_tmp_1 = lcl_4
                    lcl_4 = (rbnf_tmp_1 is None)
                    if lcl_4:
                        lcl_5 = builtin_tokens.offset
                        lcl_5 = (lcl_5, 'quote in not match')
                        lcl_5 = builtin_cons(lcl_5, builtin_nil)
                        lcl_5 = (False, lcl_5)
                        lcl_4 = lcl_5
                    else:
                        lcl_5 = rbnf_named_parse_expr(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_5
                        lcl_5 = rbnf_named__check_2[0]
                        lcl_5 = (lcl_5 == False)
                        if lcl_5:
                            lcl_5 = rbnf_named__check_2
                        else:
                            lcl_6 = rbnf_named__check_2[1]
                            rbnf_tmp_2 = lcl_6
                            lcl_6 = concat(rbnf_tmp_0)
                            lcl_6 = ELet(lcl_6, rbnf_tmp_2)
                            rbnf_tmp_1_ = lcl_6
                            lcl_6 = (True, rbnf_tmp_1_)
                            lcl_5 = lcl_6
                        lcl_4 = lcl_5
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 25):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = rbnf_named_parse_expr(builtin_state, builtin_tokens)
                rbnf_named__check_1 = lcl_3
                lcl_3 = rbnf_named__check_1[0]
                lcl_3 = (lcl_3 == False)
                if lcl_3:
                    lcl_3 = rbnf_named__check_1
                else:
                    lcl_4 = rbnf_named__check_1[1]
                    rbnf_tmp_1 = lcl_4
                    try:
                        _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                        if (_rbnf_cur_token.idint is 26):
                            builtin_tokens.offset += 1
                        else:
                            _rbnf_cur_token = None
                    except IndexError:
                        _rbnf_cur_token = None
                    lcl_4 = _rbnf_cur_token
                    rbnf_tmp_2 = lcl_4
                    lcl_4 = (rbnf_tmp_2 is None)
                    if lcl_4:
                        lcl_5 = builtin_tokens.offset
                        lcl_5 = (lcl_5, 'quote then not match')
                        lcl_5 = builtin_cons(lcl_5, builtin_nil)
                        lcl_5 = (False, lcl_5)
                        lcl_4 = lcl_5
                    else:
                        lcl_5 = rbnf_named_parse_expr(builtin_state, builtin_tokens)
                        rbnf_named__check_3 = lcl_5
                        lcl_5 = rbnf_named__check_3[0]
                        lcl_5 = (lcl_5 == False)
                        if lcl_5:
                            lcl_5 = rbnf_named__check_3
                        else:
                            lcl_6 = rbnf_named__check_3[1]
                            rbnf_tmp_3 = lcl_6
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 27):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_6 = _rbnf_cur_token
                            rbnf_tmp_4 = lcl_6
                            lcl_6 = (rbnf_tmp_4 is None)
                            if lcl_6:
                                lcl_7 = builtin_tokens.offset
                                lcl_7 = (lcl_7, 'quote else not match')
                                lcl_7 = builtin_cons(lcl_7, builtin_nil)
                                lcl_7 = (False, lcl_7)
                                lcl_6 = lcl_7
                            else:
                                lcl_7 = rbnf_named_parse_expr(builtin_state, builtin_tokens)
                                rbnf_named__check_5 = lcl_7
                                lcl_7 = rbnf_named__check_5[0]
                                lcl_7 = (lcl_7 == False)
                                if lcl_7:
                                    lcl_7 = rbnf_named__check_5
                                else:
                                    lcl_8 = rbnf_named__check_5[1]
                                    rbnf_tmp_5 = lcl_8
                                    lcl_8 = EITE(rbnf_tmp_1, rbnf_tmp_3, rbnf_tmp_5)
                                    rbnf_tmp_1_ = lcl_8
                                    lcl_8 = (True, rbnf_tmp_1_)
                                    lcl_7 = lcl_8
                                lcl_6 = lcl_7
                            lcl_5 = lcl_6
                        lcl_4 = lcl_5
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 29):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                try:
                    _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                    if (_rbnf_cur_token.idint is 5):
                        builtin_tokens.offset += 1
                    else:
                        _rbnf_cur_token = None
                except IndexError:
                    _rbnf_cur_token = None
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_1 = lcl_3
                lcl_3 = (rbnf_tmp_1 is None)
                if lcl_3:
                    lcl_4 = builtin_tokens.offset
                    lcl_4 = (lcl_4, 'name not match')
                    lcl_4 = builtin_cons(lcl_4, builtin_nil)
                    lcl_4 = (False, lcl_4)
                    lcl_3 = lcl_4
                else:
                    try:
                        _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                        if (_rbnf_cur_token.idint is 8):
                            builtin_tokens.offset += 1
                        else:
                            _rbnf_cur_token = None
                    except IndexError:
                        _rbnf_cur_token = None
                    lcl_4 = _rbnf_cur_token
                    rbnf_tmp_2 = lcl_4
                    lcl_4 = (rbnf_tmp_2 is None)
                    if lcl_4:
                        lcl_5 = builtin_tokens.offset
                        lcl_5 = (lcl_5, 'quote -> not match')
                        lcl_5 = builtin_cons(lcl_5, builtin_nil)
                        lcl_5 = (False, lcl_5)
                        lcl_4 = lcl_5
                    else:
                        lcl_5 = rbnf_named_parse_expr(builtin_state, builtin_tokens)
                        rbnf_named__check_3 = lcl_5
                        lcl_5 = rbnf_named__check_3[0]
                        lcl_5 = (lcl_5 == False)
                        if lcl_5:
                            lcl_5 = rbnf_named__check_3
                        else:
                            lcl_6 = rbnf_named__check_3[1]
                            rbnf_tmp_3 = lcl_6
                            lcl_6 = rbnf_tmp_1.value
                            lcl_6 = EFun(lcl_6, rbnf_tmp_3)
                            rbnf_tmp_1_ = lcl_6
                            lcl_6 = (True, rbnf_tmp_1_)
                            lcl_5 = lcl_6
                        lcl_4 = lcl_5
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 35):
                lcl_3 = rbnf_named_parse_call(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = rbnf_named__check_0[0]
                lcl_3 = (lcl_3 == False)
                if lcl_3:
                    lcl_3 = rbnf_named__check_0
                else:
                    lcl_4 = rbnf_named__check_0[1]
                    rbnf_tmp_0 = lcl_4
                    rbnf_tmp_1_ = rbnf_tmp_0
                    lcl_4 = (True, rbnf_tmp_1_)
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 31):
                lcl_3 = rbnf_named_parse_call(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = rbnf_named__check_0[0]
                lcl_3 = (lcl_3 == False)
                if lcl_3:
                    lcl_3 = rbnf_named__check_0
                else:
                    lcl_4 = rbnf_named__check_0[1]
                    rbnf_tmp_0 = lcl_4
                    rbnf_tmp_1_ = rbnf_tmp_0
                    lcl_4 = (True, rbnf_tmp_1_)
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 30):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = rbnf_named_parse_expr(builtin_state, builtin_tokens)
                rbnf_named__check_1 = lcl_3
                lcl_3 = rbnf_named__check_1[0]
                lcl_3 = (lcl_3 == False)
                if lcl_3:
                    lcl_3 = rbnf_named__check_1
                else:
                    lcl_4 = rbnf_named__check_1[1]
                    rbnf_tmp_1 = lcl_4
                    lcl_4 = ECoerce(rbnf_tmp_1)
                    rbnf_tmp_1_ = lcl_4
                    lcl_4 = (True, rbnf_tmp_1_)
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 22):
                lcl_3 = rbnf_named_parse_rbnfmacro_1(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = rbnf_named__check_0[0]
                lcl_3 = (lcl_3 == False)
                if lcl_3:
                    lcl_3 = rbnf_named__check_0
                else:
                    lcl_4 = rbnf_named__check_0[1]
                    rbnf_tmp_0 = lcl_4
                    try:
                        _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                        if (_rbnf_cur_token.idint is 28):
                            builtin_tokens.offset += 1
                        else:
                            _rbnf_cur_token = None
                    except IndexError:
                        _rbnf_cur_token = None
                    lcl_4 = _rbnf_cur_token
                    rbnf_tmp_1 = lcl_4
                    lcl_4 = (rbnf_tmp_1 is None)
                    if lcl_4:
                        lcl_5 = builtin_tokens.offset
                        lcl_5 = (lcl_5, 'quote in not match')
                        lcl_5 = builtin_cons(lcl_5, builtin_nil)
                        lcl_5 = (False, lcl_5)
                        lcl_4 = lcl_5
                    else:
                        lcl_5 = rbnf_named_parse_expr(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_5
                        lcl_5 = rbnf_named__check_2[0]
                        lcl_5 = (lcl_5 == False)
                        if lcl_5:
                            lcl_5 = rbnf_named__check_2
                        else:
                            lcl_6 = rbnf_named__check_2[1]
                            rbnf_tmp_2 = lcl_6
                            lcl_6 = concat(rbnf_tmp_0)
                            lcl_6 = ELet(lcl_6, rbnf_tmp_2)
                            rbnf_tmp_1_ = lcl_6
                            lcl_6 = (True, rbnf_tmp_1_)
                            lcl_5 = lcl_6
                        lcl_4 = lcl_5
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 16):
                lcl_3 = rbnf_named_parse_call(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = rbnf_named__check_0[0]
                lcl_3 = (lcl_3 == False)
                if lcl_3:
                    lcl_3 = rbnf_named__check_0
                else:
                    lcl_4 = rbnf_named__check_0[1]
                    rbnf_tmp_0 = lcl_4
                    rbnf_tmp_1_ = rbnf_tmp_0
                    lcl_4 = (True, rbnf_tmp_1_)
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 9):
                lcl_3 = rbnf_named_parse_call(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = rbnf_named__check_0[0]
                lcl_3 = (lcl_3 == False)
                if lcl_3:
                    lcl_3 = rbnf_named__check_0
                else:
                    lcl_4 = rbnf_named__check_0[1]
                    rbnf_tmp_0 = lcl_4
                    rbnf_tmp_1_ = rbnf_tmp_0
                    lcl_4 = (True, rbnf_tmp_1_)
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 5):
                lcl_3 = rbnf_named_parse_call(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = rbnf_named__check_0[0]
                lcl_3 = (lcl_3 == False)
                if lcl_3:
                    lcl_3 = rbnf_named__check_0
                else:
                    lcl_4 = rbnf_named__check_0[1]
                    rbnf_tmp_0 = lcl_4
                    rbnf_tmp_1_ = rbnf_tmp_0
                    lcl_4 = (True, rbnf_tmp_1_)
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 32):
                lcl_3 = rbnf_named_parse_call(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = rbnf_named__check_0[0]
                lcl_3 = (lcl_3 == False)
                if lcl_3:
                    lcl_3 = rbnf_named__check_0
                else:
                    lcl_4 = rbnf_named__check_0[1]
                    rbnf_tmp_0 = lcl_4
                    rbnf_tmp_1_ = rbnf_tmp_0
                    lcl_4 = (True, rbnf_tmp_1_)
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 33):
                lcl_3 = rbnf_named_parse_call(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = rbnf_named__check_0[0]
                lcl_3 = (lcl_3 == False)
                if lcl_3:
                    lcl_3 = rbnf_named__check_0
                else:
                    lcl_4 = rbnf_named__check_0[1]
                    rbnf_tmp_0 = lcl_4
                    rbnf_tmp_1_ = rbnf_tmp_0
                    lcl_4 = (True, rbnf_tmp_1_)
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            else:
                lcl_3 = (rbnf_named__off_0, 'expr lookahead failed')
                lcl_3 = builtin_cons(lcl_3, builtin_nil)
                lcl_3 = (False, lcl_3)
                lcl_1 = lcl_3
            lcl_0 = lcl_1
        else:
            lcl_1 = (rbnf_named__off_0, 'expr got EOF')
            lcl_1 = builtin_cons(lcl_1, builtin_nil)
            lcl_1 = (False, lcl_1)
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_import(builtin_state, builtin_tokens):
        try:
            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
            if (_rbnf_cur_token.idint is 3):
                builtin_tokens.offset += 1
            else:
                _rbnf_cur_token = None
        except IndexError:
            _rbnf_cur_token = None
        lcl_0 = _rbnf_cur_token
        rbnf_tmp_0 = lcl_0
        lcl_0 = (rbnf_tmp_0 is None)
        if lcl_0:
            lcl_1 = builtin_tokens.offset
            lcl_1 = (lcl_1, 'quote import not match')
            lcl_1 = builtin_cons(lcl_1, builtin_nil)
            lcl_1 = (False, lcl_1)
            lcl_0 = lcl_1
        else:
            lcl_1 = rbnf_named_parse_nameStr(builtin_state, builtin_tokens)
            rbnf_named__check_1 = lcl_1
            lcl_1 = rbnf_named__check_1[0]
            lcl_1 = (lcl_1 == False)
            if lcl_1:
                lcl_1 = rbnf_named__check_1
            else:
                lcl_2 = rbnf_named__check_1[1]
                rbnf_tmp_1 = lcl_2
                lcl_2 = builtin_tokens.offset
                rbnf_named__off_1 = lcl_2
                try:
                    builtin_tokens.array[(builtin_tokens.offset + 0)]
                    _rbnf_peek_tmp = True
                except IndexError:
                    _rbnf_peek_tmp = False
                lcl_2 = _rbnf_peek_tmp
                if lcl_2:
                    lcl_4 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                    lcl_4 = lcl_4.idint
                    if (lcl_4 == 4):
                        _rbnf_old_offset = builtin_tokens.offset
                        _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                        builtin_tokens.offset = (_rbnf_old_offset + 1)
                        lcl_5 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_5
                        lcl_5 = rbnf_named_parse_nameStr(builtin_state, builtin_tokens)
                        rbnf_named__check_3 = lcl_5
                        lcl_5 = rbnf_named__check_3[0]
                        lcl_5 = (lcl_5 == False)
                        if lcl_5:
                            lcl_5 = rbnf_named__check_3
                        else:
                            lcl_6 = rbnf_named__check_3[1]
                            rbnf_tmp_3 = lcl_6
                            lcl_6 = (rbnf_tmp_1, rbnf_tmp_3)
                            rbnf_tmp_1_ = lcl_6
                            lcl_6 = (True, rbnf_tmp_1_)
                            lcl_5 = lcl_6
                        lcl_3 = lcl_5
                    else:
                        lcl_5 = importName(rbnf_tmp_1)
                        lcl_5 = (rbnf_tmp_1, lcl_5)
                        rbnf_tmp_1_ = lcl_5
                        lcl_5 = (True, rbnf_tmp_1_)
                        lcl_3 = lcl_5
                    lcl_2 = lcl_3
                else:
                    lcl_3 = (rbnf_named__off_1, 'import got EOF')
                    lcl_3 = builtin_cons(lcl_3, builtin_nil)
                    lcl_3 = (False, lcl_3)
                    lcl_2 = lcl_3
                lcl_1 = lcl_2
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_nameStr(builtin_state, builtin_tokens):
        try:
            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
            if (_rbnf_cur_token.idint is 5):
                builtin_tokens.offset += 1
            else:
                _rbnf_cur_token = None
        except IndexError:
            _rbnf_cur_token = None
        lcl_0 = _rbnf_cur_token
        rbnf_tmp_0 = lcl_0
        lcl_0 = (rbnf_tmp_0 is None)
        if lcl_0:
            lcl_1 = builtin_tokens.offset
            lcl_1 = (lcl_1, 'name not match')
            lcl_1 = builtin_cons(lcl_1, builtin_nil)
            lcl_1 = (False, lcl_1)
            lcl_0 = lcl_1
        else:
            lcl_1 = rbnf_tmp_0.value
            rbnf_tmp_1_ = lcl_1
            lcl_1 = (True, rbnf_tmp_1_)
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_rbnfmacro_0(builtin_state, builtin_tokens):
        lcl_0 = rbnf_named_parse_import(builtin_state, builtin_tokens)
        rbnf_named__check_0 = lcl_0
        lcl_0 = rbnf_named__check_0[0]
        lcl_0 = (lcl_0 == False)
        if lcl_0:
            lcl_0 = rbnf_named__check_0
        else:
            lcl_1 = rbnf_named__check_0[1]
            rbnf_tmp_0 = lcl_1
            lcl_1 = []
            _rbnf_immediate_lst = lcl_1
            _rbnf_immediate_lst.append(rbnf_tmp_0)
            lcl_1 = _rbnf_immediate_lst
            rbnf_tmp_1_ = lcl_1
            lcl_1 = rbnf_named_lr_loop_rbnfmacro_0(rbnf_tmp_1_, builtin_state, builtin_tokens)
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_rbnfmacro_1(builtin_state, builtin_tokens):
        lcl_0 = rbnf_named_parse_declaration(builtin_state, builtin_tokens)
        rbnf_named__check_0 = lcl_0
        lcl_0 = rbnf_named__check_0[0]
        lcl_0 = (lcl_0 == False)
        if lcl_0:
            lcl_0 = rbnf_named__check_0
        else:
            lcl_1 = rbnf_named__check_0[1]
            rbnf_tmp_0 = lcl_1
            lcl_1 = []
            _rbnf_immediate_lst = lcl_1
            _rbnf_immediate_lst.append(rbnf_tmp_0)
            lcl_1 = _rbnf_immediate_lst
            rbnf_tmp_1_ = lcl_1
            lcl_1 = rbnf_named_lr_loop_rbnfmacro_1(rbnf_tmp_1_, builtin_state, builtin_tokens)
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_rbnfmacro_2(builtin_state, builtin_tokens):
        lcl_0 = rbnf_named_parse_nameStr(builtin_state, builtin_tokens)
        rbnf_named__check_0 = lcl_0
        lcl_0 = rbnf_named__check_0[0]
        lcl_0 = (lcl_0 == False)
        if lcl_0:
            lcl_0 = rbnf_named__check_0
        else:
            lcl_1 = rbnf_named__check_0[1]
            rbnf_tmp_0 = lcl_1
            lcl_1 = []
            _rbnf_immediate_lst = lcl_1
            _rbnf_immediate_lst.append(rbnf_tmp_0)
            lcl_1 = _rbnf_immediate_lst
            rbnf_tmp_1_ = lcl_1
            lcl_1 = rbnf_named_lr_loop_rbnfmacro_2(rbnf_tmp_1_, builtin_state, builtin_tokens)
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_rbnfmacro_3(builtin_state, builtin_tokens):
        lcl_0 = rbnf_named_parse_toptype(builtin_state, builtin_tokens)
        rbnf_named__check_0 = lcl_0
        lcl_0 = rbnf_named__check_0[0]
        lcl_0 = (lcl_0 == False)
        if lcl_0:
            lcl_0 = rbnf_named__check_0
        else:
            lcl_1 = rbnf_named__check_0[1]
            rbnf_tmp_0 = lcl_1
            lcl_1 = []
            _rbnf_immediate_lst = lcl_1
            _rbnf_immediate_lst.append(rbnf_tmp_0)
            lcl_1 = _rbnf_immediate_lst
            rbnf_tmp_1_ = lcl_1
            lcl_1 = rbnf_named_lr_loop_rbnfmacro_3(rbnf_tmp_1_, builtin_state, builtin_tokens)
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_rbnfmacro_4(builtin_state, builtin_tokens):
        lcl_0 = rbnf_named_parse_tvar(builtin_state, builtin_tokens)
        rbnf_named__check_0 = lcl_0
        lcl_0 = rbnf_named__check_0[0]
        lcl_0 = (lcl_0 == False)
        if lcl_0:
            lcl_0 = rbnf_named__check_0
        else:
            lcl_1 = rbnf_named__check_0[1]
            rbnf_tmp_0 = lcl_1
            lcl_1 = []
            _rbnf_immediate_lst = lcl_1
            _rbnf_immediate_lst.append(rbnf_tmp_0)
            lcl_1 = _rbnf_immediate_lst
            rbnf_tmp_1_ = lcl_1
            lcl_1 = rbnf_named_lr_loop_rbnfmacro_4(rbnf_tmp_1_, builtin_state, builtin_tokens)
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_rbnfmacro_5(builtin_state, builtin_tokens):
        lcl_0 = rbnf_named_parse_record_field_def(builtin_state, builtin_tokens)
        rbnf_named__check_0 = lcl_0
        lcl_0 = rbnf_named__check_0[0]
        lcl_0 = (lcl_0 == False)
        if lcl_0:
            lcl_0 = rbnf_named__check_0
        else:
            lcl_1 = rbnf_named__check_0[1]
            rbnf_tmp_0 = lcl_1
            lcl_1 = []
            _rbnf_immediate_lst = lcl_1
            _rbnf_immediate_lst.append(rbnf_tmp_0)
            lcl_1 = _rbnf_immediate_lst
            rbnf_tmp_1_ = lcl_1
            lcl_1 = rbnf_named_lr_loop_rbnfmacro_5(rbnf_tmp_1_, builtin_state, builtin_tokens)
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_rbnfmacro_7(builtin_state, builtin_tokens):
        lcl_0 = rbnf_named_parse_expr(builtin_state, builtin_tokens)
        rbnf_named__check_0 = lcl_0
        lcl_0 = rbnf_named__check_0[0]
        lcl_0 = (lcl_0 == False)
        if lcl_0:
            lcl_0 = rbnf_named__check_0
        else:
            lcl_1 = rbnf_named__check_0[1]
            rbnf_tmp_0 = lcl_1
            lcl_1 = []
            _rbnf_immediate_lst = lcl_1
            _rbnf_immediate_lst.append(rbnf_tmp_0)
            lcl_1 = _rbnf_immediate_lst
            rbnf_tmp_1_ = lcl_1
            lcl_1 = rbnf_named_lr_loop_rbnfmacro_7(rbnf_tmp_1_, builtin_state, builtin_tokens)
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_record_def(builtin_state, builtin_tokens):
        try:
            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
            if (_rbnf_cur_token.idint is 9):
                builtin_tokens.offset += 1
            else:
                _rbnf_cur_token = None
        except IndexError:
            _rbnf_cur_token = None
        lcl_0 = _rbnf_cur_token
        rbnf_tmp_0 = lcl_0
        lcl_0 = (rbnf_tmp_0 is None)
        if lcl_0:
            lcl_1 = builtin_tokens.offset
            lcl_1 = (lcl_1, 'quote ( not match')
            lcl_1 = builtin_cons(lcl_1, builtin_nil)
            lcl_1 = (False, lcl_1)
            lcl_0 = lcl_1
        else:
            lcl_1 = builtin_tokens.offset
            rbnf_named__off_1 = lcl_1
            try:
                builtin_tokens.array[(builtin_tokens.offset + 0)]
                _rbnf_peek_tmp = True
            except IndexError:
                _rbnf_peek_tmp = False
            lcl_1 = _rbnf_peek_tmp
            if lcl_1:
                lcl_3 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                lcl_3 = lcl_3.idint
                if (lcl_3 == 10):
                    _rbnf_old_offset = builtin_tokens.offset
                    _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                    builtin_tokens.offset = (_rbnf_old_offset + 1)
                    lcl_4 = _rbnf_cur_token
                    rbnf_tmp_1 = lcl_4
                    lcl_4 = []
                    lcl_4 = (0, lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_4 = (True, rbnf_tmp_1_)
                    lcl_2 = lcl_4
                elif (lcl_3 == 5):
                    lcl_4 = rbnf_named_parse_rbnfmacro_5(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = rbnf_named__check_1[0]
                    lcl_4 = (lcl_4 == False)
                    if lcl_4:
                        lcl_4 = rbnf_named__check_1
                    else:
                        lcl_5 = rbnf_named__check_1[1]
                        rbnf_tmp_1 = lcl_5
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 10):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_5 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_5
                        lcl_5 = (rbnf_tmp_2 is None)
                        if lcl_5:
                            lcl_6 = builtin_tokens.offset
                            lcl_6 = (lcl_6, 'quote ) not match')
                            lcl_6 = builtin_cons(lcl_6, builtin_nil)
                            lcl_6 = (False, lcl_6)
                            lcl_5 = lcl_6
                        else:
                            lcl_6 = (0, rbnf_tmp_1)
                            rbnf_tmp_1_ = lcl_6
                            lcl_6 = (True, rbnf_tmp_1_)
                            lcl_5 = lcl_6
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                else:
                    lcl_4 = (rbnf_named__off_1, 'record_def lookahead failed')
                    lcl_4 = builtin_cons(lcl_4, builtin_nil)
                    lcl_4 = (False, lcl_4)
                    lcl_2 = lcl_4
                lcl_1 = lcl_2
            else:
                lcl_2 = (rbnf_named__off_1, 'record_def got EOF')
                lcl_2 = builtin_cons(lcl_2, builtin_nil)
                lcl_2 = (False, lcl_2)
                lcl_1 = lcl_2
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_record_field_def(builtin_state, builtin_tokens):
        lcl_0 = rbnf_named_parse_nameStr(builtin_state, builtin_tokens)
        rbnf_named__check_0 = lcl_0
        lcl_0 = rbnf_named__check_0[0]
        lcl_0 = (lcl_0 == False)
        if lcl_0:
            lcl_0 = rbnf_named__check_0
        else:
            lcl_1 = rbnf_named__check_0[1]
            rbnf_tmp_0 = lcl_1
            try:
                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                if (_rbnf_cur_token.idint is 17):
                    builtin_tokens.offset += 1
                else:
                    _rbnf_cur_token = None
            except IndexError:
                _rbnf_cur_token = None
            lcl_1 = _rbnf_cur_token
            rbnf_tmp_1 = lcl_1
            lcl_1 = (rbnf_tmp_1 is None)
            if lcl_1:
                lcl_2 = builtin_tokens.offset
                lcl_2 = (lcl_2, 'quote : not match')
                lcl_2 = builtin_cons(lcl_2, builtin_nil)
                lcl_2 = (False, lcl_2)
                lcl_1 = lcl_2
            else:
                lcl_2 = rbnf_named_parse_type(builtin_state, builtin_tokens)
                rbnf_named__check_2 = lcl_2
                lcl_2 = rbnf_named__check_2[0]
                lcl_2 = (lcl_2 == False)
                if lcl_2:
                    lcl_2 = rbnf_named__check_2
                else:
                    lcl_3 = rbnf_named__check_2[1]
                    rbnf_tmp_2 = lcl_3
                    lcl_3 = Pos(rbnf_tmp_1)
                    lcl_3 = (lcl_3, rbnf_tmp_0, rbnf_tmp_2)
                    rbnf_tmp_1_ = lcl_3
                    lcl_3 = (True, rbnf_tmp_1_)
                    lcl_2 = lcl_3
                lcl_1 = lcl_2
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_toplevel(builtin_state, builtin_tokens):
        try:
            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
            if (_rbnf_cur_token.idint is 2):
                builtin_tokens.offset += 1
            else:
                _rbnf_cur_token = None
        except IndexError:
            _rbnf_cur_token = None
        lcl_0 = _rbnf_cur_token
        rbnf_tmp_0 = lcl_0
        lcl_0 = (rbnf_tmp_0 is None)
        if lcl_0:
            lcl_1 = builtin_tokens.offset
            lcl_1 = (lcl_1, 'quote module not match')
            lcl_1 = builtin_cons(lcl_1, builtin_nil)
            lcl_1 = (False, lcl_1)
            lcl_0 = lcl_1
        else:
            lcl_1 = rbnf_named_parse_nameStr(builtin_state, builtin_tokens)
            rbnf_named__check_1 = lcl_1
            lcl_1 = rbnf_named__check_1[0]
            lcl_1 = (lcl_1 == False)
            if lcl_1:
                lcl_1 = rbnf_named__check_1
            else:
                lcl_2 = rbnf_named__check_1[1]
                rbnf_tmp_1 = lcl_2
                lcl_2 = builtin_tokens.offset
                rbnf_named__off_1 = lcl_2
                try:
                    builtin_tokens.array[(builtin_tokens.offset + 0)]
                    _rbnf_peek_tmp = True
                except IndexError:
                    _rbnf_peek_tmp = False
                lcl_2 = _rbnf_peek_tmp
                if lcl_2:
                    lcl_4 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                    lcl_4 = lcl_4.idint
                    if (lcl_4 == 19):
                        lcl_5 = rbnf_named_parse_rbnfmacro_1(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_5
                        lcl_5 = rbnf_named__check_2[0]
                        lcl_5 = (lcl_5 == False)
                        if lcl_5:
                            lcl_5 = rbnf_named__check_2
                        else:
                            lcl_6 = rbnf_named__check_2[1]
                            rbnf_tmp_2 = lcl_6
                            lcl_6 = []
                            lcl_7 = concat(rbnf_tmp_2)
                            lcl_6 = ModuleRecord(rbnf_tmp_1, lcl_6, lcl_7)
                            rbnf_tmp_1_ = lcl_6
                            lcl_6 = (True, rbnf_tmp_1_)
                            lcl_5 = lcl_6
                        lcl_3 = lcl_5
                    elif (lcl_4 == 24):
                        lcl_5 = rbnf_named_parse_rbnfmacro_1(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_5
                        lcl_5 = rbnf_named__check_2[0]
                        lcl_5 = (lcl_5 == False)
                        if lcl_5:
                            lcl_5 = rbnf_named__check_2
                        else:
                            lcl_6 = rbnf_named__check_2[1]
                            rbnf_tmp_2 = lcl_6
                            lcl_6 = []
                            lcl_7 = concat(rbnf_tmp_2)
                            lcl_6 = ModuleRecord(rbnf_tmp_1, lcl_6, lcl_7)
                            rbnf_tmp_1_ = lcl_6
                            lcl_6 = (True, rbnf_tmp_1_)
                            lcl_5 = lcl_6
                        lcl_3 = lcl_5
                    elif (lcl_4 == 23):
                        lcl_5 = rbnf_named_parse_rbnfmacro_1(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_5
                        lcl_5 = rbnf_named__check_2[0]
                        lcl_5 = (lcl_5 == False)
                        if lcl_5:
                            lcl_5 = rbnf_named__check_2
                        else:
                            lcl_6 = rbnf_named__check_2[1]
                            rbnf_tmp_2 = lcl_6
                            lcl_6 = []
                            lcl_7 = concat(rbnf_tmp_2)
                            lcl_6 = ModuleRecord(rbnf_tmp_1, lcl_6, lcl_7)
                            rbnf_tmp_1_ = lcl_6
                            lcl_6 = (True, rbnf_tmp_1_)
                            lcl_5 = lcl_6
                        lcl_3 = lcl_5
                    elif (lcl_4 == 20):
                        lcl_5 = rbnf_named_parse_rbnfmacro_1(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_5
                        lcl_5 = rbnf_named__check_2[0]
                        lcl_5 = (lcl_5 == False)
                        if lcl_5:
                            lcl_5 = rbnf_named__check_2
                        else:
                            lcl_6 = rbnf_named__check_2[1]
                            rbnf_tmp_2 = lcl_6
                            lcl_6 = []
                            lcl_7 = concat(rbnf_tmp_2)
                            lcl_6 = ModuleRecord(rbnf_tmp_1, lcl_6, lcl_7)
                            rbnf_tmp_1_ = lcl_6
                            lcl_6 = (True, rbnf_tmp_1_)
                            lcl_5 = lcl_6
                        lcl_3 = lcl_5
                    elif (lcl_4 == 3):
                        lcl_5 = rbnf_named_parse_rbnfmacro_0(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_5
                        lcl_5 = rbnf_named__check_2[0]
                        lcl_5 = (lcl_5 == False)
                        if lcl_5:
                            lcl_5 = rbnf_named__check_2
                        else:
                            lcl_6 = rbnf_named__check_2[1]
                            rbnf_tmp_2 = lcl_6
                            lcl_6 = rbnf_named_parse_rbnfmacro_1(builtin_state, builtin_tokens)
                            rbnf_named__check_3 = lcl_6
                            lcl_6 = rbnf_named__check_3[0]
                            lcl_6 = (lcl_6 == False)
                            if lcl_6:
                                lcl_6 = rbnf_named__check_3
                            else:
                                lcl_7 = rbnf_named__check_3[1]
                                rbnf_tmp_3 = lcl_7
                                lcl_7 = concat(rbnf_tmp_3)
                                lcl_7 = ModuleRecord(rbnf_tmp_1, rbnf_tmp_2, lcl_7)
                                rbnf_tmp_1_ = lcl_7
                                lcl_7 = (True, rbnf_tmp_1_)
                                lcl_6 = lcl_7
                            lcl_5 = lcl_6
                        lcl_3 = lcl_5
                    elif (lcl_4 == 22):
                        lcl_5 = rbnf_named_parse_rbnfmacro_1(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_5
                        lcl_5 = rbnf_named__check_2[0]
                        lcl_5 = (lcl_5 == False)
                        if lcl_5:
                            lcl_5 = rbnf_named__check_2
                        else:
                            lcl_6 = rbnf_named__check_2[1]
                            rbnf_tmp_2 = lcl_6
                            lcl_6 = []
                            lcl_7 = concat(rbnf_tmp_2)
                            lcl_6 = ModuleRecord(rbnf_tmp_1, lcl_6, lcl_7)
                            rbnf_tmp_1_ = lcl_6
                            lcl_6 = (True, rbnf_tmp_1_)
                            lcl_5 = lcl_6
                        lcl_3 = lcl_5
                    else:
                        lcl_5 = (rbnf_named__off_1, 'toplevel lookahead failed')
                        lcl_5 = builtin_cons(lcl_5, builtin_nil)
                        lcl_5 = (False, lcl_5)
                        lcl_3 = lcl_5
                    lcl_2 = lcl_3
                else:
                    lcl_3 = (rbnf_named__off_1, 'toplevel got EOF')
                    lcl_3 = builtin_cons(lcl_3, builtin_nil)
                    lcl_3 = (False, lcl_3)
                    lcl_2 = lcl_3
                lcl_1 = lcl_2
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_toptype(builtin_state, builtin_tokens):
        lcl_0 = builtin_tokens.offset
        rbnf_named__off_0 = lcl_0
        try:
            builtin_tokens.array[(builtin_tokens.offset + 0)]
            _rbnf_peek_tmp = True
        except IndexError:
            _rbnf_peek_tmp = False
        lcl_0 = _rbnf_peek_tmp
        if lcl_0:
            lcl_2 = builtin_tokens.array[(builtin_tokens.offset + 0)]
            lcl_2 = lcl_2.idint
            if (lcl_2 == 14):
                lcl_3 = rbnf_named_parse_arrowtype(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = rbnf_named__check_0[0]
                lcl_3 = (lcl_3 == False)
                if lcl_3:
                    lcl_3 = rbnf_named__check_0
                else:
                    lcl_4 = rbnf_named__check_0[1]
                    rbnf_tmp_0 = lcl_4
                    rbnf_tmp_1_ = rbnf_tmp_0
                    lcl_4 = (True, rbnf_tmp_1_)
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 12):
                lcl_3 = rbnf_named_parse_arrowtype(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = rbnf_named__check_0[0]
                lcl_3 = (lcl_3 == False)
                if lcl_3:
                    lcl_3 = rbnf_named__check_0
                else:
                    lcl_4 = rbnf_named__check_0[1]
                    rbnf_tmp_0 = lcl_4
                    rbnf_tmp_1_ = rbnf_tmp_0
                    lcl_4 = (True, rbnf_tmp_1_)
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 6):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = rbnf_named_parse_rbnfmacro_2(builtin_state, builtin_tokens)
                rbnf_named__check_1 = lcl_3
                lcl_3 = rbnf_named__check_1[0]
                lcl_3 = (lcl_3 == False)
                if lcl_3:
                    lcl_3 = rbnf_named__check_1
                else:
                    lcl_4 = rbnf_named__check_1[1]
                    rbnf_tmp_1 = lcl_4
                    try:
                        _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                        if (_rbnf_cur_token.idint is 7):
                            builtin_tokens.offset += 1
                        else:
                            _rbnf_cur_token = None
                    except IndexError:
                        _rbnf_cur_token = None
                    lcl_4 = _rbnf_cur_token
                    rbnf_tmp_2 = lcl_4
                    lcl_4 = (rbnf_tmp_2 is None)
                    if lcl_4:
                        lcl_5 = builtin_tokens.offset
                        lcl_5 = (lcl_5, 'quote . not match')
                        lcl_5 = builtin_cons(lcl_5, builtin_nil)
                        lcl_5 = (False, lcl_5)
                        lcl_4 = lcl_5
                    else:
                        lcl_5 = rbnf_named_parse_toptype(builtin_state, builtin_tokens)
                        rbnf_named__check_3 = lcl_5
                        lcl_5 = rbnf_named__check_3[0]
                        lcl_5 = (lcl_5 == False)
                        if lcl_5:
                            lcl_5 = rbnf_named__check_3
                        else:
                            lcl_6 = rbnf_named__check_3[1]
                            rbnf_tmp_3 = lcl_6
                            lcl_6 = TForall(rbnf_tmp_1, rbnf_tmp_3)
                            rbnf_tmp_1_ = lcl_6
                            lcl_6 = (True, rbnf_tmp_1_)
                            lcl_5 = lcl_6
                        lcl_4 = lcl_5
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 15):
                lcl_3 = rbnf_named_parse_arrowtype(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = rbnf_named__check_0[0]
                lcl_3 = (lcl_3 == False)
                if lcl_3:
                    lcl_3 = rbnf_named__check_0
                else:
                    lcl_4 = rbnf_named__check_0[1]
                    rbnf_tmp_0 = lcl_4
                    rbnf_tmp_1_ = rbnf_tmp_0
                    lcl_4 = (True, rbnf_tmp_1_)
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 16):
                lcl_3 = rbnf_named_parse_arrowtype(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = rbnf_named__check_0[0]
                lcl_3 = (lcl_3 == False)
                if lcl_3:
                    lcl_3 = rbnf_named__check_0
                else:
                    lcl_4 = rbnf_named__check_0[1]
                    rbnf_tmp_0 = lcl_4
                    rbnf_tmp_1_ = rbnf_tmp_0
                    lcl_4 = (True, rbnf_tmp_1_)
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 9):
                lcl_3 = rbnf_named_parse_arrowtype(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = rbnf_named__check_0[0]
                lcl_3 = (lcl_3 == False)
                if lcl_3:
                    lcl_3 = rbnf_named__check_0
                else:
                    lcl_4 = rbnf_named__check_0[1]
                    rbnf_tmp_0 = lcl_4
                    rbnf_tmp_1_ = rbnf_tmp_0
                    lcl_4 = (True, rbnf_tmp_1_)
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 5):
                lcl_3 = rbnf_named_parse_arrowtype(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = rbnf_named__check_0[0]
                lcl_3 = (lcl_3 == False)
                if lcl_3:
                    lcl_3 = rbnf_named__check_0
                else:
                    lcl_4 = rbnf_named__check_0[1]
                    rbnf_tmp_0 = lcl_4
                    rbnf_tmp_1_ = rbnf_tmp_0
                    lcl_4 = (True, rbnf_tmp_1_)
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            else:
                lcl_3 = (rbnf_named__off_0, 'toptype lookahead failed')
                lcl_3 = builtin_cons(lcl_3, builtin_nil)
                lcl_3 = (False, lcl_3)
                lcl_1 = lcl_3
            lcl_0 = lcl_1
        else:
            lcl_1 = (rbnf_named__off_0, 'toptype got EOF')
            lcl_1 = builtin_cons(lcl_1, builtin_nil)
            lcl_1 = (False, lcl_1)
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_tvar(builtin_state, builtin_tokens):
        try:
            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
            if (_rbnf_cur_token.idint is 5):
                builtin_tokens.offset += 1
            else:
                _rbnf_cur_token = None
        except IndexError:
            _rbnf_cur_token = None
        lcl_0 = _rbnf_cur_token
        rbnf_tmp_0 = lcl_0
        lcl_0 = (rbnf_tmp_0 is None)
        if lcl_0:
            lcl_1 = builtin_tokens.offset
            lcl_1 = (lcl_1, 'name not match')
            lcl_1 = builtin_cons(lcl_1, builtin_nil)
            lcl_1 = (False, lcl_1)
            lcl_0 = lcl_1
        else:
            lcl_1 = rbnf_tmp_0.value
            lcl_1 = TVar(lcl_1)
            rbnf_tmp_1_ = lcl_1
            lcl_1 = (True, rbnf_tmp_1_)
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_type(builtin_state, builtin_tokens):
        lcl_0 = builtin_tokens.offset
        rbnf_named__off_0 = lcl_0
        try:
            builtin_tokens.array[(builtin_tokens.offset + 0)]
            _rbnf_peek_tmp = True
        except IndexError:
            _rbnf_peek_tmp = False
        lcl_0 = _rbnf_peek_tmp
        if lcl_0:
            lcl_2 = builtin_tokens.array[(builtin_tokens.offset + 0)]
            lcl_2 = lcl_2.idint
            if (lcl_2 == 14):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = rbnf_tmp_0.value
                lcl_3 = parse_str(lcl_3)
                lcl_3 = TSym(lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_3 = (True, rbnf_tmp_1_)
                lcl_1 = lcl_3
            elif (lcl_2 == 12):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = rbnf_named_parse_toptype(builtin_state, builtin_tokens)
                rbnf_named__check_1 = lcl_3
                lcl_3 = rbnf_named__check_1[0]
                lcl_3 = (lcl_3 == False)
                if lcl_3:
                    lcl_3 = rbnf_named__check_1
                else:
                    lcl_4 = rbnf_named__check_1[1]
                    rbnf_tmp_1 = lcl_4
                    try:
                        _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                        if (_rbnf_cur_token.idint is 13):
                            builtin_tokens.offset += 1
                        else:
                            _rbnf_cur_token = None
                    except IndexError:
                        _rbnf_cur_token = None
                    lcl_4 = _rbnf_cur_token
                    rbnf_tmp_2 = lcl_4
                    lcl_4 = (rbnf_tmp_2 is None)
                    if lcl_4:
                        lcl_5 = builtin_tokens.offset
                        lcl_5 = (lcl_5, 'quote } not match')
                        lcl_5 = builtin_cons(lcl_5, builtin_nil)
                        lcl_5 = (False, lcl_5)
                        lcl_4 = lcl_5
                    else:
                        lcl_5 = TImplicit(rbnf_tmp_1)
                        rbnf_tmp_1_ = lcl_5
                        lcl_5 = (True, rbnf_tmp_1_)
                        lcl_4 = lcl_5
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 15):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                try:
                    _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                    if (_rbnf_cur_token.idint is 5):
                        builtin_tokens.offset += 1
                    else:
                        _rbnf_cur_token = None
                except IndexError:
                    _rbnf_cur_token = None
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_1 = lcl_3
                lcl_3 = (rbnf_tmp_1 is None)
                if lcl_3:
                    lcl_4 = builtin_tokens.offset
                    lcl_4 = (lcl_4, 'name not match')
                    lcl_4 = builtin_cons(lcl_4, builtin_nil)
                    lcl_4 = (False, lcl_4)
                    lcl_3 = lcl_4
                else:
                    lcl_4 = rbnf_tmp_1.value
                    lcl_4 = TNew(lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_4 = (True, rbnf_tmp_1_)
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 16):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = rbnf_named_parse_type(builtin_state, builtin_tokens)
                rbnf_named__check_1 = lcl_3
                lcl_3 = rbnf_named__check_1[0]
                lcl_3 = (lcl_3 == False)
                if lcl_3:
                    lcl_3 = rbnf_named__check_1
                else:
                    lcl_4 = rbnf_named__check_1[1]
                    rbnf_tmp_1 = lcl_4
                    try:
                        _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                        if (_rbnf_cur_token.idint is 4):
                            builtin_tokens.offset += 1
                        else:
                            _rbnf_cur_token = None
                    except IndexError:
                        _rbnf_cur_token = None
                    lcl_4 = _rbnf_cur_token
                    rbnf_tmp_2 = lcl_4
                    lcl_4 = (rbnf_tmp_2 is None)
                    if lcl_4:
                        lcl_5 = builtin_tokens.offset
                        lcl_5 = (lcl_5, 'quote as not match')
                        lcl_5 = builtin_cons(lcl_5, builtin_nil)
                        lcl_5 = (False, lcl_5)
                        lcl_4 = lcl_5
                    else:
                        lcl_5 = rbnf_named_parse_nameStr(builtin_state, builtin_tokens)
                        rbnf_named__check_3 = lcl_5
                        lcl_5 = rbnf_named__check_3[0]
                        lcl_5 = (lcl_5 == False)
                        if lcl_5:
                            lcl_5 = rbnf_named__check_3
                        else:
                            lcl_6 = rbnf_named__check_3[1]
                            rbnf_tmp_3 = lcl_6
                            lcl_6 = TQuery(rbnf_tmp_3, rbnf_tmp_1)
                            rbnf_tmp_1_ = lcl_6
                            lcl_6 = (True, rbnf_tmp_1_)
                            lcl_5 = lcl_6
                        lcl_4 = lcl_5
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 9):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = builtin_tokens.offset
                rbnf_named__off_1 = lcl_3
                try:
                    builtin_tokens.array[(builtin_tokens.offset + 0)]
                    _rbnf_peek_tmp = True
                except IndexError:
                    _rbnf_peek_tmp = False
                lcl_3 = _rbnf_peek_tmp
                if lcl_3:
                    lcl_5 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                    lcl_5 = lcl_5.idint
                    if (lcl_5 == 14):
                        lcl_6 = rbnf_named_parse_rbnfmacro_3(builtin_state, builtin_tokens)
                        rbnf_named__check_1 = lcl_6
                        lcl_6 = rbnf_named__check_1[0]
                        lcl_6 = (lcl_6 == False)
                        if lcl_6:
                            lcl_6 = rbnf_named__check_1
                        else:
                            lcl_7 = rbnf_named__check_1[1]
                            rbnf_tmp_1 = lcl_7
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 10):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_7 = _rbnf_cur_token
                            rbnf_tmp_2 = lcl_7
                            lcl_7 = (rbnf_tmp_2 is None)
                            if lcl_7:
                                lcl_8 = builtin_tokens.offset
                                lcl_8 = (lcl_8, 'quote ) not match')
                                lcl_8 = builtin_cons(lcl_8, builtin_nil)
                                lcl_8 = (False, lcl_8)
                                lcl_7 = lcl_8
                            else:
                                lcl_8 = mk_tuple(TTup, rbnf_tmp_1)
                                rbnf_tmp_1_ = lcl_8
                                lcl_8 = (True, rbnf_tmp_1_)
                                lcl_7 = lcl_8
                            lcl_6 = lcl_7
                        lcl_4 = lcl_6
                    elif (lcl_5 == 12):
                        lcl_6 = rbnf_named_parse_rbnfmacro_3(builtin_state, builtin_tokens)
                        rbnf_named__check_1 = lcl_6
                        lcl_6 = rbnf_named__check_1[0]
                        lcl_6 = (lcl_6 == False)
                        if lcl_6:
                            lcl_6 = rbnf_named__check_1
                        else:
                            lcl_7 = rbnf_named__check_1[1]
                            rbnf_tmp_1 = lcl_7
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 10):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_7 = _rbnf_cur_token
                            rbnf_tmp_2 = lcl_7
                            lcl_7 = (rbnf_tmp_2 is None)
                            if lcl_7:
                                lcl_8 = builtin_tokens.offset
                                lcl_8 = (lcl_8, 'quote ) not match')
                                lcl_8 = builtin_cons(lcl_8, builtin_nil)
                                lcl_8 = (False, lcl_8)
                                lcl_7 = lcl_8
                            else:
                                lcl_8 = mk_tuple(TTup, rbnf_tmp_1)
                                rbnf_tmp_1_ = lcl_8
                                lcl_8 = (True, rbnf_tmp_1_)
                                lcl_7 = lcl_8
                            lcl_6 = lcl_7
                        lcl_4 = lcl_6
                    elif (lcl_5 == 6):
                        lcl_6 = rbnf_named_parse_rbnfmacro_3(builtin_state, builtin_tokens)
                        rbnf_named__check_1 = lcl_6
                        lcl_6 = rbnf_named__check_1[0]
                        lcl_6 = (lcl_6 == False)
                        if lcl_6:
                            lcl_6 = rbnf_named__check_1
                        else:
                            lcl_7 = rbnf_named__check_1[1]
                            rbnf_tmp_1 = lcl_7
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 10):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_7 = _rbnf_cur_token
                            rbnf_tmp_2 = lcl_7
                            lcl_7 = (rbnf_tmp_2 is None)
                            if lcl_7:
                                lcl_8 = builtin_tokens.offset
                                lcl_8 = (lcl_8, 'quote ) not match')
                                lcl_8 = builtin_cons(lcl_8, builtin_nil)
                                lcl_8 = (False, lcl_8)
                                lcl_7 = lcl_8
                            else:
                                lcl_8 = mk_tuple(TTup, rbnf_tmp_1)
                                rbnf_tmp_1_ = lcl_8
                                lcl_8 = (True, rbnf_tmp_1_)
                                lcl_7 = lcl_8
                            lcl_6 = lcl_7
                        lcl_4 = lcl_6
                    elif (lcl_5 == 15):
                        lcl_6 = rbnf_named_parse_rbnfmacro_3(builtin_state, builtin_tokens)
                        rbnf_named__check_1 = lcl_6
                        lcl_6 = rbnf_named__check_1[0]
                        lcl_6 = (lcl_6 == False)
                        if lcl_6:
                            lcl_6 = rbnf_named__check_1
                        else:
                            lcl_7 = rbnf_named__check_1[1]
                            rbnf_tmp_1 = lcl_7
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 10):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_7 = _rbnf_cur_token
                            rbnf_tmp_2 = lcl_7
                            lcl_7 = (rbnf_tmp_2 is None)
                            if lcl_7:
                                lcl_8 = builtin_tokens.offset
                                lcl_8 = (lcl_8, 'quote ) not match')
                                lcl_8 = builtin_cons(lcl_8, builtin_nil)
                                lcl_8 = (False, lcl_8)
                                lcl_7 = lcl_8
                            else:
                                lcl_8 = mk_tuple(TTup, rbnf_tmp_1)
                                rbnf_tmp_1_ = lcl_8
                                lcl_8 = (True, rbnf_tmp_1_)
                                lcl_7 = lcl_8
                            lcl_6 = lcl_7
                        lcl_4 = lcl_6
                    elif (lcl_5 == 16):
                        lcl_6 = rbnf_named_parse_rbnfmacro_3(builtin_state, builtin_tokens)
                        rbnf_named__check_1 = lcl_6
                        lcl_6 = rbnf_named__check_1[0]
                        lcl_6 = (lcl_6 == False)
                        if lcl_6:
                            lcl_6 = rbnf_named__check_1
                        else:
                            lcl_7 = rbnf_named__check_1[1]
                            rbnf_tmp_1 = lcl_7
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 10):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_7 = _rbnf_cur_token
                            rbnf_tmp_2 = lcl_7
                            lcl_7 = (rbnf_tmp_2 is None)
                            if lcl_7:
                                lcl_8 = builtin_tokens.offset
                                lcl_8 = (lcl_8, 'quote ) not match')
                                lcl_8 = builtin_cons(lcl_8, builtin_nil)
                                lcl_8 = (False, lcl_8)
                                lcl_7 = lcl_8
                            else:
                                lcl_8 = mk_tuple(TTup, rbnf_tmp_1)
                                rbnf_tmp_1_ = lcl_8
                                lcl_8 = (True, rbnf_tmp_1_)
                                lcl_7 = lcl_8
                            lcl_6 = lcl_7
                        lcl_4 = lcl_6
                    elif (lcl_5 == 10):
                        _rbnf_old_offset = builtin_tokens.offset
                        _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                        builtin_tokens.offset = (_rbnf_old_offset + 1)
                        lcl_6 = _rbnf_cur_token
                        rbnf_tmp_1 = lcl_6
                        lcl_6 = []
                        lcl_6 = TTup(lcl_6)
                        rbnf_tmp_1_ = lcl_6
                        lcl_6 = (True, rbnf_tmp_1_)
                        lcl_4 = lcl_6
                    elif (lcl_5 == 9):
                        lcl_6 = rbnf_named_parse_rbnfmacro_3(builtin_state, builtin_tokens)
                        rbnf_named__check_1 = lcl_6
                        lcl_6 = rbnf_named__check_1[0]
                        lcl_6 = (lcl_6 == False)
                        if lcl_6:
                            lcl_6 = rbnf_named__check_1
                        else:
                            lcl_7 = rbnf_named__check_1[1]
                            rbnf_tmp_1 = lcl_7
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 10):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_7 = _rbnf_cur_token
                            rbnf_tmp_2 = lcl_7
                            lcl_7 = (rbnf_tmp_2 is None)
                            if lcl_7:
                                lcl_8 = builtin_tokens.offset
                                lcl_8 = (lcl_8, 'quote ) not match')
                                lcl_8 = builtin_cons(lcl_8, builtin_nil)
                                lcl_8 = (False, lcl_8)
                                lcl_7 = lcl_8
                            else:
                                lcl_8 = mk_tuple(TTup, rbnf_tmp_1)
                                rbnf_tmp_1_ = lcl_8
                                lcl_8 = (True, rbnf_tmp_1_)
                                lcl_7 = lcl_8
                            lcl_6 = lcl_7
                        lcl_4 = lcl_6
                    elif (lcl_5 == 5):
                        lcl_6 = rbnf_named_parse_rbnfmacro_3(builtin_state, builtin_tokens)
                        rbnf_named__check_1 = lcl_6
                        lcl_6 = rbnf_named__check_1[0]
                        lcl_6 = (lcl_6 == False)
                        if lcl_6:
                            lcl_6 = rbnf_named__check_1
                        else:
                            lcl_7 = rbnf_named__check_1[1]
                            rbnf_tmp_1 = lcl_7
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 10):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_7 = _rbnf_cur_token
                            rbnf_tmp_2 = lcl_7
                            lcl_7 = (rbnf_tmp_2 is None)
                            if lcl_7:
                                lcl_8 = builtin_tokens.offset
                                lcl_8 = (lcl_8, 'quote ) not match')
                                lcl_8 = builtin_cons(lcl_8, builtin_nil)
                                lcl_8 = (False, lcl_8)
                                lcl_7 = lcl_8
                            else:
                                lcl_8 = mk_tuple(TTup, rbnf_tmp_1)
                                rbnf_tmp_1_ = lcl_8
                                lcl_8 = (True, rbnf_tmp_1_)
                                lcl_7 = lcl_8
                            lcl_6 = lcl_7
                        lcl_4 = lcl_6
                    else:
                        lcl_6 = (rbnf_named__off_1, 'type lookahead failed')
                        lcl_6 = builtin_cons(lcl_6, builtin_nil)
                        lcl_6 = (False, lcl_6)
                        lcl_4 = lcl_6
                    lcl_3 = lcl_4
                else:
                    lcl_4 = (rbnf_named__off_1, 'type got EOF')
                    lcl_4 = builtin_cons(lcl_4, builtin_nil)
                    lcl_4 = (False, lcl_4)
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 5):
                lcl_3 = rbnf_named_parse_tvar(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = rbnf_named__check_0[0]
                lcl_3 = (lcl_3 == False)
                if lcl_3:
                    lcl_3 = rbnf_named__check_0
                else:
                    lcl_4 = rbnf_named__check_0[1]
                    rbnf_tmp_0 = lcl_4
                    rbnf_tmp_1_ = rbnf_tmp_0
                    lcl_4 = (True, rbnf_tmp_1_)
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            else:
                lcl_3 = (rbnf_named__off_0, 'type lookahead failed')
                lcl_3 = builtin_cons(lcl_3, builtin_nil)
                lcl_3 = (False, lcl_3)
                lcl_1 = lcl_3
            lcl_0 = lcl_1
        else:
            lcl_1 = (rbnf_named__off_0, 'type got EOF')
            lcl_1 = builtin_cons(lcl_1, builtin_nil)
            lcl_1 = (False, lcl_1)
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_type_head(builtin_state, builtin_tokens):
        try:
            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
            if (_rbnf_cur_token.idint is 5):
                builtin_tokens.offset += 1
            else:
                _rbnf_cur_token = None
        except IndexError:
            _rbnf_cur_token = None
        lcl_0 = _rbnf_cur_token
        rbnf_tmp_0 = lcl_0
        lcl_0 = (rbnf_tmp_0 is None)
        if lcl_0:
            lcl_1 = builtin_tokens.offset
            lcl_1 = (lcl_1, 'name not match')
            lcl_1 = builtin_cons(lcl_1, builtin_nil)
            lcl_1 = (False, lcl_1)
            lcl_0 = lcl_1
        else:
            lcl_1 = builtin_tokens.offset
            rbnf_named__off_1 = lcl_1
            try:
                builtin_tokens.array[(builtin_tokens.offset + 0)]
                _rbnf_peek_tmp = True
            except IndexError:
                _rbnf_peek_tmp = False
            lcl_1 = _rbnf_peek_tmp
            if lcl_1:
                lcl_3 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                lcl_3 = lcl_3.idint
                if (lcl_3 == 5):
                    lcl_4 = rbnf_named_parse_rbnfmacro_4(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = rbnf_named__check_1[0]
                    lcl_4 = (lcl_4 == False)
                    if lcl_4:
                        lcl_4 = rbnf_named__check_1
                    else:
                        lcl_5 = rbnf_named__check_1[1]
                        rbnf_tmp_1 = lcl_5
                        lcl_5 = Pos(rbnf_tmp_0)
                        lcl_6 = rbnf_tmp_0.value
                        lcl_5 = (lcl_5, lcl_6, rbnf_tmp_1)
                        rbnf_tmp_1_ = lcl_5
                        lcl_5 = (True, rbnf_tmp_1_)
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                else:
                    lcl_4 = Pos(rbnf_tmp_0)
                    lcl_5 = rbnf_tmp_0.value
                    lcl_6 = []
                    lcl_4 = (lcl_4, lcl_5, lcl_6)
                    rbnf_tmp_1_ = lcl_4
                    lcl_4 = (True, rbnf_tmp_1_)
                    lcl_2 = lcl_4
                lcl_1 = lcl_2
            else:
                lcl_2 = (rbnf_named__off_1, 'type_head got EOF')
                lcl_2 = builtin_cons(lcl_2, builtin_nil)
                lcl_2 = (False, lcl_2)
                lcl_1 = lcl_2
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_variant_def(builtin_state, builtin_tokens):
        try:
            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
            if (_rbnf_cur_token.idint is 18):
                builtin_tokens.offset += 1
            else:
                _rbnf_cur_token = None
        except IndexError:
            _rbnf_cur_token = None
        lcl_0 = _rbnf_cur_token
        rbnf_tmp_0 = lcl_0
        lcl_0 = (rbnf_tmp_0 is None)
        if lcl_0:
            lcl_1 = builtin_tokens.offset
            lcl_1 = (lcl_1, 'quote | not match')
            lcl_1 = builtin_cons(lcl_1, builtin_nil)
            lcl_1 = (False, lcl_1)
            lcl_0 = lcl_1
        else:
            lcl_1 = rbnf_named_parse_nameStr(builtin_state, builtin_tokens)
            rbnf_named__check_1 = lcl_1
            lcl_1 = rbnf_named__check_1[0]
            lcl_1 = (lcl_1 == False)
            if lcl_1:
                lcl_1 = rbnf_named__check_1
            else:
                lcl_2 = rbnf_named__check_1[1]
                rbnf_tmp_1 = lcl_2
                lcl_2 = rbnf_named_parse_type(builtin_state, builtin_tokens)
                rbnf_named__check_2 = lcl_2
                lcl_2 = rbnf_named__check_2[0]
                lcl_2 = (lcl_2 == False)
                if lcl_2:
                    lcl_2 = rbnf_named__check_2
                else:
                    lcl_3 = rbnf_named__check_2[1]
                    rbnf_tmp_2 = lcl_3
                    try:
                        _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                        if (_rbnf_cur_token.idint is 17):
                            builtin_tokens.offset += 1
                        else:
                            _rbnf_cur_token = None
                    except IndexError:
                        _rbnf_cur_token = None
                    lcl_3 = _rbnf_cur_token
                    rbnf_tmp_3 = lcl_3
                    lcl_3 = (rbnf_tmp_3 is None)
                    if lcl_3:
                        lcl_4 = builtin_tokens.offset
                        lcl_4 = (lcl_4, 'quote : not match')
                        lcl_4 = builtin_cons(lcl_4, builtin_nil)
                        lcl_4 = (False, lcl_4)
                        lcl_3 = lcl_4
                    else:
                        lcl_4 = rbnf_named_parse_type(builtin_state, builtin_tokens)
                        rbnf_named__check_4 = lcl_4
                        lcl_4 = rbnf_named__check_4[0]
                        lcl_4 = (lcl_4 == False)
                        if lcl_4:
                            lcl_4 = rbnf_named__check_4
                        else:
                            lcl_5 = rbnf_named__check_4[1]
                            rbnf_tmp_4 = lcl_5
                            lcl_5 = Pos(rbnf_tmp_0)
                            lcl_5 = (lcl_5, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_4)
                            rbnf_tmp_1_ = lcl_5
                            lcl_5 = (True, rbnf_tmp_1_)
                            lcl_4 = lcl_5
                        lcl_3 = lcl_4
                    lcl_2 = lcl_3
                lcl_1 = lcl_2
            lcl_0 = lcl_1
        return lcl_0
    return rbnf_named_parse_START