from warnings import warn

class Token:
    offset: int
    lineno: int
    colno: int
    filename: str
    idint: int
    value: str
    __slots__ = ("offset", "lineno", "colno", "filename", "idint", "value")

    def __init__(self, offset, lineno, colno, filename, type, value):
        self.offset = offset
        self.lineno = lineno
        self.colno = colno
        self.filename = filename
        self.idint = type
        self.value = value

    def __eq__(self, other: "Token"):
        if not isinstance(other, Token):
            return False

        return (
            self.offset == other.offset
            and self.filename == other.filename
            and self.idint == other.idint
            and self.value == other.value
            and self.colno == other.colno
            and self.lineno == other.lineno
        )

    def __hash__(self):
        return (
            (self.offset ^ self.lineno ^ self.colno + 2333 + self.idint)
            ^ hash(self.filename)
            ^ hash(self.value)
        )

    def __repr__(self):
        return (
            "Token(offset=%d, lineno=%d, colno=%d, filename=%s, type=%d, value=%s)"
            % (
                self.offset,
                self.lineno,
                self.colno,
                self.filename,
                self.idint,
                repr(self.value),
            )
        )

def lexer(filename, text: str, *, pos=0, use_bof=True, use_eof=True):
    text_length = len(text)
    colno = 0
    lineno = 0
    newline = "\n"
    match = REGEX_STR.match
    ignores = IGNORES
    unionall_info = UNIONALL_INFO
    _Token = Token
    tokens = []
    append = tokens.append
    if use_bof:
        append(_Token(0, 0, 0, filename, BOF, ""))
    while True:
        if text_length <= pos:
            break
        
        res = match(text, pos)
        if not res:
            warn(f"No handler for character `{text[pos].__repr__()}`.")
            ch = text[pos]
            append(Token(pos, lineno, colno, filename, -1, ch))
            if ch == "\n":
                lineno += 1
                colno = 0
            pos += 1
            continue
        pat = res.group()
        typeid, cast_map = unionall_info[res.lastindex]
        if typeid in ignores:
            n = len(pat)
            line_inc = pat.count(newline)
            if line_inc:
                latest_newline_idx = pat.rindex(newline)
                colno = n - latest_newline_idx
                lineno += line_inc
            else:
                colno += n
            pos += n
            continue

        if cast_map:
            typeid = cast_map.get(pat, typeid)
        
        append(_Token(pos, lineno, colno, filename, typeid, pat))
        n = len(pat)
        line_inc = pat.count(newline)
        if line_inc:
            latest_newline_idx = pat.rindex(newline)
            colno = n - latest_newline_idx
            lineno += line_inc
        else:
            colno += n
        pos += n

    if use_eof:
        append(Token(pos, lineno, colno, filename, EOF, ""))
    return tokens

def lexer_lazy_bytes(filename, text: bytes, *, pos=0, use_bof=True, use_eof=True):
    text_length = len(text)
    colno = 0
    lineno = 0
    match = REGEX_BYTES.match
    ignores = IGNORES
    unionall_info = UNIONALL_INFO_BYTES
    _Token = Token
    if use_bof:
        yield _Token(0, 0, 0, filename, BOF, b"")
    
    while True:
        if text_length <= pos:
            break
        
        res = match(text, pos)
        if not res:
            warn(f"No handler for character `{str(text[pos]).__repr__()}`.")
            ch = text[pos]
            yield _Token(pos, lineno, colno, filename, -1, ch)
            if ch == b'\n':
                lineno += 1
                colno = 0
            pos += 1
            continue
        pat = res.group()
        typeid, cast_map = unionall_info[res.lastindex]
        if typeid in ignores:
            n = len(pat)
            line_inc = pat.count(b'\n')
            if line_inc:
                latest_newline_idx = pat.rindex(b'\n')
                colno = n - latest_newline_idx
                lineno += line_inc
            else:
                colno += n
            pos += n
            continue

        if cast_map:
            typeid = cast_map.get(pat, typeid)
        
        yield _Token(pos, lineno, colno, filename, typeid, pat)
        n = len(pat)
        line_inc = pat.count(b'\n')
        if line_inc:
            latest_newline_idx = pat.rindex(b'\n')
            colno = n - latest_newline_idx
            lineno += line_inc
        else:
            colno += n
        pos += n

    if use_eof:
        yield _Token(pos, lineno, colno, filename, EOF, "")

EOF = 1
BOF = 0
REGEX = '(\\s+)|([+-]?([0-9]*[.])[0-9]+)|((0|[1-9][0-9]*|0[oO]?[0-7]+|0[xX][0-9a-fA-F]+|0[bB][01]+)[lL]?)|("([^\\\\\\"]+|\\\\.)*"|\'([^\\\\\\\']+|\\\\.)*\')|([a-zA-Z_][a-zA-Z0-9_]*[\']*)|(//[^\\n\\r]*|#[^\\n\\r]*)|(\\})|(\\|)|(\\{)|(@)|(\\?)|(=)|(:)|(\\.)|(\\->)|(,)|(\\))|(\\()'
REGEX_STR = __import__('re').compile(REGEX)
REGEX_BYTES = __import__('re').compile(REGEX.encode())
IGNORES = (36, 37)
UNIONALL_INFO = ((None, None), (36, None), (33, None), (None, None), (32, None), (None, None), (14, None), (None, None), (None, None), (5, {'in': 28, 'fun': 29, 'open': 23, 'if': 25, 'false': 35, 'type': 24, 'module': 2, 'true': 34, 'val': 19, 'check': 22, 'forall': 6, 'then': 26, 'extern': 31, 'coerce': 30, 'import': 3, 'else': 27, 'let': 20, 'as': 4}), (37, None), (13, None), (18, None), (12, None), (15, None), (16, None), (21, None), (17, None), (7, None), (8, None), (11, None), (10, None), (9, None))
UNIONALL_INFO_BYTES = ((None, None), (36, None), (33, None), (None, None), (32, None), (None, None), (14, None), (None, None), (None, None), (5, {b'in': 28, b'fun': 29, b'open': 23, b'if': 25, b'false': 35, b'type': 24, b'module': 2, b'true': 34, b'val': 19, b'check': 22, b'forall': 6, b'then': 26, b'extern': 31, b'coerce': 30, b'import': 3, b'else': 27, b'let': 20, b'as': 4}), (37, None), (13, None), (18, None), (12, None), (15, None), (16, None), (21, None), (17, None), (7, None), (8, None), (11, None), (10, None), (9, None))
numbering = {'BOF': 0, 'EOF': 1, 'quote module': 2, 'quote import': 3, 'quote as': 4, 'name': 5, 'quote forall': 6, 'quote .': 7, 'quote ->': 8, 'quote (': 9, 'quote )': 10, 'quote ,': 11, 'quote {': 12, 'quote }': 13, 'str': 14, 'quote @': 15, 'quote ?': 16, 'quote :': 17, 'quote |': 18, 'quote val': 19, 'quote let': 20, 'quote =': 21, 'quote check': 22, 'quote open': 23, 'quote type': 24, 'quote if': 25, 'quote then': 26, 'quote else': 27, 'quote in': 28, 'quote fun': 29, 'quote coerce': 30, 'quote extern': 31, 'int': 32, 'float': 33, 'quote true': 34, 'quote false': 35, 'SPACE': 36, 'COMMENT': 37}
