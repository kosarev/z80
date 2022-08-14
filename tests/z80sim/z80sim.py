#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Transistor-level Z80 CPU simulator.
#
# Originated from the Visual 6502 and Z80 Explorer projects.
#
# https://github.com/trebonian/visual6502
# commit badcf8e40be74398ec55593e172acb287ca73e3b of 31 Mar 2020
#
# https://github.com/gdevic/Z80Explorer
# commit c11574c1d80352b355d297ad3ae33701a7110485 of 19 Jan 2021


import ast
import concurrent.futures
import datetime
import gc
import gzip
import hashlib
import json
import pathlib
import pprint
import pycosat
import sys
import traceback
import z3


_CACHE_ROOT = pathlib.Path('__z80sim_cache')
assert _CACHE_ROOT.exists()

HASH = hashlib.sha256

_GND_ID = 'gnd'
_PWR_ID = 'pwr'
_PINS = (
    'ab0', 'ab1', 'ab2', 'ab3', 'ab4', 'ab5', 'ab6', 'ab7',
    'ab8', 'ab9', 'ab10', 'ab11', 'ab12', 'ab13', 'ab14', 'ab15',
    'db0', 'db1', 'db2', 'db3', 'db4', 'db5', 'db6', 'db7',
    '~int', '~nmi', '~halt', '~mreq', '~iorq', '~rfsh', '~m1',
    '~reset', '~busrq', '~wait', '~busak', '~wr', '~rd', '~clk')


def _ceil_div(a, b):
    return -(a // -b)


def deep_tupilize(x):
    if isinstance(x, (str, int, bool, type(None))):
        return x
    assert isinstance(x, list), x
    return tuple(deep_tupilize(e) for e in x)


class Status(object):
    __supression_count = 0
    __parts = []
    __line = ''

    @staticmethod
    def __emit(line):
        if __class__.__supression_count != 0:
            return

        line_with_spaces = line
        if len(line) < len(__class__.__line):
            line_with_spaces += ' ' * (len(__class__.__line) - len(line))

        print(f'\r{line_with_spaces}', end='', file=sys.stderr)
        __class__.__line = line

    @staticmethod
    def __get_time():
        return datetime.datetime.now().strftime('%H:%M:%S')

    @staticmethod
    def __update():
        parts = ', '.join(p for p in __class__.__parts if p)
        __class__.__emit(f'{__class__.__get_time()}  {parts}')

    @staticmethod
    def clear():
        if __class__.__supression_count != 0:
            return
        __class__.__emit('')
        print('\r', end='', file=sys.stderr)

    @staticmethod
    def print(*args):
        if __class__.__supression_count != 0:
            return

        line = __class__.__line
        __class__.clear()

        print(f'{__class__.__get_time()} ', *args)

        __class__.__emit(line)

    @staticmethod
    def enter(s):
        __class__.__parts.append(s)
        __class__.__update()

    @staticmethod
    def exit():
        __class__.__parts.pop()
        __class__.__update()

    @staticmethod
    def do(p, option=None):
        class S:
            def __enter__(self):
                self.__show = (option is None or option in sys.argv)
                if self.__show:
                    Status.enter(p)

            def __exit__(self, exc_type, exc_val, exc_tb):
                if self.__show:
                    Status.exit()

        return S()

    @staticmethod
    def start_suppression():
        __class__.__supression_count += 1

    @staticmethod
    def end_suppression():
        assert __class__.__supression_count > 0
        __class__.__supression_count -= 1

    @staticmethod
    def suppress():
        class S:
            def __enter__(self):
                Status.start_suppression()

            def __exit__(self, exc_type, exc_val, exc_tb):
                Status.end_suppression()

        return S()


class Cache(object):
    class __Entry(object):
        def __init__(self, domain, key):
            self.domain, self.key = domain, key

        @property
        def hash(self):
            return HASH(str(self.key).encode()).hexdigest()

        def get_path(self, suffix=None):
            h = self.hash
            if suffix is not None:
                h += suffix
            return _CACHE_ROOT / self.domain / h[:3] / h[3:6] / h

        def exists(self, suffix=None):
            return self.get_path(suffix).exists()

        def load(self):
            try:
                with gzip.open(self.get_path()) as f:
                    return json.loads(f.read().decode())
            except FileNotFoundError:
                return None

        def store(self, payload):
            path = self.get_path()
            path.parent.mkdir(parents=True, exist_ok=True)

            temp_path = path.parent / (path.name + '.tmp')
            with gzip.open(temp_path, 'wb') as f:
                f.write(json.dumps(payload).encode())

            temp_path.rename(path)

        def create(self, suffix=None):
            path = self.get_path(suffix)
            path.parent.mkdir(parents=True, exist_ok=True)
            with path.open('w'):
                pass

    @staticmethod
    def get_entry(domain, key):
        return __class__.__Entry(domain, key)


class Literal(object):
    __literals = {}

    @staticmethod
    def clear():
        __class__.__literals.clear()

    @staticmethod
    def get(id):
        if id is None:
            id = f't{len(__class__.__literals) + 1}'

        t = __class__.__literals.get(id)
        if t is not None:
            return t

        t = __class__()
        i = __class__()

        t.id, t.sign = id, False
        i.id, i.sign = id, True
        t.sat_index = len(__class__.__literals) + 1
        i.sat_index = -t.sat_index
        t.__inversion = i
        i.__inversion = t
        __class__.__literals[id] = t

        return t

    def __repr__(self):
        SIGNS = {False: '', True: '~'}
        return f'{SIGNS[self.sign]}{self.id}'

    def __lt__(self, other):
        # TODO: Can we just compare SAT indexes here?
        #       (Which also represents the 'age' of the literal.)
        return (self.id, self.sign) < (other.id, other.sign)

    @staticmethod
    def cast(x):
        if isinstance(x, __class__):
            return x
        return __class__.get(x)

    class Storage(object):
        def __init__(self, *, image=None):
            self.__literals = []
            self.__indexes = {}

            if image is not None:
                for i, id in enumerate(image):
                    assert self.add(Literal.get(id)) == i * 2

        def add(self, literal):
            if literal.sign:
                return self.add(~literal) + 1

            i = self.__indexes.get(literal)
            if i is None:
                i = len(self.__literals)
                self.__literals.append(literal)
                self.__indexes[literal] = i

            return i * 2

        def get(self, image):
            i, sign = image // 2, image % 2
            t = self.__literals[i]
            return ~t if sign else t

        @property
        def image(self):
            return tuple(t.id for t in self.__literals)

    def __invert__(self):
        return self.__inversion


class Bool(object):
    __cache = {}
    __equiv_cache = {}

    @staticmethod
    def clear():
        __class__.__cache.clear()
        __class__.__equiv_cache.clear()

    @staticmethod
    def get(term):
        if isinstance(term, int):
            assert term in (0, 1)
            term = bool(term)
        elif isinstance(term, str):
            term = Literal.get(term)

        b = __class__.__cache.get(term)
        if b is not None:
            return b

        if isinstance(term, bool):
            false = __class__.__cache[False] = __class__()
            true = __class__.__cache[True] = __class__()
            false.value, true.value = False, True
            false.__inversion, true.__inversion = true, false
            return true if term else false

        assert isinstance(term, Literal)
        assert term.sign is False
        b = __class__.__cache[term] = __class__()
        b.value = None
        b.symbol = term
        b._e = None
        b.__inversion = None
        return b

    @property
    def sat_clauses(self):
        clauses = []
        syms = {}

        def add(*cs):
            clauses.extend(tuple(t.sat_index for t in c) for c in cs)

        def get(n):
            assert n.value is None, self

            r = syms.get(n.symbol)
            if r is not None:
                return r

            r = n.symbol
            if n._e is None:
                syms[r] = r
                return r

            kind, ops = n._e
            if kind == 'not':
                a, = ops
                r = ~get(a)
            elif kind == 'eq':
                # r = xnor(a, b)  ===  xor(a, b, r)
                a, b = ops
                a, b = get(a), get(b)
                add((~a, ~b, r), (a, b, r), (a, ~b, ~r), (~a, b, ~r))
            elif kind == 'neq':
                a, b = ops
                a, b = get(a), get(b)
                add((a, b), (~a, ~b))
                # TODO: 'r' remains unbounded.
                r = None
            elif kind == 'ifelse':
                i, t, e = ops
                i, t, e = get(i), get(t), get(e)
                add((~i, t, ~r), (~i, ~t, r), (i, e, ~r), (i, ~e, r))
            elif kind == 'or':
                op_syms = tuple(get(op) for op in ops)
                add(tuple(s for s in op_syms + (~r,)))
                for s in op_syms:
                    add((~s, r))
            elif kind == 'and':
                op_syms = tuple(get(op) for op in ops)
                add(tuple(~s for s in op_syms + (~r,)))
                for s in op_syms:
                    add((s, ~r))
            else:
                assert 0, n  # TODO

            syms[r] = r
            return r

        get(self)

        return clauses

    @staticmethod
    def from_ops(kind, *ops):
        syms = tuple(op.symbol for op in ops)

        COMMUTATIVE = {
            'not': False, 'ifelse': False,
            'eq': True, 'neq': True, 'or': True, 'and': True}
        if COMMUTATIVE[kind]:
            syms = tuple(sorted(syms))

        key = kind, syms
        b = __class__.__cache.get(key)
        if b is not None:
            return b

        b = __class__.__cache[key] = Bool.get(Literal.get(None))
        b._e = kind, ops
        return b

    def __repr__(self):
        if self.value is not None:
            return repr(int(self.value))
        if self._e is None:
            return str(self.symbol)

        rep = []
        syms = set()
        worklist = [self]
        while worklist:
            n = worklist.pop()
            if n.value is not None or n.symbol in syms:
                continue
            syms.add(n.symbol)
            if n._e is None:
                continue
            if len(rep) != 0:
                rep.append('; ')
            kind, ops = n._e
            rep.append(f'{n.symbol} = {kind}')

            for op in ops:
                if op.value is not None:
                    rep.append(f' {int(op.value)}')
                else:
                    rep.append(f' {op.symbol}')
            worklist.extend(ops)
        return ''.join(rep)

    class Storage(object):
        def __init__(self, literal_storage, *, image=None):
            self.__literals = literal_storage
            self.__nodes = []
            self.__node_indexes = {}
            if image is not None:
                for s, kind, ops in image:
                    if kind is None:
                        s = self.__literals.get(s)
                        assert ops is None
                        b = Bool.get(s)
                    else:
                        assert s is None
                        b = Bool.from_ops(kind,
                                          *(self.get(op) for op in ops))
                    i = len(self.__nodes)
                    self.__nodes.append(b)
                    self.__node_indexes[i] = b

        def add(self, e):
            if e.value is not None:
                return e.value
            i = self.__node_indexes.get(e.symbol)
            if i is not None:
                return i

            if e._e is None:
                s = self.__literals.add(e.symbol)
                kind = ops = None
            else:
                s = None
                kind, ops = e._e
                ops = tuple(self.add(op) for op in ops)

            i = len(self.__nodes)
            self.__nodes.append((s, kind, ops))
            self.__node_indexes[e.symbol] = i
            return i

        def get(self, image):
            if isinstance(image, bool):
                return TRUE if image else FALSE
            return self.__nodes[image]

        @property
        def image(self):
            return tuple(self.__nodes)

    @staticmethod
    def cast(x):
        return x if isinstance(x, Bool) else Bool.get(x)

    # TODO: Remove and use 'is FALSE' instead.
    def is_trivially_false(self):
        return self.value is False

    # TODO: Remove and use 'is TRUE' instead.
    def is_trivially_true(self):
        return self.value is True

    def __bool__(self):
        assert self.value is not None
        return self.value

    def __int__(self):
        return int(bool(self))

    @property
    def size(self):
        assert 0  # TODO
        return len(str(self))

    def __eq__(self, other):
        assert 0, "Bool's should not be compared; use is_equiv() instead."

    @staticmethod
    def get_or(*args):
        unique_syms = []
        unique_args = []
        for a in args:
            if a.value is not None:
                if a.value is False:
                    continue
                return TRUE
            if (a.__inversion is not None and
                    a.__inversion.symbol in unique_syms):
                return TRUE
            if a.symbol not in unique_syms:
                unique_syms.append(a.symbol)
                unique_args.append(a)

        if len(unique_args) == 0:
            return FALSE
        if len(unique_args) == 1:
            return unique_args[0]

        return __class__.from_ops('or', *unique_args)

    def __or__(self, other):
        return __class__.get_or(self, other)

    @staticmethod
    def get_and(*args):
        unique_syms = []
        unique_args = []
        for a in args:
            if a.value is not None:
                if a.value is True:
                    continue
                return FALSE
            if (a.__inversion is not None and
                    a.__inversion.symbol in unique_syms):
                return FALSE
            if a.symbol not in unique_syms:
                unique_syms.append(a.symbol)
                unique_args.append(a)

        if len(unique_args) == 0:
            return TRUE
        if len(unique_args) == 1:
            return unique_args[0]

        return __class__.from_ops('and', *unique_args)

    def __and__(self, other):
        return __class__.get_and(self, other)

    def __invert__(self):
        if self.__inversion is None:
            self.__inversion = __class__.from_ops('not', self)
            self.__inversion.__inversion = self

        return self.__inversion

    @staticmethod
    def ifelse(cond, a, b):
        if cond.value is not None:
            return a if cond.value else b
        if a.value is not None:
            return (cond | b) if a.value else (~cond & b)
        if b.value is not None:
            return (~cond | a) if b.value else (cond & a)

        if a is b:
            # cond ? a : a
            return a
        if a is b.__inversion:
            # cond ? a : ~a
            return __class__.get_eq(cond, a)

        if cond is a or cond is b.__inversion:
            #  a ? a : b
            # ~b ? a : b
            return a | b
        if cond is b or cond is a.__inversion:
            #  b ? a : b
            # ~a ? a : b
            return a & b

        return __class__.from_ops('ifelse', cond, a, b)

    @staticmethod
    def get_eq(a, b):
        if a is b:
            return TRUE
        if a is b.__inversion:
            return FALSE
        if a.value is not None:
            return b if a.value else ~b
        if b.value is not None:
            return a if b.value else ~a

        return __class__.from_ops('eq', a, b)

    @staticmethod
    def get_neq(a, b):
        if a is b:
            return FALSE
        if a is b.__inversion:
            return TRUE
        if a.value is not None:
            return ~b if a.value else b
        if b.value is not None:
            return ~a if b.value else a

        return __class__.from_ops('neq', a, b)

    def is_equiv(self, other):
        e = Bool.get_neq(self, other)
        if e.value is not None:
            return (e.value is False)

        key = e.symbol
        equiv = __class__.__equiv_cache.get(key)
        if equiv is not None:
            return equiv

        with Status.do('is_equiv', '--show-is-equiv'):
            equiv = (pycosat.solve(e.sat_clauses) == 'UNSAT')

        __class__.__equiv_cache[key] = equiv
        return equiv

    def simplified_sexpr(self):
        cache = {}

        def get(n):
            if n.value is not None:
                return z3.BoolVal(n.value)
            if n._e is None:
                return z3.Bool(n.symbol.id)

            r = cache.get(n.symbol)
            if r is not None:
                return r

            kind, ops = n._e
            OPS = {'or': z3.Or, 'and': z3.And, 'not': z3.Not,
                   'ifelse': z3.If}
            r = cache[n.symbol] = OPS[kind](*(get(op) for op in ops))
            return r

        return z3.Tactic('qe2').apply(get(self)).as_expr().sexpr()

    # TODO: Remove.
    def reduced(self):
        # TODO: It seems we do much faster without trying to
        # reduce at every step.
        return self

        if self.value is not None:
            return self

        with Status.do('reduce', '--show-reduce'):
            simplified = self.simplified()
            for c in (FALSE, TRUE):
                if simplified.is_equiv(c):
                    return c

        return simplified


FALSE = Bool.get(False)
TRUE = Bool.get(True)


class Bits(object):
    def __init__(self, bits, width=None, suffix=None):
        if isinstance(bits, int):
            assert suffix is None
            n = bits
            bits = []
            while n:
                bits.append(n & 1)
                n >>= 1

        if isinstance(bits, str):
            assert width is not None
            suffix = '' if suffix is None else suffix
            bits = tuple(f'{bits}{i}{suffix}' for i in range(width))
        elif suffix is not None:
            bits = tuple(f'{b}{suffix}' if isinstance(b, str) else b
                         for b in bits)

        bits = tuple(Bool.cast(b) for b in bits)

        if width is not None:
            assert len(bits) <= width
            if len(bits) < width:
                bits += (FALSE,) * (width - len(bits))

        self.bits = bits

    @staticmethod
    def cast(x, width=None):
        bits = x if isinstance(x, Bits) else Bits(x, width)

        if width is not None:
            assert bits.width == width

        return bits

    def __getitem__(self, i):
        return self.bits[i]

    @property
    def width(self):
        return len(self.bits)

    @property
    def size_in_bytes(self):
        return _ceil_div(self.width, 8)

    @property
    def value(self):
        if all(b is None for b in self.bits):
            return None

        values = tuple(b.value for b in self.bits)
        if all(v is not None for v in values):
            n = 0
            for i, v in enumerate(values):
                n |= int(v) << i
            return n

        return self.bits

    def __int__(self):
        v = self.value
        assert isinstance(v, int), v  # TODO
        return v

    def __repr__(self):
        return repr(self.bits)

    def __str__(self):
        v = self.value
        if v is None:
            return str(None)

        if isinstance(v, int):
            return '{:0{}x}'.format(v, self.size_in_bytes * 2)

        assert isinstance(v, tuple)
        return '(' + ', '.join(str(b) for b in v) + ')'

    def zero_extended(self, width):
        if self.width >= width:
            return self
        return Bits(self.bits + (FALSE,) * (width - self.width))

    @staticmethod
    def zero_extend_to_same_width(*args):
        w = max(a.width for a in args)
        return (a.zero_extended(w) for a in args)

    def __lshift__(self, n):
        return Bits((FALSE,) * n + self.bits)

    def __or__(self, other):
        a, b = __class__.zero_extend_to_same_width(self, other)
        return Bits((x | y).simplified()
                    for x, y in zip(a.__bits, b.__bits))

    def __eq__(self, other):
        assert isinstance(other, int)  # TODO
        return int(self) == other


class Node(object):
    def __init__(self, index, pull, *, custom_id=None, state=None):
        self.custom_id = custom_id
        assert pull is None or isinstance(pull, Bool), pull
        self.index, self.pull = index, pull
        self.state = state

        # These are not sets as we want reproducible behaviour.
        self.gate_of = []
        self.conn_of = []

    def __repr__(self):
        return self.id

    def __lt__(self, other):
        return self.index < other.index

    class Storage(object):
        def __init__(self, bool_storage, *, image=None):
            assert image is None or len(image) == 0, image
            self.__bools = bool_storage

        def add(self, n):
            pull = None if n.pull is None else self.__bools.add(n.pull)
            state = None if n.state is None else self.__bools.add(n.state)
            return pull, state

        def get(self, index, image):
            pull, state = image
            pull = None if pull is None else self.__bools.get(pull)
            state = None if state is None else self.__bools.get(state)
            return Node(index, pull, state=state)

        @property
        def image(self):
            return ()

    @property
    def id(self):
        if self.pull is None:
            pull = 'n'
        else:
            PULL_SIGNS = {None: 'x', False: 'm', True: 'p'}
            pull = PULL_SIGNS[self.pull.value]

        if self.custom_id is None:
            return f'{pull}{self.index}'

        if self.pull is None:
            return f'{self.custom_id}'

        return f'{pull}.{self.custom_id}'

    @property
    def used_in(self):
        return self.conn_of + self.gate_of

    @property
    def is_gnd(self):
        return self.custom_id == _GND_ID

    @property
    def is_pwr(self):
        return self.custom_id == _PWR_ID

    @property
    def is_gnd_or_pwr(self):
        return self.custom_id in (_GND_ID, _PWR_ID)

    @property
    def is_pin(self):
        # TODO: Mark pins explicitly.
        return self.custom_id is not None


class Transistor(object):
    def __init__(self, index, gate, c1, c2):
        self.index, self.gate, self.c1, self.c2 = index, gate, c1, c2

    def __repr__(self):
        return f'{self.c1} = {self.c2} [{self.gate}]  # {self.id}'

    def __lt__(self, other):
        return self.index < other.index

    @property
    def id(self):
        return f't{self.index}'

    @property
    def image(self):
        return self.gate.index, self.c1.index, self.c2.index

    @staticmethod
    def from_image(index, image, nodes):
        gate, c1, c2 = image
        gate = nodes[gate]
        c1 = nodes[c1]
        c2 = nodes[c2]

        t = Transistor(index, gate, c1, c2)
        gate.gate_of.append(t)
        c1.conn_of.append(t)
        c2.conn_of.append(t)
        return t

    @property
    def conns(self):
        return self.c1, self.c2

    def get_other_conn(self, n):
        assert n in self.conns
        return self.c1 if n is self.c2 else self.c2


def _make_image(nodes, trans):
    literals = Literal.Storage()
    bools = Bool.Storage(literals)
    node_storage = Node.Storage(bools)

    # TODO: Move this logic into Node.Storage.
    nodes, trans = sorted(nodes), sorted(trans)
    node_names = tuple((n.index, n.custom_id) for n in nodes
                       if n.custom_id is not None)
    nodes = tuple((n.index,) + node_storage.add(n) for n in nodes)
    trans = tuple((t.index,) + t.image for t in trans)

    return (node_storage.image, bools.image, literals.image,
            node_names, nodes, trans)


def _load_initial_image():
    __nodes_by_name = {}
    __nodes = {}
    __trans = {}

    def load_node_names():
        RENAMED_NODES = {
            # CLK is an active-low pin.
            'clk': '~clk',

            # TODO: These two refer to the same node.
            'pla33': 'pla33?',
            'pla37': 'pla33?',

            'vss': _GND_ID,
            'vcc': _PWR_ID,
            'wait': '~wait',
            'int': '~int',
            'irq': '~int',
            '~irq': '~int',
            'nmi': '~nmi',
            'busrq': '~busrq',
        }

        # In nodenames.js the alternative registers are confused
        # with the primary ones.
        for r in 'bcdehl':
            # Also, hl is confused with de.
            r2 = {'h': 'd', 'l': 'e', 'd': 'h', 'e': 'l'}.get(r, r)
            for i in range(8):
                a, b = f'reg_{r}{i}', f'reg_{r2}{r2[-1]}{i}'
                RENAMED_NODES[a] = b
                RENAMED_NODES[b] = a

        with open('nodenames.js') as f:
            for line in f:
                line = line.rstrip()
                if line.startswith('// '):
                    continue
                if line in ('var nodenames ={', '}'):
                    continue

                assert line.endswith(',')
                line = line[:-1]
                fields = line.split(':')

                id, i = fields
                assert isinstance(id, str)
                if id[0] == '_':
                    id = '~' + id[1:]
                id = RENAMED_NODES.get(id, id)

                assert isinstance(i, str)
                i = int(i)

                n = __nodes[i]

                assert (id not in __nodes_by_name or
                        __nodes_by_name[id] is n)
                __nodes_by_name[id] = n

                assert n.custom_id is None or n.custom_id == id, id
                n.custom_id = id

    def load_nodes():
        with open('segdefs.js') as f:
            for line in f:
                line = line.rstrip()
                if line in ('var segdefs = [', ']'):
                    continue

                assert line.startswith('['), repr(line)
                fields, = ast.literal_eval(line)

                (i, pull, _) = fields[:3]
                assert isinstance(i, int)
                pull = {'+': TRUE, '-': None}[pull]

                if i in __nodes:
                    n = __nodes[i]
                else:
                    n = Node(i, pull)
                    __nodes[i] = n

                assert ((n.pull is None and pull is None) or
                        n.pull.is_equiv(pull))

    def load_transistors():
        def add(t):
            assert index not in __trans
            __trans[index] = t

            if t not in gate.gate_of:
                t.gate.gate_of.append(t)
            if t not in c1.conn_of:
                t.c1.conn_of.append(t)
            if t not in c2.conn_of:
                t.c2.conn_of.append(t)

        def remove(t):
            del __trans[t.index]

            t.gate.gate_of.remove(t)
            t.c1.conn_of.remove(t)
            t.c2.conn_of.remove(t)

        with open('transdefs.js') as f:
            known = set()
            for line in f:
                line = line.rstrip()
                if line in ('var transdefs = [', ']'):
                    continue

                assert line.startswith('['), repr(line)
                line = line.replace('false', 'False').replace('true', 'True')
                fields, = ast.literal_eval(line)

                (id, gate, c1, c2, bb, _, weak) = fields
                assert isinstance(id, str)
                assert isinstance(gate, int)
                assert isinstance(c1, int)
                assert isinstance(c2, int)
                assert isinstance(weak, bool)

                assert id[0] == 't'
                index = int(id[1:])
                del id

                gate = __nodes[gate]
                c1 = __nodes[c1]
                c2 = __nodes[c2]

                # Skip duplicate transistors. Now that the
                # simulation code does not rely on counting the
                # number of connections, duplicate transistors
                # can be ignored.
                key = gate, c1, c2
                if key in known:
                    continue

                known.add(key)

                # TODO: The comment in the original source says to
                # 'ignore all the 'weak' transistors for now'.
                if weak:
                    continue

                # Skip meaningless transistors, e.g., t251(gnd, gnd, gnd).
                if c1 is c2:
                    assert c1.is_gnd
                    continue

                # TODO: Why the original source does this?
                if c1.is_gnd_or_pwr:
                    assert not c2.is_gnd_or_pwr, (c1, c2)
                    c1, c2 = c2, c1

                add(Transistor(index, gate, c1, c2))

        # Remove meaningless transistors.
        for n in __nodes.values():
            if len(n.used_in) == 1 and not n.is_pin:
                remove(*n.used_in)

    def load_defs():
        load_nodes()
        load_node_names()
        load_transistors()

        # Remove unused nodes.
        nodes = {
            i: n for i, n in __nodes.items()
            if len(n.conn_of) > 0 or len(n.gate_of) > 0}

        return _make_image(nodes.values(), __trans.values())

    return load_defs()


class Z80Simulator(object):
    @property
    def image(self):
        return _make_image(self.__nodes.values(), self.__trans.values())

    def __restore_nodes_from_image(self, names, image, node_storage):
        self.__nodes = {}
        for i in image:
            n = node_storage.get(i[0], i[1:])
            self.__nodes[n.index] = n

        self.__nodes_by_name = {}
        for index, name in names:
            assert name is not None
            n = self.__nodes[index]
            n.custom_id = name
            self.__nodes_by_name[name] = n

    def __restore_transistors_from_image(self, image):
        self.__trans = {}
        for i in image:
            index, i = i[0], i[1:]
            t = Transistor.from_image(index, i, self.__nodes)
            self.__trans[index] = t

    def get_node_states(self, ids):
        return {id: self.__nodes_by_name[id].state for id in ids}

    def __identify_group_of(self, n):
        group = []
        worklist = [n]
        while worklist:
            n = worklist.pop()
            if n in group:
                continue

            group.append(n)

            if not n.is_gnd_or_pwr:
                for t in n.conn_of:
                    if not t.gate.state.is_trivially_false():
                        worklist.append(t.get_other_conn(n))

        return group

    class __PredicateEvaluator(object):
        def __init__(self, group, get_pred):
            self.__preds = {}
            self.__group = tuple(group)
            self.__get_pred = get_pred

            # For every node of the group, find all gated
            # connections through which the nodes-to-predicate
            # can be reachable.
            self.__conns = {}
            worklist = [n for n in self.__group
                        if self.__get_pred(n) is not None]
            processed = set()
            while worklist:
                n = worklist.pop()
                if n in processed:
                    continue

                processed.add(n)

                for t in n.conn_of:
                    if t.gate.state.is_trivially_false():
                        continue

                    m = t.get_other_conn(n)
                    if m.is_gnd_or_pwr or m not in self.__group:
                        continue

                    conns = self.__conns.setdefault(m, [])
                    p = t, n
                    if p not in conns:
                        conns.append(p)
                        worklist.append(m)

        def __build_conn_graph(self, n, stack, evaluated):
            p = evaluated.get(n)
            if p is not None:
                return n, p

            stack.append(n)
            p = tuple(
                (t, self.__build_conn_graph(m, stack, evaluated))
                for t, m in self.__conns.get(n, ())
                if m not in stack and not t.gate.state.is_trivially_false())
            assert stack.pop() == n

            evaluated[n] = p
            return n, p

        def __build_pred(self, g):
            e = self.__preds.get(g)
            if e is not None:
                return e

            n, p = g

            ops = []
            for t, m in p:
                sp = t.gate.state & self.__build_pred(m)
                if sp.is_trivially_true():
                    ops = TRUE
                    break

                ops.append(sp)

            if ops is TRUE:
                e = TRUE
            else:
                e = self.__get_pred(n)
                if e is not None:
                    ops.append(e)

                e = Bool.get_or(*ops)

            self.__preds[g] = e
            return e

        def get_pred_for(self, n):
            g = self.__build_conn_graph(n, stack=[], evaluated={})
            return self.__build_pred(g)

    def __update_group_of(self, n, more, updated):
        pulldown_cache = {}

        def get_gnd_pred(n):
            return TRUE if n.is_gnd else None

        def get_pwr_pred(n):
            return TRUE if n.is_pwr else None

        def get_pullup_pred(n):
            if n.pull is None:
                return None

            assert isinstance(n.pull, Bool)
            return n.pull

        def get_pulldown_pred(n):
            if n.pull is None:
                return None

            assert isinstance(n.pull, Bool)
            p = pulldown_cache.get(n)
            if p is not None:
                return p

            pulldown_cache[n] = p = ~n.pull
            return p

        Preds = __class__.__PredicateEvaluator
        group = self.__identify_group_of(n)
        gnd_preds = Preds(group, get_gnd_pred)
        pwr_preds = Preds(group, get_pwr_pred)
        pullup_preds = Preds(group, get_pullup_pred)
        pulldown_preds = Preds(group, get_pulldown_pred)

        # Update node and transistor states.
        for i, n in enumerate(group):
            with Status.do(f'update {i}/{len(group)} {n}', '--show-nodes'):
                gnd = gnd_preds.get_pred_for(n)
                pwr = pwr_preds.get_pred_for(n)
                pullup = pullup_preds.get_pred_for(n)
                pulldown = pulldown_preds.get_pred_for(n)

                floating = n.state
                pull = Bool.ifelse(pulldown | pullup, ~pulldown, floating)
                state = Bool.ifelse(gnd | pwr, ~gnd, pull).reduced()

                # No further propagation is necessary if the state of
                # the transistor is known to be same. This includes
                # the case of a floating gate.
                if not state.is_equiv(n.state):
                    n.state = state
                    for t in n.gate_of:
                        for c in t.conns:
                            if c not in more:
                                more.append(c)

                updated.add(n)

    def __update_nodes(self, nodes):
        round = 0
        while nodes:
            round += 1
            assert round < 100, 'Loop encountered!'

            with Status.do(f'round {round}'):
                updated = set()
                more = []
                for i, n in enumerate(nodes):
                    with Status.do(f'node {i}/{len(nodes)}'):
                        if n not in updated:
                            self.__update_group_of(n, more, updated)
                nodes = more

    def __set_node_pull(self, n, pull):
        pull = Bool.cast(pull)
        if '--show-set-nodes' in sys.argv:
            Status.print(n, pull)
        n.pull = pull

    def __set_node(self, n, pull):
        self.__set_node_pull(n, pull)
        self.__update_nodes([n])

    def half_tick(self):
        if self.__memory is not None and self.clk:
            if self.mreq and not self.rfsh and not self.iorq:
                if self.m1 and self.rd and self.t2:
                    self.dbus = self.__memory[int(self.abus)]

        self.nclk = ~self.nclk

        # self.__print_state()

    def __tick(self):
        self.half_tick()
        self.half_tick()

    def clear_state(self):
        for n in self.__nodes.values():
            n.state = FALSE

        self.__gnd.state = FALSE
        self.__pwr.state = TRUE

    __DEFAULT_RESET_PROPAGATION_DELAY = 31

    def reset(self, propagation_delay=__DEFAULT_RESET_PROPAGATION_DELAY,
              waiting_for_m1_delay=None):
        self.nclk = TRUE

        self.nreset = FALSE
        self.nbusrq = TRUE
        self.nint = TRUE
        self.nnmi = TRUE
        self.nwait = TRUE

        # Propagate the reset signal.
        for _ in range(propagation_delay):
            self.half_tick()

        self.nreset = TRUE

        # Wait for the first active ~m1, which is essentially an
        # indication that the reset process is completed.
        if waiting_for_m1_delay is None:
            count = 0
            while self.__nm1.state:
                self.half_tick()
                count += 1
            if '--show-waiting-for-m1-delay' in sys.argv:
                print('waiting-for-m1 delay:', count)
        else:
            for _ in range(waiting_for_m1_delay):
                self.half_tick()

    def __init__(self, *, memory=None, skip_reset=None, image=None):
        if image is None:
            s = State()
            s.clear_state()
            s.update_all_nodes()

            if not skip_reset:
                s.reset(__class__.__DEFAULT_RESET_PROPAGATION_DELAY,
                        waiting_for_m1_delay=None)

            s.cache()
            image = s.image
        else:
            # For custom images, leave them as-is.
            assert skip_reset is None

        (node_storage, bools, literals,
         node_names, nodes, trans) = image

        literals = Literal.Storage(image=literals)
        bools = Bool.Storage(literals, image=bools)
        node_storage = Node.Storage(bools, image=node_storage)

        self.__restore_nodes_from_image(node_names, nodes, node_storage)
        self.__restore_transistors_from_image(trans)

        self.__gnd = self.__nodes_by_name[_GND_ID]
        self.__pwr = self.__nodes_by_name[_PWR_ID]
        self.__gnd_pwr = self.__gnd, self.__pwr

        self.__nclk = self.__nodes_by_name['~clk']

        self.__nbusrq = self.__nodes_by_name['~busrq']
        self.__nint = self.__nodes_by_name['~int']
        self.__niorq = self.__nodes_by_name['~iorq']
        self.__nm1 = self.__nodes_by_name['~m1']
        self.__nmreq = self.__nodes_by_name['~mreq']
        self.__nnmi = self.__nodes_by_name['~nmi']
        self.__nrd = self.__nodes_by_name['~rd']
        self.__nreset = self.__nodes_by_name['~reset']
        self.__nrfsh = self.__nodes_by_name['~rfsh']
        self.__nwait = self.__nodes_by_name['~wait']

        self.__t1 = self.__nodes_by_name['t1']
        self.__t2 = self.__nodes_by_name['t2']
        self.__t3 = self.__nodes_by_name['t3']
        self.__t4 = self.__nodes_by_name['t4']
        self.__t5 = self.__nodes_by_name['t5']
        self.__t6 = self.__nodes_by_name['t6']

        if memory is None:
            self.__memory = None
        else:
            self.__memory = bytearray(0x10000)
            self.__memory[:len(memory)] = memory

    @property
    def nclk(self):
        return self.__nclk.state

    @nclk.setter
    def nclk(self, state):
        self.__set_node(self.__nclk, state)

    @property
    def clk(self):
        return ~self.nclk

    @property
    def niorq(self):
        return self.__niorq.state

    @property
    def iorq(self):
        return ~self.niorq

    @property
    def nm1(self):
        return self.__nm1.state

    @property
    def m1(self):
        return ~self.__nm1.state

    @property
    def nmreq(self):
        return self.__nmreq.state

    @property
    def mreq(self):
        return ~self.nmreq

    @property
    def nreset(self):
        return self.__nreset.state

    @nreset.setter
    def nreset(self, state):
        self.__set_node(self.__nreset, state)

    @property
    def nrfsh(self):
        return self.__nrfsh.state

    @property
    def rfsh(self):
        return ~self.nrfsh

    @property
    def nbusrq(self):
        return self.__nbusrq.state

    @nbusrq.setter
    def nbusrq(self, state):
        self.__set_node(self.__nbusrq, state)

    @property
    def nint(self):
        return self.__nint.state

    @nint.setter
    def nint(self, state):
        self.__set_node(self.__nint, state)

    @property
    def nnmi(self):
        return self.__nnmi.state

    @nnmi.setter
    def nnmi(self, state):
        self.__set_node(self.__nnmi, state)

    @property
    def nrd(self):
        return self.__nrd.state

    @property
    def rd(self):
        return ~self.nrd

    @property
    def nwait(self):
        return self.__nwait.state

    @nwait.setter
    def nwait(self, state):
        self.__set_node(self.__nwait, state)

    @property
    def t1(self):
        return self.__t1.state

    @property
    def t2(self):
        return self.__t2.state

    @property
    def t3(self):
        return self.__t3.state

    @property
    def t4(self):
        return self.__t4.state

    @property
    def t5(self):
        return self.__t5.state

    @property
    def t6(self):
        return self.__t6.state

    def get_node(self, id):
        return self.__nodes_by_name[id]

    def read_nodes(self, id, width=8):
        return Bits(self.__nodes_by_name[f'{id}{i}'].state
                    for i in range(width))

    def __write_bits(self, name, value, width=8):
        for i in range(width):
            self.__set_node(self.__nodes_by_name[name + str(i)],
                            bool((value >> i) & 0x1))

    @property
    def abus(self):
        return self.read_nodes('ab', 16)

    @property
    def dbus(self):
        return self.read_nodes('db')

    @dbus.setter
    def dbus(self, n):
        self.__write_bits('db', n)

    @property
    def a(self):
        return self.read_nodes('reg_a')

    @property
    def r(self):
        return self.read_nodes('reg_r')

    @property
    def pc(self):
        lo = self.read_nodes('reg_pcl')
        hi = self.read_nodes('reg_pch')
        return (hi << 8) | lo

    def set_pin_state(self, pin, state):
        assert pin in _PINS
        self.__nodes_by_name[pin].state = Bool.cast(state)

    def set_pin_pull(self, pin, pull):
        assert pin in _PINS
        self.__set_node_pull(self.__nodes_by_name[pin], pull)

    def update_pin(self, pin):
        n = self.__nodes_by_name[pin]
        self.__update_nodes([n])

    def update_all_nodes(self):
        self.__update_nodes([n for n in self.__nodes.values()
                             if n not in self.__gnd_pwr])

    def dump(self):
        with open('z80.dump', mode='w') as f:
            for t in sorted(self.__trans.values()):
                print(t, file=f)

    def dump_nodes(self):
        with open('z80sim-nodes.dump', 'w') as f:
            for n in sorted(self.__nodes.values()):
                print(n, n.state, file=f)

    def dump_trans(self):
        with open('z80sim-trans.dump', 'w') as f:
            for t in sorted(self.__trans.values()):
                print(t, t.state, file=f)

    def __print_state(self):
        print(f'PC {self.pc}, '
              f'A {self.a}, '
              f'R {self.r}, '
              f'clk {self.clk}, '
              f'abus {self.abus}, '
              f'dbus {self.dbus}, '
              f'm1 {self.m1}, '
              f't1 {self.t1}, '
              f't2 {self.t2}, '
              f't3 {self.t3}, '
              f't4 {self.t4}, '
              f't5 {self.t5}, '
              f't6 {self.t6}, '
              f'rfsh {self.rfsh}, '
              f'rd {self.rd}, '
              f'mreq {self.mreq}')

    # TODO
    def do_something(self):
        for i in range(30 * 10):
            if i > 0 and self.m1 and self.t1:
                print()

            self.half_tick()


class State(object):
    def __init__(self, other=None, *, cache_all_reportable_states=False):
        if other is None:
            self.__current_steps = []
            self.__current_image = None
            self.__new_steps = []
        else:
            self.__current_steps = list(other.__current_steps)
            self.__current_image = other.__current_image
            self.__new_steps = list(other.__new_steps)

        self.__cache_all_reportable_states = cache_all_reportable_states

    @staticmethod
    def __get_cache(steps):
        # Whenever we make changes that invalidate cached states,
        # e.g., the names of the nodes are changed, the version
        # number must be bumped.
        VERSION = 7

        key = VERSION, __class__.__get_steps_image(steps)
        return Cache.get_entry('states', key)

    @staticmethod
    def __get_hash(steps):
        return __class__.__get_cache(steps).hash

    @property
    def hash(self):
        return __class__.__get_hash(self.__current_steps + self.__new_steps)

    @staticmethod
    def __get_steps_image(steps):
        literals = Literal.Storage()
        bools = Bool.Storage(literals)

        def get_step_element_image(e):
            if isinstance(e, (str, int, type(None))):
                return e
            if isinstance(e, Bool):
                return bools.add(e)
            assert 0, (e, steps)

        def get_step_image(step):
            return tuple(get_step_element_image(e) for e in step)

        return (tuple(get_step_image(s) for s in steps),
                bools.image, literals.image)

    @staticmethod
    def __cache_state(steps, image):
        with Status.do('cache state'):
            state = __class__.__get_steps_image(steps), image
            __class__.__get_cache(steps).store(state)

    @staticmethod
    def __try_load_state(steps):
        cache = __class__.__get_cache(steps)
        state = cache.load()
        if state is None:
            return None

        if '--show-loaded-state' in sys.argv:
            Status.print(cache.get_path())

        stored_steps, image = state
        stored_steps = deep_tupilize(stored_steps)
        assert stored_steps == __class__.__get_steps_image(steps)

        return image

    def __apply_new_steps(self):
        missing_steps = []
        while self.__new_steps:
            steps = self.__current_steps + self.__new_steps
            image = __class__.__try_load_state(steps)
            if image is None:
                missing_steps.insert(0, self.__new_steps.pop())
                continue

            self.__current_image = image
            self.__current_steps.extend(self.__new_steps)
            self.__new_steps = []

        if self.__current_image is None:
            assert not self.__current_steps
            self.__current_image = (
                __class__.__try_load_state(self.__current_steps))

            if self.__current_image is None:
                self.__current_image = _load_initial_image()

                # Always store the initial image.
                __class__.__cache_state(self.__current_steps,
                                        self.__current_image)

        if not missing_steps:
            return

        sim = Z80Simulator(image=self.__current_image)

        while missing_steps:
            step = missing_steps.pop(0)
            self.__apply_step(sim, step)
            self.__current_steps.append(step)

        self.__current_image = sim.image

    @property
    def image(self):
        self.__apply_new_steps()
        return self.__current_image

    def get_node_states(self, ids):
        return Z80Simulator(image=self.image).get_node_states(ids)

    def __apply_step(self, sim, step):
        kind = step[0]
        if kind == 'clear_state':
            _, = step
            sim.clear_state()
        elif kind == 'set_pin_state':
            _, pin, state = step
            sim.set_pin_state(pin, state)
        elif kind == 'set_pin_pull':
            _, pin, pull = step
            sim.set_pin_pull(pin, pull)
        elif kind == 'update_pin':
            _, pin = step
            sim.update_pin(pin)
        elif kind == 'update_all_nodes':
            _, = step
            sim.update_all_nodes()
        elif kind == 'reset':
            _, propagation_delay, waiting_for_m1_delay = step
            sim.reset(propagation_delay, waiting_for_m1_delay)
        elif kind == 'half_tick':
            _, = step
            sim.half_tick()
        else:
            assert 0, step

    def clear_state(self):
        self.__new_steps.append(('clear_state',))

    def set_pin_state(self, pin, state):
        step = 'set_pin_state', pin, Bool.cast(state)
        self.__new_steps.append(step)

    def set_pin_pull(self, pin, pull):
        step = 'set_pin_pull', pin, Bool.cast(pull)
        self.__new_steps.append(step)

    def update_pin(self, pin):
        self.__new_steps.append(('update_pin', pin))

    def set_pin_and_update(self, pin, pull):
        self.set_pin_pull(pin, pull)
        self.update_pin(pin)

    def set_pins_and_update(self, pin, bits):
        for i, b in enumerate(Bits.cast(bits).bits):
            self.set_pin_and_update(f'{pin}{i}', b)

    def set_db(self, bits):
        if isinstance(bits, str):
            bits = Bits(bits, width=8)
        else:
            bits = Bits.cast(bits, width=8)
        self.set_pins_and_update('db', bits)

    def set_db_and_wait(self, bits, ticks):
        self.set_db(bits)
        self.ticks(ticks)

    def update_all_nodes(self):
        self.__new_steps.append(('update_all_nodes',))

    def reset(self, propagation_delay, waiting_for_m1_delay):
        self.__new_steps.append(('reset', propagation_delay,
                                 waiting_for_m1_delay))

    def half_tick(self):
        self.__new_steps.append(('half_tick',))

    def tick(self):
        self.half_tick()
        self.half_tick()

    def ticks(self, n):
        for _ in range(n):
            self.tick()

    def cache(self):
        steps = self.__current_steps + self.__new_steps
        if not self.__get_cache(steps).exists():
            self.__apply_new_steps()
            __class__.__cache_state(self.__current_steps,
                                    self.__current_image)

    def report(self, id):
        if self.__cache_all_reportable_states:
            self.cache()

        if f'--{id}' not in sys.argv:
            return

        self.cache()

        # Generate image before printing results.
        image = self.image

        Status.print(id)
        Status.print(self.hash)

        s = Z80Simulator(image=image)

        def print_bit(id, with_pull=False):
            n = s.get_node(id)
            Status.print(f'  {id}: {n.state.simplified_sexpr()}')
            if with_pull:
                Status.print(f'  {id} pull: {n.pull.simplified_sexpr()}')

        def print_bits(id, width):
            bits = s.read_nodes(id, width)
            if not isinstance(bits.value, tuple):
                Status.print(f'  {id}: {bits}')
                return

            for i in reversed(range(bits.width)):
                Status.print(f'  {id}{i}: {bits[i].simplified_sexpr()}')

        for pin in _PINS:
            print_bit(pin, with_pull=True)

        for r in ('reg_pch', 'reg_pcl',
                  'reg_sph', 'reg_spl',
                  'reg_b', 'reg_c', 'reg_d', 'reg_e',
                  'reg_h', 'reg_l', 'reg_a', 'reg_f',
                  'reg_ixh', 'reg_ixl', 'reg_iyh', 'reg_iyl',
                  'reg_i', 'reg_r', 'reg_w', 'reg_z',
                  'reg_bb', 'reg_cc', 'reg_dd', 'reg_ee',
                  'reg_hh', 'reg_ll', 'reg_aa', 'reg_ff'):
            print_bits(r, 8)

        # TODO: flipflop int_disabled
        # TODO: flipflop halted
        # TODO: flipflop iff1, iff2
        # TODO: int_mode im
        # TODO: The SCF/SCC flag discovered by Patrik Rak, see
        #       https://github.com/kosarev/z80/issues/42
        # TODO: ~int
        # TODO: ~nmi
        # TODO: ~wait
        # TODO: ~reset -- whether any other definable nodes and
        #       pins leak to the state after reset.
        # TODO: ~busrq (?)

        if '--dump-nodes' in sys.argv:
            s.dump_nodes()

        if '--dump-trans' in sys.argv:
            s.dump_trans()

        Status.clear()
        sys.exit()


def test_node(instrs, n, states_before, states_after):
    a = states_before[n]
    b = states_after[n]

    def check(x):
        if b.is_equiv(x):
            return

        Status.clear()

        f = sys.stderr
        print('; '.join(instrs), n, file=f)
        print('  old state', a.simplified_sexpr(), file=f)
        print('  new state', b.simplified_sexpr(), file=f)
        print('  expected', x.simplified_sexpr(), file=f)

        assert 0

    CF = 'reg_f0'
    XF = 'reg_f3'
    YF = 'reg_f5'

    phase = len(instrs)
    instr = instrs[-1]

    prev_instr = None
    prev_mnemonic = None
    if len(instrs) >= 2:
        prev_instr = instrs[-2]
        prev_mnemonic = prev_instr.split()[0]

    if n == CF:
        if instr == 'ccf':
            return check(~a)

        if instr == 'pop af':
            return check(Bool.get(f'f0_{phase}'))

    if n in (XF, YF):
        if instr == 'ccf':
            an = states_before[f'reg_a{n[-1]}']
            if prev_instr == 'ccf':
                return check(an)
            elif prev_mnemonic in (None, 'nop', 'pop', 'ld'):
                fn = states_before[f'reg_f{n[-1]}']
                return check(an | fn)

        if instr == 'pop af':
            id = {XF: f'f3_{phase}', YF: f'f5_{phase}'}[n]
            return check(Bool.get(id))

    check(a)


def process_instr(instrs, base_state, *, test=False):
    # Additional ticks are necessary for new values
    # to reach their nodes.
    EXTRA_TICKS = 3

    TESTED_NODES = 'reg_f0', 'reg_f3', 'reg_f5'
    SAMPLED_NODES = TESTED_NODES + ('reg_a3', 'reg_a5')

    phase = len(instrs)
    id, cycles = instrs[-1]

    s = State(base_state)
    for cycle_no, (d, ticks) in enumerate(cycles):
        if not isinstance(d, int):
            d = Bits(d, width=8, suffix=f'_{phase}')
        s.set_db(d)
        for t in range(ticks):
            if cycle_no == 0 and t == EXTRA_TICKS:
                states_before = s.get_node_states(SAMPLED_NODES)
                s.cache()

            s.tick()
        s.cache()

    after_instr_state = State(s)

    if not test:
        return after_instr_state

    instr_ids = tuple(id for id, cycles in instrs)
    Status.print('; '.join(instr_ids))

    s.set_db(0x00)  # nop
    for t in range(ticks, ticks + EXTRA_TICKS):
        s.tick()
    s.cache()
    states_after = s.get_node_states(SAMPLED_NODES)

    for id in TESTED_NODES:
        with Status.do(f'test {id}'):
            test_node(instr_ids, id, states_before, states_after)

    return after_instr_state


def build_reset_state():
    s = State(cache_all_reportable_states=True)
    s.clear_state()
    s.report('after-clearing-state')

    for pin in _PINS:
        s.set_pin_state(pin, f'init.{pin}')
        if pin == '~clk':
            # The ~clk pull seems to propagate to lots of nodes,
            # including register bits, and survives the reset
            # sequence and even execution of some instructions.
            # As we are not currently interested in investigating
            # these effects, let's start from inactive ~clk as it
            # seems it is expected to be in hardware implementations.
            s.set_pin_pull(pin, TRUE)
        else:
            s.set_pin_pull(pin, f'pull.{pin}')
    s.update_all_nodes()
    s.report('after-updating-all-nodes')

    s.reset(propagation_delay=31, waiting_for_m1_delay=5)
    s.report('after-reset')

    return s


def build_symbolised_state():
    SYMBOLISING_SEQ = (
        ('exx', ((0xd9, 4),)),
        ('pop bc2', ((0xc1, 4), ('cc', 3), ('bb', 3))),
        ('pop de2', ((0xd1, 4), ('ee', 3), ('dd', 3))),
        ('pop hl2', ((0xe1, 4), ('ll', 3), ('hh', 3))),
        ('exx', ((0xd9, 4),)),

        ('pop ix', ((0xdd, 4), (0xe1, 4), ('ixl', 3), ('ixh', 3))),
        ('pop iy', ((0xfd, 4), (0xe1, 4), ('iyl', 3), ('iyh', 3))),
        ('pop bc', ((0xc1, 4), ('c', 3), ('b', 3))),
        ('pop de', ((0xd1, 4), ('e', 3), ('d', 3))),
        ('pop hl', ((0xe1, 4), ('l', 3), ('h', 3))),

        ('ex af, af2', ((0x08, 4),)),
        ('pop af2', ((0xf1, 4), ('ff', 3), ('aa', 3))),
        ('ex af, af2', ((0x08, 4),)),

        ('ld sp, <sp>', ((0x31, 4), ('spl', 3), ('sph', 3))),

        ('im <im>', ((0xed, 4),
                     (reversed((0, 1, 0, 'im1', 'im0', 1, 1, 0)), 4))),

        ('ei/di', ((reversed((1, 1, 1, 1, 'ei', 0, 1, 1)), 4),)),

        ('ld a, <ri>', ((0x3e, 4), ('ri', 3))),
        ('ld i, a', ((0xed, 4), (0x47, 5))),

        ('ld a, <rr>', ((0x3e, 4), ('rr', 3))),
        ('ld r, a', ((0xed, 4), (0x4f, 5))),

        # Drops cf for the following 'jp c, <wz>'.
        ('xor a', ((0xaf, 4),)),

        # Updates wz.
        ('jp <pc>', ((0xc3, 4), ('pcl', 3), ('pch', 3))),

        ('jp c, <wz>', ((0xda, 4), ('z', 3), ('w', 3))),

        ('pop af', ((0xf1, 4), ('f', 3), ('a', 3))),
    )

    s = build_reset_state()
    for instr, cycles in SYMBOLISING_SEQ:
        with Status.do(instr):
            for d, ticks in cycles:
                s.set_db_and_wait(d, ticks)
                s.cache()
    s.report('after-symbolising')

    return s


def test_instr_seq(seq):
    Literal.clear()
    Bool.clear()
    gc.collect()

    state = build_symbolised_state()
    for i in range(len(seq) - 1):
        state = process_instr(seq[:i + 1], state)
    process_instr(seq, state, test=True)


def test_instr_seq_concurrently(seq):
    with Status.suppress():
        try:
            test_instr_seq(seq)
        except Exception:
            return traceback.format_exc()
    return '; '.join(id for id, cycles in seq)


def test_instr_seqs(seqs):
    if '--single-thread' in sys.argv:
        for seq in seqs:
            test_instr_seq(seq)
    else:
        with concurrent.futures.ProcessPoolExecutor() as e:
            for res in e.map(test_instr_seq_concurrently, seqs):
                Status.print(res)


def get_instrs():
    AT_HL = '(hl)', '110'

    RS = (('{b, c, d, e}', '0ss'),
          ('{l, a}', '1s1'),
          ('h', '100'),
          AT_HL)

    RD = (('{b, c, d, e}', '0dd'),
          ('{l, a}', '1d1'),
          ('h', '100'),
          AT_HL)

    def pattern(s):
        bits = []
        i = 0
        for b in reversed(s):
            if b == ' ':
                continue
            if b in ('0', '1'):
                b = int(b)
            else:
                b = f'{b}{i}'
            bits.append(b)
            i += 1
        return bits

    def f(p):
        return p, 4

    def r3(p):
        return p, 3

    def w3(p):
        return p, 3

    R3 = 'r', 3
    W3 = 'w', 3

    yield 'nop', (f(0x00),)

    for rd in RD:
        rdn, rdp = rd
        for rs in RS:
            rsn, rsp = rs
            cycles = (f(pattern(f'01 {rdp} {rsp}')),)
            if rd == AT_HL and rs == AT_HL:
                instr = 'halt'
            else:
                instr = f'ld {rdn}, {rsn}'
                if rs == AT_HL:
                    cycles += (R3,)
                if rd == AT_HL:
                    cycles += (W3,)
            yield instr, cycles

    yield 'ccf', (f(0x3f),)
    yield 'pop af', (f(0xf1), r3('f'), r3('a'))


def test_instructions():
    instrs = list(get_instrs())
    test_instr_seqs((i,) for i in instrs)
    test_instr_seqs((i1, i2) for i1 in instrs for i2 in instrs)

    Status.clear()
    print('OK')


def build_symbolic_states():
    s = build_reset_state()

    # Feed it a nop.
    nop = State(s)
    nop.set_db_and_wait(0x00, 4)
    nop.report('after-nop')
    del nop

    # ld a, i8  f(4) f(5)
    ld = State(s)
    ld.set_db_and_wait(0x3e, 4)
    ld.set_db_and_wait('imm', 5)
    ld.report('after-ld-a-i8')
    del ld


def test_computing_node_values():
    # With the old function computing node values the LSB of the
    # address bus was always to 0 at the fourth half-tick of
    # fetch cycles. Make sure with the new logic the address bus
    # maintains the right value.
    s = Z80Simulator(memory=[])
    while s.abus == 0x0000:
        s.half_tick()
    while s.abus == 0x0001:
        s.half_tick()
    assert s.abus == 0x0002

    Status.print('Tests passed.')
    Status.clear()


def play_sandbox():
    memory = [
        # 0x76,  # halt
        0xd3,  # di
        # 0xc5,  # nop
        0x05,  # dec b
        0x0f,  # rrca
        0xf6, 0x40,  # or 0x40
        0xc5,  # push bc
        0x7e,  # ld a, (hl)
        0xfe, 0x0d,  # cp 0x0d
    ]
    s = Z80Simulator(memory=memory)
    s.dump()
    s.do_something()


def main():
    if '--test' in sys.argv:
        test_computing_node_values()
        return

    if '--play-sandbox' in sys.argv:
        play_sandbox()
        return

    build_symbolic_states()

    test_instructions()


if __name__ == "__main__":
    main()
