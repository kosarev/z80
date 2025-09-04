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
#
# Symbolic simulation Copyright (C) 2017-2025 Ivan Kosarev.
# mail@ivankosarev.com
# https://github.com/kosarev/z80

import ast
import datetime
import eqbool
import gc
import gzip
import hashlib
import itertools
import json
import multiprocessing
import os
import pathlib
import platform
import pprint
import random
import resource
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

CF, CF2 = 'reg_f0', 'reg_ff0'
NF, NF2 = 'reg_f1', 'reg_ff1'
PF, PF2 = 'reg_f2', 'reg_ff2'
XF, XF2 = 'reg_f3', 'reg_ff3'
HF, HF2 = 'reg_f4', 'reg_ff4'
YF, YF2 = 'reg_f5', 'reg_ff5'
ZF, ZF2 = 'reg_f6', 'reg_ff6'
SF, SF2 = 'reg_f7', 'reg_ff7'

(CF_BIT, NF_BIT, PF_BIT, XF_BIT,
 HF_BIT, YF_BIT, ZF_BIT, SF_BIT) = range(8)

EX_AF_FF = 'ex_af'
EXX_FF = 'ex_bcdehl'
EX_DE_FF0 = 'ex_dehl0'
EX_DE_FF1 = 'ex_dehl1'
EX_DE_FF = 'ex_dehl_combined'

# TODO: Can also be p1210, p1239 or p231.
# These two are suggested by Tony Brewer.
IFF1 = 'n181'
IFF2 = 'n206'

TESTED_NODES = {IFF1, IFF2, EX_AF_FF, EXX_FF,
                EX_DE_FF0, EX_DE_FF1, EX_DE_FF}
for i in range(8):
    for r in 'af':
        TESTED_NODES.update((f'reg_{r}{i}', f'reg_{r}{r}{i}'))
    for r in 'wz':
        TESTED_NODES.add(f'reg_{r}{i}')
    TESTED_NODES.add(f'instr{i}')


def get_opt(id, type):
    for a in sys.argv:
        if a.startswith(id + '='):
            return type(a.split('=', maxsplit=1)[1])
    return None


SEED = get_opt('--seed', int)


def _ceil_div(a, b):
    return -(a // -b)


def deep_tupilize(x):
    if isinstance(x, (str, int, bool, type(None))):
        return x
    assert isinstance(x, list), x
    return tuple(deep_tupilize(e) for e in x)


class Status(object):
    __start_time = datetime.datetime.now()
    __suppression_count = 0
    __parts = []
    __line = ''

    __profile = '--profile' in sys.argv
    if __profile:
        print(platform.python_implementation(), ' '.join(sys.argv))

    @staticmethod
    def __emit(line):
        if __class__.__suppression_count != 0:
            return

        line_with_spaces = line
        if len(line) < len(__class__.__line):
            line_with_spaces += ' ' * (len(__class__.__line) - len(line))

        print(f'\r{line_with_spaces}', end='', file=sys.stderr)
        __class__.__line = line

    @staticmethod
    def __get_status_prefix():
        d = datetime.datetime.now() - __class__.__start_time
        p = str(datetime.timedelta(seconds=d.seconds))
        if __class__.__profile:
            num_bools = Bool.get_cache_size()
            mem_usage = resource.getrusage(resource.RUSAGE_SELF).ru_maxrss
            p += f' [{num_bools} bools, {mem_usage // 1000} MB]'
        return p

    @staticmethod
    def __update():
        parts = ', '.join(p for p in __class__.__parts if p)
        __class__.__emit(f'{__class__.__get_status_prefix()}  {parts}')

    @staticmethod
    def clear():
        if __class__.__suppression_count != 0:
            return
        __class__.__emit('')
        print('\r', end='', file=sys.stderr)

    @staticmethod
    def print(*args):
        if __class__.__suppression_count != 0:
            return

        line = __class__.__line
        __class__.clear()

        print(f'{__class__.__get_status_prefix()} ', *args)

        __class__.__emit(line)

    @staticmethod
    def print_profile_info():
        if __class__.__profile:
            line = __class__.__line
            __class__.clear()
            print(line)
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
        __class__.__suppression_count += 1

    @staticmethod
    def end_suppression():
        assert __class__.__suppression_count > 0
        __class__.__suppression_count -= 1

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

        def get_path(self, *, intermediate=False, suffix=None):
            path = _CACHE_ROOT

            if intermediate:
                path = path / 'intermediate'

            h = self.hash
            path = path / self.domain / h[:3] / h[3:6]

            if suffix is not None:
                h += suffix
            path = path / h

            return path

        def exists(self, suffix=None):
            return self.get_path(suffix=suffix).exists()

        def load(self):
            for intermediate in (False, True):
                try:
                    path = self.get_path(intermediate=intermediate)
                    with gzip.open(path) as f:
                        return json.loads(f.read().decode())
                except FileNotFoundError:
                    pass
            return None

        def store(self, payload, *, intermediate=False):
            Status.print_profile_info()

            path = self.get_path(intermediate=intermediate)
            path.parent.mkdir(parents=True, exist_ok=True)

            temp_path = path.parent / (
                f'{path.name}-{os.getpid()}.tmp')
            with temp_path.open('wb') as f:
                with gzip.GzipFile('', 'wb', mtime=0, fileobj=f) as gzf:
                    gzf.write(json.dumps(payload).encode())

            temp_path.rename(path)

        def create(self, suffix=None):
            path = self.get_path(suffix)
            path.parent.mkdir(parents=True, exist_ok=True)
            with path.open('w'):
                pass

    @staticmethod
    def get_entry(domain, key):
        return __class__.__Entry(domain, key)


class Bool(eqbool.Bool):
    # TODO: Should all be part of eqbool.Bool?
    __slots__ = 'inversion',

    __FALSE_TRUE = None
    __cache = {}

    @staticmethod
    def clear():
        __class__.__cache.clear()

    @staticmethod
    def get_cache_size():
        return len(__class__.__cache)

    @staticmethod
    def get(term):
        if isinstance(term, int):
            assert term in (0, 1), repr(term)
            term = bool(term)

        assert isinstance(term, (bool, str))
        v = _eqbools.get(term)

        key = v.id
        b = __class__.__cache.get(key)
        if b is not None:
            return b

        if isinstance(term, bool):
            if __class__.__FALSE_TRUE is None:
                false = _eqbools.get(False)
                true = _eqbools.get(True)
                # We want the constants to have the smallest size so
                # that they are always seen the simplest expressions.
                false.inversion = true
                true.inversion = false
                __class__.__FALSE_TRUE = false, true

            false, true = __class__.__FALSE_TRUE
            __class__.__cache[false.id] = false
            __class__.__cache[true.id] = true

            return true if term else false

        assert isinstance(term, str)
        assert term.strip() == term, repr(term)
        assert term.lower() not in ('', '0', '1', 'true', 'false')

        b = v
        b.inversion = None

        __class__.__cache[key] = b

        return b

    @staticmethod
    def __get_id(v):
        if v.is_const:
            return v.kind
        if v.is_term:
            return v.term
        if v.kind == 'not':
            return '~' + __class__.__get_id(~v)
        return f't{v.id}'

    @staticmethod
    def from_ops(kind, *ops):
        if kind == 'not':
            v = _eqbools.get_inversion(ops[0])
        elif kind == 'ifelse':
            # TODO: *ops?
            v = _eqbools.ifelse(ops[0], ops[1], ops[2])
        elif kind == 'and':
            v = _eqbools.get_and(*ops)
        elif kind == 'or':
            v = _eqbools.get_or(*ops)
        elif kind == 'eq':
            # TODO: *ops?
            v = _eqbools.get_eq(ops[0], ops[1])
        else:
            assert 0, kind

        key = v.id
        b = __class__.__cache.get(key)
        if b is not None:
            return b

        b = __class__.__cache[key] = v
        b.inversion = None

        if kind == 'not':
            op, = ops
            b.inversion = op
            op.inversion = b

        # Status.print(repr(b))

        return b

    def __repr__(self):
        v = self  # TODO
        if v.kind in ('false', 'true', 'term', 'not'):
            return __class__.__get_id(v)

        rep = []
        visited = set()
        worklist = [v]
        while worklist:
            v = worklist.pop()
            if v.kind in ('false', 'true', 'term', 'not'):
                continue
            if v.id in visited:
                continue
            visited.add(v.id)
            if v.is_term:
                continue
            if len(rep) != 0:
                rep.append('; ')
            rep.append(f'{__class__.__get_id(v)} = {v.kind}')
            for op in v.args:
                rep.append(f' {__class__.__get_id(op)}')
                worklist.append(op)

        return ''.join(rep)

    def __str__(self):
        return self.simplified_sexpr()

    def __lt__(self, other):
        return self._v.id < other._v.id

    class Storage(object):
        def __init__(self, *, image=None):
            self.__nodes = []
            self.__node_indexes = {}
            if image is not None:
                for kind, ops in image:
                    if kind == 'term':
                        assert isinstance(ops, str)
                        b = Bool.get(ops)
                    else:
                        b = Bool.from_ops(kind,
                                          *(self.get(op) for op in ops))
                    i = len(self.__nodes)
                    self.__nodes.append(b)
                    self.__node_indexes[i] = b

        def __add(self, v):
            kind = v.kind
            if kind in ('false', 'true'):
                return kind == 'true'

            key = v.id
            i = self.__node_indexes.get(key)
            if i is not None:
                return i

            if kind == 'term':
                ops = v.term
            else:
                ops = [_eqbools.get_inversion(v)] if kind == 'not' else v.args
                ops = tuple(self.__add(op) for op in ops)

            i = len(self.__nodes)
            self.__nodes.append((kind, ops))
            self.__node_indexes[key] = i
            return i

        def add(self, e):
            return self.__add(e)

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

    @staticmethod
    def get_or(*args):
        return ~__class__.get_and(*(~a for a in args))

    def __or__(self, other):
        return __class__.get_or(self, other)

    @staticmethod
    def get_and(*args):
        # TODO: unique_ops and unique_args now duplicate each other.
        unique_ops = []
        unique_args = []
        for a in args:
            if a.value is not None:
                if a.value is True:
                    continue
                return FALSE
            if a.inversion in unique_ops:
                return FALSE
            if a not in unique_ops:
                unique_ops.append(a)
                unique_args.append(a)

        if len(unique_args) == 0:
            return TRUE
        if len(unique_args) == 1:
            return unique_args[0]

        return __class__.from_ops('and', *unique_args)

    def __and__(self, other):
        return __class__.get_and(self, other)

    def __xor__(self, other):
        # TODO: Would probably be more logical for neq to rely on xor?
        return __class__.get_neq(self, other)

    def __invert__(self):
        # Status.print(super().__repr__())
        if self.inversion is None:
            return __class__.from_ops('not', self)

        return self.inversion

    @staticmethod
    def ifelse(cond, a, b):
        cond, a, b = __class__.cast(cond), __class__.cast(a), __class__.cast(b)

        if cond.value is not None:
            return a if cond.value else b
        if a.value is not None:
            return (cond | b) if a.value else (~cond & b)
        if b.value is not None:
            return (~cond | a) if b.value else (cond & a)

        if a is b:
            # cond ? a : a
            return a
        if a is b.inversion:
            # TODO: Is this really a simplification?
            #       ifelse(cond, a, ~a) is just the same set of
            #       clauses as eq(cond, a).
            # cond ? a : ~a
            return __class__.get_eq(cond, a)

        if cond is a or cond is b.inversion:
            #  a ? a : b
            # ~b ? a : b
            return a | b
        if cond is b or cond is a.inversion:
            #  b ? a : b
            # ~a ? a : b
            return a & b

        return __class__.from_ops('ifelse', cond, a, b)

    # TODO: Remove ifelse() in favour of this function.
    def xifelse(self, a, b):
        return __class__.ifelse(self, a, b)

    # TODO: This should use ifelse()?
    @staticmethod
    def get_eq(a, b):
        # ifelse takes the same set of clauses as eq, so no need
        # to have a special operation for it.
        return __class__.from_ops('ifelse', a, b, ~b)

    @staticmethod
    def get_neq(a, b):
        return ~__class__.get_eq(a, b)

    @staticmethod
    def is_equiv(a, b):
        return _eqbools.is_equiv(a, b)

    def simplified_sexpr(self):
        cache = {}

        def get(v):
            kind = v.kind
            if kind in ('false', 'true'):
                return z3.BoolVal(kind == 'true')
            if kind == 'term':
                return z3.Bool(v.term)

            key = v.id
            r = cache.get(key)
            if r is not None:
                return r

            if kind == 'eq':
                a, b = v.args
                r = get(a) == get(b)
            else:
                ops = [~v] if kind == 'not' else v.args
                OPS = {'or': z3.Or, 'and': z3.And, 'not': z3.Not,
                       'ifelse': z3.If}
                r = OPS[kind](*(get(op) for op in ops))

            cache[key] = r
            return r

        e = get(self._v)
        for t in ('qe2', 'solver-subsumption') * 3:
            e = z3.Tactic(t).apply(e).as_expr()
        if z3.is_false(e):
            return '0'
        if z3.is_true(e):
            return '1'
        return e.sexpr()


_eqbools = eqbool.Context(bool_type=Bool)


FALSE = Bool.get(False)
TRUE = Bool.get(True)

def test_bools():
    def test(actual, expected):
        if actual is expected:
            return

        print('Actual:  ', actual, repr(actual))
        print('Expected:', expected, repr(expected))
        assert 0

    a, b, c, t = (Bool.get(v) for v in 'abct')
    eq = Bool.get_eq
    ifelse = Bool.ifelse

    # Canonicalise commutative operations.
    test(eq(a, b), eq(b, a))

    test(eq(a, eq(a, b)), b)
    test(eq(eq(a, b), a), b)
    test(eq(a, eq(~a, b)), ~b)
    test(eq(eq(~a, b), a), ~b)

    test(eq(a, ~eq(a, b)), ~b)
    test(eq(~eq(a, b), a), ~b)
    test(eq(a, ~eq(~a, b)), b)
    test(eq(~eq(~a, b), a), b)

    test(eq(~a, ~eq(a, b)), b)
    test(eq(~eq(a, b), ~a), b)
    test(eq(~a, ~eq(~a, b)), ~b)
    test(eq(~eq(~a, b), ~a), ~b)

    test(ifelse(c, ~t, t), eq(c, ~t))

    # This simplification is particularly important for
    # speeding up simulation of conditional half-ticks:
    t2 = ifelse(c, ~t, t)
    test(ifelse(c, ~t2, t2), t)


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
            suffix = '' if suffix is None else f'_{suffix}'
            bits = tuple(f'{bits}_b{i}{suffix}' for i in range(width))
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
            bits = bits.zero_extended(width)
            assert bits.width == width, (width, bits.width, bits)

        return bits

    def __getitem__(self, i):
        return self.bits[i]

    def __iter__(self):
        return (b for b in self.bits)

    @property
    def lsb(self):
        return self.bits[0]

    @property
    def msb(self):
        return self.bits[-1]

    @property
    def width(self):
        return len(self.bits)

    @property
    def size_in_bytes(self):
        return _ceil_div(self.width, 8)

    @property
    def value(self):
        if all(b is None for b in self):
            return None

        values = tuple(b.value for b in self)
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
        return __class__(self.bits + (FALSE,) * (width - self.width))

    @staticmethod
    def zero_extend_to_same_width(*args):
        args = tuple(__class__.cast(a) for a in args)
        w = max(a.width for a in args)
        return (a.zero_extended(w) for a in args)

    def sign_extended(self, width):
        if self.width >= width:
            return self
        return __class__(self.bits + (self.msb,) * (width - self.width))

    def truncated(self, width):
        if self.width <= width:
            return self
        return __class__(self.bits[:width])

    def __lshift__(self, n):
        if isinstance(n, int):
            return __class__((FALSE,) * n + self.bits)

        n = __class__.cast(n)
        if n.width == 0:
            return self

        s = self << __class__(n.bits[:-1])
        return __class__.ifelse(n.msb, s << (1 << (n.width - 1)), s)

    @staticmethod
    def get_mask(n):
        return Bits(1) << n

    @staticmethod
    def get_or(*args):
        args = __class__.zero_extend_to_same_width(*args)
        return __class__(Bool.get_or(*bits) for bits in zip(*args))

    def __or__(self, other):
        return __class__.get_or(self, other)

    def __and__(self, other):
        a, b = __class__.zero_extend_to_same_width(self, other)
        return __class__((x & y) for x, y in zip(a, b))

    def __xor__(self, other):
        a, b = __class__.zero_extend_to_same_width(self, other)
        return __class__((x ^ y) for x, y in zip(a, b))

    def __add__(self, other):
        a, b = __class__.zero_extend_to_same_width(self, other)

        r = []
        cf = FALSE
        for x, y in zip(a, b):
            t = x ^ y
            r.append(t ^ cf)
            cf = (x & y) | (cf & t)
        r.append(cf)
        return __class__(r)

    def __neg__(self):
        return ~self + __class__(1)

    def __sub__(self, other):
        a, b = __class__.zero_extend_to_same_width(self, other)
        return a + (-b)

    def __invert__(self):
        return __class__(~b for b in self)

    def __eq__(self, other):
        a, b = __class__.zero_extend_to_same_width(self, other)
        return Bool.get_and(*(Bool.get_eq(x, y) for x, y in zip(a, b)))

    def __ne__(self, other):
        return ~self.__eq__(other)

    def __ge__(self, other):
        a, b = __class__.zero_extend_to_same_width(self, other)
        return (a - b)[a.width]

    def __le__(self, other):
        return __class__.cast(other) >= self

    @staticmethod
    def ifelse(cond, a, b):
        cond = Bool.cast(cond)
        a, b = __class__.zero_extend_to_same_width(a, b)
        return __class__(Bool.ifelse(cond, x, y) for x, y in zip(a, b))

    @staticmethod
    def concat(*args):
        bits = []
        for a in reversed(args):
            bits.extend(a)
        return __class__(bits)

    def rol(self, n=1):
        return __class__(self.bits[-n:] + self.bits[:-n])

    def ror(self, n=1):
        return __class__(self.bits[n:] + self.bits[:n])

    def parity(self):
        # TODO: Have Bool.get_xor().
        r = TRUE
        for b in self.bits:
            r ^= b
        return r

    def is_any(self, *args):
        return Bool.get_or(*(self == a for a in args))


class Node(object):
    def __init__(self, index, pull, *, custom_id=None):
        self.custom_id = custom_id
        assert pull is None or isinstance(pull, Bool), pull
        self.index, self.pull = index, pull

        # These are not sets as we want reproducible behaviour.
        self.gate_of = []
        self.conn_of = []

        self.group = None

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
            return pull,

        def get(self, index, image):
            pull, = image
            pull = None if pull is None else self.__bools.get(pull)
            return Node(index, pull)

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

        return f'{self.custom_id}'

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
        return self.custom_id in _PINS


class NodeGroup(object):
    def __init__(self, nodes):
        self.nodes = tuple(sorted(nodes))
        self.gates = tuple(n for n in self.nodes if len(n.gate_of) > 0)


class Transistor(object):
    def __init__(self, index, gate, c1, c2, *, state=None):
        self.index, self.gate, self.c1, self.c2 = index, gate, c1, c2
        self.state = state

    def __repr__(self):
        return f'{self.c1} = {self.c2} [{self.gate}]  # {self.id}'

    def __lt__(self, other):
        return self.index < other.index

    @property
    def id(self):
        return f't{self.index}'

    class Storage(object):
        def __init__(self, bool_storage):
            self.__bools = bool_storage

        def add(self, t):
            state = None if t.state is None else self.__bools.add(t.state)
            return t.gate.index, t.c1.index, t.c2.index, state

        def get(self, index, image, nodes):
            gate, c1, c2, state = image
            gate, c1, c2 = nodes[gate], nodes[c1], nodes[c2]
            state = None if state is None else self.__bools.get(state)

            t = Transistor(index, gate, c1, c2, state=state)
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
    bools = Bool.Storage()
    node_storage = Node.Storage(bools)
    trans_storage = Transistor.Storage(bools)

    # TODO: Move this logic into Node.Storage.
    nodes, trans = sorted(nodes), sorted(trans)
    node_names = tuple((n.index, n.custom_id) for n in nodes
                       if n.custom_id is not None)
    nodes = tuple((n.index,) + node_storage.add(n) for n in nodes)
    trans = tuple((t.index,) + trans_storage.add(t) for t in trans)

    return (node_storage.image, bools.image,
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

                assert n.custom_id is None or n.custom_id == id, id
                n.custom_id = id

                if id is not None:
                    assert (id not in __nodes_by_name or
                            __nodes_by_name[id] is n)
                    __nodes_by_name[id] = n

                assert (n.id not in __nodes_by_name or
                        __nodes_by_name[n.id] is n)
                __nodes_by_name[n.id] = n

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
                        Bool.is_equiv(n.pull, pull))

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
            # if Bool.is_equiv(n.state, Bool.get('ei')):
            #     Status.print(n, n.state)

        self.__nodes_by_name = {}
        for index, name in names:
            assert name is not None
            n = self.__nodes[index]
            n.custom_id = name
            self.__nodes_by_name[name] = n

        for n in self.__nodes.values():
            self.__nodes_by_name[n.id] = n

    def __restore_transistors_from_image(self, trans_storage, image):
        self.__trans = {}
        for i in image:
            index, i = i[0], i[1:]
            t = trans_storage.get(index, i, self.__nodes)
            self.__trans[index] = t

    def get_node_states(self, ids=None):
        if ids is None:
            ids = (n.id for n in self.__nodes.values())
        return {id: self.get_node_state(self.__nodes_by_name[id])
                for id in ids}

    def __identify_group_of(self, n):
        nodes = []
        worklist = [n]
        while worklist:
            n = worklist.pop()
            if n.is_gnd_or_pwr or n in nodes:
                continue

            nodes.append(n)

            for t in n.conn_of:
                worklist.append(t.get_other_conn(n))

        return NodeGroup(nodes)

    def __identify_groups(self):
        for n in self.__nodes.values():
            if n.group is not None:
                continue

            group = self.__identify_group_of(n)
            for m in group.nodes:
                m.group = group

        assert self.__gnd.group is None
        assert self.__pwr.group is None

        for t in self.__trans.values():
            if t.c1.is_gnd_or_pwr:
                assert not t.c2.is_gnd_or_pwr
                t.conns_group = t.c2.group
            elif t.c2.is_gnd_or_pwr:
                assert not t.c1.is_gnd_or_pwr
                t.conns_group = t.c1.group
            else:
                assert t.c1.group is t.c2.group
                t.conns_group = t.c1.group

            assert t.conns_group is not None

    def __get_node_preds(self, n, new_states):
        def get_group_pred(n, get_node_pred, stack, preds):
            cyclic = False

            p = preds.get(n)
            if p is not None:
                return cyclic, p

            p = get_node_pred(n)
            if p is TRUE:
                preds[n] = p
                return cyclic, p

            if n.is_gnd_or_pwr:
                assert p is None
                p = FALSE
                preds[n] = p
                return cyclic, p

            p = [] if p is None else [p]
            stack.append(n)
            for t in n.conn_of:
                state = new_states.get(t.gate, t.state)
                if state is not FALSE:
                    m = t.get_other_conn(n)
                    if m in stack:
                        cyclic = True
                    else:
                        cc, pp = get_group_pred(m, get_node_pred, stack, preds)
                        cyclic |= cc

                        mp = state & pp
                        if mp is TRUE:
                            p = [TRUE]
                            break
                        p.append(mp)
            p = Bool.get_or(*p)
            assert stack.pop() == n

            if not cyclic:
                preds[n] = p

            return cyclic, p

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
            return ~n.pull

        _, gnd = get_group_pred(n, get_gnd_pred, [], {})
        _, pwr = get_group_pred(n, get_pwr_pred, [], {})
        _, pullup = get_group_pred(n, get_pullup_pred, [], {})
        _, pulldown = get_group_pred(n, get_pulldown_pred, [], {})

        return gnd, pwr, pullup, pulldown

    def get_node_state(self, n, new_states=None):
        if new_states is None:
            new_states = {}

        gnd, pwr, pullup, pulldown = self.__get_node_preds(n, new_states)

        if len(n.gate_of) == 0:
            floating = Bool.get('<floating-non-gate>')
        else:
            # TODO: For now we assume that all gates of the node
            # are always in the same state.
            floating = new_states.get(n, n.gate_of[0].state)
        pull = Bool.ifelse(pulldown | pullup, ~pulldown, floating)
        return Bool.ifelse(gnd | pwr, ~gnd, pull)

    def __update_gate_state(self, n, new_states):
        assert not n.is_gnd_or_pwr

        old_state = new_states.get(n, n.gate_of[0].state)
        new_state = self.get_node_state(n, new_states)

        # No further propagation is necessary if the state of
        # the transistor is known to be same. This includes
        # the case of a floating gate.
        if Bool.is_equiv(new_state, old_state):
            return False

        new_states[n] = new_state
        return True

    def __update_groups_of(self, nodes):
        # TODO: Does always updating all nodes lead to any failures?
        # nodes = list(self.__nodes.values())

        groups = []
        for n in nodes:
            if n.group not in groups:
                assert n.group is not None, n
                groups.append(n.group)
        del nodes

        round = 0
        while groups:
            round += 1
            assert round < 100, 'Loop encountered!'
            with Status.do(f'round {round}'):
                gates = sum((g.gates for g in groups), start=())
                assert len(gates) == len(set(gates))

                if SEED is not None:
                    gates = list(gates)
                    random.shuffle(gates)

                # Compute new states.
                new_states = {}
                repeat = True
                while repeat:
                    repeat = False
                    for i, n in enumerate(gates):
                        # with Status.do(f'gate {i}/{len(gates)}'):
                        repeat |= self.__update_gate_state(n, new_states)

                # Apply new states.
                groups = []
                for n, state in new_states.items():
                    for t in n.gate_of:
                        t.state = state
                        if t.conns_group not in groups:
                            groups.append(t.conns_group)

    def __set_node_pull(self, n, pull):
        pull = Bool.cast(pull)
        if '--show-set-nodes' in sys.argv:
            Status.print(n, pull)
        n.pull = pull

    def __set_node(self, n, pull):
        self.__set_node_pull(n, pull)
        self.__update_groups_of([n])

    def half_tick(self, *, cond=None):
        if self.__memory is not None and self.clk:
            if self.mreq and not self.rfsh and not self.iorq:
                if self.m1 and self.rd and self.t2:
                    self.dbus = self.__memory[int(self.abus)]

        if cond is None:
            cond = TRUE

        self.nclk = Bool.ifelse(cond, ~self.nclk, self.nclk)

        # self.__print_state()

    def __tick(self):
        self.half_tick()
        self.half_tick()

    def clear_state(self):
        for t in self.__trans.values():
            t.state = FALSE

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
            s.power_up()

            if not skip_reset:
                s.reset(__class__.__DEFAULT_RESET_PROPAGATION_DELAY,
                        waiting_for_m1_delay=None)

            s.cache()
            image = s.image
        else:
            # For custom images, leave them as-is.
            assert skip_reset is None

        (node_storage, bools,
         node_names, nodes, trans) = image

        bools = Bool.Storage(image=bools)
        node_storage = Node.Storage(bools, image=node_storage)
        trans_storage = Transistor.Storage(bools)

        self.__restore_nodes_from_image(node_names, nodes, node_storage)
        self.__restore_transistors_from_image(trans_storage, trans)

        self.__gnd = self.__nodes_by_name[_GND_ID]
        self.__pwr = self.__nodes_by_name[_PWR_ID]
        self.__gnd_pwr = self.__gnd, self.__pwr

        self.__identify_groups()

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
        return self.get_node_state(self.__nclk)

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
        return Bits(self.get_node_state(self.__nodes_by_name[f'{id}{i}'])
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

    def set_pin_pull(self, pin, pull):
        assert pin in _PINS
        self.__set_node_pull(self.__nodes_by_name[pin], pull)

    def update_pin(self, pin):
        n = self.__nodes_by_name[pin]
        self.__update_groups_of([n])

    def power_up(self):
        assert len(self.__gnd.gate_of) == 0
        assert len(self.__pwr.gate_of) == 0
        with Status.do('update nodes'):
            self.__update_groups_of(t.get_other_conn(n)
                                    for n in self.__gnd_pwr
                                    for t in n.conn_of)

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
            self.__steps_status = {}
            self.__status = []
        else:
            self.__current_steps = list(other.__current_steps)
            self.__current_image = other.__current_image
            self.__new_steps = list(other.__new_steps)
            self.__steps_status = dict(other.__steps_status)
            self.__status = list(other.__status)

        self.__cache_all_reportable_states = cache_all_reportable_states
        self.__intermediate_steps = set()

    @staticmethod
    def __get_cache(steps):
        # Whenever we make changes that invalidate cached states,
        # e.g., the names of the nodes are changed, the version
        # number must be bumped.
        VERSION = 10, SEED

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
        bools = Bool.Storage()

        def get_step_element_image(e):
            if isinstance(e, (str, int, type(None))):
                return e
            if isinstance(e, Bool):
                return bools.add(e)
            assert 0, (e, steps)

        def get_step_image(step):
            return tuple(get_step_element_image(e) for e in step)

        return (tuple(get_step_image(s) for s in steps),
                bools.image)

    @staticmethod
    def __cache_state(steps, image, *, intermediate=False):
        with Status.do('cache state'):
            state = __class__.__get_steps_image(steps), image
            __class__.__get_cache(steps).store(state,
                                               intermediate=intermediate)

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
            with Status.do(', '.join(self.__steps_status[id(step)])):
                self.__apply_step(sim, step)
                self.__current_steps.append(step)

                if id(step) in self.__intermediate_steps:
                    __class__.__cache_state(self.__current_steps,
                                            sim.image,
                                            intermediate=True)

        self.__current_image = sim.image

    @property
    def image(self):
        self.__apply_new_steps()
        return self.__current_image

    def get_node_states(self, ids=None):
        return Z80Simulator(image=self.image).get_node_states(ids)

    def __apply_step(self, sim, step):
        kind = step[1]
        if kind == 'clear_state':
            _, _, = step
            sim.clear_state()
        elif kind == 'set_pin_pull':
            _, _, pin, pull = step
            sim.set_pin_pull(pin, pull)
        elif kind == 'update_pin':
            _, _, pin = step
            sim.update_pin(pin)
        elif kind == 'power_up':
            _, _, = step
            sim.power_up()
        elif kind == 'reset':
            _, _, propagation_delay, waiting_for_m1_delay = step
            sim.reset(propagation_delay, waiting_for_m1_delay)
        elif kind == 'half_tick':
            _, _, = step
            sim.half_tick()
        elif kind == 'conditional_half_tick':
            _, _, cond = step
            sim.half_tick(cond=cond)
        else:
            assert 0, step

    def __add_step(self, step):
        index = len(self.__current_steps) + len(self.__new_steps)
        step = (index,) + step
        self.__new_steps.append(step)
        self.__steps_status[id(step)] = tuple(self.__status)

    def clear_state(self):
        self.__add_step(('clear_state',))

    def set_pin_pull(self, pin, pull):
        step = 'set_pin_pull', pin, Bool.cast(pull)
        self.__add_step(step)

    def update_pin(self, pin):
        self.__add_step(('update_pin', pin))

    def set_pin_and_update(self, pin, pull):
        self.set_pin_pull(pin, pull)
        self.update_pin(pin)

    def set_pins_and_update(self, pin, bits):
        for i, b in enumerate(Bits.cast(bits)):
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

    def power_up(self):
        self.__add_step(('power_up',))

    def reset(self, propagation_delay, waiting_for_m1_delay):
        self.__add_step(('reset', propagation_delay,
                         waiting_for_m1_delay))

    def half_tick(self, *, cond=None):
        if cond is None:
            self.__add_step(('half_tick',))
        else:
            self.__add_step(('conditional_half_tick', cond))

    def tick(self):
        self.half_tick()
        self.half_tick()

    def ticks(self, n):
        for t in range(n):
            with self.status(f'tick {t}.0'):
                self.half_tick()
            with self.status(f'tick {t}.1'):
                self.half_tick()

    def cache(self, *, intermediate=False):
        steps = self.__current_steps + self.__new_steps

        if intermediate:
            if steps:
                self.__intermediate_steps.add(id(steps[-1]))
            return

        if not self.__get_cache(steps).exists():
            self.__apply_new_steps()
            __class__.__cache_state(self.__current_steps,
                                    self.__current_image)

    def status(self, s):
        def enter():
            self.__status.append(s)

        def exit():
            assert self.__status.pop() == s

        class S:
            def __enter__(self):
                enter()

            def __exit__(self, exc_type, exc_val, exc_tb):
                exit()

        return S()

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
            Status.print(f'  {id}: {s.get_node_state(n)}')
            if with_pull:
                Status.print(f'  {id} pull: {n.pull}')

        def print_bits(id, width):
            bits = s.read_nodes(id, width)
            if not isinstance(bits.value, tuple):
                Status.print(f'  {id}: {bits}')
                return

            for i in reversed(range(bits.width)):
                Status.print(f'  {id}{i}: {bits[i]}')

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

        # TODO: flipflop halted
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


class CheckToken(object):
    pass


class TestFailure(Exception):
    pass


def test_node(instrs, n, at_start, at_end, before, after):
    def check(x):
        if n.startswith('instr'):
            b, a = at_start[n], at_end[n]
        else:
            b, a = before[n], after[n]
            x = is_active(n).xifelse(x, b)

        if Bool.is_equiv(a, x):
            return CheckToken()

        Status.clear()

        NAMES = {CF: 'cf', NF: 'nf', PF: 'pf', XF: 'xf',
                 HF: 'hf', YF: 'yf', ZF: 'zf', SF: 'sf',
                 CF2: 'cf', NF2: 'nf', PF2: 'pf', XF2: 'xf',
                 HF2: 'hf', YF2: 'yf', ZF2: 'zf', SF2: 'sf',
                 IFF1: 'iff1', IFF2: 'iff2'}
        n_name = str(n)
        if n in NAMES:
            n_name = f'{n_name} ({NAMES[n]})'
        lines = ['', 'FAILED: ' + '; '.join(instrs) + f' {n_name}']
        if '--no-before-after-expected' not in sys.argv:
            lines.extend((
                f'  before: {b}',
                f'  after: {a}',
                f'  expected: {x}',
                f'  diff: {a ^ x}'))
        print('\n'.join(lines), file=sys.stderr, flush=True)
        raise TestFailure()

    phase = len(instrs)
    instr = instrs[-1]
    cycles = TestedInstrs.get_cycles(instr, phase)
    opcode, ticks, cond = cycles[0]
    if opcode in (0xcb, 0xed):
        opcode, ticks, cond = cycles[1]
    opcode = opcode.zero_extended(8)

    AT_HL, A = 6, 7

    def phased(x, offset=0):
        if isinstance(x, str):
            assert '_p' not in x, x
            return f'{x}_p{phase + offset}'
        return x

    def bits(id):
        return Bits(before[f'{id}{i}'] for i in range(8))

    def is_active(n):
        if n in (IFF1, IFF2, EX_AF_FF, EXX_FF, EX_DE_FF):
            return TRUE
        if n in (EX_DE_FF0, EX_DE_FF1):
            exx = before[EXX_FF]
            return ~exx if n == EX_DE_FF0 else exx
        if n.startswith('reg_'):
            if n[4] in 'wz':
                return TRUE
            if n[4] in 'af':
                ex_af = before[EX_AF_FF]
                is_alt = (n[5] == n[4])
                return ~ex_af if is_alt else ex_af
        assert 0, n

    def get_b():
        return Bits.ifelse(before[EXX_FF], bits('reg_b'), bits('reg_bb'))

    def get_c():
        return Bits.ifelse(before[EXX_FF], bits('reg_c'), bits('reg_cc'))

    def get_d():
        d = Bits.ifelse(before[EXX_FF], bits('reg_d'), bits('reg_dd'))
        h = Bits.ifelse(before[EXX_FF], bits('reg_h'), bits('reg_hh'))
        return Bits.ifelse(before[EX_DE_FF], h, d)

    def get_e():
        e = Bits.ifelse(before[EXX_FF], bits('reg_e'), bits('reg_ee'))
        rl = Bits.ifelse(before[EXX_FF], bits('reg_l'), bits('reg_ll'))
        return Bits.ifelse(before[EX_DE_FF], rl, e)

    def get_h():
        d = Bits.ifelse(before[EXX_FF], bits('reg_d'), bits('reg_dd'))
        h = Bits.ifelse(before[EXX_FF], bits('reg_h'), bits('reg_hh'))
        return Bits.ifelse(before[EX_DE_FF], d, h)

    def get_l():
        e = Bits.ifelse(before[EXX_FF], bits('reg_e'), bits('reg_ee'))
        rl = Bits.ifelse(before[EXX_FF], bits('reg_l'), bits('reg_ll'))
        return Bits.ifelse(before[EX_DE_FF], e, rl)

    def get_a():
        return Bits.ifelse(before[EX_AF_FF], bits('reg_a'), bits('reg_aa'))

    def get_f():
        return Bits.ifelse(before[EX_AF_FF], bits('reg_f'), bits('reg_ff'))

    def get_active(n):
        f = get_f()
        if n == CF:
            return f[CF_BIT]
        if n == NF:
            return f[NF_BIT]
        if n == PF:
            return f[PF_BIT]
        if n == HF:
            return f[HF_BIT]
        if n == ZF:
            return f[ZF_BIT]
        if n == SF:
            return f[SF_BIT]
        assert 0, n

    def get_at_hl():
        return Bits(phased('r'), width=8)

    def get_r(r):
        return Bits.ifelse(
            r[2],
            Bits.ifelse(
                r[1],
                Bits.ifelse(r[0], get_a(), get_at_hl()),
                Bits.ifelse(r[0], get_l(), get_h())),
            Bits.ifelse(
                r[1],
                Bits.ifelse(r[0], get_e(), get_d()),
                Bits.ifelse(r[0], get_c(), get_b())))

    def get_bc():
        return Bits.concat(get_b(), get_c())

    def get_de():
        return Bits.concat(get_d(), get_e())

    def get_hl():
        return Bits.concat(get_h(), get_l())

    def get_sp():
        return Bits.concat(bits('reg_sph'), bits('reg_spl'))

    def get_wz():
        return Bits.concat(bits('reg_w'), bits('reg_z'))

    def get_rp(rp):
        return Bits.ifelse(
            rp[1],
            Bits.ifelse(rp[0], get_sp(), get_hl()),
            Bits.ifelse(rp[0], get_de(), get_bc()))

    def get_pc_plus(off):
        # On tick 3 of an instruction the value of pc has already
        # been advanced, so we in fact get (pc + 1).
        return Bits.concat(bits('reg_pch'), bits('reg_pcl')) + (off - 1)

    CFF = CF, CF2
    NFF = NF, NF2
    PFF = PF, PF2
    XFF = XF, XF2
    HFF = HF, HF2
    YFF = YF, YF2
    ZFF = ZF, ZF2
    SFF = SF, SF2

    if n in CFF:
        if instr == 'scf/ccf':
            return check(Bool.ifelse(phased('is_scf'), TRUE,
                                     ~get_active(CF)))
        if instr == 'rlca/rrca/rla/rra':
            a = get_a()
            return check(Bool.ifelse(phased('is_rl'), a[7], a[0]))

    if n in NFF:
        if instr == 'cpl':
            return check(TRUE)
        if instr in ('inc/dec {b, c, d, e, h, l, a}', 'inc/dec (hl)'):
            return check(Bool.ifelse(phased('is_inc'), FALSE, TRUE))
        if instr in ('scf/ccf', 'rlca/rrca/rla/rra', 'add hl, <rp>'):
            return check(FALSE)

    if n in XFF + YFF:
        i = int(n[-1])
        a = get_a()[i]
        if instr == 'cpl':
            return check(~a)
        if instr == 'scf/ccf':
            # See <https://github.com/kosarev/z80/issues/42>.
            # TODO: Does this agree with the current known
            # description of the behaviour?
            f = get_f()[i]
            op = Bits(at_start[f'instr{b}'] for b in range(8))
            x = Bits(op.bits[6:8])
            z = Bits(op.bits[0:3])
            q = op[3]
            is_scf_ccf = (op == 0x37) | (op == 0x3f)
            is_alu_r = (x == 2)
            is_alu_n = (x == 3) & (z == 6)
            is_inc_dec_r = (x == 0) & ((z == 4) | (z == 5))
            is_add_hl_rp = (x == 0) & q & (z == 1)
            ignores_f = (is_scf_ccf | is_alu_r | is_alu_n | is_inc_dec_r |
                         is_add_hl_rp)
            return check(Bool.ifelse(ignores_f, a, a | f))
        if instr == 'rlca/rrca/rla/rra':
            a = get_a()
            return check(Bool.ifelse(phased('is_rl'), a[i - 1], a[i + 1]))

    if n in HFF:
        if instr == 'cpl':
            return check(TRUE)
        if instr == 'rlca/rrca/rla/rra':
            return check(FALSE)
        if instr == 'scf/ccf':
            return check(Bool.ifelse(phased('is_scf'), FALSE, get_active(CF)))

    if n.startswith('reg_a'):
        i = int(n[-1])
        if instr == 'cpl':
            return check(~before[n])
        if instr in ('ld a, (nn)/ld (nn), a', 'ld (<rp>), a/ld a, (<rp>)'):
            id = 'rw3' if instr == 'ld a, (nn)/ld (nn), a' else 'rw'
            v = Bits(phased(id), width=8)
            a = Bits.ifelse(phased('is_store'), get_a(), v)
            return check(a[i])
        if instr in ('ld {b, c, d, e, h, l, a}, n',
                     'ld {b, c, d, e, h, l, a}, (hl)'):
            v = Bits(phased('r'), width=8)
            a = Bits.ifelse(Bits(opcode[3:6]) == A, v, get_a())
            return check(a[i])
        if instr == 'ld {b, c, d, e, h, l, a}, {b, c, d, e, h, l, a}':
            s = get_r(opcode[0:3])
            d = Bits(opcode[3:6])
            a = Bits.ifelse(d == A, s, get_a())
            return check(a[i])
        if instr == 'in a, (n)/out (n), a':
            v = Bits(phased('io'), width=8)
            a = Bits.ifelse(phased('is_in'), v, get_a())
            return check(a[i])
        if instr == 'inc/dec {b, c, d, e, h, l, a}':
            r = Bits(opcode[3:6])
            a = get_a()
            v = Bits.ifelse(phased('is_inc'), a + 1, a - 1)
            a = Bits.ifelse(r == A, v, a)
            return check(a[i])
        if instr == 'rlca/rrca/rla/rra':
            a = get_a()
            cf_a = Bits.concat(Bits((get_active(CF),)), a)
            rlca, rrca = a.rol(), a.ror()
            rla, rra = cf_a.rol(), cf_a.ror()
            v = Bits.ifelse(phased('is_rlca_rrca'),
                            Bits.ifelse(phased('is_rl'), rlca, rrca),
                            Bits.ifelse(phased('is_rl'), rla, rra))
            return check(v[i])

    if n.startswith('reg_w') or n.startswith('reg_z'):
        i = int(n[-1])
        if n.startswith('reg_w'):
            i += 8
        if instr in ('add hl, <rp>', 'rrd/rld', 'adc/sbc hl, <rp>'):
            return check((get_hl() + 1)[i])
        if instr in ('call nn', 'ex (sp), hl'):
            wz = Bits.concat(Bits(phased('r2'), width=8),
                             Bits(phased('r1'), width=8))
            return check(wz[i])
        if instr in ('jp nn', 'ret'):
            wz = Bits.concat(Bits(phased('hi'), width=8),
                             Bits(phased('lo'), width=8))
            return check(wz[i])
        if instr == 'reti/retn/xretn':
            wz = Bits.concat(Bits(phased('r4'), width=8),
                             Bits(phased('r3'), width=8))
            return check(wz[i])
        if instr == 'ret cc':
            sf, pf = get_active(SF), get_active(PF)
            cf, zf = get_active(CF), get_active(ZF)
            cc = Bits(phased('cc'), width=3)
            cc = Bool.ifelse(
                cc[2],
                Bool.ifelse(
                    cc[1],
                    Bool.ifelse(cc[0], sf, ~sf),
                    Bool.ifelse(cc[0], pf, ~pf)),
                Bool.ifelse(
                    cc[1],
                    Bool.ifelse(cc[0], cf, ~cf),
                    Bool.ifelse(cc[0], zf, ~zf)))
            target = Bits.concat(Bits(phased('r2'), width=8),
                                 Bits(phased('r1'), width=8))
            wz = Bits.ifelse(cc, target, get_wz())
            return check(wz[i])
        if instr == 'jr d':
            d = Bits(phased('r'), width=8)
            wz = get_pc_plus(2) + d.sign_extended(16)
            return check(wz[i])
        if instr == 'djnz d':
            b = get_b()
            d = Bits(phased('r'), width=8)
            target = get_pc_plus(2) + d.sign_extended(16)
            wz = Bits.ifelse(b != 1, target, get_wz())
            return check(wz[i])
        if instr == 'jr cc, d':
            cc2 = Bits(phased('cc2'), width=2)
            cf, zf = get_active(CF), get_active(ZF)
            cc2 = Bool.ifelse(
                cc2[1],
                Bool.ifelse(cc2[0], cf, ~cf),
                Bool.ifelse(cc2[0], zf, ~zf))
            d = Bits(phased('r'), width=8)
            target = get_pc_plus(2) + d.sign_extended(16)
            wz = Bits.ifelse(cc2, target, get_wz())
            return check(wz[i])
        if instr == 'in a, (n)/out (n), a':
            a = get_a()
            r = Bits(phased('r'), width=8)
            in_wz = Bits.concat(a, r) + 1
            out_wz = Bits.concat(a, (r + 1).truncated(8))
            return check(Bool.ifelse(phased('is_in'), in_wz[i], out_wz[i]))
        if instr == 'rst n':
            wz = (Bits(phased('y'), width=3) << 3).zero_extended(16)
            return check(wz[i])
        if instr == 'ld hl, (nn)/ld (nn), hl':
            wz = Bits.concat(Bits(phased('r2'), width=8),
                             Bits(phased('r1'), width=8)) + 1
            return check(wz[i])
        if instr == 'ld a, (nn)/ld (nn), a':
            lo = Bits(phased('r1'), width=8)
            hi = Bits(phased('r2'), width=8)
            load_wz = Bits.concat(hi, lo) + 1
            store_wz = Bits.concat(get_a(), (lo + 1).truncated(8))
            wz = Bits.ifelse(phased('is_store'), store_wz, load_wz)
            return check(wz[i])
        if instr == 'ld (<rp>), a/ld a, (<rp>)':
            rp = get_rp(opcode[4:5] + (FALSE,))
            load_wz = rp + 1
            lo = Bits(rp.bits[0:8])
            store_wz = Bits.concat(get_a(), (lo + 1).truncated(8))
            wz = Bits.ifelse(phased('is_store'), store_wz, load_wz)
            return check(wz[i])
        if instr == 'in/out r, (c)':
            wz = get_bc() + 1
            return check(wz[i])
        if instr == 'ld <rp>, (nn)/ld (nn), <rp>':
            lo = Bits(phased('r3'), width=8)
            hi = Bits(phased('r4'), width=8)
            wz = Bits.concat(hi, lo) + 1
            return check(wz[i])

    if n in (IFF1, IFF2):
        if instr == 'ei/di':
            return check(Bool.get(phased('is_ei')))

    if n == EX_AF_FF and instr == "ex af, af'":
        return check(~before[n])

    if n == EXX_FF and instr == 'exx':
        return check(~before[n])

    if n in (EX_DE_FF0, EX_DE_FF1, EX_DE_FF) and instr == 'ex de, hl':
        return check(~before[n])

    if n == EX_DE_FF and instr == 'exx':
        ex_de = before[EXX_FF].xifelse(before[EX_DE_FF0], before[EX_DE_FF1])
        return check(~ex_de)

    if instr in ('inc/dec {b, c, d, e, h, l, a}', 'inc/dec (hl)'):
        r = get_r(opcode[3:6])
        r = Bits.ifelse(phased('is_inc'), r + 1, r - 1).truncated(8)
        if n in PFF:
            return check(Bool.ifelse(phased('is_inc'), r == 0x80, r == 0x7f))
        if n in XFF + YFF:
            i = int(n[-1])
            return check(r[i])
        if n in HFF:
            r4 = r.truncated(4)
            return check(Bool.ifelse(phased('is_inc'), r4 == 0x0, r4 == 0xf))
        if n in ZFF:
            return check(r == 0x00)
        if n in SFF:
            return check(r[7])

    if instr == 'add hl, <rp>':
        hl = get_hl()
        rp = get_rp(opcode[4:6])
        r = hl + rp
        if n in CFF:
            return check(r[16])
        if n in XFF + YFF:
            i = int(n[-1])
            return check(r[i + 8])
        if n in HFF:
            return check(hl[4 + 8] ^ rp[4 + 8] ^ r[4 + 8])

    if instr == 'ld {i, r}, a/ld a, {i, r}':
        ri, rr = bits('reg_i'), bits('reg_r')
        rr = Bits((rr + 1).bits[:7] + (rr[7],))
        r = Bits.ifelse(phased('is_i_reg'), ri, rr)
        v = Bits.ifelse(phased('is_store'), get_a(), r)
        if n in NFF + HFF:
            return check(Bool.ifelse(phased('is_store'), before[n], FALSE))
        if n in PFF:
            return check(Bool.ifelse(phased('is_store'), before[n],
                                     before[IFF2]))
        if n in XFF + YFF:
            i = int(n[-1])
            return check(Bool.ifelse(phased('is_store'), before[n], r[i]))
        if n in ZFF:
            return check(Bool.ifelse(phased('is_store'), before[n], r == 0x00))
        if n in SFF:
            return check(Bool.ifelse(phased('is_store'), before[n], r[7]))
        if n.startswith('reg_a'):
            i = int(n[-1])
            return check(v[i])

    if instr == 'pop <rp2>':
        RP2_AF = 3
        p = Bits(opcode.bits[4:6])
        if n.startswith('reg_f'):
            i = int(n[-1])
            return check(Bool.ifelse(p == RP2_AF,
                                     f'lo_p{phase}_b{i}', before[n]))
        if n.startswith('reg_a'):
            i = int(n[-1])
            return check(Bool.ifelse(p == RP2_AF,
                                     f'hi_p{phase}_b{i}', before[n]))

    if instr == 'daa':
        a = get_a()
        cf = get_active(CF)
        add_0x60 = cf | (a >= 0x9a)
        if n in CFF:
            return check(add_0x60)
        hf = get_active(HF)
        add_0x06 = hf | ((a & 0x0f) >= 0x0a)
        d = (Bits.ifelse(add_0x60, 0x60, 0x00) +
             Bits.ifelse(add_0x06, 0x06, 0x00))
        nf = get_active(NF)
        r = Bits.ifelse(nf, a - d, a + d).truncated(8)
        if n in PFF:
            return check(r.parity())
        if n in XFF + YFF:
            i = int(n[-1])
            return check(r[i])
        if n in HFF:
            a4 = a.truncated(4)
            return check(Bool.ifelse(nf, hf & (a4 <= 0x5), a4 >= 0xa))
        if n in ZFF:
            return check(r == 0x00)
        if n in SFF:
            return check(r[7])
        if n.startswith('reg_a'):
            i = int(n[-1])
            return check(r[i])

    if instr in ('<alu> {b, c, d, e, h, l, a}', '<alu> (hl)', '<alu> n',
                 'neg/xneg'):
        ADD, ADC, SUB, SBC, AND, XOR, OR, CP = range(8)

        if instr == 'neg/xneg':
            a = Bits(0, width=8)
            s = get_a()
            op = Bits(SUB)
        else:
            a = get_a()
            s = get_r(opcode[0:3])
            op = Bits(opcode[3:6])

        is_add_adc = op.is_any(ADD, ADC)
        is_sub_sbc_cp = op.is_any(SUB, SBC, CP)
        is_bitwise = op.is_any(AND, XOR, OR)

        cf_in = Bits.ifelse(get_active(CF) & op.is_any(ADC, SBC), 1, 0)
        r = Bits.ifelse(
            is_add_adc, a + s + cf_in,
            Bits.ifelse(
                is_sub_sbc_cp, ((a - s) ^ 0x100) - cf_in,
                Bits.ifelse(
                    op == AND, a & s,
                    Bits.ifelse(op == XOR, a ^ s, a | s))))

        if n in CFF:
            return check(r[8])
        if n in NFF:
            return check(is_sub_sbc_cp)
        if n in PFF:
            overflow = r[8] ^ r[7] ^ a[7] ^ s[7]
            return check(Bool.ifelse(is_bitwise, r.parity(), overflow))
        if n in XFF + YFF:
            i = int(n[-1])
            return check(Bool.ifelse(op == CP, s[i], r[i]))
        if n in HFF:
            return check(Bool.ifelse(is_bitwise, op == AND,
                                     r[4] ^ a[4] ^ s[4]))
        if n in ZFF:
            return check(r.truncated(8) == 0x00)
        if n in SFF:
            return check(r[7])
        if n.startswith('reg_a'):
            i = int(n[-1])
            v = Bits.ifelse(op == CP, get_a(), r)
            return check(v[i])

    if instr == 'rrd/rld':
        a = get_a()
        al, ah = Bits(a.bits[0:4]), Bits(a.bits[4:8])
        t = Bits.concat(al, Bits(phased('r'), width=8))
        t = Bits.ifelse(phased('is_rrd'), t.ror(4), t.rol(4))
        t = Bits.concat(ah, t)
        a = Bits(t[8:16])
        if n.startswith('reg_a') or n in XFF + YFF + SFF:
            i = int(n[-1])
            return check(a[i])
        if n in NFF + HFF:
            return check(FALSE)
        if n in PFF:
            return check(a.parity())
        if n in ZFF:
            return check(a == 0x00)

    if instr == 'in/out r, (c)':
        y = Bits(phased('y'), width=3)
        io = Bits(phased('io'), width=8)
        is_in = Bool.get(phased('is_in'))
        a = Bits.ifelse(~is_in | (y != A), get_a(), io)
        if n in NFF + HFF:
            return check(Bool.ifelse(is_in, FALSE, before[n]))
        if n in PFF:
            return check(Bool.ifelse(is_in, io.parity(), before[n]))
        if n in XFF + YFF + SFF:
            i = int(n[-1])
            return check(Bool.ifelse(is_in, io[i], before[n]))
        if n in ZFF:
            return check(Bool.ifelse(is_in, io == 0, before[n]))
        if n.startswith('reg_a'):
            i = int(n[-1])
            return check(a[i])

    if instr == 'adc/sbc hl, <rp>':
        hl, rp = get_hl(), get_rp(opcode[4:6])
        cf = Bits.ifelse(get_active(CF), 1, 0)
        is_adc = Bool.get(phased('q') + '_b0')
        r = Bits.ifelse(is_adc, hl + rp + cf, ((hl - rp) ^ 0x10000) - cf)
        if n in CFF:
            return check(r[16])
        if n in NFF:
            return check(~is_adc)
        if n in PFF:
            overflow = r[16] ^ r[15] ^ hl[15] ^ rp[15]
            return check(overflow)
        if n in XFF + YFF + SFF:
            i = int(n[-1])
            return check(r[i + 8])
        if n in HFF:
            return check(r[12] ^ hl[12] ^ rp[12])
        if n in ZFF:
            return check(r.truncated(16) == 0)

    if instr in ('bit (hl)', 'rot/res/set (hl)',
                 'rot/bit/res/set {b, c, d, e, h, l, a}'):
        y = Bits(phased('y'), width=3)

        reg = Bits(opcode[0:3])
        v = get_r(reg)

        # RLC, RRC, RL, RR, SLA, SRA, SLL, SRL
        is_right = y[0]
        rot_cf = Bool.ifelse(is_right, v[0], v[7])

        is_nine_bit = y[1]
        is_shift = y[2]
        cf = get_active(CF)
        b = Bool.ifelse(
            is_shift,
            Bool.ifelse(is_nine_bit, ~is_right, is_right & v[7]),
            Bool.ifelse(is_nine_bit, cf, rot_cf))

        rot_r = Bits.ifelse(is_right,
                            Bits(v.bits[1:8] + (b,)),
                            Bits((b,) + v.bits[0:7]))

        m = Bits.get_mask(y)
        bit_r = v & m
        set_r = v | m
        res_r = v & ~m

        ROT, BIT, RES, SET = range(4)
        op = Bits(opcode[6:8])

        if n in CFF:
            return check(Bool.ifelse(op == ROT, rot_cf, cf))
        if n in NFF:
            return check(Bool.ifelse(op == ROT, FALSE,
                                     (op != BIT) & before[n]))
        if n in HFF:
            return check(Bool.ifelse(op == ROT, FALSE,
                                     (op == BIT) | before[n]))
        if n in PFF:
            return check(Bool.ifelse(
                op == BIT, bit_r == 0,
                Bool.ifelse(op == ROT, rot_r.parity(), before[n])))
        if n in XFF + YFF:
            i = int(n[-1])
            return check(Bool.ifelse(
                op == BIT,
                Bool.ifelse(reg == AT_HL, before[f'reg_w{i}'], v[i]),
                Bool.ifelse(op == ROT, rot_r[i], before[n])))
        if n in ZFF:
            return check(Bool.ifelse(
                op == BIT, bit_r == 0,
                Bool.ifelse(op == ROT, rot_r == 0, before[n])))
        if n in SFF:
            return check(Bool.ifelse(
                op == BIT, bit_r[7],
                Bool.ifelse(op == ROT, rot_r[7], before[n])))
        if n.startswith('reg_a'):
            i = int(n[-1])
            return check(Bool.ifelse(
                (op == BIT) | (reg != A), before[n],
                Bool.ifelse(op == ROT, rot_r[i],
                            Bool.ifelse(op == RES, res_r[i], set_r[i]))))

    # Test instruction latch nodes.
    if n.startswith('instr'):
        i = int(n[-1])
        return check(opcode[i])

    return check(before[n])


def get_effective_states(s, nodes=None):
    # Additional ticks are necessary for new values
    # to reach their nodes.
    EXTRA_TICKS = 3

    s = State(s)

    s.set_db(0x00)  # nop
    for t in range(EXTRA_TICKS):
        for ht in (0, 1):
            with s.status(f'extra tick {t}.{ht}'):
                s.half_tick()
    s.cache()

    return s.get_node_states(nodes)


def get_cond_as_expr(cond, phase, before):
    if cond is None:
        return None
    if cond == 'b != 1':
        b = Bits.ifelse(
                before[EXX_FF],
                Bits(before[f'reg_b{i}'] for i in range(8)),
                Bits(before[f'reg_bb{i}'] for i in range(8)))
        return b != 1
    f = Bits.ifelse(
            before[EX_AF_FF],
            Bits(before[f'reg_f{i}'] for i in range(8)),
            Bits(before[f'reg_ff{i}'] for i in range(8)))
    if cond == 'cc':
        cc = Bits(f'cc_p{phase}', width=3)
        return Bool.ifelse(
            cc[2],
            Bool.ifelse(
                cc[1],
                Bool.ifelse(cc[0], f[SF_BIT], ~f[SF_BIT]),
                Bool.ifelse(cc[0], f[PF_BIT], ~f[PF_BIT])),
            Bool.ifelse(
                cc[1],
                Bool.ifelse(cc[0], f[CF_BIT], ~f[CF_BIT]),
                Bool.ifelse(cc[0], f[ZF_BIT], ~f[ZF_BIT])))
    if cond == 'cc2':
        cc2 = Bits(f'cc2_p{phase}', width=2)
        return Bool.ifelse(
            cc2[1],
            Bool.ifelse(cc2[0], f[CF_BIT], ~f[CF_BIT]),
            Bool.ifelse(cc2[0], f[ZF_BIT], ~f[ZF_BIT]))
    assert 0, cond


def execute_instr(s, id, phase, before):
    if SEED is not None:
        random.seed(SEED)

    cycles = TestedInstrs.get_cycles(id, phase)
    for cycle_no, (d, ticks, cond) in enumerate(cycles):
        cond = get_cond_as_expr(cond, phase, before)
        s.set_db(d)
        for t in range(ticks):
            for ht in (0, 1):
                with s.status(f'tick {cycle_no}-{t}.{ht}'):
                    s.half_tick(cond=cond)
        s.cache(intermediate=True)


def process_instr(instrs, base_state, *, test=False):
    SAMPLED_NODES = set(TESTED_NODES)
    for i in range(8):
        for r in 'bcdehl':
            SAMPLED_NODES.update((f'reg_{r}{i}', f'reg_{r}{r}{i}'))
        for r in ('pch', 'pcl', 'sph', 'spl', 'i', 'r'):
            SAMPLED_NODES.add(f'reg_{r}{i}')

    phase = len(instrs)
    id = instrs[-1]

    s = State(base_state)
    at_start = s.get_node_states(SAMPLED_NODES)
    before = get_effective_states(s, SAMPLED_NODES)

    execute_instr(s, id, phase, before)

    s.cache()
    after_instr_state = State(s)

    if not test:
        return after_instr_state

    # Status.print('; '.join(instr_ids))

    at_end = s.get_node_states(SAMPLED_NODES)
    after = get_effective_states(s, SAMPLED_NODES)

    for n in sorted(TESTED_NODES):
        token = test_node(instrs, n, at_start, at_end, before, after)
        assert isinstance(token, CheckToken)

    return after_instr_state


def build_reset_state():
    s = State(cache_all_reportable_states=True)
    with s.status('clear state'):
        s.clear_state()
    s.report('after-clearing-state')

    for pin in _PINS:
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
    with s.status('power up'):
        s.power_up()
    s.report('after-updating-all-nodes')

    with s.status('reset'):
        s.reset(propagation_delay=31, waiting_for_m1_delay=5)
    s.report('after-reset')

    return s


def build_symbolised_state():
    def rev(bits):
        return tuple(reversed(bits))

    SYMBOLISING_SEQ = (
        ('exx', ((0xd9, 4),)),
        ('pop bc2', ((0xc1, 4), ('cc', 3), ('bb', 3))),
        ('pop de2', ((0xd1, 4), ('ee', 3), ('dd', 3))),
        ('pop hl2', ((0xe1, 4), ('ll', 3), ('hh', 3))),
        ('ex de, hl/nop', ((Bits.ifelse('is_ex_de_hl', 0xeb, 0x00), 4),)),
        ('exx', ((0xd9, 4),)),

        ('pop ix', ((0xdd, 4), (0xe1, 4), ('ixl', 3), ('ixh', 3))),
        ('pop iy', ((0xfd, 4), (0xe1, 4), ('iyl', 3), ('iyh', 3))),
        ('pop bc', ((0xc1, 4), ('c', 3), ('b', 3))),
        ('pop de', ((0xd1, 4), ('e', 3), ('d', 3))),
        ('pop hl', ((0xe1, 4), ('l', 3), ('h', 3))),

        ("ex af, af'", ((0x08, 4),)),
        ('pop af2', ((0xf1, 4), ('ff', 3), ('aa', 3))),
        ("ex af, af'", ((0x08, 4),)),

        ('ld sp, <sp>', ((0x31, 4), ('spl', 3), ('sph', 3))),

        ('im <im>', ((0xed, 4),
                     (rev((0, 1, 0, 'im1', 'im0', 1, 1, 0)), 4))),

        ('ei/di', ((rev((1, 1, 1, 1, 'ei', 0, 1, 1)), 4),)),

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

        ("ex af, af'/nop", ((Bits.ifelse('is_ex_af_af2', 0x08, 0x00), 4),)),
        ('ex de, hl/nop', ((Bits.ifelse('is_ex_de_hl_2', 0xeb, 0x00), 4),)),
        ('exx/nop', ((Bits.ifelse('is_exx', 0xd9, 0x00), 4),)),
    )

    s = build_reset_state()
    if '--no-symbolising' not in sys.argv:
        if SEED is not None:
            random.seed(SEED)

        with s.status('build symbolised state'):
            for id, cycles in SYMBOLISING_SEQ:
                with s.status(id):
                    for i, (d, ticks) in enumerate(cycles):
                        with s.status(f'cycle {i}'):
                            s.set_db_and_wait(d, ticks)
                            s.cache(intermediate=True)

        ''' TODO: Symbolising the latch seems to cause failures
                  on shiffling (updating nodes in random order).
        with s.status('symbolise instruction latch'):
            instr = Bits('instr', width=8)
            is_prefix = ((instr == 0xcb) | (instr == 0xed) |
                         (instr == 0xdd) | (instr == 0xfd))
            instr = Bits(b & ~is_prefix for b in instr)

            for i in range(8):
                s.set_latch_state(f'instr{i}', f'~instr{i}', instr[i])
        '''

        s.cache()
        s.report('after-symbolising')

    return s


def test_instr_seq(seq):
    # Bool.clear()
    # gc.collect()

    assert FALSE is Bool.get(False)
    assert TRUE is Bool.get(True)

    try:
        state = build_symbolised_state()
        with Status.do('; '.join(seq)):
            for i in range(len(seq) - 1):
                state = process_instr(seq[:i + 1], state)
            process_instr(seq, state, test=True)
        return True, '; '.join(seq)
    except TestFailure:
        return False, traceback.format_exc()


def test_instr_seq_concurrently(args):
    i, seq = args
    with Status.suppress():
        try:
            return i, test_instr_seq(seq)
        except Exception:
            return i, traceback.format_exc()


def get_instr_seq_id(seq):
    return '; '.join(seq)


def get_instr_seq_cache_entry(domain, seq):
    assert domain in ('passed', 'failed')
    return Cache.get_entry(domain, get_instr_seq_id(seq))


def get_instr_seq_time_stamp(domain, seq):
    t = get_instr_seq_cache_entry(domain, seq).load()
    if t is None:
        t = 0
    else:
        t, = t
    return t


def test_instr_seqs(seqs):
    def sort_key(seq):
        last_passed_at = get_instr_seq_time_stamp('passed', seq)
        last_failed_at = get_instr_seq_time_stamp('failed', seq)
        seq_id = get_instr_seq_id(seq)
        return -last_failed_at, -last_passed_at, len(seq), seq_id

    def mark(ok, seq):
        domain = 'passed' if ok else 'failed'
        now = datetime.datetime.timestamp(datetime.datetime.now())
        get_instr_seq_cache_entry(domain, seq).store((now,))

    def process_results(i, seq, res):
        if isinstance(res, str):
            Status.print(f'{i + 1}/{len(seqs)} {seq} {res}')
            return False
        ok, message = res
        if message is not None:
            Status.print(f'{i + 1}/{len(seqs)} {message}')
        mark(ok, seqs[i])
        return ok

    seqs = sorted(seqs, key=sort_key)
    ok = True
    if '--single-thread' in sys.argv:
        for i, seq in enumerate(seqs):
            if not process_results(i, seqs[i], test_instr_seq(seq)):
                assert 0
    else:
        seqs = tuple(seqs)
        num_threads = multiprocessing.cpu_count()
        if '--spare-thread' in sys.argv:
            num_threads = max(1, num_threads - 1)
        with multiprocessing.Pool(num_threads) as p:
            queue = p.imap_unordered(test_instr_seq_concurrently,
                                     enumerate(seqs))
            for i, res in queue:
                ok &= process_results(i, seqs[i], res)

    return ok


class TestedInstrs(object):
    @staticmethod
    def get_instrs():
        return tuple(sorted(id for id, cycles in __class__.__gen_instrs(0)))

    @staticmethod
    def get_cycles(instr_id, phase):
        assert phase > 0
        for id, cycles in __class__.__gen_instrs(phase):
            if id == instr_id:
                return cycles
        assert 0, f'Unknown instruction {instr_id}!'

    @staticmethod
    def __gen_instrs(phase):
        def phased(id):
            assert isinstance(id, str)
            assert '_p' not in id
            return f'{id}_p{phase}'

        def bits(x, width):
            if isinstance(x, str):
                x = phased(x)
            return Bits.cast(x, width=width)

        def ifelse(cond, a, b):
            return Bits.ifelse(phased(cond), a, b)

        def xyz(x, y, z):
            # xx yyy zzz
            return Bits.concat(bits(x, 2), bits(y, 3), bits(z, 3))

        def xpqz(x, p, q, z):
            # xx ppq zzz
            return Bits.concat(bits(x, 2), bits(p, 2), bits(q, 1), bits(z, 3))

        def pattern(p):
            bits = []
            for b in reversed(p):
                if b == ' ':
                    continue
                if b in ('0', '1'):
                    b = (b != '0')
                else:
                    b = f'{phased(b)}_b{len(bits)}'
                bits.append(b)

            return Bits(bits)

        def f(p, *, ticks=4):
            # Variable names must be phased, so don't allow them here.
            assert not isinstance(p, str)
            return Bits.cast(p), ticks, None

        def f5(p):
            return f(p, ticks=5)

        def f6(p):
            return f(p, ticks=6)

        def r3(p='r', cond=None):
            return phased(p), 3, cond

        def r4(p='r'):
            return phased(p), 4, None

        def w3(p='w'):
            return phased(p), 3, None

        def w5(p='w'):
            return phased(p), 5, None

        def rw3(p='rw'):
            return phased(p), 3, None

        def e3(p='e'):
            return phased(p), 3, None

        def e4(p='e'):
            return phased(p), 4, None

        def e5(*, cond=None):
            return phased('e'), 5, cond

        def io4():
            return phased('io'), 4, None

        AT_HL = 6

        def get_non_at_hl_r(id='reg'):
            b, c, d, e, h, l, a = 0, 1, 2, 3, 4, 5, 7
            return ifelse(f'{id}_b2',
                          ifelse(f'{id}_b1', a,
                                 ifelse(f'{id}_b0', l, h)),
                          ifelse(f'{id}_b1', ifelse(f'{id}_b0', e, d),
                                 ifelse(f'{id}_b0', c, b)))

        yield 'nop', (f(0x00),)
        yield "ex af, af'", (f(0x08),)
        yield 'jr d', (f(0x18), r3(), e5())
        yield 'daa', (f(0x27),)
        yield 'cpl', (f(0x2f),)
        yield 'scf/ccf', (f(ifelse('is_scf', 0x37, 0x3f)),)
        yield 'exx', (f(0xd9),)
        yield 'jp hl', (f(0xe9),)
        yield 'ex de, hl', (f(0xeb),)
        yield 'ld sp, hl', (f6(0xf9),)
        yield 'ei/di', (f(ifelse('is_ei', 0xfb, 0xf3)),)

        yield 'ld {b, c, d, e, h, l, a}, {b, c, d, e, h, l, a}', (
            f(xyz(1, get_non_at_hl_r('regd'), get_non_at_hl_r('regs'))),)
        yield 'ld {b, c, d, e, h, l, a}, (hl)', (
            f(xyz(1, get_non_at_hl_r('regd'), AT_HL)), r3())
        yield 'ld (hl), {b, c, d, e, h, l, a}', (
            f(xyz(1, AT_HL, get_non_at_hl_r('regs'))), w3())

        ''' TODO
        yield 'halt', (
            f(xyz(1, AT_HL, AT_HL)),)
        '''

        yield 'ld {b, c, d, e, h, l, a}, n', (
            f(xyz(0, get_non_at_hl_r(), 6)), r3())
        yield 'ld (hl), n', (
            f(xyz(0, AT_HL, 6)), r3(), w3())

        yield '<alu> {b, c, d, e, h, l, a}', (
            f(xyz(2, 'op', get_non_at_hl_r())),)
        yield '<alu> (hl)', (f(xyz(2, 'op', AT_HL)), r3())
        yield '<alu> n', (f(xyz(3, 'op', 6)), r3())

        yield 'ld hl, (nn)/ld (nn), hl', (
            f(ifelse('is_store', 0x22, 0x2a)),
            r3('r1'), r3('r2'), rw3('rw3'), rw3('rw4'))
        yield 'ld a, (nn)/ld (nn), a', (
            f(ifelse('is_store', 0x32, 0x3a)),
            r3('r1'), r3('r2'), rw3('rw3'))

        yield 'rst n', (f5(xyz(3, 'y', 7)), w3('w1'), w3('w2'))

        yield 'rlca/rrca/rla/rra', (f(ifelse('is_rlca_rrca',
                                             ifelse('is_rl', 0x07, 0x0f),
                                             ifelse('is_rl', 0x17, 0x1f))),)

        yield 'inc/dec {b, c, d, e, h, l, a}', (
            f(xyz(0, get_non_at_hl_r(), ifelse('is_inc', 4, 5))),)
        yield 'inc/dec (hl)', (
            f(xyz(0, AT_HL, ifelse('is_inc', 4, 5))), r4(), w3())

        yield 'ld <rp>, nn', (f(xpqz(0, 'rp', 0, 1)), r3('r1'), r3('r2'))
        yield 'add hl, <rp>', (f(xpqz(0, 'rp', 1, 1)), e4('e1'), e3('e2'))
        yield 'inc/dec <rp>', (f6(xpqz(0, 'rp', ifelse('is_inc', 0, 1), 3)),)
        yield 'pop <rp2>', (f(xpqz(3, 'rp2', 0, 1)), r3('lo'), r3('hi'))
        yield 'push <rp2>', (f5(xpqz(3, 'rp2', 0, 5)), w3('lo'), w3('hi'))

        pp = Bits.concat(Bits(0, width=1), Bits('p', width=1))
        yield 'ld (<rp>), a/ld a, (<rp>)', (
            f(xpqz(0, pp, Bits((~Bool.get(phased('is_store')),)), 2)), rw3())

        yield 'jp nn', (f(0xc3), r3('lo'), r3('hi'))
        yield 'ret', (f(0xc9), r3('lo'), r3('hi'))
        yield 'call nn', (f(0xcd), r3('r1'), r4('r2'), w3('w3'), w3('w4'))
        yield 'ex (sp), hl', (f(0xe3), r3('r1'), r4('r2'), w3('w3'), w5('w4'))

        yield 'in a, (n)/out (n), a', (
            f(ifelse('is_in', 0xdb, 0xd3)), r3(), io4())

        yield 'djnz d', (
            f5(0x10), r3(), e5(cond='b != 1'))
        yield 'jr cc, d', (
            f(xyz(0, Bits(phased('cc2'), width=2) | 0b100, 0)),
            r3(), e5(cond='cc2'))
        yield 'ret cc', (
            f5(xyz(3, 'cc', 0)), r3('r1', cond='cc'), r3('r2', cond='cc'))

        yield 'ld {i, r}, a/ld a, {i, r}', (
            f(0xed), f5(ifelse('is_i_reg',
                               ifelse('is_store', 0x47, 0x57),
                               ifelse('is_store', 0x4f, 0x5f))))
        yield 'rrd/rld', (
            f(0xed), f(ifelse('is_rrd', 0x67, 0x6f)), r3(), e4(), w3())
        yield 'in/out r, (c)', (
            f(0xed), f(xyz(1, 'y', ifelse('is_in', 0, 1))), io4())
        yield 'adc/sbc hl, <rp>', (
            f(0xed), f(xpqz(1, 'p', 'q', 2)), e4(), e3())
        yield 'ld <rp>, (nn)/ld (nn), <rp>', (
            f(0xed), f(xpqz(1, 'p', 'q', 3)),
            r3('r3'), r3('r4'), rw3('rw5'), rw3('rw6'))
        yield 'neg/xneg', (f(0xed), f(xyz(1, 'y', 4)))
        yield 'reti/retn/xretn', (
            f(0xed), f(xyz(1, 'y', 5)), r3('r3'), r3('r4'))
        yield 'im/xim n', (f(0xed), f(xyz(1, 'y', 6)))

        xnop_00 = pattern('00 xxx xxx')
        xnop_01 = pattern('01 11x 111')
        xnop_10 = ifelse('is_100', pattern('10 0xx xxx'),
                         pattern('10 1xx 1xx'))
        xnop_11 = pattern('11 xxx xxx')
        xnop = ifelse('is_0',
                      ifelse('is_00', xnop_00, xnop_01),
                      ifelse('is_10', xnop_10, xnop_11))
        yield 'xnop', (f(0xed), f(xnop))

        ROT, BIT, RES, SET = range(4)
        rot_bit = ifelse('is_rot', ROT, BIT)
        res_set = ifelse('is_res', RES, SET)
        rot_bit_res_set = ifelse('is_rot_bit', rot_bit, res_set)
        yield 'rot/bit/res/set {b, c, d, e, h, l, a}', (
            f(0xcb), f(xyz(rot_bit_res_set, 'y', get_non_at_hl_r())))

        yield 'bit (hl)', (
            f(0xcb), f(xyz(BIT, 'y', AT_HL)), r4())

        rot_res_set = ifelse('is_rot', ROT, res_set)
        yield 'rot/res/set (hl)', (
            f(0xcb), f(xyz(rot_res_set, 'y', AT_HL)), r4(), w3())


def test_instructions():
    seqs = []

    def add(ss):
        # Make sure short sequences that work as prefixes for
        # longer sequences are generated and tested first.
        ss = {s: get_instr_seq_time_stamp('passed', s) for s in ss}

        if '--new-only' in sys.argv:
            ss = {s: t for s, t in ss.items() if t == 0}

        if 0 in ss.values():
            return test_instr_seqs(ss.keys())

        seqs.extend(ss.keys())
        return True

    instrs = TestedInstrs.get_instrs()
    ok = True
    ok &= add((i,) for i in instrs)
    if '--two-instr-seqs' in sys.argv:
        ok &= add((i1, i2) for i1 in instrs for i2 in instrs)
    ok &= test_instr_seqs(seqs)

    Status.clear()
    print('OK' if ok else 'FAILED')


def identify_instr_state_nodes(s, instr, persistent_nodes):
    s = State(s)

    # Symbolise non-persistent nodes.
    for id in s.get_node_states():
        if id not in persistent_nodes:
            # s.set_node_state(id, Bool.get(id))
            assert 0
    s.power_up()
    s.cache()

    at_start = s.get_node_states()
    before = get_effective_states(s)

    phase = 1
    execute_instr(s, instr, phase, before)
    s.cache()

    at_end = s.get_node_states()
    after = get_effective_states(s)

    for n in TESTED_NODES:
        token = test_node((instr,), n, at_start, at_end, before, after)

    # Exclude nodes that may end up changing their state from
    # persistent nodes.
    for id, state in s.get_node_states().items():
        if id not in persistent_nodes:
            continue
        if Bool.is_equiv(state, persistent_nodes[id]):
            continue
        # Status.print(f'Found state node: {id}')
        del persistent_nodes[id]


def identify_state_nodes():
    base_state = build_reset_state()

    # Execute a couple nops to make sure the CPU is in its normal
    # operational state.
    base_state.set_db_and_wait(0x00, 4)  # nop
    base_state.set_db_and_wait(0x00, 4)  # nop
    base_state.cache()

    persistent_nodes = base_state.get_node_states()

    # Make sure there are initially no nodes with symbolic states.
    assert all(s.value is not None for s in persistent_nodes.values())

    for instr in ('nop', '<alu> (hl)', '<alu> n',
                  '<alu> {b, c, d, e, h, l, a}'):
        while True:
            pn = persistent_nodes.copy()
            with Status.do(f'{len(persistent_nodes)} persistent nodes'):
                with Status.do(f'{instr}'):
                    identify_instr_state_nodes(base_state, instr,
                                               persistent_nodes)
            if persistent_nodes == pn:
                break

    for instr in TestedInstrs.get_instrs():
        pn = persistent_nodes.copy()
        with Status.do(f'{instr}'):
            identify_instr_state_nodes(base_state, instr, persistent_nodes)
        assert persistent_nodes == pn, instr


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

    s = build_symbolised_state()
    s.report('after-symbolising')


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
    test_bools()

    if '--print-start-time' in sys.argv:
        Status.print('started')

    if '--test' in sys.argv:
        test_computing_node_values()
        return

    if '--play-sandbox' in sys.argv:
        play_sandbox()
        return

    if '--identify-state-nodes' in sys.argv:
        identify_state_nodes()
        return

    build_symbolic_states()

    test_instructions()


if __name__ == "__main__":
    main()
