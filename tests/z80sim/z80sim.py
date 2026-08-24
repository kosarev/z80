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
# Symbolic simulation Copyright (C) 2017-2026 Ivan Kosarev.
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


# The simulator needs eqbool's equivalence sessions and
# fingerprints, first released in eqbool 0.5. The check is by
# feature so development installs pass regardless of their
# stated version.
assert hasattr(eqbool, 'EquivSession') and hasattr(eqbool.Bool, 'fp'), (
    'z80sim requires eqbool 0.5 or later')

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

# These two are suggested by Tony Brewer; --probe-node-ids
# confirms them and classifies the alternates: p1210 and p231
# track IFF1, p1239 tracks IFF2.
IFF1 = 'n181'
IFF2 = 'n206'

# The interrupt mode latch, identified by --probe-node-ids:
# the pair stores the im instruction's mode field verbatim.
IM_LATCH0 = 'n205'
IM_LATCH1 = 'n179'

# The halted flip-flop: the die's one static cross-coupled
# pair among the identified state cells (--probe-latch-cells),
# HALTED_FF the side that goes low on halt entry.
HALTED_FF = 'p1126'
HALTED_FF_B = 'p143'

TESTED_NODES = {IFF1, IFF2, IM_LATCH0, IM_LATCH1,
                HALTED_FF, HALTED_FF_B,
                EX_AF_FF, EXX_FF,
                EX_DE_FF0, EX_DE_FF1, EX_DE_FF, '~halt'}
for i in range(8):
    for r in 'af':
        TESTED_NODES.update((f'reg_{r}{i}', f'reg_{r}{r}{i}'))
    for r in 'wz':
        TESTED_NODES.add(f'reg_{r}{i}')
    for r in ('pch', 'pcl', 'sph', 'spl', 'i', 'r'):
        TESTED_NODES.add(f'reg_{r}{i}')
    for r in 'bcdehl':
        TESTED_NODES.update((f'reg_{r}{i}', f'reg_{r}{r}{i}'))
    for r in ('ixh', 'ixl', 'iyh', 'iyl'):
        TESTED_NODES.add(f'reg_{r}{i}')
    TESTED_NODES.add(f'instr{i}')

SAMPLED_NODES = set(TESTED_NODES)
for i in range(8):
    for r in 'bcdehl':
        SAMPLED_NODES.update((f'reg_{r}{i}', f'reg_{r}{r}{i}'))
    for r in ('pch', 'pcl', 'sph', 'spl', 'i', 'r'):
        SAMPLED_NODES.add(f'reg_{r}{i}')


def get_opt(id, type):
    for a in sys.argv:
        if a.startswith(id + '='):
            return type(a.split('=', maxsplit=1)[1])
    return None


SEED = get_opt('--seed', int)

# The number of concurrent test workers; the run is
# single-threaded when not given.
THREADS = get_opt('--threads', int)

# The memory cap for concurrent test workers, in GiB.
WORKER_MEMORY = get_opt('--worker-memory', float)


class MemoryCapReached(Exception):
    # Raised when a worker process exceeds --worker-memory,
    # right after securing a resumable cached state, so a fresh
    # process can pick the sequence up from that very point.
    pass


def get_rss():
    with open('/proc/self/statm') as f:
        return int(f.read().split()[1]) * os.sysconf('SC_PAGESIZE')


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
    __tty = sys.stderr.isatty()
    __last_log_time = None

    __profile = '--profile' in sys.argv
    if __profile:
        print(platform.python_implementation(), ' '.join(sys.argv))

    @staticmethod
    def __emit(line):
        if __class__.__suppression_count != 0:
            return

        if not __class__.__tty:
            # With no line to rewrite, leave a plain trace at a
            # low rate so redirected runs still show progress.
            now = datetime.datetime.now()
            last = __class__.__last_log_time
            if line and (last is None or
                         (now - last).total_seconds() >= 60):
                print(line, file=sys.stderr, flush=True)
                __class__.__last_log_time = now
            __class__.__line = line
            return

        # The status line lives on the line above the cursor,
        # which always rests at the start of a fresh line, so
        # output from outside this class -- tracebacks included
        # -- never splices into the status text.
        s = ''
        if __class__.__line:
            # Cursor to the start of the previous line; erase.
            s += '\x1b[F\x1b[K'
        if line:
            s += line + '\n'
        print(s, end='', file=sys.stderr, flush=True)
        __class__.__line = line

    @staticmethod
    def __get_status_prefix():
        d = datetime.datetime.now() - __class__.__start_time
        p = str(datetime.timedelta(seconds=d.seconds))
        if __class__.__profile:
            num_bools = bools.get_cache_size()
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
    def get_context():
        return ', '.join(p for p in __class__.__parts if p)

    @staticmethod
    def enter(s):
        __class__.__parts.append(s)
        __class__.__update()

    @staticmethod
    def exit(redraw=True):
        __class__.__parts.pop()
        if redraw:
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
                    # On an exception, leave the full status on
                    # display for the report to print under.
                    Status.exit(redraw=exc_type is None)

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


class Bools(eqbool.Context):
    @staticmethod
    def clear():
        # TODO: Clear the _eqbool cache?
        pass

    @staticmethod
    def get_cache_size():
        # TODO: Query the _eqbool cache?
        pass

    def get(self, term):
        if isinstance(term, int):
            assert term in (0, 1), repr(term)
            term = bool(term)

        if isinstance(term, str):
            assert term.strip() == term, repr(term)
            assert term.lower() not in ('', '0', '1', 'true', 'false')
        else:
            assert isinstance(term, bool)

        return super().get(term)

    ''' TODO
    @staticmethod
    def __get_id(v):
        if v.is_const:
            return v.kind
        if v.is_term:
            return v.term
        if v.kind == 'not':
            return '~' + __class__.__get_id(~v)
        return f't{v.id}'

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

    def __str__(self):
        return self.simplified_sexpr()
    '''

    class Storage(object):
        def __init__(self, *, image=None):
            self.__nodes = []
            self.__node_indexes = {}
            OPS = {'not':  bools.get_inversion,
                   'ifelse': bools.ifelse,
                   'or': bools.get_or,
                   'eq': bools.get_eq}
            if image is not None:
                for kind, ops in image:
                    if kind == 'term':
                        assert isinstance(ops, str)
                        b = bools.get(ops)
                    else:
                        b = OPS[kind](*(self.get(op) for op in ops))
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
                ops = [bools.get_inversion(v)] if kind == 'not' else v.args
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
        return x if isinstance(x, eqbool.Bool) else bools.get(x)


bools = Bools()
orders = eqbool.OrderContext(bools)


FALSE = bools.get(False)
TRUE = bools.get(True)


def test_bools():
    def test(actual, expected):
        if actual is expected:
            return

        print('Actual:  ', actual, repr(actual))
        print('Expected:', expected, repr(expected))
        assert 0

    a, b, c, t = (bools.get(v) for v in 'abct')
    eq = bools.get_eq
    ifelse = bools.ifelse

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


class PartialOrders(object):
    # The set of partial orders of node updates under which a
    # value arises, represented as a condition over facts of the
    # form 'x is updated before y'. Facts are ordinary string
    # terms, like 'u29.674<u29.675'; they never appear inside
    # node value expressions, and a fact stated the other way
    # round is the negation of the same term, so two-way cycles
    # vanish on construction, and the propositional
    # simplifications --
    # cancellation of facts stated both ways in otherwise-equal
    # orders, absorption of orders that merely add facts --
    # are eqbool's ordinary business. Combining and uniting are
    # single operations on shared structure, so no products of
    # alternatives ever get materialised.
    #
    # What is not propositional is transitivity, and it is
    # supplied at the queries instead, by eqbool's order
    # context, where the facts are registered as order terms:
    # is_never() decides whether a condition can hold under any
    # consistent order, and same() escalates from identity to
    # equivalence to equivalence under consistent orders.
    __cache = {}
    __seq = 0

    # Conditions carry the pairs of sides they force to order
    # one way -- pairs true in every assignment satisfying the
    # condition. Combining conditions unions the pairs, and a
    # cycle in the union means the combination holds under no
    # consistent order -- found at the very combine that creates
    # it, where the dead entry would otherwise multiply through
    # further cross-applications and cost a solver query to
    # bury. Raw stated pairs suffice: transitive closure adds no
    # new cycles. Uniting alternatives keeps only the pairs all
    # alternatives force -- an honest weakening, with the solver
    # remaining the backstop for whatever the pairs cannot see.
    # Past the cap a condition states no pairs, which is always
    # sound.
    MAX_FORCED_PAIRS = 128

    def __init__(self, e, forced=frozenset()):
        # Use get() instead.
        self.e = e
        self.forced = forced
        self.seq = __class__.__seq
        __class__.__seq += 1

    def __repr__(self):
        if self.e.is_true:
            return 'always'
        if self.e.is_false:
            return 'never'
        return str(self.e)

    @staticmethod
    def get(e, forced=frozenset()):
        r = __class__.__cache.get(e)
        if r is None:
            r = PartialOrders(e, forced)
            __class__.__cache[e] = r
        return r

    @staticmethod
    def get_fact(before, after):
        # The term for the fact 'before is updated before
        # after', in canonical polarity.
        assert before != after
        assert '<' not in before and '<' not in after
        inverted = before > after
        if inverted:
            before, after = after, before
        e = bools.get(f'{before}<{after}')
        orders.register_order(e, before, after)
        if inverted:
            return __class__.get(~e, frozenset(((after, before),)))
        return __class__.get(e, frozenset(((before, after),)))

    @property
    def obviously_never(self):
        return self.e.is_false

    def is_never(self):
        # Whether the condition cannot hold under any order of
        # updates, transitivity taken into account.
        return orders.is_never(self.e)

    def same(self, other):
        # Whether the two conditions mean the same set of
        # orders.
        if self is other:
            return True
        if bools.is_equiv(self.e, other.e):
            return True
        return orders.is_equiv(self.e, other.e)

    @staticmethod
    def __has_cycle(pairs):
        following = {}
        for before, after in pairs:
            following.setdefault(before, []).append(after)

        # Sides are white (unvisited), grey (on the path) or
        # black (done).
        colours = {}
        for side in following:
            if colours.get(side, 0) != 0:
                continue
            path = [(side, 0)]
            colours[side] = 1
            while path:
                s, i = path[-1]
                nexts = following.get(s)
                if nexts is None or i >= len(nexts):
                    colours[s] = 2
                    path.pop()
                    continue
                path[-1] = s, i + 1
                to = nexts[i]
                colour = colours.get(to, 0)
                if colour == 1:
                    return True
                if colour == 0:
                    colours[to] = 1
                    path.append((to, 0))
        return False

    def unite(self, other):
        # The value arises whenever either condition holds.
        if self is other:
            return self
        return __class__.get(self.e | other.e,
                             self.forced & other.forced)

    @staticmethod
    def unite_all(orders):
        # The value arises whenever any of the conditions hold.
        # United n-ary, in bounded chunks: uniting a long run of
        # conditions pairwise would rebuild the growing
        # expression on every step, and one union over the whole
        # run stalls the underlying construction, whose
        # per-argument reduction is quadratic.
        os = list({id(o): o for o in orders}.values())
        forced = frozenset.intersection(*(o.forced for o in os))
        es = [o.e for o in os]
        while len(es) > 1:
            es = [bools.get_or(*es[i:i + 256])
                  for i in range(0, len(es), 256)]
        return __class__.get(es[0], forced)

    def combine(self, other):
        # The value arises whenever both conditions hold.
        if self is other:
            return self
        forced = self.forced | other.forced
        if len(forced) > __class__.MAX_FORCED_PAIRS:
            forced = frozenset()
        elif __class__.__has_cycle(forced):
            return __class__.NEVER
        return __class__.get(self.e & other.e, forced)


PartialOrders.ALWAYS = PartialOrders.get(TRUE)
PartialOrders.NEVER = PartialOrders.get(FALSE)


class PossibleValues(object):
    # The set of possible values of a node, each associated with
    # the partial orders of updates under which the simulation
    # arrives at that value. Almost everywhere a node has a
    # single possible value arising under all orders.
    def __init__(self, entries):
        self.entries = entries

    def __repr__(self):
        # str() of a value spells the expression; repr() only
        # names its kind.
        return ' / '.join(f'{v} [{os!r}]' for v, os in self.entries)

    @property
    def is_single(self):
        # A sole possible value holds under every possible
        # execution, whatever its orders say: orders only ever
        # distinguish values from each other.
        return len(self.entries) == 1

    @property
    def single_value(self):
        assert self.is_single
        return self.entries[0][0]

    # The most distinct possible values a set is allowed to
    # hold; sets going over the budget, never-arising values
    # already pruned, get clipped down to a fresh unconstrained
    # term. Deliberately below the genuine transient diversity
    # observed (about four thousand values on the 'adc n' bus
    # staging): populations under the budget still pay order
    # queries on every merge, so we clip early and bet on the
    # clipped transients being overwritten before any tested
    # node reads them.
    BUDGET = 1024

    # An unpin evaluation runs once per pinned node per
    # settling, so it affords a far larger budget than the
    # per-pass merges the ordinary budget bounds.
    UNPIN_BUDGET = BUDGET * 16

    __num_clipped = 0

    @staticmethod
    def get_num_clipped():
        return __class__.__num_clipped

    @staticmethod
    def clip(num_values):
        # The set is replaced with a fresh term: the node holds
        # some value we no longer track, arising under orders we
        # no longer track either.
        #
        # This is conservative. The term is opaque, so a tested
        # node whose value still depends on it cannot check
        # against its spec expression and fails its test -- a
        # clipped value never certifies as correct, and being a
        # sole possible value, never reports as a race either.
        # Where the term drops out -- overwritten before it
        # matters, or cancelling out of downstream expressions
        # so the value provably does not depend on the lost
        # set -- the verdicts are as good as with full tracking.
        # What is genuinely given up is completeness, never
        # soundness: an instruction whose transients exceed the
        # budget may fail its checks where full tracking would
        # have passed or shown the actual outcomes, but nothing
        # incorrect is ever proven.
        #
        # Each clip gets its own term, so values lost to
        # different clips never spuriously compare equal. A
        # clipped node's term feeds back into the very merge
        # that overflowed, so re-evaluations keep clipping;
        # __update_gate_state pins the node to one per-settling
        # term, or its state would never compare same and the
        # settling could not converge.
        # TODO: The names count from process start, so images
        # cached by different runs may reuse a name for a
        # different clipped set; content-derived names would
        # remove the caveat.
        __class__.__num_clipped += 1
        id = f'clipped{__class__.__num_clipped}'
        Status.print(f'{id} <- {num_values} possible values '
                     f'at {Status.get_context()}')
        return __class__.cast(bools.get(id))

    @staticmethod
    def get(entries, budget=None):
        # Merges equivalent values by uniting their orders and
        # drops values that arise never. Entries are kept sorted
        # by their orders, so equal sets store their entries
        # identically. Redundant combinations mostly produce
        # literally the same hash-consed value, merged here by
        # identity; only distinct values sharing a fingerprint
        # digest need equivalence checks, and those go through
        # one equivalence session -- the entries are variations
        # over a shared structure, which the session tells apart
        # having seen it once. Every merged value's conditions
        # unite in a single n-ary union at the end.
        # Whether distinct values that arise never would push
        # the count back under the budget is deliberately not
        # checked: confirming deadness means solving for every
        # group, or even every condition -- work priced by the
        # very population the budget is here to bound.
        if budget is None:
            budget = __class__.BUDGET

        session = None
        buckets = {}
        num_groups = 0

        for v, orders in entries:
            if orders.obviously_never:
                continue
            bucket = buckets.setdefault(v.fp, {})
            e = bucket.get(v.id)
            if e is None:
                for u_e in bucket.values():
                    if session is None:
                        session = eqbool.EquivSession(bools)
                    if session.is_equiv(u_e[0], v):
                        e = bucket[v.id] = u_e
                        break
                else:
                    bucket[v.id] = v, [orders]
                    num_groups += 1
                    if num_groups > budget:
                        return __class__.clip(num_groups)
                    continue
            e[1].append(orders)

        groups = [e for bucket in buckets.values()
                  for e in {id(entry): entry
                            for entry in bucket.values()}.values()]

        # A sole possible value holds under every possible
        # execution and sheds its orders -- including the union
        # of the ways of arriving at it, which for heavily
        # redundant merges is the bulk of the work.
        if len(groups) == 1:
            return PossibleValues(((groups[0][0], PartialOrders.ALWAYS),))

        merged = []
        for v, orders_list in groups:
            orders = (orders_list[0] if len(orders_list) == 1
                      else PartialOrders.unite_all(orders_list))
            merged.append((v, orders))

        # Values whose orders cannot hold once transitivity is
        # taken into account arise never.
        if len(merged) > 1:
            merged = [(v, orders) for v, orders in merged
                      if not orders.is_never()]

        # A sole possible value holds under every possible
        # execution, so it sheds the orders of the ways of
        # arriving at it.
        if len(merged) == 1:
            merged[0] = merged[0][0], PartialOrders.ALWAYS

        merged.sort(key=lambda e: e[1].seq)
        return PossibleValues(tuple(merged))

    @staticmethod
    def is_same(a, b):
        # Whether the two states -- plain values or sets of
        # possible values -- are known to be the same. Entries
        # are matched by value, as freshly built sets hold their
        # entries in an unrelated order.
        if isinstance(a, PossibleValues) or isinstance(b, PossibleValues):
            a, b = __class__.cast(a), __class__.cast(b)
            if len(a.entries) != len(b.entries):
                return False
            remaining = list(b.entries)
            for av, ao in a.entries:
                fp = av.fp
                for i, (bv, bo) in enumerate(remaining):
                    if fp == bv.fp and bools.is_equiv(av, bv) and ao.same(bo):
                        del remaining[i]
                        break
                else:
                    return False
            return True
        return bools.is_equiv(a, b)

    @staticmethod
    def cast(x):
        if isinstance(x, PossibleValues):
            return x
        return PossibleValues(((x, PartialOrders.ALWAYS),))

    @staticmethod
    def apply(f, *args, budget=None):
        # Applies a function over plain values to every
        # combination of the possible values of the arguments,
        # dropping combinations whose orders are impossible and
        # merging equivalent results. Merging at every operation
        # is what keeps propagating all orders tractable: masked
        # and equal alternatives collapse as they arise.
        args = tuple(__class__.cast(a) for a in args)

        if all(a.is_single for a in args):
            orders = PartialOrders.ALWAYS
            for a in args:
                orders = orders.combine(a.entries[0][1])
            if orders.obviously_never:
                return __class__(())
            return __class__(((f(*(a.entries[0][0] for a in args)),
                               orders),))

        results = []

        def combos(i, values, orders):
            if i == len(args):
                results.append((f(*values), orders))
                return
            for v, os in args[i].entries:
                combined = orders.combine(os)
                if not combined.obviously_never:
                    combos(i + 1, values + (v,), combined)

        combos(0, (), PartialOrders.ALWAYS)
        return __class__.get(results, budget)


def test_orders():
    f = PartialOrders.get_fact

    a_b, b_a = f('a', 'b'), f('b', 'a')
    b_c, a_c = f('b', 'c'), f('a', 'c')
    c_d = f('c', 'd')
    x_y, y_x = f('x', 'y'), f('y', 'x')

    # The reverse of a fact is its negation, so two-way cycles
    # vanish on construction.
    assert a_b.combine(b_a) is PartialOrders.NEVER
    assert x_y.unite(y_x) is PartialOrders.ALWAYS

    # Conditions are uniquified.
    assert a_b.combine(b_c) is b_c.combine(a_b)

    # A fact stated in opposite directions in otherwise-identical
    # orders does not matter.
    assert a_b.combine(x_y).unite(a_b.combine(y_x)).same(a_b)

    # An order that merely adds facts to another one present is
    # covered by it.
    assert a_b.unite(a_b.combine(c_d)).same(a_b)

    # Cycles via transitivity are impossible.
    assert a_b.combine(b_c).combine(f('c', 'a')).is_never()
    assert not a_b.combine(b_c).is_never()

    # Covering via transitivity: a_b and b_c together imply a_c.
    assert a_c.unite(a_b.combine(b_c)).same(a_c)
    assert not a_c.unite(a_b.combine(c_d)).same(a_c)

    # Combining and uniting with the constants.
    assert PartialOrders.ALWAYS.combine(b_c) is b_c
    assert PartialOrders.NEVER.unite(b_c) is b_c
    assert PartialOrders.NEVER.combine(b_c) is PartialOrders.NEVER
    assert PartialOrders.ALWAYS.combine(PartialOrders.NEVER).is_never()


class Bits(object):
    def __init__(self, bits, width=None, suffix=None):
        if isinstance(bits, int):
            assert suffix is None
            n = bits
            assert n >= 0, n
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

        bits = tuple(bools.cast(b) for b in bits)

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

        if all(b.is_const for b in self):
            n = 0
            for i, v in enumerate(b.is_true for b in self):
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
        return __class__(bools.get_or(*bits) for bits in zip(*args))

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
        return bools.get_and(*(bools.get_eq(x, y) for x, y in zip(a, b)))

    def __ne__(self, other):
        return ~self.__eq__(other)

    def __ge__(self, other):
        a, b = __class__.zero_extend_to_same_width(self, other)
        return (a - b)[a.width]

    def __le__(self, other):
        return __class__.cast(other) >= self

    @staticmethod
    def ifelse(cond, a, b):
        cond = bools.cast(cond)
        a, b = __class__.zero_extend_to_same_width(a, b)
        return __class__(bools.ifelse(cond, x, y) for x, y in zip(a, b))

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
        return bools.get_or(*(self == a for a in args))


class Node(object):
    def __init__(self, index, pull, *, custom_id=None):
        self.custom_id = custom_id
        assert pull is None or isinstance(pull, eqbool.Bool), pull
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
        elif self.pull.is_true:
            pull = 'p'
        elif self.pull.is_false:
            pull = 'm'
        else:
            pull = 'x'

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
            state = t.state
            if isinstance(state, PossibleValues):
                # A state with a single possible value stores
                # it plainly.
                if state.is_single:
                    state = self.__bools.add(state.single_value)
                else:
                    state = ['pv'] + [[self.__bools.add(v),
                                       self.__bools.add(orders.e)]
                                      for v, orders in state.entries]
            elif state is not None:
                state = self.__bools.add(state)
            return t.gate.index, t.c1.index, t.c2.index, state

        def get(self, index, image, nodes):
            gate, c1, c2, state = image
            gate, c1, c2 = nodes[gate], nodes[c1], nodes[c2]
            if isinstance(state, (list, tuple)):
                assert state[0] == 'pv'
                state = PossibleValues(tuple(
                    (self.__bools.get(v),
                     PartialOrders.get(self.__bools.get(orders)))
                    for v, orders in state[1:]))
            elif state is not None:
                state = self.__bools.get(state)

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
    bools = Bools.Storage()
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
                        bools.is_equiv(n.pull, pull))

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


class ObservedStates(object):
    # A single gate-state evaluation observes the states of other
    # gates through this class. Observations are memoised, so
    # every node is observed at most once per evaluation and all
    # traversals of the evaluation see the same state. This is
    # the single point where pending updates of other gates
    # become visible to an evaluation, and thus where the order
    # of updates manifests itself.
    #
    # Observations return sets of possible values. For a racing
    # observation -- one where the observed gate has a pending
    # update differing from the state it is about to replace --
    # a settling simulated in all orders forks: the observed set
    # holds the entries of the pending update, each extended
    # with the fact 'the observed gate is updated before the
    # evaluated node', together with the entries of the old
    # state, each extended with the fact stated the other way
    # round. A settling simulated in a single order sees the
    # pending update, as its reader is taken to follow the
    # writer. The node's own state and non-racing observations
    # contribute no facts either way.
    #
    # The order question of a competing pair is asked once per
    # settling: the first observation carries the race, and any
    # later observation of the same pair within the settling
    # sees the pending state deterministically -- the reader is
    # then re-evaluating after the competing update has landed.
    # Without this, re-evaluation trips around feedback loops
    # re-ask the same questions against already-conditioned
    # states and derive different conditions every trip, and the
    # settling never converges.
    def __init__(self, new_states, evaluated=None, all_orders=False,
                 update_no=None, raced=None):
        self.__new_states = new_states
        self.__evaluated = evaluated
        self.__all_orders = all_orders
        self.__update_no = update_no
        self.__raced = raced
        self.__observed = {}

    def __fact(self, before, after):
        # An update event is a node updating within a given
        # group update, so facts surviving a group update are
        # never confused with facts about later updates of the
        # same nodes.
        s = self.__update_no
        return PartialOrders.get_fact(f'u{s}.{before.index}',
                                      f'u{s}.{after.index}')

    @staticmethod
    def __is_same(a, b):
        if a is b:
            return True
        if len(a.entries) != 1 or len(b.entries) != 1:
            return False
        (av, aos), = a.entries
        (bv, bos), = b.entries
        return aos.same(bos) and bools.is_equiv(av, bv)

    def observe(self, t):
        gate = t.gate
        if gate in self.__observed:
            return self.__observed[gate]

        pending = self.__new_states.get(gate)

        if gate is self.__evaluated or pending is None:
            # A node's own update is not something the node's
            # evaluation can race with, and with no pending
            # update there is nothing to race with at all.
            state = t.state if pending is None else pending
            state = PossibleValues.cast(state)
        elif not self.__all_orders:
            state = PossibleValues.cast(pending)
        else:
            applied = PossibleValues.cast(t.state)
            pending = PossibleValues.cast(pending)
            e, g = self.__evaluated, gate
            pair = g.index, e.index
            if __class__.__is_same(applied, pending):
                state = pending
            elif pair in self.__raced:
                state = pending
            else:
                self.__raced.add(pair)
                state = PossibleValues.get(tuple(
                    (v, os.combine(self.__fact(g, e)))
                    for v, os in pending.entries) + tuple(
                    (v, os.combine(self.__fact(e, g)))
                    for v, os in applied.entries))

        self.__observed[gate] = state
        return state

    def observe_floating(self, n):
        # The state the node keeps while floating: its own
        # latest state, not subject to ordering.
        if n in self.__observed:
            return self.__observed[n]

        state = PossibleValues.cast(
            self.__new_states.get(n, n.gate_of[0].state))
        self.__observed[n] = state
        return state


class Z80Simulator(object):
    @property
    def image(self):
        return _make_image(self.__nodes.values(), self.__trans.values())

    def __restore_nodes_from_image(self, names, image, node_storage):
        self.__nodes = {}
        for i in image:
            n = node_storage.get(i[0], i[1:])
            self.__nodes[n.index] = n
            # if bools.is_equiv(n.state, bools.get('ei')):
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
        # A state with a single possible value is sampled as
        # that value.
        if ids is None:
            ids = (n.id for n in self.__nodes.values())
        states = {}
        for id in ids:
            pv = self.get_node_entries(self.__nodes_by_name[id])
            states[id] = pv.single_value if pv.is_single else pv
        return states

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

    def get_node_state(self, n, new_states=None):
        # The value of a node known to have a single possible
        # one; the sampler behind the pin-level accessors.
        pv = self.get_node_entries(n, new_states)
        assert pv.is_single, (n, pv)
        return pv.single_value

    def __get_node_preds(self, n, observed, budget=None):
        def is_const(pv, const):
            return pv.is_single and pv.entries[0][0] is const

        PV_TRUE = PossibleValues.cast(TRUE)
        PV_FALSE = PossibleValues.cast(FALSE)

        def get_group_pred(n, get_node_pred, stack, preds):
            cyclic = False

            p = preds.get(n)
            if p is not None:
                return cyclic, p

            p = get_node_pred(n)
            if p is not None:
                p = PossibleValues.cast(p)
                if is_const(p, TRUE):
                    preds[n] = p
                    return cyclic, p

            if n.is_gnd_or_pwr:
                assert p is None
                preds[n] = PV_FALSE
                return cyclic, PV_FALSE

            p = [] if p is None else [p]
            stack.append(n)
            for t in n.conn_of:
                state = observed.observe(t)
                if not is_const(state, FALSE):
                    m = t.get_other_conn(n)
                    if m in stack:
                        cyclic = True
                    else:
                        cc, pp = get_group_pred(m, get_node_pred,
                                                stack, preds)
                        cyclic |= cc

                        mp = PossibleValues.apply(
                            lambda s, q: s & q, state, pp,
                            budget=budget)
                        if is_const(mp, TRUE):
                            p = [PV_TRUE]
                            break
                        p.append(mp)

            # Fold pairwise: merging after every operation is
            # what keeps the sets small.
            r = None
            for x in p:
                r = x if r is None else \
                    PossibleValues.apply(bools.get_or, r, x,
                                         budget=budget)
            p = PossibleValues.apply(bools.get_or) if r is None else r
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

            assert isinstance(n.pull, eqbool.Bool)
            return n.pull

        def get_pulldown_pred(n):
            if n.pull is None:
                return None

            assert isinstance(n.pull, eqbool.Bool)
            return ~n.pull

        _, gnd = get_group_pred(n, get_gnd_pred, [], {})
        _, pwr = get_group_pred(n, get_pwr_pred, [], {})
        _, pullup = get_group_pred(n, get_pullup_pred, [], {})
        _, pulldown = get_group_pred(n, get_pulldown_pred, [], {})

        return gnd, pwr, pullup, pulldown

    def get_node_entries(self, n, new_states=None, all_orders=False,
                         budget=None):
        # The full state of node n: the set of its possible
        # values, each with the partial orders of updates under
        # which the evaluation arrives at it -- a sole value
        # arising under all orders wherever no racing observation
        # forks. Sets propagate through the whole computation,
        # merging at every operation.
        if new_states is None:
            new_states = {}
        observed = ObservedStates(new_states, evaluated=n,
                                  all_orders=all_orders,
                                  update_no=self.update_no,
                                  raced=self.__raced_pairs)
        gnd, pwr, pullup, pulldown = self.__get_node_preds(
            n, observed, budget=budget)

        if len(n.gate_of) == 0:
            floating = PossibleValues.cast(
                bools.get('<floating-non-gate>'))
        else:
            floating = observed.observe_floating(n)
        pull = PossibleValues.apply(
            lambda pd, pu, f: bools.ifelse(pd | pu, ~pd, f),
            pulldown, pullup, floating, budget=budget)
        return PossibleValues.apply(
            lambda g, p, pl: bools.ifelse(g | p, ~g, pl),
            gnd, pwr, pull, budget=budget)

    def __update_gate_state(self, n, new_states, all_orders):
        assert not n.is_gnd_or_pwr

        old_state = new_states.get(n, n.gate_of[0].state)

        # A node whose evaluation clipped is pinned for the rest
        # of the settling -- a changing stand-in would feed back
        # into the very merge that overflowed and the settling
        # could never converge. The stand-in is the node's
        # settling-start state: it is exactly what the hold path
        # of a feedback loop means, so loop members pinned this
        # way evaluate to properly anchored values at the
        # fixpoint, where every pin must then be verified -- a
        # stand-in the settled evaluation does not confirm is
        # replaced, an overflowing one by an opaque term.
        new_state = self.__clipped_nodes.get(n)
        if new_state is None:
            num_clipped = PossibleValues.get_num_clipped()
            new_state = self.get_node_entries(n, new_states, all_orders)
            if PossibleValues.get_num_clipped() != num_clipped:
                new_state = self.__settle_start_states.get(
                    n, n.gate_of[0].state)
                Status.print(f'{n} pinned to its settling-start state '
                             f'at {Status.get_context()}')
                self.__clipped_nodes[n] = new_state

        # No further propagation is necessary if the state of
        # the transistor is known to be same. This includes
        # the case of a floating gate.
        if PossibleValues.is_same(new_state, old_state):
            return False

        new_states[n] = new_state
        return True

    def __update_groups_of(self, nodes, all_orders=False):
        # TODO: Does always updating all nodes lead to any failures?
        # nodes = list(self.__nodes.values())

        # Every settling of the circuit is a new scope of update
        # events for the facts partial orders are made of, and a
        # fresh chance for every competing pair to be asked its
        # order question.
        self.update_no += 1
        self.__raced_pairs = set()
        self.__clipped_nodes = {}
        self.__settle_start_states = {}
        self.__oscillating_nodes = set()
        self.__num_unpin_waves = 0

        groups = []
        for n in nodes:
            if n.group not in groups:
                assert n.group is not None, n
                groups.append(n.group)
        del nodes

        # A pin holds the node at its settling-start state while
        # the settling converges, and the fixpoint must then
        # verify every pin: the settled evaluation, run at a
        # generous budget, either confirms the stand-in (the
        # node genuinely held), or replaces it with the true
        # settled value, or overflows even at that budget --
        # then the value is genuinely lost, and an opaque term
        # must take the node, since leaving the start value in
        # place could certify falsely. Waves repeat at every
        # fixpoint: a loop member evaluates properly only after
        # its neighbours' pins resolve. The wave cap bounds the
        # ping-pong of a loop that keeps resolving and
        # re-pinning.
        def try_unpinning():
            self.__num_unpin_waves += 1
            if self.__num_unpin_waves > 30:
                # Past the cap the remaining stand-ins must not
                # stay: an unverified start value could certify
                # falsely. The opaque terms take the nodes.
                for n, pinned in self.__clipped_nodes.items():
                    id = f'clipped.{self.update_no}.{n.index}'
                    state = PossibleValues.cast(bools.get(id))
                    if PossibleValues.is_same(state, pinned):
                        continue
                    self.__clipped_nodes[n] = state
                    Status.print(f'{id} <- the state of {n} '
                                 f'at {Status.get_context()}')
                    for t in n.gate_of:
                        t.state = state
                        if t.conns_group not in groups:
                            groups.append(t.conns_group)
                return bool(groups)
            for n in list(self.__clipped_nodes):
                with Status.do(f'verify pin {n}'):
                    num_clipped = PossibleValues.get_num_clipped()
                    state = self.get_node_entries(
                        n, all_orders=all_orders,
                        budget=PossibleValues.UNPIN_BUDGET)

                    # An oscillating node's fresh evaluation
                    # just wraps its loop once more; committing
                    # it would ping-pong through the waves, so
                    # anything but a confirmed hold turns
                    # opaque.
                    lost = PossibleValues.get_num_clipped() != num_clipped
                    if not lost and n in self.__oscillating_nodes:
                        lost = not PossibleValues.is_same(
                            state, self.__clipped_nodes[n])
                    if lost:
                        # The value is lost for good.
                        id = f'clipped.{self.update_no}.{n.index}'
                        state = PossibleValues.cast(bools.get(id))
                        if PossibleValues.is_same(
                                state, self.__clipped_nodes[n]):
                            continue
                        self.__clipped_nodes[n] = state
                        Status.print(f'{id} <- the state of {n} '
                                     f'at {Status.get_context()}')
                    elif PossibleValues.is_same(
                            state, self.__clipped_nodes[n]):
                        # The stand-in verified: the node held.
                        del self.__clipped_nodes[n]
                        Status.print(f'pin of {n} verified '
                                     f'at {Status.get_context()}')
                        continue
                    else:
                        del self.__clipped_nodes[n]
                        values = '; '.join(
                            str(v)[:60] for v, _ in state.entries[:2])
                        Status.print(
                            f'pin of {n} resolved '
                            f'({len(state.entries)} values: {values}) '
                            f'at {Status.get_context()}')
                    for t in n.gate_of:
                        t.state = state
                        if t.conns_group not in groups:
                            groups.append(t.conns_group)
            return bool(groups)

        round = 0
        while groups or try_unpinning():
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
                pass_no = 0
                repeat = True
                while repeat:
                    pass_no += 1
                    repeat = False
                    with Status.do(f'pass {pass_no}'):
                        for i, n in enumerate(gates):
                            with Status.do(f'gate {i}/{len(gates)}'):
                                changed = self.__update_gate_state(
                                    n, new_states, all_orders)
                                repeat |= changed

                                # A gate still changing this
                                # deep into the pass loop has no
                                # fixpoint to find: under some
                                # of its conditions the loop it
                                # is on genuinely oscillates,
                                # its value wrapping the loop
                                # once more per pass. Pin it;
                                # the fixpoint verification then
                                # confirms a held value or
                                # honestly turns the node
                                # opaque.
                                if changed and pass_no > 100 \
                                        and n not in self.__clipped_nodes:
                                    stand_in = \
                                        self.__settle_start_states.get(
                                            n, n.gate_of[0].state)
                                    self.__clipped_nodes[n] = stand_in
                                    self.__oscillating_nodes.add(n)
                                    new_states[n] = stand_in
                                    Status.print(
                                        f'{n} pinned as oscillating '
                                        f'at {Status.get_context()}')

                # Apply new states, keeping each node's state
                # from before the settling first touched it --
                # the value the hold path of a feedback loop
                # refers to, and so the stand-in for pinning.
                groups = []
                for n, state in new_states.items():
                    if n not in self.__settle_start_states:
                        self.__settle_start_states[n] = n.gate_of[0].state
                    for t in n.gate_of:
                        t.state = state
                        if t.conns_group not in groups:
                            groups.append(t.conns_group)

    def __set_node_pull(self, n, pull):
        pull = bools.cast(pull)
        if '--show-set-nodes' in sys.argv:
            Status.print(n, pull)
        n.pull = pull

    def __set_node(self, n, pull, all_orders=False):
        self.__set_node_pull(n, pull)
        self.__update_groups_of([n], all_orders)

    def half_tick(self, *, cond=None, all_orders=False):
        if self.__memory is not None and self.clk:
            if self.mreq and not self.rfsh and not self.iorq:
                if self.m1 and self.rd and self.t2:
                    self.dbus = self.__memory[int(self.abus)]

        if cond is None:
            cond = TRUE

        nclk = bools.ifelse(cond, ~self.nclk, self.nclk)
        self.__set_node(self.__nclk, nclk, all_orders)

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

        bools = Bools.Storage(image=bools)
        node_storage = Node.Storage(bools, image=node_storage)
        trans_storage = Transistor.Storage(bools)

        # Counts group updates; scopes their update events.
        self.update_no = 0

        # The competing pairs whose order questions the current
        # settling has already asked.
        self.__raced_pairs = set()

        # The nodes the current settling has clipped, each
        # pinned to its stand-in for the settling.
        self.__clipped_nodes = {}

        # The state each node had before the current settling
        # first touched it -- the pinning stand-in.
        self.__settle_start_states = {}

        # The nodes the current settling found oscillating --
        # changing on every pass with no fixpoint to reach.
        self.__oscillating_nodes = set()

        # Pin-verification waves already run in the current
        # settling; the cap bounds the resolve-and-re-pin
        # ping-pong a feedback loop can sustain.
        self.__num_unpin_waves = 0

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

    def get_node_id(self, index):
        return self.__nodes[index].id

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

    def update_pin(self, pin, all_orders=False):
        n = self.__nodes_by_name[pin]
        self.__update_groups_of([n], all_orders)

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
        bools = Bools.Storage()

        def get_step_element_image(e):
            if isinstance(e, (str, int, type(None))):
                return e
            if isinstance(e, eqbool.Bool):
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
            _, _, pin, *mode = step
            sim.update_pin(pin, all_orders=bool(mode))
        elif kind == 'power_up':
            _, _, = step
            sim.power_up()
        elif kind == 'reset':
            _, _, propagation_delay, waiting_for_m1_delay = step
            sim.reset(propagation_delay, waiting_for_m1_delay)
        elif kind == 'half_tick':
            _, _, *mode = step
            sim.half_tick(all_orders=bool(mode))
        elif kind == 'conditional_half_tick':
            _, _, cond, *mode = step
            sim.half_tick(cond=cond, all_orders=bool(mode))
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
        step = 'set_pin_pull', pin, bools.cast(pull)
        self.__add_step(step)

    # Order tracking is a property of how a settling is
    # simulated, so it is a parameter of the steps that cause
    # settlings, present in the step only when tracking -- the
    # step list remains the complete description of the
    # computation, and cache keys, being hashes of step lists,
    # come out mode-specific automatically. The parameters'
    # values are the clipping budget, which bounds what the
    # settled states hold, and the all-orders simulation
    # version, bumped whenever the simulation changes what a
    # settled state contains -- states computed under different
    # budgets or semantics must never share a cache entry.
    # Plain images are unaffected by either.
    ALL_ORDERS_VERSION = 7

    def update_pin(self, pin, all_orders=False):
        step = ('update_pin', pin)
        if all_orders:
            step += PossibleValues.BUDGET, self.ALL_ORDERS_VERSION
        self.__add_step(step)

    def set_pin_and_update(self, pin, pull, all_orders=False):
        self.set_pin_pull(pin, pull)
        self.update_pin(pin, all_orders)

    def set_pins_and_update(self, pin, bits, all_orders=False):
        for i, b in enumerate(Bits.cast(bits)):
            self.set_pin_and_update(f'{pin}{i}', b, all_orders)

    def set_db(self, bits, all_orders=False):
        if isinstance(bits, str):
            bits = Bits(bits, width=8)
        else:
            bits = Bits.cast(bits, width=8)
        self.set_pins_and_update('db', bits, all_orders)

    def set_db_and_wait(self, bits, ticks):
        self.set_db(bits)
        self.ticks(ticks)

    def power_up(self):
        self.__add_step(('power_up',))

    def reset(self, propagation_delay, waiting_for_m1_delay):
        self.__add_step(('reset', propagation_delay,
                         waiting_for_m1_delay))

    def half_tick(self, *, cond=None, all_orders=False):
        if cond is None:
            step = ('half_tick',)
        else:
            step = ('conditional_half_tick', cond)
        if all_orders:
            step += PossibleValues.BUDGET, self.ALL_ORDERS_VERSION
        self.__add_step(step)

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

            # The expression contexts only ever grow --
            # hash-consing never frees -- so a long computation
            # accumulates expressions nothing references any
            # more. With the state just stored, a worker over
            # the memory cap can stop here and let a fresh
            # process resume from this very point, holding only
            # what the state actually references.
            if (WORKER_MEMORY is not None and
                    THREADS is not None and
                    get_rss() > WORKER_MEMORY * (1 << 30)):
                raise MemoryCapReached()

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


# The families where the prefix redirects the HL role; the
# ED families are deliberately absent -- there the prefix
# is proven ignored.
PREFIX_HL_REDIRECT = (
    'jp hl', 'ld sp, hl', 'ex (sp), hl', 'add hl, <rp>',
    'inc/dec <rp>', 'ld <rp>, nn', 'pop <rp2>',
    'push <rp2>', 'ld hl, (nn)/ld (nn), hl')

# The families where the prefix turns the (hl) operand into
# (ix+d)/(iy+d): a conditional displacement read and the
# address computation join the cycles, and the memory operand
# address becomes the indexed one. The register operands, h
# and l included, stay the true registers -- the halves
# redirection below does not apply to the displaced forms.
PREFIX_DISP = (
    'ld {b, c, d, e, h, l, a}, (hl)',
    'ld (hl), {b, c, d, e, h, l, a}', 'ld (hl), n',
    'inc/dec (hl)',
    'add (hl)', 'adc (hl)', 'sub (hl)', 'sbc (hl)',
    'and (hl)', 'xor (hl)', 'or (hl)', 'cp (hl)',
    'rot {b, c, d, e, h, l, a}', 'rot (hl)',
    'bit {b, c, d, e, h, l, a}', 'bit (hl)',
    'res {b, c, d, e, h, l, a}', 'res (hl)',
    'set {b, c, d, e, h, l, a}', 'set (hl)')

# The scf/ccf context map: the instruction immediately before
# an scf/ccf decides whether its XF/YF race. These families
# are measured to suppress the race entirely -- the following
# scf/ccf copies A's bits deterministically under every order
# of updates. Every confirmed member writes A as well as F;
# cp n, which writes F but leaves A, is measured to yield a
# different set of its own (operand bits and the interposed
# prefix byte involved) and awaits its exact expectation. The
# tuples grow as the context map is measured; the paired
# sequences below certify each entry in the plain run.
SCF_RACE_SUPPRESSORS = (
    'adc (hl)', 'adc n', 'adc {b, c, d, e, h, l, a}', 'add (hl)',
    'add n', 'add {b, c, d, e, h, l, a}', 'and (hl)', 'and n',
    'and {b, c, d, e, h, l, a}', 'cpl', 'daa', 'neg/xneg',
    'or (hl)', 'or n', 'or {b, c, d, e, h, l, a}',
    'rlca/rrca/rla/rra', 'rrd/rld', 'sbc (hl)', 'sbc n',
    'sbc {b, c, d, e, h, l, a}', 'sub (hl)', 'sub n',
    'sub {b, c, d, e, h, l, a}', 'xor (hl)', 'xor n',
    'xor {b, c, d, e, h, l, a}')

# The measured contexts that leave the four-value race set
# intact.
SCF_RACE_KEEPERS = (
    'call cc, nn', 'call nn', 'djnz d', 'ei/di', 'ex (sp), hl',
    "ex af, af'", 'ex de, hl', 'exx', 'im/xim n',
    'in a, (n)/out (n), a', 'inc/dec <rp>', 'jp cc, nn', 'jp hl',
    'jp nn', 'jr cc, d', 'jr d', 'ld (<rp>), a/ld a, (<rp>)',
    'ld (hl), n', 'ld (hl), {b, c, d, e, h, l, a}',
    'ld <rp>, (nn)/ld (nn), <rp>', 'ld <rp>, nn',
    'ld a, (nn)/ld (nn), a', 'ld hl, (nn)/ld (nn), hl',
    'ld sp, hl', 'ld {b, c, d, e, h, l, a}, (hl)',
    'ld {b, c, d, e, h, l, a}, n',
    'ld {b, c, d, e, h, l, a}, {b, c, d, e, h, l, a}',
    'ld {i, r}, a/ld a, {i, r}', 'nop', 'out (c), r',
    'pop <rp2>', 'push <rp2>', 'res (hl)',
    'res {b, c, d, e, h, l, a}', 'ret', 'ret cc',
    'reti/retn/xretn', 'rst n', 'set (hl)',
    'set {b, c, d, e, h, l, a}', 'xnop')

# The families whose flag write forces a directly following
# scf/ccf to the accumulator, the forcing not surviving an
# interposed DD/FD prefix byte: under the prefixed variant
# the four-value race returns. scf/ccf itself is one: its
# own fresh F contains A, so the four values collapse to
# two.
SCF_FRAGILE_SUPPRESSORS = (
    'adc/sbc hl, <rp>', 'add hl, <rp>', 'bit (hl)',
    'bit {b, c, d, e, h, l, a}', 'cp (hl)', 'cp n',
    'cp {b, c, d, e, h, l, a}', 'cpi/cpd', 'cpir/cpdr',
    'in r, (c)', 'inc/dec (hl)', 'inc/dec {b, c, d, e, h, l, a}',
    'ini/ind', 'inir/indr', 'ldi/ldd', 'ldir/lddr', 'otir/otdr',
    'outi/outd', 'rot (hl)', 'rot {b, c, d, e, h, l, a}',
    'scf/ccf')

SCF_CONTEXTS = (SCF_RACE_SUPPRESSORS + SCF_RACE_KEEPERS +
                SCF_FRAGILE_SUPPRESSORS)

# The families where the prefix redirects the 8-bit H and L
# roles to the undocumented IX/IY halves; a and the other
# registers are unaffected.
PREFIX_HALVES_REDIRECT = (
    'ld {b, c, d, e, h, l, a}, {b, c, d, e, h, l, a}',
    'ld {b, c, d, e, h, l, a}, n',
    'inc/dec {b, c, d, e, h, l, a}',
    'add {b, c, d, e, h, l, a}', 'adc {b, c, d, e, h, l, a}',
    'sub {b, c, d, e, h, l, a}', 'sbc {b, c, d, e, h, l, a}',
    'and {b, c, d, e, h, l, a}', 'xor {b, c, d, e, h, l, a}',
    'or {b, c, d, e, h, l, a}', 'cp {b, c, d, e, h, l, a}')


class CheckToken(object):
    pass


class TestFailure(Exception):
    pass


def test_node(instrs, n, at_start, at_end, before, after,
              all_orders=False):
    def fail(b, a, get_expected_lines):
        Status.clear()

        NAMES = {CF: 'cf', NF: 'nf', PF: 'pf', XF: 'xf',
                 HF: 'hf', YF: 'yf', ZF: 'zf', SF: 'sf',
                 CF2: 'cf', NF2: 'nf', PF2: 'pf', XF2: 'xf',
                 HF2: 'hf', YF2: 'yf', ZF2: 'zf', SF2: 'sf',
                 IFF1: 'iff1', IFF2: 'iff2'}
        n_name = str(n)
        if n in NAMES:
            n_name = f'{n_name} ({NAMES[n]})'

        def emit(line):
            # Streamed as produced: verbalising a large outcome
            # set takes time, and a partial report is still a
            # report.
            print(line, file=sys.stderr, flush=True)

        emit('')
        emit('FAILED: ' + '; '.join(instrs) + f' {n_name}')
        if '--no-before-after-expected' not in sys.argv:
            # Verbalised the way the reporting mode does it: a
            # racing node's outcome is a set, each value with
            # the order condition it arises under.
            with Status.do('verbalise'):
                emit(f'  before: {get_z3_simplified(b)}')
                if a.is_single:
                    emit(f'  after: {get_z3_simplified(a.single_value)}')
                else:
                    emit(f'  after: {len(a.entries)} possible values')
                    for no, (v, os) in enumerate(a.entries):
                        emit(f'    {no}) {get_z3_simplified(v)}')
                        emit(f'       under {get_z3_simplified(os.e)}')
                for line in get_expected_lines():
                    emit(line)
        raise TestFailure()

    def check(x):
        if n.startswith('instr'):
            b, a = at_start[n], at_end[n]
        else:
            b, a = before[n], after[n]
            x = is_active(n).ifelse(x, b)

        # A state with several possible values fails its check:
        # racing nodes state their outcome sets via check_set().
        a = PossibleValues.cast(a)
        if a.is_single and bools.is_equiv(a.single_value, x):
            return CheckToken()

        def get_expected_lines():
            lines = [f'  expected: {get_z3_simplified(x)}']
            if a.is_single:
                lines.append(
                    f'  diff: {get_z3_simplified(a.single_value ^ x)}')
            return lines

        fail(b, a, get_expected_lines)

    def check_set(*xs):
        # A racing node's spec is the exact set of its possible
        # outcomes: every outcome must be one of these values,
        # and every value must arise. Which orders yield which
        # value is deliberately no part of the spec -- that
        # would freeze incidental internals; the set is the
        # hardware-facing promise.
        b, a = before[n], after[n]
        xs = [is_active(n).ifelse(x, b) for x in xs]
        # A spec whose values coincide in this context is the
        # smaller set -- scf/ccf after scf/ccf collapses to
        # two values because the fresh F contains A.
        distinct = []
        for x in xs:
            if not any(bools.is_equiv(x, d) for d in distinct):
                distinct.append(x)
        xs = distinct
        a = PossibleValues.cast(a)

        matched = len(a.entries) == len(xs)
        if matched:
            remaining = list(xs)
            for v, _ in a.entries:
                for j, x in enumerate(remaining):
                    if bools.is_equiv(v, x):
                        del remaining[j]
                        break
                else:
                    matched = False
                    break
        if matched:
            return CheckToken()

        def get_expected_lines():
            lines = [f'  expected: exactly these {len(xs)} values']
            for no, x in enumerate(xs):
                lines.append(f'    {no}) {get_z3_simplified(x)}')
            return lines

        fail(b, a, get_expected_lines)

    phase = len(instrs)
    instr = instrs[-1]
    cycles = TestedInstrs.get_cycles(instr, phase)
    # The main opcode: past a conditional prefix fetch and past
    # a CB/ED prefix byte.
    ci = 1 if cycles[0][2] == 'is_prefixed' else 0
    opcode, ticks, cond = cycles[ci]
    if opcode.value in (0xcb, 0xed):
        opcode, ticks, cond = cycles[ci + 1]
    opcode = opcode.zero_extended(8)
    is_prefixed = (bools.get(f'is_prefixed_p{len(instrs)}')
                   if ci else FALSE)
    prefix_bit = Bits.ifelse(is_prefixed, 1, 0)

    AT_HL, A = 6, 7

    def phased(x, offset=0):
        if isinstance(x, str):
            assert '_p' not in x, x
            return f'{x}_p{phase + offset}'
        return x

    def phased_bool(x):
        return bools.get(phased(x))

    def bits(id):
        return Bits(before[f'{id}{i}'] for i in range(8))

    def is_active(n):
        if n in (IFF1, IFF2, IM_LATCH0, IM_LATCH1,
                 HALTED_FF, HALTED_FF_B,
                 EX_AF_FF, EXX_FF, EX_DE_FF, '~halt'):
            return TRUE
        if n in (EX_DE_FF0, EX_DE_FF1):
            exx = before[EXX_FF]
            return ~exx if n == EX_DE_FF0 else exx
        if n.startswith('reg_'):
            if n[4] in 'wz':
                return TRUE
            if n[4:6] in ('pc', 'sp') or n[4] in 'ir':
                return TRUE
            if n[4:6] in ('ix', 'iy'):
                return TRUE
            if n[4] in 'bcdehl':
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

    def get_ix():
        return Bits.concat(bits('reg_ixh'), bits('reg_ixl'))

    def get_iy():
        return Bits.concat(bits('reg_iyh'), bits('reg_iyl'))

    def get_ixy():
        return Bits.ifelse(phased_bool('is_iy'), get_iy(),
                           get_ix())

    def get_hl_eff():
        # The prefix redirects the HL role to IX/IY in the
        # families where it acts.
        return Bits.ifelse(is_prefixed, get_ixy(), get_hl())

    def get_h_eff():
        return Bits.ifelse(is_prefixed,
                           Bits(get_ixy().bits[8:16]), get_h())

    def get_l_eff():
        return Bits.ifelse(is_prefixed,
                           Bits(get_ixy().bits[0:8]), get_l())

    def get_r_eff(r):
        # Reads of the 8-bit H and L roles go to the IX/IY
        # halves in the families where the prefix redirects
        # them.
        if instr not in PREFIX_HALVES_REDIRECT:
            return get_r(r)
        return Bits.ifelse(
            r[2],
            Bits.ifelse(
                r[1],
                Bits.ifelse(r[0], get_a(), get_at_hl()),
                Bits.ifelse(r[0], get_l_eff(), get_h_eff())),
            Bits.ifelse(
                r[1],
                Bits.ifelse(r[0], get_e(), get_d()),
                Bits.ifelse(r[0], get_c(), get_b())))

    def get_pc_plus(off):
        # On tick 3 of an instruction the value of pc has already
        # been advanced, so we in fact get (pc + 1).
        pc = Bits.concat(bits('reg_pch'), bits('reg_pcl'))
        if off >= 1:
            return pc + (off - 1)
        return pc - (1 - off)

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
            return check(bools.ifelse(phased_bool('is_scf'), TRUE,
                                      ~get_active(CF)))
        if instr == 'rlca/rrca/rla/rra':
            a = get_a()
            return check(bools.ifelse(phased_bool('is_rl'), a[7], a[0]))

    if n in NFF:
        if instr == 'cpl':
            return check(TRUE)
        if instr in ('inc/dec {b, c, d, e, h, l, a}', 'inc/dec (hl)'):
            return check(bools.ifelse(phased_bool('is_inc'), FALSE, TRUE))
        if instr in ('scf/ccf', 'rlca/rrca/rla/rra', 'add hl, <rp>'):
            return check(FALSE)

    if n in XFF + YFF:
        i = int(n[-1])
        a = get_a()[i]
        if instr == 'cpl':
            return check(~a)
        if instr == 'scf/ccf':
            f = get_f()[i]

            # Under all orders the outcome hangs on the
            # preceding instruction: a predecessor whose flag
            # write is measured to suppress the race leaves XF
            # and YF copying A's bits, deterministically;
            # otherwise each bit lands on one of the proven
            # race set of issue #51, independently, the choice
            # hanging on genuine update races the netlist
            # leaves undecided.
            if all_orders:
                if (len(instrs) > 1 and
                        instrs[-2] in SCF_RACE_SUPPRESSORS):
                    return check(a)
                if (len(instrs) > 1 and
                        instrs[-2] in SCF_FRAGILE_SUPPRESSORS):
                    p = bools.get(f'is_prefixed_p{phase}')
                    return check_set(
                        a,
                        bools.ifelse(p, f, a),
                        bools.ifelse(p, a | f, a),
                        bools.ifelse(p, a & f, a))
                return check_set(a, f, a | f, a & f)

            # A single-order run lands on the one value its
            # order yields; this describes the default order.
            # See <https://github.com/kosarev/z80/issues/42>.
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
            return check(bools.ifelse(ignores_f, a, a | f))
        if instr == 'rlca/rrca/rla/rra':
            a = get_a()
            return check(bools.ifelse(phased_bool('is_rl'), a[i - 1],
                                      a[i + 1]))

    if n in HFF:
        if instr == 'cpl':
            return check(TRUE)
        if instr == 'rlca/rrca/rla/rra':
            return check(FALSE)
        if instr == 'scf/ccf':
            return check(bools.ifelse(phased_bool('is_scf'), FALSE,
                                      get_active(CF)))

    if n.startswith('reg_a'):
        i = int(n[-1])
        if instr == 'cpl':
            return check(~before[n])
        if instr in ('ld a, (nn)/ld (nn), a', 'ld (<rp>), a/ld a, (<rp>)'):
            id = 'rw3' if instr == 'ld a, (nn)/ld (nn), a' else 'rw'
            v = Bits(phased(id), width=8)
            a = Bits.ifelse(phased_bool('is_store'), get_a(), v)
            return check(a[i])
        if instr in ('ld {b, c, d, e, h, l, a}, n',
                     'ld {b, c, d, e, h, l, a}, (hl)'):
            v = Bits(phased('r'), width=8)
            a = Bits.ifelse(Bits(opcode[3:6]) == A, v, get_a())
            return check(a[i])
        if instr == 'ld {b, c, d, e, h, l, a}, {b, c, d, e, h, l, a}':
            s = get_r_eff(opcode[0:3])
            d = Bits(opcode[3:6])
            a = Bits.ifelse(d == A, s, get_a())
            return check(a[i])
        if instr == 'in a, (n)/out (n), a':
            v = Bits(phased('io'), width=8)
            a = Bits.ifelse(phased_bool('is_in'), v, get_a())
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
        if instr in PREFIX_DISP:
            # The displaced forms load WZ with the computed
            # operand address; the plain (hl) forms leave it
            # alone.
            d = Bits(phased('d'), width=8)
            t = (get_ixy() + d.sign_extended(16)).truncated(16)
            return check(Bits.ifelse(is_prefixed, t,
                                     get_wz())[i])
        if instr == 'add hl, <rp>':
            return check((get_hl_eff() + 1)[i])
        if instr in ('rrd/rld', 'adc/sbc hl, <rp>'):
            return check((get_hl() + 1)[i])
        # jp cc and call cc load WZ with the target address
        # whether or not they are taken.
        if instr in ('call nn', 'call cc, nn', 'ex (sp), hl'):
            wz = Bits.concat(Bits(phased('r2'), width=8),
                             Bits(phased('r1'), width=8))
            return check(wz[i])
        if instr in ('jp nn', 'jp cc, nn', 'ret'):
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
            cc = bools.ifelse(
                cc[2],
                bools.ifelse(
                    cc[1],
                    bools.ifelse(cc[0], sf, ~sf),
                    bools.ifelse(cc[0], pf, ~pf)),
                bools.ifelse(
                    cc[1],
                    bools.ifelse(cc[0], cf, ~cf),
                    bools.ifelse(cc[0], zf, ~zf)))
            target = Bits.concat(Bits(phased('r2'), width=8),
                                 Bits(phased('r1'), width=8))
            wz = Bits.ifelse(cc, target, get_wz())
            return check(wz[i])
        if instr == 'jr d':
            d = Bits(phased('r'), width=8)
            wz = get_pc_plus(2) + prefix_bit + d.sign_extended(16)
            return check(wz[i])
        if instr == 'djnz d':
            b = get_b()
            d = Bits(phased('r'), width=8)
            target = (get_pc_plus(2) + prefix_bit +
                      d.sign_extended(16))
            wz = Bits.ifelse(b != 1, target, get_wz())
            return check(wz[i])
        if instr == 'jr cc, d':
            cc2 = Bits(phased('cc2'), width=2)
            cf, zf = get_active(CF), get_active(ZF)
            cc2 = bools.ifelse(
                cc2[1],
                bools.ifelse(cc2[0], cf, ~cf),
                bools.ifelse(cc2[0], zf, ~zf))
            d = Bits(phased('r'), width=8)
            target = (get_pc_plus(2) + prefix_bit +
                      d.sign_extended(16))
            wz = Bits.ifelse(cc2, target, get_wz())
            return check(wz[i])
        if instr == 'in a, (n)/out (n), a':
            a = get_a()
            r = Bits(phased('r'), width=8)
            in_wz = Bits.concat(a, r) + 1
            out_wz = Bits.concat(a, (r + 1).truncated(8))
            return check(bools.ifelse(phased_bool('is_in'), in_wz[i],
                                      out_wz[i]))
        if instr == 'rst n':
            wz = (Bits(phased('y'), width=3) << 3).zero_extended(16)
            return check(wz[i])
        if instr == 'ldir/lddr':
            # WZ changes only when the instruction repeats: then
            # it takes the address of the second opcode byte.
            wz = Bits.ifelse(get_bc() != 1,
                             get_pc_plus(1) + prefix_bit,
                             get_wz())
            return check(wz[i])
        if instr in ('cpi/cpd', 'cpir/cpdr'):
            wz = get_wz()
            wz = Bits.ifelse(phased_bool('is_cpd'), wz - 1, wz + 1)
            if instr == 'cpir/cpdr':
                # A repeating instruction takes the address of
                # the second opcode byte instead.
                a, s = get_a(), Bits(phased('r'), width=8)
                wz = Bits.ifelse((get_bc() != 1) & (a != s),
                                 get_pc_plus(1) + prefix_bit,
                                 wz)
            return check(wz[i])
        if instr in ('ini/ind', 'inir/indr', 'outi/outd', 'otir/otdr'):
            if instr in ('ini/ind', 'inir/indr'):
                # The in forms take BC before decrementing B;
                # the out forms after.
                bc = Bits.concat(get_b(), get_c())
                is_dec = phased_bool('is_ind')
            else:
                bc = Bits.concat((get_b() - 1).truncated(8), get_c())
                is_dec = phased_bool('is_outd')
            wz = Bits.ifelse(is_dec, bc - 1, bc + 1)
            if instr in ('inir/indr', 'otir/otdr'):
                # A repeating instruction takes the address of
                # the second opcode byte instead.
                wz = Bits.ifelse(get_b() != 1,
                                 get_pc_plus(1) + prefix_bit,
                                 wz)
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
        if instr in ('in r, (c)', 'out (c), r'):
            wz = get_bc() + 1
            return check(wz[i])
        if instr == 'ld <rp>, (nn)/ld (nn), <rp>':
            lo = Bits(phased('r3'), width=8)
            hi = Bits(phased('r4'), width=8)
            wz = Bits.concat(hi, lo) + 1
            return check(wz[i])

    def get_cc_taken():
        sf, pf = get_active(SF), get_active(PF)
        cf, zf = get_active(CF), get_active(ZF)
        cc = Bits(phased('cc'), width=3)
        return bools.ifelse(
            cc[2],
            bools.ifelse(
                cc[1],
                bools.ifelse(cc[0], sf, ~sf),
                bools.ifelse(cc[0], pf, ~pf)),
            bools.ifelse(
                cc[1],
                bools.ifelse(cc[0], cf, ~cf),
                bools.ifelse(cc[0], zf, ~zf)))

    if (n.startswith('reg_pch') or n.startswith('reg_pcl') or
            n.startswith('reg_sph') or n.startswith('reg_spl')):
        SEQ_LENGTHS = {
            'nop': 1, "ex af, af'": 1, 'daa': 1, 'cpl': 1,
            'scf/ccf': 1, 'exx': 1, 'ex de, hl': 1,
            'ld sp, hl': 1, 'ei/di': 1,
            'ld {b, c, d, e, h, l, a}, {b, c, d, e, h, l, a}': 1,
            'ld {b, c, d, e, h, l, a}, (hl)': 1,
            'ld (hl), {b, c, d, e, h, l, a}': 1,
            'rlca/rrca/rla/rra': 1,
            'inc/dec {b, c, d, e, h, l, a}': 1,
            'inc/dec (hl)': 1, 'ld <rp>, nn': 3, 'add hl, <rp>': 1,
            'inc/dec <rp>': 1, 'pop <rp2>': 1, 'push <rp2>': 1,
            'ld (<rp>), a/ld a, (<rp>)': 1, 'ex (sp), hl': 1,
            'in a, (n)/out (n), a': 2,
            'ld {b, c, d, e, h, l, a}, n': 2, 'ld (hl), n': 2,
            'ld hl, (nn)/ld (nn), hl': 3,
            'ld a, (nn)/ld (nn), a': 3,
            'ld {i, r}, a/ld a, {i, r}': 2, 'rrd/rld': 2,
            'in r, (c)': 2, 'out (c), r': 2, 'adc/sbc hl, <rp>': 2,
            'ld <rp>, (nn)/ld (nn), <rp>': 4, 'neg/xneg': 2,
            'im/xim n': 2, 'xnop': 2, 'ldi/ldd': 2, 'cpi/cpd': 2,
            'ini/ind': 2, 'outi/outd': 2, 'bit (hl)': 2,
        }
        for m in ('add', 'adc', 'sub', 'sbc', 'and', 'xor', 'or',
                  'cp'):
            SEQ_LENGTHS[f'{m} {{b, c, d, e, h, l, a}}'] = 1
            SEQ_LENGTHS[f'{m} (hl)'] = 1
            SEQ_LENGTHS[f'{m} n'] = 2
        for m in ('rot', 'bit', 'res', 'set'):
            SEQ_LENGTHS[f'{m} {{b, c, d, e, h, l, a}}'] = 2
        for m in ('rot', 'res', 'set'):
            SEQ_LENGTHS[f'{m} (hl)'] = 2

        def get_pc_expected():
            # In the sampled convention, pc reads advanced by
            # one past the next instruction's address.
            length = SEQ_LENGTHS.get(instr)
            if length is not None:
                # The prefix, where a family carries one, adds
                # a byte; the displaced forms add the
                # displacement byte too.
                v = get_pc_plus(length + 1) + prefix_bit
                if instr in PREFIX_DISP:
                    v = v + prefix_bit
                return v
            if instr == 'halt':
                # PC freezes past the halt opcode -- and past
                # its prefix where it carries one.
                return get_pc_plus(1) + prefix_bit
            if instr == 'jp hl':
                return get_hl_eff() + 1
            if instr in ('jp nn', 'ret'):
                t = Bits.concat(Bits(phased('hi'), width=8),
                                Bits(phased('lo'), width=8))
                return t + 1
            if instr == 'reti/retn/xretn':
                t = Bits.concat(Bits(phased('r4'), width=8),
                                Bits(phased('r3'), width=8))
                return t + 1
            if instr == 'jp cc, nn':
                t = Bits.concat(Bits(phased('hi'), width=8),
                                Bits(phased('lo'), width=8))
                return Bits.ifelse(
                    get_cc_taken(), t,
                    (get_pc_plus(3) +
                     prefix_bit).truncated(16)) + 1
            if instr == 'call nn':
                t = Bits.concat(Bits(phased('r2'), width=8),
                                Bits(phased('r1'), width=8))
                return t + 1
            if instr == 'call cc, nn':
                t = Bits.concat(Bits(phased('r2'), width=8),
                                Bits(phased('r1'), width=8))
                return Bits.ifelse(
                    get_cc_taken(), t,
                    (get_pc_plus(3) +
                     prefix_bit).truncated(16)) + 1
            if instr == 'ret cc':
                t = Bits.concat(Bits(phased('r2'), width=8),
                                Bits(phased('r1'), width=8))
                return Bits.ifelse(
                    get_cc_taken(), t,
                    (get_pc_plus(1) +
                     prefix_bit).truncated(16)) + 1
            if instr == 'jr d':
                d = Bits(phased('r'), width=8)
                return (get_pc_plus(2) + prefix_bit +
                        d.sign_extended(16) + 1)
            if instr == 'jr cc, d':
                cc2 = Bits(phased('cc2'), width=2)
                cf, zf = get_active(CF), get_active(ZF)
                taken = bools.ifelse(
                    cc2[1],
                    bools.ifelse(cc2[0], cf, ~cf),
                    bools.ifelse(cc2[0], zf, ~zf))
                d = Bits(phased('r'), width=8)
                t = (get_pc_plus(2) + prefix_bit +
                     d.sign_extended(16)).truncated(16)
                return Bits.ifelse(
                    taken, t,
                    (get_pc_plus(2) +
                     prefix_bit).truncated(16)) + 1
            if instr == 'djnz d':
                d = Bits(phased('r'), width=8)
                t = (get_pc_plus(2) + prefix_bit +
                     d.sign_extended(16)).truncated(16)
                return Bits.ifelse(
                    get_b() != 1, t,
                    (get_pc_plus(2) +
                     prefix_bit).truncated(16)) + 1
            if instr == 'rst n':
                t = Bits(phased('y'), width=3) << 3
                return t.zero_extended(16) + 1
            if instr == 'ldir/lddr':
                return Bits.ifelse(get_bc() != 1, get_pc_plus(1),
                                   get_pc_plus(3)) + prefix_bit
            if instr == 'cpir/cpdr':
                rep = ((get_bc() != 1) &
                       (get_a() != Bits(phased('r'), width=8)))
                return Bits.ifelse(rep, get_pc_plus(1),
                                   get_pc_plus(3)) + prefix_bit
            if instr in ('inir/indr', 'otir/otdr'):
                return Bits.ifelse(get_b() != 1, get_pc_plus(1),
                                   get_pc_plus(3)) + prefix_bit
            assert 0, instr

        def get_sp_expected():
            sp = get_sp()
            if instr == 'ld sp, hl':
                return get_hl_eff()
            if instr == 'inc/dec <rp>':
                stepped = Bits.ifelse(phased_bool('is_inc'),
                                      sp + 1, sp - 1)
                return Bits.ifelse(
                    Bits(phased('rp'), width=2) == 3,
                    stepped.truncated(16), sp)
            if instr == 'ld <rp>, nn':
                v = Bits.concat(Bits(phased('r2'), width=8),
                                Bits(phased('r1'), width=8))
                return Bits.ifelse(
                    Bits(phased('rp'), width=2) == 3, v, sp)
            if instr == 'ld <rp>, (nn)/ld (nn), <rp>':
                v = Bits.concat(Bits(phased('rw6'), width=8),
                                Bits(phased('rw5'), width=8))
                is_load = bools.get(phased('q') + '_b0')
                sel = (Bits(phased('p'), width=2) == 3) & is_load
                return Bits.ifelse(sel, v, sp)
            if instr in ('push <rp2>', 'call nn', 'rst n'):
                return (sp - 2).truncated(16)
            if instr in ('pop <rp2>', 'ret', 'reti/retn/xretn'):
                return (sp + 2).truncated(16)
            if instr == 'call cc, nn':
                return Bits.ifelse(get_cc_taken(),
                                   (sp - 2).truncated(16), sp)
            if instr == 'ret cc':
                return Bits.ifelse(get_cc_taken(),
                                   (sp + 2).truncated(16), sp)
            return sp

        i = int(n[-1])
        if n[4:6] == 'pc':
            v = get_pc_expected()
        else:
            v = get_sp_expected()
        if n[6] == 'h':
            i += 8
        return check(v[i])

    if n.startswith('reg_i') and n[4:-1] == 'i':
        i = int(n[-1])
        v = bits('reg_i')
        if instr == 'ld {i, r}, a/ld a, {i, r}':
            sel = (phased_bool('is_store') &
                   phased_bool('is_i_reg'))
            v = Bits.ifelse(sel, get_a(), v)
        return check(v[i])

    if n.startswith('reg_r'):
        # The refresh register advances once per fetch cycle;
        # the sampling adds the settling nop's own. A
        # conditional fetch -- the prefix byte, or a byte
        # fetched only unprefixed -- counts under its
        # condition.
        i = int(n[-1])
        cur = bits('reg_r')
        lo = Bits(cur.bits[0:7])
        for cd, ct, cc in cycles:
            if not isinstance(cd, Bits) or ct < 4:
                # Byte-term cycles are reads; so is the 3-tick
                # conditional cycle re-feeding a CB family's
                # opcode byte in the prefixed order -- the die
                # does not refresh on it.
                continue
            if cc is None:
                lo = lo + 1
            elif cc == 'is_prefixed':
                lo = lo + prefix_bit
            elif cc == 'not_prefixed':
                lo = lo + Bits.ifelse(is_prefixed, 0, 1)
            else:
                assert 0, cc
        lo = lo.truncated(7)
        v = Bits(lo.bits + (cur[7],))
        if instr == 'ld {i, r}, a/ld a, {i, r}':
            a = get_a()
            wl = (Bits(a.bits[0:7]) + 1).truncated(7)
            w = Bits(wl.bits + (a[7],))
            sel = (phased_bool('is_store') &
                   ~phased_bool('is_i_reg'))
            v = Bits.ifelse(sel, w, v)
        return check(v[i])

    BANK_STORAGE = ('b', 'c', 'd', 'e', 'h', 'l',
                    'bb', 'cc', 'dd', 'ee', 'hh', 'll')
    if n.startswith('reg_') and n[4:-1] in BANK_STORAGE:
        i = int(n[-1])
        storage = n[4:-1]
        letter, alt = storage[0], len(storage) == 2
        exx = before[EXX_FF]
        active = ~exx if alt else exx

        R_CODES = {'b': 0, 'c': 1, 'd': 2, 'e': 3, 'h': 4, 'l': 5}

        def arch_after(x):
            # The architectural register's expected value; the
            # families not listed leave it alone.
            cur = {'b': get_b, 'c': get_c, 'd': get_d, 'e': get_e,
                   'h': get_h, 'l': get_l}[x]()
            code = R_CODES[x]
            hi = x in 'bdh'
            pair = {'b': get_bc, 'c': get_bc, 'd': get_de,
                    'e': get_de, 'h': get_hl, 'l': get_hl}[x]()
            rp_code = {'b': 0, 'c': 0, 'd': 1, 'e': 1,
                       'h': 2, 'l': 2}[x]

            def half(v16):
                return Bits(v16.bits[8:16] if hi else
                            v16.bits[0:8])

            if instr == ('ld {b, c, d, e, h, l, a}, '
                         '{b, c, d, e, h, l, a}'):
                return Bits.ifelse(Bits(opcode[3:6]) == code,
                                   get_r_eff(Bits(opcode[0:3])),
                                   cur)
            if instr in ('ld {b, c, d, e, h, l, a}, n',
                         'ld {b, c, d, e, h, l, a}, (hl)'):
                return Bits.ifelse(Bits(opcode[3:6]) == code,
                                   Bits(phased('r'), width=8), cur)
            if instr == 'inc/dec {b, c, d, e, h, l, a}':
                stepped = Bits.ifelse(opcode[0], cur - 1, cur + 1)
                return Bits.ifelse(Bits(opcode[3:6]) == code,
                                   stepped.truncated(8), cur)
            if instr == 'pop <rp2>':
                v = Bits(phased('hi' if hi else 'lo'), width=8)
                return Bits.ifelse(Bits(opcode.bits[4:6]) == rp_code,
                                   v, cur)
            if instr == 'ld <rp>, nn':
                v = Bits(phased('r2' if hi else 'r1'), width=8)
                return Bits.ifelse(
                    Bits(phased('rp'), width=2) == rp_code, v, cur)
            if instr == 'ld <rp>, (nn)/ld (nn), <rp>':
                v = Bits(phased('rw6' if hi else 'rw5'), width=8)
                is_load = bools.get(phased('q') + '_b0')
                sel = ((Bits(phased('p'), width=2) == rp_code) &
                       is_load)
                return Bits.ifelse(sel, v, cur)
            if instr == 'inc/dec <rp>':
                stepped = Bits.ifelse(phased_bool('is_inc'),
                                      pair + 1, pair - 1)
                return Bits.ifelse(
                    Bits(phased('rp'), width=2) == rp_code,
                    half(stepped.truncated(16)), cur)
            if instr == 'add hl, <rp>' and x in 'hl':
                s = (get_hl() + get_rp(opcode[4:6])).truncated(16)
                return half(s)
            if instr == 'adc/sbc hl, <rp>' and x in 'hl':
                hlv, rpv = get_hl(), get_rp(opcode[4:6])
                cfin = Bits.ifelse(get_active(CF), 1, 0)
                is_adc = bools.get(phased('q') + '_b0')
                s = Bits.ifelse(is_adc, hlv + rpv + cfin,
                                ((hlv - rpv) ^ 0x10000) - cfin)
                return half(s.truncated(16))
            if instr == 'ex (sp), hl' and x in 'hl':
                return Bits(phased('r2' if hi else 'r1'), width=8)
            if instr == 'ld hl, (nn)/ld (nn), hl' and x in 'hl':
                v = Bits(phased('rw4' if hi else 'rw3'), width=8)
                return Bits.ifelse(phased_bool('is_store'), cur, v)
            if instr == 'djnz d' and x == 'b':
                return (cur - 1).truncated(8)
            if instr == 'in r, (c)':
                return Bits.ifelse(
                    Bits(phased('y'), width=3) == code,
                    Bits(phased('io'), width=8), cur)
            if instr in ('rot {b, c, d, e, h, l, a}',
                         'bit {b, c, d, e, h, l, a}',
                         'res {b, c, d, e, h, l, a}',
                         'set {b, c, d, e, h, l, a}'):
                yb = Bits(phased('y'), width=3)
                regf = Bits(opcode[0:3])
                # Under the prefix the operand is the memory
                # byte, whatever the register field says; the
                # field only names the copy destination.
                v = Bits.ifelse(is_prefixed, get_at_hl(),
                                get_r(regf))
                is_right = yb[0]
                rot_cf = bools.ifelse(is_right, v[0], v[7])
                fill = bools.ifelse(
                    yb[2],
                    bools.ifelse(yb[1], ~is_right,
                                 is_right & v[7]),
                    bools.ifelse(yb[1], get_active(CF), rot_cf))
                rot_v = Bits.ifelse(is_right,
                                    Bits(v.bits[1:8] + (fill,)),
                                    Bits((fill,) + v.bits[0:7]))
                m = Bits.get_mask(yb)
                ROT, BIT, RES, SET = range(4)
                op = Bits(opcode[6:8])
                new = Bits.ifelse(
                    op == ROT, rot_v,
                    Bits.ifelse(op == RES, v & ~m, v | m))
                return Bits.ifelse((op == BIT) | (regf != code),
                                   cur, new)
            if instr in ('ldi/ldd', 'ldir/lddr'):
                if x in 'bc':
                    return half((get_bc() - 1).truncated(16))
                stepped = Bits.ifelse(phased_bool('is_ldd'),
                                      pair - 1, pair + 1)
                return half(stepped.truncated(16))
            if instr in ('cpi/cpd', 'cpir/cpdr'):
                if x in 'bc':
                    return half((get_bc() - 1).truncated(16))
                if x in 'hl':
                    stepped = Bits.ifelse(phased_bool('is_cpd'),
                                          pair - 1, pair + 1)
                    return half(stepped.truncated(16))
                return cur
            if instr in ('ini/ind', 'inir/indr',
                         'outi/outd', 'otir/otdr'):
                if x == 'b':
                    return (cur - 1).truncated(8)
                if x in 'hl':
                    dec = phased_bool(
                        'is_ind' if instr in ('ini/ind',
                                              'inir/indr')
                        else 'is_outd')
                    stepped = Bits.ifelse(dec, pair - 1, pair + 1)
                    return half(stepped.truncated(16))
                return cur
            return cur

        def arch_after_routed(x):
            v = arch_after(x)
            if x in 'hl' and instr in (PREFIX_HL_REDIRECT +
                                       PREFIX_HALVES_REDIRECT):
                # The prefixed variant writes IX/IY instead of
                # the h/l storage.
                v = Bits.ifelse(is_prefixed,
                                {'h': get_h, 'l': get_l}[x](), v)
            return v

        if letter in 'bc':
            new = arch_after_routed(letter)
        else:
            # Under the DE/HL exchange, the d/e storage holds
            # HL and the h/l storage holds DE.
            partner = {'d': 'h', 'h': 'd', 'e': 'l', 'l': 'e'}
            new = Bits.ifelse(before[EX_DE_FF],
                              arch_after_routed(partner[letter]),
                              arch_after_routed(letter))
        return check(Bits.ifelse(active, new,
                                 bits(f'reg_{storage}'))[i])

    IXY_STORAGE = ('ixh', 'ixl', 'iyh', 'iyl')
    if n.startswith('reg_i') and n[4:-1] in IXY_STORAGE:
        i = int(n[-1])
        st = n[4:-1]
        hi = st[2] == 'h'
        cur = bits(f'reg_{st}')
        if instr not in PREFIX_HL_REDIRECT + PREFIX_HALVES_REDIRECT:
            return check(cur[i])
        mine = phased_bool('is_iy')
        if st[1] == 'x':
            mine = ~mine
        sel = is_prefixed & mine

        def ixy_half(v16):
            return Bits(v16.bits[8:16] if hi else v16.bits[0:8])

        eff = get_ixy()
        new = None
        if instr == 'ld <rp>, nn':
            new = Bits(phased('r2' if hi else 'r1'), width=8)
            sel = sel & (Bits(phased('rp'), width=2) == 2)
        elif instr == 'inc/dec <rp>':
            stepped = Bits.ifelse(phased_bool('is_inc'),
                                  eff + 1, eff - 1)
            new = ixy_half(stepped.truncated(16))
            sel = sel & (Bits(phased('rp'), width=2) == 2)
        elif instr == 'add hl, <rp>':
            rp_val = get_rp(opcode[4:6])
            rp_val = Bits.ifelse(Bits(opcode[4:6]) == 2, eff,
                                 rp_val)
            new = ixy_half((eff + rp_val).truncated(16))
        elif instr == 'ex (sp), hl':
            new = Bits(phased('r2' if hi else 'r1'), width=8)
        elif instr == 'pop <rp2>':
            new = Bits(phased('hi' if hi else 'lo'), width=8)
            sel = sel & (Bits(opcode.bits[4:6]) == 2)
        elif instr == 'ld hl, (nn)/ld (nn), hl':
            new = Bits(phased('rw4' if hi else 'rw3'), width=8)
            sel = sel & ~phased_bool('is_store')
        elif instr == ('ld {b, c, d, e, h, l, a}, '
                       '{b, c, d, e, h, l, a}'):
            new = get_r_eff(Bits(opcode[0:3]))
            sel = sel & (Bits(opcode[3:6]) == (4 if hi else 5))
        elif instr == 'ld {b, c, d, e, h, l, a}, n':
            new = Bits(phased('r'), width=8)
            sel = sel & (Bits(opcode[3:6]) == (4 if hi else 5))
        elif instr == 'inc/dec {b, c, d, e, h, l, a}':
            half = ixy_half(eff)
            new = Bits.ifelse(opcode[0], half - 1,
                              half + 1).truncated(8)
            sel = sel & (Bits(opcode[3:6]) == (4 if hi else 5))
        if new is None:
            return check(cur[i])
        return check(Bits.ifelse(sel, new, cur)[i])

    if n in (IFF1, IFF2):
        if instr == 'ei/di':
            return check(phased_bool('is_ei'))

    if n in (HALTED_FF, HALTED_FF_B):
        # The halted flip-flop flips on halt -- HALTED_FF is
        # the side that goes low -- and nothing else touches
        # it.
        if instr == 'halt':
            return check(FALSE if n == HALTED_FF else TRUE)
        return check(before[n])

    if n in (IM_LATCH0, IM_LATCH1):
        # The mode latch takes the im instruction's mode field
        # verbatim -- the field's top bit unused, so the
        # duplicate encodings land alike; nothing else touches
        # the latch.
        if instr == 'im/xim n':
            y = Bits(phased('y'), width=3)
            return check(y[0 if n == IM_LATCH0 else 1])
        return check(before[n])

    if n == EX_AF_FF and instr == "ex af, af'":
        return check(~before[n])

    if n == EXX_FF and instr == 'exx':
        return check(~before[n])

    if n in (EX_DE_FF0, EX_DE_FF1, EX_DE_FF) and instr == 'ex de, hl':
        return check(~before[n])

    if n == EX_DE_FF and instr == 'exx':
        ex_de = before[EXX_FF].ifelse(before[EX_DE_FF0], before[EX_DE_FF1])
        return check(~ex_de)

    if instr in ('inc/dec {b, c, d, e, h, l, a}', 'inc/dec (hl)'):
        r = get_r_eff(opcode[3:6])
        r = Bits.ifelse(phased('is_inc'), r + 1, r - 1).truncated(8)
        if n in PFF:
            return check(bools.ifelse(phased_bool('is_inc'),
                         r == 0x80, r == 0x7f))
        if n in XFF + YFF:
            i = int(n[-1])
            return check(r[i])
        if n in HFF:
            r4 = r.truncated(4)
            return check(bools.ifelse(phased_bool('is_inc'),
                                      r4 == 0x0, r4 == 0xf))
        if n in ZFF:
            return check(r == 0x00)
        if n in SFF:
            return check(r[7])

    if instr == 'add hl, <rp>':
        hl = get_hl_eff()
        rp = get_rp(opcode[4:6])
        # Under the prefix the HL-role operand is IX/IY on both
        # sides: add ix, ix and add iy, iy included.
        rp = Bits.ifelse(is_prefixed & (Bits(opcode[4:6]) == 2),
                         get_ixy(), rp)
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
        # The prefix fetch, where there is one, adds a refresh
        # before the copy.
        rr = Bits((rr + 1 + prefix_bit).bits[:7] + (rr[7],))
        r = Bits.ifelse(phased('is_i_reg'), ri, rr)
        v = Bits.ifelse(phased('is_store'), get_a(), r)
        if n in NFF + HFF:
            return check(bools.ifelse(phased_bool('is_store'),
                                      before[n], FALSE))
        if n in PFF:
            return check(bools.ifelse(phased_bool('is_store'),
                                      before[n], before[IFF2]))
        if n in XFF + YFF:
            i = int(n[-1])
            return check(bools.ifelse(phased_bool('is_store'),
                                      before[n], r[i]))
        if n in ZFF:
            return check(bools.ifelse(phased_bool('is_store'),
                                      before[n], r == 0x00))
        if n in SFF:
            return check(bools.ifelse(phased_bool('is_store'),
                                      before[n], r[7]))
        if n.startswith('reg_a'):
            i = int(n[-1])
            return check(v[i])

    if instr == 'pop <rp2>':
        RP2_AF = 3
        p = Bits(opcode.bits[4:6])
        if n.startswith('reg_f'):
            i = int(n[-1])
            return check(bools.ifelse(p == RP2_AF,
                                      bools.get(f'lo_p{phase}_b{i}'),
                                      before[n]))
        if n.startswith('reg_a'):
            i = int(n[-1])
            return check(bools.ifelse(p == RP2_AF,
                                      bools.get(f'hi_p{phase}_b{i}'),
                                      before[n]))

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
            return check(bools.ifelse(nf, hf & (a4 <= 0x5), a4 >= 0xa))
        if n in ZFF:
            return check(r == 0x00)
        if n in SFF:
            return check(r[7])
        if n.startswith('reg_a'):
            i = int(n[-1])
            return check(r[i])

    alu_ops = 'add', 'adc', 'sub', 'sbc', 'and', 'xor', 'or', 'cp'
    if instr == 'neg/xneg' or any(
            instr in (f'{m} {{b, c, d, e, h, l, a}}', f'{m} (hl)', f'{m} n')
            for m in alu_ops):
        ADD, ADC, SUB, SBC, AND, XOR, OR, CP = range(8)

        if instr == 'neg/xneg':
            a = Bits(0, width=8)
            s = get_a()
            op = Bits(SUB)
        else:
            a = get_a()
            s = get_r_eff(opcode[0:3])
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
            return check(bools.ifelse(is_bitwise, r.parity(), overflow))
        if n in XFF + YFF:
            i = int(n[-1])
            return check(bools.ifelse(op == CP, s[i], r[i]))
        if n in HFF:
            return check(bools.ifelse(is_bitwise, op == AND,
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

    # out (c), r changes no tested node except WZ, covered above,
    # so the default unchanged check covers it.
    if instr == 'in r, (c)':
        y = Bits(phased('y'), width=3)
        io = Bits(phased('io'), width=8)
        a = Bits.ifelse(y != A, get_a(), io)
        if n in NFF + HFF:
            return check(FALSE)
        if n in PFF:
            return check(io.parity())
        if n in XFF + YFF + SFF:
            i = int(n[-1])
            return check(io[i])
        if n in ZFF:
            return check(io == 0)
        if n.startswith('reg_a'):
            i = int(n[-1])
            return check(a[i])

    if instr in ('ldi/ldd', 'ldir/lddr'):
        # One iteration's effect: a repeating ldir/lddr re-runs
        # itself through PC, so the per-instruction semantics
        # are ldi/ldd's.
        if n in NFF + HFF:
            return check(FALSE)
        if n in PFF:
            return check((get_bc() - 1).truncated(16) != 0)
        if n in XFF + YFF:
            # XF takes bit 3 and YF takes bit 1 -- not 5 -- of
            # the accumulator plus the transferred byte.
            v = get_a() + Bits(phased('r'), width=8)
            x = v[3] if n in XFF else v[1]
            if instr == 'ldir/lddr':
                # A repeating iteration takes the two flags from
                # the address it re-executes from instead: XF
                # from bit 11 and YF from bit 13 of PC.
                pc = (get_pc_plus(0) +
                      prefix_bit).truncated(16)
                x = bools.ifelse(get_bc() != 1,
                                 pc[11] if n in XFF else pc[13], x)
            return check(x)

    if instr in ('cpi/cpd', 'cpir/cpdr'):
        # One iteration's effect: a repeating cpir/cpdr re-runs
        # itself through PC, so the per-instruction semantics
        # are cpi/cpd's.
        a, s = get_a(), Bits(phased('r'), width=8)
        d = a - s
        if n in NFF:
            return check(TRUE)
        if n in HFF:
            return check(d[4] ^ a[4] ^ s[4])
        if n in PFF:
            return check((get_bc() - 1).truncated(16) != 0)
        if n in ZFF:
            return check(d.truncated(8) == 0x00)
        if n in SFF:
            return check(d[7])
        if n in XFF + YFF:
            # XF takes bit 3 and YF takes bit 1 -- not 5 -- of
            # the comparison less its halfborrow.
            t = d.truncated(8) - Bits.ifelse(d[4] ^ a[4] ^ s[4], 1, 0)
            x = t[3] if n in XFF else t[1]
            if instr == 'cpir/cpdr':
                # A repeating iteration takes the two flags from
                # the address it re-executes from instead: XF
                # from bit 11 and YF from bit 13 of PC.
                pc = (get_pc_plus(0) +
                      prefix_bit).truncated(16)
                x = bools.ifelse((get_bc() != 1) & (a != s),
                                 pc[11] if n in XFF else pc[13], x)
            return check(x)

    if instr in ('ini/ind', 'inir/indr', 'outi/outd', 'otir/otdr'):
        # One iteration's effect: a repeating variant re-runs
        # itself through PC, so the per-instruction semantics
        # are the non-repeating form's, adjusted where noted.
        is_in = instr in ('ini/ind', 'inir/indr')
        repeats = instr in ('inir/indr', 'otir/otdr')
        is_dec = phased_bool('is_ind' if is_in else 'is_outd')
        data = Bits(phased('io' if is_in else 'r'), width=8)
        b1 = (get_b() - 1).truncated(8)
        # The in forms add the transferred byte to C stepped in
        # the direction the address goes; the out forms add it
        # to the updated L.
        s = get_c() if is_in else get_l()
        k = data + Bits.ifelse(is_dec, (s - 1).truncated(8),
                               (s + 1).truncated(8))
        if n in NFF:
            return check(data[7])
        if n in CFF:
            return check(k[8])
        if n in HFF:
            x = k[8]
            if repeats:
                # A repeating iteration instead reports whether
                # the carry propagates through the low nibble of
                # the decremented B, stepped down or up by where
                # bit 7 of the data byte points.
                x = bools.ifelse(
                    get_b() != 1,
                    k[8] & (b1.truncated(4) ==
                            Bits.ifelse(data[7], 0x0, 0xf)),
                    x)
            return check(x)
        if n in PFF:
            x = (k.truncated(3).zero_extended(8) ^ b1).parity()
            if repeats:
                # A repeating iteration flips PF by the odd
                # parity of the low three bits of the
                # decremented B, on carry stepped down or up by
                # where bit 7 of the data byte points.
                t = Bits.ifelse(
                    k[8],
                    Bits.ifelse(data[7],
                                (b1 - 1).truncated(8),
                                (b1 + 1).truncated(8)),
                    b1)
                x = bools.ifelse(get_b() != 1,
                                 x ^ ~t.truncated(3).parity(), x)
            return check(x)
        if n in XFF + YFF:
            x = b1[3] if n in XFF else b1[5]
            if repeats:
                # A repeating iteration takes the two flags from
                # the address it re-executes from instead: XF
                # from bit 11 and YF from bit 13 of PC.
                pc = (get_pc_plus(0) +
                      prefix_bit).truncated(16)
                x = bools.ifelse(get_b() != 1,
                                 pc[11] if n in XFF else pc[13], x)
            return check(x)
        if n in ZFF:
            return check(b1 == 0x00)
        if n in SFF:
            return check(b1[7])

    if instr == 'adc/sbc hl, <rp>':
        hl, rp = get_hl(), get_rp(opcode[4:6])
        cf = Bits.ifelse(get_active(CF), 1, 0)
        is_adc = bools.get(phased('q') + '_b0')
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

    if instr in ('rot {b, c, d, e, h, l, a}', 'rot (hl)',
                 'bit {b, c, d, e, h, l, a}', 'bit (hl)',
                 'res {b, c, d, e, h, l, a}', 'res (hl)',
                 'set {b, c, d, e, h, l, a}', 'set (hl)'):
        y = Bits(phased('y'), width=3)

        reg = Bits(opcode[0:3])
        # Under the prefix the operand is the memory byte,
        # whatever the register field says.
        v = Bits.ifelse(is_prefixed, get_at_hl(), get_r(reg))

        # RLC, RRC, RL, RR, SLA, SRA, SLL, SRL
        is_right = y[0]
        rot_cf = bools.ifelse(is_right, v[0], v[7])

        is_nine_bit = y[1]
        is_shift = y[2]
        cf = get_active(CF)
        b = bools.ifelse(
            is_shift,
            bools.ifelse(is_nine_bit, ~is_right, is_right & v[7]),
            bools.ifelse(is_nine_bit, cf, rot_cf))

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
            return check(bools.ifelse(op == ROT, rot_cf, cf))
        if n in NFF:
            return check(bools.ifelse(op == ROT, FALSE,
                                      (op != BIT) & before[n]))
        if n in HFF:
            return check(bools.ifelse(op == ROT, FALSE,
                                      (op == BIT) | before[n]))
        if n in PFF:
            return check(bools.ifelse(
                op == BIT, bit_r == 0,
                bools.ifelse(op == ROT, rot_r.parity(), before[n])))
        if n in XFF + YFF:
            i = int(n[-1])
            # bit takes the two flags from the high byte of the
            # operand pointer: WZ for (hl), the computed ix+d
            # for the prefixed forms, the operand itself for a
            # plain register.
            ixd = (get_ixy() +
                   Bits(phased('d'),
                        width=8).sign_extended(16)).truncated(16)
            ptr_xy = bools.ifelse(
                is_prefixed, ixd[8 + i],
                bools.ifelse(reg == AT_HL,
                             before[f'reg_w{i}'], v[i]))
            return check(bools.ifelse(
                op == BIT, ptr_xy,
                bools.ifelse(op == ROT, rot_r[i], before[n])))
        if n in ZFF:
            return check(bools.ifelse(
                op == BIT, bit_r == 0,
                bools.ifelse(op == ROT, rot_r == 0, before[n])))
        if n in SFF:
            return check(bools.ifelse(
                op == BIT, bit_r[7],
                bools.ifelse(op == ROT, rot_r[7], before[n])))
        if n.startswith('reg_a'):
            i = int(n[-1])
            return check(bools.ifelse(
                (op == BIT) | (reg != A), before[n],
                bools.ifelse(op == ROT, rot_r[i],
                             bools.ifelse(op == RES, res_r[i], set_r[i]))))

    if n == '~halt':
        # The pin is active low; only halt asserts it.
        return check(FALSE if instr == 'halt' else before[n])

    # Test instruction latch nodes.
    if n.startswith('instr'):
        i = int(n[-1])
        return check(opcode[i])

    return check(before[n])


def get_effective_states(s, nodes=None, all_orders=False):
    # Additional ticks are necessary for new values
    # to reach their nodes.
    EXTRA_TICKS = 3

    s = State(s)

    s.set_db(0x00, all_orders)  # nop
    for t in range(EXTRA_TICKS):
        for ht in (0, 1):
            with s.status(f'extra tick {t}.{ht}'):
                s.half_tick(all_orders=all_orders)
    s.cache()

    return s.get_node_states(nodes)


def verbalise_state_value(v):
    v = PossibleValues.cast(v)
    if v.is_single:
        return [f'    {get_z3_simplified(v.single_value)}']
    lines = [f'    {len(v.entries)} possible values']
    for no, (val, os) in enumerate(v.entries):
        lines.append(f'      {no}) {get_z3_simplified(val)}')
        lines.append(f'         under {get_z3_simplified(os.e)}')
    return lines


def check_sampled_states(what, states, get_expected, under=None):
    # With an under condition given, expected values need only
    # hold where the condition does: the check is that no
    # assignment satisfies the condition yet tells the values
    # apart.
    def fail(n, lines):
        Status.clear()
        report = ['', f'FAILED: {what} {n}'] + lines
        print('\n'.join(report), file=sys.stderr, flush=True)
        raise TestFailure()

    def get_single(n, v):
        pv = PossibleValues.cast(v)
        if not pv.is_single:
            with Status.do('verbalise'):
                fail(n, ['  not a single value:'] +
                     verbalise_state_value(v))
        return pv.single_value

    for n in sorted(states):
        expected = get_single(n, get_expected(n))
        actual = get_single(n, states[n])
        if under is None:
            ok = bools.is_equiv(actual, expected)
        else:
            ok = bools.is_equiv(under & (actual ^ expected), FALSE)
        if ok:
            continue
        with Status.do('verbalise'):
            fail(n, ['  expected:'] + verbalise_state_value(expected) +
                 ['  actual:'] + verbalise_state_value(actual))


def sampled_bits(states, prefix, width=8):
    return Bits(states[f'{prefix}{i}'] for i in range(width))


def run_halted_m1(s, db, all_orders):
    s.set_db(db, all_orders)
    for t in range(4):
        for ht in (0, 1):
            with s.status(f'halted tick {t}.{ht}'):
                s.half_tick(all_orders=all_orders)
    s.cache()
    return s.get_node_states(SAMPLED_NODES)


def test_halted_dynamics(base_state, all_orders=False):
    # The halted steady state: an M1 cycle with a symbolic byte
    # on the bus preserves the whole sampled state, certifying
    # fetch-without-execute for every possible opcode at once;
    # only the refresh register's low seven bits advance, as
    # refresh continues while halted. The first cycle clears
    # the entry transient; the second is compared against the
    # first, so the equality is the induction step for halted
    # runs of any length.
    s = State(base_state)
    with Status.do('halted M1 1'):
        s1 = run_halted_m1(s, 'hf1', all_orders)
    with Status.do('halted M1 2'):
        s2 = run_halted_m1(s, 'hf2', all_orders)

    r_lo = (sampled_bits(s1, 'reg_r', width=7) + 1).truncated(7)

    def get_expected(n):
        if n == '~halt':
            return FALSE
        if n.startswith('reg_r') and n[-1] != '7':
            return r_lo[int(n[-1])]
        if n.startswith('instr'):
            # The instruction latch keeps loading each fetched
            # byte while halted: fetching proceeds; only
            # execution is suppressed.
            return Bits('hf2', width=8)[int(n[-1])]
        return s1[n]

    check_sampled_states('halt dynamics', s2, get_expected)


_BUS_AB = tuple(f'ab{i}' for i in range(16))
_BUS_CTL = '~m1', '~mreq', '~rd', '~wr', '~iorq', '~rfsh'


def run_bus_checked_ticks(s, what, rows, all_orders, under=None):
    # Executes one half-tick per row, checking the address bus
    # and the control pins against it: the acceptance
    # sequences' measured timelines, asserted where the
    # families' spec machinery does not reach. Sampling adds
    # no steps, so the settlings replay from the cache.
    for no, (a, *pins) in enumerate(rows):
        with s.status(f'{what} {no // 2}.{no % 2}'):
            s.half_tick(all_orders=all_orders)
        s.cache()
        expected = {}
        for j, p in enumerate(_BUS_AB):
            expected[p] = a[j]
        for p, v in zip(_BUS_CTL, pins):
            expected[p] = TRUE if v else FALSE
        states = s.get_node_states(_BUS_AB + _BUS_CTL)
        check_sampled_states(f'{what} {no // 2}.{no % 2}',
                             states, lambda n: expected[n],
                             under=under)


def bus_ack_fetch_rows(addr, refresh, nxt):
    # The ~nmi acknowledge: a five-tick fetch, discarded.
    return ((addr, 0, 0, 0, 1, 1, 1),) * 3 + (
        (refresh, 1, 1, 1, 1, 1, 0),
        (refresh, 1, 0, 1, 1, 1, 0),
        (refresh, 1, 0, 1, 1, 1, 0),
        (refresh, 1, 1, 1, 1, 1, 0),
        (refresh, 1, 1, 1, 1, 1, 1),
        (refresh, 1, 1, 1, 1, 1, 1),
        (nxt, 1, 1, 1, 1, 1, 1))


def bus_int_ack_rows(addr, refresh, nxt):
    # The maskable acknowledge: ~m1 low with ~mreq silent and
    # the would-be fetch address on the bus, ~iorq from two
    # ticks in; then the refresh phase as in any fetch, over
    # seven ticks in all.
    return ((addr, 0, 1, 1, 1, 1, 1),) * 4 + (
        (addr, 0, 1, 1, 1, 0, 1),) * 3 + (
        (refresh, 1, 1, 1, 1, 1, 0),
        (refresh, 1, 0, 1, 1, 1, 0),
        (refresh, 1, 0, 1, 1, 1, 0),
        (refresh, 1, 1, 1, 1, 1, 0),
        (refresh, 1, 1, 1, 1, 1, 1),
        (refresh, 1, 1, 1, 1, 1, 1),
        (nxt, 1, 1, 1, 1, 1, 1))


def bus_write_rows(addr, nxt, nxt_m1):
    return ((addr, 1, 0, 1, 1, 1, 1), (addr, 1, 0, 1, 1, 1, 1),
            (addr, 1, 0, 1, 0, 1, 1), (addr, 1, 0, 1, 0, 1, 1),
            (addr, 1, 1, 1, 1, 1, 1),
            (nxt, nxt_m1, 1, 1, 1, 1, 1))


def bus_read_rows(addr, nxt, nxt_m1):
    return ((addr, 1, 0, 0, 1, 1, 1),) * 4 + (
        (addr, 1, 1, 1, 1, 1, 1),
        (nxt, nxt_m1, 1, 1, 1, 1, 1))


def get_ack_bus_addrs(s2):
    # The addresses an acceptance from the halted state drives:
    # the halted fetch address (the raw pc holds the next fetch
    # address itself), the acknowledge cycle's refresh address
    # -- the sampled R as it stands: a fetch drives its refresh
    # before its own increment lands, the same overlap
    # convention as pc -- and the push addresses.
    fetch = Bits.concat(sampled_bits(s2, 'reg_pch'),
                        sampled_bits(s2, 'reg_pcl'))
    refresh = Bits.concat(sampled_bits(s2, 'reg_i'),
                          sampled_bits(s2, 'reg_r'))
    sp = Bits.concat(sampled_bits(s2, 'reg_sph'),
                     sampled_bits(s2, 'reg_spl'))
    return (fetch, refresh, (sp - 1).truncated(16),
            (sp - 2).truncated(16))


def test_nmi_from_halt(base_state, all_orders=False):
    # ~nmi ends the halted state. The pin first goes definitely
    # high, then low at the start of a halted M1, well before
    # the end-of-cycle sampling; the acceptance follows: a
    # five-tick fetch cycle whose byte, fed symbolic, must be
    # ignored, then two three-tick pushes of PC. The state is
    # then sampled through the usual nop settling.
    s = State(base_state)
    s.set_pin_and_update('~nmi', TRUE, all_orders)
    with Status.do('halted M1 before ~nmi'):
        run_halted_m1(s, 'nh1', all_orders)
    s.set_pin_and_update('~nmi', FALSE, all_orders)
    with Status.do('halted M1 with ~nmi'):
        s2 = run_halted_m1(s, 'nh2', all_orders)
    s.set_pin_and_update('~nmi', TRUE, all_orders)
    s.set_db('nf', all_orders)
    fetch, refresh, sp1, sp2a = get_ack_bus_addrs(s2)
    run_bus_checked_ticks(
        s, 'nmi ack', bus_ack_fetch_rows(fetch, refresh, sp1),
        all_orders)
    run_bus_checked_ticks(
        s, 'nmi push 1', bus_write_rows(sp1, sp2a, 1),
        all_orders)
    run_bus_checked_ticks(
        s, 'nmi push 2',
        bus_write_rows(sp2a, Bits.cast(0x0066, width=16), 0),
        all_orders)
    s.cache()
    after = get_effective_states(s, SAMPLED_NODES,
                                 all_orders=all_orders)

    sp = Bits.concat(sampled_bits(s2, 'reg_sph'),
                     sampled_bits(s2, 'reg_spl'))
    sp = (sp - 2).truncated(16)
    # The acceptance fetch refreshes too, and the sampling
    # nop's increment lands within the sampled ticks: two
    # increments past the halted state.
    r_lo = (sampled_bits(s2, 'reg_r', width=7) + 2).truncated(7)
    pc = Bits.cast(0x0067, width=16)
    wz = Bits.cast(0x0066, width=16)

    def get_expected(n):
        if n == '~halt':
            # The halted state ends.
            return TRUE
        if n in (HALTED_FF, HALTED_FF_B):
            # Leaving the halted state flips the pair back.
            return TRUE if n == HALTED_FF else FALSE
        if n == IFF1:
            # Accepting ~nmi disables maskable interrupts.
            return FALSE
        if n.startswith('reg_pch'):
            # The nop settling fetches from 0x0066, and a
            # sampled pc reads advanced by one.
            return pc[int(n[-1]) + 8]
        if n.startswith('reg_pcl'):
            return pc[int(n[-1])]
        if n.startswith('reg_sph'):
            return sp[int(n[-1]) + 8]
        if n.startswith('reg_spl'):
            return sp[int(n[-1])]
        if n.startswith('reg_w'):
            return wz[int(n[-1]) + 8]
        if n.startswith('reg_z'):
            return wz[int(n[-1])]
        if n.startswith('reg_r') and n[-1] != '7':
            return r_lo[int(n[-1])]
        if n.startswith('instr'):
            # The acceptance fetch loads its byte like any
            # other fetch and it rides through the acceptance
            # unexecuted; by this sample the settling nop has
            # replaced it in the latch.
            return FALSE
        return s2[n]

    check_sampled_states('nmi from halt', after, get_expected)


def test_masked_int_in_halt(base_state, all_orders=False):
    # An active ~int must not end the halted state while
    # interrupts are disabled. IFF1 in the halted state is the
    # symbolised interrupt-enable choice, so the settled result
    # covers both branches at once: where IFF1 is clear the
    # steady state must persist exactly as without ~int,
    # byte-independence included -- the checks are conditioned
    # on that; where IFF1 is set the state proceeds into the
    # acceptance, which its own test will cover.
    s = State(base_state)
    s.set_pin_and_update('~int', TRUE, all_orders)
    with Status.do('halted M1 before ~int'):
        s1 = run_halted_m1(s, 'mi1', all_orders)
    s.set_pin_and_update('~int', FALSE, all_orders)
    with Status.do('halted M1 with ~int 1'):
        s2 = run_halted_m1(s, 'mi2', all_orders)
    with Status.do('halted M1 with ~int 2'):
        s3 = run_halted_m1(s, 'mi3', all_orders)

    r_lo = (sampled_bits(s2, 'reg_r', width=7) + 1).truncated(7)
    # The enable state as of before ~int went active: at the
    # sample right after, the accepting branch has already
    # cleared IFF1.
    masked = ~PossibleValues.cast(s1[IFF1]).single_value

    def get_expected(n):
        if n == '~halt':
            return FALSE
        if n.startswith('reg_r') and n[-1] != '7':
            return r_lo[int(n[-1])]
        if n.startswith('instr'):
            return Bits('mi3', width=8)[int(n[-1])]
        return s2[n]

    check_sampled_states('masked ~int in halt', s3, get_expected,
                         under=masked)


def probe_acceptance_latch(all_orders=False):
    # Reports the instruction latch through the ~nmi acceptance
    # fetch, half-tick by half-tick: whether the symbolic bus
    # byte ever reaches the latch before the zero that the
    # acceptance leaves there -- the write being gated and the
    # register cleared are different mechanisms with the same
    # final state, and the sampling times the zeroing.
    s = build_symbolised_state()
    s = process_instr(('halt',), s, all_orders=all_orders)

    s = State(s)

    instr_nodes = tuple(f'instr{i}' for i in range(8))

    def sample_ticks(what, ticks):
        for t in range(ticks):
            for ht in (0, 1):
                with s.status(f'{what} tick {t}.{ht}'):
                    s.half_tick(all_orders=all_orders)
                s.cache()
                latch = s.get_node_states(instr_nodes)
                with Status.do('verbalise'):
                    bits = ', '.join(
                        '; '.join(verbalise_state_value(latch[n]))
                        .strip() for n in instr_nodes)
                Status.print(f'latch after {what} {t}.{ht}: ({bits})')

    # The first halted M1 right after the halt instruction, on
    # its own branch: the halt entry test sees the halt opcode
    # still latched three ticks in, so this M1's load timing
    # is suspect of differing from the steady state's.
    steady = s
    s = State(steady)
    s.set_db('nh0', all_orders)
    sample_ticks('first halted', 4)
    s = steady

    s.set_pin_and_update('~nmi', TRUE, all_orders)
    with Status.do('halted M1 before ~nmi'):
        run_halted_m1(s, 'nh1', all_orders)
    s.set_pin_and_update('~nmi', FALSE, all_orders)
    s.set_db('nh2', all_orders)
    sample_ticks('halted', 4)
    s.set_pin_and_update('~nmi', TRUE, all_orders)
    s.set_db('nf', all_orders)
    sample_ticks('acceptance', 5)
    sample_ticks('push', 3 + 3)
    s.set_db(0x00, all_orders)
    sample_ticks('nop', 3)


def mentions_terms(v, prefix):
    seen = set()
    stack = [v]
    while stack:
        x = stack.pop()
        kind = x.kind
        if kind in ('false', 'true'):
            continue
        if x.id in seen:
            continue
        seen.add(x.id)
        if kind == 'term':
            if str(x.term).startswith(prefix):
                return True
            continue
        if kind == 'not':
            stack.append(~x)
        else:
            stack.extend(x.args)
    return False


def test_wait_states(base_state, all_orders=False):
    # ~wait stretches a machine cycle's active phase without
    # touching the architecture. A fetch with two wait ticks
    # inserted holds its address and strobes through the
    # inserted ticks, runs its refresh phase after them -- the
    # refresh address pre-increment, as always -- and ends in
    # exactly the state of the plain fetch: the complete
    # sampled state, R included, compared node for node.
    s0 = State(base_state)
    s0.set_db(0x00, all_orders)
    for t in range(4):
        for ht in (0, 1):
            with s0.status(f'plain fetch {t}.{ht}'):
                s0.half_tick(all_orders=all_orders)
    s0.cache()
    plain = get_effective_states(s0, SAMPLED_NODES,
                                 all_orders=all_orders)

    st = base_state.get_node_states(SAMPLED_NODES)
    pcv = Bits.concat(sampled_bits(st, 'reg_pch'),
                      sampled_bits(st, 'reg_pcl'))
    refresh = Bits.concat(sampled_bits(st, 'reg_i'),
                          sampled_bits(st, 'reg_r'))
    rows = ((pcv, 0, 0, 0, 1, 1, 1),) * 7 + (
        (refresh, 1, 1, 1, 1, 1, 0),
        (refresh, 1, 0, 1, 1, 1, 0),
        (refresh, 1, 0, 1, 1, 1, 0),
        (refresh, 1, 1, 1, 1, 1, 0),
        ((pcv + 1).truncated(16), 0, 1, 1, 1, 1, 1))

    s = State(base_state)
    s.set_db(0x00, all_orders)
    for no, (a, *pins) in enumerate(rows):
        t, ht = no // 2, no % 2
        if (t, ht) == (1, 0):
            s.set_pin_and_update('~wait', FALSE, all_orders)
        if (t, ht) == (3, 0):
            s.set_pin_and_update('~wait', TRUE, all_orders)
        with s.status(f'waited fetch {t}.{ht}'):
            s.half_tick(all_orders=all_orders)
        s.cache()
        expected = {}
        for j, p in enumerate(_BUS_AB):
            expected[p] = a[j]
        for p, v in zip(_BUS_CTL, pins):
            expected[p] = TRUE if v else FALSE
        states = s.get_node_states(_BUS_AB + _BUS_CTL)
        check_sampled_states(f'waited fetch bus {t}.{ht}',
                             states, lambda n: expected[n])
    after = get_effective_states(s, SAMPLED_NODES,
                                 all_orders=all_orders)
    check_sampled_states('wait states', after,
                         lambda n: plain[n])


def test_bus_grant(base_state, all_orders=False):
    # ~busrq: the fetch in flight completes, then the CPU
    # grants the bus -- ~busak asserts, the address bus and
    # the four transfer strobes genuinely float (their pull
    # terms are the expected values), while ~m1 and ~rfsh stay
    # driven high -- and no refresh runs through the grant.
    # Release costs one tick, and the granted run ends in
    # exactly the plain fetch's state, node for node.
    s0 = State(base_state)
    s0.set_db(0x00, all_orders)
    for t in range(4):
        for ht in (0, 1):
            with s0.status(f'plain fetch {t}.{ht}'):
                s0.half_tick(all_orders=all_orders)
    s0.cache()
    plain = get_effective_states(s0, SAMPLED_NODES,
                                 all_orders=all_orders)

    st = base_state.get_node_states(SAMPLED_NODES)
    pcv = Bits.concat(sampled_bits(st, 'reg_pch'),
                      sampled_bits(st, 'reg_pcl'))
    refresh = Bits.concat(sampled_bits(st, 'reg_i'),
                          sampled_bits(st, 'reg_r'))
    float_ab = Bits(bools.get(f'pull.{p}') for p in _BUS_AB)
    TRI = tuple(bools.get(f'pull.{p}')
                for p in ('~mreq', '~rd', '~wr', '~iorq'))

    def granted(busak):
        # ab and the transfer strobes floating; ~m1 and ~rfsh
        # driven high; ~busak as given.
        return (float_ab, TRUE) + TRI + (TRUE, busak)

    def driven(a, m1, mreq, rd, wr, iorq, rfsh):
        return ((a, TRUE if m1 else FALSE)
                + tuple(TRUE if v else FALSE
                        for v in (mreq, rd, wr, iorq))
                + (TRUE if rfsh else FALSE, TRUE))

    rows = (
        driven(pcv, 0, 0, 0, 1, 1, 1),) * 3 + (
        driven(refresh, 1, 1, 1, 1, 1, 0),
        driven(refresh, 1, 0, 1, 1, 1, 0),
        driven(refresh, 1, 0, 1, 1, 1, 0),
        driven(refresh, 1, 1, 1, 1, 1, 0)) + (
        granted(FALSE),) * 7 + (
        granted(TRUE),
        driven((pcv + 1).truncated(16), 0, 1, 1, 1, 1, 1))

    CTL = ('~m1', '~mreq', '~rd', '~wr', '~iorq', '~rfsh',
           '~busak')
    s = State(base_state)
    s.set_pin_and_update('~busrq', FALSE, all_orders)
    s.set_db(0x00, all_orders)
    for no, (a, *pins) in enumerate(rows):
        t, ht = no // 2, no % 2
        if (t, ht) == (6, 0):
            s.set_pin_and_update('~busrq', TRUE, all_orders)
        with s.status(f'bus grant {t}.{ht}'):
            s.half_tick(all_orders=all_orders)
        s.cache()
        expected = {}
        for j, p in enumerate(_BUS_AB):
            expected[p] = a[j]
        for p, v in zip(CTL, pins):
            expected[p] = v
        states = s.get_node_states(_BUS_AB + CTL)
        check_sampled_states(f'bus grant {t}.{ht}', states,
                             lambda n: expected[n])
    after = get_effective_states(s, SAMPLED_NODES,
                                 all_orders=all_orders)
    check_sampled_states('bus grant', after,
                         lambda n: plain[n])


def test_special_reset(base_state, all_orders=False):
    # The 'special reset', mapped by --probe-special-reset:
    # ~reset is sampled at the odd half-ticks, and the sample
    # in the second half of a fetch's T1 is unlike the others
    # -- a pulse caught there and nowhere else forces PC to
    # zero and changes nothing else, the refresh counter
    # running on uninterrupted. A catch at any later sampling
    # instant -- even a lone half-tick pulse -- is a full
    # reset. The three regimes are certified over the complete
    # sampled state: a one-clock pulse from the fetch's start
    # (the classic special-reset recipe), a half-tick pulse at
    # T2 (full), and a half-tick pulse at a non-sampling
    # instant (ignored).
    s0 = State(base_state)
    for db in (0xfb, 0x00):  # ei; nop -- the shadow passes
        s0.set_db(db, all_orders)
        for t in range(4):
            for ht in (0, 1):
                with s0.status('special reset prep'):
                    s0.half_tick(all_orders=all_orders)
    s0.cache()
    st0 = s0.get_node_states(SAMPLED_NODES)
    pc0 = Bits.concat(sampled_bits(st0, 'reg_pch'),
                      sampled_bits(st0, 'reg_pcl'))
    r0 = sampled_bits(st0, 'reg_r')

    def run(offset, width):
        s = State(s0)
        s.set_db(0x00, all_orders)
        for k in range(offset):
            with s.status('pre'):
                s.half_tick(all_orders=all_orders)
        s.set_pin_and_update('~reset', FALSE, all_orders)
        for k in range(width):
            with s.status('pulse'):
                s.half_tick(all_orders=all_orders)
        s.set_pin_and_update('~reset', TRUE, all_orders)
        for k in range(24):
            with s.status('after'):
                s.half_tick(all_orders=all_orders)
        s.cache()
        return s.get_node_states(SAMPLED_NODES)

    def r_plus(k):
        lo = (Bits(r0.bits[0:7]) + k).truncated(7)
        return Bits(lo.bits + (r0[7],))

    def check(what, st, pc, i_cleared, r, iffs_cleared):
        def get_expected(n):
            if n.startswith('reg_pch'):
                return pc[int(n[-1]) + 8]
            if n.startswith('reg_pcl'):
                return pc[int(n[-1])]
            if n[4:-1] == 'i' and n.startswith('reg_i'):
                return FALSE if i_cleared else st0[n]
            if n.startswith('reg_r'):
                return r[int(n[-1])]
            if n in (IFF1, IFF2):
                return FALSE if iffs_cleared else TRUE
            if n in (IM_LATCH0, IM_LATCH1):
                return FALSE if iffs_cleared else st0[n]
            if n.startswith('instr'):
                # Nops throughout.
                return FALSE
            return st0[n]

        check_sampled_states(what, st, get_expected)

    with Status.do('special reset: one clock from T1'):
        check('special reset', run(0, 2),
              Bits.cast(2, width=16), False, r_plus(3), False)
    with Status.do('special reset: half a tick at T2'):
        check('half-tick full reset', run(3, 1),
              Bits.cast(3, width=16), True,
              Bits.cast(2, width=8), True)
    with Status.do('special reset: unsampled instant'):
        check('ignored reset pulse', run(0, 1),
              (pc0 + 4).truncated(16), False, r_plus(3),
              False)


def test_int_sampling_instant(base_state, all_orders=False):
    # ~int is sampled at exactly one instant per instruction:
    # half-tick 5 of a four-tick fetch. A pulse one half-tick
    # wide there is accepted, with the acknowledge following
    # at the instruction's end; an equal pulse two half-ticks
    # earlier is ignored entirely, the machine running on as
    # if nothing happened -- compared node for node against a
    # run with no pulse at all. Concretely prepared
    # (im 1; ei; nop), so both regimes certify without branch
    # conditions.
    s0 = State(base_state)
    run_prep_instr(s0, ((0xed, 4), (0x56, 4)), all_orders)
    run_prep_instr(s0, ((0xfb, 4),), all_orders)
    run_prep_instr(s0, ((0x00, 4),), all_orders)
    st0 = s0.get_node_states(SAMPLED_NODES)

    def run_pulse(offset):
        s = State(s0)
        s.set_db(0x00, all_orders)
        for k in range(8):
            if k == offset:
                s.set_pin_and_update('~int', FALSE, all_orders)
            if k == offset + 1:
                s.set_pin_and_update('~int', TRUE, all_orders)
            with s.status(f'int pulse half-tick {k}'):
                s.half_tick(all_orders=all_orders)
        s.set_pin_and_update('~int', TRUE, all_orders)
        return s

    with Status.do('accepted at half-tick 5'):
        s = run_pulse(5)
        s.set_db(0x00, all_orders)
        for t in range(7 + 3 + 3):
            for ht in (0, 1):
                with s.status(f'int instant ack {t}.{ht}'):
                    s.half_tick(all_orders=all_orders)
        s.cache()
        after = get_effective_states(s, SAMPLED_NODES,
                                     all_orders=all_orders)

    sp = Bits.concat(sampled_bits(st0, 'reg_sph'),
                     sampled_bits(st0, 'reg_spl'))
    sp = (sp - 2).truncated(16)
    # Three fetches past the baseline: the pulse's nop, the
    # acknowledge, and the sampling nop's increment landing
    # within the sampled ticks.
    r_lo = (sampled_bits(st0, 'reg_r', width=7) +
            3).truncated(7)
    pc = Bits.cast(0x0039, width=16)
    wz = Bits.cast(0x0038, width=16)

    def get_expected(n):
        if n in (IFF1, IFF2):
            return FALSE
        if n.startswith('reg_pch'):
            return pc[int(n[-1]) + 8]
        if n.startswith('reg_pcl'):
            return pc[int(n[-1])]
        if n.startswith('reg_sph'):
            return sp[int(n[-1]) + 8]
        if n.startswith('reg_spl'):
            return sp[int(n[-1])]
        if n.startswith('reg_w'):
            return wz[int(n[-1]) + 8]
        if n.startswith('reg_z'):
            return wz[int(n[-1])]
        if n.startswith('reg_r') and n[-1] != '7':
            return r_lo[int(n[-1])]
        if n.startswith('instr'):
            return FALSE
        return st0[n]

    check_sampled_states('~int at the sampling instant',
                         after, get_expected)

    with Status.do('ignored at half-tick 3'):
        s = run_pulse(3)
        s.set_db(0x00, all_orders)
        for t in range(4):
            for ht in (0, 1):
                with s.status(f'int instant follow {t}.{ht}'):
                    s.half_tick(all_orders=all_orders)
        s.cache()
        pulsed = get_effective_states(s, SAMPLED_NODES,
                                      all_orders=all_orders)
        sp0 = State(s0)
        sp0.set_db(0x00, all_orders)
        for t in range(8):
            for ht in (0, 1):
                with sp0.status(f'int instant plain {t}.{ht}'):
                    sp0.half_tick(all_orders=all_orders)
        sp0.cache()
        plain = get_effective_states(sp0, SAMPLED_NODES,
                                     all_orders=all_orders)
    check_sampled_states('~int off the sampling instant',
                         pulsed, lambda n: plain[n])


def test_prefix_chains(base_state, all_orders=False):
    # Chained prefixes act as one modifier: the last prefix
    # before the opcode wins. Certified on doubly prefixed
    # ld <rp>, nn encodings with rp = 2: dd dd loads IX, dd fd
    # loads IY, and the complete sampled state shows nothing
    # else changes beyond PC, R and the instruction latch.
    before = get_effective_states(base_state, SAMPLED_NODES)

    def run(chain, lo, hi):
        s = State(base_state)
        cycles = tuple((b, 4) for b in chain) + ((lo, 3),
                                                 (hi, 3))
        run_prep_instr(s, cycles, all_orders)
        return get_effective_states(s, SAMPLED_NODES,
                                    all_orders=all_orders)

    def check(what, after, reg, lo, hi):
        def get_expected(n):
            if (n.startswith('reg_i') and
                    n[4:-1] in (f'{reg}h', f'{reg}l')):
                v = Bits(hi if n[4:-1].endswith('h') else lo,
                         width=8)
                return v[int(n[-1])]
            if n.startswith('reg_pc'):
                pc = Bits.concat(
                    sampled_bits(before, 'reg_pch'),
                    sampled_bits(before, 'reg_pcl'))
                i = int(n[-1])
                if n.startswith('reg_pch'):
                    i += 8
                return (pc + 5).truncated(16)[i]
            if n.startswith('reg_r'):
                r = sampled_bits(before, 'reg_r')
                lo7 = (Bits(r.bits[0:7]) + 3).truncated(7)
                return Bits(lo7.bits + (r[7],))[int(n[-1])]
            if n.startswith('instr'):
                # The settling nop's byte, loaded by the
                # sample point.
                return FALSE
            return before[n]

        check_sampled_states(what, after, get_expected)

    with Status.do('dd dd chain'):
        check('dd dd chain',
              run((0xdd, 0xdd, 0x21), 'k1', 'k2'),
              'ix', 'k1', 'k2')
    with Status.do('dd fd chain'):
        check('dd fd chain',
              run((0xdd, 0xfd, 0x21), 'k3', 'k4'),
              'iy', 'k3', 'k4')


def probe_node_ids(all_orders=False):
    # Reports identification candidates for state nodes the
    # tests do not yet name: nodes that flip to a new constant
    # on entering the halted state and hold it across halted
    # cycles -- the halted flip-flop and its cone -- nodes
    # whose settled state depends on the interrupt mode choice
    # -- the IM latch -- and the values of the suggested IFF
    # nodes and their alternates in a state that tells the two
    # flip-flops apart.
    base = build_symbolised_state()

    def full_after_m1(s, db):
        s.set_db(db, all_orders)
        for t in range(4):
            for ht in (0, 1):
                with s.status(f'probe tick {t}.{ht}'):
                    s.half_tick(all_orders=all_orders)
        s.cache()
        return s.get_node_states()

    with Status.do('halted flip-flop'):
        pre = base.get_node_states()
        s = State(process_instr(('halt',), base,
                                all_orders=all_orders))
        h1 = full_after_m1(s, 'ni1')
        h2 = full_after_m1(s, 'ni2')
        for n in sorted(pre, key=str):
            v0, v1, v2 = pre[n], h1[n], h2[n]
            if any(isinstance(v, PossibleValues)
                   for v in (v0, v1, v2)):
                continue
            if (v0.is_const and v1.is_const and
                    v1.kind != v0.kind and v2.kind == v1.kind):
                Status.print(f'flips on halt entry and holds: '
                             f'{n} = {v1.kind}')

    with Status.do('im latch'):
        s = run_im_ei_nop_halt_prep(base, 1, all_orders)
        st = full_after_m1(s, 0x00)
        for n in sorted(st, key=str):
            v = st[n]
            if (not isinstance(v, PossibleValues) and
                    mentions_terms(v, 'y_p1')):
                Status.print(f'mode-dependent node: {n}')

    with Status.do('iff nodes'):
        s = State(process_instr(('halt',), base,
                                all_orders=all_orders))
        s.set_pin_and_update('~nmi', TRUE, all_orders)
        run_halted_m1(s, 'nh1', all_orders)
        s.set_pin_and_update('~nmi', FALSE, all_orders)
        run_halted_m1(s, 'nh2', all_orders)
        s.set_pin_and_update('~nmi', TRUE, all_orders)
        s.set_db('nf', all_orders)
        for t in range(5 + 3 + 3):
            for ht in (0, 1):
                with s.status(f'nmi acceptance tick {t}.{ht}'):
                    s.half_tick(all_orders=all_orders)
        s.cache()
        for n in (IFF1, IFF2, 'p1210', 'p1239', 'p231'):
            try:
                v = s.get_node_states((n,))[n]
            except KeyError:
                Status.print(f'{n}: no such node')
                continue
            with Status.do('verbalise'):
                r = '; '.join(verbalise_state_value(v)).strip()
            Status.print(f'{n} after ~nmi acceptance: {r}')

    with Status.do('prefix flip-flops'):
        # The DD/FD modifier pair: nodes that leave the nop
        # baseline after a prefix fetch and hold through a
        # second one -- the chains keep the modifier pending.
        # Nodes where DD and FD agree carry the
        # prefix-pending state; nodes where they part carry
        # the IX-or-IY choice.
        def held_after_two(db):
            s = State(base)
            a = full_after_m1(s, db)
            b = full_after_m1(s, db)
            return a, b

        n0, n0b = held_after_two(0x00)
        nd, ndb = held_after_two(0xdd)
        nf, nfb = held_after_two(0xfd)
        for n in sorted(n0, key=str):
            vs = n0[n], n0b[n], nd[n], ndb[n], nf[n], nfb[n]
            if any(isinstance(v, PossibleValues) for v in vs):
                continue
            if not all(v.is_const for v in vs):
                continue
            v0, v0b, vd, vdb, vf, vfb = (v.kind for v in vs)
            if v0 != v0b or vd != vdb or vf != vfb:
                # Not held: transient state, not the modifier.
                continue
            if vd == v0 and vf == v0:
                continue
            if vd == vf:
                tag = 'dd and fd alike -- prefix pending'
            elif vd != v0 and vf != v0:
                tag = 'dd and fd apart -- the IX/IY choice'
            else:
                tag = 'dd only' if vd != v0 else 'fd only'
            Status.print(f'{n}: nop {v0}, dd {vd}, fd {vf} '
                         f'({tag})')

        # While a prefix is pending, the last fetched byte is
        # the prefix itself, so the data path's bit-5 cone
        # mimics the IX-or-IY choice. Fetching the main opcode
        # separates them: the data paths then agree, and a node
        # still telling dd from fd carries the held choice.
        def after_prefix_and_op(db):
            s = State(base)
            full_after_m1(s, db)
            return full_after_m1(s, 0x21)

        m0 = after_prefix_and_op(0x00)
        md = after_prefix_and_op(0xdd)
        mf = after_prefix_and_op(0xfd)
        for n in sorted(m0, key=str):
            vs = m0[n], md[n], mf[n]
            if any(isinstance(v, PossibleValues) for v in vs):
                continue
            if not all(v.is_const for v in vs):
                continue
            v0, vd, vf = (v.kind for v in vs)
            if vd != vf:
                Status.print(f'{n}: nop {v0}, dd {vd}, fd {vf} '
                             f'(tells dd from fd past the '
                             f'opcode -- the IX/IY choice)')
            elif vd != v0:
                # The byte paths agree here, so this is held
                # modifier state, not data.
                Status.print(f'{n}: nop {v0}, prefixed {vd} '
                             f'(prefix held past the opcode)')


def probe_latch_cells():
    # A static netlist analysis: the storage structure behind
    # the identified state nodes. Almost every state cell on
    # this die is a dynamic latch -- the state node drives two
    # inverters in series and the refreshed value returns
    # through a pass transistor gated by clk -- so the state
    # is charge, refreshed every clock: the reason a Z80 has a
    # minimum clock frequency. The halted cell is the one
    # exception: a genuine static cross-coupled pair.
    import re as _re
    trans = []
    with open('transdefs.js') as f:
        for line in f:
            m = _re.match(r"^\['t(\d+)',(\d+),(\d+),(\d+),",
                          line)
            if m:
                trans.append(tuple(int(x)
                                   for x in m.groups()[1:]))
    GND, PWR, CLK = 1, 2, 3

    out = {}
    for g, c1, c2 in trans:
        for m in (c1, c2):
            if m not in (GND, PWR):
                out.setdefault(g, []).append((m, f'inv'))
        if c1 not in (GND, PWR) and c2 not in (GND, PWR):
            out.setdefault(c1, []).append((c2, f'pass[{g}]'))
            out.setdefault(c2, []).append((c1, f'pass[{g}]'))

    gnd_gates = {}
    for g, c1, c2 in trans:
        for a, b in ((c1, c2), (c2, c1)):
            if b == GND:
                gnd_gates.setdefault(a, set()).add(g)

    KEY = (('IFF1', 181), ('IFF2', 206),
           ('IM_LATCH0', 205), ('IM_LATCH1', 179),
           ('prefix held', 1242), ('IX/IY choice', 1616),
           ('halted', 1126))
    for name, n in KEY:
        for b in sorted(gnd_gates.get(n, ())):
            if n in gnd_gates.get(b, ()):
                Status.print(f'{name} ({n}): static '
                             f'cross-coupled pair with {b}')
        # Shortest refresh loops back to the state node.
        stack = [(n, [n], [])]
        seen = []
        while stack and len(seen) < 3:
            v, path, labels = stack.pop()
            if len(path) > 4:
                continue
            for m, lab in out.get(v, ()):
                if m == n and len(path) > 1:
                    seen.append(' -> '.join(
                        f'{p}[{la}]' for p, la in
                        zip(path[1:] + [n], labels + [lab])))
                elif m not in path:
                    stack.append((m, path + [m],
                                  labels + [lab]))
        for s in seen:
            Status.print(f'{name} ({n}): loop {n} -> {s}')


def get_structural_state_ids():
    # The netlist's state cells, by structure: nodes with a
    # clk-gated two-inverter refresh loop (dynamic latches) or
    # a static cross-coupled partner. This is the closed-world
    # candidate set -- everything on the die that can remember.
    import re as _re
    trans = []
    with open('transdefs.js') as f:
        for line in f:
            m = _re.match(r"^\['t(\d+)',(\d+),(\d+),(\d+),",
                          line)
            if m:
                trans.append(tuple(int(x)
                                   for x in m.groups()[1:]))
    GND, PWR, CLK = 1, 2, 3
    gnd_gates = {}
    passes = {}
    for g, c1, c2 in trans:
        for a, b in ((c1, c2), (c2, c1)):
            if b == GND:
                gnd_gates.setdefault(a, set()).add(g)
        if c1 not in (GND, PWR) and c2 not in (GND, PWR):
            passes.setdefault(c1, []).append((c2, g))
            passes.setdefault(c2, []).append((c1, g))
    state = set()
    for a, gs in gnd_gates.items():
        for b in gs:
            if a in gnd_gates.get(b, ()):
                state.add(a)
                state.add(b)
    for n, ps in passes.items():
        for y, g in ps:
            if g != CLK:
                continue
            for x in gnd_gates.get(y, ()):
                if n in gnd_gates.get(x, ()):
                    state.add(n)
    return state


def probe_closed_world(all_orders=False):
    # The closed-world triage: sample every structural state
    # node not already in the tested set across a few covered
    # instructions, and report which of them change between
    # the boundaries -- the input for expectations and
    # documented exclusions.
    base = build_symbolised_state()
    all_states = base.get_node_states()
    nums = get_structural_state_ids()
    ext = tuple(sorted(
        n for n in all_states
        if n not in SAMPLED_NODES and
        n.lstrip('~npmx').isdigit() and
        int(n.lstrip('~npmx')) in nums))
    Status.print(f'{len(ext)} untested state nodes')

    before = base.get_node_states(ext)
    for instr in ('nop', 'ld {b, c, d, e, h, l, a}, n',
                  'add hl, <rp>', 'ldi/ldd', 'ei/di',
                  'scf/ccf'):
        with Status.do(instr):
            s = process_instr((instr,), base,
                              all_orders=all_orders)
            after = s.get_node_states(ext)
        changed = []
        for n in ext:
            b, a = before[n], after[n]
            if isinstance(a, PossibleValues) or b is not a:
                if (isinstance(a, PossibleValues) or
                        not bools.is_equiv(b, a)):
                    changed.append(n)
        Status.print(f'{instr}: {len(changed)} changed: ' +
                     ' '.join(changed))


def probe_writeback(all_orders=False):
    # The writeback schedule: per tested node, the first
    # half-tick past an instruction's nominal end at which the
    # node holds its certified final value and keeps it -- the
    # per-node retirement timetable behind the sufficient
    # EXTRA_TICKS = 3 sampling margin.
    base = build_symbolised_state()
    for instr in ('ld {b, c, d, e, h, l, a}, n',
                  'inc/dec {b, c, d, e, h, l, a}',
                  'add hl, <rp>', 'pop <rp2>', 'ex de, hl',
                  'scf/ccf', 'ei/di'):
        with Status.do(instr):
            s = State(process_instr((instr,), base,
                                    all_orders=all_orders))
            final = get_effective_states(
                s, SAMPLED_NODES, all_orders=all_orders)
            samples = [s.get_node_states(SAMPLED_NODES)]
            s.set_db(0x00, all_orders)
            for k in range(6):
                with s.status(f'writeback half-tick {k}'):
                    s.half_tick(all_orders=all_orders)
                s.cache()
                samples.append(
                    s.get_node_states(SAMPLED_NODES))

            def matches(v, f):
                v = PossibleValues.cast(v)
                f = PossibleValues.cast(f)
                return (v.is_single and f.is_single and
                        bools.is_equiv(v.single_value,
                                       f.single_value))

            by_k = {}
            for n in sorted(SAMPLED_NODES):
                k = len(samples) - 1
                while (k > 0 and
                       matches(samples[k - 1][n], final[n])):
                    k -= 1
                by_k.setdefault(k, []).append(n)
        for k in sorted(by_k):
            names = by_k[k]
            if len(names) > 12:
                names = names[:12] + [f'... {len(by_k[k])} in all']
            Status.print(f'{instr}: retired by half-tick '
                         f'{k}: ' + ' '.join(names))


def probe_special_reset(all_orders=False):
    # The 'special reset' question: what a ~reset pulse
    # shorter than the documented three clocks does, swept
    # over pulse widths and phase offsets within a fetch. The
    # preparation sets the IFFs so their survival is
    # observable; I, R, the mode latch and the registers are
    # symbolic already.
    base = build_symbolised_state()
    s0 = State(base)
    for db in (0xfb, 0x00):  # ei; nop -- the shadow passes
        s0.set_db(db, all_orders)
        for t in range(4):
            for ht in (0, 1):
                with s0.status('special reset prep'):
                    s0.half_tick(all_orders=all_orders)
    s0.cache()
    st0 = s0.get_node_states(SAMPLED_NODES)
    pc0 = Bits.concat(sampled_bits(st0, 'reg_pch'),
                      sampled_bits(st0, 'reg_pcl'))
    r0lo = sampled_bits(st0, 'reg_r', width=7)

    def desc16(v, base16):
        for k in range(8):
            if all(bools.is_equiv(a, b) for a, b in zip(
                    v.bits, Bits.cast(k, width=16).bits)):
                return f'={k}'
        for k in range(8):
            if all(bools.is_equiv(a, b) for a, b in zip(
                    v.bits,
                    ((base16 + k).truncated(16)).bits)):
                return f'base+{k}'
        return '?'

    def desc_bits(v, kept):
        if all(bools.is_equiv(b, FALSE) for b in v.bits):
            return 'cleared'
        if all(bools.is_equiv(a, b)
               for a, b in zip(v.bits, kept.bits)):
            return 'kept'
        return '?'

    def desc_bool(v, kept):
        if bools.is_equiv(v, FALSE):
            return 'cleared'
        if bools.is_equiv(v, kept):
            return 'kept'
        return '?'

    for w in (1, 2, 3, 4, 6, 8, 12):
        for o in ((0, 1, 2, 3, 4, 5, 6, 7) if w <= 2 else
                  (0, 1, 2, 3)):
            with Status.do(f'pulse w={w} o={o}'):
                s = State(s0)
                s.set_db(0x00, all_orders)
                for k in range(o):
                    with s.status('pre'):
                        s.half_tick(all_orders=all_orders)
                s.set_pin_and_update('~reset', FALSE,
                                     all_orders)
                for k in range(w):
                    with s.status('pulse'):
                        s.half_tick(all_orders=all_orders)
                s.set_pin_and_update('~reset', TRUE,
                                     all_orders)
                for k in range(24):
                    with s.status('after'):
                        s.half_tick(all_orders=all_orders)
                s.cache()
                st = s.get_node_states(SAMPLED_NODES)
            pc = Bits.concat(sampled_bits(st, 'reg_pch'),
                             sampled_bits(st, 'reg_pcl'))
            rlo = sampled_bits(st, 'reg_r', width=7)
            rk = '?'
            for k in range(8):
                if all(bools.is_equiv(a, b) for a, b in zip(
                        rlo.bits,
                        Bits.cast(k, width=7).bits)):
                    rk = f'={k}'
                    break
                if all(bools.is_equiv(a, b) for a, b in zip(
                        rlo.bits,
                        ((r0lo + k).truncated(7)).bits)):
                    rk = f'base+{k}'
                    break

            def single(states, n):
                return PossibleValues.cast(
                    states[n]).single_value

            iv = desc_bits(sampled_bits(st, 'reg_i'),
                           sampled_bits(st0, 'reg_i'))
            f1 = desc_bool(single(st, IFF1), TRUE)
            f2 = desc_bool(single(st, IFF2), TRUE)
            imv = desc_bool(single(st, IM_LATCH0),
                            single(st0, IM_LATCH0))
            wzv = desc_bits(
                Bits.concat(sampled_bits(st, 'reg_w'),
                            sampled_bits(st, 'reg_z')),
                Bits.concat(sampled_bits(st0, 'reg_w'),
                            sampled_bits(st0, 'reg_z')))
            av = desc_bits(sampled_bits(st, 'reg_a'),
                           sampled_bits(st0, 'reg_a'))
            Status.print(f'w={w} o={o}: pc{desc16(pc, pc0)} '
                         f'i={iv} r{rk} iff1={f1} iff2={f2} '
                         f'im0={imv} wz={wzv} a={av}')


# The flag-consuming families checked against representative
# predecessor classes -- a full ALU flag writer, the partial
# writers, a non-writer. A passing pair proves the family's
# certified expectations hold in that context too; pairs
# graduate into the plain run's sequences once measured.
CONTEXT_SUSPECT_PAIRS = (
    ('or n', 'jr cc, d'),
    ('or n', 'jp cc, nn'),
    ('or n', 'ret cc'),
    ('or n', 'call cc, nn'),
    ('or n', 'adc n'),
    ('or n', 'sbc n'),
    ('or n', 'rlca/rrca/rla/rra'),
    ('or n', 'daa'),
    ('cpl', 'jr cc, d'),
    ('cpl', 'adc n'),
    ('cpl', 'daa'),
    ('cpl', 'rlca/rrca/rla/rra'),
    ('inc/dec {b, c, d, e, h, l, a}', 'jr cc, d'),
    ('inc/dec {b, c, d, e, h, l, a}', 'adc n'),
    ('inc/dec {b, c, d, e, h, l, a}', 'daa'),
    ('ld {b, c, d, e, h, l, a}, {b, c, d, e, h, l, a}',
     'jr cc, d'),
    ('ld {b, c, d, e, h, l, a}, {b, c, d, e, h, l, a}',
     'adc n'),
    ('ld {b, c, d, e, h, l, a}, {b, c, d, e, h, l, a}',
     'daa'),
)


def probe_context_suspects():
    # Runs the suspect pairs, reporting per pair whether the
    # certified expectations hold in the context.
    for pred, fam in CONTEXT_SUSPECT_PAIRS:
        with Status.do(f'{pred}; {fam}'):
            ok, message = test_instr_seq((pred, fam),
                                         all_orders=True)
        Status.print(f'{pred}; {fam}: ' +
                     ('PASS' if ok else 'DIFFERS'))
        if not ok:
            Status.print(message)


def probe_context_carriers():
    # The context-leak census over the cached boundary states:
    # nodes identical after every covered predecessor carry no
    # context into the next instruction; the rest are the
    # possible carriers. The second half measures each
    # family's retirement surface -- the nodes still changing
    # across its boundary beyond the settling nop's own
    # activity: the trajectory the next instruction can
    # observe even though the settled values are correct.
    base = build_symbolised_state()
    fams = [f for f in TestedInstrs.get_instrs()
            if f != 'halt']

    ref = None
    diff = set()
    finals = {}
    for fam in fams:
        st = process_instr((fam,), base, all_orders=True)
        finals[fam] = st
        states = st.get_node_states()
        if ref is None:
            ref = states
            continue
        for n, v in states.items():
            if n in diff:
                continue
            r = ref.get(n)
            if (r is None or isinstance(v, PossibleValues) or
                    isinstance(r, PossibleValues) or
                    (r is not v and not bools.is_equiv(r, v))):
                diff.add(n)
    Status.print(f'{len(ref)} nodes; {len(diff)} differ '
                 f'across contexts, {len(ref) - len(diff)} '
                 f'provably context-free')

    def inflight(state):
        raw = state.get_node_states()
        eff = get_effective_states(state)
        out = set()
        for n, v in raw.items():
            e = eff.get(n)
            if (e is None or isinstance(v, PossibleValues) or
                    isinstance(e, PossibleValues) or
                    (e is not v and not bools.is_equiv(v, e))):
                out.add(n)
        return out

    s = State(base)
    for _ in range(2):
        s.set_db(0x00, False)
        for t in range(4):
            for ht in (0, 1):
                s.half_tick()
    s.cache()
    nop_activity = inflight(s)
    Status.print(f'nop baseline activity: '
                 f'{len(nop_activity)} nodes')
    union = set()
    for fam in fams:
        extra = inflight(finals[fam]) - nop_activity
        union |= extra
        Status.print(f'{fam}: retirement surface '
                     f'{len(extra)}')
    Status.print(f'retirement surface union: {len(union)}')


def probe_scf_classes():
    # Classifies each covered scf/ccf predecessor by the
    # actual XF and YF outcome sets of the pair's cached
    # settling, tested for equivalence against the candidate
    # patterns; 'novel' means none fits and the raw set sizes
    # are printed instead.
    base = build_symbolised_state()
    for pred in TestedInstrs.get_instrs():
        if pred == 'halt':
            continue
        with Status.do(f'{pred}; scf/ccf'):
            try:
                # The predecessor executes as a preparation --
                # single-order, exactly as in the tested
                # pairs -- and only the scf/ccf settles under
                # all orders.
                s1 = process_instr((pred,), base)
                s2 = process_instr((pred, 'scf/ccf'), s1,
                                   all_orders=True)
            except Exception as e:
                Status.print(f'{pred}: skipped '
                             f'({type(e).__name__})')
                continue
            before = get_effective_states(s1, SAMPLED_NODES)
            after = get_effective_states(
                s2, SAMPLED_NODES, all_orders=True)
        p = bools.get('is_prefixed_p2')
        verdicts = []
        for i in (3, 5):
            ex = PossibleValues.cast(
                before[EX_AF_FF]).single_value
            a = bools.ifelse(
                ex,
                PossibleValues.cast(
                    before[f'reg_a{i}']).single_value,
                PossibleValues.cast(
                    before[f'reg_aa{i}']).single_value)
            f = bools.ifelse(
                ex,
                PossibleValues.cast(
                    before[f'reg_f{i}']).single_value,
                PossibleValues.cast(
                    before[f'reg_ff{i}']).single_value)
            cands = {
                'A': (a,),
                'four-set': (a, f, a | f, a & f),
                'A-unless-prefixed': (
                    a, bools.ifelse(p, f, a),
                    bools.ifelse(p, a | f, a),
                    bools.ifelse(p, a & f, a)),
                'F': (f,),
                'A|F': (a | f,),
            }
            # Mirror check(): the expected value lives in the
            # storage node only where its bank is active;
            # otherwise the node keeps its before value.
            bef_node = before[f'reg_f{i}']
            cands = {name: tuple(bools.ifelse(ex, c, bef_node)
                                 for c in cand)
                     for name, cand in cands.items()}
            actual = PossibleValues.cast(after[f'reg_f{i}'])
            if actual.is_single:
                vals = (actual.single_value,)
            else:
                vals = tuple(v for v, _ in actual.entries)

            def matches(cand):
                if len(cand) != len(vals):
                    return False
                left = list(cand)
                for v in vals:
                    for j, c in enumerate(left):
                        if bools.is_equiv(v, c):
                            del left[j]
                            break
                    else:
                        return False
                return True

            for name, cand in cands.items():
                if matches(cand):
                    verdicts.append(name)
                    break
            else:
                verdicts.append(f'novel({len(vals)})')
        Status.print(f'{pred}: xf {verdicts[0]}, '
                     f'yf {verdicts[1]}')


def probe_scf_contexts():
    # The scf/ccf context map: scf/ccf simulated on top of
    # every covered family, reusing the families' cached
    # settlings as the preceding context. A pass means the
    # context leaves the certified outcome set unchanged; a
    # failure prints the context's actual set -- the
    # measurement behind the previous-instruction dependence
    # of XF and YF (the 'Q register' of emulator models,
    # which has no latch on this die).
    for instr in TestedInstrs.get_instrs():
        if instr == 'halt':
            # No instruction follows a halt.
            continue
        with Status.do(f'{instr}; scf/ccf'):
            ok, message = test_instr_seq((instr, 'scf/ccf'),
                                         all_orders=True)
        Status.print(f'{instr}: ' +
                     ('set unchanged' if ok else 'DIFFERS'))
        if not ok:
            Status.print(message)


def probe_bus_templates(all_orders=False):
    # Reports the address bus and the control pins through
    # representative machine cycles, half-tick by half-tick,
    # naming each sampled address by the state expression it
    # equals: the per-cycle-kind templates that address-bus
    # certification builds on.
    base = build_symbolised_state()

    AB = tuple(f'ab{i}' for i in range(16))
    CTL = '~m1', '~mreq', '~rd', '~wr', '~iorq', '~rfsh'

    def single(v):
        return PossibleValues.cast(v).single_value

    def ir_plus(st, k):
        r = sampled_bits(st, 'reg_r')
        lo = (Bits(r.bits[0:7]) + k).truncated(7)
        return Bits.concat(sampled_bits(st, 'reg_i'),
                           Bits(lo.bits + (r[7],)))

    def get_candidates(st, eff):
        # PC, IR and SP are stable at the boundary and come
        # from the raw sample; the exchange-muxed registers
        # read correctly only in the effective state.
        pc = Bits.concat(sampled_bits(st, 'reg_pch'),
                         sampled_bits(st, 'reg_pcl'))
        sp = Bits.concat(sampled_bits(st, 'reg_sph'),
                         sampled_bits(st, 'reg_spl'))
        cands = {'float': Bits(bools.get(f'pull.{p}') for p in AB)}
        for k in range(-1, 6):
            cands[f'pc{k:+}' if k else 'pc'] = (
                (pc + (0x10000 + k)).truncated(16))
        for k in range(6):
            cands[f'ir+{k}' if k else 'ir'] = ir_plus(st, k)
        st = eff
        # The bus sees the effective registers, resolved through
        # the exchange flip-flops.

        def bank(r):
            return Bits.ifelse(single(st[EXX_FF]),
                               sampled_bits(st, f'reg_{r}'),
                               sampled_bits(st, f'reg_{r}{r}'))

        ex_de = single(st[EX_DE_FF])
        cands['hl'] = Bits.concat(
            Bits.ifelse(ex_de, bank('d'), bank('h')),
            Bits.ifelse(ex_de, bank('e'), bank('l')))
        cands['de'] = Bits.concat(
            Bits.ifelse(ex_de, bank('h'), bank('d')),
            Bits.ifelse(ex_de, bank('l'), bank('e')))
        for k in range(1, 3):
            cands[f'sp-{k}'] = (sp - k).truncated(16)
        cands['sp'] = sp
        return cands

    def match(bits, cands):
        names = [name for name, c in cands.items()
                 if all(bools.is_equiv(a, b)
                        for a, b in zip(bits, c))]
        if names:
            return '/'.join(names)
        with Status.do('verbalise'):
            return ('(' +
                    ', '.join(get_z3_simplified(b) for b in bits) +
                    ')')

    def render_pin(v):
        if bools.is_equiv(v, TRUE):
            return '1'
        if bools.is_equiv(v, FALSE):
            return '0'
        with Status.do('verbalise'):
            return get_z3_simplified(v)

    def drive(s, what, db, ticks, cands):
        s.set_db(db, all_orders)
        for t in range(ticks):
            for ht in (0, 1):
                with s.status(f'{what} tick {t}.{ht}'):
                    s.half_tick(all_orders=all_orders)
                s.cache()
                ab = s.get_node_states(AB)
                ab = tuple(single(ab[p]) for p in AB)
                ctl = s.get_node_states(CTL)
                pins = ' '.join(
                    f'{p}={render_pin(single(ctl[p]))}' for p in CTL)
                Status.print(f'{what} {t}.{ht}: '
                             f'ab={match(ab, cands)} {pins}')

    effective = get_effective_states(base, SAMPLED_NODES,
                                     all_orders=all_orders)
    cands = get_candidates(base.get_node_states(SAMPLED_NODES),
                           effective)

    s = State(base)
    drive(s, 'nop M1', 0x00, 4, cands)
    drive(s, 'next M1', 0x00, 2, cands)

    s = State(base)
    drive(s, 'ld a, (hl) M1', 0x7e, 4, cands)
    drive(s, 'ld a, (hl) read', 'rb', 3, cands)

    s = State(base)
    drive(s, 'ld (hl), a M1', 0x77, 4, cands)
    drive(s, 'ld (hl), a write', 'wb', 3, cands)

    s = State(base)
    io_cands = dict(cands)
    io_cands['a:n'] = Bits.concat(
        Bits.ifelse(PossibleValues.cast(effective[EX_AF_FF])
                    .single_value,
                    sampled_bits(effective, 'reg_a'),
                    sampled_bits(effective, 'reg_aa')),
        Bits('pn', width=8))
    drive(s, 'in a, (n) M1', 0xdb, 4, io_cands)
    drive(s, 'in a, (n) imm', 'pn', 3, io_cands)
    drive(s, 'in a, (n) io', 'pi', 4, io_cands)

    s = State(base)
    drive(s, 'push bc M1', 0xc5, 5, cands)
    drive(s, 'push bc write 1', 'wp1', 3, cands)
    drive(s, 'push bc write 2', 'wp2', 3, cands)

    # The halted fetches: which address they drive was an open
    # question of the halt coverage.
    s = State(process_instr(('halt',), base, all_orders=all_orders))
    drive(s, 'halted M1 1', 'hb1', 4, cands)
    drive(s, 'halted M1 2', 'hb2', 4, cands)

    # The remaining cycle kinds.
    s = State(base)
    drive(s, 'inc (hl) M1', 0x34, 4, cands)
    drive(s, 'inc (hl) r4', 'q1', 4, cands)
    drive(s, 'inc (hl) w3', 'q2', 3, cands)

    s = State(base)
    drive(s, 'out (n) M1', 0xd3, 4, io_cands)
    drive(s, 'out (n) imm', 'pn', 3, io_cands)
    drive(s, 'out (n) io', 'po', 4, io_cands)

    s = State(base)
    drive(s, 'add hl M1', 0x09, 4, cands)
    drive(s, 'add hl e4', 'q3', 4, cands)
    drive(s, 'add hl e3', 'q4', 3, cands)

    s = State(base)
    drive(s, 'ex (sp) M1', 0xe3, 4, cands)
    drive(s, 'ex (sp) r3', 'q5', 3, cands)
    drive(s, 'ex (sp) r4', 'q6', 4, cands)
    drive(s, 'ex (sp) w3', 'q7', 3, cands)
    drive(s, 'ex (sp) w5', 'q8', 5, cands)

    # The acceptance timelines: the ~nmi acknowledge fetch and
    # the im 2 acceptance with its vector-table reads -- the
    # table address, vector bit 0 included, is the open
    # question.
    s = State(process_instr(('halt',), base, all_orders=all_orders))
    s.set_pin_and_update('~nmi', TRUE, all_orders)
    drive(s, 'pre-nmi halted M1', 'ha1', 4, cands)
    s.set_pin_and_update('~nmi', FALSE, all_orders)
    drive(s, 'nmi-edge halted M1', 'ha2', 4, cands)
    s.set_pin_and_update('~nmi', TRUE, all_orders)
    drive(s, 'nmi ack M1', 'ha3', 5, cands)
    drive(s, 'nmi push 1', 'ha4', 3, cands)
    drive(s, 'nmi push 2', 'ha5', 3, cands)

    s = State(base)
    s.set_pin_and_update('~int', TRUE, all_orders)
    for byte, name in ((0xed, 'im 2 ED'), (0x5e, 'im 2 op'),
                       (0xfb, 'ei'), (0x00, 'shadow nop')):
        s.set_db(byte, all_orders)
        for t in range(4):
            for ht in (0, 1):
                with s.status(f'{name} tick {t}.{ht}'):
                    s.half_tick(all_orders=all_orders)
    s.set_pin_and_update('~int', FALSE, all_orders)
    io_cands2 = dict(cands)
    iv = Bits('pv', width=8)
    i_reg = sampled_bits(base.get_node_states(SAMPLED_NODES),
                         'reg_i')
    io_cands2['i:v'] = Bits.concat(i_reg, iv)
    io_cands2['i:v+1'] = (Bits.concat(i_reg, iv) + 1).truncated(16)
    drive(s, 'im2 last nop', 0x00, 4, io_cands2)
    s.set_pin_and_update('~int', TRUE, all_orders)
    drive(s, 'im2 ack M1', 'pv', 7, io_cands2)
    drive(s, 'im2 push 1', 'pw1', 3, io_cands2)
    drive(s, 'im2 push 2', 'pw2', 3, io_cands2)
    drive(s, 'im2 table lo', 'pt1', 3, io_cands2)
    drive(s, 'im2 table hi', 'pt2', 3, io_cands2)

    s = State(base)
    drive(s, 'rrd ED M1', 0xed, 4, cands)
    drive(s, 'rrd op M1', 0x67, 4, cands)
    drive(s, 'rrd read', 'qb', 3, cands)
    drive(s, 'rrd e4', 'qc', 4, cands)
    drive(s, 'rrd write', 'qd', 3, cands)

    s = State(base)
    drive(s, 'ldi ED M1', 0xed, 4, cands)
    drive(s, 'ldi op M1', 0xa0, 4, cands)
    drive(s, 'ldi read', 'q9', 3, cands)
    drive(s, 'ldi write', 'qa', 5, cands)


def run_prep_instr(s, cycles, all_orders):
    # Mirrors execute_instr()'s steps exactly. Preparations
    # drive raw byte expressions instead of executing the
    # instruction families: the families' cycle lists evolve --
    # the prefix work adds conditional cycles to them -- and a
    # preparation must keep its step list, and so its cached
    # settlings, stable.
    for d, ticks in cycles:
        s.set_db(d, all_orders)
        for t in range(ticks):
            for ht in (0, 1):
                with s.status(f'prep tick {t}.{ht}'):
                    s.half_tick(all_orders=all_orders)
        s.cache(intermediate=True)
    s.cache()


def run_im_ei_nop_halt_prep(symbolised_state, upto, all_orders):
    # The acceptance tests' preparation: im/xim with the mode
    # symbolic, ei/di with the choice symbolic, a shadow nop
    # and halt -- as many of these as asked, with the terms
    # named as the family executions used to name them.
    s = State(symbolised_state)
    y = Bits('y_p1', width=3)
    im_op = Bits((FALSE, TRUE, TRUE) + tuple(y.bits) +
                 (TRUE, FALSE))
    ei_op = Bits.ifelse(bools.get('is_ei_p2'),
                        Bits.cast(0xfb, width=8),
                        Bits.cast(0xf3, width=8))
    prep = (((0xed, 4), (im_op, 4)), ((ei_op, 4),),
            ((0x00, 4),), ((0x76, 4),))
    for cycles in prep[:upto]:
        run_prep_instr(s, cycles, all_orders)
    return s


def test_int_im1_from_halt(symbolised_state, all_orders=False):
    # ~int ends the halted state where interrupts are enabled
    # and the mode is 1. The preparation sets the mode and the
    # enable symbolically and runs a nop so the EI shadow is
    # over before halt; the checks are then conditioned on the
    # enable choice and on the mode-1 encodings -- ED56 and its
    # ED76 duplicate at once. The acceptance is a seven-tick
    # acknowledge cycle whose bus byte, fed symbolic, must be
    # ignored in mode 1, and two three-tick pushes of PC.
    s = run_im_ei_nop_halt_prep(symbolised_state, 4, all_orders)

    y = Bits('y_p1', width=3)
    is_im1 = y[1] & ~y[0]
    accepts = bools.get('is_ei_p2') & is_im1

    s = State(s)
    s.set_pin_and_update('~int', TRUE, all_orders)
    with Status.do('halted M1 before ~int'):
        run_halted_m1(s, 'ia1', all_orders)
    s.set_pin_and_update('~int', FALSE, all_orders)
    with Status.do('halted M1 with ~int'):
        s2 = run_halted_m1(s, 'ia2', all_orders)
    s.set_pin_and_update('~int', TRUE, all_orders)
    # The acknowledge byte is symbolic exactly in the mode-1
    # branch, where it is to be proven ignored; the other modes
    # see a nop -- executing an arbitrary symbolic byte under
    # mode 0 is no realistic setup and needlessly hard to
    # settle.
    s.set_db(Bits.ifelse(is_im1, Bits('if1', width=8),
                         Bits.cast(0x00, width=8)), all_orders)
    fetch, refresh, sp1, sp2a = get_ack_bus_addrs(s2)
    run_bus_checked_ticks(
        s, 'im1 ack', bus_int_ack_rows(fetch, refresh, sp1),
        all_orders, under=accepts)
    run_bus_checked_ticks(
        s, 'im1 push 1', bus_write_rows(sp1, sp2a, 1),
        all_orders, under=accepts)
    run_bus_checked_ticks(
        s, 'im1 push 2',
        bus_write_rows(sp2a, Bits.cast(0x0038, width=16), 0),
        all_orders, under=accepts)
    s.cache()
    after = get_effective_states(s, SAMPLED_NODES,
                                 all_orders=all_orders)

    sp = Bits.concat(sampled_bits(s2, 'reg_sph'),
                     sampled_bits(s2, 'reg_spl'))
    sp = (sp - 2).truncated(16)
    # The acknowledge cycle refreshes too, and the sampling
    # nop's increment lands within the sampled ticks.
    r_lo = (sampled_bits(s2, 'reg_r', width=7) + 2).truncated(7)
    pc = Bits.cast(0x0039, width=16)
    wz = Bits.cast(0x0038, width=16)

    def get_expected(n):
        if n == '~halt':
            # The halted state ends.
            return TRUE
        if n in (HALTED_FF, HALTED_FF_B):
            # Leaving the halted state flips the pair back.
            return TRUE if n == HALTED_FF else FALSE
        if n in (IFF1, IFF2):
            # A maskable acceptance disables both flip-flops.
            return FALSE
        if n.startswith('reg_pch'):
            # The nop settling fetches from 0x0038, and a
            # sampled pc reads advanced by one.
            return pc[int(n[-1]) + 8]
        if n.startswith('reg_pcl'):
            return pc[int(n[-1])]
        if n.startswith('reg_sph'):
            return sp[int(n[-1]) + 8]
        if n.startswith('reg_spl'):
            return sp[int(n[-1])]
        if n.startswith('reg_w'):
            return wz[int(n[-1]) + 8]
        if n.startswith('reg_z'):
            return wz[int(n[-1])]
        if n.startswith('reg_r') and n[-1] != '7':
            return r_lo[int(n[-1])]
        if n.startswith('instr'):
            # As with the ~nmi acceptance, the acknowledge
            # byte rides through the acceptance unexecuted; by
            # this sample the settling nop has replaced it in
            # the latch.
            return FALSE
        return s2[n]

    check_sampled_states('im 1 ~int from halt', after, get_expected,
                         under=accepts)


def test_int_im2_from_halt(symbolised_state, all_orders=False):
    # ~int in mode 2 reads the handler address from the vector
    # table: the acknowledge byte is the vector and the two
    # table bytes are the address, so all three are symbolic
    # exactly in the mode-2 branch -- the other branches see
    # nops. Conditioned on interrupts enabled and mode 2, that
    # is, on ED5E and its ED7E duplicate at once: the halted
    # state ends, both interrupt flip-flops come out clear, PC
    # is pushed, and execution resumes at the address the table
    # supplied, with WZ loaded to match. The bus is checked at
    # every half-tick: the acknowledge, the pushes and the
    # table reads at I:vector with bit 0 respected.
    s = run_im_ei_nop_halt_prep(symbolised_state, 4, all_orders)

    y = Bits('y_p1', width=3)
    is_im2 = y[1] & y[0]
    accepts = bools.get('is_ei_p2') & is_im2

    def in_mode_2(id):
        return Bits.ifelse(is_im2, Bits(id, width=8),
                           Bits.cast(0x00, width=8))

    s = State(s)
    s.set_pin_and_update('~int', TRUE, all_orders)
    with Status.do('halted M1 before ~int'):
        run_halted_m1(s, 'ia1', all_orders)
    s.set_pin_and_update('~int', FALSE, all_orders)
    with Status.do('halted M1 with ~int'):
        s2 = run_halted_m1(s, 'ia2', all_orders)
    s.set_pin_and_update('~int', TRUE, all_orders)

    fetch, refresh, sp1, sp2a = get_ack_bus_addrs(s2)
    # The table address: I paired with the vector as supplied,
    # bit 0 respected.
    table = Bits.concat(sampled_bits(s2, 'reg_i'),
                        Bits('iv', width=8))
    table1 = (table + 1).truncated(16)
    target = Bits.concat(Bits('ivh', width=8),
                         Bits('ivl', width=8))

    s.set_db(in_mode_2('iv'), all_orders)
    run_bus_checked_ticks(
        s, 'im2 ack', bus_int_ack_rows(fetch, refresh, sp1),
        all_orders, under=accepts)
    run_bus_checked_ticks(
        s, 'im2 push 1', bus_write_rows(sp1, sp2a, 1),
        all_orders, under=accepts)
    run_bus_checked_ticks(
        s, 'im2 push 2', bus_write_rows(sp2a, table, 1),
        all_orders, under=accepts)
    s.set_db(in_mode_2('ivl'), all_orders)
    run_bus_checked_ticks(
        s, 'im2 table lo', bus_read_rows(table, table1, 1),
        all_orders, under=accepts)
    s.set_db(in_mode_2('ivh'), all_orders)
    run_bus_checked_ticks(
        s, 'im2 table hi', bus_read_rows(table1, target, 0),
        all_orders, under=accepts)
    s.cache()
    after = get_effective_states(s, SAMPLED_NODES,
                                 all_orders=all_orders)

    handler = Bits.concat(Bits('ivh', width=8),
                          Bits('ivl', width=8))
    pc = (handler + 1).truncated(16)
    sp = Bits.concat(sampled_bits(s2, 'reg_sph'),
                     sampled_bits(s2, 'reg_spl'))
    sp = (sp - 2).truncated(16)
    # The acknowledge cycle refreshes too, and the sampling
    # nop's increment lands within the sampled ticks.
    r_lo = (sampled_bits(s2, 'reg_r', width=7) + 2).truncated(7)

    def get_expected(n):
        if n == '~halt':
            # The halted state ends.
            return TRUE
        if n in (HALTED_FF, HALTED_FF_B):
            # Leaving the halted state flips the pair back.
            return TRUE if n == HALTED_FF else FALSE
        if n in (IFF1, IFF2):
            # A maskable acceptance disables both flip-flops.
            return FALSE
        if n.startswith('reg_pch'):
            # The settling nop fetches from the handler
            # address, and a sampled pc reads advanced by one.
            return pc[int(n[-1]) + 8]
        if n.startswith('reg_pcl'):
            return pc[int(n[-1])]
        if n.startswith('reg_sph'):
            return sp[int(n[-1]) + 8]
        if n.startswith('reg_spl'):
            return sp[int(n[-1])]
        if n.startswith('reg_w'):
            return handler[int(n[-1]) + 8]
        if n.startswith('reg_z'):
            return handler[int(n[-1])]
        if n.startswith('reg_r') and n[-1] != '7':
            return r_lo[int(n[-1])]
        if n.startswith('instr'):
            # The settling nop's byte, loaded by the sample
            # point.
            return FALSE
        return s2[n]

    check_sampled_states('im 2 ~int from halt', after, get_expected,
                         under=accepts)


def test_int_im0_from_halt(symbolised_state, all_orders=False):
    # ~int in mode 0 executes the byte the interrupting device
    # supplies. The realistic byte is an RST: 11 yyy 111 with
    # the target bits symbolic, covering all eight targets at
    # once; the other branches see nops. The mode condition is
    # a single bit of y, so the claims cover ED46, ED66 and the
    # undefined ED4E/ED6E encodings together -- proving the
    # latter behave as mode 0. Conditioned on interrupts
    # enabled and mode 0: the halted state ends, both interrupt
    # flip-flops come out clear, PC -- the halt address plus
    # one -- is pushed, and execution resumes at the RST
    # target, with WZ loaded to match.
    s = run_im_ei_nop_halt_prep(symbolised_state, 4, all_orders)

    y = Bits('y_p1', width=3)
    is_im0 = ~y[1]
    accepts = bools.get('is_ei_p2') & is_im0

    ry = Bits('ry', width=3)
    rst = Bits((TRUE, TRUE, TRUE) + tuple(ry.bits) + (TRUE, TRUE))
    ack = Bits.ifelse(is_im0, rst, Bits.cast(0x00, width=8))

    s = State(s)
    s.set_pin_and_update('~int', TRUE, all_orders)
    with Status.do('halted M1 before ~int'):
        run_halted_m1(s, 'ia1', all_orders)
    s.set_pin_and_update('~int', FALSE, all_orders)
    with Status.do('halted M1 with ~int'):
        s2 = run_halted_m1(s, 'ia2', all_orders)
    s.set_pin_and_update('~int', TRUE, all_orders)
    s.set_db(ack, all_orders)
    for t in range(7 + 3 + 3):
        for ht in (0, 1):
            with s.status(f'im0 acceptance tick {t}.{ht}'):
                s.half_tick(all_orders=all_orders)
    s.cache()
    after = get_effective_states(s, SAMPLED_NODES,
                                 all_orders=all_orders)

    target = (ry.zero_extended(16) << 3)
    pc = (target + 1).truncated(16)
    sp = Bits.concat(sampled_bits(s2, 'reg_sph'),
                     sampled_bits(s2, 'reg_spl'))
    sp = (sp - 2).truncated(16)
    # The acceptance fetch refreshes too, and the sampling
    # nop's increment lands within the sampled ticks.
    r_lo = (sampled_bits(s2, 'reg_r', width=7) + 2).truncated(7)

    def get_expected(n):
        if n == '~halt':
            # The halted state ends.
            return TRUE
        if n in (HALTED_FF, HALTED_FF_B):
            # Leaving the halted state flips the pair back.
            return TRUE if n == HALTED_FF else FALSE
        if n in (IFF1, IFF2):
            # A maskable acceptance disables both flip-flops.
            return FALSE
        if n.startswith('reg_pch'):
            # The settling nop fetches from the RST target, and
            # a sampled pc reads advanced by one.
            return pc[int(n[-1]) + 8]
        if n.startswith('reg_pcl'):
            return pc[int(n[-1])]
        if n.startswith('reg_sph'):
            return sp[int(n[-1]) + 8]
        if n.startswith('reg_spl'):
            return sp[int(n[-1])]
        if n.startswith('reg_w'):
            return target[int(n[-1]) + 8]
        if n.startswith('reg_z'):
            return target[int(n[-1])]
        if n.startswith('reg_r') and n[-1] != '7':
            return r_lo[int(n[-1])]
        if n.startswith('instr'):
            # The settling nop's byte, loaded by the sample
            # point.
            return FALSE
        return s2[n]

    check_sampled_states('im 0 ~int from halt', after, get_expected,
                         under=accepts)


def prepped_base_expected(base, n):
    # The im preparation's mode lands in the latch only after
    # the raw boundary sample -- the usual writeback overlap --
    # so 'unchanged' relative to such a baseline means the
    # prepared mode for the latch, not the sampled value.
    if n == IM_LATCH0:
        return bools.get('y_p1_b0')
    if n == IM_LATCH1:
        return bools.get('y_p1_b1')
    return base[n]


def run_ei_halt_edge(symbolised_state, what, int_pending_over_ei,
                     ack_db, all_orders):
    # The shared shape of the two sharp edges: ei and halt
    # execute with ~int active -- from before ei when the
    # pending flag is set, otherwise from the start of the halt
    # M1 itself. Either way the halted state is never entered:
    # ~halt stays high throughout, the acceptance follows the
    # halt M1 directly, and the pushed return address resumes
    # past the halt. The interrupt mode stays symbolic; the
    # acceptance is checked in its mode-1 branch.
    s = run_im_ei_nop_halt_prep(symbolised_state, 1, all_orders)
    base = s.get_node_states(SAMPLED_NODES)

    def run_m1(db, ticks=4, sample_halt=False):
        s.set_db(db, all_orders)
        for t in range(ticks):
            for ht in (0, 1):
                with s.status(f'{what} tick {t}.{ht}'):
                    s.half_tick(all_orders=all_orders)
                if sample_halt:
                    s.cache()
                    check_sampled_states(
                        f'{what} ~halt at {t}.{ht}',
                        s.get_node_states(('~halt',)),
                        lambda n: TRUE)

    s.set_pin_and_update('~int', TRUE, all_orders)
    if int_pending_over_ei:
        run_m1(0x00)
        s.set_pin_and_update('~int', FALSE, all_orders)
        run_m1(0xfb)  # ei
    else:
        run_m1(0xfb)  # ei
        run_m1(0x00)
        s.set_pin_and_update('~int', FALSE, all_orders)
    run_m1(0x76, sample_halt=True)  # halt
    s.cache()

    y = Bits('y_p1', width=1 + 2)
    accepts = y[1] & ~y[0]

    s.set_pin_and_update('~int', TRUE, all_orders)
    s.set_db(Bits.ifelse(accepts, Bits(ack_db, width=8),
                         Bits.cast(0x00, width=8)), all_orders)
    for t in range(7 + 3 + 3):
        for ht in (0, 1):
            with s.status(f'{what} acceptance tick {t}.{ht}'):
                s.half_tick(all_orders=all_orders)
            if t < 3:
                s.cache()
                check_sampled_states(
                    f'{what} ~halt at ack {t}.{ht}',
                    s.get_node_states(('~halt',)),
                    lambda n: TRUE)
    s.cache()
    after = get_effective_states(s, SAMPLED_NODES,
                                 all_orders=all_orders)

    sp = Bits.concat(sampled_bits(base, 'reg_sph'),
                     sampled_bits(base, 'reg_spl'))
    sp = (sp - 2).truncated(16)
    # Five fetch cycles past the baseline refresh: the nop, ei
    # and halt M1s, the acknowledge, and the sampling nop's
    # increment landing within the sampled ticks.
    r_lo = (sampled_bits(base, 'reg_r', width=7) + 5).truncated(7)
    pc = Bits.cast(0x0039, width=16)
    wz = Bits.cast(0x0038, width=16)

    def get_expected(n):
        if n == '~halt':
            return TRUE
        if n in (IFF1, IFF2):
            return FALSE
        if n.startswith('reg_pch'):
            return pc[int(n[-1]) + 8]
        if n.startswith('reg_pcl'):
            return pc[int(n[-1])]
        if n.startswith('reg_sph'):
            return sp[int(n[-1]) + 8]
        if n.startswith('reg_spl'):
            return sp[int(n[-1])]
        if n.startswith('reg_w'):
            return wz[int(n[-1]) + 8]
        if n.startswith('reg_z'):
            return wz[int(n[-1])]
        if n.startswith('reg_r') and n[-1] != '7':
            return r_lo[int(n[-1])]
        if n.startswith('instr'):
            return FALSE
        return prepped_base_expected(base, n)

    check_sampled_states(what, after, get_expected, under=accepts)


def test_ei_halt_atomicity(symbolised_state, all_orders=False):
    # ~int is active from before ei executes: the EI shadow
    # must carry the pending interrupt past the ei; halt then
    # executes and the acceptance resumes past it -- the ei;
    # halt idiom is atomic.
    run_ei_halt_edge(symbolised_state, 'ei; halt atomicity',
                     True, 'sf1', all_orders)


def test_int_at_halt_m1(symbolised_state, all_orders=False):
    # ~int arrives at the start of the halt M1 itself, inside
    # the window where the previous instruction's writeback
    # overlaps the fetch: the halted state is still entered and
    # the acceptance resumes past the halt.
    run_ei_halt_edge(symbolised_state, '~int at halt M1',
                     False, 'sf2', all_orders)


def test_int_at_boundary(symbolised_state, all_orders=False):
    # ~int accepted at a normal instruction boundary, no halt
    # involved: the pin is active from before ei completes, the
    # EI shadow defers the acceptance past exactly one
    # instruction -- the nop -- and the acceptance follows at
    # the nop's boundary, pushing the address after it. The
    # interrupt mode stays symbolic; the acceptance is checked
    # in its mode-1 branch.
    s = run_im_ei_nop_halt_prep(symbolised_state, 1, all_orders)
    base = s.get_node_states(SAMPLED_NODES)

    def run_m1(db):
        s.set_db(db, all_orders)
        for t in range(4):
            for ht in (0, 1):
                with s.status(f'boundary int tick {t}.{ht}'):
                    s.half_tick(all_orders=all_orders)

    s.set_pin_and_update('~int', TRUE, all_orders)
    run_m1(0x00)
    s.set_pin_and_update('~int', FALSE, all_orders)
    run_m1(0xfb)  # ei
    run_m1(0x00)  # the shadow's one instruction
    s.cache()

    y = Bits('y_p1', width=3)
    accepts = y[1] & ~y[0]

    s.set_pin_and_update('~int', TRUE, all_orders)
    s.set_db(Bits.ifelse(accepts, Bits('bf1', width=8),
                         Bits.cast(0x00, width=8)), all_orders)
    for t in range(7 + 3 + 3):
        for ht in (0, 1):
            with s.status(f'boundary int acceptance tick {t}.{ht}'):
                s.half_tick(all_orders=all_orders)
    s.cache()
    after = get_effective_states(s, SAMPLED_NODES,
                                 all_orders=all_orders)

    sp = Bits.concat(sampled_bits(base, 'reg_sph'),
                     sampled_bits(base, 'reg_spl'))
    sp = (sp - 2).truncated(16)
    # Five fetch cycles past the baseline refresh: the leading
    # nop, ei, the shadowed nop, the acknowledge, and the
    # sampling nop's increment landing within the sampled
    # ticks.
    r_lo = (sampled_bits(base, 'reg_r', width=7) + 5).truncated(7)
    pc = Bits.cast(0x0039, width=16)
    wz = Bits.cast(0x0038, width=16)

    def get_expected(n):
        if n == '~halt':
            return TRUE
        if n in (IFF1, IFF2):
            return FALSE
        if n.startswith('reg_pch'):
            return pc[int(n[-1]) + 8]
        if n.startswith('reg_pcl'):
            return pc[int(n[-1])]
        if n.startswith('reg_sph'):
            return sp[int(n[-1]) + 8]
        if n.startswith('reg_spl'):
            return sp[int(n[-1])]
        if n.startswith('reg_w'):
            return wz[int(n[-1]) + 8]
        if n.startswith('reg_z'):
            return wz[int(n[-1])]
        if n.startswith('reg_r') and n[-1] != '7':
            return r_lo[int(n[-1])]
        if n.startswith('instr'):
            return FALSE
        return prepped_base_expected(base, n)

    check_sampled_states('~int at a boundary', after, get_expected,
                         under=accepts)


def test_int_at_prefix_gap(symbolised_state, all_orders=False):
    # A pending maskable interrupt does not split a prefixed
    # instruction: with ~int active from before a DD prefix
    # fetch -- the EI shadow long past -- the M1 after the
    # prefix is a plain opcode fetch, ~iorq staying high at
    # the instants an acknowledge would drive it low, and the
    # acceptance follows at the whole instruction's boundary,
    # pushing the address after the prefixed instruction. The
    # pushed bytes are asserted on the data bus during the
    # push writes.
    s = run_im_ei_nop_halt_prep(symbolised_state, 3, all_orders)
    base = s.get_node_states(SAMPLED_NODES)
    pc0 = Bits.concat(sampled_bits(base, 'reg_pch'),
                      sampled_bits(base, 'reg_pcl'))
    ret = (pc0 + 2).truncated(16)

    y = Bits('y_p1', width=3)
    accepts = bools.get('is_ei_p2') & y[1] & ~y[0]

    s.set_pin_and_update('~int', FALSE, all_orders)
    s.set_db(0xdd, all_orders)
    for t in range(4):
        for ht in (0, 1):
            with s.status(f'gap dd {t}.{ht}'):
                s.half_tick(all_orders=all_orders)
    s.set_db(0x00, all_orders)  # the prefixed nop
    for t in range(4):
        for ht in (0, 1):
            with s.status(f'gap opcode {t}.{ht}'):
                s.half_tick(all_orders=all_orders)
            s.cache()
            check_sampled_states(
                f'prefix gap ~iorq {t}.{ht}',
                s.get_node_states(('~iorq',)),
                lambda n: TRUE, under=accepts)
    s.set_pin_and_update('~int', TRUE, all_orders)
    s.set_db(Bits.ifelse(accepts, Bits('gi1', width=8),
                         Bits.cast(0x00, width=8)), all_orders)
    for t in range(7 + 3 + 3):
        for ht in (0, 1):
            with s.status(f'gap acceptance {t}.{ht}'):
                s.half_tick(all_orders=all_orders)
            if t in (8, 11):
                # The push writes' active half-ticks: the
                # return address, high byte first.
                s.cache()
                v = ret.bits[8:16] if t == 8 else ret.bits[0:8]
                v = Bits(v)
                check_sampled_states(
                    f'gap push data {t}.{ht}',
                    s.get_node_states(
                        tuple(f'db{i}' for i in range(8))),
                    lambda n: v[int(n[2:])], under=accepts)
    s.cache()
    after = get_effective_states(s, SAMPLED_NODES,
                                 all_orders=all_orders)

    sp = Bits.concat(sampled_bits(base, 'reg_sph'),
                     sampled_bits(base, 'reg_spl'))
    sp = (sp - 2).truncated(16)
    # Four fetches past the baseline: the prefix, the opcode,
    # the acknowledge, and the sampling nop's increment
    # landing within the sampled ticks.
    r_lo = (sampled_bits(base, 'reg_r', width=7) + 4).truncated(7)
    pc = Bits.cast(0x0039, width=16)
    wz = Bits.cast(0x0038, width=16)

    def get_expected(n):
        if n == '~halt':
            return TRUE
        if n in (IFF1, IFF2):
            return FALSE
        if n.startswith('reg_pch'):
            return pc[int(n[-1]) + 8]
        if n.startswith('reg_pcl'):
            return pc[int(n[-1])]
        if n.startswith('reg_sph'):
            return sp[int(n[-1]) + 8]
        if n.startswith('reg_spl'):
            return sp[int(n[-1])]
        if n.startswith('reg_w'):
            return wz[int(n[-1]) + 8]
        if n.startswith('reg_z'):
            return wz[int(n[-1])]
        if n.startswith('reg_r') and n[-1] != '7':
            return r_lo[int(n[-1])]
        if n.startswith('instr'):
            return FALSE
        return prepped_base_expected(base, n)

    check_sampled_states('~int at a prefix gap', after,
                         get_expected, under=accepts)


def test_nmi_at_prefix_gap(symbolised_state, all_orders=False):
    # Folklore says ~nmi, unlike ~int, splits the pair -- the
    # acceptance following the prefix directly and the prefix
    # getting lost. On this netlist it does not: an ~nmi edge
    # latched during the prefix fetch defers past the complete
    # prefixed instruction, exactly as ~int does. The opcode
    # fetch and execution follow the prefix normally, the
    # acceptance comes after, and the return address pushed --
    # asserted on the data bus during the push writes -- is
    # the address past both bytes: nothing is lost. The prefix
    # cell n1242 reads back at its idle level after the
    # acceptance.
    s = run_im_ei_nop_halt_prep(symbolised_state, 3, all_orders)
    base = s.get_node_states(SAMPLED_NODES)
    pc0 = Bits.concat(sampled_bits(base, 'reg_pch'),
                      sampled_bits(base, 'reg_pcl'))
    ret = (pc0 + 2).truncated(16)

    s.set_db(0xdd, all_orders)
    for t in range(4):
        for ht in (0, 1):
            if (t, ht) == (2, 0):
                s.set_pin_and_update('~nmi', FALSE, all_orders)
            with s.status(f'nmi gap dd {t}.{ht}'):
                s.half_tick(all_orders=all_orders)
    s.set_pin_and_update('~nmi', TRUE, all_orders)
    s.set_db(0x00, all_orders)  # the prefixed instruction
    for t in range(4):
        for ht in (0, 1):
            with s.status(f'nmi gap opcode {t}.{ht}'):
                s.half_tick(all_orders=all_orders)
    for t in range(5 + 3 + 3):
        for ht in (0, 1):
            with s.status(f'nmi gap acceptance {t}.{ht}'):
                s.half_tick(all_orders=all_orders)
            if t in (6, 9):
                # The push writes' active half-ticks: the
                # return address, high byte first.
                s.cache()
                v = ret.bits[8:16] if t == 6 else ret.bits[0:8]
                v = Bits(v)
                check_sampled_states(
                    f'nmi gap push data {t}.{ht}',
                    s.get_node_states(
                        tuple(f'db{i}' for i in range(8))),
                    lambda n: v[int(n[2:])])
    s.cache()
    with Status.do('prefix cell after acceptance'):
        check_sampled_states(
            'nmi gap prefix cell',
            s.get_node_states(('n1242',)), lambda n: TRUE)
    after = get_effective_states(s, SAMPLED_NODES,
                                 all_orders=all_orders)

    sp = Bits.concat(sampled_bits(base, 'reg_sph'),
                     sampled_bits(base, 'reg_spl'))
    sp = (sp - 2).truncated(16)
    # Four fetches past the baseline: the prefix, the opcode,
    # the acknowledge, and the sampling nop's increment
    # landing within the sampled ticks.
    r_lo = (sampled_bits(base, 'reg_r', width=7) + 4).truncated(7)
    pc = Bits.cast(0x0067, width=16)
    wz = Bits.cast(0x0066, width=16)

    def get_expected(n):
        if n == '~halt':
            return TRUE
        if n == IFF1:
            return FALSE
        if n == IFF2:
            # Surviving as the witness that the acceptance was
            # the non-maskable one.
            return bools.get('is_ei_p2')
        if n.startswith('reg_pch'):
            return pc[int(n[-1]) + 8]
        if n.startswith('reg_pcl'):
            return pc[int(n[-1])]
        if n.startswith('reg_sph'):
            return sp[int(n[-1]) + 8]
        if n.startswith('reg_spl'):
            return sp[int(n[-1])]
        if n.startswith('reg_w'):
            return wz[int(n[-1]) + 8]
        if n.startswith('reg_z'):
            return wz[int(n[-1])]
        if n.startswith('reg_r') and n[-1] != '7':
            return r_lo[int(n[-1])]
        if n.startswith('instr'):
            return FALSE
        return prepped_base_expected(base, n)

    check_sampled_states('~nmi at a prefix gap', after,
                         get_expected)


def test_nmi_at_prefix_chain(symbolised_state, all_orders=False):
    # Each prefix defers the acceptance by one more
    # instruction: an ~nmi edge caught during the first of
    # three chained prefixes is accepted only after the first
    # non-prefix instruction completes, the pushed return
    # address -- asserted on the data bus during the push
    # writes -- pointing past all four bytes. An unbroken
    # stream of prefix bytes therefore holds ~nmi off
    # indefinitely: the non-maskable interrupt's one loophole.
    s = run_im_ei_nop_halt_prep(symbolised_state, 3, all_orders)
    base = s.get_node_states(SAMPLED_NODES)
    pc0 = Bits.concat(sampled_bits(base, 'reg_pch'),
                      sampled_bits(base, 'reg_pcl'))
    ret = (pc0 + 4).truncated(16)

    s.set_db(0xdd, all_orders)
    for t in range(4):
        for ht in (0, 1):
            if (t, ht) == (2, 0):
                s.set_pin_and_update('~nmi', FALSE, all_orders)
            with s.status(f'nmi chain dd1 {t}.{ht}'):
                s.half_tick(all_orders=all_orders)
    s.set_pin_and_update('~nmi', TRUE, all_orders)
    for no, byte in ((2, 0xdd), (3, 0xdd)):
        s.set_db(byte, all_orders)
        for t in range(4):
            for ht in (0, 1):
                with s.status(f'nmi chain dd{no} {t}.{ht}'):
                    s.half_tick(all_orders=all_orders)
    s.set_db(0x00, all_orders)  # the prefixed instruction
    for t in range(4):
        for ht in (0, 1):
            with s.status(f'nmi chain opcode {t}.{ht}'):
                s.half_tick(all_orders=all_orders)
    for t in range(5 + 3 + 3):
        for ht in (0, 1):
            with s.status(f'nmi chain acceptance {t}.{ht}'):
                s.half_tick(all_orders=all_orders)
            if t in (6, 9):
                # The push writes' active half-ticks: the
                # return address, high byte first.
                s.cache()
                v = ret.bits[8:16] if t == 6 else ret.bits[0:8]
                v = Bits(v)
                check_sampled_states(
                    f'nmi chain push data {t}.{ht}',
                    s.get_node_states(
                        tuple(f'db{i}' for i in range(8))),
                    lambda n: v[int(n[2:])])
    s.cache()
    after = get_effective_states(s, SAMPLED_NODES,
                                 all_orders=all_orders)

    sp = Bits.concat(sampled_bits(base, 'reg_sph'),
                     sampled_bits(base, 'reg_spl'))
    sp = (sp - 2).truncated(16)
    # Six fetches past the baseline: the three prefixes, the
    # opcode, the acknowledge, and the sampling nop's
    # increment landing within the sampled ticks.
    r_lo = (sampled_bits(base, 'reg_r', width=7) + 6).truncated(7)
    pc = Bits.cast(0x0067, width=16)
    wz = Bits.cast(0x0066, width=16)

    def get_expected(n):
        if n == '~halt':
            return TRUE
        if n == IFF1:
            return FALSE
        if n == IFF2:
            return bools.get('is_ei_p2')
        if n.startswith('reg_pch'):
            return pc[int(n[-1]) + 8]
        if n.startswith('reg_pcl'):
            return pc[int(n[-1])]
        if n.startswith('reg_sph'):
            return sp[int(n[-1]) + 8]
        if n.startswith('reg_spl'):
            return sp[int(n[-1])]
        if n.startswith('reg_w'):
            return wz[int(n[-1]) + 8]
        if n.startswith('reg_z'):
            return wz[int(n[-1])]
        if n.startswith('reg_r') and n[-1] != '7':
            return r_lo[int(n[-1])]
        if n.startswith('instr'):
            return FALSE
        return prepped_base_expected(base, n)

    check_sampled_states('~nmi across a prefix chain', after,
                         get_expected)


def test_int_between_ldir_repeats(symbolised_state, all_orders=False):
    # ~int lands between two iterations of a repeating ldir:
    # the acceptance pushes the ldir's own address, so the
    # handler returns to re-execute it, and the state the
    # handler observes is one certified iteration's worth --
    # BC down one, DE and HL up one in the active bank (ldir
    # moves DE and HL alike, so the DE/HL exchange flip-flop
    # cancels out), XF and YF from bits 11 and 13 of the
    # re-execution address, PF set as BC has not run out, NF
    # and HF clear, CF, SF, ZF and A untouched. The checks are
    # conditioned on the repeat itself (BC was not 1) and on
    # mode 1.
    s = run_im_ei_nop_halt_prep(symbolised_state, 1, all_orders)
    base = s.get_node_states(SAMPLED_NODES)

    def run_cycle(what, db, ticks):
        s.set_db(db, all_orders)
        for t in range(ticks):
            for ht in (0, 1):
                with s.status(f'{what} tick {t}.{ht}'):
                    s.half_tick(all_orders=all_orders)

    s.set_pin_and_update('~int', TRUE, all_orders)
    run_cycle('ei', 0xfb, 4)
    run_cycle('shadow nop', 0x00, 4)
    s.set_pin_and_update('~int', FALSE, all_orders)
    run_cycle('ldir ED', 0xed, 4)
    run_cycle('ldir opcode', 0xb0, 4)
    run_cycle('ldir read', 'br', 3)
    run_cycle('ldir write', 'bw', 5)
    run_cycle('ldir repeat', 0x00, 5)
    s.cache()

    y = Bits('y_p1', width=3)
    exx = PossibleValues.cast(base[EXX_FF]).single_value
    ex_af = PossibleValues.cast(base[EX_AF_FF]).single_value

    def pair_bits(hi, lo):
        return Bits.concat(sampled_bits(base, f'reg_{hi}'),
                           sampled_bits(base, f'reg_{lo}'))

    repeats = pair_bits('b', 'c') != 1
    repeats = bools.ifelse(exx, repeats, pair_bits('bb', 'cc') != 1)
    accepts = (y[1] & ~y[0]) & repeats

    s.set_pin_and_update('~int', TRUE, all_orders)
    s.set_db(Bits.ifelse(accepts, Bits('bi1', width=8),
                         Bits.cast(0x00, width=8)), all_orders)
    for t in range(7 + 3 + 3):
        for ht in (0, 1):
            with s.status(f'ldir acceptance tick {t}.{ht}'):
                s.half_tick(all_orders=all_orders)
    s.cache()
    after = get_effective_states(s, SAMPLED_NODES,
                                 all_orders=all_orders)

    changed = {}

    def step_pair(hi, lo, active, dec):
        pair = pair_bits(hi, lo)
        new = (pair - 1) if dec else (pair + 1)
        new = new.truncated(16)
        for i in range(8):
            changed[f'reg_{hi}{i}'] = bools.ifelse(
                active, new[i + 8], pair[i + 8])
            changed[f'reg_{lo}{i}'] = bools.ifelse(
                active, new[i], pair[i])

    step_pair('b', 'c', exx, dec=True)
    step_pair('bb', 'cc', ~exx, dec=True)
    step_pair('d', 'e', exx, dec=False)
    step_pair('dd', 'ee', ~exx, dec=False)
    step_pair('h', 'l', exx, dec=False)
    step_pair('hh', 'll', ~exx, dec=False)

    # The raw boundary sample holds pc as the next fetch
    # address itself -- the ei's; the ldir sits two
    # instructions past it.
    base_pc = Bits.concat(sampled_bits(base, 'reg_pch'),
                          sampled_bits(base, 'reg_pcl'))
    ldir_addr = (base_pc + 2).truncated(16)
    new_flags = {NF_BIT: FALSE, PF_BIT: TRUE, XF_BIT: ldir_addr[11],
                 HF_BIT: FALSE, YF_BIT: ldir_addr[13]}
    for i, v in new_flags.items():
        changed[f'reg_f{i}'] = bools.ifelse(
            ex_af, v, PossibleValues.cast(base[f'reg_f{i}'])
            .single_value)
        changed[f'reg_ff{i}'] = bools.ifelse(
            ex_af, PossibleValues.cast(base[f'reg_ff{i}'])
            .single_value, v)

    sp = Bits.concat(sampled_bits(base, 'reg_sph'),
                     sampled_bits(base, 'reg_spl'))
    sp = (sp - 2).truncated(16)
    # Six fetch cycles past the baseline refresh: ei, the
    # shadow nop, the ldir's two opcode fetches, the
    # acknowledge, and the sampling nop's increment landing
    # within the sampled ticks.
    r_lo = (sampled_bits(base, 'reg_r', width=7) + 6).truncated(7)
    pc = Bits.cast(0x0039, width=16)
    wz = Bits.cast(0x0038, width=16)

    def get_expected(n):
        if n in changed:
            return changed[n]
        if n == '~halt':
            return TRUE
        if n in (IFF1, IFF2):
            return FALSE
        if n.startswith('reg_pch'):
            return pc[int(n[-1]) + 8]
        if n.startswith('reg_pcl'):
            return pc[int(n[-1])]
        if n.startswith('reg_sph'):
            return sp[int(n[-1]) + 8]
        if n.startswith('reg_spl'):
            return sp[int(n[-1])]
        if n.startswith('reg_w'):
            return wz[int(n[-1]) + 8]
        if n.startswith('reg_z'):
            return wz[int(n[-1])]
        if n.startswith('reg_r') and n[-1] != '7':
            return r_lo[int(n[-1])]
        if n.startswith('instr'):
            return FALSE
        return prepped_base_expected(base, n)

    check_sampled_states('~int between ldir repeats', after,
                         get_expected, under=accepts)


def test_int_at_ld_a_ir(symbolised_state, all_orders=False):
    # The famous quirk: ld a, i and ld a, r copy IFF2 into PF,
    # and an interrupt accepted at the instruction's own
    # boundary is said to leave PF clear on NMOS parts -- the
    # acceptance's clearing of IFF2 winning over the copy. The
    # pin goes active only once the instruction has started, so
    # the acceptance lands exactly at its boundary; the checks
    # state the lore -- PF clear -- and a failure report would
    # show the true value, or the outcome set if it is a race.
    # The instruction choice is symbolic; for ld a, r the
    # copied value has both of the instruction's own refresh
    # increments already in.
    s = run_im_ei_nop_halt_prep(symbolised_state, 1, all_orders)
    base = s.get_node_states(SAMPLED_NODES)

    def run_cycle(what, db, ticks):
        s.set_db(db, all_orders)
        for t in range(ticks):
            for ht in (0, 1):
                with s.status(f'{what} tick {t}.{ht}'):
                    s.half_tick(all_orders=all_orders)

    s.set_pin_and_update('~int', TRUE, all_orders)
    run_cycle('ei', 0xfb, 4)
    run_cycle('shadow nop', 0x00, 4)
    s.set_pin_and_update('~int', FALSE, all_orders)
    is_lar = bools.get('is_lar')
    run_cycle('ld a, {i, r} ED', 0xed, 4)
    run_cycle('ld a, {i, r} opcode',
              Bits.ifelse(is_lar, Bits.cast(0x5f, width=8),
                          Bits.cast(0x57, width=8)), 5)
    s.cache()

    y = Bits('y_p1', width=3)
    accepts = y[1] & ~y[0]
    ex_af = PossibleValues.cast(base[EX_AF_FF]).single_value

    s.set_pin_and_update('~int', TRUE, all_orders)
    s.set_db(Bits.ifelse(accepts, Bits('li1', width=8),
                         Bits.cast(0x00, width=8)), all_orders)
    for t in range(7 + 3 + 3):
        for ht in (0, 1):
            with s.status(f'ld a, {{i, r}} acceptance '
                          f'tick {t}.{ht}'):
                s.half_tick(all_orders=all_orders)
    s.cache()
    after = get_effective_states(s, SAMPLED_NODES,
                                 all_orders=all_orders)

    i_reg = sampled_bits(base, 'reg_i')
    # R as of the copy: the base value plus the four fetches up
    # to and including the instruction's own -- ei, the shadow
    # nop and its two opcode fetches.
    r_at_copy = (sampled_bits(base, 'reg_r', width=7) + 4)
    r_at_copy = Bits(r_at_copy.truncated(7).bits +
                     (sampled_bits(base, 'reg_r')[7],))
    copied = Bits.ifelse(is_lar, r_at_copy, i_reg)
    changed = {}
    for j in range(8):
        changed[f'reg_a{j}'] = bools.ifelse(
            ex_af, copied[j],
            PossibleValues.cast(base[f'reg_a{j}']).single_value)
        changed[f'reg_aa{j}'] = bools.ifelse(
            ex_af,
            PossibleValues.cast(base[f'reg_aa{j}']).single_value,
            copied[j])
    new_flags = {NF_BIT: FALSE, HF_BIT: FALSE,
                 # The lore under test: the acceptance's
                 # clearing of IFF2 wins over the copy to PF.
                 PF_BIT: FALSE,
                 XF_BIT: copied[3], YF_BIT: copied[5],
                 ZF_BIT: copied == 0, SF_BIT: copied[7]}
    for j, v in new_flags.items():
        changed[f'reg_f{j}'] = bools.ifelse(
            ex_af, v,
            PossibleValues.cast(base[f'reg_f{j}']).single_value)
        changed[f'reg_ff{j}'] = bools.ifelse(
            ex_af,
            PossibleValues.cast(base[f'reg_ff{j}']).single_value, v)

    sp = Bits.concat(sampled_bits(base, 'reg_sph'),
                     sampled_bits(base, 'reg_spl'))
    sp = (sp - 2).truncated(16)
    # Six fetch cycles past the baseline refresh: ei, the
    # shadow nop, ld a, i's two opcode fetches, the
    # acknowledge, and the sampling nop's increment landing
    # within the sampled ticks.
    r_lo = (sampled_bits(base, 'reg_r', width=7) + 6).truncated(7)
    pc = Bits.cast(0x0039, width=16)
    wz = Bits.cast(0x0038, width=16)

    def get_expected(n):
        if n in changed:
            return changed[n]
        if n == '~halt':
            return TRUE
        if n in (IFF1, IFF2):
            return FALSE
        if n.startswith('reg_pch'):
            return pc[int(n[-1]) + 8]
        if n.startswith('reg_pcl'):
            return pc[int(n[-1])]
        if n.startswith('reg_sph'):
            return sp[int(n[-1]) + 8]
        if n.startswith('reg_spl'):
            return sp[int(n[-1])]
        if n.startswith('reg_w'):
            return wz[int(n[-1]) + 8]
        if n.startswith('reg_z'):
            return wz[int(n[-1])]
        if n.startswith('reg_r') and n[-1] != '7':
            return r_lo[int(n[-1])]
        if n.startswith('instr'):
            return FALSE
        return prepped_base_expected(base, n)

    check_sampled_states('~int at ld a, {i, r}', after,
                         get_expected, under=accepts)


def test_nmi_over_int_priority(symbolised_state, all_orders=False):
    # ~nmi and ~int are both active at the same instruction
    # boundary with interrupts enabled: the non-maskable one
    # must win. The proof that the maskable one never sneaked
    # in is IFF2 surviving as set -- the ~nmi acceptance
    # preserves it, where a maskable acceptance would have
    # cleared it -- while IFF1 comes out clear, blocking the
    # still-pending ~int until the handler re-enables.
    s = run_im_ei_nop_halt_prep(symbolised_state, 1, all_orders)
    base = s.get_node_states(SAMPLED_NODES)

    def run_cycle(what, db, ticks):
        s.set_db(db, all_orders)
        for t in range(ticks):
            for ht in (0, 1):
                with s.status(f'{what} tick {t}.{ht}'):
                    s.half_tick(all_orders=all_orders)

    s.set_pin_and_update('~int', TRUE, all_orders)
    s.set_pin_and_update('~nmi', TRUE, all_orders)
    run_cycle('ei', 0xfb, 4)
    s.set_pin_and_update('~int', FALSE, all_orders)
    s.set_pin_and_update('~nmi', FALSE, all_orders)
    run_cycle('shadow nop', 0x00, 4)
    s.cache()

    s.set_pin_and_update('~nmi', TRUE, all_orders)
    s.set_db('pf1', all_orders)
    for t in range(5 + 3 + 3):
        for ht in (0, 1):
            with s.status(f'priority acceptance tick {t}.{ht}'):
                s.half_tick(all_orders=all_orders)
    s.cache()
    after = get_effective_states(s, SAMPLED_NODES,
                                 all_orders=all_orders)

    sp = Bits.concat(sampled_bits(base, 'reg_sph'),
                     sampled_bits(base, 'reg_spl'))
    sp = (sp - 2).truncated(16)
    # Four fetch cycles past the baseline refresh: ei, the
    # shadow nop, the acceptance fetch, and the sampling nop's
    # increment landing within the sampled ticks.
    r_lo = (sampled_bits(base, 'reg_r', width=7) + 4).truncated(7)
    pc = Bits.cast(0x0067, width=16)
    wz = Bits.cast(0x0066, width=16)

    def get_expected(n):
        if n == '~halt':
            return TRUE
        if n == IFF1:
            return FALSE
        if n == IFF2:
            # Still set: only ~nmi accepted; a maskable
            # acceptance would have cleared it.
            return TRUE
        if n.startswith('reg_pch'):
            return pc[int(n[-1]) + 8]
        if n.startswith('reg_pcl'):
            return pc[int(n[-1])]
        if n.startswith('reg_sph'):
            return sp[int(n[-1]) + 8]
        if n.startswith('reg_spl'):
            return sp[int(n[-1])]
        if n.startswith('reg_w'):
            return wz[int(n[-1]) + 8]
        if n.startswith('reg_z'):
            return wz[int(n[-1])]
        if n.startswith('reg_r') and n[-1] != '7':
            return r_lo[int(n[-1])]
        if n.startswith('instr'):
            return FALSE
        return prepped_base_expected(base, n)

    check_sampled_states('~nmi over ~int priority', after,
                         get_expected)


def test_reset_from_halt(base_state, all_orders=False):
    # ~reset ends the halted state. Afterwards the CPU fetches
    # from address 0 with interrupts disabled and the refresh
    # counter restarted -- and everything else survives: the
    # register file, the flags, their shadows, the exchange
    # flip-flops and WZ keep their symbolic values through the
    # reset.
    s = State(base_state)
    with Status.do('halted M1'):
        s2 = run_halted_m1(s, 'rh1', all_orders)
    with Status.do('reset'):
        s.reset(propagation_delay=31, waiting_for_m1_delay=5)
    after = get_effective_states(s, SAMPLED_NODES,
                                 all_orders=all_orders)

    def get_expected(n):
        if n == '~halt':
            # The halted state ends.
            return TRUE
        if n in (HALTED_FF, HALTED_FF_B):
            # Leaving the halted state flips the pair back.
            return TRUE if n == HALTED_FF else FALSE
        if n in (IFF1, IFF2, IM_LATCH0, IM_LATCH1):
            # Reset also restarts the interrupt mode at 0.
            return FALSE
        if n.startswith('reg_pc') or n[4:-1] == 'i':
            # The settling nop fetches from address 0, and a
            # sampled pc reads advanced by one; I restarts at
            # zero.
            return TRUE if n == 'reg_pcl0' else FALSE
        if n.startswith('reg_r'):
            # R restarts at zero; the sampling nop's increment
            # lands within the sampled ticks.
            return TRUE if n == 'reg_r0' else FALSE
        if n.startswith('instr'):
            # The settling nop's byte, loaded by the sample
            # point.
            return FALSE
        return s2[n]

    check_sampled_states('reset from halt', after, get_expected)


def get_cond_as_expr(cond, phase, before):
    if cond is None:
        return None
    if cond == 'b != 1':
        b = Bits.ifelse(
                before[EXX_FF],
                Bits(before[f'reg_b{i}'] for i in range(8)),
                Bits(before[f'reg_bb{i}'] for i in range(8)))
        return b != 1
    if cond == 'bc != 1':
        b = Bits.ifelse(
                before[EXX_FF],
                Bits(before[f'reg_b{i}'] for i in range(8)),
                Bits(before[f'reg_bb{i}'] for i in range(8)))
        c = Bits.ifelse(
                before[EXX_FF],
                Bits(before[f'reg_c{i}'] for i in range(8)),
                Bits(before[f'reg_cc{i}'] for i in range(8)))
        return Bits.concat(b, c) != 1
    if cond == 'is_prefixed':
        return bools.get(f'is_prefixed_p{phase}')
    if cond == 'not_prefixed':
        return ~bools.get(f'is_prefixed_p{phase}')
    if cond == 'bc != 1 && a != r':
        b = Bits.ifelse(
                before[EXX_FF],
                Bits(before[f'reg_b{i}'] for i in range(8)),
                Bits(before[f'reg_bb{i}'] for i in range(8)))
        c = Bits.ifelse(
                before[EXX_FF],
                Bits(before[f'reg_c{i}'] for i in range(8)),
                Bits(before[f'reg_cc{i}'] for i in range(8)))
        a = Bits.ifelse(
                before[EX_AF_FF],
                Bits(before[f'reg_a{i}'] for i in range(8)),
                Bits(before[f'reg_aa{i}'] for i in range(8)))
        r = Bits(f'r_p{phase}', width=8)
        return (Bits.concat(b, c) != 1) & (a != r)
    f = Bits.ifelse(
            before[EX_AF_FF],
            Bits(before[f'reg_f{i}'] for i in range(8)),
            Bits(before[f'reg_ff{i}'] for i in range(8)))
    if cond == 'cc':
        cc = Bits(f'cc_p{phase}', width=3)
        return bools.ifelse(
            cc[2],
            bools.ifelse(
                cc[1],
                bools.ifelse(cc[0], f[SF_BIT], ~f[SF_BIT]),
                bools.ifelse(cc[0], f[PF_BIT], ~f[PF_BIT])),
            bools.ifelse(
                cc[1],
                bools.ifelse(cc[0], f[CF_BIT], ~f[CF_BIT]),
                bools.ifelse(cc[0], f[ZF_BIT], ~f[ZF_BIT])))
    if cond == 'cc2':
        cc2 = Bits(f'cc2_p{phase}', width=2)
        return bools.ifelse(
            cc2[1],
            bools.ifelse(cc2[0], f[CF_BIT], ~f[CF_BIT]),
            bools.ifelse(cc2[0], f[ZF_BIT], ~f[ZF_BIT]))
    assert 0, cond


def execute_instr(s, id, phase, before, all_orders=False):
    if SEED is not None:
        random.seed(SEED)

    cycles = TestedInstrs.get_cycles(id, phase)
    for cycle_no, (d, ticks, cond) in enumerate(cycles):
        cond = get_cond_as_expr(cond, phase, before)
        s.set_db(d, all_orders)
        for t in range(ticks):
            for ht in (0, 1):
                with s.status(f'tick {cycle_no}-{t}.{ht}'):
                    s.half_tick(cond=cond, all_orders=all_orders)
        s.cache(intermediate=True)


# The per-instruction microstate among the structural state
# nodes, flagged by --probe-closed-world across diverse
# families: instruction and prefix decode cells and the
# byte-wide data-path latch blocks. Documented exclusions from
# the closed-world unchanged checks, pending expectations of
# their own.
_CLOSED_WORLD_EXCLUDED = frozenset((
    'n1242', 'n1245', 'n1616', 'n1683', 'n701',
    'p633', 'p1702', 'p1703',
    'n1243', 'n696', 'n729', 'p1844', 'p1853', 'p1854', 'p1880',
    'p1881', 'p1957', 'p1958', 'p1985', 'p1986', 'p2057',
    'p2058', 'p2081', 'p2082', 'p2154', 'p2155', 'p2167',
    'p2178', 'p2179', 'p2259', 'p2270', 'p2271', 'p2284',
    'p2295', 'p2296', 'p2372', 'p2383', 'p2384', 'p2404',
    'p2415', 'p2416', 'p2477', 'p2488', 'p2489', 'p2515',
    'p2526', 'p2527', 'p2598', 'p2609', 'p2610', 'p2621',
    'p2632', 'p2633',
    'p1871', 'p1948', 'p1976', 'p2048', 'p2072', 'p2145',
    'p2169', 'p2261', 'p2286', 'p2374', 'p2406', 'p2479',
    'p2517', 'p2600', 'p2623', 'n1830', 'n1909',
    'n1931', 'n2013', 'n2034', 'n2114', 'n2134', 'n2222',
    'n2248', 'n2329', 'n2361', 'n2448', 'n2465', 'n2562',
    'n2588', 'n2674', 'n647', 'n697', 'n726', 'p1077', 'p1773',
    'p1783', 'p1841', 'p1842', 'p1843', 'p1845', 'p1846',
    'p1847', 'p1848', 'p1849', 'p1850', 'p1851', 'p1852',
    'p1868', 'p1869', 'p1870', 'p1872', 'p1873', 'p1874',
    'p1875', 'p1876', 'p1877', 'p1878', 'p1879', 'p1945',
    'p1946', 'p1947', 'p1949', 'p1950', 'p1951', 'p1952',
    'p1953', 'p1954', 'p1955', 'p1956', 'p1973', 'p1974',
    'p1975', 'p1977', 'p1978', 'p1979', 'p1980', 'p1981',
    'p1982', 'p1983', 'p1984', 'p2045', 'p2046', 'p2047',
    'p2049', 'p2050', 'p2051', 'p2052', 'p2053', 'p2054',
    'p2055', 'p2056', 'p2069', 'p2070', 'p2071', 'p2073',
    'p2074', 'p2075', 'p2076', 'p2077', 'p2078', 'p2079',
    'p2080', 'p2142', 'p2143', 'p2144', 'p2146', 'p2147',
    'p2148', 'p2149', 'p2150', 'p2151', 'p2152', 'p2153',
    'p2166', 'p2168', 'p2170', 'p2171', 'p2172', 'p2173',
    'p2174', 'p2175', 'p2176', 'p2177', 'p2258', 'p2260',
    'p2262', 'p2263', 'p2264', 'p2265', 'p2266', 'p2267',
    'p2268', 'p2269', 'p2283', 'p2285', 'p2287', 'p2288',
    'p2289', 'p2290', 'p2291', 'p2292', 'p2293', 'p2294',
    'p2371', 'p2373', 'p2375', 'p2376', 'p2377', 'p2378',
    'p2379', 'p2380', 'p2381', 'p2382', 'p2403', 'p2405',
    'p2407', 'p2408', 'p2409', 'p2410', 'p2411', 'p2412',
    'p2413', 'p2414', 'p2476', 'p2478', 'p2480', 'p2481',
    'p2482', 'p2483', 'p2484', 'p2485', 'p2486', 'p2487',
    'p2514', 'p2516', 'p2518', 'p2519', 'p2520', 'p2521',
    'p2522', 'p2523', 'p2524', 'p2525', 'p2597', 'p2599',
    'p2601', 'p2602', 'p2603', 'p2604', 'p2605', 'p2606',
    'p2607', 'p2608', 'p2620', 'p2622', 'p2624', 'p2625',
    'p2626', 'p2627', 'p2628', 'p2629', 'p2630', 'p2631', 'p687',
    'p98'))

_closed_world_nodes = None


def get_closed_world_nodes(s):
    # The structurally identified state nodes -- everything on
    # the die that can remember -- not already in the tested
    # set nor among the documented microstate exclusions:
    # checked as unchanged across every covered instruction.
    global _closed_world_nodes
    if _closed_world_nodes is None:
        nums = get_structural_state_ids()
        _closed_world_nodes = tuple(sorted(
            n for n in s.get_node_states()
            if n not in SAMPLED_NODES and
            n not in _CLOSED_WORLD_EXCLUDED and
            n.lstrip('~npmx').isdigit() and
            int(n.lstrip('~npmx')) in nums))
    return _closed_world_nodes


def process_instr(instrs, base_state, *, test=False, all_orders=False):
    phase = len(instrs)
    id = instrs[-1]

    s = State(base_state)
    at_start = s.get_node_states(SAMPLED_NODES)
    before = get_effective_states(s, SAMPLED_NODES)
    if test:
        cw_nodes = get_closed_world_nodes(s)
        cw_start = s.get_node_states(cw_nodes)

    # The tested instruction and the extra ticks following it
    # simulate all orders of updates when so asked; the
    # preparation stays shared with plain runs.
    execute_instr(s, id, phase, before, all_orders=all_orders)

    s.cache()
    after_instr_state = State(s)

    if not test:
        return after_instr_state

    # Status.print('; '.join(instr_ids))

    at_end = s.get_node_states(SAMPLED_NODES)
    after = get_effective_states(s, SAMPLED_NODES, all_orders=all_orders)

    for n in sorted(TESTED_NODES):
        token = test_node(instrs, n, at_start, at_end, before, after,
                          all_orders)
        assert isinstance(token, CheckToken)

    # The closed world: every other state cell on the die must
    # come out of the instruction as it went in. All offenders
    # are gathered before failing, to keep the triage loop
    # short.
    cw_end = s.get_node_states(cw_nodes)
    cw_bad = []
    for n in cw_nodes:
        b, a = cw_start[n], cw_end[n]
        if b is a:
            continue
        av = PossibleValues.cast(a)
        bv = PossibleValues.cast(b)
        if (av.is_single and bv.is_single and
                bools.is_equiv(av.single_value,
                               bv.single_value)):
            continue
        cw_bad.append(n)
    if cw_bad:
        Status.clear()
        print('FAILED: closed world: ' + ' '.join(cw_bad))
        raise TestFailure()

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


def get_z3_simplified(b, rename_term=None):
    # Verbalises an eqbool value via z3's simplification tactics
    # -- the sole use of z3 in this simulator.
    cache = {}

    def get(v):
        kind = v.kind
        if kind in ('false', 'true'):
            return z3.BoolVal(kind == 'true')
        if kind == 'term':
            t = v.term
            if rename_term is not None:
                t = rename_term(t)
            return z3.Bool(str(t))

        key = v.id
        r = cache.get(key)
        if r is not None:
            return r

        if kind == 'eq':
            x, y = v.args
            r = get(x) == get(y)
        elif kind == 'not':
            r = z3.Not(get(~v))
        else:
            OPS = {'or': z3.Or, 'and': z3.And, 'ifelse': z3.If}
            r = OPS[kind](*(get(op) for op in v.args))
        cache[key] = r
        return r

    e = get(b)
    for t in ('qe2', 'solver-subsumption') * 3:
        e = z3.Tactic(t).apply(e).as_expr()
    if z3.is_false(e):
        return '0'
    if z3.is_true(e):
        return '1'
    return str(e)


def run_all_orders(instr):
    # Executes the given instruction on a live simulator in
    # all-orders mode, starting from the symbolised state, then
    # reports the possible values of the flag and register
    # nodes. Unlike the usual step replay, this never loads
    # cached states for the executed steps.
    global SEED

    state = build_symbolised_state()
    state.cache()

    phase = 1
    cycles = TestedInstrs.get_cycles(instr, phase)

    def execute(sim, all_orders):
        def set_db(d):
            if isinstance(d, str):
                d = Bits(d, width=8)
            else:
                d = Bits.cast(d, width=8)
            for i, b in enumerate(d):
                sim.set_pin_pull(f'db{i}', b)
                sim.update_pin(f'db{i}', all_orders)

        def run_cycle(what, d, ticks):
            with Status.do(f'{what} set db'):
                set_db(d)
            for t in range(ticks):
                for ht in (0, 1):
                    with Status.do(f'{what} tick {t}.{ht}'):
                        sim.half_tick(all_orders=all_orders)

        for cycle_no, (d, ticks, cond) in enumerate(cycles):
            # TODO: Support conditional ticks.
            assert cond is None, (instr, cond)
            run_cycle(f'cycle {cycle_no}', d, ticks)

        # Let the values settle into their nodes, the same way
        # get_effective_states() does.
        run_cycle('settle', 0x00, 3)

    with Status.do('build simulator'):
        sim = Z80Simulator(image=state.image)
    execute(sim, all_orders=True)

    def rename_event(t):
        def rename(event):
            s, index = event.removeprefix('u').split('.')
            return f'{sim.get_node_id(int(index))}@{s}'

        before, after = t.split('<')
        return f'{rename(before)} < {rename(after)}'

    NODES = tuple(f'{r}{i}'
                  for r in ('reg_f', 'reg_ff', 'reg_a', 'reg_aa')
                  for i in range(8))

    results = {}
    for id in NODES:
        pv = sim.get_node_entries(sim.get_node(id), {})
        results[id] = pv
        with Status.do(f'verbalise {id}'):
            if pv.is_single:
                line = f'{id}: {get_z3_simplified(pv.single_value)}'
            else:
                lines = [f'{id}: {len(pv.entries)} possible values']
                for no, (v, orders) in enumerate(pv.entries):
                    lines.append(f'  {no}) {get_z3_simplified(v)}')
                    lines.append('    under ' + get_z3_simplified(
                        orders.e, rename_event))
                line = '\n'.join(lines)
        Status.clear()
        print(line)

    # Every single-order execution must land on one of the
    # possible values -- including the order of --seed=568 known
    # to yield an XF outcome differing from the default order's,
    # the original evidence of issue #51.
    orig_seed = SEED
    for seed in None, 1, 2, 3, 568:
        SEED = seed
        if seed is not None:
            random.seed(seed)
        with Status.do(f'seed {seed}'):
            s = Z80Simulator(image=state.image)
            execute(s, all_orders=False)

            takes = []
            for id in NODES:
                v = s.get_node_state(s.get_node(id))
                matches = [no for no, (u, _)
                           in enumerate(results[id].entries)
                           if bools.is_equiv(u, v)]
                assert len(matches) == 1, (seed, id)
                if not results[id].is_single:
                    takes.append(f'{id} {matches[0]})')
        Status.clear()
        print(f'seed {seed}: single-order results are all among '
              f'the possible values; ' + ', '.join(takes))
    SEED = orig_seed


# The measured per-cycle-kind bus templates, per half-tick:
# the address role and the ~m1/~mreq/~rd/~wr/~iorq/~rfsh
# levels. 'cur' is the cycle's own address, 'ir' the refresh
# address, 'next' the following cycle's address appearing at
# the last half-tick, where ~m1 asserts exactly when that
# cycle is a fetch.
_BUS_M1_4 = (
    ('cur', 0, 0, 0, 1, 1, 1), ('cur', 0, 0, 0, 1, 1, 1),
    ('cur', 0, 0, 0, 1, 1, 1),
    ('ir', 1, 1, 1, 1, 1, 0), ('ir', 1, 0, 1, 1, 1, 0),
    ('ir', 1, 0, 1, 1, 1, 0), ('ir', 1, 1, 1, 1, 1, 0),
    ('next', None, 1, 1, 1, 1, 1))
_BUS_M1_5 = _BUS_M1_4[:7] + (
    ('ir', 1, 1, 1, 1, 1, 1), ('ir', 1, 1, 1, 1, 1, 1),
    ('next', None, 1, 1, 1, 1, 1))
_BUS_R3 = (
    ('cur', 1, 0, 0, 1, 1, 1), ('cur', 1, 0, 0, 1, 1, 1),
    ('cur', 1, 0, 0, 1, 1, 1), ('cur', 1, 0, 0, 1, 1, 1),
    ('cur', 1, 1, 1, 1, 1, 1),
    ('next', None, 1, 1, 1, 1, 1))
_BUS_W3 = (
    ('cur', 1, 0, 1, 1, 1, 1), ('cur', 1, 0, 1, 1, 1, 1),
    ('cur', 1, 0, 1, 0, 1, 1), ('cur', 1, 0, 1, 0, 1, 1),
    ('cur', 1, 1, 1, 1, 1, 1),
    ('next', None, 1, 1, 1, 1, 1))
_BUS_M1_6 = _BUS_M1_4[:7] + (
    ('ir', 1, 1, 1, 1, 1, 1),) * 4 + (
    ('next', None, 1, 1, 1, 1, 1),)
# The bus holds its last driven value: internal cycles drive
# nothing, so they keep the previous cycle's final address --
# the refresh address after a fetch, the transfer address
# after a read -- with all strobes idle, and the next real
# cycle's address appears only at the last internal cycle's
# final half-tick. A cycle followed by an internal one
# accordingly keeps its own address at its handover half-tick.
_BUS_M1_INT = _BUS_M1_4[:7] + (('ir', 1, 1, 1, 1, 1, 1),)
_BUS_R3_INT = _BUS_R3[:5] + (('cur', 1, 1, 1, 1, 1, 1),)
_BUS_E4 = (('hold', 1, 1, 1, 1, 1, 1),) * 8
_BUS_E3_END = (('hold', 1, 1, 1, 1, 1, 1),) * 5 + (
    ('next', None, 1, 1, 1, 1, 1),)
_BUS_E5_END = (('hold', 1, 1, 1, 1, 1, 1),) * 9 + (
    ('next', None, 1, 1, 1, 1, 1),)
_BUS_E1 = (('hold', 1, 1, 1, 1, 1, 1),
           ('next', None, 1, 1, 1, 1, 1))
_BUS_R4 = (
    ('cur', 1, 0, 0, 1, 1, 1),) * 4 + (
    ('cur', 1, 1, 1, 1, 1, 1),) * 3 + (
    ('next', None, 1, 1, 1, 1, 1),)
_BUS_W5 = _BUS_W3[:5] + (
    ('cur', 1, 1, 1, 1, 1, 1),) * 4 + (
    ('next', None, 1, 1, 1, 1, 1),)
_BUS_IO_R = (
    ('cur', 1, 1, 1, 1, 1, 1),) + (
    ('cur', 1, 1, 0, 1, 0, 1),) * 5 + (
    ('cur', 1, 1, 1, 1, 1, 1),) + (
    ('next', None, 1, 1, 1, 1, 1),)
_BUS_IO_W = (
    ('cur', 1, 1, 1, 1, 1, 1),) + (
    ('cur', 1, 1, 1, 0, 0, 1),) * 5 + (
    ('cur', 1, 1, 1, 1, 1, 1),) + (
    ('next', None, 1, 1, 1, 1, 1),)
_BUS_E2_END = (('hold', 1, 1, 1, 1, 1, 1),) * 3 + (
    ('next', None, 1, 1, 1, 1, 1),)
_BUS_TEMPLATES = {'m1': _BUS_M1_4, 'm15': _BUS_M1_5,
                  'm16': _BUS_M1_6, 'm1i': _BUS_M1_INT,
                  'ri': _BUS_R3_INT, 'e4': _BUS_E4,
                  'e3end': _BUS_E3_END, 'e5end': _BUS_E5_END,
                  'e1': _BUS_E1,
                  'r': _BUS_R3, 'r4': _BUS_R4,
                  'w': _BUS_W3, 'w5': _BUS_W5,
                  'io_r': _BUS_IO_R, 'io_w': _BUS_IO_W,
                  'm1p': _BUS_M1_4, 'm1n': _BUS_M1_4,
                  'rp': _BUS_R3, 'rip': _BUS_R3_INT,
                  'ron': _BUS_R3_INT,
                  'e5p': _BUS_E5_END, 'e2p': _BUS_E2_END}
_BUS_FETCH_KINDS = ('m1', 'm15', 'm16', 'm1i', 'm1p', 'm1n')
# The kinds carrying a conditionally present instruction byte:
# each shifts later pc-relative addresses by one where it is
# there. 'ron' is deliberately absent: it re-reads, under the
# prefix, the byte the conditional 'm1n' fetch already counts.
_BUS_SHIFT_KINDS = ('m1p', 'rp', 'rip')

# Per-family cycle kinds and addresses; families with
# variant-dependent bus behaviour or cycle kinds not yet
# probed (r4, w5, the internal e cycles, the six-tick M1, the
# conditional cycles) are not listed yet.
BUS_SPECS = {
    'nop': (('m1p', 'pc'), ('m1', 'pc')),
    "ex af, af'": (('m1p', 'pc'), ('m1', 'pc')),
    'daa': (('m1p', 'pc'), ('m1', 'pc')),
    'cpl': (('m1p', 'pc'), ('m1', 'pc')),
    'scf/ccf': (('m1p', 'pc'), ('m1', 'pc')),
    'exx': (('m1p', 'pc'), ('m1', 'pc')),
    'ex de, hl': (('m1p', 'pc'), ('m1', 'pc')),
    'ld sp, hl': (('m1p', 'pc'), ('m16', 'pc')),
    'ei/di': (('m1p', 'pc'), ('m1', 'pc')),
    'jp hl': (('m1p', 'pc'), ('m1', 'pc')),
    'halt': (('m1p', 'pc'), ('m1', 'pc')),
    'ld {b, c, d, e, h, l, a}, {b, c, d, e, h, l, a}':
        (('m1p', 'pc'), ('m1', 'pc')),
    'rlca/rrca/rla/rra': (('m1p', 'pc'), ('m1', 'pc')),
    'inc/dec {b, c, d, e, h, l, a}': (('m1p', 'pc'),
                                      ('m1', 'pc')),
    'ld {b, c, d, e, h, l, a}, n':
        (('m1p', 'pc'), ('m1', 'pc'), ('r', 'pc+1')),
    'ld {b, c, d, e, h, l, a}, (hl)':
        (('m1p', 'pc'), ('m1', 'pc'), ('rip', 'pc+1'),
         ('e5p', '-'), ('r', 'hl')),
    'ld (hl), {b, c, d, e, h, l, a}':
        (('m1p', 'pc'), ('m1', 'pc'), ('rip', 'pc+1'),
         ('e5p', '-'), ('w', 'hl')),
    'ld (hl), n': (('m1p', 'pc'), ('m1', 'pc'), ('rp', 'pc+1'),
                   ('r', 'pc+1'), ('e2p', '-'), ('w', 'hl')),
    'ld <rp>, nn': (('m1p', 'pc'), ('m1', 'pc'), ('r', 'pc+1'),
                    ('r', 'pc+2')),
    'pop <rp2>': (('m1p', 'pc'), ('m1', 'pc'), ('r', 'sp'),
                  ('r', 'sp+1')),
    'push <rp2>': (('m1p', 'pc'), ('m15', 'pc'), ('w', 'sp-1'),
                   ('w', 'sp-2')),
    'jp nn': (('m1p', 'pc'), ('m1', 'pc'), ('r', 'pc+1'),
              ('r', 'pc+2')),
    'ret': (('m1p', 'pc'), ('m1', 'pc'), ('r', 'sp'),
            ('r', 'sp+1')),
    'rst n': (('m1p', 'pc'), ('m15', 'pc'), ('w', 'sp-1'),
              ('w', 'sp-2')),
}
for _m in ('add', 'adc', 'sub', 'sbc', 'and', 'xor', 'or', 'cp'):
    BUS_SPECS[f'{_m} {{b, c, d, e, h, l, a}}'] = (('m1p', 'pc'),
                                                  ('m1', 'pc'))
    BUS_SPECS[f'{_m} (hl)'] = (('m1p', 'pc'), ('m1', 'pc'),
                               ('rip', 'pc+1'), ('e5p', '-'),
                               ('r', 'hl'))
    BUS_SPECS[f'{_m} n'] = (('m1p', 'pc'), ('m1', 'pc'),
                            ('r', 'pc+1'))
BUS_SPECS.update({
    'inc/dec (hl)': (('m1p', 'pc'), ('m1', 'pc'),
                     ('rip', 'pc+1'), ('e5p', '-'),
                     ('r4', 'hl'), ('w', 'hl')),
    'inc/dec <rp>': (('m1p', 'pc'), ('m16', 'pc')),
    'ex (sp), hl': (('m1p', 'pc'), ('m1', 'pc'), ('r', 'sp'),
                    ('r4', 'sp+1'), ('w', 'sp+1'), ('w5', 'sp')),
    'ldi/ldd': (('m1p', 'pc'), ('m1', 'pc'), ('m1', 'pc+1'), ('r', 'hl'),
                ('w5', 'de')),
    'add hl, <rp>': (('m1p', 'pc'), ('m1i', 'pc'), ('e4', '-'),
                     ('e3end', '-')),
    'adc/sbc hl, <rp>': (('m1p', 'pc'), ('m1', 'pc'),
                         ('m1i', 'pc+1'), ('e4', '-'),
                         ('e3end', '-')),
    'neg/xneg': (('m1p', 'pc'), ('m1', 'pc'), ('m1', 'pc+1')),
    'im/xim n': (('m1p', 'pc'), ('m1', 'pc'), ('m1', 'pc+1')),
    'xnop': (('m1p', 'pc'), ('m1', 'pc'), ('m1', 'pc+1')),
    'ld {i, r}, a/ld a, {i, r}': (('m1p', 'pc'), ('m1', 'pc'),
                                  ('m15', 'pc+1')),
    'in r, (c)': (('m1p', 'pc'), ('m1', 'pc'), ('m1', 'pc+1'),
                  ('io_r', 'bc')),
    'out (c), r': (('m1p', 'pc'), ('m1', 'pc'), ('m1', 'pc+1'),
                   ('io_w', 'bc')),
    'in a, (n)/out (n), a': (('m1p', 'pc'), ('m1', 'pc'),
                             ('r', 'pc+1'), ('io_v', 'a:n')),
    'rrd/rld': (('m1p', 'pc'), ('m1', 'pc'), ('m1', 'pc+1'),
                ('ri', 'hl'), ('e4', '-'), ('w', 'hl')),
    'jr d': (('m1p', 'pc'), ('m1', 'pc'), ('ri', 'pc+1'),
             ('e5end', '-')),
    'call nn': (('m1p', 'pc'), ('m1', 'pc'), ('r', 'pc+1'),
                ('r4', 'pc+2'), ('w', 'sp-1'), ('w', 'sp-2')),
    'reti/retn/xretn': (('m1p', 'pc'), ('m1', 'pc'),
                        ('m1', 'pc+1'), ('r', 'sp'),
                        ('r', 'sp+1')),
    'ld a, (nn)/ld (nn), a': (('m1p', 'pc'), ('m1', 'pc'),
                              ('r', 'pc+1'), ('r', 'pc+2'),
                              ('rw_v', 'nn')),
    'ld hl, (nn)/ld (nn), hl': (('m1p', 'pc'), ('m1', 'pc'),
                                ('r', 'pc+1'), ('r', 'pc+2'),
                                ('rw_v', 'nn'), ('rw_v', 'nn+1')),
    'ld <rp>, (nn)/ld (nn), <rp>': (
        ('m1p', 'pc'), ('m1', 'pc'), ('m1', 'pc+1'),
        ('r', 'pc+2'), ('r', 'pc+3'), ('rw_v', 'nn2'),
        ('rw_v', 'nn2+1')),
    'ld (<rp>), a/ld a, (<rp>)': (('m1p', 'pc'), ('m1', 'pc'),
                                  ('rw_v', 'bc/de')),
})

# The store condition of the rw_v families, per family.
_BUS_RW_STORE = {
    'ld a, (nn)/ld (nn), a': 'is_store',
    'ld hl, (nn)/ld (nn), hl': 'is_store',
    'ld (<rp>), a/ld a, (<rp>)': 'is_store',
    'ld <rp>, (nn)/ld (nn), <rp>': '~q',
}

BUS_SPECS.update({
    'jp cc, nn': (('m1p', 'pc'), ('m1', 'pc'), ('r', 'pc+1'),
                  ('r', 'pc+2')),
    'djnz d': (('m1p', 'pc'), ('m15', 'pc'), ('r', 'pc+1'),
               ('e5end', '-')),
    'jr cc, d': (('m1p', 'pc'), ('m1', 'pc'), ('r', 'pc+1'),
                 ('e5end', '-')),
    'ret cc': (('m1p', 'pc'), ('m15', 'pc'), ('r', 'sp'),
               ('r', 'sp+1')),
    'call cc, nn': (('m1p', 'pc'), ('m1', 'pc'), ('r', 'pc+1'),
                    ('r', 'pc+2'), ('e1', '-'), ('w', 'sp-1'),
                    ('w', 'sp-2')),
    'cpi/cpd': (('m1p', 'pc'), ('m1', 'pc'), ('m1', 'pc+1'), ('ri', 'hl'),
                ('e5end', '-')),
    'cpir/cpdr': (('m1p', 'pc'), ('m1', 'pc'), ('m1', 'pc+1'), ('ri', 'hl'),
                  ('e5end', '-'), ('e5end', '-')),
    'ldir/lddr': (('m1p', 'pc'), ('m1', 'pc'), ('m1', 'pc+1'), ('r', 'hl'),
                  ('w5', 'de'), ('e5end', '-')),
    'ini/ind': (('m1p', 'pc'), ('m1', 'pc'), ('m15', 'pc+1'), ('io_r', 'bc'),
                ('w', 'hl')),
    'inir/indr': (('m1p', 'pc'), ('m1', 'pc'), ('m15', 'pc+1'), ('io_r', 'bc'),
                  ('w', 'hl'), ('e5end', '-')),
    # The out group decrements B before the port write, so
    # its port address carries the decremented value.
    'outi/outd': (('m1p', 'pc'), ('m1', 'pc'), ('m15', 'pc+1'), ('r', 'hl'),
                  ('io_w', 'b-1:c')),
    'otir/otdr': (('m1p', 'pc'), ('m1', 'pc'), ('m15', 'pc+1'), ('r', 'hl'),
                  ('io_w', 'b-1:c'), ('e5end', '-')),
})

# The CB families: under the prefix the opcode byte follows
# the displacement as a plain read -- ~m1 stays high through
# it -- and the memory operand cycles go to the indexed
# address. The register forms' memory cycles exist only under
# the prefix; their conditions come from the cycle lists.
_BUS_CB_COMMON = (('m1p', 'pc'), ('m1', 'pc'), ('m1n', 'pc+1'),
                  ('rp', 'pc+1'), ('ron', 'pc+1'), ('e2p', '-'))
for _m in ('rot', 'bit', 'res', 'set'):
    _tail = (('r4', 'hl'),)
    if _m != 'bit':
        _tail += ('w', 'hl'),
    BUS_SPECS[f'{_m} {{b, c, d, e, h, l, a}}'] = (
        _BUS_CB_COMMON + _tail)
BUS_SPECS['bit (hl)'] = _BUS_CB_COMMON + (('r4', 'hl'),)
for _m in ('rot', 'res', 'set'):
    BUS_SPECS[f'{_m} (hl)'] = _BUS_CB_COMMON + (('r4', 'hl'),
                                                ('w', 'hl'))

# The conditional families' two next-fetch addresses: the
# taken branch's and the fall-through one's.
_BUS_COND_NEXTS = {
    'djnz d': ('jr-target', 'pc+2'),
    'jr cc, d': ('jr-target', 'pc+2'),
    'ret cc': ('retcc-target', 'pc+1'),
    'call cc, nn': ('call-target', 'pc+3'),
    'ldir/lddr': ('pc', 'pc+2'),
    'cpir/cpdr': ('pc', 'pc+2'),
    'inir/indr': ('pc', 'pc+2'),
    'otir/otdr': ('pc', 'pc+2'),
}


def test_bus_timeline(instr, base_state, all_orders=False):
    # Replays the instruction's settlings -- the steps are
    # identical to the tested run's, so everything comes from
    # the cache -- sampling the address bus and the control
    # pins at every half-tick against the measured templates
    # and the family's cycle addresses.
    spec = BUS_SPECS[instr]
    raw = base_state.get_node_states(SAMPLED_NODES)
    eff = get_effective_states(base_state, SAMPLED_NODES,
                               all_orders=all_orders)

    pc = Bits.concat(sampled_bits(raw, 'reg_pch'),
                     sampled_bits(raw, 'reg_pcl'))
    sp = Bits.concat(sampled_bits(raw, 'reg_sph'),
                     sampled_bits(raw, 'reg_spl'))
    ir = Bits.concat(sampled_bits(raw, 'reg_i'),
                     sampled_bits(raw, 'reg_r'))

    def ir_plus(k):
        lo = (Bits(ir.bits[0:7]) + k).truncated(7)
        return Bits(lo.bits + (ir[7],) + ir.bits[8:16])

    def ir_plus_b(k):
        lo = (Bits(ir.bits[0:7]) + k).truncated(7)
        return Bits(lo.bits + (ir[7],) + ir.bits[8:16])

    family_prefixed = spec[0][0] == 'm1p'
    pref_b = Bits.ifelse(
        bools.get('is_prefixed_p1') if family_prefixed else FALSE,
        1, 0)

    def resolve_at(cyc_index, a):
        v = resolve(a)
        if v is not None and a.startswith('pc'):
            # Every conditionally present byte before this
            # cycle -- the prefix, the displacement -- shifts a
            # pc-relative address by one where it is there.
            for k, _ in spec[:cyc_index]:
                if k in _BUS_SHIFT_KINDS:
                    v = (v + pref_b).truncated(16)
        return v

    exx = PossibleValues.cast(eff[EXX_FF]).single_value
    ex_de = PossibleValues.cast(eff[EX_DE_FF]).single_value

    def bank(r):
        return Bits.ifelse(exx, sampled_bits(eff, f'reg_{r}'),
                           sampled_bits(eff, f'reg_{r}{r}'))

    hl = Bits.concat(Bits.ifelse(ex_de, bank('d'), bank('h')),
                     Bits.ifelse(ex_de, bank('e'), bank('l')))
    de = Bits.concat(Bits.ifelse(ex_de, bank('h'), bank('d')),
                     Bits.ifelse(ex_de, bank('l'), bank('e')))
    ex_af = PossibleValues.cast(eff[EX_AF_FF]).single_value
    a_reg = Bits.ifelse(ex_af, sampled_bits(eff, 'reg_a'),
                        sampled_bits(eff, 'reg_aa'))

    def phased(id):
        return f'{id}_p1'

    def resolve(a):
        if a.startswith('pc'):
            off = int(a[2:]) if len(a) > 2 else 0
            return (pc + off).truncated(16)
        if a.startswith('sp'):
            off = int(a[2:]) if len(a) > 2 else 0
            return (sp + (0x10000 + off)).truncated(16)
        if a == 'hl':
            if instr in PREFIX_HL_REDIRECT + PREFIX_DISP:
                ix = Bits.concat(sampled_bits(eff, 'reg_ixh'),
                                 sampled_bits(eff, 'reg_ixl'))
                iy = Bits.concat(sampled_bits(eff, 'reg_iyh'),
                                 sampled_bits(eff, 'reg_iyl'))
                ixy = Bits.ifelse(bools.get('is_iy_p1'), iy, ix)
                if instr in PREFIX_DISP:
                    d8 = Bits(phased('d'), width=8)
                    ixy = (ixy +
                           d8.sign_extended(16)).truncated(16)
                return Bits.ifelse(bools.get('is_prefixed_p1'),
                                   ixy, hl)
            return hl
        if a == 'de':
            return de
        if a == 'bc':
            return Bits.concat(bank('b'), bank('c'))
        if a == 'b-1:c':
            return Bits.concat((bank('b') - 1).truncated(8),
                               bank('c'))
        if a == 'a:n':
            return Bits.concat(a_reg, Bits(phased('r'), width=8))
        if a.startswith('nn2'):
            v = Bits.concat(Bits(phased('r4'), width=8),
                            Bits(phased('r3'), width=8))
            return (v + 1).truncated(16) if a.endswith('+1') else v
        if a.startswith('nn'):
            v = Bits.concat(Bits(phased('r2'), width=8),
                            Bits(phased('r1'), width=8))
            return (v + 1).truncated(16) if a.endswith('+1') else v
        if a == 'bc/de':
            main = cycles[1 if family_prefixed else 0][0]
            return Bits.ifelse(main[4], de,
                               Bits.concat(bank('b'), bank('c')))
        if a in ('call-target', 'retcc-target'):
            return Bits.concat(Bits(phased('r2'), width=8),
                               Bits(phased('r1'), width=8))
        if a == 'jpcc-next':
            t = Bits.concat(Bits(phased('hi'), width=8),
                            Bits(phased('lo'), width=8))
            cc = get_cond_as_expr('cc', 1, eff)
            return Bits.ifelse(cc, t,
                               (pc + 3 + pref_b).truncated(16))
        if a == 'reti-target':
            return Bits.concat(Bits(phased('r4'), width=8),
                               Bits(phased('r3'), width=8))
        if a == 'jr-target':
            d = Bits(phased('r'), width=8)
            return (pc + 2 + pref_b +
                    d.sign_extended(16)).truncated(16)
        if a == '-':
            return None
        if a == 'jp-target':
            return Bits.concat(Bits(phased('hi'), width=8),
                               Bits(phased('lo'), width=8))
        if a == 'ret-target':
            return Bits.concat(Bits(phased('hi'), width=8),
                               Bits(phased('lo'), width=8))
        if a == 'rst-target':
            t = Bits(phased('y'), width=3) << 3
            return t.zero_extended(16)
        assert 0, a

    NEXT_SPECIAL = {'jp hl': 'hl', 'jp nn': 'jp-target',
                    'ret': 'ret-target', 'rst n': 'rst-target',
                    'halt': 'pc+1', 'jr d': 'jr-target',
                    'call nn': 'call-target',
                    'reti/retn/xretn': 'reti-target',
                    'jp cc, nn': 'jpcc-next'}
    if instr in NEXT_SPECIAL:
        # The special resolvers shift themselves where their
        # targets are pc-relative; absolute targets do not
        # shift.
        # Resolving at the instruction's end shifts a
        # pc-relative special next -- halt's frozen fetch
        # address -- past the conditional bytes; the other
        # special resolvers shift themselves where needed.
        instr_next = resolve_at(len(spec), NEXT_SPECIAL[instr])
    else:
        # Count the unprefixed encoding's bytes: the shift
        # kinds are the prefix-only extras added below, and
        # 'ron' re-reads a byte its 'm1n' sibling already
        # counts.
        length = 1 + sum(1 for k, a in spec
                         if a.startswith('pc+') and
                         k not in _BUS_SHIFT_KINDS and
                         k != 'ron')
        instr_next = (pc + length).truncated(16)
        for k, a in spec:
            if k in _BUS_SHIFT_KINDS:
                instr_next = (instr_next + pref_b).truncated(16)
    if instr in _BUS_COND_NEXTS:
        tk_spec, nt_spec = _BUS_COND_NEXTS[instr]
        taken_next = resolve_at(1, tk_spec)
        nottaken_next = resolve_at(1, nt_spec)
    else:
        taken_next = nottaken_next = instr_next

    cycles = TestedInstrs.get_cycles(instr, 1)
    assert len(cycles) == len(spec), instr

    AB = tuple(f'ab{i}' for i in range(16))
    CTL = '~m1', '~mreq', '~rd', '~wr', '~iorq', '~rfsh'

    s = State(base_state)
    m1_index = Bits.cast(0, width=3)
    refresh = None
    held = None
    for cyc, ((d, ticks, cond), (kind, addr)) in enumerate(
            zip(cycles, spec)):
        under = (get_cond_as_expr(cond, 1, eff)
                 if cond is not None else None)
        if kind == 'io_v':
            is_in = bools.get(phased('is_in'))
            template = ((('cur', 1, 1, 1, 1, 1, 1),) +
                        (('cur', 1, 1, ~is_in, is_in, 0, 1),) * 5 +
                        (('cur', 1, 1, 1, 1, 1, 1),) +
                        (('next', None, 1, 1, 1, 1, 1),))
        elif kind == 'rw_v':
            t = _BUS_RW_STORE[instr]
            if t.startswith('~'):
                store = ~bools.get(phased(t[1:]) + '_b0')
            else:
                store = bools.get(phased(t))
            template = ((('cur', 1, 0, store, 1, 1, 1),) * 2 +
                        (('cur', 1, 0, store, ~store, 1, 1),) * 2 +
                        (('cur', 1, 1, 1, 1, 1, 1),) +
                        (('next', None, 1, 1, 1, 1, 1),))
        else:
            template = _BUS_TEMPLATES[kind]
        assert len(template) == ticks * 2, instr
        cur = resolve_at(cyc, addr)
        if kind in _BUS_FETCH_KINDS:
            refresh = ir_plus_b(m1_index)
            if kind == 'm1p':
                w = pref_b
            elif kind == 'm1n':
                w = Bits.ifelse(
                    bools.get('is_prefixed_p1'), 0, 1)
            else:
                w = 1
            m1_index = (m1_index + w).truncated(3)
        # Internal cycles keep the last fetch's parked refresh
        # address, so refresh persists across them.
        if cyc + 1 < len(cycles):
            # The handover target: fold over the following
            # cycles -- a conditional cycle is entered only
            # where its condition holds, the first applicable
            # one wins, and falling past the end reaches the
            # next instruction's fetch.
            arms = []
            for j in range(cyc + 1, len(cycles)):
                tk = resolve_at(j, spec[j][1])
                if tk is None:
                    tk = cur if cur is not None else held
                m1v = (FALSE if spec[j][0] in _BUS_FETCH_KINDS
                       else TRUE)
                arms.append((cycles[j][2], tk, m1v))
                if cycles[j][2] is None:
                    break
            nxt, next_m1 = nottaken_next, FALSE
            for jcond, tk, m1v in reversed(arms):
                if jcond is None:
                    nxt, next_m1 = tk, m1v
                else:
                    c = get_cond_as_expr(jcond, 1, eff)
                    nxt = Bits.ifelse(c, tk, nxt)
                    next_m1 = bools.ifelse(c, m1v, next_m1)
        else:
            nxt = taken_next
            next_m1 = FALSE
        s.set_db(d, all_orders)
        for t in range(ticks):
            for ht in (0, 1):
                with s.status(f'bus tick {cyc}:{t}.{ht}'):
                    s.half_tick(cond=under,
                                all_orders=all_orders)
                s.cache()
                role, m1, mreq, rd, wr, iorq, rfsh = (
                    template[t * 2 + ht])
                a = {'cur': cur, 'ir': refresh,
                     'next': nxt, 'hold': held}[role]
                if m1 is None:
                    m1 = next_m1
                pins = dict(zip(CTL, (m1, mreq, rd, wr, iorq,
                                      rfsh)))
                expected = {}
                for j, p in enumerate(AB):
                    expected[p] = a[j]
                for p, v in pins.items():
                    if isinstance(v, int):
                        v = TRUE if v else FALSE
                    expected[p] = v
                states = s.get_node_states(AB + CTL)
                check_sampled_states(
                    f'{instr} bus {cyc}:{t}.{ht}', states,
                    lambda x: expected[x], under=under)
                if (instr == 'out (c), r' and kind == 'io_w'
                        and isinstance(wr, int) and not wr):
                    # The byte the die drives during the port
                    # write: the named register, a for field 7
                    # -- and zero for field 6, the NMOS answer
                    # to the out (c), 0 question.
                    y = Bits(phased('y'), width=3)

                    def arch(letter):
                        # The architectural register, routed
                        # through the DE/HL exchange.
                        partner = {'d': 'h', 'h': 'd',
                                   'e': 'l', 'l': 'e'}
                        if letter in partner:
                            return Bits.ifelse(
                                ex_de,
                                bank(partner[letter]),
                                bank(letter))
                        return bank(letter)

                    v = Bits.ifelse(
                        y[2],
                        Bits.ifelse(
                            y[1],
                            Bits.ifelse(y[0], a_reg,
                                        Bits.cast(0, width=8)),
                            Bits.ifelse(y[0], arch('l'),
                                        arch('h'))),
                        Bits.ifelse(
                            y[1],
                            Bits.ifelse(y[0], arch('e'),
                                        arch('d')),
                            Bits.ifelse(y[0], arch('c'),
                                        arch('b'))))
                    DB = tuple(f'db{i}' for i in range(8))
                    dstates = s.get_node_states(DB)
                    check_sampled_states(
                        f'{instr} data {cyc}:{t}.{ht}',
                        dstates, lambda x: v[int(x[2:])],
                        under=under)
        # What the bus keeps into a following internal cycle:
        # the last role this cycle drove.
        role = template[-1][0]
        held = {'cur': cur, 'ir': refresh, 'next': nxt,
                'hold': held}[role]


def test_instr_seq(seq, all_orders=False):
    # bools.clear()
    # gc.collect()

    assert FALSE is bools.get(False)
    assert TRUE is bools.get(True)

    try:
        state = build_symbolised_state()
        with Status.do('; '.join(seq)):
            for i in range(len(seq) - 1):
                state = process_instr(seq[:i + 1], state)
            process_instr(seq, state, test=True, all_orders=all_orders)
            if len(seq) == 1 and seq[0] in BUS_SPECS:
                test_bus_timeline(seq[0], state, all_orders)
            if seq == ('ld <rp>, nn',):
                test_prefix_chains(state, all_orders)
            if seq == ('nop',):
                test_wait_states(state, all_orders)
                test_bus_grant(state, all_orders)
                test_special_reset(state, all_orders)
                test_int_sampling_instant(state, all_orders)
            if seq == ('halt',):
                halted = process_instr(seq, state, all_orders=all_orders)
                test_halted_dynamics(halted, all_orders)
                test_nmi_from_halt(halted, all_orders)
                test_masked_int_in_halt(halted, all_orders)
                test_int_im1_from_halt(state, all_orders)
                test_int_im2_from_halt(state, all_orders)
                test_int_im0_from_halt(state, all_orders)
                test_reset_from_halt(halted, all_orders)
                test_ei_halt_atomicity(state, all_orders)
                test_int_at_halt_m1(state, all_orders)
                test_int_at_boundary(state, all_orders)
                test_int_between_ldir_repeats(state, all_orders)
                test_int_at_ld_a_ir(state, all_orders)
                test_nmi_over_int_priority(state, all_orders)
                test_int_at_prefix_gap(state, all_orders)
                test_nmi_at_prefix_gap(state, all_orders)
                test_nmi_at_prefix_chain(state, all_orders)
        return True, '; '.join(seq)
    except TestFailure:
        return False, traceback.format_exc()


def test_instr_seq_concurrently(args):
    i, seq, all_orders = args
    with Status.suppress():
        try:
            return i, test_instr_seq(seq, all_orders)
        except MemoryCapReached:
            return i, None
        except Exception:
            return i, traceback.format_exc()


def get_instr_seq_id(seq):
    return '; '.join(seq)


def get_instr_seq_cache_entry(domain, seq, all_orders):
    assert domain in ('passed', 'failed')
    key = get_instr_seq_id(seq)
    if all_orders:
        key = key, 'all-orders'
    return Cache.get_entry(domain, key)


def get_instr_seq_time_stamp(domain, seq, all_orders):
    t = get_instr_seq_cache_entry(domain, seq, all_orders).load()
    if t is None:
        t = 0
    else:
        t, = t
    return t


def test_instr_seqs(seqs, all_orders=False):
    # A longer sequence depends on its shorter prefix for the
    # cached states it starts from, so sequences run in order
    # of length, and a threaded run completes each length
    # before starting the next. Records of previous passes and
    # failures never affect the order.
    def sort_key(seq):
        return len(seq), get_instr_seq_id(seq)

    def mark(ok, seq):
        domain = 'passed' if ok else 'failed'
        now = datetime.datetime.timestamp(datetime.datetime.now())
        get_instr_seq_cache_entry(domain, seq, all_orders).store((now,))

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
    if THREADS is None:
        for i, seq in enumerate(seqs):
            ok &= process_results(i, seqs[i],
                                  test_instr_seq(seq, all_orders))
    else:
        seqs = tuple(seqs)
        num_threads = THREADS
        # One instruction sequence per worker process: the
        # expression contexts only ever grow -- hash-consing
        # never frees -- so a worker serving several sequences
        # accumulates every one of them. A fresh process returns
        # the memory wholesale and reloads from the shared cache
        # in seconds, keeping the peak at the number of workers
        # times one sequence's worth. Sequences whose workers
        # hit the memory cap get requeued and resume from their
        # cached states.
        for length in sorted({len(s) for s in seqs}):
            pending = tuple((i, seq)
                            for i, seq in enumerate(seqs)
                            if len(seq) == length)
            while pending:
                retries = []
                with multiprocessing.Pool(num_threads,
                                          maxtasksperchild=1) as p:
                    queue = p.imap_unordered(
                        test_instr_seq_concurrently,
                        ((i, seq, all_orders)
                         for i, seq in pending))
                    for i, res in queue:
                        if res is None:
                            Status.print(
                                f'{i + 1}/{len(seqs)} '
                                f'{get_instr_seq_id(seqs[i])} '
                                f'to resume after hitting the '
                                f'memory cap')
                            retries.append((i, seqs[i]))
                            continue
                        ok &= process_results(i, seqs[i], res)
                pending = tuple(retries)

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

        def f(p, *, ticks=4, cond=None):
            # Variable names must be phased, so don't allow them here.
            assert not isinstance(p, str)
            return Bits.cast(p), ticks, cond

        def fp():
            # The conditional DD/FD prefix fetch: the prefix is
            # a modifier on the instruction that follows, not an
            # instruction of its own, so it joins a family's
            # cycles guarded on whether it is there at all, with
            # the IX/IY choice symbolic.
            return f(ifelse('is_iy', 0xfd, 0xdd),
                     cond='is_prefixed')

        def f5(p):
            return f(p, ticks=5)

        def f6(p):
            return f(p, ticks=6)

        def r3(p='r', cond=None):
            return phased(p), 3, cond

        def r4(p='r', cond=None):
            return phased(p), 4, cond

        def w3(p='w', cond=None):
            return phased(p), 3, cond

        def w5(p='w'):
            return phased(p), 5, None

        def rw3(p='rw'):
            return phased(p), 3, None

        def e2(p='e', *, cond=None):
            return phased(p), 2, cond

        def e3(p='e'):
            return phased(p), 3, None

        def e4(p='e'):
            return phased(p), 4, None

        def e5(p='e', *, cond=None):
            return phased(p), 5, cond

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

        yield 'nop', (fp(), f(0x00))
        yield "ex af, af'", (fp(), f(0x08))
        yield 'jr d', (fp(), f(0x18), r3(), e5())
        yield 'daa', (fp(), f(0x27))
        yield 'cpl', (fp(), f(0x2f))
        yield 'scf/ccf', (fp(), f(ifelse('is_scf', 0x37, 0x3f)))
        yield 'exx', (fp(), f(0xd9))
        yield 'jp hl', (fp(), f(0xe9))
        yield 'ex de, hl', (fp(), f(0xeb))
        yield 'ld sp, hl', (fp(), f6(0xf9))
        yield 'ei/di', (fp(), f(ifelse('is_ei', 0xfb, 0xf3)))

        yield 'ld {b, c, d, e, h, l, a}, {b, c, d, e, h, l, a}', (
            fp(),
            f(xyz(1, get_non_at_hl_r('regd'), get_non_at_hl_r('regs'))))
        yield 'ld {b, c, d, e, h, l, a}, (hl)', (
            fp(), f(xyz(1, get_non_at_hl_r('regd'), AT_HL)),
            r3('d', cond='is_prefixed'), e5(cond='is_prefixed'),
            r3())
        yield 'ld (hl), {b, c, d, e, h, l, a}', (
            fp(), f(xyz(1, AT_HL, get_non_at_hl_r('regs'))),
            r3('d', cond='is_prefixed'), e5(cond='is_prefixed'),
            w3())

        yield 'halt', (
            fp(), f(xyz(1, AT_HL, AT_HL)))

        yield 'ld {b, c, d, e, h, l, a}, n', (
            fp(), f(xyz(0, get_non_at_hl_r(), 6)), r3())
        yield 'ld (hl), n', (
            fp(), f(xyz(0, AT_HL, 6)),
            r3('d', cond='is_prefixed'), r3(),
            e2(cond='is_prefixed'), w3())

        ADD, ADC, SUB, SBC, AND, XOR, OR, CP = range(8)
        for op, m in (
                (ADD, 'add'), (ADC, 'adc'), (SUB, 'sub'), (SBC, 'sbc'),
                (AND, 'and'), (XOR, 'xor'), (OR, 'or'), (CP, 'cp')):
            yield f'{m} {{b, c, d, e, h, l, a}}', (
                fp(), f(xyz(2, op, get_non_at_hl_r())))
            yield f'{m} (hl)', (
                fp(), f(xyz(2, op, AT_HL)),
                r3('d', cond='is_prefixed'),
                e5(cond='is_prefixed'), r3())
            yield f'{m} n', (fp(), f(xyz(3, op, 6)), r3())

        yield 'ld hl, (nn)/ld (nn), hl', (
            fp(), f(ifelse('is_store', 0x22, 0x2a)),
            r3('r1'), r3('r2'), rw3('rw3'), rw3('rw4'))
        yield 'ld a, (nn)/ld (nn), a', (
            fp(), f(ifelse('is_store', 0x32, 0x3a)),
            r3('r1'), r3('r2'), rw3('rw3'))

        yield 'rst n', (fp(), f5(xyz(3, 'y', 7)), w3('w1'),
                        w3('w2'))

        yield 'rlca/rrca/rla/rra', (
            fp(), f(ifelse('is_rlca_rrca',
                           ifelse('is_rl', 0x07, 0x0f),
                           ifelse('is_rl', 0x17, 0x1f))))

        yield 'inc/dec {b, c, d, e, h, l, a}', (
            fp(),
            f(xyz(0, get_non_at_hl_r(), ifelse('is_inc', 4, 5))))
        yield 'inc/dec (hl)', (
            fp(), f(xyz(0, AT_HL, ifelse('is_inc', 4, 5))),
            r3('d', cond='is_prefixed'), e5(cond='is_prefixed'),
            r4(), w3())

        yield 'ld <rp>, nn', (fp(), f(xpqz(0, 'rp', 0, 1)),
                              r3('r1'), r3('r2'))
        yield 'add hl, <rp>', (fp(), f(xpqz(0, 'rp', 1, 1)),
                               e4('e1'), e3('e2'))
        yield 'inc/dec <rp>', (
            fp(), f6(xpqz(0, 'rp', ifelse('is_inc', 0, 1), 3)))
        yield 'pop <rp2>', (fp(), f(xpqz(3, 'rp2', 0, 1)),
                            r3('lo'), r3('hi'))
        yield 'push <rp2>', (fp(), f5(xpqz(3, 'rp2', 0, 5)),
                             w3('lo'), w3('hi'))

        pp = Bits.concat(Bits(0, width=1), Bits('p', width=1))
        yield 'ld (<rp>), a/ld a, (<rp>)', (
            fp(),
            f(xpqz(0, pp, Bits((~bools.get(phased('is_store')),)), 2)),
            rw3())

        yield 'jp nn', (fp(), f(0xc3), r3('lo'), r3('hi'))
        yield 'ret', (fp(), f(0xc9), r3('lo'), r3('hi'))
        yield 'call nn', (fp(), f(0xcd), r3('r1'), r4('r2'),
                          w3('w3'), w3('w4'))
        yield 'ex (sp), hl', (
            fp(), f(0xe3), r3('r1'), r4('r2'), w3('w3'), w5('w4'))

        yield 'in a, (n)/out (n), a', (
            fp(), f(ifelse('is_in', 0xdb, 0xd3)), r3(), io4())

        yield 'djnz d', (
            fp(), f5(0x10), r3(), e5(cond='b != 1'))
        yield 'jr cc, d', (
            fp(), f(xyz(0, Bits(phased('cc2'), width=2) | 0b100, 0)),
            r3(), e5(cond='cc2'))
        yield 'ret cc', (
            fp(), f5(xyz(3, 'cc', 0)), r3('r1', cond='cc'),
            r3('r2', cond='cc'))
        yield 'jp cc, nn', (
            fp(), f(xyz(3, 'cc', 2)), r3('lo'), r3('hi'))
        # The high-operand read runs a fourth tick when the call
        # is taken; it is modelled as a separate one-tick
        # conditional cycle.
        yield 'call cc, nn', (
            fp(), f(xyz(3, 'cc', 4)), r3('r1'), r3('r2'),
            (phased('e'), 1, 'cc'),
            w3('w3', cond='cc'), w3('w4', cond='cc'))

        yield 'ld {i, r}, a/ld a, {i, r}', (
            fp(), f(0xed),
            f5(ifelse('is_i_reg',
                      ifelse('is_store', 0x47, 0x57),
                      ifelse('is_store', 0x4f, 0x5f))))
        yield 'rrd/rld', (
            fp(), f(0xed), f(ifelse('is_rrd', 0x67, 0x6f)), r3(),
            e4(), w3())
        yield 'ldi/ldd', (
            fp(), f(0xed), f(ifelse('is_ldd', 0xa8, 0xa0)), r3(), w5())
        # The repeat cycle runs only when the decremented BC is
        # not zero, that is, when BC was not 1 -- one symbolic
        # run covers both branches for all BC values.
        yield 'ldir/lddr', (
            fp(), f(0xed), f(ifelse('is_ldd', 0xb8, 0xb0)), r3(), w5(),
            e5(cond='bc != 1'))
        yield 'cpi/cpd', (
            fp(), f(0xed), f(ifelse('is_cpd', 0xa9, 0xa1)), r3(), e5())
        # The repeat cycle runs only when the decremented BC is
        # not zero and the comparison did not match -- one
        # symbolic run covers both branches.
        yield 'cpir/cpdr', (
            fp(), f(0xed), f(ifelse('is_cpd', 0xb9, 0xb1)), r3(), e5(),
            e5('e2', cond='bc != 1 && a != r'))
        yield 'ini/ind', (
            fp(), f(0xed), f5(ifelse('is_ind', 0xaa, 0xa2)), io4(), w3())
        # The repeat cycle runs only when the decremented B is
        # not zero, that is, when B was not 1 -- one symbolic
        # run covers both branches for all B values.
        yield 'inir/indr', (
            fp(), f(0xed), f5(ifelse('is_ind', 0xba, 0xb2)), io4(), w3(),
            e5(cond='b != 1'))
        yield 'outi/outd', (
            fp(), f(0xed), f5(ifelse('is_outd', 0xab, 0xa3)), r3(), io4())
        yield 'otir/otdr', (
            fp(), f(0xed), f5(ifelse('is_outd', 0xbb, 0xb3)), r3(), io4(),
            e5(cond='b != 1'))
        yield 'in r, (c)', (
            fp(), f(0xed), f(xyz(1, 'y', 0)), io4())
        yield 'out (c), r', (
            fp(), f(0xed), f(xyz(1, 'y', 1)), io4())
        yield 'adc/sbc hl, <rp>', (
            fp(), f(0xed), f(xpqz(1, 'p', 'q', 2)), e4(), e3())
        yield 'ld <rp>, (nn)/ld (nn), <rp>', (
            fp(), f(0xed), f(xpqz(1, 'p', 'q', 3)),
            r3('r3'), r3('r4'), rw3('rw5'), rw3('rw6'))
        yield 'neg/xneg', (fp(), f(0xed), f(xyz(1, 'y', 4)))
        yield 'reti/retn/xretn', (
            fp(), f(0xed), f(xyz(1, 'y', 5)), r3('r3'), r3('r4'))
        yield 'im/xim n', (fp(), f(0xed), f(xyz(1, 'y', 6)))

        xnop_00 = pattern('00 xxx xxx')
        xnop_01 = pattern('01 11x 111')
        xnop_10 = ifelse('is_100', pattern('10 0xx xxx'),
                         pattern('10 1xx 1xx'))
        xnop_11 = pattern('11 xxx xxx')
        xnop = ifelse('is_0',
                      ifelse('is_00', xnop_00, xnop_01),
                      ifelse('is_10', xnop_10, xnop_11))
        yield 'xnop', (fp(), f(0xed), f(xnop))

        # Under the DD/FD prefix a CB family's opcode byte is a
        # plain read, not a fetch, and follows the displacement:
        # the conditional fetch carries the unprefixed byte, the
        # conditional read re-feeds the same expression in the
        # prefixed order. The register field then selects the
        # copy destination of the memory result -- the
        # undocumented ld r, rlc (ix+d) forms -- with no copy
        # for bit or field 6.
        ROT, BIT, RES, SET = range(4)

        def cb_cycles(op_b):
            return (fp(), f(0xcb), f(op_b, cond='not_prefixed'),
                    r3('d', cond='is_prefixed'),
                    f(op_b, ticks=3, cond='is_prefixed'),
                    e2(cond='is_prefixed'))

        for x, m in (
                (ROT, 'rot'),
                (BIT, 'bit'),
                (RES, 'res'),
                (SET, 'set')):
            op_b = xyz(x, 'y', get_non_at_hl_r())
            mem = (r4(cond='is_prefixed'),)
            if x != BIT:
                mem += w3(cond='is_prefixed'),
            yield f'{m} {{b, c, d, e, h, l, a}}', (
                cb_cycles(op_b) + mem)

        yield 'bit (hl)', (
            cb_cycles(xyz(BIT, 'y', AT_HL)) + (r4(),))

        for x, m in (
                (ROT, 'rot'),
                (RES, 'res'),
                (SET, 'set')):
            yield f'{m} (hl)', (
                cb_cycles(xyz(x, 'y', AT_HL)) + (r4(), w3()))


def test_instructions():
    # The tested instructions' settlings simulate all orders of
    # updates by default, so a pass means the expected values
    # hold under every order; racing nodes state their outcome
    # sets via check_set(). With --single-order only the default
    # order is simulated, against the expectations describing
    # its deterministic outcomes.
    all_orders = '--single-order' not in sys.argv

    instrs = TestedInstrs.get_instrs()

    # For iterating on a single family: --only=<instr> tests
    # just that sequence, pass records untouched aside.
    only = get_opt('--only', str)
    if only is not None:
        assert only in instrs, only
        instrs = (only,)

    seqs = [(i,) for i in instrs]
    if only is None:
        # The scf/ccf context pairs: the context map's
        # certified sequences.
        seqs += [(i, 'scf/ccf') for i in SCF_CONTEXTS]
        # The flag consumers, proven blind to their
        # predecessor in every context class.
        seqs += list(CONTEXT_SUSPECT_PAIRS)
    if '--two-instr-seqs' in sys.argv:
        seqs += [(i1, i2) for i1 in instrs for i2 in instrs]

    # Options only filter what runs; they never affect the
    # order, which is test_instr_seqs()'s own.
    if '--new-only' in sys.argv:
        seqs = [s for s in seqs
                if get_instr_seq_time_stamp('passed', s,
                                            all_orders) == 0]

    ok = test_instr_seqs(seqs, all_orders)

    Status.clear()
    print('OK' if ok else 'FAILED')


def identify_instr_state_nodes(s, instr, persistent_nodes):
    s = State(s)

    # Symbolise non-persistent nodes.
    for id in s.get_node_states():
        if id not in persistent_nodes:
            # s.set_node_state(id, bools.get(id))
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
        if bools.is_equiv(state, persistent_nodes[id]):
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

    alu_instrs = tuple(
        f'{m} {rest}'
        for m in ('add', 'adc', 'sub', 'sbc', 'and', 'xor', 'or', 'cp')
        for rest in ('(hl)', 'n', '{b, c, d, e, h, l, a}'))
    for instr in ('nop',) + alu_instrs:
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
    test_orders()

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

    if '--probe-acceptance-latch' in sys.argv:
        probe_acceptance_latch(
            all_orders='--single-order' not in sys.argv)
        Status.clear()
        return

    if '--probe-bus-templates' in sys.argv:
        probe_bus_templates(
            all_orders='--single-order' not in sys.argv)
        Status.clear()
        return

    if '--probe-node-ids' in sys.argv:
        probe_node_ids(
            all_orders='--single-order' not in sys.argv)
        Status.clear()
        return

    if '--probe-latch-cells' in sys.argv:
        probe_latch_cells()
        Status.clear()
        return

    if '--probe-closed-world' in sys.argv:
        probe_closed_world(
            all_orders='--single-order' not in sys.argv)
        Status.clear()
        return

    if '--probe-writeback' in sys.argv:
        probe_writeback(
            all_orders='--single-order' not in sys.argv)
        Status.clear()
        return

    if '--probe-special-reset' in sys.argv:
        probe_special_reset(
            all_orders='--single-order' not in sys.argv)
        Status.clear()
        return

    if '--probe-scf-contexts' in sys.argv:
        probe_scf_contexts()
        Status.clear()
        return

    if '--probe-scf-classes' in sys.argv:
        probe_scf_classes()
        Status.clear()
        return

    if '--probe-context-suspects' in sys.argv:
        probe_context_suspects()
        Status.clear()
        return

    if '--probe-context-carriers' in sys.argv:
        probe_context_carriers()
        Status.clear()
        return

    all_orders_instr = get_opt('--all-orders', str)
    if all_orders_instr is not None:
        run_all_orders(all_orders_instr)
        return

    build_symbolic_states()

    test_instructions()


if __name__ == "__main__":
    main()
