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
import hashlib
import pathlib
import pprint
import sys
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


class Bool(object):
    def __init__(self, e):
        if isinstance(e, z3.BoolRef):
            if z3.is_false(e):
                self.__e = False
            elif z3.is_true(e):
                self.__e = True
            else:
                self.__e = e
        elif isinstance(e, bool):
            self.__e = e
        else:
            assert isinstance(e, str)
            self.__e = z3.Bool(e)

    def __repr__(self):
        return (repr(self.__e) if isinstance(self.__e, bool)
                else self.__e.sexpr())

    class __Key(object):
        def __init__(self, n):
            self.n = n

        @staticmethod
        def is_less(a, b):
            if isinstance(a, bool):
                if isinstance(b, bool):
                    return a < b
                return True
            if isinstance(b, bool):
                return False

            if isinstance(a, str):
                if isinstance(b, str):
                    return a < b
                return True
            if isinstance(b, str):
                return False

            for x, y in zip(a, b):
                if __class__.is_less(x, y):
                    return True
                if x != y:
                    return False

            return len(a) < len(b)

        def __lt__(self, other):
            return __class__.is_less(self.n, other.n)

    @staticmethod
    def __get_image(e):
        d = e.decl()
        k = d.kind()
        if k == z3.Z3_OP_FALSE:
            return False
        if k == z3.Z3_OP_TRUE:
            return True
        if k == z3.Z3_OP_UNINTERPRETED:
            return str(d)

        args = [__class__.__get_image(e.arg(i))
                for i in range(e.num_args())]

        # For convenience, enforce a particular order of
        # arguments of commutative operations.
        if k != z3.Z3_OP_ITE:
            args.sort(key=__class__.__Key)

        OPS = {z3.Z3_OP_OR: 'or', z3.Z3_OP_AND: 'and',
               z3.Z3_OP_NOT: 'not', z3.Z3_OP_ITE: 'if'}
        return (OPS[k],) + tuple(args)

    @property
    def image(self):
        return (self.__e if isinstance(self.__e, bool)
                else __class__.__get_image(self.__e))

    @staticmethod
    def __from_image(i):
        if not isinstance(i, tuple):
            if isinstance(i, bool):
                return z3.BoolVal(i)
            assert isinstance(i, str)
            return z3.Bool(i)

        OPS = {'or': z3.Or, 'and': z3.And, 'not': z3.Not, 'if': z3.If}
        return OPS[i[0]](*(__class__.__from_image(a) for a in i[1:]))

    @staticmethod
    def from_image(image):
        if isinstance(image, bool):
            return TRUE if image else FALSE
        return Bool(__class__.__from_image(image))

    @staticmethod
    def boolify(x):
        if isinstance(x, bool):
            return TRUE if x else FALSE
        return x if isinstance(x, Bool) else Bool(x)

    @property
    def value(self):
        return self.__e if isinstance(self.__e, bool) else None

    def is_trivially_false(self):
        return self.__e is False

    def __bool__(self):
        assert isinstance(self.__e, bool)
        return self.__e

    def __int__(self):
        return int(bool(self))

    @property
    def size(self):
        return len(str(self))

    def __eq__(self, other):
        assert 0, "Bool's should not be compared; use is_equiv() instead."

    def __or__(self, other):
        if isinstance(self.__e, bool) and isinstance(other.__e, bool):
            return TRUE if self.__e or other.__e else FALSE
        return Bool(z3.Or(self.__e, other.__e))

    def __and__(self, other):
        if isinstance(self.__e, bool) and isinstance(other.__e, bool):
            return TRUE if self.__e and other.__e else FALSE
        return Bool(z3.And(self.__e, other.__e))

    def __invert__(self):
        if isinstance(self.__e, bool):
            return FALSE if self.__e else TRUE
        return Bool(z3.Not(self.__e))

    @staticmethod
    def ifelse(cond, a, b):
        if isinstance(cond.__e, bool):
            return a if cond.__e else b
        return Bool(z3.If(cond.__e, a.__e, b.__e))

    __equiv0_cache = set()
    __equiv1_cache = set()

    @staticmethod
    def __is_equiv(a, b):
        if isinstance(a, bool):
            if isinstance(b, bool):
                return a == b
            a = z3.BoolVal(a)
        elif isinstance(b, bool):
            b = z3.BoolVal(b)

        key = a.sexpr() + ':' + b.sexpr()
        h = HASH(key.encode()).hexdigest()

        if h in __class__.__equiv0_cache:
            return False
        if h in __class__.__equiv1_cache:
            return True

        path0 = _CACHE_ROOT / 'equiv0' / h[:3] / h[3:6] / h
        if path0.exists():
            __class__.__equiv0_cache.add(h)
            return False
        path1 = _CACHE_ROOT / 'equiv1' / h[:3] / h[3:6] / h
        if path1.exists():
            __class__.__equiv1_cache.add(h)
            return True

        s = z3.Solver()
        s.add(a != b)
        res = s.check()
        assert res in (z3.sat, z3.unsat)
        equiv = (res == z3.unsat)

        cache = __class__.__equiv1_cache if equiv else __class__.__equiv0_cache
        cache.add(h)

        path = path1 if equiv else path0
        path.parent.mkdir(parents=True, exist_ok=True)
        with path.open('w') as f:
            pass

        return equiv

    def is_equiv(self, other):
        return __class__.__is_equiv(self.__e, other.__e)

    def simplified(self):
        if isinstance(self.__e, bool):
            return self

        return Bool(z3.simplify(self.__e))

    def reduced(self):
        if isinstance(self.__e, bool):
            return self

        simplified = self.simplified()
        for c in (FALSE, TRUE):
            if __class__.__is_equiv(simplified.__e, c.__e):
                return c

        return simplified


FALSE = Bool(False)
TRUE = Bool(True)


class Bits(object):
    def __init__(self, bits):
        self.__bits = tuple(bits)

    @property
    def width(self):
        return len(self.__bits)

    @property
    def size_in_bytes(self):
        return _ceil_div(self.width, 8)

    @property
    def value(self):
        if all(b is None for b in self.__bits):
            return None

        values = tuple(b.value for b in self.__bits)
        if all(v is not None for v in values):
            n = 0
            for i, v in enumerate(values):
                n |= int(v) << i
            return n

        assert 0  # TODO

    def __int__(self):
        v = self.value
        assert isinstance(v, int)  # TODO
        return v

    def __repr__(self):
        return repr(self.__bits)

    def __str__(self):
        v = self.value
        if v is None:
            return str(None)

        if isinstance(v, int):
            return '{:0{}x}'.format(v, self.size_in_bytes * 2)

        assert 0  # TODO

    def zero_extended(self, width):
        if self.width >= width:
            return self
        return Bits(self.__bits + (FALSE,) * (width - self.width))

    @staticmethod
    def zero_extend_to_same_width(*args):
        w = max(a.width for a in args)
        return (a.zero_extended(w) for a in args)

    def __lshift__(self, n):
        return Bits((FALSE,) * n + self.__bits)

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
        assert pull is None or isinstance(pull, Bool)
        self.index, self.pull = index, pull
        self.state = state

        # These are not sets as we want reproducible behaviour.
        self.gate_of = []
        self.conn_of = []

    def __repr__(self):
        return self.id

    def __lt__(self, other):
        return self.index < other.index

    @property
    def image(self):
        pull = None if self.pull is None else self.pull.image
        state = None if self.state is None else self.state.image
        return pull, state

    @staticmethod
    def from_image(index, image):
        pull, state = image
        pull = None if pull is None else Bool.from_image(pull)
        state = None if state is None else Bool.from_image(state)
        return Node(index, pull, state=state)

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
    def __init__(self, index, gate, c1, c2, state=None):
        self.index, self.gate, self.c1, self.c2 = index, gate, c1, c2
        assert state is None or isinstance(state, Bool)
        self.state = state

    def __repr__(self):
        return f'{self.c1} = {self.c2} [{self.gate}]  # {self.id}'

    def __lt__(self, other):
        return self.index < other.index

    @property
    def id(self):
        return f't{self.index}'

    @property
    def image(self):
        state = None if self.state is None else self.state.image
        return self.gate.index, self.c1.index, self.c2.index, state

    @staticmethod
    def from_image(index, image, nodes):
        gate, c1, c2, state = image
        gate = nodes[gate]
        c1 = nodes[c1]
        c2 = nodes[c2]
        state = None if state is None else Bool.from_image(state)

        t = Transistor(index, gate, c1, c2, state)
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
    nodes, trans = sorted(nodes), sorted(trans)
    node_names = tuple((n.index, n.custom_id) for n in nodes
                       if n.custom_id is not None)
    nodes = tuple((n.index,) + n.image for n in nodes)
    trans = tuple((t.index,) + t.image for t in trans)
    return node_names, nodes, trans


def _load_initial_image():
    __nodes_by_name = {}
    __nodes = {}
    __trans = {}

    def load_node_names():
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
                id = {
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
                    }.get(id, id)

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

    def __restore_nodes_from_image(self, names, image):
        self.__nodes = {}
        for i in image:
            n = Node.from_image(i[0], i[1:])
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

    def __evaluate_state_predicates(self, n, stack):
        gnd = pwr = pullup = pulldown = FALSE

        if n in stack:
            pass
        elif n.is_gnd:
            gnd = TRUE
        elif n.is_pwr:
            pwr = TRUE
        else:
            if n.pull is None:
                pass
            elif isinstance(n.pull, Bool):
                pullup = n.pull
                pulldown = ~n.pull
            else:
                assert 0, n.pull

            stack.append(n)

            for t in n.conn_of:
                if not t.state.is_trivially_false():
                    g, p, u, d = self.__evaluate_state_predicates(
                        t.get_other_conn(n), stack)

                    gnd |= t.state & g
                    pwr |= t.state & p
                    pullup |= t.state & u
                    pulldown |= t.state & d

            assert stack.pop() == n

        return gnd, pwr, pullup, pulldown

    __predicate_sizes = {}

    def __update_group_of(self, n, more):
        # Identify nodes of the group.
        group = []
        worklist = [n]
        while worklist:
            n = worklist.pop()
            if n in group or n.is_gnd_or_pwr:
                continue

            group.append(n)

            for t in n.conn_of:
                if not t.state.is_trivially_false():
                    worklist.append(t.get_other_conn(n))

        # Compute state predicates.
        group = {n: self.__evaluate_state_predicates(n, [])
                 for n in group}

        # Update node and transistor states.
        for n, preds in group.items():
            gnd, pwr, pullup, pulldown = preds

            if 0:
                size = sum(p.size for p in preds)
                sizes = __class__.__predicate_sizes.setdefault(n, [])
                sizes.append(size)
                print(n, sizes)

            floating = n.state
            pull = Bool.ifelse(pulldown | pullup, ~pulldown, floating)
            n.state = Bool.ifelse(gnd | pwr, ~gnd, pull).reduced()
            # print(n, n.state)

            # No further propagation is necessary if the state of
            # the transistor is known to be same. This includes
            # the case of a floating gate.
            for t in n.gate_of:
                if not t.state.is_equiv(n.state):
                    # print(t, t.state)
                    t.state = n.state
                    more.extend(t.conns)

    def __update_nodes(self, nodes):
        round = 0
        while nodes:
            round += 1
            assert round < 100, 'Loop encountered!'

            if '--show-rounds' in sys.argv:
                print(f'Round {round}, {len(nodes)} nodes.')

            more = []
            for n in nodes:
                self.__update_group_of(n, more)
            nodes = more

    def __set_node(self, n, pull):
        n.pull = Bool.boolify(pull)
        self.__update_nodes([n])

    def half_tick(self):
        if self.clk:
            if self.mreq and not self.rfsh and not self.iorq:
                if self.m1 and self.rd and self.t2:
                    self.dbus = self.__memory[int(self.abus)]

        self.nclk = ~self.nclk

        self.__print_state()

    def __tick(self):
        self.half_tick()
        self.half_tick()

    def clear_state(self):
        for n in self.__nodes.values():
            n.state = FALSE

        for t in self.__trans.values():
            t.state = FALSE

        self.__gnd.state = FALSE
        self.__pwr.state = TRUE

    def __init_chip(self, skip_reset):
        self.clear_state()

        self.update_all_nodes()

        if skip_reset:
            return

        self.nreset = FALSE
        self.nclk = TRUE
        self.nbusrq = TRUE
        self.nint = TRUE
        self.nnmi = TRUE
        self.nwait = TRUE

        # Propagate the reset signal.
        for _ in range(31):
            self.half_tick()

        self.nreset = TRUE

        # Wait for the first active ~m1, which is essentially an
        # indication that the reset process is completed.
        while self.__nm1.state:
            self.half_tick()

    def __init__(self, *, memory=None, skip_reset=None, image=None):
        if image is None:
            image = State().image
            skip_init = False
        else:
            # For custom images, leave them as-is.
            assert skip_reset is None
            skip_init = True

        node_names, nodes, trans = image
        self.__restore_nodes_from_image(node_names, nodes)
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

        if not skip_init:
            self.__init_chip(skip_reset)

        self.__memory = bytearray(0x10000)
        if memory is not None:
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

    def __read_bits(self, name, width=8):
        return Bits(self.__nodes_by_name[f'{name}{i}'].state
                    for i in range(width))

    def __write_bits(self, name, value, width=8):
        for i in range(width):
            self.__set_node(self.__nodes_by_name[name + str(i)],
                            bool((value >> i) & 0x1))

    @property
    def abus(self):
        return self.__read_bits('ab', 16)

    @property
    def dbus(self):
        return self.__read_bits('db')

    @dbus.setter
    def dbus(self, n):
        self.__write_bits('db', n)

    @property
    def a(self):
        return self.__read_bits('reg_a')

    @property
    def r(self):
        return self.__read_bits('reg_r')

    @property
    def pc(self):
        lo = self.__read_bits('reg_pcl')
        hi = self.__read_bits('reg_pch')
        return (hi << 8) | lo

    def set_pin_state(self, pin, state):
        assert pin in _PINS
        self.__nodes_by_name[pin].state = Bool.boolify(state)

    def set_pin_pull(self, pin, pull):
        assert pin in _PINS
        self.__nodes_by_name[pin].pull = Bool.boolify(pull)

    def update_all_nodes(self):
        self.__update_nodes([n for n in self.__nodes.values()
                             if n not in self.__gnd_pwr])

    def dump(self):
        with open('z80.dump', mode='w') as f:
            for t in sorted(self.__trans.values()):
                print(t, file=f)

    def __print_state(self):
        print(f'PC {self.pc}, '
              f'A {self.a}, '
              f'R {self.r}, '
              f'clk {int(self.clk)}, '
              f'abus {self.abus}, '
              f'dbus {self.dbus}, '
              f'm1 {int(self.m1)}, '
              f't1 {int(self.t1)}, '
              f't2 {int(self.t2)}, '
              f't3 {int(self.t3)}, '
              f't4 {int(self.t4)}, '
              f't5 {int(self.t5)}, '
              f't6 {int(self.t6)}, '
              f'rfsh {int(self.rfsh)}, '
              f'rd {int(self.rd)}, '
              f'mreq {int(self.mreq)}')

    # TODO
    def do_something(self):
        for i in range(30 * 10):
            if i > 0 and self.m1 and self.t1:
                print()

            self.half_tick()


class State(object):
    def __init__(self, other=None):
        if other is None:
            self.__current_steps = []
            self.__current_image = None
            self.__new_steps = []
        else:
            self.__current_steps = list(other.__current_steps)
            self.__current_image = other.__current_image
            self.__new_steps = list(other.__new_steps)

    @staticmethod
    def __get_path(steps):
        key = str(tuple(steps)).encode()
        h = HASH(key).hexdigest()
        return _CACHE_ROOT / 'states' / h[:3] / h[3:6] / h

    @staticmethod
    def __store_state(steps, image):
        state = tuple(steps), image

        path = __class__.__get_path(steps)
        path.parent.mkdir(parents=True, exist_ok=True)

        temp_path = path.parent / (path.name + '.tmp')
        with temp_path.open('w') as f:
            pprint.pprint(state, compact=True, stream=f)

        temp_path.rename(path)

    @staticmethod
    def __try_load_state(steps):
        try:
            with __class__.__get_path(steps).open() as f:
                state = ast.literal_eval(f.read())
            stored_steps, image = state
            assert stored_steps == tuple(steps)
            return image
        except FileNotFoundError:
            return None

    def __apply_new_steps(self):
        if self.__current_image is None:
            assert not self.__current_steps
            self.__current_image = (
                __class__.__try_load_state(self.__current_steps))

            if self.__current_image is None:
                self.__current_image = _load_initial_image()

                # Always store the initial image.
                __class__.__store_state(self.__current_steps,
                                        self.__current_image)

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

        if not missing_steps:
            return

        sim = Z80Simulator(image=self.__current_image)

        while missing_steps:
            step = missing_steps.pop(0)
            __class__.__apply_step(sim, step)
            self.__current_steps.append(step)

        self.__current_image = sim.image

    @property
    def image(self):
        self.__apply_new_steps()
        return self.__current_image

    @staticmethod
    def __apply_step(sim, step):
        kind = step[0]
        if kind == 'clear_state':
            _, = step
            sim.clear_state()
        elif kind == 'set_pin_state':
            _, pin, state = step
            sim.set_pin_state(pin, Bool.from_image(state))
        elif kind == 'set_pin_pull':
            _, pin, pull = step
            sim.set_pin_state(pin, Bool.from_image(pull))
        elif kind == 'update_all_nodes':
            _, = step
            sim.update_all_nodes()
        else:
            assert 0, step

    def clear_state(self):
        self.__new_steps.append(('clear_state',))

    def set_pin_state(self, pin, state):
        step = 'set_pin_state', pin, Bool.boolify(state).image
        self.__new_steps.append(step)

    def set_pin_pull(self, pin, pull):
        step = 'set_pin_pull', pin, Bool.boolify(pull).image
        self.__new_steps.append(step)

    def update_all_nodes(self):
        self.__new_steps.append(('update_all_nodes',))

    def store(self):
        self.__apply_new_steps()
        if not self.__get_path(self.__current_steps).exists():
            __class__.__store_state(self.__current_steps,
                                    self.__current_image)

    def report(self):
        s = Z80Simulator(image=self.image)
        print(f'a: {s.a}')


def test_computing_node_values():
    # With the old function computing node values the LSB of the
    # address bus was always to 0 at the fourth half-tick of
    # fetch cycles. Make sure with the new logic the address bus
    # maintains the right value.
    s = Z80Simulator()
    while s.abus == 0x0000:
        s.half_tick()
    while s.abus == 0x0001:
        s.half_tick()
    assert s.abus == 0x0002

    print('Tests passed.')


def main():
    if '--no-tests' not in sys.argv:
        test_computing_node_values()

    if '--symbolic' in sys.argv:
        initial = State()
        initial.clear_state()
        initial.store()
        initial.report()

        propagated = State(initial)
        for pin in _PINS:
            propagated.set_pin_state(pin, f'init.{pin}')
            propagated.set_pin_pull(pin, f'pull.{pin}')
        propagated.update_all_nodes()
        propagated.store()
        propagated.report()

        return

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


if __name__ == "__main__":
    main()
