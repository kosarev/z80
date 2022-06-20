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

_GND_ID = 'gnd'
_PWR_ID = 'pwr'


class BoolExpr(object):
    def __init__(self, term):
        if isinstance(term, z3.BoolRef):
            self.__e = term
        elif isinstance(term, str):
            self.__e = z3.Bool(term)
        else:
            assert term is False or term is True
            self.__e = z3.BoolVal(term)

    def __repr__(self):
        return self.__e.sexpr()

    @property
    def value(self):
        if z3.is_false(self.__e):
            return False
        if z3.is_true(self.__e):
            return True
        return None

    def is_trivially_false(self):
        return self.value is False

    @property
    def size(self):
        return len(str(self))

    def __or__(self, other):
        return BoolExpr(z3.Or(self.__e, other.__e))

    def __and__(self, other):
        return BoolExpr(z3.And(self.__e, other.__e))

    def __invert__(self):
        return BoolExpr(z3.Not(self.__e))

    @staticmethod
    def ifelse(cond, a, b):
        return BoolExpr(z3.If(cond.__e, a.__e, b.__e))

    __equiv0_cache = set()
    __equiv1_cache = set()

    @staticmethod
    def __is_equiv(a, b):
        key = a.sexpr() + ':' + b.sexpr()
        h = hashlib.sha256(key.encode()).hexdigest()

        if h in __class__.__equiv0_cache:
            return False
        if h in __class__.__equiv1_cache:
            return True

        path0 = _CACHE_ROOT / 'equiv0' / h[:3] / h[3:6] / h[6:]
        if path0.exists():
            __class__.__equiv0_cache.add(h)
            return False
        path1 = _CACHE_ROOT / 'equiv1' / h[:3] / h[3:6] / h[6:]
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

    def reduced(self):
        simplified = z3.simplify(self.__e)
        for c in (FALSE, TRUE):
            if __class__.__is_equiv(simplified, c.__e):
                return c

        return BoolExpr(simplified)


FALSE = BoolExpr(False)
TRUE = BoolExpr(True)


class Node(object):
    __PULL_SIGNS = {None: 'n', False: 'm', True: 'p'}

    def __init__(self, index, pull, custom_id=None):
        self.custom_id = custom_id
        self.index, self.pull = index, pull
        self.state = None

        # These are not sets as we want reproducible behaviour.
        self.gate_of = []
        self.conn_of = []

    def __repr__(self):
        return self.id

    def __lt__(self, other):
        return self.index < other.index

    @property
    def fields(self):
        return self.index, self.custom_id, self.pull

    @staticmethod
    def by_fields(fields):
        index, custom_id, pull = fields
        return Node(index, pull, custom_id)

    @property
    def id(self):
        if isinstance(self.pull, BoolExpr):
            pull = 'x'
        else:
            pull = self.__PULL_SIGNS[self.pull]

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
        self.state = None

    def __repr__(self):
        return f'{self.c1} = {self.c2} [{self.gate}]  # {self.id}'

    def __lt__(self, other):
        return self.index < other.index

    @property
    def id(self):
        return f't{self.index}'

    @property
    def fields(self):
        return self.index, self.gate.index, self.c1.index, self.c2.index

    @staticmethod
    def by_fields(fields, nodes):
        index, gate, c1, c2 = fields
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


class Z80Simulator(object):
    __cache = None

    def __load_node_names(self):
        self.__nodes = {}
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

                n = self.__indexes_to_nodes[i]

                assert id not in self.__nodes or self.__nodes[id] is n
                self.__nodes[id] = n

                assert n.custom_id is None or n.custom_id == id, id
                n.custom_id = id

    def __load_nodes(self):
        self.__indexes_to_nodes = {}
        with open('segdefs.js') as f:
            for line in f:
                line = line.rstrip()
                if line in ('var segdefs = [', ']'):
                    continue

                assert line.startswith('['), repr(line)
                fields, = ast.literal_eval(line)

                (i, pull, _) = fields[:3]
                assert isinstance(i, int)
                pull = {'+': True, '-': None}[pull]

                if i in self.__indexes_to_nodes:
                    n = self.__indexes_to_nodes[i]
                else:
                    n = Node(i, pull)
                    self.__indexes_to_nodes[i] = n

                assert n.pull == pull

    def __get_nodes_cache(self):
        return (n.fields for n in sorted(self.__indexes_to_nodes.values()))

    def __restore_nodes_from_cache(self, cache):
        self.__nodes = {}
        self.__indexes_to_nodes = {}
        for fields in cache:
            n = Node.by_fields(fields)
            if n.custom_id is not None:
                self.__nodes[n.custom_id] = n
            self.__indexes_to_nodes[n.index] = n

    def __load_transistors(self):
        self.__trans = {}

        def add(t):
            assert index not in self.__trans
            self.__trans[index] = t

            if t not in gate.gate_of:
                t.gate.gate_of.append(t)
            if t not in c1.conn_of:
                t.c1.conn_of.append(t)
            if t not in c2.conn_of:
                t.c2.conn_of.append(t)

        def remove(t):
            del self.__trans[t.index]

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

                gate = self.__indexes_to_nodes[gate]
                c1 = self.__indexes_to_nodes[c1]
                c2 = self.__indexes_to_nodes[c2]

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
        for n in self.__indexes_to_nodes.values():
            if len(n.used_in) == 1 and not n.is_pin:
                remove(*n.used_in)

    def __get_transistors_cache(self):
        return (t.fields for t in sorted(self.__trans.values()))

    def __restore_transistors_from_cache(self, cache):
        self.__trans = {}
        for fields in cache:
            t = Transistor.by_fields(fields, self.__indexes_to_nodes)
            self.__trans[t.index] = t

    def __load_defs(self):
        CACHE_FILENAME = 'z80.cache'
        try:
            if Z80Simulator.__cache is None:
                with open(CACHE_FILENAME) as f:
                    Z80Simulator.__cache = ast.literal_eval(f.read())
            nodes, trans = Z80Simulator.__cache
            self.__restore_nodes_from_cache(nodes)
            self.__restore_transistors_from_cache(trans)
        except FileNotFoundError:
            self.__load_nodes()
            self.__load_node_names()
            self.__load_transistors()

            # Remove unused nodes.
            self.__indexes_to_nodes = {
                i: n for i, n in self.__indexes_to_nodes.items()
                if len(n.conn_of) > 0 or len(n.gate_of) > 0}

            with open(CACHE_FILENAME, 'w') as f:
                Z80Simulator.__cache = (tuple(self.__get_nodes_cache()),
                                        tuple(self.__get_transistors_cache()))
                pprint.pp(Z80Simulator.__cache, compact=True, stream=f)

        self.__gnd = self.__nodes[_GND_ID]
        self.__pwr = self.__nodes[_PWR_ID]
        self.__gnd_pwr = self.__gnd, self.__pwr

        self.__nclk = self.__nodes['~clk']

        self.__nbusrq = self.__nodes['~busrq']
        self.__nint = self.__nodes['~int']
        self.__niorq = self.__nodes['~iorq']
        self.__nm1 = self.__nodes['~m1']
        self.__nmreq = self.__nodes['~mreq']
        self.__nnmi = self.__nodes['~nmi']
        self.__nrd = self.__nodes['~rd']
        self.__nreset = self.__nodes['~reset']
        self.__nrfsh = self.__nodes['~rfsh']
        self.__nwait = self.__nodes['~wait']

        self.__t1 = self.__nodes['t1']
        self.__t2 = self.__nodes['t2']
        self.__t3 = self.__nodes['t3']
        self.__t4 = self.__nodes['t4']
        self.__t5 = self.__nodes['t5']
        self.__t6 = self.__nodes['t6']

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
            elif n.pull is True:
                pullup = TRUE
            elif n.pull is False:
                assert 0  # There are no pull-downs in the net itself.
                pulldown = TRUE
            elif isinstance(n.pull, BoolExpr):
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

    def __symbolically_update_group_of(self, n, more):
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
            pull = BoolExpr.ifelse(pulldown | pullup, ~pulldown, floating)
            n.state = BoolExpr.ifelse(gnd | pwr, ~gnd, pull).reduced()
            # print(n, n.state)

            # No further propagation is necessary if the state of
            # the transistor is known to be same. This includes
            # the case of a floating gate.
            for t in n.gate_of:
                if not t.state.is_equiv(n.state):
                    # print(t, t.state)
                    t.state = n.state
                    more.extend(t.conns)

    def __update_group_of(self, n, more):
        if self.__symbolic:
            self.__symbolically_update_group_of(n, more)
            return

        if n in self.__gnd_pwr:
            return

        group = []
        worklist = [n]
        while worklist:
            n = worklist.pop()
            if n in group:
                continue

            group.append(n)

            if n not in self.__gnd_pwr:
                for t in n.conn_of:
                    if t.state:
                        worklist.append(t.get_other_conn(n))

        state = None
        if self.__gnd in group:
            state = False
        elif self.__pwr in group:
            state = True
        else:
            for n in group:
                if n.pull is not None:
                    state = n.pull
                    break

        if state is None:
            # Floating group.
            return

        for n in group:
            if n.state == state:
                continue

            n.state = state

            for t in n.gate_of:
                if t.state == state:
                    continue

                t.state = state

                if t.c1 not in self.__gnd_pwr and t.c1 not in more:
                    more.append(t.c1)

                # It only makes sense to update the group of the second
                # connection if the transistor became closed.
                if not state:
                    if t.c2 not in self.__gnd_pwr and t.c2 not in more:
                        more.append(t.c2)

    def __update_nodes(self, nodes):
        round = 0
        while nodes:
            round += 1
            assert round < 100, 'Loop encountered!'
            print(f'Round {round}, {len(nodes)} nodes.')

            more = []
            for n in nodes:
                self.__update_group_of(n, more)
            nodes = more

    def __set_node(self, n, pull):
        n.pull = pull
        self.__update_nodes([n])

    def half_tick(self):
        if self.clk:
            if self.mreq and not self.rfsh and not self.iorq:
                if self.m1 and self.rd and self.t2:
                    self.dbus = self.__memory[self.abus]

        self.nclk ^= True

        self.__print_state()

    def __tick(self):
        self.half_tick()
        self.half_tick()

    def __init_chip(self, skip_reset):
        if self.__symbolic:
            assert skip_reset is None

            for n in self.__indexes_to_nodes.values():
                if not n.is_gnd_or_pwr:
                    n.state = FALSE

            for t in self.__trans.values():
                t.state = FALSE
            return

        for n in self.__indexes_to_nodes.values():
            n.state = False

        self.__gnd.state = False
        self.__pwr.state = True

        for t in self.__trans.values():
            t.state = False

        if skip_reset:
            return

        self.nreset = False
        self.nclk = True
        self.nbusrq = True
        self.nint = True
        self.nnmi = True
        self.nwait = True

        self.__update_nodes([n for n in self.__indexes_to_nodes.values()
                             if n not in self.__gnd_pwr])

        # Propagate the reset signal.
        for _ in range(31):
            self.half_tick()

        self.nreset = True

        # Wait for the first active ~m1, which is essentially an
        # indication that the reset process is completed.
        while self.__nm1.state:
            self.half_tick()

    def __init__(self, *, memory=None, skip_reset=None, symbolic=False):
        self.__symbolic = symbolic

        self.__load_defs()
        self.__init_chip(skip_reset)

        if self.__symbolic:
            assert memory is None
        else:
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
        return not self.nclk

    @property
    def niorq(self):
        return self.__niorq.state

    @property
    def iorq(self):
        return not self.niorq

    @property
    def nm1(self):
        return self.__nm1.state

    @property
    def m1(self):
        return not self.__nm1.state

    @property
    def nmreq(self):
        return self.__nmreq.state

    @property
    def mreq(self):
        return not self.nmreq

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
        return not self.nrfsh

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
        return not self.nrd

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
        res = 0
        for i in range(width):
            state = self.__nodes[name + str(i)].state
            if isinstance(state, BoolExpr):
                state = state.value

            assert isinstance(state, bool)
            res |= int(state) << i
        return res

    def __write_bits(self, name, value, width=8):
        for i in range(width):
            self.__set_node(self.__nodes[name + str(i)],
                            (value >> i) & 0x1)

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

    def dump(self):
        with open('z80.dump', mode='w') as f:
            for t in sorted(self.__trans.values()):
                print(t, file=f)

    def __print_state(self):
        print(f'PC {self.pc:04x}, '
              f'A {self.a:02x}, '
              f'R {self.r:02x}, '
              f'clk {int(self.clk)}, '
              f'abus {self.abus:04x}, '
              f'dbus {self.dbus:02x}, '
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

    # TODO
    def do_something_symbolically(self):
        PINS = (
            'ab0', 'ab1', 'ab2', 'ab3', 'ab4', 'ab5', 'ab6', 'ab7',
            'ab8', 'ab9', 'ab10', 'ab11', 'ab12', 'ab13', 'ab14', 'ab15',
            'db0', 'db1', 'db2', 'db3', 'db4', 'db5', 'db6', 'db7',
            '~int', '~nmi', '~halt', '~mreq', '~iorq', '~rfsh', '~m1',
            '~reset', '~busrq', '~wait', '~busak', '~wr', '~rd', '~clk')
        for pin in PINS:
            n = self.__nodes[pin]
            n.state = BoolExpr(f'init.{n.id}')
            n.pull = BoolExpr(f'pull.{n.id}')

        self.__update_nodes([n for n in self.__indexes_to_nodes.values()
                             if n not in self.__gnd_pwr])

        print(f'a: {self.a:#04x}')


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
        s = Z80Simulator(symbolic=True)
        s.do_something_symbolically()
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
    # s.do_something()


if __name__ == "__main__":
    main()
