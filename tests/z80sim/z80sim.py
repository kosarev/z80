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


class Node(object):
    __PULL_SIGNS = {None: 'n', False: 'm', True: 'p'}

    def __init__(self, index, pull):
        self.custom_id = None
        self.index, self.pull = index, pull
        self.state = False

        # These are not sets as we want reproducible behaviour.
        self.gate_of = []
        self.conn_of = []

    def __repr__(self):
        return self.id

    @property
    def id(self):
        pull = self.__PULL_SIGNS[self.pull]

        if self.custom_id is None:
            return f'{pull}{self.index}'

        if self.pull is None:
            return f'{self.custom_id}'

        return f'{pull}.{self.custom_id}'


class Transistor(object):
    def __init__(self, index, gate, c1, c2):
        self.index, self.gate, self.c1, self.c2 = index, gate, c1, c2
        self.state = False

    def __repr__(self):
        return f'{self.id}({self.c1}, {self.gate}, {self.c2})'

    def __lt__(self, other):
        return self.index < other.index

    @property
    def id(self):
        return f't{self.index}'


class Z80Simulator(object):
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

                    'vss': 'gnd',
                    'vcc': 'pwr',
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

    def __load_transistors(self):
        self.__trans = {}
        with open('transdefs.js') as f:
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

                # TODO: The comment in the original source says to
                # 'ignore all the 'weak' transistors for now'.
                if weak:
                    continue

                # Skip meaningless transistors, e.g., t251(gnd, gnd, gnd).
                if c1 is c2:
                    assert c1 is self.__gnd
                    continue

                # TODO: Why the original source does this?
                if c1 in self.__gnd_pwr:
                    assert c2 not in self.__gnd_pwr, (c1, c2)
                    c1, c2 = c2, c1

                t = Transistor(index, gate, c1, c2)

                assert index not in self.__trans
                self.__trans[index] = t

                gate.gate_of.append(t)
                c1.conn_of.append(t)
                c2.conn_of.append(t)

    def __load_defs(self):
        self.__load_nodes()
        self.__load_node_names()

        self.__gnd = self.__nodes['gnd']
        self.__pwr = self.__nodes['pwr']
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

        self.__load_transistors()

    def __add_node_to_group(self, n, group):
        if n in group:
            return

        group.append(n)

        if n in self.__gnd_pwr:
            return

        for t in n.conn_of:
            if t.state:
                other = t.c1 if n is t.c2 else t.c2
                self.__add_node_to_group(other, group)

    def __get_group_state(self, group):
        # 1. deal with power connections first
        if self.__gnd in group:
            return False
        if self.__pwr in group:
            return True

        # 2. deal with pullup/pulldowns next
        for n in group:
            if n.pull is not None:
                return n.pull

        # 3. resolve connected set of floating nodes
        # based on state of largest (by #connections) node
        # (previously this was any node with state true wins)
        max_state = False
        max_connections = 0
        for n in group:
            connections = len(n.gate_of) + len(n.conn_of)
            if max_connections < connections:
                max_connections = connections
                max_state = n.state

        return max_state

    def __add_recalc_node(self, n, recalc_nodes):
        if n in self.__gnd_pwr:
            return
        if n in recalc_nodes:
            return
        recalc_nodes.append(n)

    def __set_transistor(self, t, state, recalc_nodes):
        if t.state == state:
            return
        t.state = state
        self.__add_recalc_node(t.c1, recalc_nodes)
        if state:
            # TODO: Why don't we do self.__add_recalc_node(t.c2)?
            pass
        else:
            self.__add_recalc_node(t.c2, recalc_nodes)

    def __recalc_node(self, n, recalc_nodes):
        if n in self.__gnd_pwr:
            return

        group = []
        self.__add_node_to_group(n, group)

        new_state = self.__get_group_state(group)

        for n in group:
            if n.state == new_state:
                continue
            n.state = new_state
            for t in n.gate_of:
                self.__set_transistor(t, n.state, recalc_nodes)

    def __recalc_node_list(self, recalc_nodes):
        attempt = 0
        while recalc_nodes:
            # Loop limiter.
            attempt += 1
            if attempt > 100:
                break

            nodes = recalc_nodes
            recalc_nodes = []
            for n in nodes:
                self.__recalc_node(n, recalc_nodes)

    def __set_node(self, n, pull):
        n.pull = pull
        self.__recalc_node_list([n])

    def half_tick(self):
        if self.clk:
            if self.mreq and not self.rfsh and not self.iorq:
                if self.m1 and self.rd and self.t2:
                    self.dbus = self.__memory[self.abus]

        self.nclk ^= True

    def __tick(self):
        self.half_tick()
        self.half_tick()

    def __init_chip(self, skip_reset):
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

        self.__recalc_node_list(
            [n for n in self.__indexes_to_nodes.values()
             if n not in self.__gnd_pwr])

        # Propagate the reset signal.
        for _ in range(31):
            self.half_tick()

        self.nreset = True

        # Wait for the first active ~m1, which is essentially an
        # indication that the reset process is completed.
        while self.__nm1.state:
            self.half_tick()

    def __init__(self, *, memory=None, skip_reset=False):
        self.__load_defs()
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
            res |= int(self.__nodes[name + str(i)].state) << i
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

    # TODO
    def do_something(self):
        for i in range(30):
            if i > 0 and self.m1 and self.t1:
                print()

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

            self.half_tick()


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


def main():
    test_computing_node_values()

    if 0:
        memory = [
            0x76,  # halt
            0xc5,  # nop
        ]
        s = Z80Simulator(memory=memory)
        s.do_something()
    else:
        s = Z80Simulator(skip_reset=True)
        s.dump()


if __name__ == "__main__":
    main()
