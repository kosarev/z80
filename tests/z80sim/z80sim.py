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
    def __init__(self, index, pullup):
        self.index, self.pullup = index, pullup
        self.pulldown = False  # TODO: Correct?
        self.state = False
        self.gates = []
        self.c1c2s = []


class Transistor(object):
    def __init__(self, id, gate, c1, c2):
        self.id, self.gate, self.c1, self.c2 = id, gate, c1, c2
        self.state = False


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
                if id == 'clk':
                    # CLK is an active-low pin.
                    id = '~clk'

                assert isinstance(i, str)
                i = int(i)

                n = self.__indexes_to_nodes[i]

                assert id not in self.__nodes
                self.__nodes[id] = n
                n.id = id

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
                pullup = {'+': True, '-': False}[pull]

                if i in self.__indexes_to_nodes:
                    n = self.__indexes_to_nodes[i]
                else:
                    n = Node(i, pullup)
                    self.__indexes_to_nodes[i] = n

                assert n.pullup == pullup

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

                gate = self.__indexes_to_nodes[gate]
                c1 = self.__indexes_to_nodes[c1]
                c2 = self.__indexes_to_nodes[c2]

                # TODO: The comment in the original source says to
                # 'ignore all the 'weak' transistors for now'.
                if weak:
                    continue

                # TODO: Why the original source does this?
                if c1 in (self.__gnd, self.__pwr):
                    c1, c2 = c2, c1

                t = Transistor(id, gate, c1, c2)

                assert id not in self.__trans
                self.__trans[id] = t

                gate.gates.append(t)
                c1.c1c2s.append(t)
                c2.c1c2s.append(t)

    def __load_defs(self):
        self.__load_nodes()
        self.__load_node_names()

        self.__gnd = self.__nodes['vss']
        self.__pwr = self.__nodes['vcc']

        self.__nclk = self.__nodes['~clk']

        self.__nbusrq = self.__nodes['~busrq']
        self.__nint = self.__nodes['~int']
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

    def __all_nodes(self):
        return [n for n in self.__indexes_to_nodes.values()
                if n not in (self.__gnd, self.__pwr)]

    def __add_node_to_group(self, n):
        if n in self.__group:
            return

        self.__group.append(n)
        if n in (self.__gnd, self.__pwr):
            return
        for t in n.c1c2s:
            if not t.state:
                continue

            if t.c1 is n:
                other = t.c2
            if t.c2 is n:
                other = t.c1
            self.__add_node_to_group(other)

    def __get_node_group(self, n):
        self.__group = []
        self.__add_node_to_group(n)

    def __get_node_value(self):
        # 1. deal with power connections first
        if self.__gnd in self.__group:
            return False
        if self.__pwr in self.__group:
            return True

        # 2. deal with pullup/pulldowns next
        for n in self.__group:
            if n.pullup:
                return True
            if n.pulldown:
                return False

            # TODO: Do not look at the state until all pullups
            # and pulldowns are considered.
            # if n.state:
            #     return True

        # 3. resolve connected set of floating nodes
        # based on state of largest (by #connections) node
        # (previously this was any node with state true wins)
        max_state = False
        max_connections = 0
        for n in self.__group:
            connections = len(n.gates) + len(n.c1c2s)
            if max_connections < connections:
                max_connections = connections
                max_state = n.state

        return max_state

    def __add_recalc_node(self, n):
        if n in (self.__gnd, self.__pwr):
            return
        if n in self.__recalc_hash:
            return
        self.__recalc_list.append(n)
        self.__recalc_hash.add(n)

    def __set_transistor(self, t, state):
        if t.state == state:
            return
        t.state = state
        self.__add_recalc_node(t.c1)
        if state:
            # TODO: Why don't we do self.__add_recalc_node(t.c2)?
            pass
        else:
            self.__add_recalc_node(t.c2)

    def __recalc_node(self, n):
        if n in (self.__gnd, self.__pwr):
            return

        self.__get_node_group(n)

        new_state = self.__get_node_value()

        for n in self.__group:
            if n.state == new_state:
                continue
            n.state = new_state
            for t in n.gates:
                self.__set_transistor(t, n.state)

    def __recalc_node_list(self, nodes):
        self.__recalc_list = []
        self.__recalc_hash = set()
        for j in range(100):  # Loop limiter.
            if len(nodes) == 0:
                return

            for n in nodes:
                self.__recalc_node(n)

            nodes = self.__recalc_list
            self.__recalc_list = []
            self.__recalc_hash = set()

    def __set_node(self, n, state):
        n.pullup = state
        n.pulldown = not state
        self.__recalc_node_list([n])

    def half_tick(self):
        self.nclk ^= True

        # A comment from the original source:
        # DMB: It's almost certainly wrong to execute these on both clock edges
        # TODO: handleBusRead();
        # TODO: handleBusWrite();

    def __tick(self):
        self.half_tick()
        self.half_tick()

    def __init_chip(self):
        for n in self.__indexes_to_nodes.values():
            n.state = False

        self.__gnd.state = False
        self.__pwr.state = True

        for t in self.__trans.values():
            t.state = False

        self.nreset = False
        self.nclk = True
        self.nbusrq = True
        self.nint = True
        self.nnmi = True
        self.nwait = True

        self.__recalc_node_list(self.__all_nodes())

        # Propagate the reset signal.
        for _ in range(31):
            self.half_tick()

        self.nreset = True

        # Wait for the first active ~m1, which is essentially an
        # indication that the reset process is completed.
        while self.__nm1.state:
            self.half_tick()

    def __init__(self):
        self.__load_defs()
        self.__init_chip()

    @property
    def nclk(self):
        return self.__nclk.state

    @nclk.setter
    def nclk(self, state):
        self.__set_node(self.__nclk, state)

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
    def nreset(self):
        return self.__nreset.state

    @nreset.setter
    def nreset(self, state):
        self.__set_node(self.__nreset, state)

    @property
    def nrfsh(self):
        return self.__nrfsh.state

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

    def __read_bits(self, name, n=8):
        res = 0
        for i in range(n):
            res |= int(self.__nodes[name + str(i)].state) << i
        return res

    @property
    def abus(self):
        return self.__read_bits('ab', 16)

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

    # TODO
    def do_something(self):
        for i in range(30):
            if i > 0 and self.m1 and self.t1:
                print()

            print(f'PC {self.pc:04x}, '
                  f'A {self.a:02x}, '
                  f'R {self.r:02x}, '
                  f'~clk {int(self.nclk)}, '
                  f'abus {self.abus:04x}, '
                  f'~m1 {int(self.nm1)}, '
                  f't1 {int(self.t1)}, '
                  f't2 {int(self.t2)}, '
                  f't3 {int(self.t3)}, '
                  f't4 {int(self.t4)}, '
                  f't5 {int(self.t5)}, '
                  f't6 {int(self.t6)}, '
                  f'~rfsh {int(self.nrfsh)}, '
                  f'~rd {int(self.nrd)}, '
                  f'~mreq {int(self.nmreq)}')

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

    s = Z80Simulator()
    s.do_something()


if __name__ == "__main__":
    main()
