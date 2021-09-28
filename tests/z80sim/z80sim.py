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
    def __init__(self, id, pullup):
        self.id, self.pullup = id, pullup
        self.pulldown = False  # TODO: Correct?
        self.state = False
        self.gates = []
        self.c1c2s = []


class Transistor(object):
    def __init__(self, id, gate, c1, c2):
        self.id, self.gate, self.c1, self.c2 = id, gate, c1, c2
        self.on = False


class Z80Simulator(object):
    def __load_node_names(self):
        self.__node_ids = {}
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

                assert isinstance(i, str)
                i = int(i)

                assert id not in self.__node_ids
                self.__node_ids[id] = i

    def __load_nodes(self):
        self.__nodes = {}
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

                if i in self.__nodes:
                    n = self.__nodes[i]
                else:
                    n = Node(i, pullup)
                    self.__nodes[i] = n

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

                # TODO: The comment in the original source says to
                # 'ignore all the 'weak' transistors for now'.
                if weak:
                    continue

                # TODO: Why the original source does this?
                if c1 in (self.__ngnd, self.__npwr):
                    c1, c2 = c2, c1

                t = Transistor(id, gate, c1, c2)

                assert id not in self.__trans
                self.__trans[id] = t

                self.__nodes[gate].gates.append(t)
                self.__nodes[c1].c1c2s.append(t)
                self.__nodes[c2].c1c2s.append(t)

    def __load_defs(self):
        self.__load_node_names()
        self.__load_nodes()

        self.__ngnd = self.__node_ids['vss']
        self.__npwr = self.__node_ids['vcc']

        self.__load_transistors()

    def __all_nodes(self):
        res = []
        for i in self.__nodes:
            if i != self.__npwr and i != self.__ngnd:
                res.append(i)
        return res

    def __add_node_to_group(self, i):
        if i in self.__group:
            return

        self.__group.append(i)
        if i == self.__ngnd:
            return
        if i == self.__npwr:
            return
        for t in self.__nodes[i].c1c2s:
            if not t.on:
                continue

            if t.c1 == i:
                other = t.c2
            if t.c2 == i:
                other = t.c1
            self.__add_node_to_group(other)

    def __get_node_group(self, i):
        self.__group = []
        self.__add_node_to_group(i)

    def __get_node_value(self):
        if self.__ngnd in self.__group:
            return False
        if self.__npwr in self.__group:
            return True
        for i in self.__group:
            n = self.__nodes[i]
            if n.pullup:
                return True
            if n.pulldown:
                return False
            if n.state:
                return True
        return False

    def __add_recalc_node(self, nn):
        if nn == self.__ngnd:
            return
        if nn == self.__npwr:
            return
        '''
        if nn in self.__recalc_hash:
            return
        '''
        self.__recalc_list.append(nn)
        self.__recalc_hash.add(nn)

    def __turn_transistor_on(self, t):
        if t.on:
            return
        t.on = True
        self.__add_recalc_node(t.c1)
        # TODO: Why don't we do self.__add_recalc_node(t.c2)?

    def __turn_transistor_off(self, t):
        if not t.on:
            return
        t.on = False
        self.__add_recalc_node(t.c1)
        self.__add_recalc_node(t.c2)

    def __recalc_node(self, node):
        if node == self.__ngnd:
            return
        if node == self.__npwr:
            return

        self.__get_node_group(node)

        new_state = self.__get_node_value()

        x = self.__group[:]

        for i in self.__group:
            n = self.__nodes[i]
            if n.state == new_state:
                continue
            n.state = new_state
            for t in n.gates:
                if n.state:
                    self.__turn_transistor_on(t)
                else:
                    self.__turn_transistor_off(t)

        # TODO
        assert self.__group == x

    def __recalc_node_list(self, list):
        self.__recalc_list = []
        self.__recalc_hash = set()
        for j in range(100):  # Loop limiter.
            if len(list) == 0:
                return

            for i in list:
                self.__recalc_node(i)

            list = self.__recalc_list
            self.__recalc_list = []
            self.__recalc_hash = set()

    def __set_low(self, id):
        i = self.__node_ids[id]
        self.__nodes[i].pullup = False
        self.__nodes[i].pulldown = True
        self.__recalc_node_list([i])

    def __set_high(self, id):
        i = self.__node_ids[id]
        self.__nodes[i].pullup = True
        self.__nodes[i].pulldown = False
        self.__recalc_node_list([i])

    def __is_node_high(self, nn):
        return self.__nodes[nn].state

    def __half_tick(self):
        clk = self.__is_node_high(self.__node_ids['clk'])
        # print(f'__half_tick(): clk {clk}')
        if clk:
            self.__set_low('clk')
        else:
            self.__set_high('clk')

        # A comment from the original source:
        # DMB: It's almost certainly wrong to execute these on both clock edges
        # TODO: handleBusRead();
        # TODO: handleBusWrite();

    def __tick(self):
        self.__half_tick()
        self.__half_tick()

    def __init_chip(self):
        for n in self.__nodes.values():
            n.state = False

        self.__nodes[self.__ngnd].state = False
        self.__nodes[self.__npwr].state = True

        for t in self.__trans.values():
            t.on = False

        self.__set_low('~reset')
        self.__set_high('clk')
        self.__set_high('~busrq')
        self.__set_high('~int')
        self.__set_high('~nmi')
        self.__set_high('~wait')

        self.__recalc_node_list(self.__all_nodes())

        # Propagate the reset signal.
        for _ in range(31):
            self.__half_tick()

        self.__set_high('~reset')

    def __init__(self):
        self.__load_defs()
        self.__init_chip()

    def __read_bits(self, name, n=8):
        res = 0
        for i in range(n):
            nn = self.__node_ids[name + str(i)]
            res += (1 if self.__is_node_high(nn) else 0) << i
        return res

    def __read_a(self):
        return self.__read_bits('reg_a')

    def __read_r(self):
        return self.__read_bits('reg_r')

    def __read_pc(self):
        lo = self.__read_bits('reg_pcl')
        hi = self.__read_bits('reg_pch')
        return (hi << 8) | lo

    # TODO
    def do_something(self):
        for _ in range(20):
            print(f'{self.__read_pc():04x} '
                  f'{self.__read_a():02x} '
                  f'{self.__read_r():02x}')
            self.__tick()


def main():
    s = Z80Simulator()
    s.do_something()


if __name__ == "__main__":
    main()
