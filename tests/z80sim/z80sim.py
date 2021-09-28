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
        assert 0

    def __init__(self):
        self.__load_defs()


def main():
    Z80Simulator()


if __name__ == "__main__":
    main()
