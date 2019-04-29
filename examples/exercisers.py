#!/usr/bin/env python3

import sys
import z80


class _CPMLikeMachineMixin(object):
    def __init__(self, filename):
        with open(filename, 'rb') as f:
            image = f.read()

        self.set_memory_block(0x0100, image)
        self.set_pc(0x0100)

        self.set_breakpoint(0x0005)  # BDOS calls.
        self.set_memory_block(0x0005, b'\xc9')  # ret

        self.set_breakpoint(0x0000)  # Quit program.

        self._output = []

    def get_output(self):
        return ''.join(self._output)

    def output(self, s):
        self._output.append(s)
        sys.stdout.write(s)

    def _C_WRITE(self):
        self.output(chr(self.get_e()))

    def _C_WRITESTR(self):
        addr = self.get_de()
        while True:
            c = self.get_memory_byte(addr)
            if c == ord('$'):
                break

            self.output(chr(c))
            addr = (addr + 1) % 0xffff

    def _handle_bdos_call(self):
        c = self.get_c()
        if c == 0x02:
            self._C_WRITE()
        elif c == 0x09:
            self._C_WRITESTR()
        else:
            assert 0, 'BDOS call: c = 0x%02x' % c

    def _handle_breakpoint(self):
        pc = self.get_pc()
        if pc == 0x0000:
            # Quit program.
            return False
        elif pc == 0x0005:
            self._handle_bdos_call()
        else:
            assert 0, 'Breakpoint hit: pc = 0x%04x' % pc

        return True

    def run(self):
        while True:
            events = super().run()

            if events & self._BREAKPOINT_HIT:
                if not self._handle_breakpoint():
                    break

        self.output('\n')


class I8080CPMLikeMachine(_CPMLikeMachineMixin, z80.I8080Machine):
    def __init__(self, filename):
        z80.I8080Machine.__init__(self)
        _CPMLikeMachineMixin.__init__(self, filename)


class Z80CPMLikeMachine(_CPMLikeMachineMixin, z80.Z80Machine):
    def __init__(self, filename):
        z80.Z80Machine.__init__(self)
        _CPMLikeMachineMixin.__init__(self, filename)


tests = [
    (I8080CPMLikeMachine, [
        'supplements/cputest.com',
        'supplements/8080pre.com',
        'supplements/8080exm.com',
    ]),
    (Z80CPMLikeMachine, [
        'supplements/cputest.com',
        'supplements/prelim.com',
        'supplements/zexall.com',
    ]),
]

for machine_type, files in tests:
    PASS_STAMPS = [
        'CPU TESTS OK',
        'Preliminary tests complete',
        'Tests complete',
    ]

    for file in files:
        m = machine_type(file)
        m.run()
        output = m.get_output()
        assert any(s in output for s in PASS_STAMPS)
