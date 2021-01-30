#!/usr/bin/env python3

import sys
import z80


class _CPMLikeMachineMixin(object):
    _QUIT = 0x0000
    _BDOS_CALL = 0x0005
    _ENTRY = 0x0100

    # BDOS routines.
    _C_WRITE = 0x02
    _C_WRITESTR = 0x09

    def __init__(self, filename):
        with open(filename, 'rb') as f:
            image = f.read()

        self.set_memory_block(self._ENTRY, image)
        self.pc = self._ENTRY

        self.set_breakpoint(self._BDOS_CALL)
        self.set_memory_block(self._BDOS_CALL, b'\xc9')  # ret

        self.set_breakpoint(self._QUIT)

        self._output = []

    def get_output(self):
        return ''.join(self._output)

    def output(self, s):
        self._output.append(s)
        sys.stdout.write(s)

    def _handle_c_write(self):
        self.output(chr(self.e))

    def _handle_writestr(self):
        addr = self.de
        while True:
            c = self.memory[addr]
            if c == ord('$'):
                break

            self.output(chr(c))
            addr = (addr + 1) & 0xffff

    def _handle_bdos_call(self):
        c = self.c
        if c == self._C_WRITE:
            self._handle_c_write()
        elif c == self._C_WRITESTR:
            self._handle_writestr()
        else:
            assert 0, 'BDOS call: c = 0x%02x' % c

    def _handle_breakpoint(self):
        pc = self.pc
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
