
#   Z80 CPU Emulator.
#   https://github.com/kosarev/z80
#
#   Copyright (C) 2017-2019 Ivan Kosarev.
#   ivan@kosarev.info
#
#   Published under the MIT license.

import struct
from ._z80 import _I8080Machine, _Z80Machine


class I8080State(object):
    _STATE_FIELDS = {
        'c': (0, 'B'), 'b': (1, 'B'), 'bc': (0, '<H'),
        'e': (2, 'B'), 'd': (3, 'B'), 'de': (2, '<H'),
        'l': (4, 'B'), 'h': (5, 'B'), 'hl': (4, '<H'),
        'f': (2, 'B'), 'a': (7, 'B'), 'af': (6, '<H'),
        'pc': (8, '<H'), 'sp': (10, '<H'),
        'iff': (16, 'B'), 'int_disabled': (17, 'B'), 'halted': (18, 'B'),
    }

    def __init__(self, image):
        self._image = image

    def get(self, id):
        offset, format = self._STATE_FIELDS[id]
        size = struct.calcsize(format)
        return struct.unpack(format, self._image[offset:offset + size])[0]

    def set(self, id, n):
        offset, format = self._STATE_FIELDS[id]
        size = struct.calcsize(format)
        self._image[offset:offset + size] = struct.pack(format, n)

    def get_bc(self):
        return self.get('bc')

    def get_pc(self):
        return self.get('pc')

    def set_pc(self, n):
        self.set('pc', n)

    def _get_memory_view(self):
        return self._image[20:]

    def set_memory(self, addr, block):
        memory = self._get_memory_view()
        memory[addr:addr + len(block)] = block


class I8080Machine(_I8080Machine, I8080State):
    # Events.
    _NO_EVENTS         = 0
    _END_OF_FRAME      = 1 << 1
    _BREAKPOINT_HIT    = 1 << 2

    def __init__(self):
        I8080State.__init__(self, self.get_state_view())
