
#   Z80 CPU Emulator.
#   https://github.com/kosarev/z80
#
#   Copyright (C) 2017-2019 Ivan Kosarev.
#   ivan@kosarev.info
#
#   Published under the MIT license.

import struct
from ._main import main
from ._z80 import _I8080Machine, _Z80Machine


class _Error(Exception):
    def __init__(self, reason):
        super().__init__(reason)


class _StateBase(object):
    _STATE_FIELDS = {
        'c': (0, 'B'), 'b': (1, 'B'), 'bc': (0, '<H'),
        'e': (2, 'B'), 'd': (3, 'B'), 'de': (2, '<H'),
        'l': (4, 'B'), 'h': (5, 'B'), 'hl': (4, '<H'),
        'f': (2, 'B'), 'a': (7, 'B'), 'af': (6, '<H'),
        'pc': (8, '<H'), 'sp': (10, '<H'),
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

    def get_c(self):
        return self.get('c')

    def get_e(self):
        return self.get('e')

    def get_bc(self):
        return self.get('bc')

    def get_de(self):
        return self.get('de')

    def get_pc(self):
        return self.get('pc')

    def set_pc(self, n):
        self.set('pc', n)

    def get_memory_byte(self, addr):
        memory = self._get_memory_view()
        return memory[addr]

    def set_memory_block(self, addr, block):
        memory = self._get_memory_view()
        memory[addr:addr + len(block)] = block


class I8080State(_StateBase):
    def __init__(self, image):
        _StateBase.__init__(self, image)
        self._STATE_FIELDS.update({
            'iff': (16, 'B'), 'int_disabled': (17, 'B'), 'halted': (18, 'B'),
        })

    def _get_memory_view(self):
        return self._image[20:]


class Z80State(_StateBase):
    def __init__(self, image):
        super().__init__(image)
        self._STATE_FIELDS.update({
            'ixl': (16, 'B'), 'ixh': (17, 'B'), 'ix': (16, '<H'),
            'iyl': (18, 'B'), 'iyh': (19, 'B'), 'iy': (18, '<H'),

            'alt_c': (20, 'B'), 'alt_b': (21, 'B'), 'alt_bc': (20, '<H'),
            'alt_e': (22, 'B'), 'alt_d': (23, 'B'), 'alt_de': (22, '<H'),
            'alt_l': (24, 'B'), 'alt_h': (25, 'B'), 'alt_hl': (24, '<H'),
            'alt_f': (26, 'B'), 'alt_a': (27, 'B'), 'alt_af': (26, '<H'),

            'r': (28, 'B'), 'i': (29, 'B'), 'iy': (28, '<H'),
            'iff1': (30, 'B'), 'iff2': (31, 'B'),

            'int_disabled': (32, 'B'), 'halted': (33, 'B'),
            'int_mode': (34, 'B'), 'index_rp_kind': (35, 'B'),
        })

    def _get_memory_view(self):
        return self._image[36:]


class _MachineBase(object):
    # Events.
    _NO_EVENTS = 0
    _END_OF_FRAME = 1 << 0
    _BREAKPOINT_HIT = 1 << 1

    # Address marks.
    _NO_MARKS = 0
    _BREAKPOINT_MARK = 1 << 0

    def mark_addr(self, addr, marks):
        self.mark_addrs(addr, 1, marks)

    def set_breakpoint(self, addr):
        self.mark_addr(addr, self._BREAKPOINT_MARK)


class I8080Machine(_MachineBase, _I8080Machine, I8080State):
    def __init__(self):
        I8080State.__init__(self, self.get_state_view())


class Z80Machine(_MachineBase, _Z80Machine, Z80State):
    def __init__(self):
        Z80State.__init__(self, self.get_state_view())
