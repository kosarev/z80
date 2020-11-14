
#   Z80 CPU Emulator.
#   https://github.com/kosarev/z80
#
#   Copyright (C) 2017-2019 Ivan Kosarev.
#   ivan@kosarev.info
#
#   Published under the MIT license.

from ._z80 import _I8080Machine, _Z80Machine


class _ByteState(object):
    def __init__(self, image):
        assert len(image) >= 1
        self.__image = image[0:1]

    def get(self):
        return self.__image[0]

    def set(self, value):
        assert 0  # TODO
        self.__image[0] = value

    value = property(get, set)


class _WordState(object):
    def __init__(self, image):
        assert len(image) >= 2
        self.__image = image[0:2]
        self.lo = _ByteState(image[0:1])
        self.hi = _ByteState(image[1:2])

    def get(self):
        return self.__image[0] + self.__image[1] * 0x100

    def set(self, value):
        self.__image[0] = value % 0x100
        self.__image[1] = value // 0x100

    value = property(get, set)


class _U32State(object):
    def __init__(self, image):
        assert len(image) >= 4
        self.__image = image[0:4]

    def get(self):
        return (self.__image[0] + self.__image[1] * 0x100 +
                self.__image[2] * 0x10000 + self.__image[2] * 0x1000000)

    def set(self, value):
        self.__image[0] = value % 0x100
        self.__image[1] = (value // 0x100) % 0x100
        self.__image[2] = (value // 0x10000) % 0x100
        self.__image[3] = value // 0x1000000

    value = property(get, set)


class _ImageParser(object):
    def __init__(self, image):
        self.__image = image

    def parse_block(self, size):
        assert len(self.__image) >= size
        block = self.__image[0:size]
        self.__image = self.__image[size:]
        return block

    def parse_rest(self):
        return self.parse_block(len(self.__image))

    def parse_byte(self):
        return _ByteState(self.parse_block(1))

    def parse_word(self):
        return _WordState(self.parse_block(2))

    def parse_u32(self):
        return _U32State(self.parse_block(4))


class _StateBase(object):
    def __init__(self, image):
        self._image = image

    def __parse_common_fields(self, parser):
        self.__bc = parser.parse_word()
        self.__de = parser.parse_word()
        self.__hl = parser.parse_word()
        self.__af = parser.parse_word()
        self.__pc = parser.parse_word()
        self.__sp = parser.parse_word()
        self.__wz = parser.parse_word()
        self.__last_read_addr = parser.parse_word()
        self.__ticks_to_stop = parser.parse_u32()

        self.__a = self.__af.hi
        self.__b = self.__bc.hi
        self.__c = self.__bc.lo
        self.__d = self.__de.hi
        self.__e = self.__de.lo
        self.__f = self.__af.lo
        self.__h = self.__hl.hi
        self.__l = self.__hl.lo

    def __parse_memory(self, parser):
        block = parser.parse_rest()
        assert len(block) == 0x10000, len(block)
        self.memory = block

    def get_b(self):
        return self.__b.value

    def set_b(self, value):
        self.__b.value = value

    def get_c(self):
        return self.__c.value

    def set_c(self, value):
        self.__c.value = value

    def get_d(self):
        return self.__d.value

    def set_d(self, value):
        self.__d.value = value

    def get_e(self):
        return self.__e.value

    def set_e(self, value):
        self.__e.value = value

    def get_bc(self):
        return self.__bc.value

    def set_bc(self, value):
        self.__bc.value = value

    def get_de(self):
        return self.__de.value

    def set_de(self, value):
        self.__de.value = value

    def get_hl(self):
        return self.__hl.value

    def set_hl(self, value):
        self.__hl.value = value

    def get_pc(self):
        return self.__pc.value

    def set_pc(self, value):
        self.__pc.value = value

    def get_sp(self):
        return self.__sp.value

    def set_sp(self, value):
        self.__sp.value = value

    def get_ticks_to_stop(self):
        return self.__tick_to_stop.value

    def set_ticks_to_stop(self, value):
        self.__ticks_to_stop.value = value

    b = property(get_b, set_b)
    c = property(get_c, set_c)
    d = property(get_d, set_d)
    e = property(get_e, set_e)

    bc = property(get_bc, set_bc)
    de = property(get_de, set_de)
    hl = property(get_hl, set_hl)
    pc = property(get_pc, set_pc)
    sp = property(get_sp, set_sp)

    ticks_to_stop = property(get_ticks_to_stop, set_ticks_to_stop)

    def get_memory_byte(self, addr):
        return self.memory[addr]

    def set_memory_block(self, addr, block):
        self.memory[addr:addr + len(block)] = block


class I8080State(_StateBase):
    def __init__(self, image):
        _StateBase.__init__(self, image)

        parser = _ImageParser(image)
        self._StateBase__parse_common_fields(parser)
        self.__iff = parser.parse_byte()
        self.__int_disabled = parser.parse_byte()
        self.__halted = parser.parse_byte()
        parser.parse_byte()  # Padding.
        self._StateBase__parse_memory(parser)


class Z80State(_StateBase):
    def __init__(self, image):
        super().__init__(image)

        parser = _ImageParser(image)
        self._StateBase__parse_common_fields(parser)
        self.__ix = parser.parse_word()
        self.__iy = parser.parse_word()
        self.__alt_bc = parser.parse_word()
        self.__alt_de = parser.parse_word()
        self.__alt_hl = parser.parse_word()
        self.__alt_af = parser.parse_word()
        self.__ir = parser.parse_word()
        self.__iff1 = parser.parse_byte()
        self.__iff2 = parser.parse_byte()
        self.__int_disabled = parser.parse_byte()
        self.__halted = parser.parse_byte()
        self.__int_mode = parser.parse_byte()
        self.__index_rp_kind = parser.parse_byte()
        self._StateBase__parse_memory(parser)


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
