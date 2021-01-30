
#   Z80 CPU Emulator.
#   https://github.com/kosarev/z80
#
#   Copyright (C) 2017-2019 Ivan Kosarev.
#   ivan@kosarev.info
#
#   Published under the MIT license.

from ._z80 import _I8080Machine, _Z80Machine


def _get_u16(image):
    return image[0] + image[1] * 0x100


def _set_u16(image, value):
    image[0] = value % 0x100
    image[1] = value // 0x100


def _get_u32(image):
    return (self.__image[0] + self.__image[1] * 0x100 +
            self.__image[2] * 0x10000 + self.__image[2] * 0x1000000)


def _set_u32(image, value):
    image[0] = value % 0x100
    image[1] = (value // 0x100) % 0x100
    image[2] = (value // 0x10000) % 0x100
    image[3] = value // 0x1000000


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
        return self.parse_block(1)

    def parse_word(self):
        return self.parse_block(2)

    def parse_u32(self):
        return self.parse_block(4)


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

        self.__a = self.__af[0:1]
        self.__b = self.__bc[1:2]
        self.__c = self.__bc[0:1]
        self.__d = self.__de[1:2]
        self.__e = self.__de[0:1]
        self.__f = self.__af[0:1]
        self.__h = self.__hl[1:2]
        self.__l = self.__hl[0:1]

    def __parse_memory(self, parser):
        block = parser.parse_rest()
        assert len(block) == 0x10000, len(block)
        self.memory = block

    @property
    def b(self):
        return self.__b[0]

    @b.setter
    def b(self, value):
        self.__b[0] = value

    @property
    def c(self):
        return self.__c[0]

    @c.setter
    def c(self, value):
        self.__c[0] = value

    @property
    def d(self):
        return self.__d[0]

    @d.setter
    def d(self, value):
        self.__d[0] = value

    @property
    def e(self):
        return self.__e[0]

    @e.setter
    def e(self, value):
        self.__e[0] = value

    @property
    def bc(self):
        return _get_u16(self.__bc)

    @bc.setter
    def bc(self, value):
        _set_u16(self.__bc, value)

    @property
    def de(self):
        return _get_u16(self.__de)

    @de.setter
    def de(self, value):
        _set_u16(self.__de, value)

    @property
    def hl(self):
        return _get_u16(self.__hl)

    @hl.setter
    def hl(self, value):
        _set_u16(self.__hl, value)

    @property
    def pc(self):
        return _get_u16(self.__pc)

    @pc.setter
    def pc(self, value):
        _set_u16(self.__pc, value)

    @property
    def sp(self):
        return _get_u16(self.__sp)

    @sp.setter
    def sp(self, value):
        _set_u16(self.__sp, value)

    @property
    def ticks_to_stop(self):
        return _get_u32(self.__tick_to_stop)

    @ticks_to_stop.setter
    def ticks_to_stop(self, value):
        _set_u32(self.__ticks_to_stop, value)

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
