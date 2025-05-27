
#   Z80 CPU Emulator.
#   https://github.com/kosarev/z80
#
#   Copyright (C) 2017-2025 Ivan Kosarev.
#   mail@ivankosarev.com
#
#   Published under the MIT license.

import typing

from ._instr import HL, IX, IY
from ._instr import Reg
from ._z80 import _I8080Machine, _Z80Machine

WritableBytes: typing.TypeAlias = bytearray | memoryview
Bytes: typing.TypeAlias = bytes | WritableBytes


def _get_u16(image: Bytes) -> int:
    return image[0] + image[1] * 0x100


def _set_u16(image: WritableBytes, value: int) -> None:
    image[0] = value % 0x100
    image[1] = value // 0x100


def _get_u32(image: Bytes) -> int:
    return (image[0] + image[1] * 0x100 +
            image[2] * 0x10000 + image[2] * 0x1000000)


def _set_u32(image: WritableBytes, value: int) -> None:
    image[0] = value % 0x100
    image[1] = (value // 0x100) % 0x100
    image[2] = (value // 0x10000) % 0x100
    image[3] = value // 0x1000000


class _ImageParser(object):
    def __init__(self, image: Bytes) -> None:
        self.__image = image

    def parse_block(self, size: int) -> Bytes:
        assert len(self.__image) >= size
        block = self.__image[0:size]
        self.__image = self.__image[size:]
        return block

    def parse_rest(self) -> Bytes:
        return self.parse_block(len(self.__image))

    def parse_byte(self) -> Bytes:
        return self.parse_block(1)

    def parse_word(self) -> Bytes:
        return self.parse_block(2)

    def parse_u32(self) -> Bytes:
        return self.parse_block(4)


def _rename_attr_to_silence_pip8_e741(old: typing.Any,
                                      new: typing.Any) -> typing.Any:
    def rename(cls: typing.Any) -> typing.Any:
        setattr(cls, new, getattr(cls, old))
        delattr(cls, old)
        return cls
    return rename


@_rename_attr_to_silence_pip8_e741('plain_l', 'l')
class _StateBase(object):
    def __init__(self, image: Bytes) -> None:
        self._image = image

    def _parse_common_fields(self, parser: _ImageParser) -> None:
        self.__bc = parser.parse_word()
        self.__de = parser.parse_word()
        self.__hl = parser.parse_word()
        self.__af = parser.parse_word()
        self.__pc = parser.parse_word()
        self.__sp = parser.parse_word()
        self.__wz = parser.parse_word()
        self.__last_read_addr = parser.parse_word()
        self.__ticks_to_stop = parser.parse_u32()

        self.__a = self.__af[1:2]
        self.__b = self.__bc[1:2]
        self.__c = self.__bc[0:1]
        self.__d = self.__de[1:2]
        self.__e = self.__de[0:1]
        self.__f = self.__af[0:1]
        self.__h = self.__hl[1:2]
        self.__l = self.__hl[0:1]

    def _parse_memory(self, parser: _ImageParser) -> None:
        block = parser.parse_rest()
        assert len(block) == 0x10000, len(block)
        self.memory = block

    @property
    def af(self) -> int:
        return _get_u16(self.__af)

    @af.setter
    def af(self, value: int) -> None:
        assert isinstance(self.__af, WritableBytes)
        _set_u16(self.__af, value)

    @property
    def a(self) -> int:
        return self.__a[0]

    @a.setter
    def a(self, value: int) -> None:
        assert isinstance(self.__a, WritableBytes)
        self.__a[0] = value

    @property
    def b(self) -> int:
        return self.__b[0]

    @b.setter
    def b(self, value: int) -> None:
        assert isinstance(self.__b, WritableBytes)
        self.__b[0] = value

    @property
    def c(self) -> int:
        return self.__c[0]

    @c.setter
    def c(self, value: int) -> None:
        assert isinstance(self.__c, WritableBytes)
        self.__c[0] = value

    @property
    def d(self) -> int:
        return self.__d[0]

    @d.setter
    def d(self, value: int) -> None:
        assert isinstance(self.__d, WritableBytes)
        self.__d[0] = value

    @property
    def e(self) -> int:
        return self.__e[0]

    @e.setter
    def e(self, value: int) -> None:
        assert isinstance(self.__e, WritableBytes)
        self.__e[0] = value

    @property
    def f(self) -> int:
        return self.__f[0]

    @f.setter
    def f(self, value: int) -> None:
        assert isinstance(self.__f, WritableBytes)
        self.__f[0] = value

    @property
    def h(self) -> int:
        return self.__h[0]

    @h.setter
    def h(self, value: int) -> None:
        assert isinstance(self.__h, WritableBytes)
        self.__h[0] = value

    # The class decorator above renames this to just 'l'.
    @property
    def plain_l(self) -> int:
        return self.__l[0]

    @plain_l.setter
    def plain_l(self, value: int) -> None:
        assert isinstance(self.__l, WritableBytes)
        self.__l[0] = value

    @property
    def bc(self) -> int:
        return _get_u16(self.__bc)

    @bc.setter
    def bc(self, value: int) -> None:
        assert isinstance(self.__bc, WritableBytes)
        _set_u16(self.__bc, value)

    @property
    def de(self) -> int:
        return _get_u16(self.__de)

    @de.setter
    def de(self, value: int) -> None:
        assert isinstance(self.__de, WritableBytes)
        _set_u16(self.__de, value)

    @property
    def hl(self) -> int:
        return _get_u16(self.__hl)

    @hl.setter
    def hl(self, value: int) -> None:
        assert isinstance(self.__hl, WritableBytes)
        _set_u16(self.__hl, value)

    @property
    def pc(self) -> int:
        return _get_u16(self.__pc)

    @pc.setter
    def pc(self, value: int) -> None:
        assert isinstance(self.__pc, WritableBytes)
        _set_u16(self.__pc, value)

    @property
    def sp(self) -> int:
        return _get_u16(self.__sp)

    @sp.setter
    def sp(self, value: int) -> None:
        assert isinstance(self.__sp, WritableBytes)
        _set_u16(self.__sp, value)

    @property
    def ticks_to_stop(self) -> int:
        return _get_u32(self.__ticks_to_stop)

    @ticks_to_stop.setter
    def ticks_to_stop(self, value: int) -> None:
        assert isinstance(self.__ticks_to_stop, WritableBytes)
        _set_u32(self.__ticks_to_stop, value)

    def set_memory_block(self, addr: int, block: Bytes) -> None:
        assert isinstance(self.memory, WritableBytes)
        self.memory[addr:addr + len(block)] = block


class I8080State(_StateBase):
    def __init__(self, image: Bytes) -> None:
        _StateBase.__init__(self, image)

        parser = _ImageParser(image)
        self._parse_common_fields(parser)
        self.__iff = parser.parse_byte()
        self.__int_disabled = parser.parse_byte()
        self.__halted = parser.parse_byte()
        parser.parse_byte()  # Padding.
        self._parse_memory(parser)


class Z80State(_StateBase):
    def __init__(self, image: Bytes) -> None:
        super().__init__(image)

        parser = _ImageParser(image)
        self._parse_common_fields(parser)
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
        self._parse_memory(parser)

    @property
    def ix(self) -> int:
        return _get_u16(self.__ix)

    @ix.setter
    def ix(self, value: int) -> None:
        assert isinstance(self.__ix, WritableBytes)
        _set_u16(self.__ix, value)

    @property
    def iy(self) -> int:
        return _get_u16(self.__iy)

    @iy.setter
    def iy(self, value: int) -> None:
        assert isinstance(self.__iy, WritableBytes)
        _set_u16(self.__iy, value)

    @property
    def alt_bc(self) -> int:
        return _get_u16(self.__alt_bc)

    @alt_bc.setter
    def alt_bc(self, value: int) -> None:
        assert isinstance(self.__alt_bc, WritableBytes)
        _set_u16(self.__alt_bc, value)

    @property
    def alt_de(self) -> int:
        return _get_u16(self.__alt_de)

    @alt_de.setter
    def alt_de(self, value: int) -> None:
        assert isinstance(self.__alt_de, WritableBytes)
        _set_u16(self.__alt_de, value)

    @property
    def alt_hl(self) -> int:
        return _get_u16(self.__alt_hl)

    @alt_hl.setter
    def alt_hl(self, value: int) -> None:
        assert isinstance(self.__alt_hl, WritableBytes)
        _set_u16(self.__alt_hl, value)

    @property
    def int_disabled(self) -> bool:
        return bool(self.__int_disabled[0])

    @property
    def halted(self) -> bool:
        return bool(self.__halted[0])

    @halted.setter
    def halted(self, value: int) -> None:
        assert isinstance(self.__halted, WritableBytes)
        self.__halted[0] = int(value)

    @property
    def index_rp_kind(self) -> Reg:
        IREGPS = {0: HL, 1: IX, 2: IY}
        return IREGPS[self.__index_rp_kind[0]]


class _MachineBase(object):
    # Events.
    _NO_EVENTS = 0
    _END_OF_FRAME = 1 << 0
    _BREAKPOINT_HIT = 1 << 1

    # Address marks.
    _NO_MARKS = 0
    _BREAKPOINT_MARK = 1 << 0

    def mark_addrs(self, addr: int, size: int, marks: int) -> None:
        ...

    def mark_addr(self, addr: int, marks: int) -> None:
        self.mark_addrs(addr, 1, marks)

    def unmark_addrs(self, addr: int, size: int, marks: int) -> None:
        ...

    def unmark_addr(self, addr: int, marks: int) -> None:
        self.unmark_addrs(addr, 1, marks)

    def set_breakpoint(self, addr: int) -> None:
        self.mark_addr(addr, self._BREAKPOINT_MARK)

    def clear_breakpoint(self, addr: int) -> None:
        self.unmark_addr(addr, self._BREAKPOINT_MARK)


class I8080Machine(_I8080Machine, I8080State, _MachineBase):
    def __init__(self) -> None:
        I8080State.__init__(self, self.get_state_view())


class Z80Machine(_Z80Machine, Z80State, _MachineBase):
    def __init__(self) -> None:
        Z80State.__init__(self, self.get_state_view())
