
#   Z80 CPU Emulator.
#   https://github.com/kosarev/z80
#
#   Copyright (C) 2017-2026 Ivan Kosarev.
#   mail@ivankosarev.com
#
#   Published under the MIT license.

import typing

from ._instr import HL, IX, IY, Reg
from ._z80 import _I8080Machine, _Z80Machine

WritableBytes: typing.TypeAlias = bytearray | memoryview
Bytes: typing.TypeAlias = bytes | WritableBytes


class _ImageParser:
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


class _StateBase:
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
        self.__frame_tick = parser.parse_u32()

    def _parse_memory(self, parser: _ImageParser) -> None:
        block = parser.parse_rest()
        assert len(block) == 0x10000, len(block)
        self.memory = block

    @property
    def af(self) -> int:
        return int.from_bytes(self.__af, 'little')

    @af.setter
    def af(self, value: int) -> None:
        assert isinstance(self.__af, WritableBytes)
        self.__af[:] = value.to_bytes(2, 'little')

    @property
    def a(self) -> int:
        return self.__af[1]

    @a.setter
    def a(self, value: int) -> None:
        assert isinstance(self.__af, WritableBytes)
        self.__af[1] = value

    @property
    def b(self) -> int:
        return self.__bc[1]

    @b.setter
    def b(self, value: int) -> None:
        assert isinstance(self.__bc, WritableBytes)
        self.__bc[1] = value

    @property
    def c(self) -> int:
        return self.__bc[0]

    @c.setter
    def c(self, value: int) -> None:
        assert isinstance(self.__bc, WritableBytes)
        self.__bc[0] = value

    @property
    def d(self) -> int:
        return self.__de[1]

    @d.setter
    def d(self, value: int) -> None:
        assert isinstance(self.__de, WritableBytes)
        self.__de[1] = value

    @property
    def e(self) -> int:
        return self.__de[0]

    @e.setter
    def e(self, value: int) -> None:
        assert isinstance(self.__de, WritableBytes)
        self.__de[0] = value

    @property
    def f(self) -> int:
        return self.__af[0]

    @f.setter
    def f(self, value: int) -> None:
        assert isinstance(self.__af, WritableBytes)
        self.__af[0] = value

    @property
    def h(self) -> int:
        return self.__hl[1]

    @h.setter
    def h(self, value: int) -> None:
        assert isinstance(self.__hl, WritableBytes)
        self.__hl[1] = value

    @property
    def l(self) -> int:
        return self.__hl[0]

    @l.setter
    def l(self, value: int) -> None:
        assert isinstance(self.__hl, WritableBytes)
        self.__hl[0] = value

    @property
    def bc(self) -> int:
        return int.from_bytes(self.__bc, 'little')

    @bc.setter
    def bc(self, value: int) -> None:
        assert isinstance(self.__bc, WritableBytes)
        self.__bc[:] = value.to_bytes(2, 'little')

    @property
    def de(self) -> int:
        return int.from_bytes(self.__de, 'little')

    @de.setter
    def de(self, value: int) -> None:
        assert isinstance(self.__de, WritableBytes)
        self.__de[:] = value.to_bytes(2, 'little')

    @property
    def hl(self) -> int:
        return int.from_bytes(self.__hl, 'little')

    @hl.setter
    def hl(self, value: int) -> None:
        assert isinstance(self.__hl, WritableBytes)
        self.__hl[:] = value.to_bytes(2, 'little')

    @property
    def pc(self) -> int:
        return int.from_bytes(self.__pc, 'little')

    @pc.setter
    def pc(self, value: int) -> None:
        assert isinstance(self.__pc, WritableBytes)
        self.__pc[:] = value.to_bytes(2, 'little')

    @property
    def sp(self) -> int:
        return int.from_bytes(self.__sp, 'little')

    @sp.setter
    def sp(self, value: int) -> None:
        assert isinstance(self.__sp, WritableBytes)
        self.__sp[:] = value.to_bytes(2, 'little')

    @property
    def ticks_to_stop(self) -> int:
        return int.from_bytes(self.__ticks_to_stop, 'little')

    @ticks_to_stop.setter
    def ticks_to_stop(self, value: int) -> None:
        assert isinstance(self.__ticks_to_stop, WritableBytes)
        self.__ticks_to_stop[:] = value.to_bytes(4, 'little')

    @property
    def frame_tick(self) -> int:
        return int.from_bytes(self.__frame_tick, 'little')

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
        return int.from_bytes(self.__ix, 'little')

    @ix.setter
    def ix(self, value: int) -> None:
        assert isinstance(self.__ix, WritableBytes)
        self.__ix[:] = value.to_bytes(2, 'little')

    @property
    def ixh(self) -> int:
        return self.__ix[1]

    @ixh.setter
    def ixh(self, value: int) -> None:
        assert isinstance(self.__ix, WritableBytes)
        self.__ix[1] = value

    @property
    def ixl(self) -> int:
        return self.__ix[0]

    @ixl.setter
    def ixl(self, value: int) -> None:
        assert isinstance(self.__ix, WritableBytes)
        self.__ix[0] = value

    @property
    def iy(self) -> int:
        return int.from_bytes(self.__iy, 'little')

    @iy.setter
    def iy(self, value: int) -> None:
        assert isinstance(self.__iy, WritableBytes)
        self.__iy[:] = value.to_bytes(2, 'little')

    @property
    def iyh(self) -> int:
        return self.__iy[1]

    @iyh.setter
    def iyh(self, value: int) -> None:
        assert isinstance(self.__iy, WritableBytes)
        self.__iy[1] = value

    @property
    def iyl(self) -> int:
        return self.__iy[0]

    @iyl.setter
    def iyl(self, value: int) -> None:
        assert isinstance(self.__iy, WritableBytes)
        self.__iy[0] = value

    @property
    def alt_bc(self) -> int:
        return int.from_bytes(self.__alt_bc, 'little')

    @alt_bc.setter
    def alt_bc(self, value: int) -> None:
        assert isinstance(self.__alt_bc, WritableBytes)
        self.__alt_bc[:] = value.to_bytes(2, 'little')

    @property
    def alt_b(self) -> int:
        return self.__alt_bc[1]

    @alt_b.setter
    def alt_b(self, value: int) -> None:
        assert isinstance(self.__alt_bc, WritableBytes)
        self.__alt_bc[1] = value

    @property
    def alt_c(self) -> int:
        return self.__alt_bc[0]

    @alt_c.setter
    def alt_c(self, value: int) -> None:
        assert isinstance(self.__alt_bc, WritableBytes)
        self.__alt_bc[0] = value

    @property
    def alt_de(self) -> int:
        return int.from_bytes(self.__alt_de, 'little')

    @alt_de.setter
    def alt_de(self, value: int) -> None:
        assert isinstance(self.__alt_de, WritableBytes)
        self.__alt_de[:] = value.to_bytes(2, 'little')

    @property
    def alt_d(self) -> int:
        return self.__alt_de[1]

    @alt_d.setter
    def alt_d(self, value: int) -> None:
        assert isinstance(self.__alt_de, WritableBytes)
        self.__alt_de[1] = value

    @property
    def alt_e(self) -> int:
        return self.__alt_de[0]

    @alt_e.setter
    def alt_e(self, value: int) -> None:
        assert isinstance(self.__alt_de, WritableBytes)
        self.__alt_de[0] = value

    @property
    def alt_hl(self) -> int:
        return int.from_bytes(self.__alt_hl, 'little')

    @alt_hl.setter
    def alt_hl(self, value: int) -> None:
        assert isinstance(self.__alt_hl, WritableBytes)
        self.__alt_hl[:] = value.to_bytes(2, 'little')

    @property
    def alt_h(self) -> int:
        return self.__alt_hl[1]

    @alt_h.setter
    def alt_h(self, value: int) -> None:
        assert isinstance(self.__alt_hl, WritableBytes)
        self.__alt_hl[1] = value

    @property
    def alt_l(self) -> int:
        return self.__alt_hl[0]

    @alt_l.setter
    def alt_l(self, value: int) -> None:
        assert isinstance(self.__alt_hl, WritableBytes)
        self.__alt_hl[0] = value

    @property
    def alt_af(self) -> int:
        return int.from_bytes(self.__alt_af, 'little')

    @alt_af.setter
    def alt_af(self, value: int) -> None:
        assert isinstance(self.__alt_af, WritableBytes)
        self.__alt_af[:] = value.to_bytes(2, 'little')

    @property
    def alt_a(self) -> int:
        return self.__alt_af[1]

    @alt_a.setter
    def alt_a(self, value: int) -> None:
        assert isinstance(self.__alt_af, WritableBytes)
        self.__alt_af[1] = value

    @property
    def alt_f(self) -> int:
        return self.__alt_af[0]

    @alt_f.setter
    def alt_f(self, value: int) -> None:
        assert isinstance(self.__alt_af, WritableBytes)
        self.__alt_af[0] = value

    @property
    def ir(self) -> int:
        return int.from_bytes(self.__ir, 'little')

    @ir.setter
    def ir(self, value: int) -> None:
        assert isinstance(self.__ir, WritableBytes)
        self.__ir[:] = value.to_bytes(2, 'little')

    @property
    def i(self) -> int:
        return self.__ir[1]

    @i.setter
    def i(self, value: int) -> None:
        assert isinstance(self.__ir, WritableBytes)
        self.__ir[1] = value

    @property
    def r(self) -> int:
        return self.__ir[0]

    @r.setter
    def r(self, value: int) -> None:
        assert isinstance(self.__ir, WritableBytes)
        self.__ir[0] = value

    @property
    def iff1(self) -> bool:
        return bool(self.__iff1[0])

    @iff1.setter
    def iff1(self, value: int) -> None:
        assert isinstance(self.__iff1, WritableBytes)
        self.__iff1[0] = int(value)

    @property
    def iff2(self) -> bool:
        return bool(self.__iff2[0])

    @iff2.setter
    def iff2(self, value: int) -> None:
        assert isinstance(self.__iff2, WritableBytes)
        self.__iff2[0] = int(value)

    # Tells whether the CPU is at a point where maskable interrupts
    # cannot be accepted, e.g., just after an EI instruction. Not to
    # be confused with iff1, which tells whether maskable interrupts
    # are enabled (issue #69).
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


class _MachineBase:
    # Events. The bit values follow the composition of the modules
    # declaring them; the bits not listed (retry_input,
    # stop_requested) are internal and not exposed here.
    _NO_EVENTS = 0
    _BREAKPOINT_HIT = 1 << 0
    _TICKS_LIMIT_HIT = 1 << 2
    _END_OF_FRAME = 1 << 3

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
