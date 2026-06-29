# -*- coding: utf-8 -*-

#   Z80 CPU Emulator.
#   https://github.com/kosarev/z80
#
#   Copyright (C) 2017-2026 Ivan Kosarev.
#   mail@ivankosarev.com
#
#   Published under the MIT license.

import pytest
import z80


_TESTS = (
    (b'\xcb\x30', 'sll b'),
    (b'\xdd\x24', 'inc ixh'),
    (b'\xdd\x2c', 'inc ixl'),
    (b'\xfd\x24', 'inc iyh'),
    (b'\xfd\x2c', 'inc iyl'),
    (b'\xdd\xe5', 'push ix'),
    (b'\xfd\xe1', 'pop iy'),
    (b'\xdd\xe8', 'ret pe'),
    (b'\xed\x00', 'xnop 0xed00'),
    (b'\xed\x45', 'retn'),
    (b'\xed\x55', 'xretn 0xed55'),
    (b'\xed\x4d', 'reti'),
    (b'\xed\x4c', 'xneg 0xed4c'),
    (b'\xed\x4e', 'xim 0xed4e, 0x0'),
    (b'\xed\x4f', 'ld r, a'),
    (b'\xed\x50', 'in d, (c)'),
    (b'\xed\x63\x00\x00', 'xld 0xed63, (0x0), hl'),
    (b'\xed\xa3', 'outi'),
    (b'\xed\xb3', 'otir'),
    (b'\xed\xab', 'outd'),
    (b'\xed\xbb', 'otdr'),
    (b'\xed\x67', 'rrd'),
    (b'\xed\xa0', 'ldi'),
    (b'\xed\xa1', 'cpi'),
    (b'\xed\xb1', 'cpir'),
    (b'\xed\xb9', 'cpdr'),
    (b'\xed\xa9', 'cpd'),
    (b'\xed\xa2', 'ini'),
    (b'\xed\xb2', 'inir'),
    (b'\xed\xaa', 'ind'),
    (b'\xed\xba', 'indr'),
    (b'\xed\xa8', 'ldd'),
)


@pytest.mark.parametrize('code, text', _TESTS)
def test_instr_builder(code: bytes, text: str) -> None:
    instr = z80.Z80InstrBuilder().build_instr(0, code)
    if isinstance(instr, z80.UnknownInstr):
        pytest.fail(f'unknown instruction: {instr.text}')

    assert str(instr) == text
