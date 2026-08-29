#   Z80 CPU Emulator.
#   https://github.com/kosarev/z80
#
#   Copyright (C) 2026 Ivan Kosarev.
#   mail@ivankosarev.com
#
#   Published under the MIT license.

import pytest

import z80

# The operand-free instructions and their fixed encodings.
_TESTS = (
    (z80.NOP(), b'\x00'),
    (z80.RLCA(), b'\x07'),
    (z80.RRCA(), b'\x0f'),
    (z80.RLA(), b'\x17'),
    (z80.RRA(), b'\x1f'),
    (z80.DAA(), b'\x27'),
    (z80.CPL(), b'\x2f'),
    (z80.SCF(), b'\x37'),
    (z80.CCF(), b'\x3f'),
    (z80.HALT(), b'\x76'),
    (z80.EXX(), b'\xd9'),
    (z80.DI(), b'\xf3'),
    (z80.EI(), b'\xfb'),
    (z80.NEG(), b'\xed\x44'),
    (z80.RETN(), b'\xed\x45'),
    (z80.RETI(), b'\xed\x4d'),
    (z80.RRD(), b'\xed\x67'),
    (z80.RLD(), b'\xed\x6f'),
    (z80.LDI(), b'\xed\xa0'),
    (z80.CPI(), b'\xed\xa1'),
    (z80.INI(), b'\xed\xa2'),
    (z80.OUTI(), b'\xed\xa3'),
    (z80.LDD(), b'\xed\xa8'),
    (z80.CPD(), b'\xed\xa9'),
    (z80.IND(), b'\xed\xaa'),
    (z80.OUTD(), b'\xed\xab'),
    (z80.LDIR(), b'\xed\xb0'),
    (z80.CPIR(), b'\xed\xb1'),
    (z80.INIR(), b'\xed\xb2'),
    (z80.OTIR(), b'\xed\xb3'),
    (z80.LDDR(), b'\xed\xb8'),
    (z80.CPDR(), b'\xed\xb9'),
    (z80.INDR(), b'\xed\xba'),
    (z80.OTDR(), b'\xed\xbb'),
)


@pytest.mark.parametrize('instr, image', _TESTS, ids=str)
def test_encode(instr: z80.Instr, image: bytes) -> None:
    assert instr.encode() == image


def test_no_encoding() -> None:
    with pytest.raises(z80.Error, match="No encoding for 'ld a, b'."):
        z80.LD(z80.A, z80.B).encode()
