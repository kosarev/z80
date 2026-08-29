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
    (z80.RST(0x00), b'\xc7'),
    (z80.RST(0x08), b'\xcf'),
    (z80.RST(0x10), b'\xd7'),
    (z80.RST(0x18), b'\xdf'),
    (z80.RST(0x20), b'\xe7'),
    (z80.RST(0x28), b'\xef'),
    (z80.RST(0x30), b'\xf7'),
    (z80.RST(0x38), b'\xff'),
    (z80.RST(z80.Symbol('main', 0x28)), b'\xef'),
    (z80.IM(0), b'\xed\x46'),
    (z80.IM(1), b'\xed\x56'),
    (z80.IM(2), b'\xed\x5e'),
    (z80.XPREFIX(0xdd), b'\xdd'),
    (z80.XPREFIX(0xfd), b'\xfd'),
    (z80.EX(z80.AF, z80.AF2), b'\x08'),
    (z80.EX(z80.DE, z80.HL), b'\xeb'),
    (z80.EX(z80.At(z80.SP), z80.HL), b'\xe3'),
    (z80.EX(z80.At(z80.SP), z80.IX), b'\xdd\xe3'),
    (z80.EX(z80.At(z80.SP), z80.IY), b'\xfd\xe3'),
    (z80.JP(z80.At(z80.HL)), b'\xe9'),
    (z80.JP(z80.At(z80.IX)), b'\xdd\xe9'),
    (z80.JP(z80.At(z80.IY)), b'\xfd\xe9'),
    (z80.RET(), b'\xc9'),
    (z80.IN(z80.At(z80.C)), b'\xed\x70'),
    (z80.OUT(z80.At(z80.C), 0), b'\xed\x71'),
    (z80.LD(z80.A, z80.At(z80.BC)), b'\x0a'),
    (z80.LD(z80.A, z80.At(z80.DE)), b'\x1a'),
    (z80.LD(z80.At(z80.BC), z80.A), b'\x02'),
    (z80.LD(z80.At(z80.DE), z80.A), b'\x12'),
    (z80.LD(z80.SP, z80.HL), b'\xf9'),
    (z80.LD(z80.SP, z80.IX), b'\xdd\xf9'),
    (z80.LD(z80.SP, z80.IY), b'\xfd\xf9'),
    (z80.LD(z80.IReg, z80.A), b'\xed\x47'),
    (z80.LD(z80.R, z80.A), b'\xed\x4f'),
    (z80.LD(z80.A, z80.IReg), b'\xed\x57'),
    (z80.LD(z80.A, z80.R), b'\xed\x5f'),
    (z80.XNEG(0xed4c), b'\xed\x4c'),
    (z80.XNEG(0xed54), b'\xed\x54'),
    (z80.XNEG(0xed7c), b'\xed\x7c'),
    (z80.XRETN(0xed55), b'\xed\x55'),
    (z80.XRETN(0xed5d), b'\xed\x5d'),
    (z80.XRETN(0xed7d), b'\xed\x7d'),
    (z80.XIM(0xed4e, 0), b'\xed\x4e'),
    (z80.XIM(0xed66, 0), b'\xed\x66'),
    (z80.XIM(0xed6e, 0), b'\xed\x6e'),
    (z80.XIM(0xed76, 1), b'\xed\x76'),
    (z80.XIM(0xed7e, 2), b'\xed\x7e'),
    (z80.PUSH(z80.BC), b'\xc5'),
    (z80.PUSH(z80.DE), b'\xd5'),
    (z80.PUSH(z80.HL), b'\xe5'),
    (z80.PUSH(z80.AF), b'\xf5'),
    (z80.PUSH(z80.IX), b'\xdd\xe5'),
    (z80.PUSH(z80.IY), b'\xfd\xe5'),
    (z80.POP(z80.BC), b'\xc1'),
    (z80.POP(z80.DE), b'\xd1'),
    (z80.POP(z80.HL), b'\xe1'),
    (z80.POP(z80.AF), b'\xf1'),
    (z80.POP(z80.IX), b'\xdd\xe1'),
    (z80.POP(z80.IY), b'\xfd\xe1'),
    (z80.INC(z80.B), b'\x04'),
    (z80.INC(z80.A), b'\x3c'),
    (z80.INC(z80.At(z80.HL)), b'\x34'),
    (z80.INC(z80.IXH), b'\xdd\x24'),
    (z80.INC(z80.IYL), b'\xfd\x2c'),
    (z80.INC(z80.BC), b'\x03'),
    (z80.INC(z80.SP), b'\x33'),
    (z80.INC(z80.IX), b'\xdd\x23'),
    (z80.INC(z80.IY), b'\xfd\x23'),
    (z80.DEC(z80.B), b'\x05'),
    (z80.DEC(z80.A), b'\x3d'),
    (z80.DEC(z80.At(z80.HL)), b'\x35'),
    (z80.DEC(z80.IXH), b'\xdd\x25'),
    (z80.DEC(z80.IYL), b'\xfd\x2d'),
    (z80.DEC(z80.BC), b'\x0b'),
    (z80.DEC(z80.SP), b'\x3b'),
    (z80.DEC(z80.IX), b'\xdd\x2b'),
    (z80.DEC(z80.IY), b'\xfd\x2b'),
    (z80.XNOP(0xed00), b'\xed\x00'),
    (z80.XNOP(0xed77), b'\xed\x77'),
    (z80.XNOP(0xed7f), b'\xed\x7f'),
    (z80.XNOP(0xeda4), b'\xed\xa4'),
    (z80.XNOP(0xedff), b'\xed\xff'),
)


@pytest.mark.parametrize('instr, image', _TESTS, ids=str)
def test_encode(instr: z80.Instr, image: bytes) -> None:
    assert instr.encode() == image


def test_no_such_form() -> None:
    with pytest.raises(z80.InstrError,
                       match='The target must be one of '
                             r'0x00, 0x08, \.\.\., 0x38.') as exc_info:
        z80.RST(0x29)

    assert str(exc_info.value.instr) == 'rst 0x29'

    with pytest.raises(z80.Error, match="Undefined symbol 'main'."):
        z80.RST(z80.Symbol('main'))

    with pytest.raises(z80.InstrError,
                       match='The mode must be 0, 1 or 2.'):
        z80.IM(3)

    with pytest.raises(z80.InstrError,
                       match='The prefix byte must be 0xdd or 0xfd.'):
        z80.XPREFIX(0x12)

    with pytest.raises(z80.InvalidInstr) as exc_info:
        z80.EX(z80.HL, z80.DE)

    assert str(exc_info.value.instr) == 'ex hl, de'

    with pytest.raises(z80.InvalidInstr):
        z80.JP(z80.At(z80.BC))

    with pytest.raises(z80.InvalidInstr):
        z80.JP(z80.HL)

    with pytest.raises(z80.InvalidInstr):
        z80.RET(z80.HL)

    with pytest.raises(z80.InvalidInstr):
        z80.IN(z80.B, z80.At(0x12))

    with pytest.raises(z80.InvalidInstr):
        z80.IN(z80.At(z80.B))

    with pytest.raises(z80.InvalidInstr):
        z80.OUT(z80.At(z80.C), 1)

    with pytest.raises(z80.InvalidInstr):
        z80.OUT(z80.At(0x12), z80.B)

    with pytest.raises(z80.InvalidInstr):
        z80.LD(z80.IReg, z80.B)

    with pytest.raises(z80.InvalidInstr):
        z80.LD(z80.HL, z80.DE)

    with pytest.raises(z80.InvalidInstr):
        z80.LD(z80.B, z80.HL)

    with pytest.raises(z80.InvalidInstr):
        z80.LD(z80.HL, z80.B)

    # ED 44 is the true neg, not a duplicate.
    with pytest.raises(z80.InvalidInstr):
        z80.XNEG(0xed44)

    with pytest.raises(z80.InvalidInstr):
        z80.XNEG(0xed4d)

    # ED 45 is the true retn and ED 4D is reti.
    with pytest.raises(z80.InvalidInstr):
        z80.XRETN(0xed45)

    with pytest.raises(z80.InvalidInstr):
        z80.XRETN(0xed4d)

    # ED 46 is a true im encoding, and ED 4E means mode 0.
    with pytest.raises(z80.InvalidInstr):
        z80.XIM(0xed46, 0)

    with pytest.raises(z80.InvalidInstr):
        z80.XIM(0xed4e, 1)

    with pytest.raises(z80.InvalidInstr):
        z80.PUSH(z80.SP)

    with pytest.raises(z80.InvalidInstr):
        z80.POP(z80.SP)

    with pytest.raises(z80.InvalidInstr):
        z80.INC(z80.AF)

    with pytest.raises(z80.InvalidInstr):
        z80.INC(z80.At(z80.BC))

    with pytest.raises(z80.InvalidInstr):
        z80.DEC(z80.AF)

    # ED 44 is neg and ED A0 is ldi: assigned, so not xnops.
    with pytest.raises(z80.InvalidInstr):
        z80.XNOP(0xed44)

    with pytest.raises(z80.InvalidInstr):
        z80.XNOP(0xeda0)


def test_no_encoding() -> None:
    with pytest.raises(z80.InstrError, match='No encoding.') as exc_info:
        z80.LD(z80.A, z80.B).encode()

    assert str(exc_info.value.instr) == 'ld a, b'
