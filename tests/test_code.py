
#   Z80 CPU Emulator.
#   https://github.com/kosarev/z80
#
#   Copyright (C) 2026 Ivan Kosarev.
#   mail@ivankosarev.com
#
#   Published under the MIT license.

import pytest

import z80


def test_symbol_interning() -> None:
    code = z80.Code()
    assert code.get_symbol('main') is code.get_symbol('main')


def test_forward_reference() -> None:
    code = z80.Code()
    loop = code.get_symbol('loop')
    skip = code.get_symbol('skip')
    code.add(z80.JP(skip))
    code.add(z80.LabelDef(loop), z80.HALT())
    code.add(z80.LabelDef(skip), z80.JR(loop))
    code.resolve()
    assert loop.value == 3
    assert skip.value == 4
    assert code.encode() == [(0, b'\xc3\x04\x00\x76\x18\xfd')]


def test_symbolic_block_address() -> None:
    code = z80.Code()
    start = code.get_symbol('start')
    entry = code.get_symbol('entry')
    code.org(0x8000)
    code.add(z80.LabelDef(start), z80.NOP())
    code.org(z80.Add(start, 0x100))
    code.add(z80.LabelDef(entry), z80.RET())
    code.resolve()
    assert entry.value == 0x8100
    assert code.encode() == [(0x8000, b'\x00'), (0x8100, b'\xc9')]


# A block whose address depends on a label a later block defines
# lays out on a later round of the worklist; the produced blocks
# still follow the intake order.
def test_block_dependency_order() -> None:
    code = z80.Code()
    base = code.get_symbol('base')
    code.org(z80.Add(base, 2))
    code.add(z80.HALT())
    code.org(0x6000)
    code.add(z80.LabelDef(base), z80.NOP())
    code.resolve()
    assert code.encode() == [(0x6002, b'\x76'), (0x6000, b'\x00')]


def test_unresolvable_block_address() -> None:
    code = z80.Code()
    code.org(code.get_symbol('nowhere'))
    code.add(z80.NOP())
    with pytest.raises(z80.Error,
                       match="Undefined symbol 'nowhere'."):
        code.resolve()


def test_duplicate_label() -> None:
    code = z80.Code()
    twice = code.get_symbol('twice')
    code.add(z80.LabelDef(twice), z80.NOP(), z80.LabelDef(twice))
    with pytest.raises(z80.Error,
                       match="Symbol 'twice' is already defined."):
        code.resolve()
