
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


def test_define_symbol() -> None:
    code = z80.Code()
    port = code.define_symbol('port', 0xfe)
    assert port is code.get_symbol('port')
    assert port.evaluate() == 0xfe

    with pytest.raises(z80.Error,
                       match="Symbol 'port' is already defined."):
        code.define_symbol('port', 0xff)


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
    code.start_block(0x8000)
    code.add(z80.LabelDef(start), z80.NOP())
    code.start_block(z80.Add(start, 0x100))
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
    code.start_block(z80.Add(base, 2))
    code.add(z80.HALT())
    code.start_block(0x6000)
    code.add(z80.LabelDef(base), z80.NOP())
    code.resolve()
    assert code.encode() == [(0x6002, b'\x76'), (0x6000, b'\x00')]


# Data directives lay out like instructions, so labels bind past
# them at their shape-determined offsets.
def test_db() -> None:
    code = z80.Code()
    after = code.get_symbol('after')
    fill = code.get_symbol('fill')
    code.add(z80.Db(1, 2, fill), z80.LabelDef(after), z80.NOP())
    fill.define(0xff)
    code.resolve()
    assert after.value == 3
    assert code.encode() == [(0, b'\x01\x02\xff\x00')]

    code = z80.Code()
    code.add(z80.Db(0x100))
    with pytest.raises(z80.Error, match='0x100 does not fit a byte.'):
        code.encode()


def test_unresolvable_block_address() -> None:
    code = z80.Code()
    code.start_block(code.get_symbol('nowhere'))
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
