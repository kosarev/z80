#   Z80 CPU Emulator.
#   https://github.com/kosarev/z80
#
#   Copyright (C) 2026 Ivan Kosarev.
#   mail@ivankosarev.com
#
#   Published under the MIT license.

import z80


def test_add() -> None:
    e = z80.Add(z80.IX, 5)
    assert isinstance(e, z80.BinaryExpr)
    assert isinstance(e, z80.Expr)
    assert str(e) == 'ix + 0x5'
    assert repr(e) == 'Add(IX, 5)'


def test_add_negative_offset() -> None:
    e = z80.Add(z80.IY, -5)
    assert str(e) == 'iy - 0x5'
    assert repr(e) == 'Add(IY, -5)'
