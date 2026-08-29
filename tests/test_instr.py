#   Z80 CPU Emulator.
#   https://github.com/kosarev/z80
#
#   Copyright (C) 2026 Ivan Kosarev.
#   mail@ivankosarev.com
#
#   Published under the MIT license.

import pytest

import z80


def test_const() -> None:
    c = z80.Const(5)
    assert isinstance(c, z80.Expr)
    assert str(c) == '0x5'
    assert repr(c) == 'Const(5)'
    assert c.evaluate() == 5


def test_add() -> None:
    # Numeric operands cast to Const leaves automatically.
    e = z80.Add(z80.IX, 5)
    assert isinstance(e, z80.BinaryExpr)
    assert isinstance(e, z80.Expr)
    assert str(e) == 'ix + 0x5'
    assert repr(e) == 'Add(IX, Const(5))'


def test_add_negative_offset() -> None:
    e = z80.Add(z80.IY, -5)
    assert str(e) == 'iy - 0x5'
    assert repr(e) == 'Add(IY, Const(-5))'


def test_symbol() -> None:
    s = z80.Symbol('main')
    assert isinstance(s, z80.Expr)
    assert str(s) == 'main'
    assert repr(s) == "Symbol('main')"
    assert str(z80.JP(s)) == 'jp main'


def test_symbol_evaluation() -> None:
    s = z80.Symbol('main')
    with pytest.raises(z80.Error, match="Undefined symbol 'main'."):
        s.evaluate()

    s.value = 0x8000
    assert s.evaluate() == 0x8000

    s2 = z80.Symbol('start', 0x4000)
    assert repr(s2) == "Symbol('start', 16384)"
    assert s2.evaluate() == 0x4000


def test_add_evaluation() -> None:
    s = z80.Symbol('main', 0x8000)
    assert z80.Add(s, 5).evaluate() == 0x8005
    assert z80.Add(z80.Add(s, 1), -2).evaluate() == 0x7fff


def test_non_value_evaluation() -> None:
    with pytest.raises(z80.Error, match="'ix' is not a value."):
        z80.Add(z80.IX, 5).evaluate()


def test_op_casting() -> None:
    i = z80.LD(z80.A, 5)
    assert repr(i) == 'LD(A, Const(5))'
    assert str(i) == 'ld a, 0x5'

    m = z80.At(0x1234)
    assert repr(m) == 'At((Const(4660),))'
    assert str(m) == '(0x1234)'
