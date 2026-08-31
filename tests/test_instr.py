#   Z80 CPU Emulator.
#   https://github.com/kosarev/z80
#
#   Copyright (C) 2026 Ivan Kosarev.
#   mail@ivankosarev.com
#
#   Published under the MIT license.

import pytest

import z80


# The sanctioned creation path for symbols is their context's
# get_symbol(); a fresh context per symbol serves these tests.
def _symbol(name: str, value: int | None = None) -> z80.Symbol:
    symbol = z80.Code().get_symbol(name)
    if value is not None:
        symbol.define(value)

    return symbol


def test_const() -> None:
    c = z80.Const(5)
    assert isinstance(c, z80.Expr)
    assert str(c) == '0x5'
    assert repr(c) == 'Const(5)'
    assert c.evaluate() == 5


def test_add() -> None:
    # Numeric operands cast to Const leaves automatically.
    e = z80.Add(_symbol('main'), 5)
    assert isinstance(e, z80.BinaryExpr)
    assert isinstance(e, z80.Expr)
    assert str(e) == 'main + 0x5'
    assert repr(e) == "Add(Symbol('main'), Const(5))"


def test_add_negative_operand() -> None:
    e = z80.Add(_symbol('main'), -5)
    assert str(e) == 'main - 0x5'
    assert repr(e) == "Add(Symbol('main'), Const(-5))"


def test_symbol() -> None:
    s = _symbol('main')
    assert isinstance(s, z80.Expr)
    assert str(s) == 'main'
    assert repr(s) == "Symbol('main')"
    assert str(z80.JP(s)) == 'jp main'


def test_symbol_evaluation() -> None:
    s = _symbol('main')
    with pytest.raises(z80.Error, match="Undefined symbol 'main'."):
        s.evaluate()

    s.define(0x8000)
    assert s.evaluate() == 0x8000

    with pytest.raises(z80.Error,
                       match="Symbol 'main' is already defined."):
        s.define(0x9000)

    s2 = _symbol('start', 0x4000)
    assert repr(s2) == "Symbol('start', 16384)"
    assert s2.evaluate() == 0x4000

    with pytest.raises(z80.Error,
                       match="Symbol 'start' is already defined."):
        s2.define(0x9000)


def test_add_evaluation() -> None:
    s = _symbol('main', 0x8000)
    assert z80.Add(s, 5).evaluate() == 0x8005
    assert z80.Add(z80.Add(s, 1), -2).evaluate() == 0x7fff


def test_sub() -> None:
    s = _symbol('main', 0x8000)
    e = z80.Sub(s, 5)
    assert isinstance(e, z80.BinaryExpr)
    assert str(e) == 'main - 0x5'
    assert repr(e) == "Sub(Symbol('main', 32768), Const(5))"
    assert e.evaluate() == 0x7ffb

    # A negative constant renders under the opposite sign.
    assert str(z80.Sub(s, -5)) == 'main + 0x5'

    # A compound right operand renders parenthesised.
    e = z80.Sub(s, z80.Add(s, 1))
    assert str(e) == 'main - (main + 0x1)'
    assert e.evaluate() == -1


def test_neg() -> None:
    s = _symbol('main', 0x8000)
    n = z80.Neg(s)
    assert isinstance(n, z80.UnaryExpr)
    assert isinstance(n, z80.Expr)
    assert str(n) == '-main'
    assert repr(n) == "Neg(Symbol('main', 32768))"
    assert n.evaluate() == -0x8000

    assert str(z80.Neg(5)) == '-0x5'
    assert str(z80.Neg(z80.Add(s, 1))) == '-(main + 0x1)'
    assert z80.Neg(z80.Add(s, 1)).evaluate() == -0x8001


def test_op_casting() -> None:
    i = z80.LD(z80.A, 5)
    assert repr(i) == 'LD(A, Const(5))'
    assert str(i) == 'ld a, 0x5'

    m = z80.At(0x1234)
    assert repr(m) == 'At((Const(4660),))'
    assert str(m) == '(0x1234)'


def test_at_displacement() -> None:
    m = z80.At(z80.IX, 5)
    assert repr(m) == 'At((IX, Const(5)))'
    assert str(m) == '(ix + 0x5)'
    assert m.reg is z80.IX
    assert isinstance(m.disp, z80.Const) and m.disp.value == 5

    assert str(z80.At(z80.IY, -5)) == '(iy - 0x5)'

    # (i) means (i + 0); plain addresses have no register.
    m = z80.At(z80.IX)
    assert m.reg is z80.IX
    assert isinstance(m.disp, z80.Const) and m.disp.value == 0

    m = z80.At(0x1234)
    assert m.reg is None and m.disp is None

    with pytest.raises(z80.Error,
                       match='Only ix and iy take displacements.'):
        z80.At(z80.HL, 5)
