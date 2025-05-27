#!/usr/bin/env python3
# -*- coding: utf-8 -*-

#   Z80 CPU Emulator.
#   https://github.com/kosarev/z80
#
#   Copyright (C) 2017-2025 Ivan Kosarev.
#   mail@ivankosarev.com
#
#   Published under the MIT license.

import typing

from ._source import _SourcePos


class _InstrElement(type):
    _str: str

    def __new__(cls: typing.Type[type], name: str, bases: tuple[type, ...],
                attrs: dict[str, typing.Any]) -> typing.Any:
        attrs.setdefault('_str', name.lower())
        return type.__new__(cls, name, bases, attrs)

    def __repr__(cls: '_InstrElement') -> str:
        return cls.__name__

    def __str__(cls: '_InstrElement') -> str:
        return cls._str


class CondFlag(_InstrElement):
    pass


class CF(metaclass=CondFlag):
    _str = 'c'


class NC(metaclass=CondFlag):
    pass


class M(metaclass=CondFlag):
    pass


class P(metaclass=CondFlag):
    pass


class PE(metaclass=CondFlag):
    pass


class PO(metaclass=CondFlag):
    pass


class Z(metaclass=CondFlag):
    pass


class NZ(metaclass=CondFlag):
    pass


class Reg(_InstrElement):
    pass


class IndexReg(Reg):
    pass


class A(metaclass=Reg):
    pass


class B(metaclass=Reg):
    pass


class C(metaclass=Reg):
    pass


class D(metaclass=Reg):
    pass


class E(metaclass=Reg):
    pass


class H(metaclass=Reg):
    pass


class L(metaclass=Reg):
    pass


class IReg(metaclass=Reg):
    _str = 'i'


class R(metaclass=Reg):
    pass


class AF(metaclass=Reg):
    pass


class AF2(metaclass=Reg):
    _str = "af'"


class BC(metaclass=Reg):
    pass


class DE(metaclass=Reg):
    pass


class HL(metaclass=Reg):
    pass


class IX(metaclass=IndexReg):
    pass


class IXH(metaclass=IndexReg):
    pass


class IXL(metaclass=IndexReg):
    pass


class IY(metaclass=IndexReg):
    pass


class IYH(metaclass=IndexReg):
    pass


class IYL(metaclass=IndexReg):
    pass


class SP(metaclass=Reg):
    pass


Op: typing.TypeAlias = typing.Union[int, str, Reg, 'At', 'Add']


def _str_op(op: Op) -> str:
    if isinstance(op, int):
        return '%#x' % op

    return str(op)


class At(object):
    def __init__(self, op: Op) -> None:
        self.ops = [op]

    def __repr__(self) -> str:
        return 'At(%r)' % tuple(self.ops)

    def __str__(self) -> str:
        op, = tuple(self.ops)
        return '(%s)' % _str_op(op)


class Add(object):
    def __init__(self, a: Op, b: Op) -> None:
        self.ops = [a, b]

    def __repr__(self) -> str:
        return 'Add(%r, %r)' % tuple(self.ops)

    def __str__(self) -> str:
        a, b = tuple(self.ops)

        sign = '+'
        if isinstance(b, int) and b < 0:
            sign = '-'
            b = -b

        return '%s %c %s' % (_str_op(a), sign, _str_op(b))


# Base class for all instructions.
class Instr(object):
    addr: int | None
    size: int | None
    origin: _SourcePos | None

    def __init__(self, *ops: Op, origin: int | None = None) -> None:
        self.addr = None
        self.size = None
        self.ops = ops
        self.origin = None

    def __repr__(self) -> str:
        return (type(self).__name__ +
                '(' + ', '.join(repr(op) for op in self.ops) + ')')

    def __str__(self) -> str:
        s = type(self).__name__.lower()
        if self.ops:
            s += ' ' + ', '.join(_str_op(op) for op in self.ops)
        return s


class UnknownInstr(Instr):
    text: str | None

    def __init__(self, addr: int, opcode: int) -> None:
        super().__init__()
        self.addr = addr
        self.size = 1
        self.opcode = opcode
        self.text = None

    def __str__(self) -> str:
        return 'db %#04x' % self.opcode


# Instructions that may affect control flow.
class JumpInstr(Instr):
    @property
    def target(self) -> Op:
        return self.ops[-1]

    @property
    def conditional(self) -> bool:
        if isinstance(self, DJNZ):
            return True

        return len(self.ops) > 0 and isinstance(self.ops[0], CondFlag)


class CallInstr(JumpInstr):
    pass


class RetInstr(JumpInstr):
    pass


class ADD(Instr):
    pass


class ADC(Instr):
    pass


class AND(Instr):
    pass


class BIT(Instr):
    pass


class CALL(CallInstr):
    pass


class CCF(Instr):
    pass


class CP(Instr):
    pass


class CPD(Instr):
    pass


class CPDR(Instr):
    pass


class CPI(Instr):
    pass


class CPIR(Instr):
    pass


class CPL(Instr):
    pass


class DAA(Instr):
    pass


class DEC(Instr):
    pass


class DI(Instr):
    pass


class DJNZ(JumpInstr):
    pass


class EI(Instr):
    pass


class EX(Instr):
    pass


class EXX(Instr):
    pass


class HALT(Instr):
    pass


class IM(Instr):
    pass


class XIM(Instr):
    pass


class IN(Instr):
    pass


class IND(Instr):
    pass


class INDR(Instr):
    pass


class INI(Instr):
    pass


class INIR(Instr):
    pass


class INC(Instr):
    pass


class JP(JumpInstr):
    pass


class JR(JumpInstr):
    pass


class LD(Instr):
    pass


class XLD(Instr):
    pass


class LDD(Instr):
    pass


class LDDR(Instr):
    pass


class LDI(Instr):
    pass


class LDIR(Instr):
    pass


class NEG(Instr):
    pass


class XNEG(Instr):
    pass


class NOP(Instr):
    pass


class XNOP(Instr):
    pass


class OR(Instr):
    pass


class OUT(Instr):
    pass


class OUTD(Instr):
    pass


class OTDR(Instr):
    pass


class OUTI(Instr):
    pass


class OTIR(Instr):
    pass


class POP(Instr):
    pass


class PUSH(Instr):
    pass


class RES(Instr):
    pass


class RET(RetInstr):
    pass


class RETN(RetInstr):
    pass


class XRETN(RetInstr):
    pass


class RETI(RetInstr):
    pass


class RL(Instr):
    pass


class RLA(Instr):
    pass


class RLC(Instr):
    pass


class RLCA(Instr):
    pass


class RLD(Instr):
    pass


class RR(Instr):
    pass


class RRA(Instr):
    pass


class RRD(Instr):
    pass


class RRC(Instr):
    pass


class RRCA(Instr):
    pass


class RST(CallInstr):
    pass


class SBC(Instr):
    pass


class SCF(Instr):
    pass


class SET(Instr):
    pass


class SLA(Instr):
    pass


class SLL(Instr):
    pass


class SRA(Instr):
    pass


class SRL(Instr):
    pass


class SUB(Instr):
    pass


class XOR(Instr):
    pass
