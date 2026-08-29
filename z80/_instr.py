#   Z80 CPU Emulator.
#   https://github.com/kosarev/z80
#
#   Copyright (C) 2017-2026 Ivan Kosarev.
#   mail@ivankosarev.com
#
#   Published under the MIT license.

import typing

from ._error import Error
from ._source import _SourcePos

# The public vocabulary re-exported by the package's __init__. This is the
# single source of truth for what 'import z80' exposes from this module.
__all__ = [
    'ADC',
    'ADD',
    'AF',
    'AF2',
    'AND',
    'BC',
    'BIT',
    'CALL',
    'CCF',
    'CF',
    'CP',
    'CPD',
    'CPDR',
    'CPI',
    'CPIR',
    'CPL',
    'DAA',
    'DE',
    'DEC',
    'DI',
    'DJNZ',
    'EI',
    'EX',
    'EXX',
    'HALT',
    'HL',
    'IM',
    'IN',
    'INC',
    'IND',
    'INDR',
    'INI',
    'INIR',
    'IX',
    'IXH',
    'IXL',
    'IY',
    'IYH',
    'IYL',
    'JP',
    'JR',
    'LD',
    'LDD',
    'LDDR',
    'LDI',
    'LDIR',
    'NC',
    'NEG',
    'NOP',
    'NZ',
    'OR',
    'OTDR',
    'OTIR',
    'OUT',
    'OUTD',
    'OUTI',
    'PE',
    'PO',
    'POP',
    'PUSH',
    'RES',
    'RET',
    'RETI',
    'RETN',
    'RL',
    'RLA',
    'RLC',
    'RLCA',
    'RLD',
    'RR',
    'RRA',
    'RRC',
    'RRCA',
    'RRD',
    'RST',
    'SBC',
    'SCF',
    'SET',
    'SLA',
    'SLL',
    'SP',
    'SRA',
    'SRL',
    'SUB',
    'XIM',
    'XLD',
    'XNEG',
    'XNOP',
    'XOR',
    'XPREFIX',
    'XRETN',
    'A',
    'Add',
    'At',
    'B',
    'BinaryExpr',
    'C',
    'CallInstr',
    'Const',
    'D',
    'E',
    'Expr',
    'H',
    'IReg',
    'IndexReg',
    'JumpInstr',
    'L',
    'M',
    'P',
    'R',
    'RetInstr',
    'Symbol',
    'UnknownInstr',
    'Z',
]


class _InstrElement(type):
    _str: str

    def __new__(cls: type[type], name: str, bases: tuple[type, ...],
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


# Registers holding a 16-bit value. Loading one takes an immediate
# wide enough to be an address, which the 8-bit registers cannot.
class Reg16(Reg):
    pass


class IndexReg(Reg):
    pass


# The index registers themselves, as opposed to their halves.
class IndexReg16(IndexReg, Reg16):
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


class AF(metaclass=Reg16):
    pass


class AF2(metaclass=Reg16):
    _str = "af'"


class BC(metaclass=Reg16):
    pass


class DE(metaclass=Reg16):
    pass


class HL(metaclass=Reg16):
    pass


class IX(metaclass=IndexReg16):
    pass


class IXH(metaclass=IndexReg):
    pass


class IXL(metaclass=IndexReg):
    pass


class IY(metaclass=IndexReg16):
    pass


class IYH(metaclass=IndexReg):
    pass


class IYL(metaclass=IndexReg):
    pass


class SP(metaclass=Reg16):
    pass


Op: typing.TypeAlias = typing.Union[str, Reg, CondFlag, 'At', 'Expr']


# How plain integers enter the operand domain.
def _cast_op(op: int | Op) -> Op:
    return Const(op) if isinstance(op, int) else op


# Base class for expression operands.
class Expr:
    ops: list[Op]

    def __init__(self, *ops: int | Op) -> None:
        self.ops = [_cast_op(op) for op in ops]

    def __repr__(self) -> str:
        return (type(self).__name__ +
                '(' + ', '.join(repr(op) for op in self.ops) + ')')

    def evaluate(self) -> int:
        raise NotImplementedError()

    # The value of a subexpression. Registers and other non-value
    # operands can only appear in addressing forms, which are no
    # subject of expression evaluation.
    @staticmethod
    def _evaluate_op(op: Op) -> int:
        if not isinstance(op, Expr):
            raise Error(f'{str(op)!r} is not a value.')

        return op.evaluate()


# A constant value leaf.
class Const(Expr):
    def __init__(self, value: int) -> None:
        super().__init__()
        self.value = value

    def __repr__(self) -> str:
        return f'Const({self.value!r})'

    def __str__(self) -> str:
        return f'{self.value:#x}'

    def evaluate(self) -> int:
        return self.value


class BinaryExpr(Expr):
    def __init__(self, a: int | Op, b: int | Op) -> None:
        super().__init__(a, b)


# A named value, possibly not defined yet.
class Symbol(Expr):
    def __init__(self, name: str, value: int | None = None) -> None:
        super().__init__()
        self.name = name
        self.value = value

    def __repr__(self) -> str:
        if self.value is None:
            return f'Symbol({self.name!r})'

        return f'Symbol({self.name!r}, {self.value!r})'

    def __str__(self) -> str:
        return self.name

    def evaluate(self) -> int:
        if self.value is None:
            raise Error(f'Undefined symbol {self.name!r}.')

        return self.value


class At:
    def __init__(self, op: int | Op) -> None:
        self.ops = [_cast_op(op)]

    def __repr__(self) -> str:
        return f'At({tuple(self.ops)!r})'

    def __str__(self) -> str:
        op, = tuple(self.ops)
        return f'({op})'


class Add(BinaryExpr):
    def __str__(self) -> str:
        a, b = tuple(self.ops)

        sign = '+'
        if isinstance(b, Const) and b.value < 0:
            sign = '-'
            b = Const(-b.value)

        return f'{a} {sign} {b}'

    def evaluate(self) -> int:
        a, b = tuple(self.ops)
        return self._evaluate_op(a) + self._evaluate_op(b)


# Base class for all instructions.
class Instr:
    addr: int | None
    size: int | None
    origin: _SourcePos | None

    def __init__(self, *ops: int | Op, origin: int | None = None) -> None:
        self.addr = None
        self.size = None
        self.ops = tuple(_cast_op(op) for op in ops)
        self.origin = None

    def __repr__(self) -> str:
        return (type(self).__name__ +
                '(' + ', '.join(repr(op) for op in self.ops) + ')')

    def __str__(self) -> str:
        s = type(self).__name__.lower()
        if self.ops:
            s += ' ' + ', '.join(str(op) for op in self.ops)
        return s


# TODO: This only exists because the instruction-building facility
# does not cover all codes yet; there is no such thing as an unknown
# instruction otherwise. Replace with an assert once all codes are
# supported.
class UnknownInstr(Instr):
    text: str | None

    def __init__(self, addr: int, opcode: int) -> None:
        super().__init__()
        self.addr = addr
        self.size = 1
        self.opcode = opcode
        self.text = None

    def __str__(self) -> str:
        return f'db {self.opcode:#04x}'


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


# An index prefix followed by another prefix: a one-byte quantum
# whose whole effect, selecting the index register, the following
# prefix overwrites -- the last prefix of a chain wins. The operand
# is the prefix byte.
class XPREFIX(Instr):
    pass
