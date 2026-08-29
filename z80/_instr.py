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
    'Instr',
    'InstrError',
    'JumpInstr',
    'L',
    'M',
    'Neg',
    'P',
    'R',
    'RetInstr',
    'Sub',
    'Symbol',
    'UnaryExpr',
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

# A value operand: an expression, or a plain integer standing for
# a constant.
Value: typing.TypeAlias = typing.Union[int, 'Expr']


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
    _sign: str

    def __init_subclass__(cls, *, sign: str) -> None:
        super().__init_subclass__()
        cls._sign = sign

    def __init__(self, a: int | Op, b: int | Op) -> None:
        super().__init__(a, b)

    def __str__(self) -> str:
        a, b = tuple(self.ops)

        # A negative constant operand renders under the opposite
        # sign; a compound right operand renders parenthesised.
        sign = self._sign
        b_str = f'{b}'
        if isinstance(b, Const) and b.value < 0:
            sign = '-' if sign == '+' else '+'
            b_str = f'{Const(-b.value)}'
        elif isinstance(b, BinaryExpr):
            b_str = f'({b})'

        return f'{a} {sign} {b_str}'


class UnaryExpr(Expr):
    def __init__(self, op: int | Op) -> None:
        super().__init__(op)


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


class Add(BinaryExpr, sign='+'):
    def evaluate(self) -> int:
        a, b = tuple(self.ops)
        return self._evaluate_op(a) + self._evaluate_op(b)


class Sub(BinaryExpr, sign='-'):
    def evaluate(self) -> int:
        a, b = tuple(self.ops)
        return self._evaluate_op(a) - self._evaluate_op(b)


class Neg(UnaryExpr):
    def __str__(self) -> str:
        op, = tuple(self.ops)
        if isinstance(op, BinaryExpr):
            return f'-({op})'

        return f'-{op}'

    def evaluate(self) -> int:
        op, = tuple(self.ops)
        return -self._evaluate_op(op)


# Base class for all instructions.
class Instr:
    addr: int | None
    size: int | None
    origin: _SourcePos | None

    _x: int | None = None
    _y: int | None = None
    _z: int | None = None
    _prefix: bytes = b''

    def __init_subclass__(cls, *, x: int | None = None,
                          y: int | None = None, z: int | None = None,
                          prefix: int | None = None) -> None:
        super().__init_subclass__()

        cls._x = x
        cls._y = y
        cls._z = z
        if prefix is not None:
            cls._prefix = bytes((prefix,))

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

    # The bytes of the instruction. The address is only consulted
    # where the encoding depends on it, as with relative jump
    # displacements.
    def encode(self, addr: int | None = None) -> bytes:
        x, y, z = self._x, self._y, self._z
        if x is not None and y is not None and z is not None:
            return self._prefix + bytes((x << 6 | y << 3 | z,))

        raise InstrError(self, 'No encoding.')


# An error about a specific instruction. The instruction is
# carried as constructed, for the reporting code to verbalise as
# needed.
class InstrError(Error):
    def __init__(self, instr: Instr, reason: str) -> None:
        super().__init__(reason)
        self.instr = instr


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


class CCF(Instr, x=0, y=7, z=7):
    pass


class CP(Instr):
    pass


class CPD(Instr, prefix=0xed, x=2, y=5, z=1):
    pass


class CPDR(Instr, prefix=0xed, x=2, y=7, z=1):
    pass


class CPI(Instr, prefix=0xed, x=2, y=4, z=1):
    pass


class CPIR(Instr, prefix=0xed, x=2, y=6, z=1):
    pass


class CPL(Instr, x=0, y=5, z=7):
    pass


class DAA(Instr, x=0, y=4, z=7):
    pass


class DEC(Instr):
    pass


class DI(Instr, x=3, y=6, z=3):
    pass


class DJNZ(JumpInstr):
    pass


class EI(Instr, x=3, y=7, z=3):
    pass


class EX(Instr):
    pass


class EXX(Instr, x=3, y=3, z=1):
    pass


class HALT(Instr, x=1, y=6, z=6):
    pass


class IM(Instr, prefix=0xed, x=1, z=6):
    def __init__(self, mode: Value) -> None:
        super().__init__(mode)

        # IM im[y], the modes sitting at y 0, 2 and 3.
        m = Expr._evaluate_op(self.ops[0])
        if m not in (0, 1, 2):
            raise InstrError(self, 'The mode must be 0, 1 or 2.')

        self._y = 0 if m == 0 else m + 1


class XIM(Instr):
    pass


class IN(Instr):
    pass


class IND(Instr, prefix=0xed, x=2, y=5, z=2):
    pass


class INDR(Instr, prefix=0xed, x=2, y=7, z=2):
    pass


class INI(Instr, prefix=0xed, x=2, y=4, z=2):
    pass


class INIR(Instr, prefix=0xed, x=2, y=6, z=2):
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


class LDD(Instr, prefix=0xed, x=2, y=5, z=0):
    pass


class LDDR(Instr, prefix=0xed, x=2, y=7, z=0):
    pass


class LDI(Instr, prefix=0xed, x=2, y=4, z=0):
    pass


class LDIR(Instr, prefix=0xed, x=2, y=6, z=0):
    pass


class NEG(Instr, prefix=0xed, x=1, y=0, z=4):
    pass


class XNEG(Instr):
    pass


class NOP(Instr, x=0, y=0, z=0):
    pass


class XNOP(Instr):
    pass


class OR(Instr):
    pass


class OUT(Instr):
    pass


class OUTD(Instr, prefix=0xed, x=2, y=5, z=3):
    pass


class OTDR(Instr, prefix=0xed, x=2, y=7, z=3):
    pass


class OUTI(Instr, prefix=0xed, x=2, y=4, z=3):
    pass


class OTIR(Instr, prefix=0xed, x=2, y=6, z=3):
    pass


class POP(Instr):
    pass


class PUSH(Instr):
    pass


class RES(Instr):
    pass


class RET(RetInstr):
    pass


class RETN(RetInstr, prefix=0xed, x=1, y=0, z=5):
    pass


class XRETN(RetInstr):
    pass


class RETI(RetInstr, prefix=0xed, x=1, y=1, z=5):
    pass


class RL(Instr):
    pass


class RLA(Instr, x=0, y=2, z=7):
    pass


class RLC(Instr):
    pass


class RLCA(Instr, x=0, y=0, z=7):
    pass


class RLD(Instr, prefix=0xed, x=1, y=5, z=7):
    pass


class RR(Instr):
    pass


class RRA(Instr, x=0, y=3, z=7):
    pass


class RRD(Instr, prefix=0xed, x=1, y=4, z=7):
    pass


class RRC(Instr):
    pass


class RRCA(Instr, x=0, y=1, z=7):
    pass


class RST(CallInstr, x=3, z=7):
    def __init__(self, target: Value) -> None:
        super().__init__(target)

        # RST y*8: the y field is the target, which therefore
        # must be one of the eight encodable addresses.
        addr = Expr._evaluate_op(self.ops[0])
        if addr & ~0x38:
            raise InstrError(self, 'The target must be one of '
                             '0x00, 0x08, ..., 0x38.')

        self._y = addr >> 3


class SBC(Instr):
    pass


class SCF(Instr, x=0, y=6, z=7):
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
class XPREFIX(Instr, x=3, z=5):
    def __init__(self, value: Value) -> None:
        super().__init__(value)

        v = Expr._evaluate_op(self.ops[0])
        if v not in (0xdd, 0xfd):
            raise InstrError(self, 'The prefix byte must be '
                             '0xdd or 0xfd.')

        self._y = 3 if v == 0xdd else 7
