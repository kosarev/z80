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
    'InvalidInstr',
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
    # The condition's field code in its encodings.
    _code: int


class CF(metaclass=CondFlag):
    _str = 'c'
    _code = 3


class NC(metaclass=CondFlag):
    _code = 2


class M(metaclass=CondFlag):
    _code = 7


class P(metaclass=CondFlag):
    _code = 6


class PE(metaclass=CondFlag):
    _code = 5


class PO(metaclass=CondFlag):
    _code = 4


class Z(metaclass=CondFlag):
    _code = 1


class NZ(metaclass=CondFlag):
    _code = 0


class Reg(_InstrElement):
    # The prefix byte selecting the register, for the index
    # registers and their halves.
    _prefix: bytes = b''

    # The register's field code in its encodings, ix/iy taking
    # hl's slot under their prefixes.
    _code: int | None = None


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
    _code = 7


class B(metaclass=Reg):
    _code = 0


class C(metaclass=Reg):
    _code = 1


class D(metaclass=Reg):
    _code = 2


class E(metaclass=Reg):
    _code = 3


class H(metaclass=Reg):
    _code = 4


class L(metaclass=Reg):
    _code = 5


class IReg(metaclass=Reg):
    _str = 'i'


class R(metaclass=Reg):
    pass


class AF(metaclass=Reg16):
    _code = 3


class AF2(metaclass=Reg16):
    _str = "af'"


class BC(metaclass=Reg16):
    _code = 0


class DE(metaclass=Reg16):
    _code = 1


class HL(metaclass=Reg16):
    _code = 2


class IX(metaclass=IndexReg16):
    _prefix = b'\xdd'
    _code = 2


class IXH(metaclass=IndexReg):
    _prefix = b'\xdd'
    _code = 4


class IXL(metaclass=IndexReg):
    _prefix = b'\xdd'
    _code = 5


class IY(metaclass=IndexReg16):
    _prefix = b'\xfd'
    _code = 2


class IYH(metaclass=IndexReg):
    _prefix = b'\xfd'
    _code = 4


class IYL(metaclass=IndexReg):
    _prefix = b'\xfd'
    _code = 5


class SP(metaclass=Reg16):
    _code = 3


Op: typing.TypeAlias = typing.Union[str, Reg, CondFlag, 'At', 'Expr']

# A value operand: an expression, or a plain integer standing for
# a constant.
Value: typing.TypeAlias = typing.Union[int, 'Expr']


# How plain integers enter the operand domain.
def _cast_op(op: int | Op) -> Op:
    return Const(op) if isinstance(op, int) else op


# Base class for expression operands.
class Expr:
    ops: list['Expr']

    def __init__(self, *ops: Value) -> None:
        self.ops = []
        for op in ops:
            if isinstance(op, int):
                op = Const(op)
            elif not isinstance(op, Expr):
                raise Error(f'{str(op)!r} is not a value.')

            self.ops.append(op)

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

    def __init__(self, a: Value, b: Value) -> None:
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
    def __init__(self, op: Value) -> None:
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
    # The optional displacement makes the (i + d) memory operand.
    def __init__(self, op: Value | Reg, disp: Value | None = None) -> None:
        self.ops = [_cast_op(op)]

        # The register content with its displacement -- (i)
        # meaning (i + 0) -- or None for plain-address operands.
        self.reg: Reg | None = op if isinstance(op, Reg) else None
        self.disp: Expr | None = None
        if self.reg is not None:
            self.disp = Const(0)

        if disp is not None:
            if op not in (IX, IY):
                raise Error('Only ix and iy take displacements.')

            self.disp = Const(disp) if isinstance(disp, int) else disp
            self.ops.append(self.disp)

    def __repr__(self) -> str:
        return f'At({tuple(self.ops)!r})'

    def __str__(self) -> str:
        if len(self.ops) == 1:
            return f'({self.ops[0]})'

        # A negative constant displacement renders under the
        # opposite sign.
        a, b = self.ops
        sign = '+'
        b_str = f'{b}'
        if isinstance(b, Const) and b.value < 0:
            sign, b_str = '-', f'{Const(-b.value)}'

        return f'({a} {sign} {b_str})'


class Add(BinaryExpr, sign='+'):
    def evaluate(self) -> int:
        a, b = tuple(self.ops)
        return a.evaluate() + b.evaluate()


class Sub(BinaryExpr, sign='-'):
    def evaluate(self) -> int:
        a, b = tuple(self.ops)
        return a.evaluate() - b.evaluate()


class Neg(UnaryExpr):
    def __str__(self) -> str:
        op, = tuple(self.ops)
        if isinstance(op, BinaryExpr):
            return f'-({op})'

        return f'-{op}'

    def evaluate(self) -> int:
        op, = tuple(self.ops)
        return -op.evaluate()


# Base class for all instructions.
class Instr:
    addr: int | None
    size: int | None
    origin: _SourcePos | None

    _x: int | None = None
    _y: int | None = None
    _z: int | None = None
    _prefix: bytes = b''

    # The immediate byte and little-endian word operands following
    # the opcode. Unlike the values that select opcode fields,
    # immediates are data, so they evaluate at encoding time,
    # letting forward references construct first and bind later.
    _n: 'Expr | None' = None
    _nn: 'Expr | None' = None

    # The pc-relative jump target. Stored absolute; the
    # displacement byte derives from the instruction address at
    # encoding time.
    _target: 'Expr | None' = None

    # The index displacement byte of the (i + d) memory operands,
    # sitting right after the opcode.
    _disp: 'Expr | None' = None

    def __init_subclass__(cls, *, x: int | None = None,
                          y: int | None = None, z: int | None = None,
                          prefix: int | None = None) -> None:
        super().__init_subclass__()

        # Omitted parameters inherit the base's fields, as with
        # subclasses of the private per-pair bases below.
        if x is not None:
            cls._x = x
        if y is not None:
            cls._y = y
        if z is not None:
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
        if x is None or y is None or z is None:
            raise InstrError(self, 'No encoding.')

        image = self._prefix + bytes((x << 6 | y << 3 | z,))

        if self._disp is not None:
            disp = self._disp.evaluate()
            if not -0x80 <= disp <= 0x7f:
                raise InstrError(
                    self, f'The displacement {disp:#x} is out of range.')

            image += (disp & 0xff).to_bytes(1, 'little')

        if self._n is not None:
            n = self._n.evaluate()
            if not -0x80 <= n <= 0xff:
                raise InstrError(
                    self, f'{n:#x} does not fit a byte.')

            image += (n & 0xff).to_bytes(1, 'little')

        if self._nn is not None:
            nn = self._nn.evaluate()
            if not -0x8000 <= nn <= 0xffff:
                raise InstrError(
                    self, f'{nn:#x} does not fit a word.')

            image += (nn & 0xffff).to_bytes(2, 'little')

        if self._target is not None:
            if addr is None:
                raise InstrError(
                    self, 'The instruction address is unknown.')

            target = self._target.evaluate()
            disp = target - (addr + len(image) + 1)
            if not -0x80 <= disp <= 0x7f:
                raise InstrError(
                    self, f'The target {target:#x} is out of range.')

            image += (disp & 0xff).to_bytes(1, 'little')

        return image


# An error about a specific instruction. The instruction is
# carried as constructed, for the reporting code to verbalise as
# needed.
class InstrError(Error):
    def __init__(self, instr: Instr, reason: str) -> None:
        super().__init__(reason)
        self.instr = instr


# The operands match no form of the instruction.
class InvalidInstr(InstrError):
    def __init__(self, instr: Instr) -> None:
        super().__init__(instr, 'No such instruction form.')


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


# The contribution of an alu[y] operand.
def _encode_alu_op(instr: Instr, op: Op) -> None:
    # alu[y] r[z]
    if op in (B, C, D, E, H, L, A, IXH, IXL, IYH, IYL):
        assert isinstance(op, Reg) and op._code is not None
        instr._z = op._code
        instr._prefix = op._prefix
    elif isinstance(op, At) and op.reg is HL:
        instr._z = 6
    # alu[y] n
    elif isinstance(op, Expr):
        instr._x, instr._z = 3, 6
        instr._n = op
    # alu[y] (i + d)
    elif isinstance(op, At) and op.reg in (IX, IY):
        assert op.reg is not None and op.disp is not None
        instr._z = 6
        instr._prefix = op.reg._prefix
        instr._disp = op.disp
    else:
        raise InvalidInstr(instr)


# The alu operations, the y= class parameter selecting the
# operation. Add, adc and sbc stand alone: their 16-bit forms are
# no alu operations.
class _Alu(Instr, x=2):
    def __init__(self, op: int | Op) -> None:
        super().__init__(op)
        _encode_alu_op(self, self.ops[0])


class ADD(Instr, y=0):
    def __init__(self, a: Reg | At, b: int | Op) -> None:
        super().__init__(a, b)

        match self.ops:
            # ADD HL, rp[p] and the ix/iy forms, whose rp slot 2
            # is the destination register itself.
            case (Reg() as dst, Reg() as rp) if (
                    dst in (HL, IX, IY) and
                    (rp in (BC, DE, SP) or rp is dst)):
                assert rp._code is not None
                self._x, self._z = 0, 1
                self._y = rp._code << 1 | 1
                self._prefix = dst._prefix
            # ADD A, ...
            case (Reg() as dst, op) if dst is A:
                self._x = 2
                _encode_alu_op(self, op)
            case _:
                raise InvalidInstr(self)


class ADC(Instr, y=1):
    def __init__(self, a: Reg | At, b: int | Op) -> None:
        super().__init__(a, b)

        match self.ops:
            # ADC HL, rp[p]
            case (Reg() as dst, Reg() as rp) if (
                    dst is HL and rp in (BC, DE, HL, SP)):
                assert rp._code is not None
                self._prefix = b'\xed'
                self._x, self._z = 1, 2
                self._y = rp._code << 1 | 1
            # ADC A, ...
            case (Reg() as dst, op) if dst is A:
                self._x = 2
                _encode_alu_op(self, op)
            case _:
                raise InvalidInstr(self)


class AND(_Alu, y=4):
    pass


# The bit test, reset and set operations, the x= class parameter
# selecting the operation.
class _Bit(Instr, prefix=0xcb):
    def __init__(self, *ops: int | Op) -> None:
        super().__init__(*ops)

        # x[y], r[z]
        if len(self.ops) == 2:
            bit, op = self.ops
            b = Expr._evaluate_op(bit)
            if not 0 <= b <= 7:
                raise InstrError(self, 'The bit number must be '
                                 '0 through 7.')

            self._y = b
            if op in (B, C, D, E, H, L, A):
                assert isinstance(op, Reg) and op._code is not None
                self._z = op._code
            elif isinstance(op, At) and op.reg is HL:
                self._z = 6
            # TODO: Encode the (ix/iy + d) forms.
            elif (isinstance(op, At) and op.reg in (IX, IY)):
                pass
            else:
                raise InvalidInstr(self)
        # TODO: The ddcb forms carrying the copy-field operand.
        elif len(self.ops) == 3:
            pass
        else:
            raise InvalidInstr(self)


class BIT(_Bit, x=1):
    pass


class RES(_Bit, x=2):
    pass


class SET(_Bit, x=3):
    pass


class CALL(CallInstr):
    def __init__(self, *ops: int | Op) -> None:
        super().__init__(*ops)

        match self.ops:
            # CALL nn
            case (Expr() as nn,):
                self._x, self._y, self._z = 3, 1, 5
                self._nn = nn
            # CALL cc[y], nn
            case (CondFlag() as cc, Expr() as nn):
                self._x, self._y, self._z = 3, cc._code, 4
                self._nn = nn
            case _:
                raise InvalidInstr(self)


class CCF(Instr, x=0, y=7, z=7):
    def __init__(self) -> None:
        super().__init__()


class CP(_Alu, y=7):
    pass


class CPD(Instr, prefix=0xed, x=2, y=5, z=1):
    def __init__(self) -> None:
        super().__init__()


class CPDR(Instr, prefix=0xed, x=2, y=7, z=1):
    def __init__(self) -> None:
        super().__init__()


class CPI(Instr, prefix=0xed, x=2, y=4, z=1):
    def __init__(self) -> None:
        super().__init__()


class CPIR(Instr, prefix=0xed, x=2, y=6, z=1):
    def __init__(self) -> None:
        super().__init__()


class CPL(Instr, x=0, y=5, z=7):
    def __init__(self) -> None:
        super().__init__()


class DAA(Instr, x=0, y=4, z=7):
    def __init__(self) -> None:
        super().__init__()


# INC and DEC, told apart by the z= of their register forms, the
# register-pair forms repeating the choice in q.
class _IncDec(Instr, x=0):
    def __init__(self, op: Reg | At) -> None:
        super().__init__(op)

        z = self._z
        assert z is not None

        # INC/DEC r[y]
        if op in (B, C, D, E, H, L, A, IXH, IXL, IYH, IYL):
            assert isinstance(op, Reg) and op._code is not None
            self._y = op._code
            self._prefix = op._prefix
        elif isinstance(op, At) and op.reg is HL:
            self._y = 6
        # INC/DEC rp[p]
        elif op in (BC, DE, HL, SP, IX, IY):
            assert isinstance(op, Reg) and op._code is not None
            self._y, self._z = op._code << 1 | (z - 4), 3
            self._prefix = op._prefix
        # INC/DEC (i + d)
        elif isinstance(op, At) and op.reg in (IX, IY):
            assert op.reg is not None and op.disp is not None
            self._y = 6
            self._prefix = op.reg._prefix
            self._disp = op.disp
        else:
            raise InvalidInstr(self)


class DEC(_IncDec, z=5):
    pass


class INC(_IncDec, z=4):
    pass


class DI(Instr, x=3, y=6, z=3):
    def __init__(self) -> None:
        super().__init__()


class DJNZ(JumpInstr):
    def __init__(self, target: Value) -> None:
        super().__init__(target)

        # DJNZ d
        t = self.ops[0]
        assert isinstance(t, Expr)
        self._x, self._y, self._z = 0, 2, 0
        self._target = t


class EI(Instr, x=3, y=7, z=3):
    def __init__(self) -> None:
        super().__init__()


class EX(Instr):
    def __init__(self, a: Reg | At, b: Reg) -> None:
        super().__init__(a, b)

        # EX AF, AF'
        if a is AF and b is AF2:
            self._x, self._y, self._z = 0, 1, 0
        # EX DE, HL
        elif a is DE and b is HL:
            self._x, self._y, self._z = 3, 5, 3
        # EX (SP), HL and the ix/iy forms.
        elif (isinstance(a, At) and a.reg is SP and
                b in (HL, IX, IY)):
            self._x, self._y, self._z = 3, 4, 3
            self._prefix = b._prefix
        else:
            raise InvalidInstr(self)


class EXX(Instr, x=3, y=3, z=1):
    def __init__(self) -> None:
        super().__init__()


class HALT(Instr, x=1, y=6, z=6):
    def __init__(self) -> None:
        super().__init__()


class IM(Instr, prefix=0xed, x=1, z=6):
    def __init__(self, mode: Value) -> None:
        super().__init__(mode)

        # IM im[y], the modes sitting at y 0, 2 and 3.
        m = Expr._evaluate_op(self.ops[0])
        if m not in (0, 1, 2):
            raise InstrError(self, 'The mode must be 0, 1 or 2.')

        self._y = 0 if m == 0 else m + 1


class XIM(Instr, prefix=0xed, x=1, z=6):
    def __init__(self, encoding: Value, mode: Value) -> None:
        super().__init__(encoding, mode)

        # The duplicate im encodings: ED 4E, 66, 6E, 76 and 7E.
        # The mode sits in y & 3 the same way the true encodings
        # have it.
        v = Expr._evaluate_op(self.ops[0])
        m = Expr._evaluate_op(self.ops[1])
        y = (v >> 3) & 7
        if (v >> 8 != 0xed or v & 0xc7 != 0x46 or y in (0, 2, 3) or
                m != (0 if y & 3 < 2 else (y & 3) - 1)):
            raise InvalidInstr(self)

        self._y = y


class IN(Instr):
    def __init__(self, *ops: int | Op) -> None:
        super().__init__(*ops)

        # IN (C)
        if (len(self.ops) == 1 and isinstance(self.ops[0], At) and
                self.ops[0].reg is C):
            self._prefix = b'\xed'
            self._x, self._y, self._z = 1, 6, 0
            return

        if len(self.ops) == 2:
            a, b = self.ops
            # IN r[y], (C)
            if (a in (B, C, D, E, H, L, A) and
                    isinstance(b, At) and b.reg is C):
                assert isinstance(a, Reg) and a._code is not None
                self._prefix = b'\xed'
                self._x, self._y, self._z = 1, a._code, 0
                return
            # IN A, (n)
            if (a is A and isinstance(b, At) and
                    isinstance(b.ops[0], Expr)):
                self._x, self._y, self._z = 3, 3, 3
                self._n = b.ops[0]
                return

        raise InvalidInstr(self)


class IND(Instr, prefix=0xed, x=2, y=5, z=2):
    def __init__(self) -> None:
        super().__init__()


class INDR(Instr, prefix=0xed, x=2, y=7, z=2):
    def __init__(self) -> None:
        super().__init__()


class INI(Instr, prefix=0xed, x=2, y=4, z=2):
    def __init__(self) -> None:
        super().__init__()


class INIR(Instr, prefix=0xed, x=2, y=6, z=2):
    def __init__(self) -> None:
        super().__init__()


class JP(JumpInstr):
    def __init__(self, *ops: int | Op) -> None:
        super().__init__(*ops)

        match self.ops:
            # JP (HL) and the ix/iy forms, taking no displacement.
            case (At() as op,):
                if op.reg not in (HL, IX, IY):
                    raise InvalidInstr(self)

                assert op.reg is not None and op.disp is not None
                if op.disp.evaluate() != 0:
                    raise InvalidInstr(self)

                self._x, self._y, self._z = 3, 5, 1
                self._prefix = op.reg._prefix
            # JP nn
            case (Expr() as nn,):
                self._x, self._y, self._z = 3, 0, 3
                self._nn = nn
            # JP cc[y], nn
            case (CondFlag() as cc, Expr() as nn):
                self._x, self._y, self._z = 3, cc._code, 2
                self._nn = nn
            case _:
                raise InvalidInstr(self)


class JR(JumpInstr):
    def __init__(self, *ops: int | Op) -> None:
        super().__init__(*ops)

        match self.ops:
            # JR d
            case (Expr() as target,):
                self._x, self._y, self._z = 0, 3, 0
                self._target = target
            # JR cc[y-4], d
            case (CondFlag() as cc, Expr() as target) if cc._code < 4:
                self._x, self._y, self._z = 0, cc._code + 4, 0
                self._target = target
            case _:
                raise InvalidInstr(self)


class LD(Instr):
    def __init__(self, a: Op, b: int | Op) -> None:
        super().__init__(a, b)

        b = self.ops[1]

        # LD A, (BC)/(DE) and the store counterparts.
        if a is A and isinstance(b, At) and b.reg is BC:
            self._x, self._y, self._z = 0, 1, 2
        elif a is A and isinstance(b, At) and b.reg is DE:
            self._x, self._y, self._z = 0, 3, 2
        elif isinstance(a, At) and a.reg is BC and b is A:
            self._x, self._y, self._z = 0, 0, 2
        elif isinstance(a, At) and a.reg is DE and b is A:
            self._x, self._y, self._z = 0, 2, 2
        # LD SP, HL and the ix/iy forms.
        elif a is SP and b in (HL, IX, IY):
            assert isinstance(b, Reg)
            self._x, self._y, self._z = 3, 7, 1
            self._prefix = b._prefix
        # The I and R transfers.
        elif a is IReg and b is A:
            self._prefix = b'\xed'
            self._x, self._y, self._z = 1, 0, 7
        elif a is R and b is A:
            self._prefix = b'\xed'
            self._x, self._y, self._z = 1, 1, 7
        elif a is A and b is IReg:
            self._prefix = b'\xed'
            self._x, self._y, self._z = 1, 2, 7
        elif a is A and b is R:
            self._prefix = b'\xed'
            self._x, self._y, self._z = 1, 3, 7
        # LD r[y], r[z]
        elif (a in (B, C, D, E, H, L, A, IXH, IXL, IYH, IYL) and
                b in (B, C, D, E, H, L, A, IXH, IXL, IYH, IYL)):
            assert isinstance(a, Reg) and isinstance(b, Reg)

            # The index register halves take the h/l slots under
            # their prefix, and the prefix redirects both slots at
            # once, so a half never pairs with true H or L, nor
            # with a half of the other index register.
            prefix = a._prefix or b._prefix
            if prefix:
                for r in a, b:
                    if r not in (B, C, D, E, A) and r._prefix != prefix:
                        raise InvalidInstr(self)

            assert a._code is not None and b._code is not None
            self._x, self._y, self._z = 1, a._code, b._code
            self._prefix = prefix
        # The (HL), (HL) slot is halt.
        elif (isinstance(a, At) and a.reg is HL and
                isinstance(b, At) and b.reg is HL):
            raise InvalidInstr(self)
        # LD r[y], (HL) and the store counterpart. The displaced
        # forms do not redirect the halves, so true H and L load
        # fine, and the halves never pair with memory.
        elif (a in (B, C, D, E, H, L, A) and
                isinstance(b, At) and b.reg is HL):
            assert isinstance(a, Reg) and a._code is not None
            self._x, self._y, self._z = 1, a._code, 6
        elif (isinstance(a, At) and a.reg is HL and
                b in (B, C, D, E, H, L, A)):
            assert isinstance(b, Reg) and b._code is not None
            self._x, self._y, self._z = 1, 6, b._code
        # LD r[y], n
        elif (a in (B, C, D, E, H, L, A, IXH, IXL, IYH, IYL) and
                isinstance(b, Expr)):
            assert isinstance(a, Reg) and a._code is not None
            self._x, self._y, self._z = 0, a._code, 6
            self._prefix = a._prefix
            self._n = b
        # LD (HL), n
        elif (isinstance(a, At) and a.reg is HL and
                isinstance(b, Expr)):
            self._x, self._y, self._z = 0, 6, 6
            self._n = b
        # LD rp[p], nn
        elif a in (BC, DE, HL, SP, IX, IY) and isinstance(b, Expr):
            assert isinstance(a, Reg) and a._code is not None
            self._x, self._y, self._z = 0, a._code << 1, 1
            self._prefix = a._prefix
            self._nn = b
        # LD A, (nn) and the store counterpart.
        elif a is A and isinstance(b, At) and isinstance(b.ops[0], Expr):
            self._x, self._y, self._z = 0, 7, 2
            self._nn = b.ops[0]
        elif isinstance(a, At) and isinstance(a.ops[0], Expr) and b is A:
            self._x, self._y, self._z = 0, 6, 2
            self._nn = a.ops[0]
        # LD HL, (nn), the ix/iy forms and the store counterparts.
        elif (a in (HL, IX, IY) and isinstance(b, At) and
                isinstance(b.ops[0], Expr)):
            assert isinstance(a, Reg)
            self._x, self._y, self._z = 0, 5, 2
            self._prefix = a._prefix
            self._nn = b.ops[0]
        elif (isinstance(a, At) and isinstance(a.ops[0], Expr) and
                b in (HL, IX, IY)):
            assert isinstance(b, Reg)
            self._x, self._y, self._z = 0, 4, 2
            self._prefix = b._prefix
            self._nn = a.ops[0]
        # The ED-page LD rp[p], (nn) and stores.
        elif (a in (BC, DE, SP) and isinstance(b, At) and
                isinstance(b.ops[0], Expr)):
            assert isinstance(a, Reg) and a._code is not None
            self._prefix = b'\xed'
            self._x, self._y, self._z = 1, a._code << 1 | 1, 3
            self._nn = b.ops[0]
        elif (isinstance(a, At) and isinstance(a.ops[0], Expr) and
                b in (BC, DE, SP)):
            assert isinstance(b, Reg) and b._code is not None
            self._prefix = b'\xed'
            self._x, self._y, self._z = 1, b._code << 1, 3
            self._nn = a.ops[0]
        # LD r[y], (i + d), the store counterpart, and
        # LD (i + d), n.
        elif (a in (B, C, D, E, H, L, A) and
                isinstance(b, At) and b.reg in (IX, IY)):
            assert isinstance(a, Reg) and a._code is not None
            assert b.reg is not None and b.disp is not None
            self._x, self._y, self._z = 1, a._code, 6
            self._prefix = b.reg._prefix
            self._disp = b.disp
        elif (isinstance(a, At) and a.reg in (IX, IY) and
                b in (B, C, D, E, H, L, A)):
            assert isinstance(b, Reg) and b._code is not None
            assert a.reg is not None and a.disp is not None
            self._x, self._y, self._z = 1, 6, b._code
            self._prefix = a.reg._prefix
            self._disp = a.disp
        elif (isinstance(a, At) and a.reg in (IX, IY) and
                isinstance(b, Expr)):
            assert a.reg is not None and a.disp is not None
            self._x, self._y, self._z = 0, 6, 6
            self._prefix = a.reg._prefix
            self._disp = a.disp
            self._n = b
        # I and R transfer with A only, and no register-pair
        # transfers exist beyond the SP forms above.
        elif (a in (IReg, R) or b in (IReg, R) or
                (isinstance(a, Reg16) and isinstance(b, Reg)) or
                (isinstance(a, Reg) and isinstance(b, Reg16))):
            raise InvalidInstr(self)

        # TODO: Encode the remaining ld forms.


class XLD(Instr):
    pass


class LDD(Instr, prefix=0xed, x=2, y=5, z=0):
    def __init__(self) -> None:
        super().__init__()


class LDDR(Instr, prefix=0xed, x=2, y=7, z=0):
    def __init__(self) -> None:
        super().__init__()


class LDI(Instr, prefix=0xed, x=2, y=4, z=0):
    def __init__(self) -> None:
        super().__init__()


class LDIR(Instr, prefix=0xed, x=2, y=6, z=0):
    def __init__(self) -> None:
        super().__init__()


class NEG(Instr, prefix=0xed, x=1, y=0, z=4):
    def __init__(self) -> None:
        super().__init__()


class XNEG(Instr, prefix=0xed, x=1, z=4):
    def __init__(self, encoding: Value) -> None:
        super().__init__(encoding)

        # The duplicate neg encodings: ED 4C, 54, ..., 7C.
        v = Expr._evaluate_op(self.ops[0])
        y = (v >> 3) & 7
        if v >> 8 != 0xed or v & 0xc7 != 0x44 or y == 0:
            raise InvalidInstr(self)

        self._y = y


class NOP(Instr, x=0, y=0, z=0):
    def __init__(self) -> None:
        super().__init__()


class XNOP(Instr, prefix=0xed):
    def __init__(self, encoding: Value) -> None:
        super().__init__(encoding)

        # The unassigned ED page encodings: the whole x=0 and x=3
        # quarters, the x=2 quarter outside the block operations,
        # and ED 77 and ED 7F within the otherwise fully assigned
        # x=1 quarter.
        v = Expr._evaluate_op(self.ops[0])
        x, y, z = (v >> 6) & 3, (v >> 3) & 7, v & 7
        if not (v >> 8 == 0xed and
                (x in (0, 3) or
                 (x == 2 and (z >= 4 or y < 4)) or
                 (x == 1 and z == 7 and y in (6, 7)))):
            raise InvalidInstr(self)

        self._x, self._y, self._z = x, y, z


class OR(_Alu, y=6):
    pass


class OUT(Instr):
    def __init__(self, *ops: int | Op) -> None:
        super().__init__(*ops)

        if len(self.ops) == 2:
            a, b = self.ops
            if isinstance(a, At) and a.reg is C:
                # OUT (C), 0
                if isinstance(b, Const) and b.value == 0:
                    self._prefix = b'\xed'
                    self._x, self._y, self._z = 1, 6, 1
                    return
                # OUT (C), r[y]
                if b in (B, C, D, E, H, L, A):
                    assert isinstance(b, Reg) and b._code is not None
                    self._prefix = b'\xed'
                    self._x, self._y, self._z = 1, b._code, 1
                    return
            # OUT (n), A
            if (isinstance(a, At) and isinstance(a.ops[0], Expr) and
                    b is A):
                self._x, self._y, self._z = 3, 2, 3
                self._n = a.ops[0]
                return

        raise InvalidInstr(self)


class OUTD(Instr, prefix=0xed, x=2, y=5, z=3):
    def __init__(self) -> None:
        super().__init__()


class OTDR(Instr, prefix=0xed, x=2, y=7, z=3):
    def __init__(self) -> None:
        super().__init__()


class OUTI(Instr, prefix=0xed, x=2, y=4, z=3):
    def __init__(self) -> None:
        super().__init__()


class OTIR(Instr, prefix=0xed, x=2, y=6, z=3):
    def __init__(self) -> None:
        super().__init__()


# PUSH rp2[p] and POP rp2[p], told apart by z.
class _PushPop(Instr, x=3):
    def __init__(self, rp: Reg) -> None:
        super().__init__(rp)

        if rp in (BC, DE, HL, AF, IX, IY):
            assert rp._code is not None
            self._y = rp._code << 1
            self._prefix = rp._prefix
        else:
            raise InvalidInstr(self)


class POP(_PushPop, z=1):
    pass


class PUSH(_PushPop, z=5):
    pass


class RET(RetInstr):
    def __init__(self, *ops: int | Op) -> None:
        super().__init__(*ops)

        match self.ops:
            # RET
            case ():
                self._x, self._y, self._z = 3, 1, 1
            # RET cc[y]
            case (CondFlag() as cc,):
                self._x, self._y, self._z = 3, cc._code, 0
            case _:
                raise InvalidInstr(self)


class RETN(RetInstr, prefix=0xed, x=1, y=0, z=5):
    def __init__(self) -> None:
        super().__init__()


class XRETN(RetInstr, prefix=0xed, x=1, z=5):
    def __init__(self, encoding: Value) -> None:
        super().__init__(encoding)

        # The duplicate retn encodings: ED 55, 5D, ..., 7D. ED 45
        # is the true retn and ED 4D is reti.
        v = Expr._evaluate_op(self.ops[0])
        y = (v >> 3) & 7
        if v >> 8 != 0xed or v & 0xc7 != 0x45 or y in (0, 1):
            raise InvalidInstr(self)

        self._y = y


class RETI(RetInstr, prefix=0xed, x=1, y=1, z=5):
    def __init__(self) -> None:
        super().__init__()


# The rotate and shift operations, the y= class parameter
# selecting the operation.
class _Rot(Instr, prefix=0xcb, x=0):
    def __init__(self, op: Reg | At) -> None:
        super().__init__(op)

        # rot[y] r[z]
        if op in (B, C, D, E, H, L, A):
            assert isinstance(op, Reg) and op._code is not None
            self._z = op._code
        elif isinstance(op, At) and op.reg is HL:
            self._z = 6
        # TODO: Encode rot[y] (ix/iy + d).
        elif (isinstance(op, At) and op.reg in (IX, IY)):
            pass
        else:
            raise InvalidInstr(self)


class RLC(_Rot, y=0):
    pass


class RRC(_Rot, y=1):
    pass


class RL(_Rot, y=2):
    pass


class RR(_Rot, y=3):
    pass


class SLA(_Rot, y=4):
    pass


class SRA(_Rot, y=5):
    pass


class SLL(_Rot, y=6):
    pass


class SRL(_Rot, y=7):
    pass


class RLA(Instr, x=0, y=2, z=7):
    def __init__(self) -> None:
        super().__init__()


class RLCA(Instr, x=0, y=0, z=7):
    def __init__(self) -> None:
        super().__init__()


class RLD(Instr, prefix=0xed, x=1, y=5, z=7):
    def __init__(self) -> None:
        super().__init__()


class RRA(Instr, x=0, y=3, z=7):
    def __init__(self) -> None:
        super().__init__()


class RRD(Instr, prefix=0xed, x=1, y=4, z=7):
    def __init__(self) -> None:
        super().__init__()


class RRCA(Instr, x=0, y=1, z=7):
    def __init__(self) -> None:
        super().__init__()


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


class SBC(Instr, y=3):
    def __init__(self, a: Reg | At, b: int | Op) -> None:
        super().__init__(a, b)

        match self.ops:
            # SBC HL, rp[p]
            case (Reg() as dst, Reg() as rp) if (
                    dst is HL and rp in (BC, DE, HL, SP)):
                assert rp._code is not None
                self._prefix = b'\xed'
                self._x, self._z = 1, 2
                self._y = rp._code << 1
            # SBC A, ...
            case (Reg() as dst, op) if dst is A:
                self._x = 2
                _encode_alu_op(self, op)
            case _:
                raise InvalidInstr(self)


class SCF(Instr, x=0, y=6, z=7):
    def __init__(self) -> None:
        super().__init__()


class SUB(_Alu, y=2):
    pass


class XOR(_Alu, y=5):
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
