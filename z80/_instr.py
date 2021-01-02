#!/usr/bin/env python3
# -*- coding: utf-8 -*-

#   Z80 CPU Emulator.
#   https://github.com/kosarev/z80
#
#   Copyright (C) 2017-2021 Ivan Kosarev.
#   ivan@kosarev.info
#
#   Published under the MIT license.


class CondFlag(type):
    def __repr__(cls):
        return cls.__name__

    def __str__(cls):
        s = cls.__name__
        if s == 'CF':
            s = 'C'
        return s.lower()


class CF(metaclass=CondFlag):
    pass


class NC(metaclass=CondFlag):
    pass


class M(metaclass=CondFlag):
    pass


class P(metaclass=CondFlag):
    pass


class PO(metaclass=CondFlag):
    pass


class Z(metaclass=CondFlag):
    pass


class NZ(metaclass=CondFlag):
    pass


class Reg(type):
    __STR_NAMES = {
        'AF2': "af'",
        'IReg': 'I',
    }

    def __repr__(cls):
        return cls.__name__

    def __str__(cls):
        n = cls.__name__
        return cls.__STR_NAMES.get(n, n).lower()


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
    pass


class AF(metaclass=Reg):
    pass


class AF2(metaclass=Reg):
    pass


class BC(metaclass=Reg):
    pass


class DE(metaclass=Reg):
    pass


class HL(metaclass=Reg):
    pass


class IX(metaclass=IndexReg):
    pass


class IY(metaclass=IndexReg):
    pass


class SP(metaclass=Reg):
    pass


def _str_op(op):
    if isinstance(op, int):
        return '%#x' % op

    return str(op)


class At(object):
    def __init__(self, op):
        self.ops = [op]

    def __repr__(self):
        return 'At(%r)' % tuple(self.ops)

    def __str__(self):
        op, = tuple(self.ops)
        return '(%s)' % _str_op(op)


class Add(object):
    def __init__(self, a, b):
        self.ops = [a, b]

    def __repr__(self):
        return 'Add(%r, %r)' % tuple(self.ops)

    def __str__(self):
        a, b = tuple(self.ops)

        sign = '+'
        if isinstance(b, int) and b < 0:
            sign = '-'
            b = -b

        return '%s %c %s' % (_str_op(a), sign, _str_op(b))


# Base class for all instructions.
class Instr(object):
    def __init__(self, *ops, origin=None):
        self.addr = None
        self.size = None
        self.ops = ops
        self.origin = None

    def __repr__(self):
        return (type(self).__name__ +
                '(' + ', '.join(repr(op) for op in self.ops) + ')')

    def __str__(self):
        s = type(self).__name__.lower()
        if self.ops:
            s += ' ' + ', '.join(_str_op(op) for op in self.ops)
        return s


class UnknownInstr(Instr):
    def __init__(self, addr, opcode):
        super().__init__()
        self.addr = addr
        self.size = 1
        self.opcode = opcode
        self.text = None

    def __str__(self):
        return 'db %#04x' % self.opcode


# Instructions that may affect control flow.
class JumpInstr(Instr):
    @property
    def target(self):
        return self.ops[-1]

    @property
    def conditional(self):
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


class IN(Instr):
    pass


class INC(Instr):
    pass


class JP(JumpInstr):
    pass


class JR(JumpInstr):
    pass


class LD(Instr):
    pass


class LDDR(Instr):
    pass


class LDIR(Instr):
    pass


class NEG(Instr):
    pass


class NOP(Instr):
    pass


class OR(Instr):
    pass


class OUT(Instr):
    pass


class POP(Instr):
    pass


class PUSH(Instr):
    pass


class RES(Instr):
    pass


class RET(RetInstr):
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


class SRA(Instr):
    pass


class SRL(Instr):
    pass


class SUB(Instr):
    pass


class XOR(Instr):
    pass
