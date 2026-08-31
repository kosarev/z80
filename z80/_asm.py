
#   Z80 CPU Emulator.
#   https://github.com/kosarev/z80
#
#   Copyright (C) 2026 Ivan Kosarev.
#   mail@ivankosarev.com
#
#   Published under the MIT license.

import re
import typing

from ._code import Code, Db, LabelDef
from ._error import Error
from ._instr import (
    ADC,
    ADD,
    AND,
    BC,
    BIT,
    CALL,
    CCF,
    CF,
    CP,
    CPIR,
    CPL,
    DAA,
    DE,
    DEC,
    DI,
    DJNZ,
    EI,
    EX,
    EXX,
    HALT,
    HL,
    IM,
    IN,
    INC,
    IX,
    IY,
    JP,
    JR,
    LD,
    LDDR,
    LDI,
    LDIR,
    NC,
    NEG,
    NOP,
    NZ,
    OR,
    OUT,
    PE,
    PO,
    POP,
    PUSH,
    RES,
    RET,
    RL,
    RLA,
    RLC,
    RLCA,
    RLD,
    RR,
    RRA,
    RRC,
    RRCA,
    RST,
    SBC,
    SCF,
    SET,
    SLA,
    SP,
    SRA,
    SRL,
    SUB,
    XOR,
    A,
    At,
    B,
    C,
    CondFlag,
    D,
    E,
    Expr,
    H,
    Instr,
    L,
    M,
    Op,
    P,
    Reg,
    Z,
)
from ._source import _SourceFile
from ._token import _Token, _Tokeniser


# An error about a source construct.
class AsmError(Error):
    def __init__(self, subject: _Token, message: str) -> None:
        if subject.literal in (None, '\n'):
            # There is nothing to name at positions past the last
            # token of the line.
            super().__init__(f'{subject.pos.inline_text}: {message}')
        else:
            super().__init__(
                f'{subject.pos.inline_text}: {subject}: {message}')
        self.subject = subject
        self.message = message

    def verbalize(self, program_name: str | None = None) -> str:
        def g() -> typing.Generator[str, None, None]:
            yield self.subject.pos.context_text

            if program_name is not None:
                yield f'{program_name}: '

            yield self.reason

        return ''.join(g())


# The source-level assembler: a driver of Code's semantic actions,
# translating source constructs into calls on the Code it builds.
class Asm:
    __NAME = re.compile(r'[_a-zA-Z][_0-9a-zA-Z]*$')

    __TOKENS = re.compile(
        r'([_0-9a-zA-Z]+)|'   # A number or identifier.
        r'\n|'                # An end of line.
        r'.')                 # Or any other single character.

    __OPS: typing.ClassVar[dict[str, Reg]] = {
        'a': A, 'b': B, 'c': C, 'd': D, 'e': E, 'h': H, 'l': L,
        'bc': BC, 'de': DE, 'hl': HL, 'sp': SP, 'ix': IX, 'iy': IY,
    }

    __INSTRS: typing.ClassVar[dict[str, type[Instr]]] = {
        'adc': ADC,
        'add': ADD,
        'and': AND,
        'bit': BIT,
        'call': CALL,
        'ccf': CCF,
        'cp': CP,
        'cpir': CPIR,
        'cpl': CPL,
        'daa': DAA,
        'dec': DEC,
        'di': DI,
        'djnz': DJNZ,
        'ei': EI,
        'ex': EX,
        'exx': EXX,
        'halt': HALT,
        'im': IM,
        'in': IN,
        'inc': INC,
        'jp': JP,
        'jr': JR,
        'ld': LD,
        'lddr': LDDR,
        'ldi': LDI,
        'ldir': LDIR,
        'neg': NEG,
        'nop': NOP,
        'or': OR,
        'out': OUT,
        'pop': POP,
        'push': PUSH,
        'res': RES,
        'ret': RET,
        'rl': RL,
        'rla': RLA,
        'rlc': RLC,
        'rlca': RLCA,
        'rld': RLD,
        'rr': RR,
        'rra': RRA,
        'rrc': RRC,
        'rrca': RRCA,
        'rst': RST,
        'sbc': SBC,
        'scf': SCF,
        'set': SET,
        'sla': SLA,
        'sra': SRA,
        'srl': SRL,
        'sub': SUB,
        'xor': XOR,
    }

    # Condition names read as conditions only in the condition
    # slot -- the first operand of these instructions.
    __CONDS: typing.ClassVar[dict[str, CondFlag]] = {
        'nz': NZ, 'z': Z, 'nc': NC, 'c': CF,
        'po': PO, 'pe': PE, 'p': P, 'm': M,
    }

    __COND_INSTRS = frozenset(('jp', 'jr', 'call', 'ret'))

    __toks: _Tokeniser
    __code: Code

    def __fetch_token(self) -> _Token:
        self.__toks.skip_whitespace()
        self.__toks.start_token()
        self.__toks.skip(self.__TOKENS)
        tok = self.__toks.end_token()

        if tok.literal == ';':
            # A comment runs to the end of the line.
            self.__toks.skip_rest_of_line()
            self.__toks.start_token()
            self.__toks.skip(self.__TOKENS)
            tok = self.__toks.end_token()

        return tok

    # Fetches a subsequent token and checks it is the expected one.
    def __skip_token(self, literal: str,
                     error: str | None = None) -> None:
        tok = self.__fetch_token()
        if tok != literal:
            if error is None:
                error = f'{literal!r} expected.'

            raise AsmError(tok, error)

    def __parse_end_of_line(self) -> None:
        tok = self.__fetch_token()
        if tok.literal not in (None, '\n'):
            raise AsmError(tok, 'End of line expected.')

    # Fetches a subsequent token and evaluates it as a number.
    def __parse_number(self) -> int:
        tok = self.__fetch_token()
        if tok.literal is not None:
            try:
                return int(tok.literal, base=0)
            except ValueError:
                pass

        raise AsmError(tok, 'A number expected.')

    def __parse_op(self, tok: _Token) -> 'int | Reg | Expr | At':
        lit = tok.literal

        # A pair spanning the entire operand marks an access form.
        if lit == '(':
            op = self.__parse_op(self.__fetch_token())
            if isinstance(op, At):
                raise AsmError(tok, 'Invalid operand.')

            # A sign after the operand starts a displacement.
            t = self.__fetch_token()
            if t.literal in ('+', '-'):
                disp = self.__parse_number()
                if t == '-':
                    disp = -disp

                self.__skip_token(')')
                try:
                    return At(op, disp)
                except Error as e:
                    raise AsmError(tok, e.reason) from e

            if t != ')':
                raise AsmError(t, "')' expected.")

            return At(op)

        if lit is not None:
            reg = self.__OPS.get(lit)
            if reg is not None:
                return reg

            if lit[0].isdigit():
                try:
                    return int(lit, base=0)
                except ValueError:
                    raise AsmError(tok, 'Invalid operand.') from None

            if self.__NAME.match(lit) is not None:
                return self.__code.get_symbol(lit)

        raise AsmError(tok, 'An operand expected.')

    def __parse_instr(self, tok: _Token) -> None:
        assert tok.literal is not None
        instr_type = self.__INSTRS[tok.literal]

        ops: list[int | Op] = []
        t = self.__fetch_token()
        if t.literal not in (None, '\n'):
            if (tok.literal in self.__COND_INSTRS and
                    t.literal in self.__CONDS):
                ops.append(self.__CONDS[t.literal])
            else:
                ops.append(self.__parse_op(t))
            while True:
                t = self.__fetch_token()
                if t.literal in (None, '\n'):
                    break

                if t != ',':
                    raise AsmError(t, "',' expected.")

                ops.append(self.__parse_op(self.__fetch_token()))

        try:
            self.__code.add(instr_type(*ops))
        except Error as e:
            raise AsmError(tok, e.reason) from e

    def __parse_db(self) -> None:
        values = [self.__parse_number()]
        while True:
            tok = self.__fetch_token()
            if tok.literal in (None, '\n'):
                break

            if tok != ',':
                raise AsmError(tok, "',' expected.")

            values.append(self.__parse_number())

        self.__code.add(Db(*values))

    def __parse_org(self) -> None:
        self.__code.start_block(self.__parse_number())
        self.__parse_end_of_line()

    def __parse_line(self, tok: _Token) -> None:
        lit = tok.literal
        assert lit is not None

        if lit in self.__INSTRS:
            self.__parse_instr(tok)
            return

        if lit == 'db':
            self.__parse_db()
            return

        if lit == 'org':
            self.__parse_org()
            return

        # Not a directive or instruction, so a symbol definition.
        if self.__NAME.match(lit) is None:
            raise AsmError(tok, 'Invalid construct.')

        self.__skip_token(':', "':' expected after a symbol name.")
        self.__code.add(LabelDef(self.__code.get_symbol(lit)))

    def assemble(self, source_file: _SourceFile) -> Code:
        self.__toks = _Tokeniser(source_file)
        self.__code = Code()
        while True:
            tok = self.__fetch_token()
            if tok.literal is None:
                break

            if tok.literal != '\n':
                self.__parse_line(tok)

        return self.__code
