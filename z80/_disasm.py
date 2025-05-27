#!/usr/bin/env python3
# -*- coding: utf-8 -*-

#   Z80 CPU Emulator.
#   https://github.com/kosarev/z80
#
#   Copyright (C) 2017-2025 Ivan Kosarev.
#   mail@ivankosarev.com
#
#   Published under the MIT license.

import collections
import os
import tempfile
import typing

from ._error import Error
from ._instr import (ADD, ADC, AND, CP, CPD, CPDR, CPI, CPIR,
                     OR, SBC, SUB, XOR, BIT, CALL, CCF, CPL,
                     DAA, DEC, DI, DJNZ, EI, EX, EXX, HALT, IM, XIM,
                     INC, IN, IND, INDR, INI, INIR, JP, JR, LD, XLD,
                     LDD, LDDR, LDI, LDIR, NEG, XNEG, NOP, XNOP,
                     RLC, RL, RR, RRC, SLA, SLL, SRA,
                     SRL, OUT, OUTD, OTDR, OUTI, OTIR, POP, PUSH, RES,
                     RET, RETN, XRETN, RETI,
                     RLA, RLCA, RLD, RRA, RRD, RRCA,
                     RST, SCF, SET, A, AF, AF2, CF, M, NC, NZ, PE, PO, P, Z,
                     DE, BC, HL, IReg, R, IY, IX, SP,
                     B, C, D, E, H, L, IXH, IXL, IYH, IYL,
                     UnknownInstr, JumpInstr, CallInstr, RetInstr, At,
                     IndexReg, Add)
from ._instr import Instr
from ._instr import Op
from ._machine import Z80Machine
from ._source import _SourcePos
from ._token import _Token


class _DisasmError(Error):
    def __init__(self, subject: '_Tag', message: str,
                 *notes: '_DisasmError') -> None:
        assert isinstance(subject.origin, _SourcePos)
        super().__init__('%s: %s: %s' % (subject.origin.inline_text,
                                         subject, message))
        self.subject = subject
        self.message = message
        self.notes = notes

    def verbalize(self, program_name: str | None = None) -> str:
        def g() -> typing.Generator[str, None, None]:
            assert isinstance(self.subject.origin, _SourcePos)
            yield self.subject.origin.context_text

            if program_name is not None:
                yield '%s: ' % program_name

            yield '%s' % self.reason

            for n in self.notes:
                yield '\n'
                yield n.verbalize()

        return ''.join(g())


class _Tag(object):
    ID: str
    comment: str | _Token | None  # TODO

    def __init__(self, origin: _SourcePos | None, addr: int,
                 size: int) -> None:
        self.origin = origin
        self.addr = addr
        self.size = size
        self.comment = None

    def __str__(self) -> str:
        return '%s tag' % self.ID

    def __repr__(self) -> str:
        return '(%#06x, %s, %r)' % (self.addr, self.ID, self.comment)


class _CommentTag(_Tag):
    ID = 'comment'

    def __init__(self, origin: _SourcePos | None, addr: int,
                 comment: str) -> None:
        super().__init__(origin, addr, size=0)
        self.comment = comment


class _InlineCommentTag(_Tag):
    ID = 'inline_comment'

    def __init__(self, origin: _SourcePos, addr: int, comment: str) -> None:
        super().__init__(origin, addr, size=0)
        self.comment = comment


class _HintTag(_Tag):
    ID = 'hint'

    def __init__(self, origin: _SourcePos | None, addr: int, hint: str):
        super().__init__(origin, addr, size=0)
        self.comment = hint


class _ByteTag(_Tag):
    ID = 'byte'

    def __init__(self, origin: _SourcePos | None, addr: int, value: int):
        super().__init__(origin, addr, size=1)
        self.value = value

    def __repr__(self) -> str:
        return '(%#06x, %s, %#04x, %r)' % (
            self.addr, self.ID, self.value, self.comment)


class _IncludeBinaryTag(_Tag):
    ID = 'include_binary'

    def __init__(self, origin: _SourcePos, addr: int, filename: _Token,
                 image: bytes):
        super().__init__(origin, addr, size=len(image))
        self.filename = filename
        self.image = image

    def __repr__(self) -> str:
        return '(%#06x, %s, %s, %s)' % (
            self.addr, self.ID, self.filename, self.comment)


class _InstrTag(_Tag):
    ID = 'instr'

    def __init__(self, origin: _SourcePos, addr: int) -> None:
        super().__init__(origin, addr, size=0)


class _DisasmTag(_Tag):
    instr: Instr

    ID = 'disasm'

    def __init__(self, origin: _SourcePos | None, addr: int) -> None:
        super().__init__(origin, addr, size=0)


class _UnknownInstrError(Exception):
    pass


class Z80InstrBuilder(object):
    __INSTRS = {
        'Aadd': ADD,
        'Aadc': ADC,
        'Aand': AND,
        'Acp': CP,
        'add': ADD,
        'adc': ADC,
        'Aor': OR,
        'Asbc': SBC,
        'Asub': SUB,
        'Axor': XOR,
        'bit': BIT,
        'call': CALL,
        'ccf': CCF,
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
        'xim': XIM,
        'inc': INC,
        'in': IN,
        'Iind': IND,
        'Iindr': INDR,
        'Iini': INI,
        'Iinir': INIR,
        'jp': JP,
        'jr': JR,
        'ld': LD,
        'xld': XLD,
        'Lldd': LDD,
        'Llddr': LDDR,
        'Lldi': LDI,
        'Lldir': LDIR,
        'Mcpd': CPD,
        'Mcpdr': CPDR,
        'Mcpi': CPI,
        'Mcpir': CPIR,
        'neg': NEG,
        'xneg': XNEG,
        'nop': NOP,
        'xnop': XNOP,
        'Orlc': RLC,
        'Orl': RL,
        'Orr': RR,
        'Orrc': RRC,
        'Osla': SLA,
        'Osll': SLL,
        'Osra': SRA,
        'Osrl': SRL,
        'out': OUT,
        'pop': POP,
        'push': PUSH,
        'res': RES,
        'ret': RET,
        'retn': RETN,
        'xretn': XRETN,
        'reti': RETI,
        'rla': RLA,
        'rlca': RLCA,
        'rld': RLD,
        'rra': RRA,
        'rrd': RRD,
        'rrca': RRCA,
        'rst': RST,
        'sbc': SBC,
        'scf': SCF,
        'set': SET,
        'Toutd': OUTD,
        'Totdr': OTDR,
        'Touti': OUTI,
        'Totir': OTIR,
    }

    __OPS = {
        'a': A,
        'af': AF,
        'af\'': AF2,
        'c': C,
        'Cc': CF,
        'Cm': M,
        'Cnc': NC,
        'Cnz': NZ,
        'Cpe': PE,
        'Cpo': PO,
        'Cp': P,
        'Cz': Z,
        'de': DE,
        'Gaf': AF,
        'Gbc': BC,
        'Gde': DE,
        'Ghl': HL,
        'Gix': IX,
        'Giy': IY,
        'hl': HL,
        'i': IReg,
        'r': R,
        'ix': IX,
        'iy': IY,
        'Pbc': BC,
        'Pde': DE,
        'Phl': HL,
        'Pix': IX,
        'Piy': IY,
        'Psp': SP,
        'Ra': A,
        'Rb': B,
        'Rc': C,
        'Rd': D,
        'Re': E,
        'Rh': H,
        'Rl': L,
        'Rixh': IXH,
        'Rixl': IXL,
        'Riyh': IYH,
        'Riyl': IYL,
        'sp': SP,
    }

    def __build_op(self, addr: int, text: str) -> Op:
        if text.startswith('R('):
            text = text[1:]

        if text.startswith('('):
            assert text[-1] == ')'
            return At(self.__build_op(addr, text[1:-1]))

        # Base operand.
        # TODO: Have a proper base class for operand objects other than
        # primitive integers and strings.
        op: Op
        if text in self.__OPS:
            op = self.__OPS[text]
            text = ''
        elif text.startswith(('W', 'N', 'U')):
            ops = text.split()
            op = int(ops[0][1:], base=0)
            text = ' '.join(ops[1:])
        elif text.startswith('D$'):
            op = addr
            text = text[2:].strip()
        elif text.startswith(('ix', 'iy')):
            op = self.__OPS[text[:2]]
            text = text[2:].strip()
        else:
            raise _UnknownInstrError()

        # Offset.
        if text != '' and text[0] in ('+', '-'):
            sign = text[0]
            text = text[1:].strip()

            offset = int(text, base=0)
            text = ''

            if sign == '-':
                offset = -offset

            if isinstance(op, int):
                op += offset
            elif isinstance(op, IndexReg):
                op = Add(op, offset)
            else:
                raise _UnknownInstrError()

        assert text == '', text

        return op

    def build_instr(self, addr: int, image: bytes) -> Instr:
        original_text, size = Z80Machine._disasm(image)
        if size > len(image):
            # TODO: Too few bytes to disassemble this instruction.
            assert 0, image

        try:
            text = original_text.split(maxsplit=1)

            assert len(text) > 0, (original_text, image)
            name = text.pop(0)

            if name not in self.__INSTRS:
                raise _UnknownInstrError()

            # Parse operands.
            ops = []
            if text:
                text = text[0].split(',')
                while text:
                    op_text = text.pop(0).strip()
                    op = self.__build_op(addr, op_text)

                    if op is not None:
                        ops.append(op)

            instr = self.__INSTRS[name](*ops)
            instr.addr = addr
            instr.size = size
        except _UnknownInstrError:
            instr = UnknownInstr(addr, image[0])
            instr.text = original_text
            return instr

        return instr


class _TagSet(object):
    def __init__(self) -> None:
        self.infront_tags: list[_Tag] = []
        self.inline_tags: list[_Tag] = []
        self.byte_tag: _Tag | None = None
        self.disasm_tag: _Tag | None = None

    @property
    def empty(self) -> bool:
        return (len(self.infront_tags) == 0 and
                len(self.inline_tags) == 0 and
                self.byte_tag is None and
                self.disasm_tag is None)


class _AsmLine(object):
    _MAX_NUM_OF_BYTES_PER_LINE = 4
    _BYTES_INDENT = 40
    __COMMENT_INDENT = (_BYTES_INDENT + len('; @@ 0x0000') +
                        len(' ff') * _MAX_NUM_OF_BYTES_PER_LINE +
                        len('  '))

    def __init__(self, command: str | _Tag | None = None,
                 addr: int | None = None, xbytes: list[int] = [],
                 comment: str | _Tag | None = None, size: int = 0):
        self.command = command
        self.addr = addr
        self.xbytes = xbytes
        self.comment = comment
        self.size = size

    @staticmethod
    def _verbalize_comment(comment: str, force_leader: bool = True) -> str:
        if comment.startswith('.'):
            force_leader = True
        if force_leader:
            comment = '-- %s' % comment
        return comment

    def __str__(self) -> str:
        line = ' ' * 4
        out_of_line = isinstance(self.command, _Tag)
        if self.command is not None and not out_of_line:
            line += str(self.command)
        if (self.addr is not None or
                len(self.xbytes) > 0 or
                self.comment is not None):
            if not out_of_line:
                line = line.ljust(self._BYTES_INDENT)
            line += ';'
        if self.addr is not None:
            if not isinstance(self.comment, _HintTag):
                line += ' @@'
            else:
                line += '   '
            line += ' %#06x' % self.addr
        if len(self.xbytes) > 0:
            assert self.addr is not None
            line += ' %s' % ' '.join('%02x' % b for b in self.xbytes)
        if out_of_line:
            assert isinstance(self.command, _Tag)
            assert isinstance(self.command.comment, str)
            line += ' %s' % self._verbalize_comment(self.command.comment,
                                                    force_leader=False)
        if self.comment is not None:
            line = line.ljust(self.__COMMENT_INDENT)
            if isinstance(self.comment, _HintTag):
                line += str(self.comment.comment)
            else:
                line += str(self.comment)
        return line.rstrip()


class _Disasm(object):
    __TAG_PRIORITIES = {
        # These form the binary image so they have to be
        # processed first.
        _ByteTag: 0,
        _IncludeBinaryTag: 0,
        _InstrTag: 0,

        _DisasmTag: 1,

        _CommentTag: 2,
        _InlineCommentTag: 2,
    }

    def __init__(self) -> None:
        # TODO: Let user choose the CPU type.
        self.__instr_builder = Z80InstrBuilder()

        # Translates addresses to tags associated with those
        # addresses.
        self.__tags: typing.DefaultDict[int, _TagSet] = (
            collections.defaultdict(_TagSet))

        # Tags to process stored in order.
        self.__worklists: dict[int, typing.Deque[_Tag]] = dict()

    def __get_worklist(self, tag: _Tag) -> typing.Deque[_Tag]:
        # Use deque because of its popleft() being much faster
        # than list's pop(0).
        Worklist = collections.deque

        priority = self.__TAG_PRIORITIES[type(tag)]
        if priority not in self.__worklists:
            self.__worklists[priority] = Worklist()
        return self.__worklists[priority]

    def add_tags(self, *tags: _Tag) -> None:
        for tag in reversed(tags):
            self.__get_worklist(tag).appendleft(tag)

    def __process_byte_tag(self, tag: _Tag) -> None:
        prev_tag = self.__tags[tag.addr].byte_tag
        if prev_tag is not None:
            raise _DisasmError(
                tag, 'Byte redefined.',
                _DisasmError(prev_tag, 'Previously defined here.'))

        self.__tags[tag.addr].byte_tag = tag

    def __process_include_binary_tag(self, tag: _Tag) -> None:
        assert isinstance(tag, _IncludeBinaryTag)
        new_tags: list[_Tag] = []

        comment = 'Included from binary file %r.' % tag.filename.literal
        new_tags.append(_CommentTag(tag.origin, tag.addr, comment))

        if tag.comment is not None:
            assert isinstance(tag.comment, _Token)  # TODO
            assert isinstance(tag.comment.literal, str)  # TODO
            new_tags.append(_CommentTag(tag.origin, tag.addr,
                                        tag.comment.literal))

        for i, b in enumerate(tag.image):
            new_tags.append(_ByteTag(tag.origin, tag.addr + i, b))

        # TODO: Not really adding tags.
        self.add_tags(*new_tags)

    def __process_comment_tag(self, tag: _Tag) -> None:
        assert isinstance(tag, _CommentTag)
        self.__tags[tag.addr].infront_tags.append(tag)

    def __process_inline_comment_tag(self, tag: _Tag) -> None:
        assert isinstance(tag, _InlineCommentTag)
        self.__tags[tag.addr].inline_tags.append(tag)

    def __process_instr_tag(self, tag: _Tag) -> None:
        assert isinstance(tag, _InstrTag)
        self.__tags[tag.addr].inline_tags.append(tag)
        self.add_tags(_DisasmTag(tag.origin, tag.addr))

    def __process_disasm_tag(self, tag: _Tag) -> None:
        assert isinstance(tag, _DisasmTag)
        tags = self.__tags[tag.addr]
        if tags.disasm_tag is not None:
            return

        MAX_INSTR_SIZE = 4

        instr_image = []
        assert isinstance(tag.addr, int), tag.addr
        for i in range(tag.addr, tag.addr + MAX_INSTR_SIZE):
            if self.__tags[i].byte_tag is None:
                break

            t = self.__tags[i].byte_tag
            assert isinstance(t, _ByteTag)
            instr_image.append(t.value)

        if len(instr_image) == 0:
            return

        instr = self.__instr_builder.build_instr(tag.addr,
                                                 bytes(instr_image))
        tag.instr = instr
        self.__tags[tag.addr].disasm_tag = tag

        if not isinstance(instr, UnknownInstr):
            # Disassemble the following instruction.
            if (not isinstance(instr, JumpInstr) or
                    isinstance(instr, CallInstr) or
                    instr.conditional):
                assert isinstance(instr.addr, int)
                assert isinstance(instr.size, int)
                target = instr.addr + instr.size
                self.add_tags(_DisasmTag(instr.origin, target))

            # Disassemble jump targets.
            if (isinstance(instr, JumpInstr) and
                    not isinstance(instr, RetInstr)):
                assert isinstance(instr.target, int)
                target = instr.target
                if not isinstance(target, At):
                    assert isinstance(target, int)
                    self.add_tags(_DisasmTag(instr.origin, target))

    __TAG_PROCESSORS = {
        _ByteTag: __process_byte_tag,
        _CommentTag: __process_comment_tag,
        _IncludeBinaryTag: __process_include_binary_tag,
        _InlineCommentTag: __process_inline_comment_tag,
        _InstrTag: __process_instr_tag,
        _DisasmTag: __process_disasm_tag,
    }

    def __process_tag(self, tag: _Tag) -> None:
        assert tag.addr is not None
        process = self.__TAG_PROCESSORS[type(tag)]
        process(self, tag)

    def disassemble(self) -> None:
        while self.__worklists:
            priority = min(self.__worklists)
            worklist = self.__worklists[priority]
            tag = worklist.popleft()

            if len(worklist) == 0:
                del self.__worklists[priority]

            self.__process_tag(tag)

    def __get_inline_comments(
            self, addr: int,
            first_instr_byte: bool = False) -> (
                typing.Generator[str | _Tag, None, None]):
        for tag in self.__tags[addr].inline_tags:
            if isinstance(tag, _InstrTag):
                comment = '.instr'
                if tag.comment is not None:
                    assert isinstance(tag.comment, _Token)
                    assert isinstance(tag.comment.literal, str)
                    comment += ' %s' % (
                        _AsmLine._verbalize_comment(tag.comment.literal))
                yield comment
            elif isinstance(tag, _InlineCommentTag):
                assert isinstance(tag.comment, str)
                yield _AsmLine._verbalize_comment(tag.comment,
                                                  force_leader=False)
            else:
                assert 0, tag

        disasm_tag = self.__tags[addr].disasm_tag
        if disasm_tag is not None:
            assert isinstance(disasm_tag, _DisasmTag)
            instr = disasm_tag.instr
            if not first_instr_byte:
                yield _HintTag(instr.origin, addr,
                               'warning: overlapping instruction: '
                               '%r' % str(instr))

            if isinstance(instr, UnknownInstr):
                yield _HintTag(instr.origin, addr,
                               'warning: unknown instruction: '
                               '%r' % instr.text)

    def __is_commentless_addr(self, addr: int) -> bool:
        tags = self.__tags[addr]
        if tags.disasm_tag is not None or len(tags.infront_tags) != 0:
            return False

        if any(True for _ in self.__get_inline_comments(addr)):
            return False

        return True

    def __get_instr_lines(self, instr: Instr) -> (
            typing.Generator[_AsmLine, None, None]):
        command: None | str = str(instr)
        addr = instr.addr
        assert isinstance(addr, int)
        t = self.__tags[addr].byte_tag
        assert isinstance(t, _ByteTag)
        xbytes = [t.value]

        assert isinstance(instr.size, int)
        end_addr = addr + instr.size
        byte_addr = addr + 1

        while addr < end_addr:
            while (byte_addr < end_addr and
                   (len(xbytes) == 0 or
                    self.__is_commentless_addr(byte_addr)) and
                   len(xbytes) < _AsmLine._MAX_NUM_OF_BYTES_PER_LINE):
                t = self.__tags[byte_addr].byte_tag
                assert isinstance(t, _ByteTag)
                xbytes.append(t.value)
                byte_addr += 1

            for tag in self.__tags[addr].infront_tags:
                yield _AsmLine(addr=addr, command=tag)

            first_instr_byte = addr == instr.addr
            inline_comments = list(
                self.__get_inline_comments(addr, first_instr_byte))
            while len(xbytes) > 0 or len(inline_comments) > 0:
                comment = None
                if (len(inline_comments) > 0 and
                    (len(xbytes) == 0 or
                        not isinstance(inline_comments[0], _HintTag))):
                    comment = inline_comments.pop(0)

                yield _AsmLine(command=command, addr=addr, xbytes=xbytes,
                               comment=comment, size=len(xbytes))

                command = None
                xbytes = []

            addr = byte_addr

    def __get_data_lines(self, addr: int) -> (
            typing.Generator[_AsmLine, None, None]):
        tags = self.__tags[addr]
        for tag in tags.infront_tags:
            yield _AsmLine(addr=addr, command=tag)

        if tags.byte_tag is None:
            return

        assert isinstance(tags.byte_tag, _ByteTag)
        xbytes = [tags.byte_tag.value]
        inline_comments = list(self.__get_inline_comments(addr))

        if len(inline_comments) == 0:
            byte_addr = addr + 1

            while len(xbytes) < _AsmLine._MAX_NUM_OF_BYTES_PER_LINE:
                byte_tag = self.__tags[byte_addr].byte_tag
                if byte_tag is None:
                    break

                if not self.__is_commentless_addr(byte_addr):
                    break

                # Break at aligned addresses to reduce the amount
                # of changes when adding more addresses to
                # disassemble. This makes the output more
                # friendly to source version control systems,
                # such as git.
                if byte_addr % _AsmLine._MAX_NUM_OF_BYTES_PER_LINE == 0:
                    break

                assert isinstance(byte_tag, _ByteTag)
                xbytes.append(byte_tag.value)
                byte_addr += 1

        while len(xbytes) > 0 or len(inline_comments) > 0:
            comment = None
            if len(inline_comments) > 0:
                comment = inline_comments.pop(0)

            command = 'db %s' % ', '.join('%#04x' % b for b in xbytes)

            yield _AsmLine(command=command, addr=addr, xbytes=xbytes,
                           comment=comment, size=len(xbytes))
            xbytes = []

    def __get_lines_for_addr(self, addr: int) -> (
            typing.Generator[_AsmLine, None, None]):
        disasm_tag = self.__tags[addr].disasm_tag
        if disasm_tag is not None:
            assert isinstance(disasm_tag, _DisasmTag)
            yield from self.__get_instr_lines(disasm_tag.instr)
        else:
            yield from self.__get_data_lines(addr)

    def __get_asm_lines(self) -> typing.Generator[_AsmLine, None, None]:
        yield _AsmLine()

        addr = None
        for a in sorted(a for a, t in self.__tags.items() if not t.empty):
            if addr is None:
                addr = a
                yield _AsmLine(command='org 0x%x' % addr)
            elif a < addr:
                continue
            elif a > addr:
                yield _AsmLine(command='.space %d' % (a - addr))
                addr = a

            assert a == addr

            for line in self.__get_lines_for_addr(addr):
                yield line
                addr += line.size

    def _get_output(self) -> typing.Generator[str, None, None]:
        for line in self.__get_asm_lines():
            yield '%s\n' % line

    def save_output(self, filename: str) -> None:
        try:
            with tempfile.NamedTemporaryFile(mode='w', delete=False) as f:
                for chunk in self._get_output():
                    f.write(chunk)

            os.rename(f.name, filename)
            del f
        finally:
            if f is not None:
                os.remove(f.name)
