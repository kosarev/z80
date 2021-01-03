#!/usr/bin/env python3
# -*- coding: utf-8 -*-

#   Z80 CPU Emulator.
#   https://github.com/kosarev/z80
#
#   Copyright (C) 2017-2021 Ivan Kosarev.
#   ivan@kosarev.info
#
#   Published under the MIT license.

import collections
import os
import tempfile
from ._error import Error
from ._instr import (ADD, ADC, AND, CP, OR, SBC, SUB, XOR, BIT, CALL, CCF, CPL,
                     DAA, DEC, DI, DJNZ, EI, EX, EXX, HALT, IM, INC, IN, JP,
                     JR, LD, LDDR, LDIR, NEG, NOP, RLC, RL, RR, RRC, SLA, SRA,
                     SRL, OUT, POP, PUSH, RES, RET, RLA, RLCA, RLD, RRA, RRCA,
                     RST, SCF, SET, A, AF, AF2, CF, M, NC, NZ, PO, P, Z, DE,
                     BC, HL, IReg, IY, IX, SP, B, C, D, E, H, L, UnknownInstr,
                     JumpInstr, CallInstr, RetInstr, At, IndexReg, Add)
from ._machine import Z80Machine


class _DisasmError(Error):
    def __init__(self, subject, message, *notes):
        super().__init__('%s: %s: %s' % (subject.origin.inline_text,
                                         subject, message))
        self.subject = subject
        self.message = message
        self.notes = notes

    def verbalize(self, program_name=None):
        def g():
            yield self.subject.origin.context_text

            if program_name is not None:
                yield '%s: ' % program_name

            yield '%s' % self.reason

            for n in self.notes:
                yield '\n'
                yield n.verbalize()

        return ''.join(g())


class _Tag(object):
    def __init__(self, origin, addr, size):
        self.origin = origin
        self.addr = addr
        self.size = size
        self.comment = None

    def __str__(self):
        return '%s tag' % self.ID

    def __repr__(self):
        return '(%#06x, %s, %r)' % (self.addr, self.ID, self.comment)


class _CommentTag(_Tag):
    ID = 'comment'

    def __init__(self, origin, addr, comment):
        super().__init__(origin, addr, size=0)
        self.comment = comment


class _InlineCommentTag(_Tag):
    ID = 'inline_comment'

    def __init__(self, origin, addr, comment):
        super().__init__(origin, addr, size=0)
        self.comment = comment


class _HintTag(_Tag):
    ID = 'hint'

    def __init__(self, origin, addr, hint):
        super().__init__(origin, addr, size=0)
        self.comment = hint


class _ByteTag(_Tag):
    ID = 'byte'

    def __init__(self, origin, addr, value):
        super().__init__(origin, addr, size=1)
        self.value = value

    def __repr__(self):
        return '(%#06x, %s, %#04x, %r)' % (
            self.addr, self.ID, self.value, self.comment)


class _IncludeBinaryTag(_Tag):
    ID = 'include_binary'

    def __init__(self, origin, addr, filename, image):
        super().__init__(origin, addr, size=len(image))
        self.filename = filename
        self.image = image

    def __repr__(self):
        return '(%#06x, %s, %s, %s)' % (
            self.addr, self.ID, self.filename, self.comment)


class _InstrTag(_Tag):
    ID = 'instr'

    def __init__(self, origin, addr):
        super().__init__(origin, addr, size=0)


class _DisasmTag(_Tag):
    ID = 'disasm'

    def __init__(self, origin, addr):
        super().__init__(origin, addr, size=0)


class _UnknownInstrError(Exception):
    pass


class _Z80InstrBuilder(object):
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
        'inc': INC,
        'in': IN,
        'jp': JP,
        'jr': JR,
        'ld': LD,
        'Llddr': LDDR,
        'Lldir': LDIR,
        'neg': NEG,
        'nop': NOP,
        'Orlc': RLC,
        'Orl': RL,
        'Orr': RR,
        'Orrc': RRC,
        'Osla': SLA,
        'Osra': SRA,
        'Osrl': SRL,
        'out': OUT,
        'pop': POP,
        'push': PUSH,
        'res': RES,
        'ret': RET,
        'rla': RLA,
        'rlca': RLCA,
        'rld': RLD,
        'rra': RRA,
        'rrca': RRCA,
        'rst': RST,
        'sbc': SBC,
        'scf': SCF,
        'set': SET,
    }

    __OPS = {
        'a': A,
        'af': AF,
        'af\'': AF2,
        'Cc': CF,
        'Cm': M,
        'Cnc': NC,
        'Cnz': NZ,
        'Cpo': PO,
        'Cp': P,
        'Cz': Z,
        'de': DE,
        'Gaf': AF,
        'Gbc': BC,
        'Gde': DE,
        'Ghl': HL,
        'hl': HL,
        'i': IReg,
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
        'sp': SP,
    }

    def __build_op(self, addr, text):
        if text.startswith('R('):
            text = text[1:]

        if text.startswith('('):
            assert text[-1] == ')'
            return At(self.__build_op(addr, text[1:-1]))

        # Base operand.
        if text in self.__OPS:
            op = self.__OPS[text]
            text = ''
        elif text.startswith(('W', 'N', 'U')):
            text = text.split()
            op = int(text[0][1:], base=0)
            text = ' '.join(text[1:])
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

    def build_instr(self, addr, image):
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
    def __init__(self):
        self.infront_tags = []
        self.inline_tags = []
        self.byte_tag = None
        self.disasm_tag = None

    @property
    def empty(self):
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

    def __init__(self, command=None, addr=None, xbytes=[], comment=None,
                 size=0):
        self.command = command
        self.addr = addr
        self.xbytes = xbytes
        self.comment = comment
        self.size = size

    @staticmethod
    def _verbalize_comment(comment, force_leader=True):
        if comment.startswith('.'):
            force_leader = True
        if force_leader:
            comment = '-- %s' % comment
        return comment

    def __str__(self):
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

    def __init__(self):
        # TODO: Let user choose the CPU type.
        self.__instr_builder = _Z80InstrBuilder()

        # Translates addresses to tags associated with those
        # addresses.
        self.__tags = collections.defaultdict(_TagSet)

        # Tags to process stored in order.
        self.__worklists = dict()

    def __get_worklist(self, tag):
        # Use deque because of its popleft() being much faster
        # than list's pop(0).
        Worklist = collections.deque

        priority = self.__TAG_PRIORITIES[type(tag)]
        if priority not in self.__worklists:
            self.__worklists[priority] = Worklist()
        return self.__worklists[priority]

    def add_tags(self, *tags):
        for tag in reversed(tags):
            self.__get_worklist(tag).appendleft(tag)

    def __process_byte_tag(self, tag):
        prev_tag = self.__tags[tag.addr].byte_tag
        if prev_tag is not None:
            raise _DisasmError(
                tag, 'Byte redefined.',
                _DisasmError(prev_tag, 'Previously defined here.'))

        self.__tags[tag.addr].byte_tag = tag

    def __process_include_binary_tag(self, tag):
        new_tags = []

        comment = 'Included from binary file %r.' % tag.filename.literal
        new_tags.append(_CommentTag(tag.origin, tag.addr, comment))

        if tag.comment is not None:
            new_tags.append(_CommentTag(tag.origin, tag.addr,
                                        tag.comment.literal))

        for i, b in enumerate(tag.image):
            new_tags.append(_ByteTag(tag.origin, tag.addr + i, b))

        # TODO: Not really adding tags.
        self.add_tags(*new_tags)

    def __process_comment_tag(self, tag):
        self.__tags[tag.addr].infront_tags.append(tag)

    def __process_inline_comment_tag(self, tag):
        self.__tags[tag.addr].inline_tags.append(tag)

    def __process_instr_tag(self, tag):
        self.__tags[tag.addr].inline_tags.append(tag)
        self.add_tags(_DisasmTag(tag.origin, tag.addr))

    def __process_disasm_tag(self, tag):
        tags = self.__tags[tag.addr]
        if tags.disasm_tag is not None:
            return

        MAX_INSTR_SIZE = 4

        instr_image = []
        assert isinstance(tag.addr, int), tag.addr
        for i in range(tag.addr, tag.addr + MAX_INSTR_SIZE):
            if self.__tags[i].byte_tag is None:
                break

            instr_image.append(self.__tags[i].byte_tag.value)

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
                target = instr.addr + instr.size
                self.add_tags(_DisasmTag(instr.origin, target))

            # Disassemble jump targets.
            if (isinstance(instr, JumpInstr) and
                    not isinstance(instr, RetInstr)):
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

    def __process_tag(self, tag):
        assert tag.addr is not None
        process = self.__TAG_PROCESSORS[type(tag)]
        process(self, tag)

    def disassemble(self):
        while self.__worklists:
            priority = min(self.__worklists)
            worklist = self.__worklists[priority]
            tag = worklist.popleft()

            if len(worklist) == 0:
                del self.__worklists[priority]

            self.__process_tag(tag)

    def __get_inline_comments(self, addr, first_instr_byte=False):
        for tag in self.__tags[addr].inline_tags:
            if isinstance(tag, _InstrTag):
                comment = '.instr'
                if tag.comment is not None:
                    comment += ' %s' % (
                        _AsmLine._verbalize_comment(tag.comment.literal))
                yield comment
            elif isinstance(tag, _InlineCommentTag):
                yield _AsmLine._verbalize_comment(tag.comment,
                                                  force_leader=False)
            else:
                assert 0, tag

        disasm_tag = self.__tags[addr].disasm_tag
        if disasm_tag is not None:
            instr = disasm_tag.instr
            if not first_instr_byte:
                yield _HintTag(instr.origin, addr,
                               'warning: overlapping instruction: '
                               '%r' % str(instr))

            if isinstance(instr, UnknownInstr):
                yield _HintTag(instr.origin, addr,
                               'warning: unknown instruction: '
                               '%r' % instr.text)

    def __is_commentless_addr(self, addr):
        tags = self.__tags[addr]
        if tags.disasm_tag is not None or len(tags.infront_tags) != 0:
            return False

        if any(True for _ in self.__get_inline_comments(addr)):
            return False

        return True

    def __get_instr_lines(self, instr):
        command = str(instr)
        addr = instr.addr
        xbytes = [self.__tags[addr].byte_tag.value]

        end_addr = addr + instr.size
        byte_addr = addr + 1

        while addr < end_addr:
            while (byte_addr < end_addr and
                   (len(xbytes) == 0 or
                    self.__is_commentless_addr(byte_addr)) and
                   len(xbytes) < _AsmLine._MAX_NUM_OF_BYTES_PER_LINE):
                xbytes.append(self.__tags[byte_addr].byte_tag.value)
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

    def __get_data_lines(self, addr):
        tags = self.__tags[addr]
        for tag in tags.infront_tags:
            yield _AsmLine(addr=addr, command=tag)

        if tags.byte_tag is None:
            return

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

    def __get_lines_for_addr(self, addr):
        disasm_tag = self.__tags[addr].disasm_tag
        if disasm_tag is not None:
            yield from self.__get_instr_lines(disasm_tag.instr)
        else:
            yield from self.__get_data_lines(addr)

    def __get_asm_lines(self):
        yield _AsmLine()

        addr = None
        for a in sorted(a for a, t in self.__tags.items() if not t.empty):
            if addr is None:
                addr = a
            elif a < addr:
                continue
            elif a > addr:
                yield _AsmLine(command='.space %d' % (a - addr))
                addr = a

            assert a == addr

            for line in self.__get_lines_for_addr(addr):
                yield line
                addr += line.size

    def _get_output(self):
        for line in self.__get_asm_lines():
            yield '%s\n' % line

    def save_output(self, filename):
        try:
            with tempfile.NamedTemporaryFile(mode='w', delete=False) as f:
                for chunk in self._get_output():
                    f.write(chunk)

            os.rename(f.name, filename)
            f = None
        finally:
            if f is not None:
                os.remove(f.name)
