#!/usr/bin/env python3
# -*- coding: utf-8 -*-

#   Z80 CPU Emulator.
#   https://github.com/kosarev/z80
#
#   Copyright (C) 2017-2020 Ivan Kosarev.
#   ivan@kosarev.info
#
#   Published under the MIT license.

import bisect
import collections
import re
from ._error import Error
from ._machine import Z80Machine


class _SourcePos(object):
    def __init__(self, offset, file):
        self.__offset = offset
        self.__file = file

    @property
    def __coordinates(self):
        return self.__file.get_coordinates(self.__offset)

    @property
    def inline_text(self):
        line, line_no, column_no = self.__coordinates
        return '%s:%s:%s' % (self.__file, line_no, column_no)

    def __str__(self):
        return self.inline_text

    @property
    def context_text(self):
        line, line_no, column_no = self.__coordinates
        return '%s\n%s^\n' % (line, ' ' * column_no)


class _SourceFile(object):
    def __init__(self, filename, image=None):
        self.__filename = filename

        if image is None:
            with open(self.__filename) as f:
                image = f.read()

        self.__image = image

        self.__line_breaks = tuple(
            i for i, c in enumerate(self.__image) if c == '\n')

    def __repr__(self):
        return self.__filename

    def get_image(self):
        return self.__image

    def get_coordinates(self, offset):
        i = bisect.bisect_left(self.__line_breaks, offset)

        line_start = self.__line_breaks[i - 1] + 1 if i > 0 else 0
        line_end = (self.__line_breaks[i] if i < len(self.__line_breaks)
                    else len(self.__image))
        line = self.__image[line_start:line_end]

        line_no = i + 1
        column_no = offset - line_start

        return line, line_no, column_no


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


class _Token(object):
    def __init__(self, literal, pos):
        self.literal = literal
        self.pos = pos

    def __eq__(self, other):
        literal = other.literal if isinstance(other, _Token) else other
        return self.literal == literal

    def __repr__(self):
        return '%s: %s' % (self.pos, self.literal)

    def __str__(self):
        return self.literal

    @property
    def origin(self):
        return self.pos


class _Tokenizer(object):
    __WHITESPACE = re.compile(r'(\ |\t)+')
    __END_OF_LINE = re.compile(r'(\n|$)')

    def __init__(self, source_file):
        self.__source_file = source_file
        self.__image = self.__source_file.get_image()
        self.__offset = 0
        self.__token_offset = self.__offset

    @property
    def pos(self):
        return _SourcePos(self.__offset, self.__source_file)

    def skip(self, pattern):
        match = pattern.match(self.__image, self.__offset)
        if match is not None:
            self.__offset = match.end()

    def skip_whitespace(self):
        self.skip(self.__WHITESPACE)

    def skip_to(self, pattern):
        match = pattern.search(self.__image, self.__offset)
        if match is not None:
            self.__offset = match.start()

    def skip_rest_of_line(self):
        self.skip_to(self.__END_OF_LINE)

    def skip_next(self, pattern):
        match = pattern.search(self.__image, self.__offset)
        if match is not None:
            self.__offset = match.end()
        return match

    def start_token(self):
        self.__token_offset = self.__offset

    def end_token(self):
        pos = _SourcePos(self.__token_offset, self.__source_file)

        literal = None
        if self.__token_offset < self.__offset:
            literal = self.__image[self.__token_offset:self.__offset]

        # Ending a token implicitly starts another one.
        self.start_token()

        return _Token(literal, pos)


class _Tag(object):
    def __init__(self, origin, addr, size, implicit=False):
        self.origin = origin
        self.addr = addr
        self.size = size
        self.comment = None
        self.implicit = implicit

    def __str__(self):
        return '%s tag' % self.ID

    def __repr__(self):
        return '(%#06x, %s, %r)' % (self.addr, self.ID, self.comment)


class _CommentTag(_Tag):
    ID = 'comment'

    def __init__(self, origin, addr, comment):
        super().__init__(origin, addr, size=0)
        self.comment = comment


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

    def __init__(self, origin, addr, implicit=False):
        super().__init__(origin, addr, size=0, implicit=implicit)


class CondFlag(type):
    def __repr__(cls):
        return cls.__name__

    def __str__(cls):
        return cls.__name__.lower()[:-1]


class CF(metaclass=CondFlag):
    pass


class NCF(metaclass=CondFlag):
    pass


class ZF(metaclass=CondFlag):
    pass


class NZF(metaclass=CondFlag):
    pass


class Reg(type):
    def __repr__(cls):
        return cls.__name__

    def __str__(cls):
        s = cls.__name__.lower()
        if s.endswith('reg'):
            s = s[:-3]
        return s


class IndexReg(Reg):
    pass


class A(metaclass=Reg):
    pass


class B(metaclass=Reg):
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


class BC(metaclass=Reg):
    pass


class DE(metaclass=Reg):
    pass


class HL(metaclass=Reg):
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
    def __init__(self):
        self.addr = None
        self.size = None
        self.ops = []

    def __repr__(self):
        return (type(self).__name__ +
                '(' + ', '.join(repr(op) for op in self.ops) + ')')

    def __str__(self):
        s = type(self).__name__.lower()
        if self.ops:
            s += ' ' + ', '.join(_str_op(op) for op in self.ops)
        return s


class UnknownInstr(Instr):
    def __init__(self, opcode):
        super().__init__()
        self.opcode = opcode
        self.size = 1
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
        return len(self.ops) > 0 and isinstance(self.ops[0], CondFlag)


class CallInstr(JumpInstr):
    pass


class RetInstr(JumpInstr):
    pass


class ADD(Instr):
    pass


class AND(Instr):
    pass


class BIT(Instr):
    pass


class CALL(CallInstr):
    pass


class CP(Instr):
    pass


class DEC(Instr):
    pass


class DI(Instr):
    pass


class EI(Instr):
    pass


class EX(Instr):
    pass


class EXX(Instr):
    pass


class IM(Instr):
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


class NOP(Instr):
    pass


class OUT(Instr):
    pass


class POP(Instr):
    pass


class PUSH(Instr):
    pass


class RET(RetInstr):
    pass


class SBC(Instr):
    pass


class SET(Instr):
    pass


class XOR(Instr):
    pass


class _Z80InstrBuilder(object):
    __INSTRS = {
        'Aadd': ADD,
        'Aand': AND,
        'Acp': CP,
        'add': ADD,
        'Axor': XOR,
        'bit': BIT,
        'call': CALL,
        'dec': DEC,
        'di': DI,
        'ei': EI,
        'ex': EX,
        'exx': EXX,
        'im': IM,
        'inc': INC,
        'jp': JP,
        'jr': JR,
        'ld': LD,
        'Llddr': LDDR,
        'Lldir': LDIR,
        'nop': NOP,
        'out': OUT,
        'pop': POP,
        'push': PUSH,
        'ret': RET,
        'sbc': SBC,
        'set': SET,
    }

    __OPS = {
        'a': A,
        'Cc': CF,
        'Cnc': NCF,
        'Cnz': NZF,
        'Cz': ZF,
        'de': DE,
        'Gde': DE,
        'Ghl': HL,
        'hl': HL,
        'i': IReg,
        'iy': IY,
        'Pbc': BC,
        'Pde': DE,
        'Phl': HL,
        'Piy': IY,
        'Ra': A,
        'Rb': B,
        'Rd': D,
        'Re': E,
        'Rh': H,
        'Rl': L,
        'sp': SP,
    }

    class __UnknownInstrError(Exception):
        pass

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
            raise self.__UnknownInstrError()

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
                raise self.__UnknownInstrError()

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
                raise self.__UnknownInstrError()

            instr = self.__INSTRS[name]()
            instr.addr = addr
            instr.size = size

            # Parse operands.
            if text:
                text = text[0].split(',')
                while text:
                    op_text = text.pop(0).strip()
                    op = self.__build_op(addr, op_text)

                    if op is not None:
                        instr.ops.append(op)
        except self.__UnknownInstrError:
            instr = UnknownInstr(image[0])
            instr.text = original_text
            return instr

        return instr


class _TagParser(object):
    __TAG_LEADER = re.compile('@@')

    __TOKENS = re.compile(
        r'([_a-z][_0-9a-z]*)|'      # An identifier.
        r'([0-9][0-9a-z]*)|'        # A number.
        r"'(\\'|\\\\|[^'\\\n])*'|"  # A string.
        r'.')                       # Or any other single character.

    def __init__(self, source_file):
        self.__toks = _Tokenizer(source_file)
        self.__tok = None

    def __fetch_token(self, error=None):
        self.__toks.skip_whitespace()

        self.__toks.start_token()
        self.__toks.skip(self.__TOKENS)
        tok = self.__toks.end_token()

        if tok.literal == '\n':
            tok.literal = None

        if tok.literal is None:
            if error is not None:
                raise _DisasmError(tok.pos, error)

            tok = None
        else:
            # Translate escape sequences.
            if tok.literal.startswith("'"):
                if tok.literal == "'":
                    raise _DisasmError(tok.pos, 'Unterminated string.')

                tok.literal = (tok.literal[1:-1].
                               replace('\\\\', '\\').
                               replace("\\'", "'"))

        self.__tok = tok

        return self.__tok

    def __evaluate_numeric_literal(self, literal):
        try:
            return int(literal, base=0)
        except ValueError:
            return None

    def __parse_comment(self):
        self.__toks.skip_whitespace()
        self.__toks.start_token()
        self.__toks.skip_rest_of_line()
        self.__tok = self.__toks.end_token()
        return self.__tok

    def __parse_include_binary_tag(self, addr, name):
        filename = self.__fetch_token('A filename expected.')
        self.__fetch_token()

        with open(filename.literal, 'rb') as f:
            image = f.read()

        return _IncludeBinaryTag(name.pos, addr, filename, image)

    def __parse_instr_tag(self, addr, name):
        self.__fetch_token()
        return _InstrTag(name.pos, addr)

    __TAG_PARSERS = {
        _IncludeBinaryTag.ID: __parse_include_binary_tag,
        _InstrTag.ID: __parse_instr_tag,
    }

    # Parses and returns a subsequent tag.
    def __iter__(self):
        while self.__toks.skip_next(self.__TAG_LEADER):
            tok = self.__fetch_token()

            # Parse optional tag address.
            addr = self.__evaluate_numeric_literal(tok.literal)
            if addr is not None:
                tok = self.__fetch_token()

            tags = []
            subject_tag = None

            # Collect bytes, if any specified.
            byte_offset = 0
            while tok is not None:
                value = self.__evaluate_numeric_literal(tok.literal)
                if value is None:
                    break

                tag = _ByteTag(tok.pos, addr + byte_offset, value)
                if subject_tag is None:
                    subject_tag = tag

                tags.append(tag)
                byte_offset += 1

                tok = self.__fetch_token()

            # Parse regular tags.
            if tok is not None and tok != ':':
                parser = self.__TAG_PARSERS.get(tok.literal, None)
                if not parser:
                    raise _DisasmError(tok, 'Unknown tag.')

                subject_tag = parser(self, addr, tok)
                tags.append(subject_tag)
                tok = self.__tok

            # Parse comments.
            if tok is not None:
                if tok != ':':
                    raise _DisasmError(tok,
                                       'End of line or a comment expected.')

                comment = self.__parse_comment()

                if subject_tag is None:
                    tags.append(_CommentTag(tok.pos, addr, comment))
                else:
                    assert subject_tag.comment is None
                    subject_tag.comment = comment

            yield from tags


class _AsmOutput(object):
    __COMMAND_INDENT = 4
    __COMMENT_INDENT = 40

    def __init__(self):
        self.__needs_empty_line = True

    def __write_empty_line_if_needed(self):
        if self.__needs_empty_line:
            yield '\n'
            self.__needs_empty_line = False

    def write_line(self, command, tag_addr, tag_body, tag_comment,
                   comment=None):
        yield from self.__write_empty_line_if_needed()

        line = ' ' * self.__COMMAND_INDENT

        if command is not None:
            line += command

        if comment is None:
            comment = []
        else:
            comment = [comment]

        if tag_addr is not None:
            comment.append('@@ %#06x' % tag_addr)

        if tag_body is not None:
            comment.append('%s' % tag_body)

        if tag_comment is not None:
            comment.append(': %s' % tag_comment)

        if command is not None and comment:
            line = line.ljust(self.__COMMENT_INDENT)

        if comment:
            line += '; %s' % ' '.join(comment)

        line += '\n'

        yield line

    def write_space_directive(self, size):
        yield from self.write_line('.space %d' % size, None, None, None)


class _Disasm(object):
    __TAG_PRIORITIES = {
        # These form the binary image so they have to be
        # processed first.
        _ByteTag: 0,
        _IncludeBinaryTag: 0,

        _InstrTag: 1,
        _CommentTag: 2,
    }

    def __init__(self):
        # TODO: Let user choose the CPU type.
        self.__instr_builder = _Z80InstrBuilder()

        # Translate addresses to tags associated with those
        # addresses.
        self.__tags = dict()

        # Tags to process stored in order.
        self.__worklists = dict()

        # Byte image.
        self.__bytes = dict()

        # Instructions.
        self.__instrs = dict()

    def __get_worklist(self, tag):
        # Use deque because of its popleft() is much faster than
        # list's pop(0).
        Worklist = collections.deque

        priority = self.__TAG_PRIORITIES[type(tag)]
        if priority not in self.__worklists:
            self.__worklists[priority] = Worklist()
        return self.__worklists[priority]

    def __add_tags(self, *tags, to_end_of_queue=False):
        for tag in tags:
            self.__tags.setdefault(tag.addr, []).append(tag)

        if to_end_of_queue:
            for tag in tags:
                self.__get_worklist(tag).append(tag)
        else:
            for tag in reversed(tags):
                self.__get_worklist(tag).appendleft(tag)

    def parse_tags(self, filename, image=None):
        addr = 0
        for tag in _TagParser(_SourceFile(filename, image)):
            if tag.addr is None:
                tag.addr = addr
            else:
                addr = tag.addr

            self.__add_tags(tag, to_end_of_queue=True)

            addr += tag.size

    def __process_byte_tag(self, tag):
        if tag.addr in self.__bytes:
            raise _DisasmError(
                tag, 'Byte redefined.',
                _DisasmError(self.__bytes[tag.addr],
                             'Previously defined here.'))

        self.__bytes[tag.addr] = tag

    def __process_include_binary_tag(self, tag):
        new_tags = []

        comment = 'Included from binary file %r.' % tag.filename.literal
        new_tags.append(_CommentTag(tag.origin, tag.addr, comment))

        if tag.comment is not None:
            new_tags.append(_CommentTag(tag.origin, tag.addr, tag.comment))

        for i, b in enumerate(tag.image):
            new_tags.append(_ByteTag(tag.origin, tag.addr + i, b))

        self.__add_tags(*new_tags)

    def __process_instr_tag(self, tag):
        if tag.addr in self.__instrs:
            return

        MAX_INSTR_SIZE = 4

        instr_image = []
        for i in range(tag.addr, tag.addr + MAX_INSTR_SIZE):
            if i not in self.__bytes:
                break

            instr_image.append(self.__bytes[i].value)

        if len(instr_image) == 0:
            return

        instr = self.__instr_builder.build_instr(tag.addr,
                                                 bytes(instr_image))
        tag.instr = instr
        self.__instrs[tag.addr] = tag

        if not isinstance(instr, UnknownInstr):
            # Disassemble the following instruction.
            if (not isinstance(instr, JumpInstr) or
                    isinstance(instr, CallInstr) or
                    instr.conditional):
                self.__add_tags(_InstrTag(None, instr.addr + instr.size,
                                          implicit=True))

            # Disassemble jump targets.
            if (isinstance(instr, JumpInstr) and
                    not isinstance(instr, RetInstr)):
                self.__add_tags(_InstrTag(None, instr.target,
                                          implicit=True))

    def __skip_processing_tag(self, tag):
        pass

    __TAG_PROCESSORS = {
        _ByteTag: __process_byte_tag,
        _CommentTag: __skip_processing_tag,
        _IncludeBinaryTag: __process_include_binary_tag,
        _InstrTag: __process_instr_tag,
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

    def __write_comment_tag(self, tag, out):
        yield from out.write_line(None, tag.addr, None, tag.comment)

    def __write_byte_tag(self, tag, out):
        tag_body = '%#04x' % tag.value
        yield from out.write_line('db %#04x' % tag.value,
                                  tag.addr, tag_body, tag.comment)

    def __write_instr_tag(self, tag, out):
        command = '%s' % tag.instr

        addrs = range(tag.addr, tag.addr + tag.instr.size)
        tag_body = ['%#04x' % self.__bytes[i].value for i in addrs]

        if not tag.implicit:
            tag_body.append('instr')

        tag_body = ' '.join(tag_body)

        comment = None
        if isinstance(tag.instr, UnknownInstr):
            comment = 'unknown_instr %r' % tag.instr.text

        yield from out.write_line(command,
                                  tag.addr, tag_body, tag.comment,
                                  comment)

    def __write_tags(self, addr, out):
        tags = self.__tags.get(addr, [])
        instr_tag = self.__instrs.get(addr, None)
        byte_tag = self.__bytes.get(addr, None)

        def g():
            # Emit tags in order.
            for tag in tags:
                if isinstance(tag, (_IncludeBinaryTag, _ByteTag, _InstrTag)):
                    pass
                elif isinstance(tag, _CommentTag):
                    yield from self.__write_comment_tag(tag, out)
                else:
                    # TODO
                    assert 0, tag

            if instr_tag is not None:
                yield from self.__write_instr_tag(instr_tag, out)
            elif byte_tag is not None:
                yield from self.__write_byte_tag(byte_tag, out)

        if instr_tag is not None:
            addr += instr_tag.instr.size
        elif byte_tag is not None:
            addr += 1

        return addr, g()

    def _get_output(self):
        out = _AsmOutput()

        addrs = sorted(self.__tags)
        if len(addrs) == 0:
            return

        addr = addrs[0]
        for a in addrs:
            if a < addr:
                # TODO: Handle cases when there are tags in the
                # middle of an instruction.
                assert len(self.__tags[a]) == 1
                assert isinstance(self.__tags[a][0], _ByteTag)
                continue

            if addr < a:
                yield from out.write_space_directive(a - addr)
                addr = a

            assert addr == a, (addr, a)
            addr, g = self.__write_tags(a, out)

            yield from g

    def save_output(self, filename):
        with open(filename, 'w') as f:
            for chunk in self._get_output():
                f.write(chunk)
