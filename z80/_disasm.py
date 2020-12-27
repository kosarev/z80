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

            # Collect bytes, if any specified.
            byte_offset = 0
            while tok is not None:
                value = self.__evaluate_numeric_literal(tok.literal)
                if value is None:
                    break

                tags.append(_ByteTag(tok.pos, addr + byte_offset, value))
                byte_offset += 1

                tok = self.__fetch_token()

            # Parse regular tags.
            if tok is not None and tok != ':':
                parser = self.__TAG_PARSERS.get(tok.literal, None)
                if not parser:
                    raise _DisasmError(tok, 'Unknown tag.')

                tags.append(parser(self, addr, tok))
                tok = self.__tok

            # Parse comments.
            if tok is not None:
                if tok != ':':
                    raise _DisasmError(tok,
                                       'End of line or a comment expected.')

                comment = self.__parse_comment()

                if len(tags) == 0:
                    tags.append(_CommentTag(tok.pos, addr, comment))
                else:
                    assert tags[0].comment is None
                    tags[0].comment = comment

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

    def write_line(self, command, tag_addr, tag_body, tag_comment):
        yield from self.__write_empty_line_if_needed()

        line = ' ' * self.__COMMAND_INDENT

        if command is not None:
            line += command

        comment = ''

        if tag_addr is not None:
            comment += '@@ %#06x' % tag_addr

        if tag_body is not None:
            comment += ' %s' % tag_body

        if tag_comment is not None:
            comment += ' : %s' % tag_comment

        if command is not None and comment != '':
            line = line.ljust(self.__COMMENT_INDENT)

        if comment != '':
            line += '; %s' % comment

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
        # Translate addresses to tags associated with those
        # addresses.
        self.__byte_tags = dict()
        self.__common_tags = dict()

        # Tags to process stored in order.
        self.__worklists = dict()

        # Byte image.
        self.__image = []

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
            if isinstance(tag, _ByteTag):
                if tag.addr in self.__byte_tags:
                    raise _DisasmError(
                        tag, 'Byte redefined.',
                        _DisasmError(self.__byte_tags[tag.addr],
                                     'Previously defined here.'))
                self.__byte_tags[tag.addr] = tag
            else:
                self.__common_tags.setdefault(tag.addr, []).append(tag)

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
        required_size = tag.addr + 1

        if len(self.__image) < required_size:
            additional_size = required_size - len(self.__image)
            self.__image.extend([None] * additional_size)

        assert self.__image[tag.addr] is None
        self.__image[tag.addr] = tag.value

    def __process_include_binary_tag(self, tag):
        new_tags = []

        comment = 'Included from binary file %r.' % tag.filename.literal
        new_tags.append(_CommentTag(tag.origin, tag.addr, comment))

        if tag.comment is not None:
            new_tags.append(_CommentTag(tag.origin, tag.addr, tag.comment))

        for i, b in enumerate(tag.image):
            new_tags.append(_ByteTag(tag.origin, tag.addr + i, b))

        self.__add_tags(*new_tags)

    def __skip_processing_tag(self, tag):
        pass

    __TAG_PROCESSORS = {
        _ByteTag: __process_byte_tag,
        _CommentTag: __skip_processing_tag,
        _IncludeBinaryTag: __process_include_binary_tag,
        _InstrTag: __skip_processing_tag,
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

    def __write_byte_tag(self, tag, instr, out):
        tag_body = '%#04x' % tag.value

        if instr is not None:
            tag_body += ' instr'

        yield from out.write_line('db %#04x' % tag.value,
                                  tag.addr, tag_body, tag.comment)

    def __write_tags(self, addr, out):
        comments = []
        instr = []
        xbyte = []
        ignored = []

        KINDS = {
            _ByteTag: xbyte,
            _CommentTag: comments,
            _IncludeBinaryTag: ignored,
            _InstrTag: instr,
        }

        # Sort tags out by categories.
        t = self.__byte_tags.get(addr, None)
        if t is not None:
            KINDS[type(t)].append(t)
        for t in self.__common_tags.get(addr, []):
            KINDS[type(t)].append(t)

        if len(xbyte) == 0:
            xbyte.append(None)
        xbyte, = tuple(xbyte)

        if len(instr) == 0:
            instr.append(None)
        instr, = tuple(instr)

        def g():
            # Emit tags in order.
            for t in comments:
                yield from self.__write_comment_tag(t, out)

            if xbyte is not None:
                yield from self.__write_byte_tag(xbyte, instr, out)

        if xbyte is not None:
            addr += 1

        return addr, g()

    def _get_output(self):
        out = _AsmOutput()

        addrs = sorted(set(self.__byte_tags) | set(self.__common_tags))
        if len(addrs) == 0:
            return

        addr = addrs[0]
        for a in addrs:
            if addr < a:
                yield from out.write_space_directive(a - addr)
                addr = a

            assert addr == a
            addr, g = self.__write_tags(a, out)

            yield from g

    def save_output(self, filename):
        with open(filename, 'w') as f:
            for chunk in self._get_output():
                f.write(chunk)
