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
    def __init__(self, offset, source_file):
        self.__offset = offset
        self.__source_file = source_file

    def __repr__(self):
        file = self.__source_file
        line, line_no, column_no = file.get_coordinates(self.__offset)
        return '%s\n%s^\n%s:%s:%s' % (line, ' ' * column_no, file,
                                      line_no, column_no)


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


class _SourceError(Error):
    def __init__(self, subject, message):
        super().__init__('%r: %s' % (subject, message))


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


class _Tokenizer(object):
    __WHITESPACE = re.compile(r'(\ |\t)+')
    __END_OF_LINE = re.compile(r'(\n|$)')

    def __init__(self, source_file):
        self.__source_file = source_file
        self.__image = self.__source_file.get_image()
        self.__offset = 0
        self.__token_offset = self.__offset

    def skip(self, pat):
        match = pat.match(self.__image, self.__offset)
        if match is None:
            return None

        self.__offset = match.end()
        return match.group(0)

    def skip_whitespace(self):
        self.skip(self.__WHITESPACE)

    def skip_to(self, pat):
        match = pat.search(self.__image, self.__offset)
        if match is not None:
            self.__offset = match.start()

    def skip_rest_of_line(self):
        self.skip_to(self.__END_OF_LINE)

    def skip_next(self, pat):
        match = pat.search(self.__image, self.__offset)
        if match is None:
            return None

        self.__offset = match.end()
        return match.group(0)

    def start_token(self):
        self.__token_offset = self.__offset

    @property
    def pos(self):
        return _SourcePos(self.__offset, self.__source_file)

    def end_token(self):
        pos = _SourcePos(self.__token_offset, self.__source_file)

        literal = None
        if self.__token_offset < self.__offset:
            literal = self.__image[self.__token_offset:self.__offset]

        return _Token(literal, pos)

    class _Lookahead(object):
        def __init__(self, tokenizer):
            self.__tokenizer = tokenizer
            self.__offset = tokenizer._Tokenizer__offset
            self.__consumed = False

        def consume(self):
            self.__consumed = True

        def __enter__(self):
            return self

        def __exit__(self, exc_type, exc_value, traceback):
            if not self.__consumed:
                self.__tokenizer._Tokenizer__offset = self.__offset

    def lookahead(self):
        return _Tokenizer._Lookahead(self)


class _Tag(object):
    def __init__(self, addr, size):
        self.addr = addr
        self.size = size
        self.comment = None

    def __repr__(self):
        return '(%#06x, %s, %r)' % (self.addr, self.ID, self.comment)


class _CommentTag(_Tag):
    ID = 'comment'

    def __init__(self, addr, comment):
        super().__init__(addr, size=0)
        self.comment = comment


class _ByteTag(_Tag):
    ID = 'byte'

    def __init__(self, addr, value):
        super().__init__(addr, size=1)
        self.value = value

    def __repr__(self):
        return '(%#06x, %s, %#04x, %r)' % (
            self.addr, self.ID, self.value, self.comment)


class _IncludeBinaryTag(_Tag):
    ID = 'include_binary'

    def __init__(self, addr, filename, image):
        super().__init__(addr, size=len(image))
        self.filename = filename
        self.image = image

    def __repr__(self):
        return '(%#06x, %s, %s, %s)' % (
            self.addr, self.ID, self.filename, self.comment)


class _InstrTag(_Tag):
    ID = 'instr'


class _TagParser(object):
    __TAG_LEADER = re.compile('@@')

    __TOKENS = re.compile(
        r'([_a-z][_0-9a-z]*)|'      # An identifier.
        r'([0-9][0-9a-z]*)|'        # A number.
        r"'(\\'|\\\\|[^'\\\n])*'|"  # A string.
        r'.')                       # Or any other single character.

    def __init__(self, source_file):
        self.__toks = _Tokenizer(source_file)

    def __fetch_token(self, error=None):
        self.__toks.skip_whitespace()

        self.__toks.start_token()
        self.__toks.skip(self.__TOKENS)
        tok = self.__toks.end_token()

        if tok.literal == '\n':
            tok.literal = None

        if tok.literal is None:
            if error is not None:
                raise _SourceError(tok.pos, error)

            return None

        # Translate escape sequences.
        if tok.literal.startswith("'"):
            if tok.literal == "'":
                raise _SourceError(tok.pos, 'Unterminated string.')

            tok.literal = (tok.literal[1:-1].
                           replace('\\\\', '\\').
                           replace("\\'", "'"))

        return tok

    def __evaluate_numeric_literal(self, literal):
        try:
            return int(literal, base=0)
        except ValueError:
            return None

    def __parse_optional_tag_address(self):
        with self.__toks.lookahead() as la:
            tok = self.__fetch_token()
            if tok is None:
                return None

            n = self.__evaluate_numeric_literal(tok.literal)
            if n is None:
                return None

            la.consume()
            return n

    def __parse_comment_body(self):
        self.__toks.skip_whitespace()
        self.__toks.start_token()
        self.__toks.skip_rest_of_line()
        return self.__toks.end_token()

    def __parse_optional_comment(self):
        tok = self.__fetch_token()
        if tok is None:
            return None

        if tok != ':':
            raise _SourceError(tok, 'End of line or a comment expected.')

        return self.__parse_comment_body()

    def __parse_include_binary_tag(self, addr, name):
        filename = self.__fetch_token('A filename expected.')

        with open(filename.literal, 'rb') as f:
            image = f.read()

        return _IncludeBinaryTag(addr, filename, image)

    def __parse_instr_tag(self, addr, name):
        return _InstrTag(addr)

    __TAG_PARSERS = {
        _IncludeBinaryTag.ID: __parse_include_binary_tag,
        _InstrTag.ID: __parse_instr_tag,
    }

    # Parses and returns a subsequent tag.
    def __iter__(self):
        while self.__toks.skip_next(self.__TAG_LEADER):
            addr = self.__parse_optional_tag_address()
            name = self.__fetch_token('Tag name or comment expected.')

            # Handle comment tags.
            if name == ':':
                yield _CommentTag(addr, self.__parse_comment_body())
                continue

            # Handle other tags.
            parser = self.__TAG_PARSERS.get(name.literal, None)
            if not parser:
                raise _SourceError(name, 'Unknown tag.')

            tag = parser(self, addr, name)
            tag.comment = self.__parse_optional_comment()

            yield tag


class _AsmOutput(object):
    __COMMAND_INDENT = 4
    __COMMENT_INDENT = 40

    def __init__(self):
        self.__needs_empty_line = True

    def __write_empty_line_if_needed(self):
        if self.__needs_empty_line:
            yield '\n'
            self.__needs_empty_line = False

    def write_line(self, line, tag=None):
        yield from self.__write_empty_line_if_needed()

        line = ' ' * self.__COMMAND_INDENT + line

        comment = None
        if tag is not None:
            comment = '@@ %#06x %s' % tag

        if comment is not None:
            line = line.ljust(self.__COMMENT_INDENT) + '; %s' % comment

        line += '\n'

        yield line

    def write_space_directive(self, size):
        yield from self.write_line('.space %d' % size)


class _Disasm(object):
    def __init__(self):
        self.__tags = dict()

        # Use deque because of its popleft() is much faster than
        # list's pop(0).
        self.__worklist = collections.deque()

    def __new_tag(self, tag):
        self.__tags.setdefault(tag.addr, []).append(tag)
        self.__worklist.append(tag)

    def parse_tags(self, filename, image=None):
        addr = 0
        for tag in _TagParser(_SourceFile(filename, image)):
            if tag.addr is None:
                tag.addr = addr
            else:
                addr = tag.addr

            self.__new_tag(tag)

            addr += tag.size

    def __process_byte_tag(self, tag):
        pass

    def __process_include_binary_tag(self, tag):
        comment = 'Included from binary file %r.' % tag.filename.literal
        self.__new_tag(_CommentTag(tag.addr, comment))
        self.__new_tag(_CommentTag(tag.addr, tag.comment))

        for i, b in enumerate(tag.image):
            self.__new_tag(_ByteTag(tag.addr + i, b))

    __TAG_PROCESSORS = {
        _CommentTag: lambda self, tag: None,
        _ByteTag: __process_byte_tag,
        _IncludeBinaryTag: __process_include_binary_tag,
    }

    def __process_tag(self, tag):
        assert tag.addr is not None
        process = self.__TAG_PROCESSORS[type(tag)]
        process(self, tag)

    def disassemble(self):
        while self.__worklist:
            self.__process_tag(self.__worklist.popleft())

    def __write_comment_tag(self, tag, out):
        yield from out.write_line('; @@ {:#06x} : {}'.format(tag.addr, tag.comment))

    def __write_byte_tag(self, tag, out):
        yield from out.write_line('db %#04x' % tag.value,
                                  tag=(tag.addr, '%#04x' % tag.value))

    __TAG_WRITERS = {
        _CommentTag: __write_comment_tag,
        _ByteTag: __write_byte_tag,
        _IncludeBinaryTag: lambda self, tag, out: iter(()),
    }

    def __write_tag(self, tag, out):
        write = self.__TAG_WRITERS[type(tag)]
        yield from write(self, tag, out)

    def _get_output(self):
        out = _AsmOutput()

        addr = 0
        for a in sorted(self.__tags):
            for tag in self.__tags[a]:
                if addr < tag.addr:
                    yield from out.write_space_directive(tag.addr - addr)
                    addr = tag.addr

                yield from self.__write_tag(tag, out)
                addr += tag.size

    def save_output(self, filename):
        with open(filename, 'w') as f:
            for chunk in self._get_output():
                f.write(chunk)
