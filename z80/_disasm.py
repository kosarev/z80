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
    def __init__(self, filename):
        self.__filename = filename

        with open(self.__filename) as f:
            self.__image = f.read()

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
        return f'{self.pos}: {self.literal}'

    def __str__(self):
        return self.literal


class _Tokenizer(object):
    def __init__(self, source_file):
        self.__source_file = source_file
        self.__image = self.__source_file.get_image()
        self.__offset = 0
        self.__token_offset = self.__offset

    def __get_end_offset(self):
        return len(self.__image)

    def __find_all(self, *anchors):
        # Remember the offset to not depend on its changes.
        start_offset = self.__offset
        for a in anchors:
            i = self.__image.find(a, start_offset)
            if i >= 0:
                yield a, i

    def __find_next(self, *anchors):
        anchor, offset = None, None
        for a, i in self.__find_all(*anchors):
            if offset is None or offset > i:
                anchor, offset = a, i
        return anchor, offset

    def skip_to(self, *anchors):
        anchor, offset = self.__find_next(*anchors)
        if offset is None:
            self.__offset = self.__get_end_offset()
        else:
            self.__offset = offset
        return anchor

    def skip_rest_of_line(self):
        self.skip_to('\n')

    def skip_next(self, *anchors):
        anchor = self.skip_to(*anchors)
        if anchor is not None:
            self.__offset += len(anchor)
        return anchor

    def __get_front(self, size=None):
        end = self.__offset + size if size is not None else None
        return self.__image[self.__offset:end]

    def __follows_with(self, *fillers):
        for filler in fillers:
            if self.__get_front().startswith(filler):
                return filler
        return None

    def skip(self, *fillers):
        follower = self.__follows_with(*fillers)
        if follower is not None:
            self.__offset += len(follower)

        return follower

    def skip_all(self, *fillers):
        while self.skip(*fillers):
            pass

    def skip_whitespace(self):
        self.skip_all(' ', '\t')

    def skip_char(self):
        c = self.__get_front(1)
        self.__offset += 1
        return c

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
        return f'({self.addr:#06x}, {self.ID}, {self.comment!r})'


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
        return (f'({self.addr:#06x}, {self.ID}, '
                f'{self.value:#04x}, {self.comment!r})')


class _IncludeBinaryTag(_Tag):
    ID = 'include_binary'

    def __init__(self, addr, filename, image):
        super().__init__(addr, size=len(image))
        self.filename = filename
        self.image = image

    def __repr__(self):
        return (f'({self.addr:#06x}, {self.ID}, '
                f'{self.filename}, {self.comment})')


class _InstrTag(_Tag):
    ID = 'instr'


class _TagParser(object):
    __DELIMITERS = '\n :,()[]'

    def __init__(self, source_file):
        self.__toks = _Tokenizer(source_file)

    def __fetch_token(self, error=None):
        self.__toks.skip_whitespace()

        self.__toks.start_token()
        if self.__toks.skip_char() not in self.__DELIMITERS:
            self.__toks.skip_to(*self.__DELIMITERS)
        tok = self.__toks.end_token()

        if tok.literal == '\n':
            tok.literal = None

        if tok.literal is not None:
            return tok

        if error is not None:
            raise _SourceError(tok.pos, error)

        return None

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

    def __parse_tag_name(self):
        return self.__fetch_token('Tag name expected.')

    def __parse_optional_comment(self):
        tok = self.__fetch_token()
        if tok is None:
            return None

        if tok != ':':
            raise _SourceError(tok, 'End of line or a comment expected.')

        self.__toks.skip_whitespace()
        self.__toks.start_token()
        self.__toks.skip_rest_of_line()
        return self.__toks.end_token()

    def __parse_string(self):
        toks = self.__toks
        toks.skip_whitespace()

        toks.start_token()
        quote = toks.skip('\'', '"')
        if quote is None:
            raise _SourceError(toks.pos, 'A quoted string expected.')

        toks.skip_to(quote, '\n')

        if toks.skip(quote) is None:
            raise _SourceError(toks.pos,
                               f'Missed closing quote {repr(quote)}.')

        tok = toks.end_token()
        tok.value = tok.literal[1:-1]

        return tok

    def __parse_include_binary_tag(self, addr, name):
        filename = self.__parse_string()

        with open(filename.value, 'rb') as f:
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
        while self.__toks.skip_next('@@'):
            addr = self.__parse_optional_tag_address()
            name = self.__parse_tag_name()

            parser = self.__TAG_PARSERS.get(name.literal, None)
            if not parser:
                raise _SourceError(name, 'Unknown tag.')

            tag = parser(self, addr, name)
            tag.comment = self.__parse_optional_comment()

            yield tag


class _AsmOutput(object):
    __COMMAND_INDENT = 4
    __COMMENT_INDENT = 40

    def __init__(self, stream):
        self.__stream = stream
        self.__needs_empty_line = True

    def __write_empty_line_if_needed(self):
        if self.__needs_empty_line:
            self.__stream.write('\n')
            self.__needs_empty_line = False

    def write_line(self, line, tag=None):
        self.__write_empty_line_if_needed()

        line = ' ' * self.__COMMAND_INDENT + line

        comment = None
        if tag is not None:
            comment = '@@ %#06x %s' % tag

        if comment is not None:
            line = line.ljust(self.__COMMENT_INDENT) + '; %s' % comment

        line += '\n'

        self.__stream.write(line)

    def write_space_directive(self, size):
        self.write_line('.space %d' % size)


class _Disasm(object):
    def __init__(self):
        self.__tags = dict()

        # Use deque because of its popleft() is much faster than
        # list's pop(0).
        self.__worklist = collections.deque()

    def __new_tag(self, tag):
        self.__tags.setdefault(tag.addr, []).append(tag)
        self.__worklist.append(tag)

    def read_tags(self, filename):
        addr = 0
        for tag in _TagParser(_SourceFile(filename)):
            if tag.addr is None:
                tag.addr = addr
            else:
                addr = tag.addr

            self.__new_tag(tag)

            addr += tag.size

    def __process_byte_tag(self, tag):
        pass

    def __process_include_binary_tag(self, tag):
        comment = 'Included from binary file %r.' % tag.filename.value
        self.__new_tag(_CommentTag(tag.addr, comment))

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
        out.write_line('; @@: %s' % tag.comment)

    def __write_byte_tag(self, tag, out):
        out.write_line('db %#04x' % tag.value,
                       tag=(tag.addr, '%#04x' % tag.value))

    __TAG_WRITERS = {
        _CommentTag: __write_comment_tag,
        _ByteTag: __write_byte_tag,
        _IncludeBinaryTag: lambda self, tag, out: None,
    }

    def __write_tag(self, tag, out):
        write = self.__TAG_WRITERS[type(tag)]
        write(self, tag, out)

    def save(self, filename):
        with open(filename, 'w') as f:
            out = _AsmOutput(f)

            addr = 0
            for a in sorted(self.__tags):
                for tag in self.__tags[a]:
                    if addr < tag.addr:
                        out.write_space_directive(tag.addr - addr)
                        addr = tag.addr

                    self.__write_tag(tag, out)
                    addr += tag.size
