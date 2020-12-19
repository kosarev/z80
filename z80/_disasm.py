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

    def __skip(self, *fillers):
        while True:
            filler = self.__follows_with(*fillers)
            if filler is None:
                break

            self.__offset += len(filler)

    def skip_whitespace(self):
        self.__skip(' ', '\t')

    def skip_char(self):
        c = self.__get_front(1)
        self.__offset += 1
        return c

    def start_token(self):
        self.__token_offset = self.__offset

    def end_token(self):
        pos = _SourcePos(self.__token_offset, self.__source_file)

        literal = None
        if self.__token_offset < self.__offset:
            literal = self.__image[self.__token_offset:self.__offset]

        return literal, pos


class _ProfileTag(object):
    def __init__(self, kind, addr, comment):
        self._kind = kind
        self._addr = addr
        self._comment = comment

    def get_kind(self):
        return self._kind

    def get_addr(self):
        return self._addr

    def get_comment(self):
        return self._comment

    def __repr__(self):
        comment = self.get_comment()
        comment = '' if not comment else ' : %s' % comment
        return '@@ 0x%04x %s%s' % (self.get_addr(), self.get_kind(), comment)


class _InstrTag(_ProfileTag):
    ID = 'instr'

    def __init__(self, addr, comment):
        super().__init__(self.ID, addr, comment)


class _Token(object):
    def __init__(self, literal, source_pos):
        self.__literal = literal
        self.__source_pos = source_pos

    def __repr__(self):
        return '%r: %r' % (self.__source_pos, self.__literal)

    def get_literal(self):
        return self.__literal

    def get_source_pos(self):
        return self.__source_pos


class _TagParser(object):
    __DELIMITERS = '\n :,()[]'

    def __init__(self, source_file):
        self.__toks = _Tokenizer(source_file)

    def __fetch_token(self, error=None):
        self.__toks.skip_whitespace()

        self.__toks.start_token()
        if self.__toks.skip_char() not in self.__DELIMITERS:
            self.__toks.skip_to(*self.__DELIMITERS)
        literal, pos = self.__toks.end_token()

        if literal == '\n':
            literal = None

        if literal is not None:
            return _Token(literal, pos)

        if error is not None:
            raise _SourceError(pos, error)

        return None

    def __parse_tag_address(self):
        tok = self.__fetch_token('Tag address expected.')
        try:
            return int(tok.get_literal(), base=0)
        except ValueError:
            raise _SourceError(tok, 'Malformed tag address.')

    def __parse_tag_name(self):
        return self.__fetch_token('Tag name expected.')

    def __parse_optional_comment(self):
        tok = self.__fetch_token()
        if tok is None:
            return None

        if tok.get_literal() == ':':
            self.__toks.skip_whitespace()
            self.__toks.start_token()
            self.__toks.skip_rest_of_line()
            literal, pos = self.__toks.end_token()
            return literal

        raise _SourceError(tok, 'End of line or a comment expected.')

    def __parse_instr_tag(self, addr, name):
        comment = self.__parse_optional_comment()
        return _InstrTag(addr, comment)

    __TAG_PARSERS = {
        _InstrTag.ID: __parse_instr_tag,
    }

    # Parses and returns a subsequent tag.
    def __iter__(self):
        while self.__toks.skip_next('@@'):
            addr = self.__parse_tag_address()
            name = self.__parse_tag_name()

            parser = self.__TAG_PARSERS.get(name.get_literal(), None)
            if not parser:
                raise _SourceError(name, 'Unknown tag.')

            yield parser(self, addr, name)


class _Profile(object):
    __tags = dict()

    def load_if_exists(self, filename):
        try:
            file = _SourceFile(filename)
        except FileNotFoundError:
            return

        for tag in _TagParser(file):
            self.__tags.setdefault(tag.get_addr(), []).append(tag)


class _Disasm(object):
    def __init__(self, image):
        self.__image = image

    def disassemble(self, profile):
        self._tags_to_process = []
        assert 0, profile._Profile__tags
