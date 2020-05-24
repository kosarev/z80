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


class _Literal(object):
    def __init__(self, text, source_pos):
        self.__text = text
        self.__source_pos = source_pos

    def __repr__(self):
        return '%r: %r' % (self.__source_pos, self.__text)

    def get_text(self):
        return self.__text

    def get_source_pos(self):
        return self.__source_pos


class _SourceError(Error):
    def __init__(self, subject, message):
        super().__init__('%r: %s' % (subject, message))


class _SourceParser(object):
    def __init__(self, source_file):
        self.__source_file = source_file
        self.__image = source_file.get_image()
        self.__offset = 0

    def get_current_pos(self):
        return _SourcePos(self.__offset, self.__source_file)

    def __get_front(self):
        return self.__image[self.__offset:]

    def __peek_char(self):
        return self.__get_front()[:1]

    def __follows_with(self, s):
        return self.__get_front().startswith(s)

    def __skip(self, n):
        if n == 0:
            return None

        lit = _Literal(self.__get_front()[:n], self.get_current_pos())
        self.__offset += n
        return lit

    def __skip_to_offset(self, offset):
        assert self.__offset <= offset
        return self.__skip(offset - self.__offset)

    def __skip_to_end(self):
        return self.__skip_to_offset(len(self.__image))

    def skip_to(self, delims):
        pos = len(self.__image)
        for delim in delims:
            i = self.__image.find(delim, self.__offset)
            if i >= 0 and pos > i:
                pos = i

        if pos is None:
            return self.__skip_to_end()

        return self.__skip_to_offset(pos)

    def __skip_while(self, allowed):
        end = self.__offset
        while self.__image[end:].startswith(allowed):
            end += len(allowed)
        return self.__skip_to_offset(end)

    def skip_spaces(self):
        return self.__skip_while(' ')

    def eat(self, s):
        if not self.__follows_with(s):
            return None

        return self.__skip(len(s))

    def skip_to_and_eat(self, s):
        self.skip_to((s,))
        return self.eat(s)

    def skip_rest_of_line(self):
        return self.skip_to('\n')


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


class _TagParser(_SourceParser):
    _DELIMITERS = ' :'

    def __init__(self, source_file):
        super().__init__(source_file)

    def __parse_token(self):
        return self.skip_to(self._DELIMITERS)

    def __parse_address(self):
        tok = self.__parse_token()
        if not tok:
            raise _SourceError(self.get_current_pos(),
                               'Tag address expected.')

        try:
            return int(tok.get_text(), base=0)
        except ValueError:
            raise _SourceError(tok, 'Malformed tag address.')

    def __parse_name(self):
        tok = self.__parse_token()
        if not tok:
            raise _SourceError(self.get_current_pos(),
                               'Tag name expected.')
        return tok

    def __parse_optional_comment(self):
        if self.eat(':'):
            self.skip_spaces()
            return self.skip_rest_of_line().get_text()

        tok = self.skip_rest_of_line()
        if not tok:
            return None

        raise _SourceError(tok, 'End of line or a comment expected.')

    def __parse_instr_tag(self, addr, name):
        comment = self.__parse_optional_comment()
        return _InstrTag(addr, comment)

    _TAG_PARSERS = {
        _InstrTag.ID: __parse_instr_tag,
    }

    # Parses and returns a subsequent tag.
    def __iter__(self):
        if self.skip_to_and_eat('@@'):
            self.skip_spaces()
            addr = self.__parse_address()

            self.skip_spaces()
            name = self.__parse_name()

            self.skip_spaces()
            parser = self._TAG_PARSERS.get(name.get_text(), None)
            if not parser:
                raise _SourceError(name, 'Unknown tag.')

            yield parser(self, addr, name)


class _Profile(object):
    __tags = dict()

    def load_if_exists(self, filename):
        try:
            parser = _TagParser(_SourceFile(filename))
            for tag in parser:
                if tag:
                    self.__tags.setdefault(tag.get_addr(), []).append(tag)
        except FileNotFoundError:
            pass


class _Disassembler(object):
    def __init__(self, image):
        self.__image = image

    def disassemble(self, profile):
        self._tags_to_process = []
        assert 0, profile._Profile__tags
