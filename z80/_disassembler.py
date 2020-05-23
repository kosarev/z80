#!/usr/bin/env python3
# -*- coding: utf-8 -*-

#   Z80 CPU Emulator.
#   https://github.com/kosarev/z80
#
#   Copyright (C) 2017-2020 Ivan Kosarev.
#   ivan@kosarev.info
#
#   Published under the MIT license.


import z80


class _InputText(object):
    def __init__(self, filename):
        self._filename = filename

    def __iter__(self):
        self._line_no = 1
        with open(self._filename) as f:
            for line in f:
                line = line.rstrip()
                self._line = line
                yield line
                self._line_no += 1

    def error(self, msg):
        raise z80._Error('%s\n%s:%d: %s' % (
            self._line, self._filename, self._line_no, msg))


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


class _TagParser(object):
    def __init__(self, input, line):
        self._input = input
        self._line = line

    def _error(self, msg):
        self._input.error(msg)

    def _tokenize(self):
        parts = self._line.split(maxsplit=1)
        tok = parts[0] if parts else None
        rest = parts[1] if len(parts) == 2 else ''
        return tok, rest

    def _get_token(self, expected=None):
        tok, rest = self._tokenize()
        if expected is not None and tok != expected:
            return None

        self._line = rest
        return tok

    def _get_rest(self):
        line = self._line
        self._line = ''
        return line

    def _parse_optional_comment(self):
        tok = self._get_token()
        if not tok:
            return None

        if tok != ':':
            self._error('%r: End of line or a comment expected.' % tok)

        return self._get_rest()

    def _parse_instr_tag(self, addr, name):
        comment = self._parse_optional_comment()
        return _InstrTag(addr, comment)

    _TAG_PARSERS = {
        _InstrTag.ID: _parse_instr_tag,
    }

    def parse(self):
        # Look up for the tag.
        tag_pos = self._line.find('@@')
        if tag_pos < 0:
            return

        self._line = self._line[tag_pos + 2:]

        # Parse the address of the tag.
        addr = self._get_token()
        if not addr:
            self._error('Profile tag address expected.')

        try:
            addr = int(addr, base=0)
        except ValueError:
            self._error('%r: Malformed profile tag address.' % addr)

        # Parse the name of the tag.
        name = self._get_token()
        if not name:
            self._error('Profile tag name expected.')

        # Call tag-specific parser.
        parser = self._TAG_PARSERS.get(name, None)
        if not parser:
            self._error('%r: Unknown tag.' % name)

        return parser(self, addr, name)


class _Profile(object):
    _tags = dict()

    def _parse_line(self, tag_parser):
        tag = tag_parser.parse()
        if tag:
            self._tags.setdefault(tag.get_addr(), []).append(tag)

    def load_if_exists(self, filename):
        input = _InputText(filename)
        try:
            for line in input:
                tag_parser = _TagParser(input, line)
                self._parse_line(tag_parser)
        except FileNotFoundError:
            pass


class _Disassembler(object):
    def __init__(self, image):
        self._image = image

    def disassemble(self, profile):
        self._tags_to_process = []
        assert 0
