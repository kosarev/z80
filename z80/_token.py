#!/usr/bin/env python3
# -*- coding: utf-8 -*-

#   Z80 CPU Emulator.
#   https://github.com/kosarev/z80
#
#   Copyright (C) 2017-2021 Ivan Kosarev.
#   ivan@kosarev.info
#
#   Published under the MIT license.

import re
from ._source import _SourcePos


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


class _Tokeniser(object):
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

        return _Token(literal, pos)
