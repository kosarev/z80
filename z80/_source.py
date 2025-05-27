#!/usr/bin/env python3
# -*- coding: utf-8 -*-

#   Z80 CPU Emulator.
#   https://github.com/kosarev/z80
#
#   Copyright (C) 2017-2025 Ivan Kosarev.
#   mail@ivankosarev.com
#
#   Published under the MIT license.

import bisect


class _SourcePos(object):
    def __init__(self, offset: int, file: '_SourceFile') -> None:
        self.__offset = offset
        self.__file = file

    @property
    def __coordinates(self) -> tuple[str, int, int]:
        return self.__file.get_coordinates(self.__offset)

    @property
    def column_no(self) -> int:
        line, line_no, column_no = self.__coordinates
        return column_no

    @property
    def inline_text(self) -> str:
        line, line_no, column_no = self.__coordinates
        return '%s:%s:%s' % (self.__file, line_no, column_no)

    def __str__(self) -> str:
        return self.inline_text

    @property
    def context_text(self) -> str:
        line, line_no, column_no = self.__coordinates
        return '%s\n%s^\n' % (line, ' ' * column_no)


class _SourceFile(object):
    def __init__(self, filename: str, image: str | None = None) -> None:
        self.__filename = filename

        if image is None:
            with open(self.__filename) as f:
                image = f.read()

        self.__image = image

        self.__line_breaks = tuple(
            i for i, c in enumerate(self.__image) if c == '\n')

    def __repr__(self) -> str:
        return self.__filename

    def get_image(self) -> str:
        return self.__image

    def get_coordinates(self, offset: int) -> tuple[str, int, int]:
        i = bisect.bisect_left(self.__line_breaks, offset)

        line_start = self.__line_breaks[i - 1] + 1 if i > 0 else 0
        line_end = (self.__line_breaks[i] if i < len(self.__line_breaks)
                    else len(self.__image))
        line = self.__image[line_start:line_end]

        line_no = i + 1
        column_no = offset - line_start

        return line, line_no, column_no
