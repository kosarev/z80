#!/usr/bin/env python3
# -*- coding: utf-8 -*-

#   Z80 CPU Emulator.
#   https://github.com/kosarev/z80
#
#   Copyright (C) 2017-2020 Ivan Kosarev.
#   ivan@kosarev.info
#
#   Published under the MIT license.


class _Profile:
    def load(self, filename):
        assert 0  # TODO


class _Disassembler(object):
    def __init__(self, image):
        self._image = image
