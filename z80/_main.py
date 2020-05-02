#!/usr/bin/env python3
# -*- coding: utf-8 -*-

#   Z80 CPU Emulator.
#   https://github.com/kosarev/z80
#
#   Copyright (C) 2017-2020 Ivan Kosarev.
#   ivan@kosarev.info
#
#   Published under the MIT license.

import sys


class _Error(Exception):
    def __init__(self, reason):
        super().__init__(reason)


def _disasm(args):
    # TODO
    assert 0, args


def _handle_command_line(args):
    if not args:
        raise _Error('Nothing to do.')

    command = args.pop(0)
    if command == 'disasm':
        _disasm(args)
        return

    raise _Error('Unknown command %r.' % command)


def main():
    try:
        _handle_command_line(sys.argv[1:])
    except _Error as e:
        print('z80: %s' % e.args)


if __name__ == "__main__":
    main()
