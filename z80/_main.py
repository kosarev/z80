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

from ._disassembler import _Disassembler


class _Error(Exception):
    def __init__(self, reason):
        super().__init__(reason)


def _pop_argument(args, error):
    if not args:
        raise _Error(error)

    return args.pop(0)


def _handle_extra_arguments(args):
    if args:
        raise _Error('Extra argument %r.' % args[0])


def _disasm(args):
    file_to_disasm = _pop_argument(args, 'The file to disassemble not '
                                         'specified.')
    _handle_extra_arguments(args)

    with open(file_to_disasm, 'rb') as f:
        d = _Disassembler(f.read())
        assert 0, d  # TODO


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
