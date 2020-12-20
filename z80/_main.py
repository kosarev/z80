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
from ._disasm import _Disasm
from ._error import Error


def _pop_argument(args, error):
    if not args:
        raise Error(error)

    return args.pop(0)


def _handle_extra_arguments(args):
    if args:
        raise Error('Extra argument %r.' % args[0])


def _disasm(args):
    filename = _pop_argument(args, 'The assembly file is not specified.')
    _handle_extra_arguments(args)

    d = _Disasm()
    d.read_tags(filename)
    d.disassemble()
    d.save(filename + '.disasm')


def _handle_command_line(args):
    if not args:
        raise Error('Nothing to do.')

    command = args.pop(0)
    if command == 'disasm':
        _disasm(args)
        return

    raise Error('Unknown command %r.' % command)


def main():
    try:
        _handle_command_line(sys.argv[1:])
    except Error as e:
        lines = e.args[0].split('\n')
        lines[-1] = 'z80: %s' % lines[-1]
        sys.exit('\n'.join(lines))


if __name__ == "__main__":
    main()
