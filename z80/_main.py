#!/usr/bin/env python3
# -*- coding: utf-8 -*-

#   Z80 CPU Emulator.
#   https://github.com/kosarev/z80
#
#   Copyright (C) 2017-2025 Ivan Kosarev.
#   mail@ivankosarev.com
#
#   Published under the MIT license.

import sys
from ._disasm import _Disasm
from ._disasm_parser import _DisasmTagParser
from ._error import Error
from ._source import _SourceFile


def _pop_argument(args: list[str], error: str) -> str:
    if not args:
        raise Error(error)

    return args.pop(0)


def _handle_extra_arguments(args: list[str]) -> None:
    if args:
        raise Error('Extra argument %r.' % args[0])


def _disasm(args: list[str]) -> None:
    filename = _pop_argument(args, 'The assembly file is not specified.')
    _handle_extra_arguments(args)

    d = _Disasm()
    d.add_tags(*_DisasmTagParser(_SourceFile(filename)).parse())
    d.disassemble()
    d.save_output(filename)


def _handle_command_line(args: list[str]) -> None:
    if not args:
        raise Error('Nothing to do.')

    command = args.pop(0)
    if command == 'disasm':
        _disasm(args)
        return

    raise Error('Unknown command %r.' % command)


def main() -> None:
    try:
        _handle_command_line(sys.argv[1:])
    except Error as e:
        sys.exit(e.verbalize(program_name='z80'))


if __name__ == "__main__":
    main()
