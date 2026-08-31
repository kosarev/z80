#   Z80 CPU Emulator.
#   https://github.com/kosarev/z80
#
#   Copyright (C) 2017-2026 Ivan Kosarev.
#   mail@ivankosarev.com
#
#   Published under the MIT license.

import os
import sys

from ._asm import Asm
from ._disasm import _Disasm
from ._disasm_parser import _DisasmTagParser
from ._error import Error
from ._source import SourceFile


def _pop_argument(args: list[str], error: str) -> str:
    if not args:
        raise Error(error)

    return args.pop(0)


def _handle_extra_arguments(args: list[str]) -> None:
    if args:
        raise Error(f'Extra argument {args[0]!r}.')


def _asm(args: list[str]) -> None:
    filename = _pop_argument(args, 'The source file is not specified.')
    output = args.pop(0) if args else None
    _handle_extra_arguments(args)

    code = Asm().assemble(SourceFile(filename))
    code.resolve()
    blocks = code.encode()

    if output is None:
        root, _ = os.path.splitext(filename)
        output = root + '.bin'

    # Flatten the blocks into one contiguous image, in their
    # intake order.
    image = bytearray()
    if blocks:
        base = min(addr for addr, _ in blocks)
        top = max(addr + len(data) for addr, data in blocks)
        image = bytearray(top - base)
        for addr, data in blocks:
            image[addr - base:addr - base + len(data)] = data

    with open(output, 'wb') as f:
        f.write(image)


def _disasm(args: list[str]) -> None:
    filename = _pop_argument(args, 'The assembly file is not specified.')
    _handle_extra_arguments(args)

    d = _Disasm()
    d.add_tags(*_DisasmTagParser(SourceFile(filename)).parse())
    d.disassemble()
    d.save_output(filename)


def _handle_command_line(args: list[str]) -> None:
    if not args:
        raise Error('Nothing to do.')

    command = args.pop(0)
    if command == 'asm':
        _asm(args)
        return

    if command == 'disasm':
        _disasm(args)
        return

    raise Error(f'Unknown command {command!r}.')


def main() -> None:
    try:
        _handle_command_line(sys.argv[1:])
    except Error as e:
        sys.exit(e.verbalize(program_name='z80'))


if __name__ == "__main__":
    main()
