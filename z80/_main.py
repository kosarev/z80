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
import sys

from ._disassembler import _Profile, _Disassembler


def _pop_argument(args, error):
    if not args:
        raise z80._Error(error)

    return args.pop(0)


def _handle_extra_arguments(args):
    if args:
        raise z80._Error('Extra argument %r.' % args[0])


def _disasm(args):
    image_filename = _pop_argument(args, 'The image file to disassemble is '
                                         'not specified.')
    asm_filename = _pop_argument(args, 'The assembly file is not specified.')
    profile_filename = _pop_argument(args, 'The profile is not specified.')
    _handle_extra_arguments(args)

    with open(image_filename, 'rb') as f:
        image = f.read()

    IMAGE_SIZE = 0x10000
    if len(image) != IMAGE_SIZE:
        raise z80._Error('The image file shall be of exactly %d bytes in '
                         'size.' % IMAGE_SIZE)

    profile = _Profile()
    profile.load_if_exists(asm_filename)
    profile.load_if_exists(profile_filename)

    d = _Disassembler(image)
    d.disassemble(profile)
    assert 0, d  # TODO


def _handle_command_line(args):
    if not args:
        raise z80._Error('Nothing to do.')

    command = args.pop(0)
    if command == 'disasm':
        _disasm(args)
        return

    raise z80._Error('Unknown command %r.' % command)


def main():
    try:
        _handle_command_line(sys.argv[1:])
    except z80._Error as e:
        lines = e.args[0].split('\n')
        lines[-1] = 'z80: %s' % lines[-1]
        sys.exit('\n'.join(lines))


if __name__ == "__main__":
    main()
