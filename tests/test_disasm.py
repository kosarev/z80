# -*- coding: utf-8 -*-

#   Z80 CPU Emulator.
#   https://github.com/kosarev/z80
#
#   Copyright (C) 2017-2026 Ivan Kosarev.
#   mail@ivankosarev.com
#
#   Published under the MIT license.

import os
import pytest
import z80


_DISASM_DIR = os.path.join(os.path.dirname(__file__), 'disasm')
_CASES = sorted(f for f in os.listdir(_DISASM_DIR) if f.endswith('.asm'))


@pytest.mark.parametrize('name', _CASES)
def test_disasm(name: str) -> None:
    path = os.path.join(_DISASM_DIR, name)
    short_path = os.path.join('disasm', name)

    with open(path) as f:
        text = f.read()

    expected_output = ''
    expected_error = ''

    split = tuple(text.split('===', maxsplit=1))
    if len(split) == 2:
        input, expected_error = split
    else:
        split = tuple(text.split('---', maxsplit=1))
        if len(split) == 2:
            input, expected_output = split
        else:
            input, = split
            expected_output = '\n' + input

    # Tags such as 'include_binary' refer to files relative to the
    # source, so run with the disassembly directory as the working one.
    old_wd = os.getcwd()
    os.chdir(_DISASM_DIR)
    try:
        d = z80._Disasm()
        error = ''
        try:
            source = z80._SourceFile(short_path, input)
            d.add_tags(*z80._DisasmTagParser(source).parse())
            d.disassemble()

            output = ''.join(d._get_output())
            assert output == expected_output
        except z80.Error as e:
            error = '\n%s\n' % e.verbalize()

        assert error == expected_error
    finally:
        os.chdir(old_wd)
