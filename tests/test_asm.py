
#   Z80 CPU Emulator.
#   https://github.com/kosarev/z80
#
#   Copyright (C) 2026 Ivan Kosarev.
#   mail@ivankosarev.com
#
#   Published under the MIT license.

import os

import pytest

import z80

_ASM_DIR = os.path.join(os.path.dirname(__file__), 'asm')
_CASES = sorted(f for f in os.listdir(_ASM_DIR) if f.endswith('.asm'))


@pytest.mark.parametrize('name', _CASES)
def test_asm(name: str) -> None:
    path = os.path.join(_ASM_DIR, name)

    with open(path) as f:
        text = f.read()

    expected_output = ''
    expected_error = ''

    split = tuple(text.split('===', maxsplit=1))
    if len(split) == 2:
        input, expected_error = split
    else:
        input, expected_output = tuple(text.split('---', maxsplit=1))

    error = ''
    try:
        short_path = os.path.join('asm', name)
        source = z80.SourceFile(short_path, input)
        code = z80.Asm().assemble(source)
        code.resolve()

        output = '\n'
        for addr, image in code.encode():
            output += f'{addr:#06x}: {image.hex(" ")}\n'

        assert output == expected_output
    except z80.Error as e:
        error = f'\n{e.verbalize()}\n'

    assert error == expected_error
