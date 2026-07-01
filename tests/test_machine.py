# -*- coding: utf-8 -*-

#   Z80 CPU Emulator.
#   https://github.com/kosarev/z80
#
#   Copyright (C) 2017-2026 Ivan Kosarev.
#   mail@ivankosarev.com
#
#   Published under the MIT license.

import z80


def test_ticks_to_stop_round_trip() -> None:
    # A full 32-bit value must round-trip through the field.
    m = z80.Z80Machine()
    m.ticks_to_stop = 0x12345678
    assert m.ticks_to_stop == 0x12345678
