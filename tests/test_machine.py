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


def test_exit_halted_state() -> None:
    # Clearing the 'halted' field must let a halted machine resume
    # execution (issue #65).
    INC_A = 0x3c
    HALT = 0x76
    m = z80.Z80Machine()
    m.set_memory_block(0x0000, bytes([INC_A, HALT, INC_A]))

    def step() -> None:
        m.ticks_to_stop = 1
        m.run()

    # Execute the first 'inc a', then the 'halt'.
    step()
    assert (m.a, m.halted) == (0x01, False)
    step()
    assert (m.pc, m.a, m.halted) == (0x0002, 0x01, True)

    # While halted, stepping must not make any progress.
    step()
    step()
    assert (m.pc, m.a, m.halted) == (0x0002, 0x01, True)

    # Leaving the halted mode must resume with the second 'inc a'.
    m.halted = False
    step()
    assert (m.pc, m.a, m.halted) == (0x0003, 0x02, False)
