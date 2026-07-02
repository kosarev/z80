# -*- coding: utf-8 -*-

#   Z80 CPU Emulator.
#   https://github.com/kosarev/z80
#
#   Copyright (C) 2017-2026 Ivan Kosarev.
#   mail@ivankosarev.com
#
#   Published under the MIT license.

import pytest
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


# On Z80, output callbacks get the full 16-bit port address, with
# the value of A in the high byte.
@pytest.mark.parametrize('machine_type, output_addr', [
    (z80.Z80Machine, 0x42fe),
    (z80.I8080Machine, 0xfe),
])
def test_write_and_output_callbacks(
        machine_type: type[z80.I8080Machine | z80.Z80Machine],
        output_addr: int) -> None:
    # Write and output callbacks take an (addr, value) pair of
    # arguments (issue #62).
    m = machine_type()
    writes: list[tuple[int, int]] = []
    outputs: list[tuple[int, int]] = []

    m.set_write_callback(lambda addr, value: writes.append((addr, value)))
    m.set_output_callback(lambda addr, value: outputs.append((addr, value)))

    # ld a, 0x42; ld (0x8000), a; out (0xfe), a
    # (Same encoding for the i8080 counterparts.)
    code = bytes([0x3e, 0x42, 0x32, 0x00, 0x80, 0xd3, 0xfe])
    m.set_memory_block(0x0000, code)

    # Enough ticks to execute the three instructions; the trailing
    # nops are harmless.
    m.ticks_to_stop = 40
    m.run()

    assert writes == [(0x8000, 0x42)]
    assert outputs == [(output_addr, 0x42)]
