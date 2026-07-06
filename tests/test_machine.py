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


def test_registers_round_trip() -> None:
    # All registers must be accessible as public properties,
    # including 'alt_af' and 'ir' (issue #68).
    m = z80.Z80Machine()

    REGPS = ('bc', 'de', 'hl', 'af', 'pc', 'sp', 'ix', 'iy',
             'alt_bc', 'alt_de', 'alt_hl', 'alt_af', 'ir')
    values = {rp: ((2 * i + 1) << 8) | (2 * i + 2)
              for i, rp in enumerate(REGPS)}
    for rp, value in values.items():
        setattr(m, rp, value)
    for rp, value in values.items():
        assert getattr(m, rp) == value, rp

    # All register pairs except 'pc' and 'sp' have their 8-bit
    # halves accessible as registers on their own.
    for rp, value in values.items():
        if rp in ('pc', 'sp'):
            continue
        if rp in ('ix', 'iy'):
            high, low = rp + 'h', rp + 'l'
        else:
            prefix, pair = rp[:-2], rp[-2:]
            high, low = prefix + pair[0], prefix + pair[1]
        assert getattr(m, high) == value >> 8, high
        assert getattr(m, low) == value & 0xff, low

        setattr(m, high, (value >> 8) ^ 0xff)
        setattr(m, low, (value & 0xff) ^ 0xff)
        assert getattr(m, rp) == value ^ 0xffff, rp


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


def test_iff_and_int_disabled() -> None:
    # The interrupt flip-flops are accessible as public properties,
    # and 'int_disabled' tells whether accepting interrupts is
    # temporarily suppressed, and not whether they are enabled
    # (issue #69).
    EI = 0xfb
    NOP = 0x00
    m = z80.Z80Machine()
    m.set_memory_block(0x0000, bytes([EI, NOP]))

    def step() -> None:
        m.ticks_to_stop = 1
        m.run()

    assert (m.iff1, m.iff2, m.int_disabled) == (False, False, False)

    # 'ei' enables interrupts, but suppresses accepting them until
    # after the following instruction.
    step()
    assert (m.iff1, m.iff2, m.int_disabled) == (True, True, True)

    step()
    assert (m.iff1, m.iff2, m.int_disabled) == (True, True, False)

    # The flip-flops are also writable.
    m.iff1 = False
    m.iff2 = True
    assert (m.iff1, m.iff2) == (False, True)


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
