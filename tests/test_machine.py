
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


@pytest.mark.parametrize('machine_type', [z80.Z80Machine, z80.I8080Machine])
def test_frame_tick_counts_and_wraps(
        machine_type: type[z80.I8080Machine | z80.Z80Machine]) -> None:
    # 'frame_tick' exposes the number of ticks into the current
    # 100000-tick frame (issue #67).
    m = machine_type()
    assert m.frame_tick == 0

    # Execute nops for exactly 1000 ticks.
    m.ticks_to_stop = 1000
    events = m.run()
    assert events == m._TICKS_LIMIT_HIT
    assert m.frame_tick == 1000

    # Completing the frame raises the end-of-frame event and wraps
    # the counter.
    events = m.run()
    assert events == m._END_OF_FRAME
    assert m.frame_tick == 0


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


def test_read_callback_timing() -> None:
    # Callbacks observe the clock at the hardware sampling positions:
    # an opcode read fires entering the t3 of its fetch cycle, with
    # two ticks of the cycle completed.
    m = z80.Z80Machine()
    ticks: list[int] = []

    def read(addr: int) -> int:
        ticks.append(m.frame_tick)
        return 0x00  # nop

    m.set_read_callback(read)
    m.ticks_to_stop = 8  # Two nops.
    events = m.run()
    assert events == m._TICKS_LIMIT_HIT
    assert ticks == [2, 6]


def test_read_callback_exception() -> None:
    # An exception raised in a callback stops the run at the end of
    # the current instruction and propagates unchanged; no further
    # callbacks are invoked.
    m = z80.Z80Machine()
    calls: list[int] = []

    def read(addr: int) -> int:
        calls.append(addr)
        raise RuntimeError('boom')

    m.set_read_callback(read)
    with pytest.raises(RuntimeError, match='boom'):
        m.run()
    assert calls == [0x0000]


def test_read_callback_bad_result() -> None:
    # A callback returning a non-integer raises a TypeError.
    m = z80.Z80Machine()
    m.set_read_callback(
        lambda addr: 'xyzzy')  # type: ignore[arg-type, return-value]
    with pytest.raises(TypeError, match='must be integer'):
        m.run()


def test_write_callback_exception() -> None:
    m = z80.Z80Machine()
    m.set_memory_block(0x0000, bytes([0x32, 0x00, 0x80]))  # ld (0x8000), a

    def write(addr: int, value: int) -> None:
        raise RuntimeError('boom')

    m.set_write_callback(write)
    with pytest.raises(RuntimeError, match='boom'):
        m.run()


def test_input_callback_exception() -> None:
    m = z80.Z80Machine()
    m.set_memory_block(0x0000, bytes([0xdb, 0xfe]))  # in a, (0xfe)

    def input_(addr: int) -> int:
        raise RuntimeError('boom')

    m.set_input_callback(input_)
    with pytest.raises(RuntimeError, match='boom'):
        m.run()


def test_int_vector_callback_exception() -> None:
    # Execute 'ei; im 2', then attempt an interrupt whose vector
    # callback raises.
    m = z80.Z80Machine()
    m.set_memory_block(0x0000, bytes([0xfb, 0xed, 0x5e]))
    m.ticks_to_stop = 12
    events = m.run()
    assert events == m._TICKS_LIMIT_HIT

    def vector() -> int:
        raise RuntimeError('boom')

    m.set_get_int_vector_callback(vector)
    with pytest.raises(RuntimeError, match='boom'):
        m.on_handle_active_int()


def test_breakpoint_trip_and_resume() -> None:
    # A breakpoint fires on the attempt to execute the marked
    # instruction, before it makes any progress; resuming means
    # explicitly stepping over it (issue #70).
    NOP = 0x00
    INC_A = 0x3c
    JP = 0xc3
    m = z80.Z80Machine()
    # 0x0000: nop
    # 0x0001: inc a
    # 0x0002: jp 0x0001
    m.set_memory_block(0x0000, bytes([NOP, INC_A, JP, 0x01, 0x00]))
    m.set_breakpoint(0x0001)

    events = m.run()
    assert events == m._BREAKPOINT_HIT
    assert (m.pc, m.a) == (0x0001, 0x00)

    # Running again is just another attempt to execute the marked
    # instruction, so it re-traps with no progress made.
    events = m.run()
    assert events == m._BREAKPOINT_HIT
    assert (m.pc, m.a) == (0x0001, 0x00)

    # Stepping over the breakpoint executes the marked instruction;
    # the 'jp' then loops back and the breakpoint fires again.
    events = m.step_over_breakpoint()
    assert events == m._NO_EVENTS
    assert (m.pc, m.a) == (0x0002, 0x01)

    events = m.run()
    assert events == m._BREAKPOINT_HIT
    assert (m.pc, m.a) == (0x0001, 0x01)


def test_breakpoint_beyond_jp() -> None:
    # Reading the operand of a 'jp' walks PC across the following
    # addresses; a breakpoint there must not fire when the 'jp'
    # executes (issue #70).
    NOP = 0x00
    JP = 0xc3
    m = z80.Z80Machine()
    m.set_memory_block(0x0000, bytes([NOP, JP, 0x00, 0x00]))
    m.set_breakpoint(0x0004)

    m.ticks_to_stop = 100
    events = m.run()
    assert events == m._TICKS_LIMIT_HIT
    assert m.pc == 0x0001


def test_breakpoint_while_halted() -> None:
    # While halted, the CPU keeps fetching the byte after the 'halt'
    # without executing it, so a breakpoint there must not fire until
    # the instruction is actually attempted (issue #70).
    HALT = 0x76
    NOP = 0x00
    m = z80.Z80Machine()
    m.set_memory_block(0x0000, bytes([HALT, NOP]))
    m.set_breakpoint(0x0000)
    m.set_breakpoint(0x0001)

    # The breakpoint on the 'halt' itself fires once, before it
    # executes.
    events = m.run()
    assert events == m._BREAKPOINT_HIT
    assert (m.pc, m.halted) == (0x0000, False)

    # Stepping over it executes the 'halt'.
    events = m.step_over_breakpoint()
    assert events == m._NO_EVENTS
    assert (m.pc, m.halted) == (0x0001, True)

    # The halted steps do not trip the breakpoint at PC.
    m.ticks_to_stop = 100
    events = m.run()
    assert events == m._TICKS_LIMIT_HIT
    assert (m.pc, m.halted) == (0x0001, True)

    # Once the machine is un-halted, as by an interrupt, the next run
    # does attempt the marked instruction.
    m.halted = False
    events = m.run()
    assert events == m._BREAKPOINT_HIT
    assert (m.pc, m.halted) == (0x0001, False)


def test_breakpoint_on_ldir() -> None:
    # Every iteration of 'ldir' rewinds PC and re-attempts the
    # instruction, so a breakpoint on it fires once per iteration and
    # each resume advances the copying by one byte (issue #70).
    m = z80.Z80Machine()
    m.set_memory_block(0x0000, bytes([0xed, 0xb0]))  # ldir
    m.set_memory_block(0x1000, b'abc')
    m.bc = 3
    m.hl = 0x1000
    m.de = 0x2000
    m.set_breakpoint(0x0000)

    events = m.run()
    assert events == m._BREAKPOINT_HIT
    assert (m.pc, m.bc) == (0x0000, 3)

    # All iterations but the last end with PC rewound back to the
    # instruction, so each resume copies one byte and re-traps.
    for count in 2, 1:
        events = m.step_over_breakpoint()
        assert events == m._NO_EVENTS
        assert (m.pc, m.bc) == (0x0000, count)

        events = m.run()
        assert events == m._BREAKPOINT_HIT
        assert (m.pc, m.bc) == (0x0000, count)

    # The last iteration moves past the instruction.
    events = m.step_over_breakpoint()
    assert events == m._NO_EVENTS
    assert (m.pc, m.bc) == (0x0002, 0)
    assert bytes(m.memory[0x2000:0x2003]) == b'abc'
