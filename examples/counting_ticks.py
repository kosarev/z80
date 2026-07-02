#!/usr/bin/env python3

#   Z80 CPU Emulator.
#   https://github.com/kosarev/z80
#
#   Copyright (C) 2026 Ivan Kosarev.
#   mail@ivankosarev.com
#
#   Published under the MIT license.

import z80


def main():
    LD_B_N = 0x06
    DJNZ = 0x10
    NOP = 0x00

    # A delay loop taking 7 + 99 * 13 + 8 = 1302 ticks, followed
    # by a 4-tick nop.
    code = bytes([
        LD_B_N, 100,  # 0x0000: ld b, 100
        DJNZ, 0xfe,   # 0x0002: djnz 0x0002
        NOP,          # 0x0004: nop
    ])

    m = z80.Z80Machine()
    m.set_memory_block(0x0000, code)

    # Stop when the loop is done. Mind that reading the offset of
    # the djnz instruction advances PC to 0x0004, so a breakpoint
    # there would trigger on the first iteration of the loop --
    # hence the trailing nop and the breakpoint placed past it, at
    # an address PC never reaches until the loop is done.
    STOP_AT = 0x0005
    m.set_breakpoint(STOP_AT)

    # Arm the tick limit with a value large enough for the run to
    # stop at the breakpoint rather than by exhausting the limit;
    # the remaining number of ticks is then exact. (A run that
    # does exhaust the limit overruns it by a few ticks that go
    # uncounted.)
    TICKS_LIMIT = 1_000_000
    m.ticks_to_stop = TICKS_LIMIT
    m.run()
    assert m.pc == STOP_AT

    elapsed = TICKS_LIMIT - m.ticks_to_stop
    print(f'{elapsed} ticks')  # 1306 ticks


if __name__ == "__main__":
    main()
