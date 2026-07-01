#!/usr/bin/env python3

#   Z80 CPU Emulator.
#   https://github.com/kosarev/z80
#
#   Copyright (C) 2017-2026 Ivan Kosarev.
#   mail@ivankosarev.com
#
#   Published under the MIT license.

import z80


def main():
    INC_A = 0x3c
    HALT = 0x76

    code = bytes([INC_A, HALT, INC_A])

    m = z80.Z80Machine()
    m.set_memory_block(0x0000, code)

    def step():
        m.ticks_to_stop = 1
        m.run()

        print(f'pc={m.pc:04x} a={m.a:02x} halted={m.halted}')

    # Execute the first 'inc a'.
    step()

    # Execute the halt.
    # From now on, stepping will not progress until we exit the halted mode.
    step()
    step()
    step()

    # Exit the halted mode and execute the second 'inc a'.
    m.halted = False
    step()


if __name__ == "__main__":
    main()
