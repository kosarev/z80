#!/usr/bin/env python3

#   Z80 CPU Emulator.
#   https://github.com/kosarev/z80
#
#   Copyright (C) 2017-2026 Ivan Kosarev.
#   mail@ivankosarev.com
#
#   Published under the MIT license.

from z80 import HALT, INC, A, Code, Z80Machine


def main():
    code = Code()
    code.add(INC(A),
             HALT(),
             INC(A))
    addr, image = code.encode()[0]

    m = Z80Machine()
    m.set_memory_block(addr, image)

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
