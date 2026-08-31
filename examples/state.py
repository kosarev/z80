#!/usr/bin/env python3

#   Z80 CPU Emulator.
#   https://github.com/kosarev/z80
#
#   Copyright (C) 2017-2026 Ivan Kosarev.
#   mail@ivankosarev.com
#
#   Published under the MIT license.

from z80 import ADD, HALT, A, B, Code, Z80Machine


def main():
    m = Z80Machine()

    # A one-instruction routine that adds the B register to the
    # accumulator and then halts.
    code = Code()
    code.add(ADD(A, B),
             HALT())
    addr, image = code.encode()[0]
    m.set_memory_block(addr, image)

    # Alter the state: seed the operands into the registers and point
    # the program counter at the routine, all from Python.
    m.a = 30
    m.b = 12
    m.pc = 0x0000

    m.run()

    # Examine the state: read the result back out of the accumulator.
    print(m.a)   # 42


if __name__ == "__main__":
    main()
