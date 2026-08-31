#!/usr/bin/env python3

#   Z80 CPU Emulator.
#   https://github.com/kosarev/z80
#
#   Copyright (C) 2026 Ivan Kosarev.
#   mail@ivankosarev.com
#
#   Published under the MIT license.

from z80 import HALT, LD, OUT, A, At, Code, Z80Machine


def main():
    # Instructions are objects that encode themselves, so programs
    # assemble without spelling opcodes. Symbols live in a Code
    # context and name values; the port could equally be a plain
    # number.
    code = Code()
    port = code.define_symbol('port', 0xfe)
    store = code.define_symbol('store', 0x8000)

    for c in 'Hello!\n':
        code.add(LD(A, ord(c)),
                 OUT(At(port), A))
    code.add(LD(At(store), A),
             HALT())

    # Lay the instructions out, starting at address 0 by default.
    (addr, image), = code.encode()

    m = Z80Machine()
    m.set_memory_block(addr, image)

    # On Z80, the high 8 bits of the port address carry the value
    # of A for 'out (n), a' instructions, so mask them off to get
    # the port number.
    def on_output(addr, value):
        if addr & 0xff == port.value:
            print(chr(value), end='')

    m.set_output_callback(on_output)

    m.ticks_to_stop = 10_000
    m.run()
    assert m.halted
    assert m.memory[store.value] == ord('\n')


if __name__ == "__main__":
    main()
