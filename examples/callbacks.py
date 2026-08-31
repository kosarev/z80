#!/usr/bin/env python3

#   Z80 CPU Emulator.
#   https://github.com/kosarev/z80
#
#   Copyright (C) 2026 Ivan Kosarev.
#   mail@ivankosarev.com
#
#   Published under the MIT license.

from z80 import LD, OUT, A, At, Code, Z80Machine


def main():
    # Send a greeting to port 0xfe, then store the last character
    # at address 0x8000.
    code = Code()
    for c in 'Hello!\n':
        code.add(LD(A, ord(c)),
                 OUT(At(0xfe), A))
    code.add(LD(At(0x8000), A))
    _, image = code.encode()[0]

    m = Z80Machine()

    # With a read callback set, all memory reads, including
    # instruction fetches, go through it, so the machine's memory
    # may live entirely on the Python side.
    def on_read(addr):
        NOP = 0x00
        return image[addr] if addr < len(image) else NOP

    def on_write(addr, value):
        print(f'write of {value:#04x} at {addr:#06x}')

    # On Z80, the high 8 bits of the port address carry the value
    # of A for 'out (n), a' instructions, so mask them off to get
    # the port number.
    def on_output(addr, value):
        if addr & 0xff == 0xfe:
            print(chr(value), end='')

    m.set_read_callback(on_read)
    m.set_write_callback(on_write)
    m.set_output_callback(on_output)

    # Stop when the program is done.
    m.set_breakpoint(len(image))
    m.ticks_to_stop = 10_000
    m.run()
    assert m.pc == len(image)


if __name__ == "__main__":
    main()
