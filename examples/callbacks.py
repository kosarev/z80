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

    # With a read callback set, reads of read-marked addresses,
    # instruction fetches included, go through it, so the marked
    # memory may live entirely on the Python side.
    m.mark_addrs(0x0000, len(image), Z80Machine.READ_MARK)
    m.mark_addr(0x8000, Z80Machine.WRITE_MARK)

    def on_read(addr):
        return image[addr]

    def on_write(addr, value):
        print(f'write of {value:#04x} at {addr:#06x}')

    # Z80 port addresses are 16-bit, so match the low 8 bits.
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
