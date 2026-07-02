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
    LD_A_N = 0x3e
    OUT_N_A = 0xd3
    LD_NN_A = 0x32

    # Send a greeting to port 0xfe, then store the last character
    # at address 0x8000.
    code = b''
    for c in 'Hello!\n':
        code += bytes([LD_A_N, ord(c), OUT_N_A, 0xfe])
    code += bytes([LD_NN_A, 0x00, 0x80])

    m = z80.Z80Machine()

    # With a read callback set, all memory reads, including
    # instruction fetches, go through it, so the machine's memory
    # may live entirely on the Python side.
    def on_read(addr):
        NOP = 0x00
        return code[addr] if addr < len(code) else NOP

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
    m.set_breakpoint(len(code))
    m.ticks_to_stop = 10_000
    m.run()
    assert m.pc == len(code)


if __name__ == "__main__":
    main()
