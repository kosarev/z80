#!/usr/bin/env python3

#   Z80 CPU Emulator.
#   https://github.com/kosarev/z80
#
#   Copyright (C) 2026 Ivan Kosarev.
#   mail@ivankosarev.com
#
#   Published under the MIT license.

from z80 import Asm, SourceFile, Z80Machine

SOURCE = '''
port equ 0xfe

    org 0x8000
    ld hl, greeting
loop:
    ld a, (hl)
    or a
    jr z, done
    out (port), a
    inc hl
    jr loop
done:
    halt

greeting:
    db 0x48, 0x69, 0x21, 0x0a, 0x00
'''


def main():
    code = Asm().assemble(SourceFile('greeting.asm', SOURCE))
    code.resolve()
    addr, image = code.encode()[0]

    m = Z80Machine()
    m.set_memory_block(addr, image)
    m.pc = addr

    # On Z80, the high 8 bits of the port address carry the value
    # of A for 'out (n), a' instructions, so mask them off to get
    # the port number.
    def on_output(port, value):
        if port & 0xff == 0xfe:
            print(chr(value), end='')

    m.set_output_callback(on_output)

    m.ticks_to_stop = 10_000
    m.run()
    assert m.halted


if __name__ == "__main__":
    main()
