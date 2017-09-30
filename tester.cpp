
/*  Z80 CPU Simulator.

    Copyright (C) 2017 Ivan Kosarev.
    ivan@kosarev.info

    Published under the MIT license.
*/

#include "z80.h"

int main() {
    z80::simple_memory_interface memory;
    z80::instructions_decoder<z80::simple_memory_interface> decoder(memory);
    z80::opcode_kind opcode = decoder.decode_opcode();
    assert(opcode == z80::opcode_kind::nop);
}
