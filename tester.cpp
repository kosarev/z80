
/*  Z80 CPU Simulator.

    Copyright (C) 2017 Ivan Kosarev.
    ivan@kosarev.info

    Published under the MIT license.
*/

#include "z80.h"

int main() {
    typedef z80::simple_memory_implementation memory_type;
    memory_type memory;
    z80::instructions_decoder<memory_type> decoder(memory);
    z80::opcode_kind opcode = decoder.decode_opcode();
    assert(opcode == z80::opcode_kind::nop);
}
