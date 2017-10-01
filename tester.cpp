
/*  Z80 CPU Simulator.

    Copyright (C) 2017 Ivan Kosarev.
    ivan@kosarev.info

    Published under the MIT license.
*/

#include "z80.h"

int main() {
    typedef z80::dummy_ticks_handler ticks_handler;
    ticks_handler ticks;

    typedef z80::trivial_memory_handler<ticks_handler> memory_handler;
    memory_handler memory(ticks);

    z80::instructions_decoder<memory_handler> decoder(memory);
    z80::opcode_kind opcode = decoder.decode_opcode();
    assert(opcode == z80::opcode_kind::nop);
}
