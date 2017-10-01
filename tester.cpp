
/*  Z80 CPU Simulator.

    Copyright (C) 2017 Ivan Kosarev.
    ivan@kosarev.info

    Published under the MIT license.
*/

#include <cstring>

#include "z80.h"

int main() {
    typedef z80::dummy_ticks_handler ticks_handler;
    ticks_handler ticks;

    typedef z80::trivial_memory_handler<ticks_handler> memory_handler;
    memory_handler memory(ticks);

    typedef z80::disassembling_handler instrs_handler;
    instrs_handler instrs;

    z80::instructions_decoder<memory_handler,
                              instrs_handler> decoder(memory, instrs);
    decoder.decode();
    assert(std::strcmp(instrs.get_output(), "nop") == 0);
}
