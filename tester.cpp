
/*  Z80 CPU Simulator.

    Copyright (C) 2017 Ivan Kosarev.
    ivan@kosarev.info

    Published under the MIT license.
*/

#include <cstring>

#include "z80.h"

static void test_disassembling() {
    typedef z80::dummy_ticks_handler ticks_handler;
    ticks_handler ticks;

    typedef z80::trivial_memory_handler<ticks_handler> memory_handler;
    memory_handler memory(ticks);

    typedef z80::disassembling_handler disasm_type;
    disasm_type disasm;

    z80::instructions_decoder<memory_handler,
                              disasm_type> decoder(memory, disasm);
    decoder.decode();
    assert(std::strcmp(disasm.get_output(), "nop") == 0);
}

static void test_execution() {
    typedef z80::trivial_ticks_handler<uint_fast32_t> ticks_handler;
    ticks_handler ticks;

    typedef z80::trivial_memory_handler<ticks_handler> memory_handler;
    memory_handler memory(ticks);

    typedef z80::cpu_instance cpu_type;
    cpu_type cpu;

    z80::instructions_decoder<memory_handler,
                              cpu_type> decoder(memory, cpu);
    assert(cpu.get_pc() == 0);
    decoder.decode();
    assert(cpu.get_pc() == 1);
}

int main() {
    test_disassembling();
    test_execution();
}
