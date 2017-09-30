
/*  Z80 CPU Simulator.

    Copyright (C) 2017 Ivan Kosarev.
    ivan@kosarev.info

    Published under the MIT license.
*/

#include <cstdio>
#include <cstdlib>

#include "z80.h"

namespace z80 {

memory_interface::memory_interface(bool use_custom_read_handler,
                                   bool use_custom_write_handler)
        : use_custom_read_handler(use_custom_read_handler),
          use_custom_write_handler(use_custom_write_handler) {
    clear();
}

void memory_interface::clear() {
    for(least_u8 &cell : image)
        cell = 0;
}

fast_u8 memory_interface::on_read(fast_u16 addr, memory_access_kind kind) {
    unused(addr, kind);

    // When a custom read handler is in use, it must override this default
    // implementation.
    assert(0);
}

void memory_interface::on_write(fast_u16 addr, fast_u8 value,
                                memory_access_kind kind) {
    unused(addr, value, kind);

    // When a custom write handler is in use, it must override this default
    // implementation.
    assert(0);
}

instructions_decoder::instructions_decoder(memory_interface &memory)
    : current_addr(0), transparent(true), memory(memory)
{}

opcode_kind instructions_decoder::decode_opcode() {
    fast_u16 addr = current_addr;
    fast_u8 op = fetch_opcode();
    if(op == 0)
        return opcode_kind::nop;

    // TODO
    std::fprintf(stderr, "Unknown opcode 0x%02x at 0x%04x.\n",
                 static_cast<unsigned>(op), static_cast<unsigned>(addr));
    std::abort();
}

cpu_instance::cpu_instance(memory_interface &memory)
    : decoder(memory)
{}

}  // namespace z80
