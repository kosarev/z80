
/*  Z80 CPU Simulator.

    Copyright (C) 2017 Ivan Kosarev.
    ivan@kosarev.info

    Published under the MIT license.
*/

#include <cstdio>
#include <cstdlib>

#include "z80.h"

namespace z80 {

opcode_kind instructions_decoder_base::decode(fast_u8 op, fast_u16 addr) {
    if(op == 0)
        return opcode_kind::nop;

    // TODO
    std::fprintf(stderr, "Unknown opcode 0x%02x at 0x%04x.\n",
                 static_cast<unsigned>(op), static_cast<unsigned>(addr));
    std::abort();
}

}  // namespace z80
