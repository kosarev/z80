
/*  Z80 CPU Simulator.

    Copyright (C) 2017 Ivan Kosarev.
    ivan@kosarev.info

    Published under the MIT license.
*/

#include "z80.h"

namespace z80 {

void disassembling_handler::nop() {
    std::snprintf(output, max_output_size, "nop");
}

}  // namespace z80
