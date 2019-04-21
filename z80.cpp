
/*  Z80 CPU Simulator.
    https://github.com/kosarev/z80

    Copyright (C) 2017-2019 Ivan Kosarev.
    ivan@kosarev.info

    Published under the MIT license.
*/

#include "z80.h"

namespace z80 {

bool is_two_operand_alu_instr(alu k) {
    return k == alu::add || k == alu::adc || k == alu::sbc;
}

const char *get_condition_name(condition cc) {
    switch(cc) {
    case condition::nz: return "nz";
    case condition::z: return "z";
    case condition::nc: return "nc";
    case condition::c: return "c";
    case condition::po: return "po";
    case condition::pe: return "pe";
    case condition::p: return "p";
    case condition::m: return "m";
    }
    assert(0);
}

}  // namespace z80
